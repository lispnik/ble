(in-package #:ble)

;;; Central-side Nordic UART Service (NUS) client.
;;;
;;; Unlike the scanning and advertising code, which is connectionless, this
;;; opens a *connection* to a device and talks GATT to it. NUS is Nordic
;;; Semiconductor's serial-over-GATT profile: any Nordic-based peripheral
;;; exposing it can be connected to and streamed bytes over.
;;;
;;; The ATT machinery underneath it -- MTU exchange, discovery, CCCD
;;; subscribe, notifications -- is generic and usable on its own; NUS is one
;;; profile built on it, not the only thing it serves.
;;;
;;; We let the kernel BlueZ stack do the heavy lifting (LE connection +
;;; L2CAP) by opening an L2CAP socket bound to the ATT fixed channel
;;; (CID 0x0004) and connecting it to the device address. Over that socket
;;; we speak the ATT protocol directly:
;;;
;;;   - Exchange MTU                       (bigger writes / notifications)
;;;   - Read-By-Type over 0x2803           (enumerate characteristics)
;;;   - match the NUS RX / TX char UUIDs   (find the value handles)
;;;   - Find-Information over the TX range  (find the 0x2902 CCCD)
;;;   - Write 0x0001 to the CCCD            (subscribe to notifications)
;;;   - Write-Command to RX                 (host -> device)
;;;   - Handle-Value-Notification on TX     (device -> host)
;;;
;;; This needs the same CAP_NET_RAW + CAP_NET_ADMIN as the scanner. The
;;; scanner uses HCI_CHANNEL_RAW (channel 0), which leaves the kernel BLE
;;; stack live, so this L2CAP-socket path coexists with it.
;;;
;;; NUS UUIDs (Nordic):
;;;   Service 6E400001-B5A3-F393-E0A9-E50E24DCCA9E
;;;   RX      6E400002-...  (Write / Write-Without-Response: host -> device)
;;;   TX      6E400003-...  (Notify:                          device -> host)

;;; --- socket / address-family constants --------------------------------

(defconstant +sock-seqpacket+   5)   ; each send/recv is one ATT PDU
(defconstant +att-cid+          #x0004)
;; sockaddr address-type values (NOT the HCI 0/1 advertising addr-type)
(defconstant +bdaddr-le-public+ 1)
(defconstant +bdaddr-le-random+ 2)

;;; --- ATT protocol opcodes ---------------------------------------------

(defconstant +att-error-rsp+         #x01)
(defconstant +att-exchange-mtu-req+  #x02)
(defconstant +att-exchange-mtu-rsp+  #x03)
(defconstant +att-find-info-req+     #x04)
(defconstant +att-find-info-rsp+     #x05)
(defconstant +att-read-by-type-req+  #x08)
(defconstant +att-read-by-type-rsp+  #x09)
(defconstant +att-write-req+         #x12)
(defconstant +att-write-rsp+         #x13)
(defconstant +att-handle-value-ntf+  #x1B)
(defconstant +att-write-cmd+         #x52)

(defconstant +att-err-attr-not-found+ #x0A)

(defconstant +gatt-characteristic-decl+ #x2803)
(defconstant +gatt-cccd+                #x2902)

;;; NUS characteristic UUIDs in ATT wire order (128-bit, little-endian).
;;; 6E400002-B5A3-F393-E0A9-E50E24DCCA9E reversed, etc. Only byte 12 differs.
(defparameter +nus-rx-uuid-le+
  (coerce-octets #(#x9E #xCA #xDC #x24 #x0E #xE5 #xA9 #xE0
                   #x93 #xF3 #xA3 #xB5 #x02 #x00 #x40 #x6E))
  "NUS RX (host -> device) characteristic UUID, ATT wire order.")
(defparameter +nus-tx-uuid-le+
  (coerce-octets #(#x9E #xCA #xDC #x24 #x0E #xE5 #xA9 #xE0
                   #x93 #xF3 #xA3 #xB5 #x03 #x00 #x40 #x6E))
  "NUS TX (device -> host) characteristic UUID, ATT wire order.")

;;; fcntl / poll / getsockopt constants
(defconstant +o-nonblock+   #o4000)   ; 0x800 on Linux

;;; --- L2CAP socket ------------------------------------------------------

(defun connect-with-timeout (fd sa salen timeout-ms)
  "connect(2) FD to SA, but give up after TIMEOUT-MS instead of blocking on
the kernel's long LE connection timeout. Signals an error on failure or
timeout. Leaves FD in blocking mode on success."
  (let ((orig (%fcntl fd +f-getfl+ 0)))
    (%fcntl fd +f-setfl+ (logior orig +o-nonblock+))
    (unwind-protect
         (let ((rc (%connect fd sa salen)))
           (cond
             ((zerop rc) t)                       ; connected immediately
             ((/= (errno) +einprogress+)
              (error "connect(L2CAP) failed: ~A (errno ~D)" (%strerror (errno)) (errno)))
             (t
              ;; In progress: poll for writability, then read SO_ERROR.
              (cffi:with-foreign-object (pfd :unsigned-char 8)
                (dotimes (i 8) (setf (cffi:mem-aref pfd :unsigned-char i) 0))
                (setf (cffi:mem-aref pfd :int 0) fd
                      (cffi:mem-aref pfd :short 2) +pollout+)
                (let ((pr (%poll pfd 1 timeout-ms)))
                  (cond
                    ((zerop pr) (error "connect(L2CAP) timed out after ~Dms" timeout-ms))
                    ((< pr 0) (check-syscall pr "poll"))
                    (t (cffi:with-foreign-object (err :int)
                         ;; 'socklen must be quoted: with-foreign-object/mem-ref
                         ;; evaluate the type form (a bare SOCKLEN would be read
                         ;; as an undefined variable).
                         (cffi:with-foreign-object (len 'socklen)
                           (setf (cffi:mem-ref len 'socklen) 4
                                 (cffi:mem-ref err :int) 0)
                           (%getsockopt fd +sol-socket+ +so-error+ err len)
                           (let ((so-err (cffi:mem-ref err :int)))
                             (unless (zerop so-err)
                               (error "connect(L2CAP) failed: ~A (errno ~D)"
                                      (%strerror so-err) so-err))))))))))))
      ;; Restore blocking mode for the subsequent ATT request/response I/O.
      (%fcntl fd +f-setfl+ orig))))

(defun %fill-sockaddr-l2 (sa &key (psm 0) (cid +att-cid+)
                                  (bdaddr #(0 0 0 0 0 0))
                                  (bdaddr-type +bdaddr-le-public+))
  "Populate a 14-byte struct sockaddr_l2 at foreign pointer SA.
  { uint16 family; uint16 psm; uint8 bdaddr[6]; uint16 cid; uint8 type; }"
  (dotimes (i 14) (setf (cffi:mem-aref sa :unsigned-char i) 0))
  (setf (cffi:mem-aref sa :unsigned-char 0) (logand +af-bluetooth+ #xFF)
        (cffi:mem-aref sa :unsigned-char 1) (logand (ash +af-bluetooth+ -8) #xFF)
        (cffi:mem-aref sa :unsigned-char 2) (logand psm #xFF)
        (cffi:mem-aref sa :unsigned-char 3) (logand (ash psm -8) #xFF))
  (loop for i below 6
        do (setf (cffi:mem-aref sa :unsigned-char (+ 4 i)) (aref bdaddr i)))
  (setf (cffi:mem-aref sa :unsigned-char 10) (logand cid #xFF)
        (cffi:mem-aref sa :unsigned-char 11) (logand (ash cid -8) #xFF)
        (cffi:mem-aref sa :unsigned-char 12) bdaddr-type))

(defun l2cap-att-connect (mac &key (addr-type :random) (timeout 10)
                                   dev set-default-phy)
  "Open an L2CAP socket and connect to MAC (6 bytes, on-air byte order)
on the ATT fixed channel. Returns the connected file descriptor.

The kernel performs the LE connection. ADDR-TYPE is :PUBLIC or :RANDOM --
the peer's address type, as a keyword rather than the raw HCI constant, so
callers do not have to import the constants to say something this simple.
TIMEOUT is the connect timeout in seconds. DEV, if
given, is an HCI adapter index: the L2CAP source is bound to that
adapter's BD_ADDR so the connection is initiated from it specifically
(otherwise the kernel routes via any adapter).

NOTE: connect() drives the kernel's accept-list auto-connect -- a background
scan on both 1M and Coded PHY, then initiate on a connectable advert -- and
it does not always establish. Observed failures include peripherals that
demonstrably advertise connectable but never link (MGMT Connect Failed,
status 0x0e, even at a 30 s timeout), and controllers wedged such that this
path hangs indefinitely while HCI-USER-ATT-CONNECT succeeds immediately.
Neither is root-caused. When determinism matters, drive HCI yourself; see
src/hci-conn.lisp."
  ;; Optionally bias the controller's PHY preference to 1M + Coded before
  ;; connecting (probe whether it helps reach Coded-only connectable peers).
  (when (and set-default-phy dev)
    (hci-set-default-phy :dev dev :tx-phys #x05 :rx-phys #x05))
  (let ((bdaddr-type (ecase addr-type
                       (:public +bdaddr-le-public+)
                       (:random +bdaddr-le-random+)))
        (fd (check-syscall
             (%socket +af-bluetooth+ +sock-seqpacket+ +btproto-l2cap+)
             "socket(L2CAP)"))
        (src (when dev (hci-read-bd-addr :dev dev))))
    (handler-case
        (progn
          ;; Bind the source: the chosen adapter (or any), ATT channel.
          ;; An adapter's identity BD_ADDR is a public address.
          (cffi:with-foreign-object (sa :unsigned-char 14)
            (%fill-sockaddr-l2 sa :bdaddr (or src #(0 0 0 0 0 0))
                                  :bdaddr-type +bdaddr-le-public+)
            (check-syscall (%bind fd sa 14) "bind(L2CAP)"))
          ;; Connect to the peer on the ATT channel, with a timeout.
          (cffi:with-foreign-object (sa :unsigned-char 14)
            (%fill-sockaddr-l2 sa :bdaddr (coerce-octets mac)
                                  :bdaddr-type bdaddr-type)
            (connect-with-timeout fd sa 14 (round (* 1000 timeout))))
          fd)
      (error (c) (%close fd) (error c)))))

;;; --- ATT send / receive over the seqpacket socket ---------------------

(defparameter *att-buffer-size* 1024)

(defparameter *att-rx-mtu* 23
  "Our advertised ATT receive MTU. 23 (the default) keeps every PDU within
one HCI ACL packet so no L2CAP fragmentation is needed. Raise it to let a
device send larger single notifications (the HCI transport reassembles).")

;;; A "channel" is either an integer fd (kernel L2CAP socket; see
;;; NUS-CONNECT) or an HCI-CONN struct (HCI_CHANNEL_USER transport; see
;;; hci-conn.lisp). ATT-SEND / ATT-RECV dispatch on which it is, so the ATT
;;; protocol code below is shared by both transports. The HCI-CONN helpers
;;; are forward references resolved at load time (hci-conn loads after nus).

(defun att-send (chan pdu)
  "Send one ATT PDU over CHAN."
  (if (integerp chan)
      (let* ((pdu (coerce-octets pdu))
             (n (length pdu)))
        (cffi:with-foreign-object (buf :unsigned-char (max 1 n))
          (bytes-to-foreign pdu buf)
          (check-syscall (%write chan buf n) "att write")))
      (hci-acl-send-att chan pdu)))

(defun att-recv (chan &optional (timeout-ms 5000))
  "Receive one ATT PDU from CHAN: octet vector, NIL on timeout/EOF, or the
symbol :DISCONNECTED (HCI transport only). TIMEOUT-MS now bounds BOTH
transports -- the kernel-socket path used to block indefinitely, which meant
a caller could not stop on a deadline."
  (if (integerp chan)
      (when (fd-readable-p chan timeout-ms)
        (cffi:with-foreign-object (buf :unsigned-char *att-buffer-size*)
          (let ((n (%read chan buf *att-buffer-size*)))
            (when (> n 0) (foreign-to-bytes buf n)))))
      (hci-acl-recv-att chan timeout-ms)))

(defun att-write-value (chan handle value)
  "Write VALUE to HANDLE with a Write Request and wait for the answer.

Returns T when the peer acknowledged, :TIMEOUT, or the ATT error code if it
refused. The difference from a Write Command matters when probing: a command
is fire-and-forget, so a device rejecting the write looks exactly like a
device that accepted it and did nothing."
  (let* ((value (coerce-octets value))
         (pdu (make-octets (+ 3 (length value)))))
    (setf (aref pdu 0) +att-write-req+)
    (u16le-put pdu 1 handle)
    (replace pdu value :start1 3)
    (let ((rsp (att-request chan pdu :expect +att-write-rsp+)))
      (cond ((null rsp) :timeout)
            ((att-error-p rsp) (when (>= (length rsp) 5) (aref rsp 4)))
            (t t)))))

(defun att-request (chan pdu &key expect)
  "Send PDU and return the response PDU matching EXPECT (or an error
response). Server-initiated traffic that arrives meanwhile is handled and
skipped: stray Handle-Value-Notifications are ignored, and a peer Exchange
MTU Request is answered (devices often send their own right after connect,
which would otherwise desync our request/response pairing). If EXPECT is
NIL, the first non-skipped PDU is returned."
  (att-send chan pdu)
  (loop for rsp = (att-recv chan)
        while (and rsp (vectorp rsp))
        for op = (aref rsp 0)
        do (cond ((= op +att-handle-value-ntf+) nil)        ; stray notification
                 ((= op +att-exchange-mtu-req+)             ; peer-initiated MTU: answer
                  (let ((r (make-octets 3)))
                    (setf (aref r 0) +att-exchange-mtu-rsp+)
                    (u16le-put r 1 *att-rx-mtu*)            ; our rx MTU
                    (att-send chan r)))
                 ((= op +att-error-rsp+) (return rsp))
                 ((or (null expect) (= op expect)) (return rsp))
                 (t nil))))                                  ; non-matching: skip, keep reading

(defun att-error-p (rsp) (and rsp (= (aref rsp 0) +att-error-rsp+)))

;;; --- GATT discovery ----------------------------------------------------

(defun att-exchange-mtu (chan &optional (client-mtu *att-rx-mtu*))
  "Negotiate the ATT MTU. Returns the agreed MTU (>= 23).

CLIENT-MTU defaults to what we advertise, so callers with no opinion -- which
is most of them -- do not have to have one."
  (let ((req (make-octets 3)))
    (setf (aref req 0) +att-exchange-mtu-req+)
    (u16le-put req 1 client-mtu)
    (let ((rsp (att-request chan req :expect +att-exchange-mtu-rsp+)))
      (if (and rsp (= (aref rsp 0) +att-exchange-mtu-rsp+))
          (max 23 (min client-mtu (u16-le rsp 1)))
          23))))

;;; --- UUIDs and characteristics ----------------------------------------
;;;
;;; UUIDs are held the way ATT puts them on the wire: little-endian octets,
;;; two of them for a 16-bit UUID and sixteen for a full one. Comparing octet
;;; vectors keeps both cases on one code path.

(defun uuid16 (value)
  "The ATT wire form of a 16-bit UUID."
  (let ((v (make-octets 2)))
    (u16le-put v 0 value)
    v))

(defun uuid-string (uuid)
  "Render an ATT-order UUID for humans: \"FFE1\", or the dashed 128-bit form."
  (let ((be (reverse (coerce uuid 'list))))
    (if (= (length be) 2)
        (format nil "~{~2,'0X~}" be)
        (format nil "~{~2,'0X~}-~{~2,'0X~}-~{~2,'0X~}-~{~2,'0X~}-~{~2,'0X~}"
                (subseq be 0 4) (subseq be 4 6) (subseq be 6 8)
                (subseq be 8 10) (subseq be 10 16)))))

(defstruct gatt-char
  "One characteristic found by discovery: its value handle, its property
bitmap, and its UUID in ATT wire order.

The properties are the reason this is a struct rather than the (HANDLE . UUID)
cons it used to be. Knowing whether a characteristic is notify, write, or
write-without-response is how you tell a device's command channel from its
data channel without guessing."
  handle properties uuid)

(defun gatt-char-uuid-string (c) (uuid-string (gatt-char-uuid c)))

(defconstant +char-prop-read+          #x02)
(defconstant +char-prop-write-no-rsp+  #x04)
(defconstant +char-prop-write+         #x08)
(defconstant +char-prop-notify+        #x10)
(defconstant +char-prop-indicate+      #x20)

(defun gatt-char-property-names (c)
  "The property bits of C as a list of keywords."
  (let ((p (gatt-char-properties c)))
    (remove nil
            (list (when (logtest p +char-prop-read+)         :read)
                  (when (logtest p +char-prop-write-no-rsp+) :write-without-response)
                  (when (logtest p +char-prop-write+)        :write)
                  (when (logtest p +char-prop-notify+)       :notify)
                  (when (logtest p +char-prop-indicate+)     :indicate)))))

(defun att-write-command (chan handle value)
  "Write VALUE to HANDLE with a Write Command -- no response, no ack."
  (let* ((value (coerce-octets value))
         (pdu (make-octets (+ 3 (length value)))))
    (setf (aref pdu 0) +att-write-cmd+)
    (u16le-put pdu 1 handle)
    (replace pdu value :start1 3)
    (att-send chan pdu)))

(defun att-subscribe (chan cccd-handle &key indications)
  "Enable notifications (or indications) by writing to the CCCD at
CCCD-HANDLE. Signals on an ATT error."
  (let ((rsp-or-code (att-write-value chan cccd-handle
                                      (let ((v (make-octets 2)))
                                        (u16le-put v 0 (if indications #x0002 #x0001))
                                        v))))
    (unless (eq rsp-or-code t)
      (error "failed to subscribe at handle 0x~4,'0X (~A)"
             cccd-handle
             (if (integerp rsp-or-code)
                 (format nil "ATT error 0x~2,'0X" rsp-or-code)
                 rsp-or-code)))
    t))

(defun att-next-notification (chan handle &optional (timeout-ms 5000))
  "Block for the next Handle Value Notification on HANDLE and return its
value octets. NIL on timeout, on disconnect, or on any other PDU."
  (let ((pdu (att-recv chan timeout-ms)))
    (when (and pdu (vectorp pdu) (>= (length pdu) 3)
               (= (aref pdu 0) +att-handle-value-ntf+)
               (= (u16-le pdu 1) handle))
      (subseq pdu 3))))

(defun att-channel-close (chan)
  "Close an ATT channel, whichever transport it is: a kernel L2CAP socket, or
an HCI-CONN whose adapter has to be handed back to the kernel."
  (cond ((integerp chan) (%close chan))
        (chan (hci-conn-close chan))))

(defun att-discover-characteristics (fd)
  "Walk Read-By-Type (0x2803) over the whole handle space. Returns a list of
GATT-CHAR, in handle order."
  (let ((chars nil) (start 1))
    (loop
      (let ((req (make-octets 7)))
        (setf (aref req 0) +att-read-by-type-req+)
        (u16le-put req 1 start)
        (u16le-put req 3 #xFFFF)
        (u16le-put req 5 +gatt-characteristic-decl+)
        (let ((rsp (att-request fd req :expect +att-read-by-type-rsp+)))
          (when (or (null rsp) (att-error-p rsp)
                    (/= (aref rsp 0) +att-read-by-type-rsp+))
            (return))
          ;; rsp: opcode(1) each-len(1) then entries of each-len bytes:
          ;;   decl-handle(2) properties(1) value-handle(2) uuid(2 or 16)
          (let ((each (aref rsp 1)) (i 2) (last 0))
            (loop while (<= (+ i each) (length rsp)) do
              (let ((decl-handle (u16-le rsp i)))
                (push (make-gatt-char :handle (u16-le rsp (+ i 3))
                                      :properties (aref rsp (+ i 2))
                                      :uuid (subseq rsp (+ i 5) (+ i each)))
                      chars)
                (setf last decl-handle)
                (incf i each)))
            (when (>= last #xFFFF) (return))
            (setf start (1+ last))))))
    (nreverse chars)))

(defun find-char-by-uuid (chars uuid)
  "The GATT-CHAR in CHARS whose UUID matches UUID (ATT wire order), or NIL."
  (find (coerce-octets uuid) chars :key #'gatt-char-uuid :test #'equalp))

(defun char-handle-by-uuid (chars uuid-le)
  "Value handle of the characteristic whose UUID matches UUID-LE, or NIL."
  (let ((c (find-char-by-uuid chars uuid-le)))
    (when c (gatt-char-handle c))))

(defun att-find-cccd (fd tx-handle)
  "Find the Client Characteristic Configuration descriptor (0x2902) that
follows the TX characteristic value. Returns its handle, or NIL."
  (let ((req (make-octets 5)))
    (setf (aref req 0) +att-find-info-req+)
    (u16le-put req 1 (1+ tx-handle))
    (u16le-put req 3 (min #xFFFF (+ tx-handle 3)))
    (let ((rsp (att-request fd req :expect +att-find-info-rsp+)))
      (when (and rsp (= (aref rsp 0) +att-find-info-rsp+))
        (let* ((fmt (aref rsp 1))          ; 1 = 16-bit UUIDs, 2 = 128-bit
               (step (if (= fmt 1) 4 18))
               (i 2))
          (loop while (<= (+ i step) (length rsp)) do
            (let ((handle (u16-le rsp i)))
              (when (and (= fmt 1) (= (u16-le rsp (+ i 2)) +gatt-cccd+))
                (return-from att-find-cccd handle))
              (incf i step)))))
      nil)))

;;; --- NUS connection ----------------------------------------------------

(defstruct nus
  "An open NUS GATT connection. FD is the connected L2CAP/ATT socket."
  fd mtu rx-handle tx-handle cccd-handle bdaddr-type)

(defun nus-connect (mac &key (addr-type :random) (mtu 247) (timeout 10)
                             dev set-default-phy)
  "Connect to the NUS server on the device at MAC (6 octets, on-air byte
order), discover the RX/TX characteristics, and subscribe to TX
notifications. ADDR-TYPE is :public or :random. TIMEOUT is the connect
timeout in seconds. DEV, if given, binds the connection to that HCI
adapter (otherwise the kernel chooses one). Returns a NUS, or signals.

Use NUS-SEND to write to the device and NUS-RECV to read notifications;
NUS-CLOSE to tear down."
  (let* ((bdtype (ecase addr-type (:public +bdaddr-le-public+) (:random +bdaddr-le-random+)))
         (fd (l2cap-att-connect mac :addr-type addr-type :timeout timeout :dev dev
                                    :set-default-phy set-default-phy))
         (conn (make-nus :fd fd :mtu 23 :bdaddr-type bdtype)))
    (handler-case
        (progn
          (setf (nus-mtu conn) (att-exchange-mtu fd mtu))
          (let ((chars (att-discover-characteristics fd)))
            (setf (nus-rx-handle conn) (char-handle-by-uuid chars +nus-rx-uuid-le+)
                  (nus-tx-handle conn) (char-handle-by-uuid chars +nus-tx-uuid-le+))
            (unless (and (nus-rx-handle conn) (nus-tx-handle conn))
              (error "NUS characteristics not found (rx=~A tx=~A); is this a NUS device?"
                     (nus-rx-handle conn) (nus-tx-handle conn)))
            (setf (nus-cccd-handle conn)
                  (or (att-find-cccd fd (nus-tx-handle conn))
                      (1+ (nus-tx-handle conn))))
            (att-subscribe fd (nus-cccd-handle conn)))
          conn)
      (error (c) (%close fd) (error c)))))

(defun nus-close (conn)
  "Close the NUS connection (dispatches on the transport)."
  (let ((chan (nus-fd conn)))
    (cond ((integerp chan) (%close chan))
          (chan (hci-conn-close chan)))   ; HCI transport (hci-conn.lisp)
    (setf (nus-fd conn) nil)))

(defun nus-send (conn data)
  "Write DATA (a string or octet vector) to the NUS RX characteristic using
a Write-Command (no response). Splits into MTU-3 byte chunks if needed."
  (let* ((bytes (coerce-octets (if (stringp data)
                                   (map 'vector #'char-code data)
                                   data)))
         (chunk (max 1 (- (nus-mtu conn) 3))))
    (loop for start from 0 below (max 1 (length bytes)) by chunk
          for end = (min (length bytes) (+ start chunk))
          do (att-write-command (nus-fd conn) (nus-rx-handle conn)
                                (subseq bytes start end))
          while (< end (length bytes)))))

(defun nus-recv (conn &optional (timeout-ms 3600000))
  "Block for the next inbound PDU. Returns the value octets of a TX
Handle-Value-Notification, or NIL on close / non-notification PDU.
TIMEOUT-MS bounds the wait on the HCI transport (effectively blocking by
default); the kernel-socket transport blocks regardless."
  (att-next-notification (nus-fd conn) (nus-tx-handle conn) timeout-ms))
