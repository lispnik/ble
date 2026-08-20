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
(defconstant +att-find-by-type-value-req+ #x06)
(defconstant +att-find-by-type-value-rsp+ #x07)
(defconstant +att-read-by-group-type-req+ #x10)
(defconstant +att-read-by-group-type-rsp+ #x11)
(defconstant +att-read-req+          #x0A)
(defconstant +att-read-rsp+          #x0B)
(defconstant +att-read-blob-req+     #x0C)
(defconstant +att-read-multiple-req+ #x0E)
(defconstant +att-read-multiple-rsp+ #x0F)
(defconstant +att-prepare-write-req+ #x16)
(defconstant +att-prepare-write-rsp+ #x17)
(defconstant +att-execute-write-req+ #x18)
(defconstant +att-execute-write-rsp+ #x19)
(defconstant +att-read-blob-rsp+     #x0D)
(defconstant +att-read-by-type-req+  #x08)
(defconstant +att-read-by-type-rsp+  #x09)
(defconstant +att-write-req+         #x12)
(defconstant +att-write-rsp+         #x13)
(defconstant +att-handle-value-ntf+  #x1B)
(defconstant +att-handle-value-ind+  #x1D)
(defconstant +att-handle-value-cfm+  #x1E)
(defconstant +att-write-cmd+         #x52)

(defconstant +att-err-attr-not-found+     #x0A)
(defconstant +att-err-invalid-offset+     #x07)
(defconstant +att-err-attr-not-long+      #x0B)

(defconstant +gatt-primary-service+     #x2800)
(defconstant +gatt-secondary-service+   #x2801)
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
          ;; Registered so CLOSE-ALL-ATT-CHANNELS can find it; see
          ;; src/teardown.lisp.
          (register-att-channel fd))
      (error (c) (%close fd) (error c)))))

;;; --- ATT send / receive over the seqpacket socket ---------------------

(defparameter *att-buffer-size* 1024)

(defparameter *att-rx-mtu* 23
  "Our advertised ATT receive MTU. 23 (the default) keeps every PDU within
one HCI ACL packet so no L2CAP fragmentation is needed. Raise it to let a
device send larger single notifications (the HCI transport reassembles).")

;;; A "channel" is one of three things: an integer fd (kernel L2CAP socket;
;;; see NUS-CONNECT), an HCI-CONN struct (HCI_CHANNEL_USER transport; see
;;; hci-conn.lisp), or an ATT-TEST-CHANNEL with a scripted responder behind
;;; it. ATT-SEND / ATT-RECV dispatch on which, so every line of ATT protocol
;;; below is shared by all three. The HCI-CONN helpers are forward references
;;; resolved at load time (hci-conn loads after nus).

;;; --- a channel with no radio behind it ---------------------------------
;;;
;;; Every ATT operation below is a loop with a termination condition -- walk
;;; until the peer says stop, until the handles run out, until a response
;;; comes back short. Those conditions are where the bugs are, and none of
;;; them is reachable from a test that needs a device.
;;;
;;; This lives in the library rather than the test suite because a consumer
;;; building its own GATT profile has exactly the same problem, and because
;;; keeping all the dispatch in one place is what makes the ATT code
;;; transport-blind to begin with.

(defstruct (att-test-channel (:constructor make-att-test-channel (&key responder)))
  "An ATT channel backed by a function instead of a socket.

RESPONDER is called with each PDU sent and returns what the peer would reply:
an octet vector, a list of them, or NIL for silence. SENT keeps every PDU in
order, so a test can assert on what went out as well as what came back --
which for something like a range-scoped discovery is the only place the
behaviour is visible at all."
  responder
  (sent '())
  (inbox '()))

(defun att-test-channel-sent-pdus (chan)
  "The PDUs written to CHAN, oldest first."
  (reverse (att-test-channel-sent chan)))

(defun %test-channel-send (chan pdu)
  (let ((pdu (coerce-octets pdu)))
    (push pdu (att-test-channel-sent chan))
    (let ((reply (funcall (att-test-channel-responder chan) pdu)))
      (dolist (r (cond ((null reply) '())
                       ((listp reply) reply)
                       (t (list reply))))
        (setf (att-test-channel-inbox chan)
              (append (att-test-channel-inbox chan) (list (coerce-octets r))))))
    (length pdu)))

(defun att-send (chan pdu)
  "Send one ATT PDU over CHAN."
  (cond
    ((att-test-channel-p chan) (%test-channel-send chan pdu))
    ((integerp chan)
     (let* ((pdu (coerce-octets pdu))
            (n (length pdu)))
       (cffi:with-foreign-object (buf :unsigned-char (max 1 n))
         (bytes-to-foreign pdu buf)
         (check-syscall (%write chan buf n) "att write"))))
    (t (hci-acl-send-att chan pdu))))

(defun att-recv (chan &optional (timeout-ms 5000))
  "Receive one ATT PDU from CHAN: octet vector, NIL on timeout/EOF, or the
symbol :DISCONNECTED (HCI transport only). TIMEOUT-MS now bounds BOTH
transports -- the kernel-socket path used to block indefinitely, which meant
a caller could not stop on a deadline."
  (cond
    ((att-test-channel-p chan) (pop (att-test-channel-inbox chan)))
    ((integerp chan)
     (when (fd-readable-p chan timeout-ms)
       (cffi:with-foreign-object (buf :unsigned-char *att-buffer-size*)
         (let ((n (%read chan buf *att-buffer-size*)))
           (when (> n 0) (foreign-to-bytes buf n))))))
    (t (hci-acl-recv-att chan timeout-ms))))

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

;;; --- reading --------------------------------------------------------

(defun %read-req-pdu (handle)
  (let ((pdu (make-octets 3)))
    (setf (aref pdu 0) +att-read-req+)
    (u16le-put pdu 1 handle)
    pdu))

(defun %read-blob-req-pdu (handle offset)
  (let ((pdu (make-octets 5)))
    (setf (aref pdu 0) +att-read-blob-req+)
    (u16le-put pdu 1 handle)
    (u16le-put pdu 3 offset)
    pdu))

(defun att-read-value (chan handle)
  "Read the value of the attribute at HANDLE with a Read Request.

Returns (VALUES OCTETS ERROR). ERROR is NIL on success, :TIMEOUT, or the ATT
error code. Two octets come back rather than one flag because for a read the
payload is the point -- ATT-WRITE-VALUE can get away with returning T.

The value may be TRUNCATED: a Read Response carries at most MTU-1 octets, and
the peer does not say whether more exists. See ATT-READ-LONG-VALUE, and
VALUE-MAY-BE-TRUNCATED-P for how to tell."
  (let ((rsp (att-request chan (%read-req-pdu handle) :expect +att-read-rsp+)))
    (cond ((null rsp) (values nil :timeout))
          ((att-error-p rsp) (values nil (when (>= (length rsp) 5) (aref rsp 4))))
          (t (values (subseq rsp 1) nil)))))

(defun value-may-be-truncated-p (value mtu)
  "True when VALUE exactly fills what a Read Response can carry at MTU.

ATT gives no length field and no more-data flag: a full response is the only
hint that the attribute is longer, and it is ambiguous -- a value that happens
to be exactly MTU-1 octets looks identical to a truncated one. That ambiguity
is why reading long values takes a second round trip that usually finds
nothing."
  (>= (length value) (1- mtu)))

(defun att-read-long-value (chan handle &key (mtu *att-rx-mtu*) (max-length 512))
  "Read the whole value at HANDLE, continuing with Read Blob Requests while
the peer keeps filling responses.

Returns (VALUES OCTETS ERROR) like ATT-READ-VALUE. MAX-LENGTH caps the result
-- the ATT maximum attribute length is 512 octets, and a peer that answers
blob requests forever should not be able to exhaust memory here.

An ATT error on a continuation is not necessarily a failure: a peer answering
Attribute Not Long or Invalid Offset is saying the value ended exactly on the
boundary, so what has been read is complete and is returned."
  (multiple-value-bind (value error) (att-read-value chan handle)
    (when error (return-from att-read-long-value (values nil error)))
    (loop
      (unless (value-may-be-truncated-p value mtu) (return))
      (when (>= (length value) max-length) (return))
      (let ((rsp (att-request chan (%read-blob-req-pdu handle (length value))
                              :expect +att-read-blob-rsp+)))
        (cond
          ((null rsp) (return-from att-read-long-value (values value :timeout)))
          ((att-error-p rsp)
           (let ((code (when (>= (length rsp) 5) (aref rsp 4))))
             (if (member code (list +att-err-attr-not-long+ +att-err-invalid-offset+))
                 (return)                       ; ended on the boundary
                 (return-from att-read-long-value (values value code)))))
          (t
           (let ((chunk (subseq rsp 1)))
             (when (zerop (length chunk)) (return))
             (setf value (concatenate '(simple-array (unsigned-byte 8) (*))
                                      value chunk))
             ;; A short blob response means the value ended here.
             (when (< (length chunk) (- mtu 1)) (return)))))))
    (values (if (> (length value) max-length) (subseq value 0 max-length) value)
            nil)))

(defun att-read-multiple (chan handles)
  "Read several attributes in one round trip. Returns (VALUES OCTETS ERROR).

The response is the values CONCATENATED, with no lengths and no delimiters --
ATT assumes the client already knows how wide each attribute is. If you do
not, this tells you less than reading them one at a time, and reading them
one at a time is then the right call. Offered because when you do know, it
turns N round trips into one."
  (let* ((handles (coerce handles 'list))
         (pdu (make-octets (+ 1 (* 2 (length handles))))))
    (setf (aref pdu 0) +att-read-multiple-req+)
    (loop for h in handles
          for i from 1 by 2
          do (u16le-put pdu i h))
    (let ((rsp (att-request chan pdu :expect +att-read-multiple-rsp+)))
      (cond ((null rsp) (values nil :timeout))
            ((att-error-p rsp) (values nil (when (>= (length rsp) 5) (aref rsp 4))))
            (t (values (subseq rsp 1) nil))))))

;;; --- long writes ------------------------------------------------------

(defun att-prepare-write (chan handle offset part)
  "Queue PART of a value at OFFSET on the peer. Returns (VALUES ECHO ERROR).

The peer echoes back what it queued, and the echo is worth checking: it is
the only confirmation that the offset and the bytes arrived as sent, and a
queue that has gone wrong is still sitting there waiting to be executed."
  (let* ((part (coerce-octets part))
         (pdu (make-octets (+ 5 (length part)))))
    (setf (aref pdu 0) +att-prepare-write-req+)
    (u16le-put pdu 1 handle)
    (u16le-put pdu 3 offset)
    (replace pdu part :start1 5)
    (let ((rsp (att-request chan pdu :expect +att-prepare-write-rsp+)))
      (cond ((null rsp) (values nil :timeout))
            ((att-error-p rsp) (values nil (when (>= (length rsp) 5) (aref rsp 4))))
            (t (values (subseq rsp 1) nil))))))

(defun att-execute-write (chan &key cancel)
  "Commit the queued prepared writes, or discard them with CANCEL.

Returns T, :TIMEOUT, or the ATT error code. Cancelling matters: a failed long
write leaves a queue on the peer, and the next client to prepare a write
inherits it."
  (let ((pdu (make-octets 2)))
    (setf (aref pdu 0) +att-execute-write-req+
          (aref pdu 1) (if cancel 0 1))
    (let ((rsp (att-request chan pdu :expect +att-execute-write-rsp+)))
      (cond ((null rsp) :timeout)
            ((att-error-p rsp) (when (>= (length rsp) 5) (aref rsp 4)))
            (t t)))))

(defun att-write-long-value (chan handle value &key (mtu *att-rx-mtu*))
  "Write VALUE to HANDLE however long it is, via Prepare Write and Execute
Write. Returns T, :TIMEOUT, or an ATT error code.

A plain Write Request carries at most MTU-3 octets, which is 20 at the
default MTU -- this is how you write more. Each part is queued with its
offset and nothing takes effect until the execute, so a value arrives whole
or not at all.

On any failure the queue is CANCELLED before returning. Leaving it would hand
the next client a half-written value to commit."
  (let* ((value (coerce-octets value))
         (chunk (max 1 (- mtu 5))))          ; opcode + handle + offset
    (loop for offset from 0 below (max 1 (length value)) by chunk
          for part = (subseq value offset (min (length value) (+ offset chunk)))
          do (multiple-value-bind (echo error) (att-prepare-write chan handle offset part)
               (declare (ignore echo))
               (when error
                 (att-execute-write chan :cancel t)
                 (return-from att-write-long-value error))))
    (att-execute-write chan)))

(defun att-read-characteristic (chan chars uuid &key long (mtu *att-rx-mtu*))
  "Read the characteristic in CHARS whose UUID matches, in one step.

The common case: discover once, then read by UUID rather than by a handle
number that means nothing at the call site. Returns (VALUES OCTETS ERROR),
with ERROR :NOT-FOUND when no characteristic matches."
  (let ((c (find-char-by-uuid chars uuid)))
    (cond ((null c) (values nil :not-found))
          (long (att-read-long-value chan (gatt-char-handle c) :mtu mtu))
          (t (att-read-value chan (gatt-char-handle c))))))

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
                 ((= op +att-handle-value-ind+)             ; stray indication
                  ;; Skipping without confirming would wedge the peer for
                  ;; every later exchange, so this is not optional here
                  ;; either -- discovery is exactly when strays arrive.
                  (att-confirm-indication chan))
                 ((= op +att-exchange-mtu-req+)             ; peer-initiated MTU: answer
                  (let ((r (make-octets 3)))
                    (setf (aref r 0) +att-exchange-mtu-rsp+)
                    (u16le-put r 1 *att-rx-mtu*)            ; our rx MTU
                    (att-send chan r)))
                 ((= op +att-error-rsp+) (return rsp))
                 ((or (null expect) (= op expect)) (return rsp))
                 (t nil))))                                  ; non-matching: skip, keep reading

(defun att-confirm-indication (chan)
  "Answer an indication with a Handle Value Confirmation.

Not politeness. An indicating peer is required to wait for this before it may
send another, so a client that never confirms receives exactly one indication
and then silence -- which presents as a device that stopped talking rather
than as a protocol error."
  (att-send chan (let ((pdu (make-octets 1)))
                   (setf (aref pdu 0) +att-handle-value-cfm+)
                   pdu)))

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
CCCD-HANDLE. Signals on an ATT error.

INDICATIONS selects the confirmed variant. ATT-NEXT-NOTIFICATION returns
those alongside notifications and sends the required confirmation; until it
did, this flag wrote the right CCCD value and then stalled the peer after a
single indication, which is worse than not offering it."
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
  "Block for the next Handle Value Notification OR Indication on HANDLE and
return its value octets. NIL on timeout, on disconnect, or on any other PDU.

An indication is confirmed before returning. The two are deliberately not
distinguished in the return value: which one a device uses is its choice, the
payload means the same either way, and a caller made to handle both would end
up writing this function again."
  (let ((pdu (att-recv chan timeout-ms)))
    (when (and pdu (vectorp pdu) (>= (length pdu) 3)
               (= (u16-le pdu 1) handle))
      (let ((op (aref pdu 0)))
        (cond ((= op +att-handle-value-ntf+) (subseq pdu 3))
              ((= op +att-handle-value-ind+)
               (att-confirm-indication chan)
               (subseq pdu 3)))))))

(defun att-channel-close (chan)
  "Close an ATT channel, whichever transport it is: a kernel L2CAP socket, or
an HCI-CONN whose adapter has to be handed back to the kernel."
  (unregister-att-channel chan)
  (cond ((integerp chan) (%close chan))
        (chan (hci-conn-close chan))))


;;; --- services ---------------------------------------------------------

(defstruct gatt-service
  "One service found by discovery: the handle range it owns, and its UUID in
ATT wire order.

The range is the useful part. Characteristics belong to whichever service
encloses them, and ATT will not tell you that -- you discover characteristics
WITHIN a range, which is why ATT-DISCOVER-CHARACTERISTICS takes one."
  start end uuid)

(defun gatt-service-uuid-string (s) (uuid-string (gatt-service-uuid s)))

(defun att-discover-services (chan &key (type +gatt-primary-service+))
  "Walk Read By Group Type over the whole handle space. Returns a list of
GATT-SERVICE in handle order.

TYPE is +GATT-PRIMARY-SERVICE+ by default; pass +GATT-SECONDARY-SERVICE+ for
the ones only meant to be included by others."
  (let ((services nil) (start 1))
    (loop
      (let ((req (make-octets 7)))
        (setf (aref req 0) +att-read-by-group-type-req+)
        (u16le-put req 1 start)
        (u16le-put req 3 #xFFFF)
        (u16le-put req 5 type)
        (let ((rsp (att-request chan req :expect +att-read-by-group-type-rsp+)))
          (when (or (null rsp) (att-error-p rsp)
                    (/= (aref rsp 0) +att-read-by-group-type-rsp+))
            (return))
          ;; opcode(1) each-len(1), then records of each-len octets:
          ;;   start handle(2) end group handle(2) uuid(2 or 16)
          (let ((each (aref rsp 1)) (i 2) (last 0))
            (when (< each 6) (return))
            (loop while (<= (+ i each) (length rsp))
                  do (let ((group-end (u16-le rsp (+ i 2))))
                       (push (make-gatt-service :start (u16-le rsp i)
                                                :end group-end
                                                :uuid (subseq rsp (+ i 4) (+ i each)))
                             services)
                       (setf last group-end)
                       (incf i each)))
            ;; Two ways this walk can fail to end, and both hang rather than
            ;; error. A group ending at 0xFFFF is the last one there can be,
            ;; so asking again would wrap START back to 0. And a peer that
            ;; answers with handles at or below where we asked has not moved
            ;; us forward -- repeating the request would repeat the answer.
            ;; Neither is hypothetical: the second one was found by a test
            ;; whose stub responder simply returned the same reply twice.
            (when (or (>= last #xFFFF) (< last start)) (return))
            (setf start (1+ last))))))
    (nreverse services)))

(defun find-service-by-uuid (services uuid)
  "The GATT-SERVICE in SERVICES whose UUID matches, or NIL."
  (find (coerce-octets uuid) services :key #'gatt-service-uuid :test #'equalp))

(defun att-find-service (chan uuid &key (type +gatt-primary-service+))
  "Ask the peer for the handle range of the service with UUID, using Find By
Type Value. Returns a GATT-SERVICE, or NIL.

One round trip instead of walking every service on the device -- the peer
does the matching. What you want when you already know what you are after."
  (let* ((uuid (coerce-octets uuid))
         (req (make-octets (+ 7 (length uuid)))))
    (setf (aref req 0) +att-find-by-type-value-req+)
    (u16le-put req 1 1)
    (u16le-put req 3 #xFFFF)
    (u16le-put req 5 type)
    (replace req uuid :start1 7)
    (let ((rsp (att-request chan req :expect +att-find-by-type-value-rsp+)))
      (when (and rsp (vectorp rsp) (not (att-error-p rsp))
                 (= (aref rsp 0) +att-find-by-type-value-rsp+)
                 (>= (length rsp) 5))
        ;; Handles Information List: found handle(2), group end handle(2).
        (make-gatt-service :start (u16-le rsp 1)
                           :end (u16-le rsp 3)
                           :uuid uuid)))))

;;; --- descriptors ------------------------------------------------------

(defun att-discover-descriptors (chan start end)
  "Every attribute handle in [START, END] with its UUID, via Find Information.
Returns a list of (HANDLE . UUID-OCTETS) in handle order.

This is the raw attribute list -- characteristic declarations and values
included. Find Information does not filter and neither does this. For the
descriptors of one characteristic, ask over the range after its value handle."
  (let ((out nil) (next start))
    (loop
      (when (> next end) (return))
      (let ((req (make-octets 5)))
        (setf (aref req 0) +att-find-info-req+)
        (u16le-put req 1 next)
        (u16le-put req 3 end)
        (let ((rsp (att-request chan req :expect +att-find-info-rsp+)))
          (when (or (null rsp) (att-error-p rsp)
                    (/= (aref rsp 0) +att-find-info-rsp+)
                    (< (length rsp) 4))
            (return))
          (let* ((fmt (aref rsp 1))          ; 1 = 16-bit UUIDs, 2 = 128-bit
                 (step (if (= fmt 1) 4 18))
                 (i 2) (last 0))
            (loop while (<= (+ i step) (length rsp))
                  do (let ((handle (u16-le rsp i)))
                       (push (cons handle (subseq rsp (+ i 2) (+ i step))) out)
                       (setf last handle)
                       (incf i step)))
            (when (or (zerop last) (>= last end) (>= last #xFFFF) (< last next))
              (return))
            (setf next (1+ last))))))
    (nreverse out)))

;;; --- characteristics ---------------------------------------------------

(defun att-discover-characteristics (fd &key (start 1) (end #xFFFF))
  "Walk Read-By-Type (0x2803) and return a list of GATT-CHAR in handle order.

START and END bound the search. The whole handle space finds every
characteristic on the device, which is usually what you want; passing a
GATT-SERVICE's range is how you find the ones belonging to that service,
since ATT has no other notion of membership."
  (let ((chars nil))
    (loop
      (let ((req (make-octets 7)))
        (setf (aref req 0) +att-read-by-type-req+)
        (u16le-put req 1 start)
        (u16le-put req 3 end)
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
            ;; Stop at the end of the range, at the top of the handle space,
            ;; or the moment the peer stops moving us forward. See the note
            ;; in ATT-DISCOVER-SERVICES: without the last test a peer that
            ;; repeats itself loops here forever.
            (when (or (>= last end) (>= last #xFFFF) (< last start)) (return))
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
