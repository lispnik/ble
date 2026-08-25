(in-package #:ble)

;;; Raw HCI: sockets, controller commands, adapter enumeration, LE
;;; scanning, and advertising-report parsing.
;;;
;;; Uses a raw HCI socket (AF_BLUETOOTH, SOCK_RAW, BTPROTO_HCI) to send
;;; LE scan commands directly to the controller and read advertising
;;; reports back. Targets Linux/BlueZ on a Raspberry Pi.
;;;
;;; The firmware (radio/advertising.c) enables extended advertising with a
;;; configurable primary PHY (1M or Coded), so we configure extended LE
;;; scanning on both PHYs to be safe.
;;;
;;; Permissions: the SBCL binary needs CAP_NET_RAW + CAP_NET_ADMIN. Either
;;;   sudo setcap 'cap_net_raw,cap_net_admin+eip' $(readlink -f $(which sbcl))
;;; or run as root.

;;; Local helpers --------------------------------------------------------

;;; FORMAT-MAC now lives in the portable core (src/mac.lisp) alongside
;;; PARSE-MAC, so both byte-order conversions share one tested source.

;;; HCI constants --------------------------------------------------------

(defconstant +sol-hci+            0)
(defconstant +hci-filter+         2)
(defconstant +hci-channel-raw+    0)
(defconstant +hci-channel-user+   1)

(defconstant +hci-cmd-pkt+        #x01)
(defconstant +hci-event-pkt+      #x04)
(defconstant +hci-le-meta-evt+    #x3E)

(defconstant +sub-le-adv-report+      #x02)
(defconstant +sub-le-ext-adv-report+  #x0D)

(defconstant +ogf-le+ #x08)
(defconstant +ocf-le-set-extended-scan-parameters+ #x0041)
(defconstant +ocf-le-set-extended-scan-enable+     #x0042)

(defconstant +ogf-info-params+    #x04)
(defconstant +ocf-read-bd-addr+   #x0009)
(defconstant +ocf-le-set-default-phy+ #x0031)
(defconstant +hci-cmd-complete-evt+ #x0E)
;; Here rather than beside its friends in hci-conn.lisp because SEND-HCI-COMMAND
;; needs it and that file loads later -- a forward reference compiles to an
;; undefined variable and a warning nobody reads.
(defconstant +hci-cmd-status-evt+   #x0F)

(define-condition hci-command-error (ble-error)
  ((opcode :initarg :opcode :reader hci-command-error-opcode)
   (status :initarg :status :reader hci-command-error-status)
   (name   :initarg :name   :initform nil :reader hci-command-error-name))
  (:report (lambda (c s)
             (format s "HCI command ~@[~A ~]0x~4,'0X refused by the controller ~
                        (status 0x~2,'0X)"
                     (hci-command-error-name c)
                     (hci-command-error-opcode c)
                     (hci-command-error-status c))))
  (:documentation
   "The controller refused a command. Raised by SEND-HCI-COMMAND rather than
returned, because every caller of it was written on the assumption that
sending is the same as doing, and a return value they do not look at would
preserve exactly the silence this condition exists to break."))

(defun hci-opcode (ogf ocf)
  (logior ocf (ash ogf 10)))

;;; HCI socket -----------------------------------------------------------

(defstruct hci-socket
  "An open HCI socket: its fd, which adapter it is bound to, and the
controller's ACL data length once known.

ACL-LEN is kept here rather than only returned from OPEN-HCI-USER-SOCKET
because it is a property of the controller, and anything that wraps the
constructor -- WITH-HCI-USER-SOCKET, for one -- would otherwise drop it and
leave the caller unable to fragment correctly.

PENDING holds packets that a checked SEND-HCI-COMMAND read while looking for
its own Command Complete. They are not ours to discard -- on a live
connection they are ACL data and events that HCI-PUMP is waiting for -- so
they are put back here and READ-HCI-PACKET hands them out before reading the
socket again. Without it, checking a command status would quietly eat another
reader's packets, which is a bug this library has already had six times in
other guises."
  fd dev acl-len (pending nil))

(defun %hci-filter-bytes ()
  "The 16-octet struct hci_filter that lets every event packet through.
Split out from SET-HCI-FILTER so the layout can be asserted on: the kernel
sizes this struct at 16 and rejects any other length with EINVAL, so the two
bytes of tail padding are load-bearing rather than incidental."
  (let ((b (make-octets 16)))
    (dotimes (i 12) (setf (aref b i) #xFF))
    b))

(defun set-hci-filter (fd)
  "Allow event packets and all event codes through. struct hci_filter is
{uint32 type_mask; uint32 event_mask[2]; uint16 opcode;} which the kernel
sizes as 16 bytes (2 bytes of trailing pad), and setsockopt rejects any
other length with EINVAL."
  (cffi:with-foreign-object (filt :unsigned-char 16)
    (let ((bytes (%hci-filter-bytes)))
      (dotimes (i 16) (setf (cffi:mem-aref filt :unsigned-char i) (aref bytes i))))
    (check-syscall (%setsockopt fd +sol-hci+ +hci-filter+ filt 16)
                   "setsockopt HCI_FILTER")))

(defun open-hci-socket (&key (dev 0) (channel +hci-channel-raw+))
  "Open and bind a raw HCI socket on hci<DEV>. CHANNEL is +hci-channel-raw+
(default, shares the adapter with the kernel) or +hci-channel-user+
(exclusive control; the adapter must be DOWN first -- see
OPEN-HCI-USER-SOCKET in hci-conn.lisp)."
  (let ((fd (check-syscall
             (%socket +af-bluetooth+ +sock-raw+ +btproto-hci+) "socket")))
    (handler-case
        ;; struct sockaddr_hci { uint16 family; uint16 dev; uint16 channel; }
        (cffi:with-foreign-object (sa :unsigned-char 8)
          (loop for i below 8 do (setf (cffi:mem-aref sa :unsigned-char i) 0))
          (setf (cffi:mem-aref sa :unsigned-char 0) (logand +af-bluetooth+ #xFF)
                (cffi:mem-aref sa :unsigned-char 1) (logand (ash +af-bluetooth+ -8) #xFF)
                (cffi:mem-aref sa :unsigned-char 2) (logand dev #xFF)
                (cffi:mem-aref sa :unsigned-char 3) (logand (ash dev -8) #xFF)
                (cffi:mem-aref sa :unsigned-char 4) channel
                (cffi:mem-aref sa :unsigned-char 5) 0)
          (check-syscall (%bind fd sa 6) "bind")
          (when (= channel +hci-channel-raw+)
            (set-hci-filter fd))
          (make-hci-socket :fd fd :dev dev))
      (error (c)
        (%close fd)
        (error c)))))

(defun close-hci-socket (sock)
  (when (hci-socket-fd sock)
    (%close (hci-socket-fd sock))
    (setf (hci-socket-fd sock) nil)))

(defparameter *hci-command-timeout-ms* 1000
  "How long a checked SEND-HCI-COMMAND waits for the controller to answer.")

(defun %command-answer-p (pkt opcode)
  "Is PKT this command's Command Complete or Command Status?

The two have different shapes and both must be recognised. A Command Complete
carries the opcode at offsets 4-5 and the status at 6; a Command Status
carries the status at 3 and the opcode at 5-6. Commands that take time --
creating a connection, disconnecting, reading a remote feature -- answer with
Status and deliver their result later as an event, so a checker that knew only
about Complete would wait out its timeout on exactly the commands most worth
checking."
  (and (>= (length pkt) 7)
       (= (aref pkt 0) +hci-event-pkt+)
       (or (and (= (aref pkt 1) +hci-cmd-complete-evt+) (= (u16-le pkt 4) opcode))
           (and (= (aref pkt 1) +hci-cmd-status-evt+) (= (u16-le pkt 5) opcode)))))

(defun command-answer-status (pkt)
  "The status octet of a Command Complete or Command Status packet."
  (if (= (aref pkt 1) +hci-cmd-status-evt+) (aref pkt 3) (aref pkt 6)))

(defun command-return-params (pkt)
  "The return parameters of a Command Complete, after the status octet.
NIL for a Command Status, which has none."
  (when (and (= (aref pkt 1) +hci-cmd-complete-evt+) (> (length pkt) 7))
    (subseq pkt 7)))

(defun send-hci-command (sock ogf ocf params
                         &key (check t) (timeout-ms *hci-command-timeout-ms*)
                              name)
  "Build and write a single HCI command packet, and by default check that the
controller accepted it.

Returns the Command Complete or Command Status packet, or NIL if none arrived
within TIMEOUT-MS. Signals HCI-COMMAND-ERROR when the controller answers with
a non-zero status.

CHECK defaults to true because the alternative is what this library did for a
long time: write the command and walk away. A controller that refuses one --
because a parameter is out of range, or the command is not supported, or the
adapter is in the wrong state -- says so, and nobody was listening. The
symptom is a device that configures itself successfully and then does nothing,
with no error anywhere to suggest why.

Worth being exact about what this does and does not buy. It catches a
controller saying no. It cannot catch a controller saying yes to something
that was not what you meant: an advertising set was once configured, enabled
and invisible for an hour of debugging here, and every one of its commands had
returned success -- the fault was in how they were sequenced, not in any of
them. What the checking would have saved there was the throwaway diagnostic
written to rule this out, which is worth something, but not the diagnosis.

Pass :CHECK NIL where a caller wants to read the answer itself, or where
there is no answer to read."
  (let* ((opcode (hci-opcode ogf ocf))
         (params (coerce-octets params))
         (plen (length params))
         (pkt (make-octets (+ 4 plen))))
    (setf (aref pkt 0) +hci-cmd-pkt+)
    (u16le-put pkt 1 opcode)
    (setf (aref pkt 3) plen)
    (replace pkt params :start1 4)
    (cffi:with-foreign-object (buf :unsigned-char (length pkt))
      (bytes-to-foreign pkt buf)
      (check-syscall (%write (hci-socket-fd sock) buf (length pkt)) "write"))
    (when check
      (let ((deadline (+ (get-internal-real-time)
                         (round (* timeout-ms internal-time-units-per-second)
                                1000))))
        (loop
          (let ((remaining (round (* 1000 (- deadline (get-internal-real-time)))
                                  internal-time-units-per-second)))
            (when (<= remaining 0) (return nil))
            (let ((answer (read-hci-packet sock :timeout-ms (min 200 remaining))))
              (cond
                ((null answer))            ; nothing yet; the deadline decides
                ((%command-answer-p answer opcode)
                 (let ((status (command-answer-status answer)))
                   (unless (zerop status)
                     (error 'hci-command-error :opcode opcode :status status
                                               :name name))
                   (return answer)))
                ;; Not ours. Put it back for whoever it belongs to.
                (t (setf (hci-socket-pending sock)
                         (nconc (hci-socket-pending sock) (list answer))))))))))))

(defparameter *hci-read-buffer-size* 2048)

(defun read-hci-packet (sock &key timeout-ms)
  "Read one HCI packet. Returns an octet vector, or NIL on EOF or timeout.

With TIMEOUT-MS the read is bounded by a poll first, which is what lets a
caller stop scanning on a deadline instead of blocking until the next packet
happens to arrive. Without it the read blocks, which is the historic
behaviour and still what an endless scan loop wants."
  ;; Anything a checked command read and put back comes out first, in order,
  ;; and without consulting the timeout: it has already arrived.
  (when (hci-socket-pending sock)
    (return-from read-hci-packet (pop (hci-socket-pending sock))))
  (when (and timeout-ms (not (fd-readable-p (hci-socket-fd sock) timeout-ms)))
    (return-from read-hci-packet nil))
  (cffi:with-foreign-object (buf :unsigned-char *hci-read-buffer-size*)
    (let ((n (%read (hci-socket-fd sock) buf *hci-read-buffer-size*)))
      (cond ((= n 0) nil)
            ((< n 0) (error "read failed: ~A (errno ~D)"
                            (%strerror (errno)) (errno)))
            (t (foreign-to-bytes buf n))))))

(defun hci-read-bd-addr (&key (dev 0) sock)
  "The controller's own BD_ADDR, on-air byte order."
  (let* ((own-socket (null sock))
         (sock (or sock (open-hci-socket :dev dev))))
    (unwind-protect
         ;; The address is this command's return parameters, and
         ;; SEND-HCI-COMMAND already waited for them and checked the status --
         ;; so there is no second read here and no second status check.
         (let ((params (command-return-params
                        (or (send-hci-command sock +ogf-info-params+
                                              +ocf-read-bd-addr+ #()
                                              :name "Read_BD_ADDR"
                                              :timeout-ms 2000)
                            (error "no response to Read_BD_ADDR on hci~D" dev)))))
           (unless (and params (>= (length params) 6))
             (error "Read_BD_ADDR on hci~D returned no address" dev))
           (subseq params 0 6))
      ;; Only close what we opened; a caller's socket is not ours to shut.
      (when own-socket (close-hci-socket sock)))))

(defun hci-set-default-phy (&key (dev 0) (tx-phys #x05) (rx-phys #x05))
  "Issue HCI LE Set Default PHY on hci<DEV>. TX-PHYS / RX-PHYS are bitmasks
(bit0 = 1M, bit1 = 2M, bit2 = Coded); default #x05 = 1M + Coded. all_phys
is 0 (we name both TX and RX explicitly). Returns T on success.

This sets the controller's default PHY preference for the PHY Update
procedure. It says nothing about which PHY a connection is initiated on --
that is chosen per connection by LE Extended Create Connection.

A controller that does not support the Coded PHY refuses this outright, and
SEND-HCI-COMMAND turns that refusal into a condition rather than letting the
caller believe it now prefers a PHY it cannot use."
  (let ((sock (open-hci-socket :dev dev)))
    (unwind-protect
         (progn (send-hci-command sock +ogf-le+ +ocf-le-set-default-phy+
                                  (vector 0 tx-phys rx-phys)
                                  :name "LE Set Default PHY" :timeout-ms 2000)
                t)
      (close-hci-socket sock))))

;;; Adapter enumeration ---------------------------------------------------
;;;
;;; Which hciN is which MATTERS here and cannot be assumed: a device
;;; advertises on Coded PHY only, which the Pi's built-in UART radio cannot
;;; receive at all, and the kernel's hciN numbering drifts across reboots. So
;;; callers should pick an adapter by BUS (USB dongle) rather than by index.
;;;
;;; Read from sysfs rather than via ioctl(HCIGETDEVLIST): no extra foreign
;;; structs, works on a downed adapter, and needs no privileges. Linux only --
;;; everywhere else this simply returns NIL.

(defstruct hci-adapter
  index        ; N in hciN
  bus          ; :usb | :serial | :other
  product      ; USB product string, or NIL
  address)     ; BD_ADDR (on-air order) if it could be read, else NIL

(defun hci-adapter-usb-p (a) (eq (hci-adapter-bus a) :usb))

(defparameter +sysfs-bluetooth+ #p"/sys/class/bluetooth/")

(defun %adapter-bus (index)
  "Bus this adapter hangs off, from the subsystem symlink of its device."
  (let ((link (ignore-errors
               (truename (merge-pathnames
                          (format nil "hci~D/device/subsystem" index)
                          +sysfs-bluetooth+)))))
    (cond ((null link) :other)
          ((search "usb" (namestring link)) :usb)
          ((search "serial" (namestring link)) :serial)
          (t :other))))

(defun %adapter-product (index)
  "USB product string, read from the parent of the interface directory (the
interface itself doesn't carry one). NIL for non-USB or unreadable."
  (let ((dev (ignore-errors
              (truename (merge-pathnames (format nil "hci~D/device/" index)
                                         +sysfs-bluetooth+)))))
    (when dev
      (let ((file (merge-pathnames "../product" dev)))
        (ignore-errors
         (string-trim '(#\Space #\Newline #\Return)
                      (uiop:read-file-string file)))))))

(defun list-hci-adapters (&key (max-index 15) (read-address t))
  "Enumerate local HCI adapters, lowest index first. READ-ADDRESS also asks
each controller for its BD_ADDR, which needs CAP_NET_RAW and can fail on a
busy or downed adapter -- failure just leaves ADDRESS NIL."
  (loop for i from 0 to max-index
        when (probe-file (merge-pathnames (format nil "hci~D/uevent" i)
                                          +sysfs-bluetooth+))
          collect (make-hci-adapter
                   :index i
                   :bus (%adapter-bus i)
                   :product (%adapter-product i)
                   :address (and read-address
                                 (ignore-errors (hci-read-bd-addr :dev i))))))

(defun default-hci-dev (&optional adapters (prefer :usb))
  "Index of the adapter to use when the caller didn't name one.

PREFER :USB picks the first USB dongle -- the right rule when you need the
Coded PHY, which built-in radios generally cannot receive at all. PREFER :LOWEST picks the lowest index present, which is the right rule
for an ordinary 1M-PHY peripheral and is not merely the lazy option: on the
development Pi the built-in radio is the only one that can hear some devices,
while both USB dongles report nothing.

Which rule is correct depends on the device you are looking for, so it is a
parameter rather than a default someone has to remember to override."
  (let ((adapters (sort (copy-list (or adapters (list-hci-adapters :read-address nil)))
                        #'< :key #'hci-adapter-index)))
    (ecase prefer
      (:usb    (let ((usb (find-if #'hci-adapter-usb-p adapters)))
                 (cond (usb      (hci-adapter-index usb))
                       (adapters (hci-adapter-index (first adapters)))
                       (t 0))))
      (:lowest (if adapters (hci-adapter-index (first adapters)) 0)))))

(defun hci-adapter-label (a &key (address t))
  "Human-readable one-liner for an adapter, e.g.
\"hci1 - TP-TP+ Bluetooth USB Adapter (USB) A1:B2:C3:D4:E5:F6\".

With :ADDRESS NIL the BD_ADDR is left off. That form is for UI widgets: the
address roughly doubles the width, and in the web GUI's adapter picker it was
wide enough to wrap the whole control bar onto a second row."
  (format nil "hci~D - ~A (~A)~@[ ~A~]"
          (hci-adapter-index a)
          (or (hci-adapter-product a)
              (case (hci-adapter-bus a)
                (:serial "built-in UART radio")
                (t "adapter")))
          (case (hci-adapter-bus a) (:usb "USB") (:serial "UART") (t "?"))
          (and address (hci-adapter-address a)
               (format-mac (hci-adapter-address a)))))

;;; Advertising-report parsing -------------------------------------------

(defun decode-rssi (byte)
  "Signed dBm from an HCI RSSI byte, or NIL when the controller says it has
none. 0x7F is the spec's \"RSSI is not available\" sentinel; taken literally
it would render as a nonsensical +127 dBm, and every consumer here already
handles a missing RSSI (replay has none either)."
  (let ((v (if (>= byte 128) (- byte 256) byte)))
    (unless (= v 127) v)))

(defstruct adv-report
  event-type  ; uint8 (legacy) or uint16 bitfield (extended)
  addr-type   ; 0 = public, 1 = random, ...
  address     ; 6-byte octet vector, on-air byte order (matches what the
              ; firmware passes to ble_gap_addr_t for encryption)
  data        ; advertising data blob (sequence of AD records)
  rssi)       ; signed dBm or NIL

(defun parse-le-adv-report (params)
  "Legacy LE Advertising Report sub-event (0x02). Per-report fixed = 9 bytes
plus data; RSSIs (one signed byte per report) come at the very end."
  (let ((num (aref params 0))
        (offset 1)
        (reports nil))
    (dotimes (_ num)
      (let* ((event-type (aref params offset))
             (addr-type  (aref params (+ offset 1)))
             (address    (subseq params (+ offset 2) (+ offset 8)))
             (data-len   (aref params (+ offset 8)))
             (data       (subseq params (+ offset 9) (+ offset 9 data-len))))
        (push (make-adv-report :event-type event-type
                               :addr-type addr-type
                               :address address
                               :data data)
              reports)
        (incf offset (+ 9 data-len))))
    (let ((reports (nreverse reports)))
      (loop for r in reports for i from 0
            for byte = (aref params (+ offset i))
            do (setf (adv-report-rssi r) (decode-rssi byte)))
      reports)))

(defun parse-le-ext-adv-report (params)
  "Extended LE Advertising Report sub-event (0x0D). Per-report fixed = 24
bytes including RSSI inline, then data."
  (let ((num (aref params 0))
        (offset 1)
        (reports nil))
    (dotimes (_ num)
      (let* ((event-type (u16-le params offset))
             (addr-type  (aref params (+ offset 2)))
             (address    (subseq params (+ offset 3) (+ offset 9)))
             (rssi       (decode-rssi (aref params (+ offset 13))))
             (data-len   (aref params (+ offset 23)))
             (data       (subseq params (+ offset 24) (+ offset 24 data-len))))
        (push (make-adv-report :event-type event-type
                               :addr-type addr-type
                               :address address
                               :data data
                               :rssi rssi)
              reports)
        (incf offset (+ 24 data-len))))
    (nreverse reports)))

;;; Extended scan setup --------------------------------------------------
;;;
;;; LE Set Extended Scan Parameters (Core spec v5.0, Vol 4, Part E §7.8.64):
;;;   own_addr_type       1 byte
;;;   scan_filter_policy  1 byte
;;;   scanning_phys       1 byte (bit 0 = 1M, bit 2 = Coded)
;;;   per enabled PHY:
;;;     scan_type         1 byte (0 passive, 1 active)
;;;     scan_interval     2 bytes (units of 0.625 ms)
;;;     scan_window       2 bytes
;;;
;;; LE Set Extended Scan Enable (§7.8.65):
;;;   enable              1 byte
;;;   filter_duplicates   1 byte
;;;   duration            2 bytes (0 = until disabled)
;;;   period              2 bytes

(defun start-extended-scan (sock)
  (let ((params (make-octets 13)))
    (setf (aref params 0) 0          ; own_addr_type = public
          (aref params 1) 0          ; scan_filter_policy = accept all
          (aref params 2) #x05)      ; scanning_phys = 1M | Coded
    ;; 1M PHY: active scan, 10 ms interval, 10 ms window
    (setf (aref params 3) 1)
    (u16le-put params 4 #x0010)
    (u16le-put params 6 #x0010)
    ;; Coded PHY: same
    (setf (aref params 8) 1)
    (u16le-put params 9 #x0010)
    (u16le-put params 11 #x0010)
    (send-hci-command sock +ogf-le+ +ocf-le-set-extended-scan-parameters+ params))
  (let ((params (make-octets 6)))
    (setf (aref params 0) 1)         ; enable = 1, others = 0
    (send-hci-command sock +ogf-le+ +ocf-le-set-extended-scan-enable+ params)))

(defconstant +ocf-le-set-scan-parameters+ #x000B)
(defconstant +ocf-le-set-scan-enable+     #x000C)

(defun start-le-scan (sock &key (active t) (interval #x0010) (window #x0010))
  "Legacy LE Set Scan Parameters + Set Scan Enable (4.0).

Deliberately kept alongside the extended form. The extended commands are the
only way to scan the Coded PHY, but they are not implemented by every
controller -- notably some built-in radios -- and an ordinary legacy
advertiser on the 1M PHY needs nothing more than this. ACTIVE requests scan
responses, which is where many devices put their name."
  (let ((params (make-octets 7)))
    (setf (aref params 0) (if active 1 0))
    (u16le-put params 1 interval)
    (u16le-put params 3 window)
    (setf (aref params 5) 0             ; own address type: public
          (aref params 6) 0)            ; filter policy: accept all
    (send-hci-command sock +ogf-le+ +ocf-le-set-scan-parameters+ params))
  (let ((params (make-octets 2)))
    (setf (aref params 0) 1             ; enable
          (aref params 1) 0)            ; do not filter duplicates
    (send-hci-command sock +ogf-le+ +ocf-le-set-scan-enable+ params)))

(defun stop-le-scan (sock)
  (ignore-errors
   (send-hci-command sock +ogf-le+ +ocf-le-set-scan-enable+ (make-octets 2))))

(defun stop-extended-scan (sock)
  (ignore-errors
   (send-hci-command sock +ogf-le+ +ocf-le-set-extended-scan-enable+
                     (make-octets 6))))

;;; --- advertising reports out of an HCI packet -------------------------

(defun reports-from-packet (pkt)
  "Extract any LE Advertising Reports embedded in an HCI packet."
  (when (and (>= (length pkt) 4)
             (= (aref pkt 0) +hci-event-pkt+)
             (= (aref pkt 1) +hci-le-meta-evt+))
    (let* ((param-len (aref pkt 2))
           (subevt    (aref pkt 3))
           (params    (subseq pkt 4 (+ 3 param-len))))
      (cond ((= subevt +sub-le-adv-report+)     (parse-le-adv-report params))
            ((= subevt +sub-le-ext-adv-report+) (parse-le-ext-adv-report params))
            (t nil)))))

;;; --- generic scanning --------------------------------------------------
;;;
;;; SCAN-REPORTS rather than SCAN, and the extra word is load-bearing. A
;;; consumer package that USEs #:ble and defines its own `scan' -- a
;;; protocol-aware one that only dispatches its own devices -- would not
;;; shadow a `scan' exported from here. It would silently redefine it. The
;;; plainer name is left free for consumers to take.

(defun scan-reports (callback &key (dev 0) seconds max-reports (extended t))
  "Run an LE scan on hci<DEV>, calling CALLBACK with each ADV-REPORT.

Stops after SECONDS or MAX-REPORTS, whichever comes first; with neither, runs
until CALLBACK performs a non-local exit. EXTENDED selects the 5.0 scan
commands (needed for Coded PHY) or the legacy 4.0 pair. Returns the number of
reports delivered."
  (let ((sock (open-hci-socket :dev dev))
        (deadline (when seconds
                    (+ (get-internal-real-time)
                       (round (* seconds internal-time-units-per-second)))))
        (count 0))
    (unwind-protect
         (progn
           (if extended (start-extended-scan sock) (start-le-scan sock))
           (loop
             (when (and deadline (>= (get-internal-real-time) deadline)) (return))
             (dolist (r (reports-from-packet (read-hci-packet sock :timeout-ms 250)))
               (funcall callback r)
               (incf count)
               (when (and max-reports (>= count max-reports)) (return)))
             (when (and max-reports (>= count max-reports)) (return))))
      (if extended (stop-extended-scan sock) (stop-le-scan sock))
      (close-hci-socket sock))
    count))

(defstruct discovered
  "One device seen during DISCOVER, merged across all of its reports."
  address addr-type name rssi service-uuids)

(defun discover (&key (dev 0) (seconds 8) (extended t) filter)
  "Scan for SECONDS and return a list of DISCOVERED devices, strongest first.

Reports for one address are merged, which is the point: a name learned from a
scan response arrives in a different report from the advertisement carrying
the service UUIDs, and a caller that treats reports as devices sees each
device several times, differently, and never completely.

FILTER, if given, is a predicate on a DISCOVERED."
  (let ((table (make-hash-table :test #'equalp)))
    (scan-reports
     (lambda (r)
       (let* ((key (adv-report-address r))
              (d (or (gethash key table)
                     (setf (gethash key table)
                           (make-discovered :address key
                                            :addr-type (adv-report-addr-type r))))))
         (let ((name (adv-local-name (adv-report-data r))))
           (when (and name (plusp (length name))) (setf (discovered-name d) name)))
         (dolist (u (adv-service-uuids-16 (adv-report-data r)))
           (pushnew u (discovered-service-uuids d)))
         (when (adv-report-rssi r) (setf (discovered-rssi d) (adv-report-rssi r)))))
     :dev dev :seconds seconds :extended extended)
    (let ((all (sort (loop for d being the hash-values of table collect d)
                     #'> :key (lambda (d) (or (discovered-rssi d) -999)))))
      (if filter (remove-if-not filter all) all))))
