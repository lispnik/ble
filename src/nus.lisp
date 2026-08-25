(in-package #:ble)

;;; The Nordic UART Service: serial bytes over GATT.
;;;
;;; A profile, and a thin one -- two characteristics and a subscription. All
;;; the work is in the ATT layer in src/att.lisp, which this uses exactly as
;;; any other profile would; there is nothing privileged about it.
;;;
;;; It is here because a great many hobbyist and vendor peripherals expose it
;;; as their transport -- anything built on Nordic's stack tends to -- so
;;; having it ready saves rewriting the same forty lines.
;;;
;;;   Service 6E400001-B5A3-F393-E0A9-E50E24DCCA9E
;;;   RX      6E400002-...  Write / Write-Without-Response: host -> device
;;;   TX      6E400003-...  Notify:                        device -> host
;;;
;;; The direction names are from the DEVICE's point of view, which is the
;;; usual trap: a host WRITES to RX and READS notifications from TX.

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
    ;; Via ATT-CHANNEL-CLOSE, which also drops it from *OPEN-ATT-CHANNELS*.
    ;; Closing the fd directly left a stale entry there, and an fd number is
    ;; reused -- so the exit hook would later close whatever now held it.
    (when chan (att-channel-close chan))
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
