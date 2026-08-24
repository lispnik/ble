(in-package #:ble)

;;; Asking about, and changing, an established connection.
;;;
;;; All of these drive the controller directly, so they need the
;;; HCI_CHANNEL_USER transport -- an HCI-CONN, not a kernel L2CAP socket. With
;;; the kernel path we do not have the controller and cannot issue commands on
;;; it; BlueZ owns those decisions and exposes them over D-Bus instead.
;;;
;;; The connection interval is the single knob that trades latency against
;;; power on an established link. A peripheral picks the parameters it wants
;;; when it advertises, but the central is the one that sets them, so this is
;;; how you shorten a link that is answering too slowly or lengthen one that
;;; is draining a battery.

(defconstant +ocf-le-connection-update+     #x0013)
(defconstant +ocf-le-read-remote-features+  #x0016)
(defconstant +ocf-read-remote-version+      #x001D)
(defconstant +ogf-status-params+            #x05)
(defconstant +ocf-read-rssi+                #x0005)

(defconstant +hci-remote-version-evt+       #x0C)
(defconstant +hci-command-complete-evt+     #x0E)
(defconstant +le-conn-update-complete-evt+  #x03)   ; LE Meta subevent
(defconstant +le-read-remote-features-evt+  #x04)   ; LE Meta subevent

;;; Interval and timeout are carried in the controller's own units, which are
;;; a well-known source of quiet mistakes -- 0x0018 is 30 ms, not 24 ms, and a
;;; supervision timeout of 500 is five seconds. These take milliseconds and
;;; convert, so the units live in one place instead of at every call site.

(defun ms-to-interval-units (ms) (max 6 (min #x0C80 (round ms 5/4))))
(defun interval-units-to-ms (units) (/ (* units 5) 4))
(defun ms-to-timeout-units (ms) (max 10 (min #x0C80 (round ms 10))))
(defun timeout-units-to-ms (units) (* units 10))

(defun %conn-sock (conn)
  (etypecase conn
    (hci-conn (hci-conn-sock conn))
    (hci-socket conn)))

(defun %conn-handle (conn &optional handle)
  (or handle (etypecase conn (hci-conn (hci-conn-handle conn)))))

(defun %await-hci-event (conn &key event subevent (timeout-ms 5000))
  "Wait for one HCI event, keeping everything else that arrives meanwhile.

Reads through HCI-PUMP, so ACL data is filed rather than read past: a
connection carrying notifications does not stop carrying them because the host
asked the controller a question. Notifications are then moved to the
notification queue and anything else -- the response to a request already in
flight, for instance -- is left in PENDING where the ATT layer will look for
it. Taking the lot was a real bug: it presented as a long write timing out for
no visible reason."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms internal-time-units-per-second) 1000))))
    (loop
      (let ((remaining (- deadline (get-internal-real-time))))
        (when (<= remaining 0) (return nil))
        (let ((r (hci-pump conn (max 1 (round (* remaining 1000)
                                              internal-time-units-per-second)))))
          (cond
            ((eq r :disconnected) (return :disconnected))
            ((eq r :data)
             (let ((keep '()))
               (loop for pdu = (pop (hci-conn-pending conn))
                     while pdu
                     do (unless (%att-note-server-pdu conn pdu) (push pdu keep)))
               (setf (hci-conn-pending conn)
                     (nconc (nreverse keep) (hci-conn-pending conn)))))
            ((and (vectorp r) event (>= (length r) 2) (= (aref r 1) event)
                  (or (null subevent)
                      (and (>= (length r) 4) (= (aref r 3) subevent))))
             (return r))))))))


(defun hci-connection-update (conn &key (min-interval-ms 30) (max-interval-ms 50)
                                        (latency 0) (supervision-timeout-ms 4000)
                                        (min-ce 0) (max-ce 0) handle
                                        (timeout-ms 8000) (await t))
  "Ask the controller to renegotiate the connection parameters.

Returns (VALUES INTERVAL-MS LATENCY TIMEOUT-MS) once the peer agrees, or
:TIMEOUT, :DISCONNECTED, or the HCI status octet if the controller refused.

The values that come back are the ones in force, which need not be the ones
asked for: this is a request to the peer, and it may answer with anything
inside the range. Using the requested numbers afterwards rather than the
returned ones is how a link ends up being driven at an interval it is not
running at."
  (let ((sock (%conn-sock conn))
        (h (%conn-handle conn handle))
        (params (make-octets 14)))
    (u16le-put params 0 h)
    (u16le-put params 2 (ms-to-interval-units min-interval-ms))
    (u16le-put params 4 (ms-to-interval-units max-interval-ms))
    (u16le-put params 6 latency)
    (u16le-put params 8 (ms-to-timeout-units supervision-timeout-ms))
    (u16le-put params 10 min-ce)
    (u16le-put params 12 max-ce)
    (send-hci-command sock +ogf-le+ +ocf-le-connection-update+ params)
    ;; AWAIT NIL issues the command and returns. The completion event arrives
    ;; whenever the peer gets round to it, and a caller that is inside its own
    ;; read loop -- answering a peer's request, say -- must not stall there
    ;; for it: everything else on the link stops while it does.
    (unless await (return-from hci-connection-update :requested))
    (let ((evt (%await-hci-event conn :event +hci-le-meta-evt+
                                      :subevent +le-conn-update-complete-evt+
                                      :timeout-ms timeout-ms)))
      (cond
        ((null evt) :timeout)
        ((eq evt :disconnected) :disconnected)
        ((< (length evt) 13) :timeout)
        ((/= (aref evt 4) 0) (aref evt 4))       ; HCI status
        (t (values (interval-units-to-ms (u16-le evt 7))
                   (u16-le evt 9)
                   (timeout-units-to-ms (u16-le evt 11))))))))

(defun hci-read-remote-features (conn &key handle (timeout-ms 5000))
  "The peer's LE feature bitmap, as 8 octets. :TIMEOUT, :DISCONNECTED, or the
status octet on failure.

Worth asking before assuming a peer can do something: whether it supports the
Coded PHY, a longer packet, or a 2M PHY is in here, and a controller told to
use a feature the peer lacks fails in ways that look like interference."
  (let ((sock (%conn-sock conn))
        (h (%conn-handle conn handle))
        (params (make-octets 2)))
    (u16le-put params 0 h)
    (send-hci-command sock +ogf-le+ +ocf-le-read-remote-features+ params)
    (let ((evt (%await-hci-event conn :event +hci-le-meta-evt+
                                      :subevent +le-read-remote-features-evt+
                                      :timeout-ms timeout-ms)))
      (cond
        ((null evt) :timeout)
        ((eq evt :disconnected) :disconnected)
        ((< (length evt) 15) :timeout)
        ((/= (aref evt 4) 0) (aref evt 4))
        (t (subseq evt 7 15))))))

(defun hci-read-remote-version (conn &key handle (timeout-ms 5000))
  "(VALUES VERSION MANUFACTURER SUBVERSION) for the peer's controller.

VERSION is the Bluetooth core specification number the controller implements
-- 0x0A is 5.1, 0x0B is 5.2 -- and MANUFACTURER is the assigned company
identifier."
  (let ((sock (%conn-sock conn))
        (h (%conn-handle conn handle))
        (params (make-octets 2)))
    (u16le-put params 0 h)
    (send-hci-command sock +ogf-link-ctl+ +ocf-read-remote-version+ params)
    (let ((evt (%await-hci-event conn :event +hci-remote-version-evt+
                                      :timeout-ms timeout-ms)))
      (cond
        ((null evt) :timeout)
        ((eq evt :disconnected) :disconnected)
        ((< (length evt) 11) :timeout)
        ((/= (aref evt 3) 0) (aref evt 3))
        (t (values (aref evt 6) (u16-le evt 7) (u16-le evt 9)))))))

(defun hci-read-rssi (conn &key handle (timeout-ms 3000))
  "The last received signal strength on this connection, in dBm.

A connection RSSI, not an advertising one: it is measured on packets from a
peer that is already connected, so it reflects the link actually in use rather
than whatever an advertisement happened to arrive at."
  (let ((sock (%conn-sock conn))
        (h (%conn-handle conn handle))
        (params (make-octets 2)))
    (u16le-put params 0 h)
    (send-hci-command sock +ogf-status-params+ +ocf-read-rssi+ params)
    (let ((evt (%await-hci-event conn :event +hci-command-complete-evt+
                                      :timeout-ms timeout-ms)))
      (cond
        ((null evt) :timeout)
        ((eq evt :disconnected) :disconnected)
        ((< (length evt) 10) :timeout)
        ((/= (aref evt 6) 0) (aref evt 6))       ; status
        ;; The RSSI octet is signed -- see S8.
        (t (s8 evt 9))))))
