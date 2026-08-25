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
(defconstant +ocf-le-set-phy+               #x0032)
(defconstant +ocf-read-remote-version+      #x001D)
(defconstant +ogf-status-params+            #x05)
(defconstant +ocf-read-rssi+                #x0005)

(defconstant +hci-remote-version-evt+       #x0C)
(defconstant +hci-command-complete-evt+     #x0E)
(defconstant +le-conn-update-complete-evt+  #x03)   ; LE Meta subevent
(defconstant +le-phy-update-complete-evt+   #x0C)   ; LE Meta subevent
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
      ;; Something else may already have pulled it off the socket.
      (let ((queued (hci-take-event conn :event event :subevent subevent)))
        (when queued (return queued)))
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
    ;; Anything of this type still queued belongs to an earlier
    ;; exchange and would be claimed as this command's answer.
    (hci-drop-events conn :event +hci-le-meta-evt+ :subevent +le-conn-update-complete-evt+)
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

(defun %phy-name (code)
  (case code (1 :1m) (2 :2m) (3 :coded) (t code)))

(defun %phy-bits (phys)
  "A PHY bitmask from a keyword, a list of them, or an integer already."
  (if (integerp phys)
      phys
      (let ((phys (if (listp phys) phys (list phys))))
        (reduce #'logior phys :initial-value 0
                :key (lambda (p) (ecase p (:1m #x01) (:2m #x02) (:coded #x04)))))))

(defun hci-set-phy (conn &key (tx :2m) (rx :2m) (coded-preference :none)
                              handle (timeout-ms 8000) (await t))
  "Ask to move an established connection onto another PHY.

TX and RX are :1M, :2M, :CODED, or a list of them meaning `any of these\'.
Returns (VALUES TX-PHY RX-PHY) as keywords once the change completes, or
:TIMEOUT, :DISCONNECTED, or the status octet.

This is not HCI-SET-DEFAULT-PHY, which only states a preference for future
negotiations and says nothing about a link already up. This asks for the
change now, on this connection.

The 2M PHY halves the air time of every packet, which on a link whose
throughput is bounded by how many packets fit in a connection event is worth
close to double. It carries no further and is not available on every
controller: LE Read Remote Features says whether the peer has it, and asking
a peer that does not is answered with the PHY it is already using rather than
an error, which is why the returned values are the ones to believe.

CODED-PREFERENCE is :NONE, :S2, or :S8, and matters only when moving to the
Coded PHY."
  (let ((sock (%conn-sock conn))
        (h (%conn-handle conn handle))
        (params (make-octets 7)))
    (u16le-put params 0 h)
    ;; All_PHYs = 0: we are naming both directions explicitly. A bit set here
    ;; means `no preference\', and setting it by accident hands the choice
    ;; back to the controller -- which then reports success having changed
    ;; nothing.
    (setf (aref params 2) 0
          (aref params 3) (%phy-bits tx)
          (aref params 4) (%phy-bits rx))
    (u16le-put params 5 (ecase coded-preference (:none 0) (:s2 1) (:s8 2)))
    (hci-drop-events conn :event +hci-le-meta-evt+
                          :subevent +le-phy-update-complete-evt+)
    (send-hci-command sock +ogf-le+ +ocf-le-set-phy+ params :name "LE Set PHY")
    (unless await (return-from hci-set-phy :requested))
    (let ((evt (%await-hci-event conn :event +hci-le-meta-evt+
                                      :subevent +le-phy-update-complete-evt+
                                      :timeout-ms timeout-ms)))
      (cond
        ((null evt) :timeout)
        ((eq evt :disconnected) :disconnected)
        ((< (length evt) 9) :timeout)
        ((/= (aref evt 4) 0) (aref evt 4))          ; HCI status
        (t (values (%phy-name (aref evt 7)) (%phy-name (aref evt 8))))))))

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
    ;; Anything of this type still queued belongs to an earlier
    ;; exchange and would be claimed as this command's answer.
    (hci-drop-events conn :event +hci-le-meta-evt+ :subevent +le-read-remote-features-evt+)
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
    ;; Anything of this type still queued belongs to an earlier
    ;; exchange and would be claimed as this command's answer.
    (hci-drop-events conn :event +hci-remote-version-evt+)
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
than whatever an advertisement happened to arrive at.

Returns the dBm value, :TIMEOUT, or the controller's status octet if it
refused.

The answer comes from SEND-HCI-COMMAND rather than from the connection's event
queue. This used to drop every queued Command Complete first and then wait for
a fresh one, which worked but was a guess dressed as a precaution: any other
command answered in the same window was indistinguishable from this one's. Now
the match is by opcode, and anything else read on the way is put back for
HCI-PUMP rather than dropped."
  (let ((sock (%conn-sock conn))
        (h (%conn-handle conn handle))
        (params (make-octets 2)))
    (u16le-put params 0 h)
    (let ((answer (handler-case
                      (send-hci-command sock +ogf-status-params+ +ocf-read-rssi+
                                        params :timeout-ms timeout-ms
                                               :name "Read RSSI")
                    ;; A refusal is this function's answer, not its caller's
                    ;; problem: the documented return already includes a status.
                    (hci-command-error (e) (hci-command-error-status e)))))
      (cond
        ((integerp answer) answer)              ; refused, with its status
        ((null answer) :timeout)
        (t (let ((rp (command-return-params answer)))
             ;; Return parameters are the handle (2 octets) then the RSSI,
             ;; which is signed.
             (if (and rp (>= (length rp) 3)) (s8 rp 2) :timeout)))))))
