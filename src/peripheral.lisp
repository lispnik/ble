(in-package #:ble)

;;; Being a peripheral: accepting a connection, and serving it.
;;;
;;; The GATT server answers requests and the advertiser puts the device on the
;;; air, but between them sits a lifecycle that every peripheral needs and that
;;; was, until this file, written out by hand each time -- inside this package,
;;; because turning an accepted connection into something GATT-SERVE takes
;;; needed an unexported constructor. That made a peripheral something only
;;; this library could write, which was not the intention.
;;;
;;; Two things here are less obvious than they look, and both cost real time
;;; before they were understood:
;;;
;;; ADVERTISING STOPS THE MOMENT A CENTRAL CONNECTS. That is the specification,
;;; not a fault. A peripheral that does not re-enable it after a disconnect
;;; silently vanishes -- the process looks healthy, the adapter is up, and the
;;; device is simply not there any more.
;;;
;;; A DISCONNECT IS NOT A QUEUED EVENT. HCI-PUMP reports it as :DISCONNECTED
;;; and files nothing, so a loop that ignores the pump's return value never
;;; learns the peer has gone. It then waits forever for a client that left.

(defun peripheral-accept (sock &key (timeout-ms 60000))
  "Wait for a central to connect to this adapter.

Returns (VALUES CONN PEER-ADDR PEER-ADDR-TYPE), or NIL on timeout. SOCK must
already be advertising connectably -- see SET-ADV-PARAMETERS and
WITH-ADVERTISING.

Accepts both the plain and the Enhanced Connection Complete subevents. Which
one a controller sends depends on its event mask, the peer address sits at the
same offset in both, and handling only the first is a bug that presents as a
peripheral nobody can connect to."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms internal-time-units-per-second) 1000))))
    (loop
      (when (<= (- deadline (get-internal-real-time)) 0) (return nil))
      (let ((pkt (hci-poll-read sock 200)))
        (when (and pkt (>= (length pkt) 15)
                   (= (aref pkt 0) #x04) (= (aref pkt 1) +hci-le-meta-evt+)
                   (member (aref pkt 3) '(#x01 #x0A))
                   (zerop (aref pkt 4)))
          (return
            (values (make-hci-conn :sock sock
                                   :handle (u16-le pkt 5)
                                   :acl-len (hci-socket-acl-len sock))
                    (subseq pkt 9 15)
                    (if (= 1 (aref pkt 8)) :random :public))))))))

(defun serve-peripheral (server sock &key on-connect on-tick on-disconnect
                                          (accept-timeout-ms 60000)
                                          seconds (tick-ms 50))
  "Advertise, accept a central, serve GATT to it, and go back to advertising
when it leaves. Runs until SECONDS elapses, or forever when SECONDS is NIL.

The hooks are where a peripheral does its own work:

  ON-CONNECT     (conn peer-addr peer-addr-type) once per connection
  ON-TICK        (conn request) between polls -- where a sensor notifies.
                 REQUEST is the ATT opcode just answered, or NIL if nothing
                 arrived, so a peripheral can see what it is being asked for
                 without reading the socket itself.
  ON-DISCONNECT  (conn) after the peer leaves, before advertising resumes

Reads only through the connection once one exists. Polling the socket
separately as well makes two readers race for the same packets, and whichever
loses simply never sees them."
  (let ((deadline (and seconds (+ (get-internal-real-time)
                                  (* seconds internal-time-units-per-second))))
        (conn nil))
    (unwind-protect
         (loop
           (when (and deadline (> (get-internal-real-time) deadline)) (return))
           (cond
             ((null conn)
              (set-adv-enable sock t)
              (multiple-value-bind (new peer ptype)
                  (peripheral-accept sock :timeout-ms
                                     (if deadline
                                         (max 1 (min accept-timeout-ms
                                                     (round (* 1000 (- deadline (get-internal-real-time)))
                                                            internal-time-units-per-second)))
                                         accept-timeout-ms))
                (when new
                  (setf conn new)
                  ;; The controller stopped advertising when it accepted; it
                  ;; stays stopped until we say otherwise.
                  (when on-connect (funcall on-connect conn peer ptype)))))
             (t
              ;; The pump's return value IS the disconnect notification.
              (let ((r (hci-pump conn tick-ms)))
                (if (eq r :disconnected)
                    (progn
                      (when on-disconnect (funcall on-disconnect conn))
                      (setf conn nil))
                    (let ((op (gatt-serve server conn :timeout-ms 0)))
                      (if (eq op :disconnected)
                          (progn
                            (when on-disconnect (funcall on-disconnect conn))
                            (setf conn nil))
                          (when on-tick (funcall on-tick conn op)))))))))
      (ignore-errors (set-adv-enable sock nil)))))
