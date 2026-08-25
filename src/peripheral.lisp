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

;;; --- pairing ------------------------------------------------------------
;;;
;;; A peripheral cannot start pairing. All it can do is ask, and require
;;; security on something the central already wants; the central decides. What
;;; follows is not much logic, but it is exact, and getting the order wrong
;;; fails in ways that look like something else:
;;;
;;;   - The Long Term Key Request arrives from the CONTROLLER, not the peer,
;;;     and must be answered from the session just negotiated or from a stored
;;;     bond. Unanswered, the link simply never encrypts and the central gives
;;;     up without saying why.
;;;
;;;   - Keys are distributed OVER the encrypted link, so the identity exchange
;;;     belongs after Encryption Change and not when SMP-PAIR returns.
;;;     Collecting them earlier stores a bond against an address that expires
;;;     within minutes, and the peer is then a stranger the next time.
;;;
;;; This was written out by hand in two places before it lived here, and the
;;; second copy was already drifting from the first.

(defstruct (peripheral-pairing (:constructor make-peripheral-pairing
                                   (&key local-addr (local-addr-type :random)
                                         irk (io-capability :no-input-no-output)
                                         passkey (request t) (bond t)
                                         on-paired)))
  "How a peripheral should handle pairing, and what it learned doing it.

LOCAL-ADDR is the address being advertised, and is not optional: it is bound
into the pairing crypto, so a peripheral that pairs as one address while
advertising another produces confirm values the peer cannot verify. IRK, when
given, is distributed so a bonded peer can recognise this device at a rotated
address.

REQUEST asks the central to pair as soon as it connects. BOND distributes and
stores keys once the link is encrypted. ON-PAIRED, if given, is called with
(CONN SESSION BOND) after encryption succeeds -- BOND is NIL when bonding is
off or the peer sent no identity.

SESSION and PEER are filled in as a connection proceeds and reset for the
next one."
  local-addr local-addr-type irk io-capability passkey request bond on-paired
  peer peer-type session asked
  ;; Key collection runs across ticks rather than in one blocking
  ;; call. COLLECT-UNTIL is when to stop waiting for a peer that is
  ;; not going to send any; SETTLED is set once the outcome, keys or
  ;; no keys, has been dealt with.
  collect-until settled)

(defun %pairing-reset (p peer peer-type)
  (setf (peripheral-pairing-peer p) peer
        (peripheral-pairing-peer-type p) peer-type
        (peripheral-pairing-session p) nil
        (peripheral-pairing-asked p) nil
        (peripheral-pairing-collect-until p) nil
        (peripheral-pairing-settled p) nil))

(defun %pairing-settle (p conn)
  "Finish a pairing: store the bond and tell the caller. Once per connection."
  (declare (ignorable conn))
  (setf (peripheral-pairing-settled p) t
        (peripheral-pairing-collect-until p) nil)
  (let* ((session (peripheral-pairing-session p))
         (bond (when (and (peripheral-pairing-bond p) (smp-session-p session))
                 (ignore-errors
                  (let ((b (bond-from-session
                            session
                            :identity-addr (peripheral-pairing-peer p)
                            :identity-addr-type (peripheral-pairing-peer-type p))))
                    (store-bond b)
                    b)))))
    (when (peripheral-pairing-on-paired p)
      (funcall (peripheral-pairing-on-paired p) conn session bond))
    bond))

(defun %pairing-disconnected (p conn)
  "The peer left. Settle anything still in flight.

A central that got what it came for and disconnected inside the collection
window would otherwise leave the pairing unreported and its bond unstored --
which is most of them, since a short session is the normal case."
  (when (and p (not (peripheral-pairing-settled p))
             (smp-session-p (peripheral-pairing-session p)))
    (ignore-errors (%pairing-settle p conn))))

(defun %drive-pairing (p server conn)
  "One tick's worth of pairing. Called from SERVE-PERIPHERAL.

Events are claimed from the connection's queue rather than read from the
socket, because HCI-PUMP is the only reader and a second one would race it."
  ;; Ask, once per connection.
  (unless (peripheral-pairing-asked p)
    (setf (peripheral-pairing-asked p) t)
    (when (peripheral-pairing-request p)
      (ignore-errors (smp-request-security conn))))
  ;; The controller wants a key: either from the pairing just done, or from a
  ;; bond, which is a peer we already know coming back and needs no pairing.
  (let ((ltk (hci-take-event conn :event #x3E :subevent #x05)))
    (when ltk
      (let* ((session (peripheral-pairing-session p))
             (known (and (not (smp-session-p session))
                         (peripheral-pairing-peer p)
                         (find-bond (peripheral-pairing-peer p)))))
        (ignore-errors
         (smp-answer-ltk-request conn ltk
                                 :session (and (smp-session-p session) session)
                                 :ltk (and known (bond-ltk known)))))))
  ;; Encryption Change: the only thing that may set the server encrypted.
  ;; Keys are distributed over the encrypted link, so this is where collecting
  ;; them begins -- but it does NOT wait for them here. Waiting would stop this
  ;; peripheral answering ATT, and a peer whose request times out gets its
  ;; response late and every response after that belongs to the request before
  ;; it. That desync survives the pairing and looks like anything but pairing.
  (let ((enc (hci-take-event conn :event #x08)))
    (when (and enc (>= (length enc) 7))
      (let ((ok (and (zerop (aref enc 3)) (= 1 (aref enc 6)))))
        (setf (gatt-server-encrypted server) ok)
        (cond
          ((not ok) (setf (peripheral-pairing-settled p) t))
          ((and (peripheral-pairing-bond p)
                (smp-session-p (peripheral-pairing-session p)))
           (ignore-errors
            (smp-send-identity (peripheral-pairing-session p)
                               :irk (peripheral-pairing-irk p)
                               :identity-addr (peripheral-pairing-local-addr p)
                               :identity-addr-type (peripheral-pairing-local-addr-type p)))
           (setf (peripheral-pairing-collect-until p)
                 (+ (get-internal-real-time)
                    (* 5 internal-time-units-per-second))))
          (t (%pairing-settle p conn))))))
  ;; Collecting, a tick at a time. A peer that distributes nothing is not an
  ;; error -- it simply cannot be recognised at a new address later -- so the
  ;; deadline settles the pairing rather than failing it.
  (let ((until (peripheral-pairing-collect-until p)))
    (when (and until (not (peripheral-pairing-settled p)))
      (when (or (smp-poll-keys (peripheral-pairing-session p))
                (> (get-internal-real-time) until))
        (%pairing-settle p conn))))
  ;; A Pairing Request from the peer arrives on the SMP channel.
  (when (and (smp-pairing-requested-p conn)
             (not (smp-session-p (peripheral-pairing-session p))))
    (handler-case
        (setf (peripheral-pairing-session p)
              (smp-pair conn :role :peripheral
                             :local-addr (peripheral-pairing-local-addr p)
                             :local-addr-type (peripheral-pairing-local-addr-type p)
                             :peer-addr (peripheral-pairing-peer p)
                             :peer-addr-type (peripheral-pairing-peer-type p)
                             :io-capability (peripheral-pairing-io-capability p)
                             :passkey (peripheral-pairing-passkey p)
                             :timeout-ms 60000))
      (smp-error (e)
        (declare (ignorable e))
        ;; Hold the link long enough for the Pairing Failed we just sent to
        ;; leave the controller. Tearing down now discards it, and the peer
        ;; reports a timeout instead of the reason it was given.
        (dotimes (i 20) (hci-pump conn 100))))))

(defun serve-peripheral (server sock &key on-connect on-tick on-disconnect
                                          pairing
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
        (conn nil)
        ;; Whether the controller is currently advertising. Tracked rather
        ;; than re-asserted every pass: LE Set Advertising Enable is Command
        ;; Disallowed when advertising is already on, and SEND-HCI-COMMAND
        ;; now reports a refused command instead of discarding it -- so the
        ;; old habit of enabling on every iteration became fatal the moment
        ;; the accept timed out once and went round again.
        (advertising nil))
    (unwind-protect
         (loop
           (when (and deadline (> (get-internal-real-time) deadline)) (return))
           (cond
             ((null conn)
              (unless advertising
                (set-adv-enable sock t)
                (setf advertising t))
              (multiple-value-bind (new peer ptype)
                  (peripheral-accept sock :timeout-ms
                                     (if deadline
                                         (max 1 (min accept-timeout-ms
                                                     (round (* 1000 (- deadline (get-internal-real-time)))
                                                            internal-time-units-per-second)))
                                         accept-timeout-ms))
                (when new
                  ;; The controller stops advertising the moment it accepts.
                  (setf conn new advertising nil)
                  ;; Before ON-CONNECT, so a hook that inspects the pairing
                  ;; state sees this connection's, not the last one's.
                  (when pairing
                    (setf (gatt-server-encrypted server) nil)
                    (%pairing-reset pairing peer ptype))
                  ;; The controller stopped advertising when it accepted; it
                  ;; stays stopped until we say otherwise.
                  (when on-connect (funcall on-connect conn peer ptype)))))
             (t
              ;; The pump's return value IS the disconnect notification.
              (let ((r (hci-pump conn tick-ms)))
                (if (eq r :disconnected)
                    (progn
                      (when pairing (%pairing-disconnected pairing conn))
                      (when on-disconnect (funcall on-disconnect conn))
                      (setf conn nil))
                    (let ((op (gatt-serve server conn :timeout-ms 0)))
                      (if (eq op :disconnected)
                          (progn
                            (when pairing (%pairing-disconnected pairing conn))
                            (when on-disconnect (funcall on-disconnect conn))
                            (setf conn nil))
                          (progn
                            ;; Before ON-TICK: a peripheral whose
                            ;; characteristics require encryption wants the
                            ;; link secured before it is asked to do work.
                            (when pairing (%drive-pairing pairing server conn))
                            (when on-tick (funcall on-tick conn op))))))))))
      (ignore-errors (set-adv-enable sock nil)))))
