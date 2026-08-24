;;;; Pair with an independent stack: a phone.
;;;;
;;;; The point is interop. Both ends of the two-radio test are this code, so
;;;; matching keys there prove consistency and not conformance -- a
;;;; systematically wrong f5 agrees with itself perfectly. A phone's Security
;;;; Manager was written by somebody else from the same specification, so if
;;;; it pairs, the exchange is right.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))
(in-package #:ble)
(setf *smp-trace* t *gatt-server-trace* t)

(defvar *our-irk* nil)

(defun env-int (name d)
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v))) (parse-integer v :junk-allowed t) d)))

(defparameter +dev+ (env-int "PERIPH_DEV" 1))
(defparameter +minutes+ (env-int "MINUTES" 5))
(defparameter *name* "BLE-PAIR-TEST")

(defun build-server ()
  "Something for the phone to discover. A phone that finds no services often
disconnects before anyone gets round to pairing."
  (let ((server (make-gatt-server :mtu 23)))
    ;; Generic Access first, because iOS reads it the moment it connects and
    ;; a peripheral without it is a well-known way to get connected to and
    ;; then dropped. It must be the first service: handles are allocated in
    ;; order and GAP is expected at the bottom of the database.
    (gatt-add-service server #x1800)
    (gatt-add-characteristic server :uuid #x2A00 :properties '(:read)
                                    :value *name*)          ; Device Name
    (gatt-add-characteristic server :uuid #x2A01 :properties '(:read)
                                    :value #(#xC0 #x03))    ; Appearance: generic tag
    ;; Generic Attribute, with Service Changed. This is how a peripheral tells
    ;; a client its database moved. Clients cache the database by address and
    ;; do not re-discover on reconnect -- iOS notably -- so without this, a
    ;; peripheral that gains a characteristic keeps showing the old set until
    ;; the client's cache is cleared by other means.
    (gatt-add-service server #x1801)
    ;; Protected deliberately. The log shows iOS writing this CCCD on every
    ;; connection, unprompted, to subscribe to Service Changed -- so requiring
    ;; encryption on it makes the phone pair of its own accord, with nobody
    ;; tapping anything. A peripheral cannot start pairing; all it can do is
    ;; require security on something the central already wants.
    (gatt-add-characteristic server :uuid #x2A05 :properties '(:indicate)
                                    :value #(1 0 255 255)
                                    :security :encrypted)
    (gatt-add-service server #x180A)                       ; Device Information
    (gatt-add-characteristic server :uuid #x2A29 :properties '(:read)
                                    :value "lispnik")
    (gatt-add-characteristic server :uuid #x2A24 :properties '(:read)
                                    :value "ble")
    (gatt-add-service server #xFFE0)
    (gatt-add-characteristic server :uuid #xFFE1
                             :properties '(:read :write :notify)
                             :value #(0))
    ;; The one that forces the issue. iOS gives an app no way to ask for
    ;; bonding; it starts pairing when a read comes back with Insufficient
    ;; Authentication, so a peripheral that wants to be paired with has to
    ;; protect something and wait to be asked for it.
    (gatt-add-characteristic server :uuid #xFFE2
                             :properties '(:read)
                             :value "secret"
                             :security :encrypted)
    server))

(defun adv-payload ()
  (concatenate '(vector (unsigned-byte 8))
               (vector 2 1 6)                              ; flags: general disc
               (vector (1+ (length *name*)) 9)             ; complete local name
               (map 'vector #'char-code *name*)))

(let ((server (build-server)))
  (with-hci-user-socket (sock +dev+)
    ;; Advertise from a fresh static random address every run. iOS caches a
    ;; GATT database by address and only re-discovers on a Service Changed
    ;; indication from a BONDED peer -- so an unbonded peripheral that gains a
    ;; characteristic keeps being shown its old database forever. A new
    ;; address is a device it has never met, which sidesteps the cache
    ;; entirely. The address is also bound into the pairing keys, so it has to
    ;; be the one handed to SMP-PAIR.
    (setf *bond-file* #p"/tmp/ble-phone-bonds.lisp")
    (let ((local-addr (static-random-address (smp-random-octets sock 6)))
          (*our-irk* (smp-random-octets sock 16)))
      (set-random-address sock local-addr)
      (format t "~&================================================~%")
      (format t "~&Advertising as ~S on hci~D (~A)~%" *name* +dev+
              (format-mac local-addr))
      (format t "~&Connect, then READ characteristic FFE2 (\"secret\") in~%~
                   service FFE0. It is protected, so the read comes back~%~
                   Insufficient Authentication and the phone will pair.~%")
      (format t "~&build: bonds5+readvertise-fixed~%")
      (format t "~&~D bond(s) remembered from earlier runs~%"
              (load-bonds *bond-file*))
      (format t "~&Waiting up to ~D minutes...~%" +minutes+)
      (format t "~&================================================~%")
      (force-output)
      (set-adv-parameters sock :adv-type +adv-ind+ :own-addr-type 1)
      (set-adv-data sock (adv-payload))
      (with-advertising (sock)
        (let ((deadline (+ (get-internal-real-time)
                           (* +minutes+ 60 internal-time-units-per-second)))
              (conn nil) (session nil) (asked nil) (peer-addr nil) (peer-type :public))
          (loop
            (when (> (get-internal-real-time) deadline)
              (format t "~&timed out -- nobody bonded~%") (return))
            ;; Once connected, read ONLY through the connection: hci-pump
            ;; files events where hci-take-event can find them. Polling the
            ;; socket here as well made two readers race for the same packets,
            ;; and whichever lost simply never saw them.
            ;; hci-pump reports a disconnect as :DISCONNECTED and does NOT
            ;; queue it, so it has to be acted on here. Throwing that return
            ;; value away is what left the peripheral believing it was still
            ;; connected: advertising stops the moment a central connects, and
            ;; without the disconnect nothing ever turned it back on. It sat
            ;; there invisible.
            (when (and conn (eq :disconnected (hci-pump conn 60)))
              (format t "~&phone disconnected; advertising again~%")
              (force-output)
              (setf conn nil session nil asked nil)
              (set-adv-enable sock t))
            (let ((pkt (if conn
                           (or (hci-take-event conn :event #x3E :subevent #x05)
                               (hci-take-event conn :event #x08))
                           (hci-poll-read sock 200))))
              (when pkt
                (cond
                  ;; connected
                  ((and (>= (length pkt) 15) (= (aref pkt 0) #x04)
                        (= (aref pkt 1) #x3E) (member (aref pkt 3) '(#x01 #x0A))
                        (zerop (aref pkt 4)) (null conn))
                   (let ((peer (subseq pkt 9 15))
                         (ptype (smp-peer-addr-type pkt)))
                     ;; Keep these where the later branches can see them: the
                     ;; encryption event arrives in a different clause, and
                     ;; the bond has to be stored against the peer, not
                     ;; against whatever is in scope there.
                     (setf peer-addr peer peer-type ptype)
                     (setf conn (make-hci-conn :sock sock :handle (u16-le pkt 5)
                                               :acl-len (hci-socket-acl-len sock)))
                     (format t "~&connected: ~A (~(~A~) address)~%"
                             (format-mac peer) ptype)
                     (let ((known (find-bond peer)))
                       (if known
                           (format t "~&*** RECOGNISED as the peer bonded at ~
                                      ~A -- resolved from a~%    changed ~
                                      address with the identity key it gave ~
                                      us ***~%"
                                   (format-mac (bond-identity-addr known)))
                           (format t "~&not recognised; this will be a fresh ~
                                      pairing~%")))
                     (force-output)
                     ;; Serve GATT and pair in the same loop; a phone browses
                     ;; the database and bonds in whichever order it likes.
                     (setf session (list peer ptype))))
                  ;; the key request that follows the phone starting encryption
                  ((and (>= (length pkt) 5) (= (aref pkt 0) #x04)
                        (= (aref pkt 1) #x3E) (= (aref pkt 3) #x05))
                   ;; Either we have just paired, or this is a peer we
                   ;; remember coming back -- which is the whole point of a
                   ;; bond, and needs no pairing at all.
                   (let* ((known (and (not (smp-session-p session))
                                      peer-addr (find-bond peer-addr)))
                          (answered
                            (smp-answer-ltk-request
                             conn pkt
                             :session (and (smp-session-p session) session)
                             :ltk (and known (bond-ltk known)))))
                     (format t "~&long-term key requested -- ~A~%"
                             (cond ((not answered) "we have no key; refused")
                                   (known "answered from the stored bond")
                                   (t "answered from the pairing just done")))
                     (force-output)))
                  ;; encryption result
                  ((and (>= (length pkt) 7) (= (aref pkt 0) #x04)
                        (= (aref pkt 1) #x08))
                   (when (and (zerop (aref pkt 3)) (= 1 (aref pkt 6)))
                     ;; The server cannot see this for itself; tell it, so the
                     ;; protected characteristic becomes readable.
                     (setf (gatt-server-encrypted server) t))
                   (if (and (zerop (aref pkt 3)) (= 1 (aref pkt 6)))
                       (format t "~&*** LINK ENCRYPTED -- the phone accepted our keys ***~%")
                       (format t "~&encryption failed: status 0x~2,'0X~%" (aref pkt 3)))
                   (force-output)
                   (when (and (zerop (aref pkt 3)) (smp-session-p session))
                     ;; NOW the keys can be exchanged: distribution happens on
                     ;; the encrypted link, which is also why it cannot be
                     ;; done as part of pairing.
                     (ignore-errors
                      (smp-send-identity session :irk *our-irk*
                                                 :identity-addr local-addr
                                                 :identity-addr-type :random)
                      (smp-receive-keys session :timeout-ms 8000))
                     (let ((b (bond-from-session session :identity-addr peer-addr
                                                         :identity-addr-type peer-type)))
                       (store-bond b)
                       (format t "~&bond stored for ~A~:[ -- NO IRK, so this ~
                                  peer cannot be recognised at a new address~;~
                                   (IRK received)~]~%"
                               (format-mac (bond-identity-addr b))
                               (bond-irk b))
                       (force-output))))
                  ((and (>= (length pkt) 4) (= (aref pkt 0) #x04)
                        (= (aref pkt 1) #x05))
                   ;; Connectable advertising stops the moment a central
                   ;; connects, so a peripheral that does not restart it
                   ;; disappears after the first connection -- which looks
                   ;; from the other side like the device being switched off.
                   ;; Disconnection Complete: status(1) handle(2) reason(1),
                   ;; so the reason is at 6. Reading 5 gave the handle's high
                   ;; byte -- a constant zero dressed up as a diagnosis.
                   (format t "~&phone disconnected (reason 0x~2,'0X); ~
                              advertising again~%"
                           (if (>= (length pkt) 7) (aref pkt 6) 255))
                   (force-output)
                   (setf conn nil session nil asked nil)
                   (set-adv-enable sock t)))))
            (when conn
              ;; serve the database, and say what was asked for. Inferring
              ;; from silence is what made the last three attempts useless:
              ;; a phone that connects and leaves looks identical whether it
              ;; never asked for the protected value or asked and refused.
              (let ((op (gatt-serve server conn :timeout-ms 20)))

                (when (eq op :disconnected)
                  (format t "~&phone disconnected; advertising again~%")
                  (force-output)
                  (setf conn nil session nil asked nil)
                  (set-adv-enable sock t)))
              ;; ask once, then respond to the pairing it triggers
              (when (and conn (consp session) (not asked))
                (setf asked t)
                (format t "~&asking the phone to pair...~%") (force-output)
                (smp-request-security conn))
              (when (and conn (hci-conn-smp-pending conn))
                (format t "~&  smp frame 0x~2,'0X arrived~%"
                        (aref (first (hci-conn-smp-pending conn)) 0))
                (force-output))
              (when (and conn (consp session) (hci-conn-smp-pending conn))
                (destructuring-bind (peer ptype) session
                  (handler-case
                      (let ((s (smp-pair conn :role :peripheral
                                              :local-addr local-addr
                                              :local-addr-type :random
                                              :peer-addr peer
                                              :peer-addr-type ptype
                                              :timeout-ms 60000)))
                        (setf session s)
                        (format t "~&*** PAIRED with the phone ***~%LTK ~{~2,'0X~}~%"
                                (coerce (smp-session-ltk s) 'list))
                        (force-output)
                        ;; Keys are distributed over the ENCRYPTED link, so
                        ;; not here -- asking now gets nothing, and the bond
                        ;; would be stored against the address the peer
                        ;; happens to be using rather than its identity.
                        )
                    (smp-error (e)
                      (format t "~&PAIRING FAILED: ~A~%" e) (force-output)
                      (setf session (list peer ptype) asked t))))))))))))
(sb-ext:exit)
