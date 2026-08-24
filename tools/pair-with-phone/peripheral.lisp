;;;; Pair with an independent stack: a phone.
;;;;
;;;; The point is interop. Both ends of the two-radio test are this code, so
;;;; matching keys there prove consistency and not conformance -- a
;;;; systematically wrong f5 agrees with itself perfectly, and did. A phone's
;;;; Security Manager was written by somebody else from the same specification,
;;;; so if it pairs, the exchange is right.
;;;;
;;;; Rewritten in its own package on the public API. It used to live inside
;;;; #:ble and reach for internals, which meant this file was not evidence that
;;;; a consumer could write a peripheral -- only that the library could. If it
;;;; needs a `ble::' symbol again, something is missing from the API.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))

(defpackage #:pair-with-phone (:use #:common-lisp) (:export #:run))
(in-package #:pair-with-phone)

(setf ble:*smp-trace* t ble:*gatt-server-trace* t)

(defun env-int (name d)
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v))) (parse-integer v :junk-allowed t) d)))

(defparameter *name* "BLE-PAIR-TEST")
(defparameter *bond-file* #p"/tmp/ble-phone-bonds.lisp")

(defun build-server ()
  "Something for the phone to discover, with one thing it cannot have.

A phone that finds no services often disconnects before anyone gets round to
pairing, and Generic Access must be first: handles are allocated in order and
iOS reads Device Name and Appearance the moment it connects."
  (let ((server (ble:make-gatt-server :mtu 23)))
    (ble:gatt-add-service server ble:+service-generic-access+)
    (ble:gatt-add-characteristic server :uuid ble:+char-device-name+
                                        :properties '(:read) :value *name*)
    (ble:gatt-add-characteristic server :uuid ble:+char-appearance+
                                        :properties '(:read)
                                        :value (ble:appearance
                                                ble:+appearance-generic-tag+))
    ;; Generic Attribute. Its CCCD is protected deliberately: iOS writes it on
    ;; every connection, unprompted, to subscribe to Service Changed -- so
    ;; requiring encryption on it makes the phone pair of its own accord, with
    ;; nobody tapping anything. A peripheral cannot start pairing; all it can
    ;; do is require security on something the central already wants.
    (ble:gatt-add-service server ble:+service-generic-attribute+)
    (ble:gatt-add-characteristic server :uuid ble:+char-service-changed+
                                        :properties '(:indicate)
                                        :value (ble:service-changed-range)
                                        :security :encrypted)
    (ble:gatt-add-service server ble:+service-device-information+)
    (ble:gatt-add-characteristic server :uuid ble:+char-manufacturer-name+
                                        :properties '(:read) :value "lispnik")
    (ble:gatt-add-characteristic server :uuid ble:+char-model-number+
                                        :properties '(:read) :value "ble")
    (ble:gatt-add-service server #xFFE0)
    (ble:gatt-add-characteristic server :uuid #xFFE1
                                 :properties '(:read :write :notify)
                                 :value #(0))
    (ble:gatt-add-characteristic server :uuid #xFFE2 :properties '(:read)
                                        :value "secret"
                                        :security :encrypted)
    server))

;;; --- one connection's worth of state ------------------------------------

(defstruct link
  "What the hooks need to remember between ticks."
  peer peer-type session asked local-addr irk)

(defun on-connect (state server conn peer ptype)
  (declare (ignore conn))
  (setf (link-peer state) peer
        (link-peer-type state) ptype
        (link-session state) nil
        (link-asked state) nil)
  (setf (ble:gatt-server-encrypted server) nil)
  (format t "~&connected: ~A (~(~A~) address)~%" (ble:format-mac peer) ptype)
  (let ((known (ble:find-bond peer)))
    (if known
        (format t "~&*** RECOGNISED as the peer bonded at ~A -- resolved from ~
                   a changed~%    address with the identity key it gave us ***~%"
                (ble:format-mac (ble:bond-identity-addr known)))
        (format t "~&not recognised; this will be a fresh pairing~%")))
  (force-output))

(defun handle-key-request (state server event conn)
  "Answer the controller's Long Term Key Request.

Either a pairing has just finished, or this is a peer we remember coming back
-- which needs no pairing at all, and is the whole point of a bond."
  (let* ((session (link-session state))
         (known (and (not (ble:smp-session-p session))
                     (link-peer state)
                     (ble:find-bond (link-peer state))))
         (answered (ble:smp-answer-ltk-request
                    conn event
                    :session (and (ble:smp-session-p session) session)
                    :ltk (and known (ble:bond-ltk known)))))
    (declare (ignore server))
    (format t "~&long-term key requested -- ~A~%"
            (cond ((not answered) "we have no key; refused")
                  (known "answered from the stored bond")
                  (t "answered from the pairing just done")))
    (force-output)))

(defun handle-encryption (state server event conn)
  "Encryption Change. Keys are distributed over the encrypted link, so this is
where the identity exchange belongs -- doing it when pairing returns collects
nothing and stores a bond against an address that expires within minutes."
  (let ((ok (and (zerop (aref event 3)) (= 1 (aref event 6)))))
    (format t "~&~:[encryption failed: status 0x~2,'0X~;*** LINK ENCRYPTED -- ~
               the phone accepted our keys ***~]~%"
            ok (aref event 3))
    (force-output)
    (when ok
      (setf (ble:gatt-server-encrypted server) t)
      (let ((session (link-session state)))
        (when (ble:smp-session-p session)
          (ignore-errors
           (ble:smp-send-identity session :irk (link-irk state)
                                          :identity-addr (link-local-addr state)
                                          :identity-addr-type :random)
           (ble:smp-receive-keys session :timeout-ms 8000))
          (let ((b (ble:bond-from-session
                    session :identity-addr (link-peer state)
                            :identity-addr-type (link-peer-type state))))
            (ble:store-bond b)
            (format t "~&bond stored for ~A~:[ -- NO IRK, so this peer cannot ~
                       be recognised at a new address~; (IRK received)~]~%"
                    (ble:format-mac (ble:bond-identity-addr b))
                    (ble:bond-irk b))
            (force-output)))))
    (declare (ignorable conn))))

(defun on-tick (state server conn)
  "Ask once, then answer whatever the request provokes.

Events are claimed from the queue rather than read off the socket: SERVE-
PERIPHERAL is already the only reader, and a second one racing it would mean
whichever lost never saw the packet."
  (unless (link-asked state)
    (setf (link-asked state) t)
    (format t "~&asking the phone to pair...~%") (force-output)
    (ble:smp-request-security conn))
  (let ((ltk (ble:hci-take-event conn :event #x3E :subevent #x05)))
    (when ltk (handle-key-request state server ltk conn)))
  (let ((enc (ble:hci-take-event conn :event #x08)))
    (when (and enc (>= (length enc) 7)) (handle-encryption state server enc conn)))
  ;; A Pairing Request from the phone arrives on the SMP channel.
  (when (and (ble:smp-pairing-requested-p conn)
             (not (ble:smp-session-p (link-session state))))
    (handler-case
        (let ((s (ble:smp-pair conn :role :peripheral
                                    :local-addr (link-local-addr state)
                                    :local-addr-type :random
                                    :peer-addr (link-peer state)
                                    :peer-addr-type (link-peer-type state)
                                    :timeout-ms 60000)))
          (setf (link-session state) s)
          (format t "~&*** PAIRED with the phone ***~%LTK ~{~2,'0X~}~%"
                  (coerce (ble:smp-session-ltk s) 'list))
          (force-output))
      (ble:smp-error (e)
        (format t "~&~A~%" e) (force-output)
        ;; Hold the link a moment so the Pairing Failed we just sent actually
        ;; leaves the controller; tearing down now discards it and the peer
        ;; reports a timeout instead of the reason we gave it.
        (dotimes (i 20) (ble:hci-pump conn 100))
        (setf (link-asked state) t)))))

;;; --- running it ---------------------------------------------------------

(defun run (&key (dev (env-int "PERIPH_DEV" 1)) (minutes (env-int "MINUTES" 5)))
  (ble:install-adapter-teardown)
  (setf ble:*bond-file* *bond-file*)
  (let ((server (build-server))
        (state (make-link)))
    (ble:with-hci-user-socket (sock dev)
      ;; A fresh static random address each run. Clients cache a GATT database
      ;; by address and only re-discover on a Service Changed indication from a
      ;; BONDED peer, so an unbonded peripheral that gains a characteristic is
      ;; shown its old database forever. A new address is a device the phone
      ;; has never met. It is bound into the pairing keys, so SMP-PAIR gets the
      ;; same one.
      (setf (link-local-addr state) (ble:static-random-address
                                     (ble:smp-random-octets sock 6))
            (link-irk state) (ble:smp-random-octets sock 16))
      (ble:set-random-address sock (link-local-addr state))
      (ble:set-adv-parameters sock :adv-type ble:+adv-ind+ :own-addr-type 1)
      (ble:set-adv-data sock (ble:adv-data
                              :flags '(:general-discoverable :no-bredr)
                              :name *name*))
      (format t "~&================================================~%~
                 Advertising as ~S on hci~D (~A)~%~
                 Connect; the Service Changed CCCD is protected, so the phone~%~
                 will offer to pair without anyone tapping anything.~%~
                 ~D bond(s) remembered from earlier runs~%~
                 Waiting up to ~D minutes...~%~
                 ================================================~%"
              *name* dev (ble:format-mac (link-local-addr state))
              (ble:load-bonds *bond-file*) minutes)
      (force-output)
      (ble:serve-peripheral
       server sock :seconds (* 60 minutes)
       :on-connect (lambda (conn peer ptype)
                     (on-connect state server conn peer ptype))
       :on-disconnect (lambda (conn)
                        (declare (ignore conn))
                        (format t "~&phone disconnected; advertising again~%")
                        (force-output))
       :on-tick (lambda (conn) (on-tick state server conn))))))

(run)
(sb-ext:exit)
