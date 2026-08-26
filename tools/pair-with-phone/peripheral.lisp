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
    ;; SAY THAT WE DO EATT. Without this the phone has no way to know: EATT is
    ;; not advertised and there is nothing in the connection to infer it from.
    ;; Bit 0 of Server Supported Features is the whole mechanism, and a
    ;; conforming client that cannot read it will never try the PSM -- which
    ;; is exactly what the first run of this test showed. Pairing succeeded,
    ;; GATT was browsed, and no bearer was ever asked for, because we had
    ;; implemented EATT without telling anybody.
    (ble:gatt-add-characteristic server
                                 :uuid ble:+char-server-supported-features+
                                 :properties '(:read)
                                 :value (vector ble:+server-feature-eatt+))
    ;; And let the phone declare its own. Bit 1 is the client's EATT support;
    ;; iOS writes this when it has something to say, and refusing the write
    ;; would be a poor way to start.
    (ble:gatt-add-characteristic server
                                 :uuid ble:+char-client-supported-features+
                                 :properties '(:read :write)
                                 :value (vector 0)
                                 :on-write
                                 (lambda (s a v)
                                   (declare (ignore s a))
                                   (format t "~&phone's client features: ~
                                              ~{~2,'0X~} ~:[~;(it says it ~
                                              does EATT)~]~%"
                                           (coerce v 'list)
                                           (and (plusp (length v))
                                                (logtest ble:+client-feature-eatt+
                                                         (aref v 0))))
                                   (force-output)
                                   nil))
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

;;; --- what the run reports -----------------------------------------------
;;;
;;; The sequencing -- ask, answer the controller's key request, distribute
;;; identity after Encryption Change, store the bond -- used to be written out
;;; here, and again in examples/glucose/. It is BLE:SERVE-PERIPHERAL's now,
;;; driven from a BLE:PERIPHERAL-PAIRING. What is left in this file is what
;;; this file is actually for: saying out loud what the phone did, because
;;; that is the evidence the exchange interoperates with a stack somebody else
;;; wrote.

(defun report-connect (peer ptype)
  (format t "~&connected: ~A (~(~A~) address)~%" (ble:format-mac peer) ptype)
  (let ((known (ble:find-bond peer)))
    (if known
        (format t "~&*** RECOGNISED as the peer bonded at ~A -- resolved from ~
                   a changed~%    address with the identity key it gave us ***~%"
                (ble:format-mac (ble:bond-identity-addr known)))
        (format t "~&not recognised; this will be a fresh pairing~%")))
  (force-output))

(defun report-paired (conn session bond)
  (declare (ignore conn))
  (format t "~&*** LINK ENCRYPTED -- the phone accepted our keys ***~%")
  (when (ble:smp-session-p session)
    (format t "~&LTK ~{~2,'0X~}~%" (coerce (ble:smp-session-ltk session) 'list)))
  (if bond
      (format t "~&bond stored for ~A~:[ -- NO IRK, so this peer cannot be ~
                 recognised at a new address~; (IRK received)~]~%"
              (ble:format-mac (ble:bond-identity-addr bond)) (ble:bond-irk bond))
      (format t "~&no bond stored~%"))
  (force-output))

(defun run (&key (dev (env-int "PERIPH_DEV" 1)) (minutes (env-int "MINUTES" 5)))
  (ble:install-adapter-teardown)
  (setf ble:*bond-file* *bond-file*)
  (let ((server (build-server))
        (pairing nil))
    (ble:with-hci-user-socket (sock dev)
      ;; A fresh static random address each run. Clients cache a GATT database
      ;; by address and only re-discover on a Service Changed indication from a
      ;; BONDED peer, so an unbonded peripheral that gains a characteristic is
      ;; shown its old database forever. A new address is a device the phone
      ;; has never met. It is bound into the pairing keys, so SMP-PAIR gets the
      ;; same one.
      (setf pairing (ble:make-peripheral-pairing
                     :local-addr (ble:static-random-address
                                  (ble:smp-random-octets sock 6))
                     :irk (ble:smp-random-octets sock 16)
                     :on-paired #'report-paired))
      (ble:set-random-address sock (ble:peripheral-pairing-local-addr pairing))
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
              *name* dev (ble:format-mac (ble:peripheral-pairing-local-addr pairing))
              (ble:load-bonds *bond-file*) minutes)
      (force-output)
      (let ((bearers 0))
        (ble:serve-peripheral
         server sock :seconds (* 60 minutes)
         :pairing pairing
         ;; The other half of the interop question. A phone's Bluetooth stack
         ;; decides for itself whether to open Enhanced ATT bearers -- no app
         ;; asks for them -- so offering the PSM and watching is the whole
         ;; test. iOS will only try it over an encrypted link, which is why
         ;; this belongs in the tool that pairs rather than the bench harness
         ;; that does not.
         :eatt t
         :on-connect (lambda (conn peer ptype)
                       (declare (ignore conn))
                       (setf bearers 0)
                       (report-connect peer ptype))
         :on-disconnect (lambda (conn)
                          (declare (ignore conn))
                          (format t "~&phone disconnected; advertising again~%")
                          (force-output))
         ;; Report bearers as they appear. Counted rather than hooked because
         ;; the interesting event is `the phone decided to open one', and that
         ;; happens inside the library with nothing to call back to.
         :on-tick (lambda (conn request)
                    (declare (ignore request))
                    (let ((n (length (ble:hci-conn-eatt-bearers conn))))
                      (when (/= n bearers)
                        (format t "~&EATT: ~D bearer(s) open~:[~; -- the phone ~
                                   asked for them, which is the answer~]~%"
                                n (plusp n))
                        (force-output)
                        (setf bearers n)))))))))

(run)
(sb-ext:exit)
