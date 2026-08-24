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
(setf *smp-trace* t)

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
    (let ((local-addr (hci-read-bd-addr :sock sock)))
      (format t "~&================================================~%")
      (format t "~&Advertising as ~S on hci~D (~A)~%" *name* +dev+
              (format-mac local-addr))
      (format t "~&Connect, then READ characteristic FFE2 (\"secret\") in~%~
                   service FFE0. It is protected, so the read comes back~%~
                   Insufficient Authentication and the phone will pair.~%")
      (format t "~&Waiting up to ~D minutes...~%" +minutes+)
      (format t "~&================================================~%")
      (force-output)
      (set-adv-parameters sock :adv-type +adv-ind+ :own-addr-type 0)
      (set-adv-data sock (adv-payload))
      (with-advertising (sock)
        (let ((deadline (+ (get-internal-real-time)
                           (* +minutes+ 60 internal-time-units-per-second)))
              (conn nil) (session nil) (asked nil))
          (loop
            (when (> (get-internal-real-time) deadline)
              (format t "~&timed out -- nobody bonded~%") (return))
            (let ((pkt (hci-poll-read sock 200)))
              (when pkt
                (cond
                  ;; connected
                  ((and (>= (length pkt) 15) (= (aref pkt 0) #x04)
                        (= (aref pkt 1) #x3E) (member (aref pkt 3) '(#x01 #x0A))
                        (zerop (aref pkt 4)) (null conn))
                   (let ((peer (subseq pkt 9 15))
                         (ptype (smp-peer-addr-type pkt)))
                     (setf conn (make-hci-conn :sock sock :handle (u16-le pkt 5)
                                               :acl-len (hci-socket-acl-len sock)))
                     (format t "~&connected: ~A (~(~A~) address)~%"
                             (format-mac peer) ptype)
                     (force-output)
                     ;; Serve GATT and pair in the same loop; a phone browses
                     ;; the database and bonds in whichever order it likes.
                     (setf session (list peer ptype))))
                  ;; the key request that follows the phone starting encryption
                  ((and (>= (length pkt) 5) (= (aref pkt 0) #x04)
                        (= (aref pkt 1) #x3E) (= (aref pkt 3) #x05))
                   (format t "~&long-term key requested~%") (force-output)
                   (smp-answer-ltk-request (and (smp-session-p session) session)
                                           pkt))
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
                   (when (zerop (aref pkt 3)) (return)))
                  ((and (>= (length pkt) 4) (= (aref pkt 0) #x04)
                        (= (aref pkt 1) #x05))
                   (format t "~&phone disconnected~%") (force-output)
                   (setf conn nil session nil asked nil)))))
            (when conn
              ;; serve the database
              (let ((op (gatt-serve server conn :timeout-ms 20)))
                (when (eq op :disconnected)
                  (format t "~&phone disconnected~%") (force-output)
                  (setf conn nil session nil asked nil)))
              ;; ask once, then respond to the pairing it triggers
              (when (and conn (consp session) (not asked))
                (setf asked t)
                (format t "~&asking the phone to pair...~%") (force-output)
                (smp-request-security conn))
              (when (and conn (consp session) (hci-conn-smp-pending conn))
                (destructuring-bind (peer ptype) session
                  (handler-case
                      (let ((s (smp-pair conn :role :peripheral
                                              :local-addr local-addr
                                              :local-addr-type :public
                                              :peer-addr peer
                                              :peer-addr-type ptype
                                              :timeout-ms 60000)))
                        (setf session s)
                        (format t "~&*** PAIRED with the phone ***~%LTK ~{~2,'0X~}~%"
                                (coerce (smp-session-ltk s) 'list))
                        (force-output))
                    (smp-error (e)
                      (format t "~&PAIRING FAILED: ~A~%" e) (force-output)
                      (setf session (list peer ptype) asked t))))))))))))
(sb-ext:exit)
