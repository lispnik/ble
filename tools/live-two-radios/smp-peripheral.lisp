;;;; Pairing, responder side.
;;;;
;;;; In its own package on the public API. This has no GATT database worth the
;;;; name -- the point is the Security Manager exchange, not what is served
;;;; over it -- but it still uses PERIPHERAL-ACCEPT rather than hand-rolling
;;;; the wait for a connection, because that is where the Enhanced Connection
;;;; Complete subevent was missed once already.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))

(defpackage #:smp-responder (:use #:common-lisp) (:export #:run))
(in-package #:smp-responder)

(defun env-int (name d)
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v))) (parse-integer v :junk-allowed t) d)))

(defun run (&key (dev (env-int "PERIPH_DEV" 1))
                 (passkey (env-int "PASSKEY" 123456)))
  (ble:install-adapter-teardown)
  (ble:with-hci-user-socket (sock dev)
    (let ((local-addr (ble:hci-read-bd-addr :sock sock)))
      (format t "~&[p] hci~D, address ~A~%" dev (ble:format-mac local-addr))
      (ble:set-adv-parameters sock :adv-type ble:+adv-ind+ :own-addr-type 0)
      (ble:set-adv-data sock (ble:adv-data
                              :flags '(:general-discoverable :no-bredr)
                              :name "PAIR"))
      (ble:with-advertising (sock)
        (format t "~&[p] advertising~%") (force-output)
        (multiple-value-bind (conn peer ptype)
            (ble:peripheral-accept sock :timeout-ms 90000)
          (if (null conn)
              (format t "~&[p] nobody connected~%")
              (progn
                (format t "~&[p] connected to ~A~%" (ble:format-mac peer))
                (force-output)
                (handler-case
                    (let ((session (ble:smp-pair conn :role :peripheral
                                                      :local-addr local-addr
                                                      :local-addr-type :public
                                                      :peer-addr peer
                                                      :peer-addr-type ptype
                                                      :io-capability :display-only
                                                      :passkey passkey)))
                      (format t "~&[p] PAIRED, LTK ~{~2,'0X~}~%"
                              (coerce (ble:smp-session-ltk session) 'list))
                      (force-output)
                      ;; Answer the key request the central's encryption
                      ;; triggers, then report what the controller made of it.
                      (loop repeat 60
                            for pkt = (progn (ble:hci-pump conn 500)
                                             (or (ble:hci-take-event
                                                  conn :event #x3E :subevent #x05)
                                                 (ble:hci-take-event conn :event #x08)))
                            do (when pkt
                                 (cond
                                   ((= (aref pkt 1) #x3E)
                                    (format t "~&[p] LTK requested~%") (force-output)
                                    (ble:smp-answer-ltk-request conn pkt
                                                                :session session))
                                   (t
                                    (format t "~&[p] ENCRYPTED: status ~D enabled ~D~%"
                                            (aref pkt 3) (aref pkt 6))
                                    (force-output)
                                    (return))))))
                  (ble:smp-error (e)
                    (format t "~&[p] ~A~%" e) (force-output)
                    ;; Hold the link a moment so the Pairing Failed we just
                    ;; sent actually leaves the controller. Tearing down now
                    ;; discards it, and the peer reports a timeout instead of
                    ;; the reason we gave it.
                    (dotimes (i 20) (ble:hci-pump conn 100)))))))))))

(run)
(sb-ext:exit)
