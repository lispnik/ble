;;;; Pairing, initiator side.
;;;;
;;;; In its own package on the public API, like the responder. The local
;;;; address is read from the connection's own controller: f5 and f6 bind the
;;;; keys to BOTH addresses, so an initiator that does not know its own cannot
;;;; derive the same key as the peer.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))

(defpackage #:smp-initiator (:use #:common-lisp) (:export #:run))
(in-package #:smp-initiator)

(defun env-or (name d)
  (let ((v (sb-ext:posix-getenv name))) (if (and v (plusp (length v))) v d)))

(defun run (&key (peer (env-or "PEER_MAC" "3C:64:CF:2D:55:A3"))
                 (dev (parse-integer (env-or "CENTRAL_DEV" "2")))
                 (passkey (parse-integer (env-or "PASSKEY" "123456"))))
  (ble:install-adapter-teardown)
  (ble:with-att-channel
      (conn (ble:hci-user-att-connect (ble:parse-mac peer) :addr-type :public
                                      :dev dev :init-phys #x01
                                      :timeout 25 :retries 3))
    (let ((local (ble:hci-read-bd-addr :sock (ble:hci-conn-sock conn))))
      (format t "~&[c] connected; local ~A peer ~A~%" (ble:format-mac local) peer)
      (force-output)
      (handler-case
          (let ((session (ble:smp-pair conn :role :central
                                            :local-addr local
                                            :local-addr-type :public
                                            :peer-addr (ble:parse-mac peer)
                                            :peer-addr-type :public
                                            :io-capability :keyboard-only
                                            :passkey passkey)))
            (format t "~&[c] PAIRED, LTK ~{~2,'0X~}~%"
                    (coerce (ble:smp-session-ltk session) 'list))
            (force-output)
            (format t "~&[c] encryption -> ~A~%"
                    (ble:smp-start-encryption session)))
        (ble:smp-error (e) (format t "~&[c] ~A~%" e))))))

(run)
(sb-ext:exit)
