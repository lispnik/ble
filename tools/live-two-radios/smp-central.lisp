;;;; Pairing, initiator side.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))
(in-package #:ble)
(install-adapter-teardown)

(defun env-or (name d)
  (let ((v (sb-ext:posix-getenv name))) (if (and v (plusp (length v))) v d)))
(defparameter *peer* (env-or "PEER_MAC" "3C:64:CF:2D:55:A3"))
(defparameter *dev* (parse-integer (env-or "CENTRAL_DEV" "2")))

(let ((conn (hci-user-att-connect (parse-mac *peer*) :addr-type :public
                                                     :dev *dev* :init-phys #x01
                                                     :timeout 25 :retries 3)))
  (unwind-protect
       (let ((local (hci-read-bd-addr :sock (hci-conn-sock conn))))
         (format t "~&[c] connected; local ~A peer ~A~%"
                 (format-mac local) *peer*)
         (force-output)
         (handler-case
             (let ((session (smp-pair conn :role :central
                                           :local-addr local
                                           :local-addr-type :public
                                           :peer-addr (parse-mac *peer*)
                                           :peer-addr-type :public)))
               (format t "~&[c] PAIRED, LTK ~{~2,'0X~}~%"
                       (coerce (smp-session-ltk session) 'list))
               (force-output)
               (let ((r (smp-start-encryption session)))
                 (format t "~&[c] encryption -> ~A~%" r)))
           (smp-error (e) (format t "~&[c] ~A~%" e))))
    (att-channel-close conn)))
(sb-ext:exit)
