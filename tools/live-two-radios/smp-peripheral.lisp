;;;; Pairing, responder side.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))
(in-package #:ble)

(defun env-int (name d)
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v))) (parse-integer v :junk-allowed t) d)))
(defparameter +dev+ (env-int "PERIPH_DEV" 1))

(with-hci-user-socket (sock +dev+)
  (let ((local-addr (hci-read-bd-addr :sock sock)))
    (format t "~&[p] hci~D, address ~A~%" +dev+ (format-mac local-addr))
    (set-adv-parameters sock :adv-type +adv-ind+ :own-addr-type 0)
    (set-adv-data sock (concatenate '(vector (unsigned-byte 8))
                                    (vector 2 1 6 5 9)
                                    (map 'vector #'char-code "PAIR")))
    (with-advertising (sock)
      (format t "~&[p] advertising~%") (force-output)
      (let ((handle nil) (peer nil))
        (loop repeat 180
              for pkt = (hci-poll-read sock 500)
              do (when (and pkt (>= (length pkt) 15) (= (aref pkt 0) #x04)
                            (= (aref pkt 1) #x3E)
                            ;; Enhanced Connection Complete as well as the
                            ;; plain one: which a controller sends depends on
                            ;; the event mask, and the peer address sits at
                            ;; the same offset in both.
                            (member (aref pkt 3) '(#x01 #x0A))
                            (zerop (aref pkt 4)))
                   (setf handle (u16-le pkt 5)
                         peer (subseq pkt 9 15))
                   (return)))
        (if (null handle)
            (format t "~&[p] nobody connected~%")
            (let ((conn (make-hci-conn :sock sock :handle handle
                                       :acl-len (hci-socket-acl-len sock))))
              (format t "~&[p] connected to ~A~%" (format-mac peer))
              (force-output)
              (handler-case
                  (let ((session (smp-pair conn :role :peripheral
                                                :local-addr local-addr
                                                :local-addr-type :public
                                                :peer-addr peer
                                                :peer-addr-type :public
                                           :io-capability :display-only
                                           :passkey (parse-integer
                                                     (or (sb-ext:posix-getenv "PASSKEY")
                                                         "123456")))))
                    (format t "~&[p] PAIRED, LTK ~{~2,'0X~}~%"
                            (coerce (smp-session-ltk session) 'list))
                    (force-output)
                    ;; Answer the key request the central's encryption triggers.
                    (loop repeat 60
                          for pkt = (hci-poll-read sock 500)
                          do (when (and pkt (>= (length pkt) 4)
                                        (= (aref pkt 0) #x04))
                               (cond
                                 ((and (= (aref pkt 1) #x3E) (= (aref pkt 3) #x05))
                                  (format t "~&[p] LTK requested~%") (force-output)
                                  (smp-answer-ltk-request conn pkt :session session))
                                 ((= (aref pkt 1) #x08)
                                  (format t "~&[p] ENCRYPTED: status ~D enabled ~D~%"
                                          (aref pkt 3) (aref pkt 6))
                                  (force-output)
                                  (return))))))
                (smp-error (e)
                  (format t "~&[p] ~A~%" e) (force-output)
                  ;; Keep the link a moment so the Pairing Failed we just sent
                  ;; actually leaves the controller. Tearing down immediately
                  ;; discards it, and the peer then reports a timeout instead
                  ;; of the reason we gave it.
                  (dotimes (i 20) (ble:hci-pump conn 100))))))))))
(sb-ext:exit)
