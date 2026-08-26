;;;; Check the pseudo-terminal half, which needs no radio at all.
;;;;
;;;; Unusually for an example here, the interesting part is fully testable
;;;; without hardware: a PTY is a kernel object, and whether bytes written to
;;;; the master come out of the slave is a question the kernel answers on any
;;;; machine. What running against a radio proves is only that the other end
;;;; of the pump is a Bluetooth device rather than a loopback.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename "./")) :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble/examples))

(defvar *problems* 0)
(defun check (ok fmt &rest args)
  (format t "~&[~A] ~?~%" (if ok " ok " "FAIL") fmt args)
  (unless ok (incf *problems*)))

(let ((pty (nus-pty:open-pty)))
  (unwind-protect
       (progn
         (check (nus-pty:pty-name pty) "a pseudo-terminal opens")
         (check (search "/dev/" (nus-pty:pty-name pty))
                "and names a device node: ~A" (nus-pty:pty-name pty))
         ;; The property the whole example rests on: what goes in the master
         ;; comes out of the slave, which is what lets any serial program be
         ;; pointed at it without knowing anything about radios.
         ;;
         ;; The slave is opened FIRST and kept open, which is not incidental:
         ;; a master whose slave nobody has opened discards what is written to
         ;; it. That is also true of the bridge -- it happily runs before
         ;; anyone starts screen, and everything the peer sends in the meantime
         ;; is gone.
         (with-open-file (slave (nus-pty:pty-name pty)
                                :direction :io :if-exists :overwrite
                                :element-type '(unsigned-byte 8))
           ;; Raw mode is re-applied AFTER the open. Opening a pty slave
           ;; resets its line discipline on at least macOS, so settings made
           ;; while nobody had it open are gone by the time anybody does --
           ;; which showed up here as the master reading back its own writes,
           ;; the signature of echo. A real consumer does not care, because
           ;; screen and minicom set their own modes on opening; a bridge
           ;; feeding `cat\' does, and this is the check that would notice.
           (funcall (find-symbol "SET-RAW" :nus-pty) (nus-pty:pty-name pty))
           (let ((write (find-symbol "PTY-WRITE" :nus-pty))
                 (read (find-symbol "PTY-READ" :nus-pty)))
             (funcall write pty (map '(simple-array (unsigned-byte 8) (*))
                                     #'char-code "hello pty"))
             (sleep 0.2)
             (let ((got (make-array 0 :element-type '(unsigned-byte 8)
                                      :adjustable t :fill-pointer t)))
               (loop repeat 64 while (listen slave)
                     do (vector-push-extend (read-byte slave) got))
               (check (string= "hello pty" (map 'string #'code-char got))
                      "what is written to the master comes out of the slave: ~S"
                      (map 'string #'code-char got)))
             ;; And the direction a terminal types on.
             (write-sequence (map '(simple-array (unsigned-byte 8) (*))
                                  #'char-code "typed")
                             slave)
             (finish-output slave)
             (sleep 0.2)
             (let ((got (funcall read pty)))
               (check (and got (string= "typed" (map 'string #'code-char got)))
                      "and what is written to the slave arrives at the master: ~S"
                      (and got (map 'string #'code-char got)))))))
    (nus-pty:close-pty pty)))

;; Closing twice must not explode: BRIDGE closes in an unwind-protect that can
;; run after an error that already closed it.
(let ((pty (nus-pty:open-pty)))
  (nus-pty:close-pty pty)
  (check (progn (nus-pty:close-pty pty) t) "closing an already-closed pty is safe"))

(check (fboundp (find-symbol "BRIDGE" :nus-pty)) "the bridge itself is defined")

(format t "~&~%NUS PTY CHECK: ~:[clean~;~:*~D problem(s)~]~%" *problems*)
(sb-ext:exit :code (if (zerop *problems*) 0 1))
