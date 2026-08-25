;;;; Load the REPL and check it, without a radio.
;;;;
;;;; Almost all of this example is checkable here, which is unusual for one
;;;; that needs an adapter to be useful: the framing and the evaluation are
;;;; pure, and they are also the two parts that are easy to get subtly wrong.
;;;; What running proves is only that the bytes arrive.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename "./")) :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble/examples))

(defvar *problems* 0)
(defun check (ok fmt &rest args)
  (format t "~&[~A] ~?~%" (if ok " ok " "FAIL") fmt args)
  (unless ok (incf *problems*)))

;; --- knowing when a form is whole ---------------------------------------
;;
;; The question a REPL over a stream has to answer, and the one a terminal
;; gets to ignore. A write arrives as one ATT PDU; a long form arrives as
;; several, and nothing marks the end.
(check (lisp-repl:complete-form-p "(+ 1 2)") "a balanced form is complete")
(check (not (lisp-repl:complete-form-p "(+ 1 2")) "an unbalanced one is not")
(check (not (lisp-repl:complete-form-p "(list \"unterminated"))
       "nor is one with an unterminated string -- which a paren count alone ~
        would call complete")
(check (lisp-repl:complete-form-p "42") "an atom is a form")
(check (not (lisp-repl:complete-form-p "")) "nothing is not")
(check (not (lisp-repl:complete-form-p "   "))
       "and neither is whitespace, which a newline-terminated reader would ~
        have evaluated")
(check (lisp-repl:complete-form-p ")")
       "a malformed form counts as complete: there is nothing to wait for, ~
        and the peer is owed the complaint")

;; --- evaluating ----------------------------------------------------------
(check (string= "3" (lisp-repl:eval-string "(+ 1 2)"))
       "a value comes back printed")
(check (search "\"hi\"" (lisp-repl:eval-string "\"hi\""))
       "a string comes back readably, with its quotes")
(check (search "; No values" (lisp-repl:eval-string "(values)"))
       "no values says so rather than coming back empty")
(check (search ";" (lisp-repl:eval-string "(values 1 2)"))
       "several values are all reported")

;; Output is captured, because a form whose whole purpose is to print is
;; otherwise silent over a wire.
(let ((r (lisp-repl:eval-string "(progn (princ \"side\") :done)")))
  (check (search "side" r) "what a form prints comes back with it")
  (check (search ":DONE" r) "and so does its value"))

;; Errors are rendered, never signalled: a condition escaping here takes the
;; peripheral down and the peer just sees a device that stopped answering.
(check (search "DIVISION-BY-ZERO" (lisp-repl:eval-string "(/ 1 0)"))
       "an error is rendered, not signalled")
(check (search "UNDEFINED-FUNCTION" (lisp-repl:eval-string "(no-such-function)"))
       "including an undefined function")
(check (stringp (lisp-repl:eval-string "(")) "and a read error is survivable")

;; --- the reader is not a back door --------------------------------------
;;
;; #. evaluates at READ time, before any check a REPL makes afterwards could
;; run. Binding *READ-EVAL* to NIL while reading is the whole defence.
(let ((r (lisp-repl:eval-string "#.(+ 1 2)")))
  (check (not (string= "3" r))
         "#. does not evaluate at read time -- it is refused, not computed")
  (check (search "READER-ERROR" (string-upcase r))
         "and it is refused as a reader error: ~A" r))

;; --- the package persists ------------------------------------------------
(multiple-value-bind (reply package)
    (lisp-repl:eval-string "(in-package :ble)")
  (declare (ignore reply))
  (check (eq package (find-package :ble))
         "IN-PACKAGE changes the package the next form is read in")
  ;; And a symbol that only exists there now reads without a prefix. Checked
  ;; by whether it resolves at all, not against a particular value -- the
  ;; point is the package, and pinning the number would make this test fail
  ;; the day somebody changes a default for unrelated reasons.
  (let ((r (lisp-repl:eval-string "*att-rx-mtu*" :package package)))
    (check (every #'digit-char-p (string-trim '(#\Newline) r))
           "which is what makes it useful: an unqualified symbol resolves (~A)" r)
    (let ((elsewhere (lisp-repl:eval-string "*att-rx-mtu*"
                                            :package (find-package :cl-user))))
      (check (search "UNBOUND" (string-upcase elsewhere))
             "and the same text read in CL-USER is an unbound symbol, which ~
              is what makes the persistence matter: ~A"
             (string-trim '(#\Newline) elsewhere)))))

;; --- the database --------------------------------------------------------
(let* ((s (lisp-repl:build-server))
       (server (slot-value s 'lisp-repl::server))
       (uuids (loop for a across (ble:gatt-server-attributes server)
                    collect (ble:uuid-string (ble:gatt-attribute-uuid a)))))
  (check (member "6E400002-B5A3-F393-E0A9-E50E24DCCA9E" uuids :test #'string=)
         "it is a NUS device: RX present")
  (check (member "6E400003-B5A3-F393-E0A9-E50E24DCCA9E" uuids :test #'string=)
         "and TX")
  ;; The part that matters most in this particular example.
  (let ((rx (ble:gatt-find-attribute server (slot-value s 'lisp-repl::rx-handle)))
        (tx (ble:gatt-find-attribute server (slot-value s 'lisp-repl::tx-handle))))
    (check (ble:gatt-attribute-security rx)
           "RX requires an encrypted link BY DEFAULT -- this is a remote ~
            evaluator, and an unauthenticated one is a remote shell")
    (check (ble:gatt-attribute-security tx) "and so does TX")))

(let* ((s (lisp-repl:build-server :secure nil))
       (server (slot-value s 'lisp-repl::server)))
  (check (not (ble:gatt-attribute-security
               (ble:gatt-find-attribute server (slot-value s 'lisp-repl::rx-handle))))
         ":secure nil turns it off, for a bench with nothing else in range"))

;; --- framing across writes ----------------------------------------------
;;
;; A form split over three writes must evaluate once, not three times, and
;; not at all until it is whole.
(let* ((s (lisp-repl:build-server))
       (server (slot-value s 'lisp-repl::server))
       (write (ble:gatt-attribute-on-write
               (ble:gatt-find-attribute server (slot-value s 'lisp-repl::rx-handle))))
       (take (find-symbol "TAKE-FORM" :lisp-repl)))
  (flet ((send (text)
           (funcall write server nil
                    (map '(simple-array (unsigned-byte 8) (*)) #'char-code text))))
    (send "(+ 1")
    (check (null (funcall take s)) "half a form is not taken")
    (send " 2")
    (check (null (funcall take s)) "still not")
    (send ")")
    (check (string= "(+ 1 2)" (funcall take s)) "and the whole one is")
    (check (null (funcall take s)) "leaving nothing behind")
    ;; Two forms in one write are two evaluations, not one and a remainder
    ;; quietly dropped.
    (send "(+ 1 2)(* 3 4)")
    (check (string= "(+ 1 2)" (funcall take s)) "two forms in one write: first")
    (check (string= "(* 3 4)" (string-left-trim " " (funcall take s))) "then second")))

(check (find-package :lisp-repl-client) "the client half loads too")

(format t "~&~%LISP REPL CHECK: ~:[clean~;~:*~D problem(s)~]~%" *problems*)
(sb-ext:exit :code (if (zerop *problems*) 0 1))
