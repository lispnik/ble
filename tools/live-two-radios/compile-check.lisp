;;; Compile every form of the live harness without touching a radio.
;;; Definitions are evaluated so later forms see them; the one top-level form
;;; that drives hardware is compiled but not run. build-server is pure, so it
;;; also gets called and its layout inspected.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename "./")) :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))

(defvar *problems* 0)

(defun note (fmt &rest args)
  (incf *problems*)
  (format t "~&  ~?~%" fmt args))

(defun check-file (path)
  (format t "~&==== ~A ====~%" path)
  (let ((*package* (find-package :cl-user)))
    (with-open-file (in path)
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            do (handler-bind
                   ((warning (lambda (c)
                               (unless (typep c 'style-warning)
                                 (note "~A: ~A" (type-of c) c))
                               (muffle-warning c)))
                    (error (lambda (c) (note "ERROR: ~A" c))))
                 (cond
                   ((and (consp form) (eq (car form) 'in-package))
                    (setf *package* (find-package (second form))))
                   ((and (consp form)
                         (member (car form) '(require asdf:initialize-source-registry
                                              sb-ext:exit)))
                    nil)
                   ((and (consp form) (eq (car form) 'handler-bind)) nil)
                   ;; definitions: evaluate, so later forms resolve them
                   ;; Definitions are evaluated so later forms see them --
                   ;; including DEFPACKAGE, without which the IN-PACKAGE that
                   ;; follows it names a package that does not exist yet.
                   ((and (consp form)
                         (member (car form) '(defpackage defparameter defvar
                                              defconstant defun defstruct)))
                    (ignore-errors (eval form)))
                   ;; anything else drives hardware: compile only
                   (t (ignore-errors (compile nil `(lambda () ,form))))))))))

(check-file "tools/live-two-radios/pick-adapters.lisp")
(check-file "tools/live-two-radios/peripheral.lisp")

;; build-server is pure, so check what it actually produces.
(let ((f (find-symbol "BUILD-SERVER" :two-radios-peripheral)))
  (if (and f (fboundp f))
      (let* ((s (funcall f))
             (get (lambda (slot) (slot-value s (find-symbol slot :two-radios-peripheral))))
             (server (funcall get "SERVER"))
             (ffe1 (funcall get "FFE1")) (cccd1 (funcall get "CCCD1"))
             (ffe2 (funcall get "FFE2")) (cccd2 (funcall get "CCCD2")))
        (format t "~&  built: ~D attributes, ~D services~%"
                (ble:gatt-attribute-count server)
                (length (ble:gatt-server-services server)))
        (unless (= cccd1 (1+ ffe1)) (note "FFE1 CCCD is not right after its value"))
        (unless (= cccd2 (1+ ffe2)) (note "FFE2 CCCD is not right after its value"))
        ;; every characteristic declaration must precede its value handle
        (dolist (h (list ffe1 ffe2))
          (let ((decl (ble:gatt-find-attribute server (1- h))))
            (unless (and decl (equalp (ble:uuid16 #x2803)
                                      (ble:gatt-attribute-uuid decl)))
              (note "no declaration immediately before handle ~D" h))
            (unless (= h (ble:u16-le (ble:gatt-attribute-value decl) 1))
              (note "declaration before ~D points elsewhere" h)))))
      (note "build-server was not defined")))

(check-file "tools/live-two-radios/central.lisp")
(format t "~&~%COMPILE CHECK: ~:[clean~;~:*~D problem(s)~]~%" *problems*)
(sb-ext:exit :code (if (zerop *problems*) 0 1))
