;;;; A Lisp REPL over Bluetooth.
;;;;
;;;; The tenth example, and the one to read the warning on first.
;;;;
;;;; WHAT THIS IS. A remote evaluator. Anything sent to it runs in this image,
;;;; with this process's privileges, on the machine it is running on. That is
;;;; the point -- it is the same bargain SLIME makes over TCP, and it is a
;;;; genuinely good way to work on a device you cannot easily attach a screen
;;;; to. It is also, exactly, an unauthenticated remote shell if you let it be.
;;;;
;;;; So it requires an encrypted link by default. BUILD-SERVER takes
;;;; :SECURE NIL to turn that off, and the only honest reason to pass it is a
;;;; bench with nothing else in radio range. SERVE-PERIPHERAL's pairing hook
;;;; does the work; see examples/glucose/ for the same mechanism protecting
;;;; something less dangerous.
;;;;
;;;; *READ-EVAL* is bound to NIL while reading. Without that, #. evaluates at
;;;; READ time -- before any of the checks below run, and before the form is
;;;; even a form. A REPL that reads attacker input with *READ-EVAL* true is
;;;; not protected by anything it does afterwards.
;;;;
;;;; WHAT IS INTERESTING ABOUT IT, beyond the obvious. NUS is a pipe with no
;;;; framing: a write arrives as one ATT PDU, a long one arrives as several,
;;;; and nothing marks where a message ends. A terminal can ignore that. A
;;;; REPL cannot -- it has to know when it has a whole form. So this
;;;; accumulates until the reader says the text is complete, which is a
;;;; different question from "contains a newline", and answers replies with an
;;;; explicit terminator rather than hoping the reader stops in the right
;;;; place.
;;;;
;;;; Its own package, using only exported symbols, for the same reason the
;;;; others are.

(defpackage #:lisp-repl
  (:use #:common-lisp)
  (:export #:eval-string #:complete-form-p #:build-server #:run
           #:*name* #:*repl-package* #:+end-of-reply+))

(in-package #:lisp-repl)

(defparameter *name* "Lisp REPL")

(defparameter *repl-package* (find-package :cl-user)
  "Where forms are read and evaluated. A session may change it, and the change
persists, which is what makes IN-PACKAGE work over the wire.")

;;; NUS gives no framing, so the reply needs its own. EOT rather than a
;;; newline, because a printed result may contain newlines and a client
;;; reading until one would cut the answer in half.
(defconstant +end-of-reply+ #x04)

;;; --- reading -------------------------------------------------------------

(defun complete-form-p (text)
  "Does TEXT hold at least one whole form?

The question a REPL over a stream has to answer, and it is not `does it
contain a newline'. An unbalanced form spread over several writes is
incomplete; a malformed one is complete enough to complain about, which is
why a read error other than end-of-file counts as done."
  (handler-case
      (let ((*read-eval* nil))
        (read-from-string text)
        t)
    (end-of-file () nil)
    (error () t)))

;;; --- evaluating ----------------------------------------------------------

(defun eval-string (text &key (package *repl-package*))
  "Read one form from TEXT, evaluate it, and return what a REPL would print.

Returns (VALUES REPLY NEW-PACKAGE). Anything the form writes to standard
output is captured and comes back with the values, because a form whose whole
purpose is to print is otherwise silent over a wire.

Every error is caught and rendered. A REPL that lets a condition escape takes
the peripheral down with it, and the peer -- which cannot see the backtrace --
learns only that the device stopped answering."
  (let ((*package* package)
        (*read-eval* nil))
    (handler-case
        (let* ((form (read-from-string text))
               (output (make-string-output-stream))
               (values (multiple-value-list
                        (let ((*standard-output* output)
                              (*trace-output* output))
                          ;; *READ-EVAL* is restored for the duration of the
                          ;; evaluation itself: a form that is already trusted
                          ;; enough to run may legitimately read.
                          (let ((*read-eval* t)) (eval form)))))
               (printed (get-output-stream-string output)))
          (values
           (with-output-to-string (s)
             (write-string printed s)
             (when (and (plusp (length printed))
                        (char/= (char printed (1- (length printed))) #\Newline))
               (terpri s))
             (if values
                 (format s "~{~S~^ ;~%~}" values)
                 (write-string "; No values" s)))
           *package*))
      (error (e)
        (values (format nil "; ~A: ~A" (type-of e) e) *package*)))))

;;; --- the device ----------------------------------------------------------

(defstruct session
  "The server, the handles, and one conversation's worth of state.

INPUT accumulates until it holds a whole form; OUTPUT is the reply still to be
sent, a chunk per tick. PACKAGE survives between forms so IN-PACKAGE means
what it says."
  server rx-handle tx-handle tx-cccd
  (input "") (output nil) (package (find-package :cl-user))
  (forms 0))

(defun build-server (&key (secure t) (mtu 247))
  "Generic Access and the Nordic UART Service, with the REPL behind it.

SECURE requires an encrypted link before anything can be written to RX. It
defaults to true and should stay that way: see the warning at the top of this
file."
  (let ((server (ble:make-gatt-server :mtu mtu))
        (s nil))
    (ble:gatt-add-service server ble:+service-generic-access+)
    (ble:gatt-add-characteristic server :uuid ble:+char-device-name+
                                        :properties '(:read) :value *name*)
    (ble:gatt-add-service server ble:+nus-service-uuid-le+)
    (let ((rx (ble:gatt-add-characteristic
               server :uuid ble:+nus-rx-uuid-le+
                      :properties '(:write :write-without-response)
                      :security (and secure :encrypted)
                      :on-write (lambda (srv attr v)
                                  (declare (ignore srv attr))
                                  ;; Accumulate only. Evaluating here would
                                  ;; run arbitrary code inside the write
                                  ;; handler, before the Write Response its
                                  ;; return value decides has been sent.
                                  (setf (session-input s)
                                        (concatenate 'string (session-input s)
                                                     (map 'string #'code-char v)))
                                  nil))))
      (multiple-value-bind (tx tx-cccd)
          (ble:gatt-add-characteristic server :uuid ble:+nus-tx-uuid-le+
                                              :properties '(:notify)
                                              :security (and secure :encrypted))
        (setf s (make-session :server server :rx-handle rx
                              :tx-handle tx :tx-cccd tx-cccd))
        s))))

(defun take-form (s)
  "If INPUT holds a whole form, remove and return its text. Else NIL."
  (let ((text (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                                (session-input s))))
    (setf (session-input s) text)
    (when (and (plusp (length text)) (complete-form-p text))
      ;; Take exactly one form: READ-FROM-STRING reports where it stopped, so
      ;; two forms written together are two evaluations rather than one and a
      ;; silently discarded remainder.
      (multiple-value-bind (form end)
          (handler-case (let ((*read-eval* nil)) (read-from-string text))
            (error () (values nil (length text))))
        (declare (ignore form))
        (prog1 (subseq text 0 end)
          (setf (session-input s) (subseq text end)))))))

(defun answer (s text)
  "Queue TEXT as the reply, terminated so the peer knows where it ends."
  (setf (session-output s)
        (concatenate 'string text (string (code-char +end-of-reply+)))))

(defun pump-output (s conn)
  "Send a little of the pending reply. A chunk per tick, not the whole thing.

GATT-NOTIFY truncates to the negotiated MTU rather than failing, so a reply
longer than one packet has to be cut here or it is silently shortened -- and
a REPL that returns most of an answer is worse than one that returns none."
  (let ((out (session-output s)))
    (when (and out (plusp (length out)))
      (let* ((room (max 1 (- (ble:gatt-server-mtu (session-server s)) 3)))
             (n (min room (length out)))
             (chunk (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                         (subseq out 0 n))))
        (when (ble:gatt-notify (session-server s) conn (session-tx-handle s) chunk)
          (setf (session-output s) (subseq out n)))))))

;;; --- running it ----------------------------------------------------------

(defun run (&key (dev nil) (seconds nil) (secure t))
  "Advertise as a Lisp REPL and evaluate what is sent to it.

Connect with examples/lisp-repl/read-it.lisp. With SECURE, which is the
default, the peer must pair before it can write anything."
  (let* ((s (build-server :secure secure))
         (dev (or dev (ble:default-hci-dev)))
         (pairing nil))
    (ble:install-adapter-teardown)
    (ble:with-hci-user-socket (sock dev)
      (let ((addr (ble:static-random-address (ble:smp-random-octets sock 6))))
        (when secure
          (setf pairing (ble:make-peripheral-pairing
                         :local-addr addr
                         :irk (ble:smp-random-octets sock 16)
                         :on-paired (lambda (conn session bond)
                                      (declare (ignore conn session bond))
                                      (format t "~&*** link encrypted -- the ~
                                                 REPL is now reachable ***~%")
                                      (force-output)))))
        (ble:set-random-address sock addr)
        (ble:set-adv-parameters sock :adv-type ble:+adv-ind+ :own-addr-type 1)
        (ble:set-adv-data sock (ble:adv-data
                                :flags '(:general-discoverable :no-bredr)
                                :name *name*))
        (format t "~&~A on hci~D as ~A~:[~;, encrypted link required~]~%"
                *name* dev (ble:format-mac addr) secure)
        (unless secure
          (format t "~&*** UNPROTECTED: anything in radio range can evaluate ~
                     code in this image ***~%"))
        (force-output)
        (ble:serve-peripheral
         (session-server s) sock
         :seconds seconds
         :pairing pairing
         :tick-ms 10
         :on-connect (lambda (conn peer ptype)
                       (declare (ignore conn))
                       (setf (session-input s) "" (session-output s) nil
                             (session-package s) (find-package :cl-user))
                       (format t "~&connected: ~A (~(~A~))~%"
                               (ble:format-mac peer) ptype)
                       (force-output))
         :on-disconnect (lambda (conn)
                          (declare (ignore conn))
                          (format t "~&disconnected after ~D form(s)~%"
                                  (session-forms s))
                          (force-output))
         :on-tick
         (lambda (conn request)
           (declare (ignore request))
           ;; One form per tick at most, and only when the last reply has
           ;; gone. Evaluation can take arbitrarily long -- that is what a
           ;; REPL is -- and starting another while a reply is still going out
           ;; would interleave two answers on a pipe with no framing to
           ;; separate them.
           (cond
             ((and (session-output s) (plusp (length (session-output s))))
              (pump-output s conn))
             (t
              (let ((text (take-form s)))
                (when text
                  (incf (session-forms s))
                  (format t "~&  <- ~A~%" text) (force-output)
                  (multiple-value-bind (reply package)
                      (eval-string text :package (session-package s))
                    (setf (session-package s) package)
                    (answer s reply)
                    (format t "~&  -> ~A~%" reply)
                    (force-output)
                    (pump-output s conn))))))))))))
