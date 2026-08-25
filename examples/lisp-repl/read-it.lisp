;;;; Talk to the Lisp REPL over Bluetooth.
;;;;
;;;; The central half. Pairs -- the peripheral will not accept a write on an
;;;; unencrypted link -- then reads forms and prints what came back.
;;;;
;;;; Two things it has to do that the other clients do not.
;;;;
;;;; It pairs before it can say anything at all, rather than reading something
;;;; harmless first: the REPL's RX characteristic requires encryption, so an
;;;; unpaired write is refused with Insufficient Authentication.
;;;;
;;;; And it reads replies to a terminator rather than to a timeout. NUS has no
;;;; framing, a printed result may contain newlines, and a client that stopped
;;;; at the first quiet moment would cut long answers in half and report the
;;;; halves as complete.
(defpackage #:lisp-repl-client
  (:use #:common-lisp)
  (:export #:run #:eval-remote #:demo #:*demo-script*))
(in-package #:lisp-repl-client)

(defun eval-remote (nus text &key (timeout-ms 30000))
  "Send TEXT to the remote REPL and return its reply as a string.

TIMEOUT-MS is generous on purpose: the far end is evaluating whatever was
sent, and how long that takes is not this function's business. A timeout here
means the peer stopped answering, not that the form was slow -- there is no
way to tell those apart over a pipe, which is worth knowing before relying on
it for anything that must not be run twice."
  (ble:nus-send nus (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                         text))
  (let ((reply (make-string-output-stream))
        (deadline (+ (get-internal-real-time)
                     (round (* timeout-ms internal-time-units-per-second)
                            1000))))
    (loop
      (when (> (get-internal-real-time) deadline)
        (return (values (get-output-stream-string reply) :timeout)))
      (let ((chunk (ble:nus-recv nus 500)))
        (cond
          ((or (null chunk) (eq chunk :disconnected))
           (when (eq chunk :disconnected)
             (return (values (get-output-stream-string reply) :disconnected))))
          (t
           (let ((end (position lisp-repl:+end-of-reply+ chunk)))
             (write-string (map 'string #'code-char (subseq chunk 0 (or end (length chunk))))
                           reply)
             (when end
               (return (values (get-output-stream-string reply) :ok))))))))))

(defun pair-with (chan mac addr-type)
  "Pair as the central. The REPL refuses everything until this succeeds."
  (let ((local (ble:hci-read-bd-addr :sock (ble:hci-conn-sock chan))))
    (handler-case
        (let ((session (ble:smp-pair chan :role :central
                                          :local-addr local
                                          :local-addr-type :public
                                          :peer-addr (ble:parse-mac mac)
                                          :peer-addr-type addr-type
                                          :io-capability :no-input-no-output)))
          (format t "~&paired; encryption -> ~A~%"
                  (ble:smp-start-encryption session))
          (force-output)
          t)
      (ble:smp-error (e)
        (format t "~&pairing failed: ~A~%" e)
        (force-output)
        nil))))

(defparameter *demo-script*
  '(("(machine-instance)"
     "Where am I? Evaluated on the Pi, over the air.")
    ("(list (lisp-implementation-type) (lisp-implementation-version))"
     "And what is running there.")
    ("(with-open-file (s \"/sys/class/thermal/thermal_zone0/temp\") (/ (read s) 1000.0))"
     "A real sensor reading -- no characteristic was defined for this, and
      none had to be.")
    ("(defun celsius->f (c) (+ 32 (* 9/5 c)))"
     "Now change the running image. This function did not exist a moment ago.")
    ("(celsius->f 21)"
     "...and call it. The device learned a new capability mid-conversation,
      which is the difference between a REPL and a protocol.")
    ("(in-package :ble)"
     "Move into the library's own package.")
    ("(mapcar (lambda (a) (list (hci-adapter-index a) (hci-adapter-bus a)))
              (list-hci-adapters))"
     "Ask the Bluetooth stack, over Bluetooth, what radios it has.")
    ("(hci-socket-acl-credits (hci-conn-sock lisp-repl:*connection*))"
     "Now the self-referential part: the flow-control window of the very link
      this answer is travelling on.")
    ("(hci-read-rssi lisp-repl:*connection*)"
     "And its signal strength, in dBm. The device is telling us how well it
      can hear us, using the link it is telling us over.")
    ("(/ 1 0)"
     "Errors come back as values rather than taking the device down.")
    ("#.(+ 1 2)"
     "And the reader is not a back door: #. is refused, not computed."))
  "What DEMO runs, as (FORM . NARRATION) pairs.

Chosen to build an argument rather than to list features: where am I, what
can I see, now change me, now ask me about myself.")

(defun demo (mac &key (dev 2) (addr-type :random))
  "Run the demo script against the REPL at MAC, narrating as it goes."
  (run mac :dev dev :addr-type addr-type :interactive nil
           :script *demo-script*))

(defun one-line (text)
  "Collapse a source-wrapped string onto one line.

The script is written to be readable in the file, so its strings carry the
indentation of the file rather than of the output."
  (let ((out (make-string-output-stream)) (spacing nil))
    (loop for c across text
          do (if (member c '(#\Space #\Newline #\Tab))
                 (setf spacing t)
                 (progn (when (and spacing (plusp (file-position out)))
                          (write-char #\Space out))
                        (setf spacing nil)
                        (write-char c out))))
    (get-output-stream-string out)))

(defun run (mac &key (dev 2) (addr-type :random) (forms nil) (script nil)
                     (interactive t))
  "Connect to the REPL at MAC and evaluate FORMS, then read from the terminal.

FORMS is a list of strings, sent in order -- useful for a scripted session or
a test. INTERACTIVE then hands over to the keyboard; an empty line or `:quit'
ends it."
  (ble:install-adapter-teardown)
  ;; Connect, pair, THEN attach -- not WITH-NUS-HCI, which would subscribe on
  ;; the way in. The REPL's TX characteristic requires an encrypted link, so
  ;; the subscribe is refused until pairing has happened, and connecting and
  ;; subscribing in one call leaves nowhere to do it.
  (ble:with-att-channel
      (chan (ble:hci-user-att-connect (ble:parse-mac mac) :addr-type addr-type
                                      :dev dev :init-phys #x01 :timeout 25))
    (format t "~&connected to ~A~%" mac)
    (force-output)
    (unless (pair-with chan mac addr-type)
      (return-from run nil))
    (let ((nus (ble:nus-attach chan :mtu 247
                                    :bdaddr-type (if (eq addr-type :random) 1 0))))
      (format t "~&NUS attached, MTU ~D~%" (ble:nus-mtu nus))
      (force-output)
    ;; A REPL is a conversation, not a transfer, but each form still costs a
    ;; round trip and the reply may be several packets.
    (ble:hci-connection-update (ble:nus-fd nus)
                               :min-interval-ms 15 :max-interval-ms 30)
    (dolist (text forms)
      (format t "~&~A~%" text)
      (multiple-value-bind (reply how) (eval-remote nus text)
        (format t "~A~@[  [~(~A~)]~]~%" reply (unless (eq how :ok) how))
        (force-output)))
    (loop for (text narration) in script
          for n from 1
          do (format t "~&~%~D. ~A~%" n (one-line narration))
             (format t "~&   ~A~%" (one-line text))
             (force-output)
             (multiple-value-bind (reply how) (eval-remote nus text)
               (format t "~&   => ~A~@[  [~(~A~)]~]~%"
                       (string-trim '(#\Newline) reply)
                       (unless (eq how :ok) how))
               (force-output)
               (when (eq how :disconnected) (return))))
    (when interactive
      (format t "~&~%Remote REPL. Blank line or :quit to leave.~%")
      (loop
        (format t "~&remote> ") (force-output)
        (let ((line (read-line *standard-input* nil nil)))
          (when (or (null line)
                    (zerop (length (string-trim " " line)))
                    (string-equal ":quit" (string-trim " " line)))
            (return))
          (multiple-value-bind (reply how) (eval-remote nus line)
            (format t "~A~@[  [~(~A~)]~]~%" reply (unless (eq how :ok) how))
            (force-output)
            (when (eq how :disconnected) (return)))))))))
