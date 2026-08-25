;;;; A serial port over BLE: the Nordic UART Service.
;;;;
;;;; The seventh example, and the first that is not a SIG profile at all. NUS
;;;; is Nordic's, and it is here because a great many vendor peripherals speak
;;;; it -- anything built on Nordic's stack tends to -- so it is the shape a
;;;; consumer meets when they buy a device rather than design one.
;;;;
;;;; Two things are different from every example before it.
;;;;
;;;; The UUIDs are 128-bit. Every other example uses the SIG's 16-bit numbers,
;;;; which fit in two octets and can be written as integers; a vendor has no
;;;; assigned number, so it uses a full UUID and pays for it in every ATT PDU.
;;;; They are written here the way Nordic publishes them, and BLE:UUID128
;;;; reverses them, because the wire form is the reverse of the written one
;;;; and a hand-reversed literal cannot be reviewed -- nobody can tell a
;;;; transposition from a decision by looking at sixteen octets.
;;;;
;;;; And the naming is from the device's point of view, which catches
;;;; everybody once: a host WRITES to RX and READS notifications from TX. RX
;;;; is what the device receives, not what you do.
;;;;
;;;; Its own package, using only exported symbols, for the same reason the
;;;; others are.

(defpackage #:nordic-uart
  (:use #:common-lisp)
  (:export #:build-server #:run #:handle-line #:*name*))

(in-package #:nordic-uart)

(defparameter *name* "Lisp UART")

;;; --- the terminal -------------------------------------------------------
;;;
;;; Something worth typing at, so the example is a device rather than an echo.
;;; Pure, so the whole command set is testable without a radio.

(defun handle-line (line &key (uptime 0))
  "Answer one line of input. Returns the string to send back.

A serial protocol is only a serial protocol -- there is no framing here that
GATT does not give us, and each write arrives as one ATT PDU. That is the
convenience and the trap: a peer sending more than one MTU's worth gets it
split across writes, and reassembling that is the application's problem, not
NUS's."
  (let ((line (string-trim '(#\Space #\Tab #\Return #\Newline) line)))
    (cond
      ((string-equal line "help")
       "commands: help, time, uptime, echo <text>, mtu")
      ((string-equal line "time")
       (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
         (format nil "~2,'0D:~2,'0D:~2,'0D" h m s)))
      ((string-equal line "uptime")
       (format nil "~D second(s)" uptime))
      ((and (> (length line) 5) (string-equal "echo " (subseq line 0 5)))
       (subseq line 5))
      ((string-equal line "mtu")
       "ask the stack, not me -- but every reply here is cut to fit one")
      ((zerop (length line)) "")
      (t (format nil "no such command: ~A" line)))))

;;; --- the device ---------------------------------------------------------

(defstruct terminal
  "The server, the two handles a serial device needs, and what has been typed.

PENDING is what the write handler leaves for the tick to answer. The handler
cannot reply itself: its return value decides the Write Response, and a
notification sent from inside it would go out before that response, which is
a peer seeing the answer to a question it has not been told we received."
  server rx-handle tx-handle tx-cccd (started 0) (lines 0) (pending nil))

(defun build-server ()
  "Generic Access, Generic Attribute, and the Nordic UART Service.

The service and both characteristics are 128-bit, which is what makes this
database look different from every other example's: three sixteen-octet UUIDs
where the others have two-octet ones."
  (let ((server (ble:make-gatt-server :mtu 23))
        (term nil))
    (ble:gatt-add-service server ble:+service-generic-access+)
    (ble:gatt-add-characteristic server :uuid ble:+char-device-name+
                                        :properties '(:read) :value *name*)
    (ble:gatt-add-characteristic server :uuid ble:+char-appearance+
                                        :properties '(:read)
                                        :value (ble:appearance
                                                ble:+appearance-generic-tag+))
    (ble:gatt-add-service server ble:+service-generic-attribute+)
    (ble:gatt-add-characteristic server :uuid ble:+char-service-changed+
                                        :properties '(:indicate)
                                        :value (ble:service-changed-range))
    ;; A 128-bit service UUID, passed as the octet vector the library holds it
    ;; in rather than an integer. GATT-ADD-SERVICE takes either.
    (ble:gatt-add-service server ble:+nus-service-uuid-le+)
    ;; RX: the device receives here, so the host writes. Write Without
    ;; Response as well as Write, because a terminal is a stream and waiting
    ;; for a response to every keystroke is what makes a serial link feel
    ;; slow -- a peer that wants acknowledgement can still use Write.
    (let ((rx (ble:gatt-add-characteristic
               server :uuid ble:+nus-rx-uuid-le+
                      :properties '(:write :write-without-response)
                      :on-write (lambda (s a v)
                                  (declare (ignore s a))
                                  (setf (terminal-lines term)
                                        (1+ (terminal-lines term)))
                                  (push (map 'string #'code-char v)
                                        (terminal-pending term))
                                  nil))))
      (multiple-value-bind (tx tx-cccd)
          (ble:gatt-add-characteristic server :uuid ble:+nus-tx-uuid-le+
                                              :properties '(:notify))
        (setf term (make-terminal :server server :rx-handle rx
                                  :tx-handle tx :tx-cccd tx-cccd))
        term))))

;;; --- running it ---------------------------------------------------------

(defun run (&key (dev nil) (seconds nil))
  "Advertise as a Nordic UART device and answer what is typed at it.

Connect with nRF Connect, or with examples/nordic-uart/read-it.lisp, and send
`help'."
  (let* ((term (build-server))
         (dev (or dev (ble:default-hci-dev))))
    (setf (terminal-started term) (get-universal-time))
    (ble:install-adapter-teardown)
    (ble:with-hci-user-socket (sock dev)
      (let ((addr (ble:static-random-address (ble:smp-random-octets sock 6))))
        (ble:set-random-address sock addr)
        (ble:set-adv-parameters sock :adv-type ble:+adv-ind+ :own-addr-type 1)
        ;; The name only. A 128-bit service UUID is sixteen octets, and a
        ;; legacy advertisement has thirty-one to spend in total -- with the
        ;; flags and the name there is no room, which is exactly why NUS
        ;; devices are found by name and why the extended advertising example
        ;; exists.
        (ble:set-adv-data sock (ble:adv-data
                                :flags '(:general-discoverable :no-bredr)
                                :name *name*))
        (format t "~&~A advertising on hci~D as ~A~%~
                   (the NUS UUID does not fit beside the name in a legacy~%~
                    advertisement, so this device is found by name)~%"
                *name* dev (ble:format-mac addr))
        (force-output)
        (ble:serve-peripheral
         (terminal-server term) sock
         :seconds seconds
         :on-connect (lambda (conn peer ptype)
                       (declare (ignore conn))
                       (setf (terminal-pending term) nil)
                       (format t "~&connected: ~A (~(~A~))~%"
                               (ble:format-mac peer) ptype)
                       (force-output))
         :on-disconnect (lambda (conn)
                          (declare (ignore conn))
                          (setf (terminal-pending term) nil)
                          (format t "~&disconnected; advertising again~%")
                          (force-output))
         :on-tick
         (lambda (conn request)
           (declare (ignore request))
           (let ((input (nreverse (terminal-pending term))))
             (setf (terminal-pending term) nil)
             (dolist (line input)
               (format t "~&  <- ~S~%" line)
               (let ((reply (handle-line
                             line :uptime (- (get-universal-time)
                                             (terminal-started term)))))
                 (when (plusp (length reply))
                   ;; GATT-NOTIFY truncates to the negotiated MTU rather than
                   ;; failing, which is the right behaviour for a terminal and
                   ;; the wrong assumption for a protocol -- a caller that
                   ;; needs all of it must chunk.
                   (ble:gatt-notify (terminal-server term) conn
                                    (terminal-tx-handle term) reply)
                   (format t "~&  -> ~S~%" reply)))
               (force-output)))))))))
