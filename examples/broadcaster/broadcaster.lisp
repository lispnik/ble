;;;; A beacon: extended advertising, and the long-range PHY.
;;;;
;;;; The eighth example, and the only device here that never accepts a
;;;; connection. Everything before it advertises in order to be connected to;
;;;; a broadcaster advertises because the advertisement *is* the message. No
;;;; GATT database, no ATT, no pairing -- just a payload, repeated.
;;;;
;;;; Two limits of legacy advertising are what this exists to get past.
;;;;
;;;; Thirty-one octets. That is the whole legacy payload, and it goes quickly:
;;;; flags cost three, a name costs its length plus two, and a single 128-bit
;;;; service UUID costs eighteen. The Nordic UART example cannot advertise its
;;;; own service UUID for exactly this reason and has to be found by name. An
;;;; extended advertising set carries 251, so this one advertises the same
;;;; UUID in full, with a name and a payload beside it.
;;;;
;;;; And the PHY. Legacy advertising is 1M only. An extended set can be put on
;;;; the Coded PHY, which trades data rate for range -- the difference between
;;;; a device heard across a room and one heard across a building. It is not
;;;; free: a Coded advertisement occupies the air for several times as long,
;;;; and a scanner not listening on Coded will never see it at all. That last
;;;; part is the one that costs people days. A whole fleet can be advertising
;;;; steadily and a 1M-only scan reports an empty world, with no error
;;;; anywhere to suggest otherwise.
;;;;
;;;; examples/scanner/ is the other half: START-EXTENDED-SCAN listens on both
;;;; PHYs, so it can hear this.
;;;;
;;;; Its own package, using only exported symbols, for the same reason the
;;;; others are.

(defpackage #:broadcaster
  (:use #:common-lisp)
  (:export #:run #:payload #:legacy-payload #:*name*))

(in-package #:broadcaster)

(defparameter *name* "Lisp Beacon")

;;; The advertising set. A controller supports several, each with its own
;;; parameters, address and payload; this uses one and names it rather than
;;; scattering a bare 0 through the code.
(defconstant +set-handle+ 0)

;;; --- the payload --------------------------------------------------------

(defun payload (&key (name *name*) (counter 0) (max-length 251))
  "What this beacon broadcasts.

The full Nordic UART service UUID, a name, and a manufacturer-data record
carrying a counter so a watcher can see the thing is live rather than a
retained copy of one report.

0xFFFF as the company identifier is the one the SIG reserves for testing and
internal use. Putting a real company's number on a payload that is not theirs
is how a beacon ends up being decoded by somebody else's app."
  (ble:adv-data :flags '(:general-discoverable :no-bredr)
                :name name
                :services-128 (list ble:+nus-service-uuid-le+)
                :company-id #xFFFF
                :manufacturer (let ((v (make-array 4 :element-type '(unsigned-byte 8))))
                                (dotimes (i 4 v)
                                  (setf (aref v i) (ldb (byte 8 (* 8 i)) counter))))
                :max-length max-length))

(defun legacy-payload (&key (name *name*))
  "The same thing, built for a legacy advertisement, to show what does not fit.

Signals. That is the point of it: it is here so the compile-check can assert
the limit rather than describe it, and so the number in the error message is
the real one rather than one written in a comment that has drifted."
  (payload :name name :max-length 31))

;;; --- running it ---------------------------------------------------------

(defun run (&key (dev nil) (seconds 60) (phy :coded) (interval-ms 200)
                 (update-seconds 10) (name *name*))
  "Broadcast until SECONDS elapses. PHY is :CODED or :1M.

Nothing here opens a GATT server or waits for a connection, because an
extended set configured this way is non-connectable and non-scannable: there
is no scan response to ask for and no connection to make. A watcher sees the
payload or it does not.

UPDATE-SECONDS must stay comfortably longer than INTERVAL-MS, and that is not
a tuning preference. Setting the data of a live advertising set restarts it,
so a beacon that rewrites its payload as often as it advertises restarts
itself before it has transmitted and goes out on the air never -- while every
HCI command returns success, because each one individually did. This example
was written that way first and was invisible to a scanner sitting next to it
until the two numbers were pulled apart."
  (let ((dev (or dev (ble:default-hci-dev)))
        (counter 0))
    (ble:install-adapter-teardown)
    (ble:with-hci-user-socket (sock dev)
      (let ((addr (ble:static-random-address (ble:smp-random-octets sock 6))))
        ;; PARAMETERS FIRST, and the order is not stylistic. Setting the
        ;; parameters is what CREATES the advertising set; an extended set
        ;; carries its own address, set per handle rather than for the
        ;; controller as a whole -- which is what lets several sets advertise
        ;; as different devices at once -- but there is no handle to attach an
        ;; address to until the set exists. The spec is explicit: addressing a
        ;; set that does not exist earns Unknown Advertising Identifier
        ;; (0x42). This example had it the other way round and worked anyway,
        ;; because the Realtek dongles it was written on create the set
        ;; implicitly and say nothing; a Barrot BT5.4 controller refuses it
        ;; exactly as written. Two chipsets, one of them lenient, is how a
        ;; spec violation stays invisible.
        (ble:set-extended-adv-parameters sock +set-handle+
                                         :interval-ms interval-ms :phy phy)
        (ble:set-adv-set-random-address sock +set-handle+ addr)
        (let ((data (payload :name name :counter counter)))
          (ble:set-extended-adv-data sock +set-handle+ data)
          (format t "~&~A broadcasting on hci~D as ~A~%~
                     PHY ~(~A~), ~D octet(s) of payload -- ~
                     ~:[~;more than a legacy advertisement can hold~]~%"
                  name dev (ble:format-mac addr) phy (length data)
                  (> (length data) 31))
          (when (eq phy :coded)
            (format t "~&note: a scanner that does not listen on the Coded PHY~%~
                       will not see this at all, and will not say so~%"))
          (force-output))
        (ble:set-extended-adv-enable sock +set-handle+ t)
        (unwind-protect
             ;; Re-publish occasionally so a watcher can tell a live beacon
             ;; from a retained copy of one report -- but rarely, for the
             ;; reason in the docstring.
             (let ((deadline (+ (get-internal-real-time)
                                (* seconds internal-time-units-per-second))))
               (loop while (< (get-internal-real-time) deadline)
                     ;; Plain SLEEP: there is no connection to service and
                     ;; nothing arriving to read. A broadcaster is the one
                     ;; device here with no receive path at all.
                     do (sleep (min update-seconds
                                    (max 1 (round (- deadline (get-internal-real-time))
                                                  internal-time-units-per-second))))
                        (incf counter)
                        (ble:set-extended-adv-data
                         sock +set-handle+ (payload :name name :counter counter))
                        (format t "~&  payload updated (~D), still broadcasting~%"
                                counter)
                        (force-output)))
          (ignore-errors (ble:set-extended-adv-enable sock +set-handle+ nil))
          (format t "~&stopped after ~D update(s)~%" counter)
          (force-output))))))
