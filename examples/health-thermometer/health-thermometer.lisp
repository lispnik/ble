;;;; A Bluetooth health thermometer.
;;;;
;;;; The companion to examples/heart-rate/, and written for the two things it
;;;; does differently. A heart rate sensor notifies; a thermometer indicates,
;;;; and an indication is acknowledged -- only one may be outstanding at a
;;;; time, so a peripheral that sends the next before the confirmation arrives
;;;; is violating GATT however well it appears to work. The other difference
;;;; is the encoding: temperatures are IEEE-11073 FLOATs, not integers.
;;;;
;;;; Both characteristics are here on purpose, in one device: Temperature
;;;; Measurement indicates the settled reading, Intermediate Temperature
;;;; notifies the reading on its way there. Side by side, the difference is
;;;; the whole point of the example.
;;;;
;;;; Its own package, using only exported symbols, for the same reason the
;;;; heart rate example is: if this file needs anything from inside #:ble then
;;;; the library has not finished the job.

(defpackage #:health-thermometer
  (:use #:common-lisp)
  (:export #:temperature-measurement #:medfloat #:build-server #:run #:*name*))

(in-package #:health-thermometer)

(defparameter *name* "Lisp Thermometer")

;;; Values the Health Thermometer profile defines and the SIG does not name
;;; centrally, so they live here rather than in ble's assigned-numbers file.

;;; Temperature Type (0x2A1D): where on the body the reading was taken.
(defconstant +type-armpit+ #x01)
(defconstant +type-body+   #x02)
(defconstant +type-ear+    #x03)
(defconstant +type-mouth+  #x06)

;;; A Common Profile and Service error code, not an HTS invention: 0xE0-0xFF
;;; is the range the Core specification sets aside for them, and 0xFF is Out
;;; of Range. Returned when a write to Measurement Interval falls outside the
;;; Valid Range descriptor this service publishes.
(defconstant +err-out-of-range+ #xFF)

;;; What this thermometer will accept as a measurement interval, in seconds.
;;; The pair is published as the Valid Range descriptor, so a client can find
;;; out what is acceptable rather than discovering it by being refused.
(defconstant +interval-min+ 1)
(defconstant +interval-max+ 3600)

;;; --- IEEE-11073 FLOAT ---------------------------------------------------
;;;
;;; Personal health devices do not use IEEE 754. A 32-bit medical FLOAT is a
;;; signed 8-bit exponent and a signed 24-bit mantissa, value = mantissa x
;;; 10^exponent, sent little-endian so the mantissa comes first.
;;;
;;; Decimal, not binary, and that is the point: 36.85 is exactly 3685 x 10^-2,
;;; where in binary floating point it is not exactly anything. A thermometer
;;; reporting hundredths of a degree can say precisely what it measured.

(defconstant +medfloat-nan+ #x007FFFFF
  "The mantissa/exponent pattern reserved for `not a number'.")

(defun medfloat (value &optional (exponent -2))
  "Encode VALUE as a 32-bit IEEE-11073 FLOAT, little-endian.

EXPONENT is the power of ten the mantissa is scaled by; -2 gives hundredths,
which is the resolution a clinical thermometer reports. Pass :NAN for a
reading that is unavailable, which the profile has a defined pattern for and
which is the honest thing to send when a sensor has not settled.

Signals if VALUE cannot be represented, rather than silently truncating the
mantissa into a different temperature."
  (let* ((mantissa (if (eq value :nan)
                       +medfloat-nan+
                       (round (* value (expt 10 (- exponent))))))
         (exponent (if (eq value :nan) 0 exponent)))
    (unless (typep exponent '(signed-byte 8))
      (error "medfloat: exponent ~D does not fit in a signed octet" exponent))
    (unless (or (eq value :nan) (<= (- (expt 2 23)) mantissa (1- (expt 2 23))))
      (error "medfloat: ~A at 10^~D needs a mantissa of ~D, which does not fit ~
              in 24 signed bits" value exponent mantissa))
    (let ((m (ldb (byte 24 0) mantissa))          ; two's complement, 24 bits
          (out (make-array 4 :element-type '(unsigned-byte 8))))
      (setf (aref out 0) (ldb (byte 8 0) m)
            (aref out 1) (ldb (byte 8 8) m)
            (aref out 2) (ldb (byte 8 16) m)
            (aref out 3) (ldb (byte 8 0) exponent))
      out)))

;;; --- the measurement ----------------------------------------------------

(defun temperature-measurement (celsius &key timestamp type (units :celsius))
  "Encode a Temperature Measurement (0x2A1C) or Intermediate Temperature.

Both characteristics carry the same structure: a flags octet, the temperature
as a 32-bit medical FLOAT, then a timestamp and a type if the flags say so.

CELSIUS may be :NAN for a reading that is not available. TIMESTAMP is a
universal time, or T for now. TYPE is one of the Temperature Type values.
UNITS is :CELSIUS or :FAHRENHEIT -- the flag and the number must agree, and
nothing on the wire can tell you they do not, so this does not convert: pass
the value in the units you name."
  (let* ((flags (logior (ecase units (:celsius 0) (:fahrenheit 1))
                        (if timestamp #x02 0)
                        (if type #x04 0)))
         (stamp (when timestamp
                  (ble:date-time (if (eq timestamp t)
                                     (get-universal-time) timestamp))))
         (out (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer t)))
    (vector-push-extend flags out)
    (loop for b across (medfloat celsius) do (vector-push-extend b out))
    (when stamp (loop for b across stamp do (vector-push-extend b out)))
    (when type (vector-push-extend type out))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;;; --- the database -------------------------------------------------------

(defstruct thermometer
  "The server, the handles it sends on, and the indication discipline.

OUTSTANDING is what makes this a thermometer rather than a heart rate sensor
with different numbers: while an indication is unconfirmed, no further one
may be sent. SENT-AT is when it went out, so a peer that never confirms can
be noticed instead of stalling the device forever."
  server measurement-handle intermediate-handle interval-handle
  (interval 5) (outstanding nil) (sent-at 0))

(defun build-server (&key (interval 5))
  "Generic Access, Generic Attribute, and the Health Thermometer service.

The first two are not optional in practice: iOS reads Device Name and
Appearance the moment it connects, and without Generic Attribute a client has
no way to learn the database changed."
  (let ((server (ble:make-gatt-server :mtu 23))
        (thermo nil))
    (ble:gatt-add-service server ble:+service-generic-access+)
    (ble:gatt-add-characteristic server :uuid ble:+char-device-name+
                                        :properties '(:read) :value *name*)
    (ble:gatt-add-characteristic server :uuid ble:+char-appearance+
                                        :properties '(:read)
                                        :value (ble:appearance
                                                ble:+appearance-generic-thermometer+))
    (ble:gatt-add-service server ble:+service-generic-attribute+)
    (ble:gatt-add-characteristic server :uuid ble:+char-service-changed+
                                        :properties '(:indicate)
                                        :value (ble:service-changed-range))
    (ble:gatt-add-service server ble:+service-health-thermometer+)
    ;; Indicate only. Like the heart rate measurement, the profile forbids
    ;; reading it -- but for a different reason worth keeping straight: a
    ;; temperature is a reading taken at a moment, and the profile wants the
    ;; client to have acknowledged each one rather than sampling whatever
    ;; happens to be in the attribute.
    (let ((measurement (ble:gatt-add-characteristic
                        server :uuid ble:+char-temperature-measurement+
                               :properties '(:indicate))))
      (ble:gatt-add-characteristic server :uuid ble:+char-temperature-type+
                                          :properties '(:read)
                                          :value (vector +type-mouth+))
      ;; The contrast, in the same service: intermediate readings are
      ;; notified. They are provisional and frequent, and losing one costs
      ;; nothing, which is exactly when a notification is the right choice.
      (let ((intermediate (ble:gatt-add-characteristic
                           server :uuid ble:+char-intermediate-temperature+
                                  :properties '(:notify))))
        (let ((interval-handle
                (ble:gatt-add-characteristic
                 server :uuid ble:+char-measurement-interval+
                        :properties '(:read :write :indicate)
                        :value (let ((v (make-array 2 :element-type '(unsigned-byte 8))))
                                 (setf (aref v 0) (ldb (byte 8 0) interval)
                                       (aref v 1) (ldb (byte 8 8) interval))
                                 v)
                        :on-write
                        (lambda (s a v)
                          (declare (ignore s a))
                          (if (< (length v) 2)
                              +err-out-of-range+
                              (let ((secs (+ (aref v 0) (ash (aref v 1) 8))))
                                (if (<= +interval-min+ secs +interval-max+)
                                    (progn (setf (thermometer-interval thermo) secs)
                                           nil)   ; accepted
                                    +err-out-of-range+)))))))
          ;; Mandatory whenever Measurement Interval is writable, and this is
          ;; the reason ble grew GATT-ADD-DESCRIPTOR: a client is entitled to
          ;; discover the bounds rather than probe for them.
          (ble:gatt-add-descriptor
           server :uuid ble:+descriptor-valid-range+
                  :value (let ((v (make-array 4 :element-type '(unsigned-byte 8))))
                           (setf (aref v 0) (ldb (byte 8 0) +interval-min+)
                                 (aref v 1) (ldb (byte 8 8) +interval-min+)
                                 (aref v 2) (ldb (byte 8 0) +interval-max+)
                                 (aref v 3) (ldb (byte 8 8) +interval-max+))
                           v))
          (setf thermo (make-thermometer :server server
                                         :measurement-handle measurement
                                         :intermediate-handle intermediate
                                         :interval-handle interval-handle
                                         :interval interval))
          thermo)))))

;;; --- running it ---------------------------------------------------------

(defconstant +indication-timeout-seconds+ 30
  "The ATT transaction timeout. A peer that has not confirmed in this long is
not going to; the Core specification says the bearer is finished at that
point, and continuing to wait is how a device hangs instead of recovering.")

(defun settling-temperature ()
  "A reading that climbs towards a body temperature and wobbles there.

Written so a viewer sees the intermediate values move and then settle, which
is the behaviour the two characteristics exist to distinguish."
  (let ((current 3400))                      ; hundredths of a degree
    (lambda (&key settled)
      (let ((target (if settled 3685 (+ 3400 (random 300)))))
        (setf current (+ current (round (- target current) 3)))
        (/ (+ current (- (random 11) 5)) 100.0)))))

(defun run (&key (dev nil) (seconds nil) (interval 5)
                 (temp-fn (settling-temperature)) (intermediate-ms 500))
  "Advertise as a health thermometer, notifying intermediate readings and
indicating the settled one every INTERVAL seconds.

DEV defaults to the first USB adapter, since hciN numbering drifts across
reboots and the built-in radio on a Pi cannot do everything a dongle can."
  (let* ((thermo (build-server :interval interval))
         (dev (or dev (ble:default-hci-dev))))
    (ble:install-adapter-teardown)
    (ble:with-hci-user-socket (sock dev)
      ;; A fresh random address each run. Clients cache a GATT database by
      ;; address and will not re-discover an unbonded peer, so during
      ;; development a new address is the difference between seeing changes
      ;; and seeing a stale copy of the database forever.
      (let ((addr (ble:static-random-address (ble:smp-random-octets sock 6)))
            (next-intermediate 0)
            (next-measurement 0))
        (ble:set-random-address sock addr)
        (ble:set-adv-parameters sock :adv-type ble:+adv-ind+ :own-addr-type 1)
        ;; 0x1809 in the payload is what makes a thermometer app list this
        ;; device at all, however correct the database behind it.
        (ble:set-adv-data sock (ble:adv-data
                                :flags '(:general-discoverable :no-bredr)
                                :name *name*
                                :services-16 (list ble:+service-health-thermometer+)))
        (format t "~&~A advertising on hci~D as ~A~%" *name* dev
                (ble:format-mac addr))
        (force-output)
        (flet ((units (ms) (round (* ms internal-time-units-per-second) 1000)))
          (ble:serve-peripheral
           (thermometer-server thermo) sock
           :seconds seconds
           :on-connect (lambda (conn peer ptype)
                         (declare (ignore conn))
                         (setf (thermometer-outstanding thermo) nil)
                         (format t "~&connected: ~A (~(~A~))~%"
                                 (ble:format-mac peer) ptype)
                         (force-output))
           :on-disconnect (lambda (conn)
                            (declare (ignore conn))
                            ;; The confirmation is never coming from a peer
                            ;; that has gone away. Clearing this is what lets
                            ;; the next client be served at all.
                            (setf (thermometer-outstanding thermo) nil)
                            (format t "~&disconnected; advertising again~%")
                            (force-output))
           :on-tick
           (lambda (conn request)
             ;; The confirmation arrives as an ordinary PDU, so this is where
             ;; a peripheral learns it may indicate again. Without it the
             ;; device would either stall forever or, worse, keep sending.
             (when (eql request ble:+att-handle-value-cfm+)
               (setf (thermometer-outstanding thermo) nil)
               (format t "~&  confirmed~%")
               (force-output))
             (let ((now (get-internal-real-time)))
               ;; A peer that subscribed and then stopped confirming must not
               ;; wedge the thermometer.
               (when (and (thermometer-outstanding thermo)
                          (> now (+ (thermometer-sent-at thermo)
                                    (* +indication-timeout-seconds+
                                       internal-time-units-per-second))))
                 (format t "~&  no confirmation in ~Ds; giving up on it~%"
                         +indication-timeout-seconds+)
                 (force-output)
                 (setf (thermometer-outstanding thermo) nil))
               ;; Intermediates: fire and forget, as often as we like.
               (when (> now next-intermediate)
                 (setf next-intermediate (+ now (units intermediate-ms)))
                 (ble:gatt-notify (thermometer-server thermo) conn
                                  (thermometer-intermediate-handle thermo)
                                  (temperature-measurement
                                   (funcall temp-fn) :type +type-mouth+)))
               ;; The settled reading: one at a time, and only once the last
               ;; was acknowledged.
               (when (and (> now next-measurement)
                          (not (thermometer-outstanding thermo)))
                 (setf next-measurement
                       (+ now (units (* 1000 (thermometer-interval thermo)))))
                 (let ((c (funcall temp-fn :settled t)))
                   (when (ble:gatt-notify
                          (thermometer-server thermo) conn
                          (thermometer-measurement-handle thermo)
                          (temperature-measurement c :timestamp t
                                                     :type +type-mouth+)
                          :indications t)
                     (setf (thermometer-outstanding thermo) t
                           (thermometer-sent-at thermo) now)
                     (format t "~&  ~,2F C indicated~%" c)
                     (force-output))))))))))))
