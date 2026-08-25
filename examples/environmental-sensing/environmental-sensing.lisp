;;;; A Bluetooth environmental sensor.
;;;;
;;;; The fourth example, and the smallest, written for one property none of
;;;; the others have: a service carrying the same characteristic more than
;;;; once. This device has three Temperatures -- indoor, outdoor, and a probe
;;;; -- and they are all 0x2A6E. Nothing in the characteristic distinguishes
;;;; them. A client that looks up 0x2A6E and stops has silently picked one of
;;;; three, and will report the wrong room's temperature forever without ever
;;;; seeing an error.
;;;;
;;;; What tells them apart is their descriptors, which is the other half of
;;;; the point. A Characteristic User Description (0x2901) names one for a
;;;; person; an ES Measurement (0x290C) says how it samples. Both hang off the
;;;; characteristic and neither is a CCCD, so this is the example that needed
;;;; GATT-ADD-DESCRIPTOR to be more than a special case.
;;;;
;;;; Its own package, using only exported symbols, for the same reason the
;;;; others are.

(defpackage #:environmental-sensing
  (:use #:common-lisp)
  (:export #:temperature #:humidity #:pressure #:es-measurement
           #:build-server #:run #:*name*))

(in-package #:environmental-sensing)

(defparameter *name* "Lisp Weather")

;;; --- the values ---------------------------------------------------------
;;;
;;; No medical floats here. Environmental Sensing uses plain scaled integers,
;;; each characteristic with its own fixed resolution, and the resolution is
;;; the only thing to get wrong -- there is no exponent on the wire saying
;;; what was meant, so a client and a server that disagree simply disagree.

(defun temperature (celsius)
  "Temperature (0x2A6E): a signed 16-bit count of hundredths of a degree."
  (let ((v (round (* celsius 100)))
        (out (make-array 2 :element-type '(unsigned-byte 8))))
    (unless (<= -32768 v 32767)
      (error "temperature: ~A C is outside a signed 16-bit hundredth" celsius))
    (setf (aref out 0) (ldb (byte 8 0) v)
          (aref out 1) (ldb (byte 8 8) v))
    out))

(defun humidity (percent)
  "Humidity (0x2A6F): an unsigned 16-bit count of hundredths of a percent."
  (let ((v (round (* percent 100)))
        (out (make-array 2 :element-type '(unsigned-byte 8))))
    (unless (<= 0 v 65535)
      (error "humidity: ~A%% is outside an unsigned 16-bit hundredth" percent))
    (setf (aref out 0) (ldb (byte 8 0) v)
          (aref out 1) (ldb (byte 8 8) v))
    out))

(defun pressure (pascals)
  "Pressure (0x2A6D): an unsigned 32-bit count of tenths of a pascal.

Tenths of a pascal, not hectopascals, which is what a weather report shows
and what makes this the easiest of the three to be wrong about by a factor of
ten thousand. Standard atmosphere is 101325 Pa, so 1013250 here."
  (let ((v (round (* pascals 10)))
        (out (make-array 4 :element-type '(unsigned-byte 8))))
    (unless (<= 0 v #xFFFFFFFF)
      (error "pressure: ~A Pa is outside an unsigned 32-bit tenth" pascals))
    (dotimes (i 4) (setf (aref out i) (ldb (byte 8 (* 8 i)) v)))
    out))

;;; --- the ES Measurement descriptor --------------------------------------

;;; Sampling functions. Instantaneous is what a sensor read on demand does;
;;; the others describe a device that aggregates over its measurement period.
(defconstant +sampling-unspecified+   #x00)
(defconstant +sampling-instantaneous+ #x01)
(defconstant +sampling-arithmetic-mean+ #x02)

;;; The Application field says what the sensor is measuring against, and the
;;; SIG defines values for placements like indoor and outdoor. This example
;;; sends Unspecified rather than guessing at numbers it has not checked --
;;; a wrong application code is a confident lie about where the sensor is,
;;; where an unspecified one is merely quiet. The User Description below
;;; carries the placement instead, which is legible to a person either way.
(defconstant +application-unspecified+ #x00)

(defun es-measurement (&key (sampling +sampling-instantaneous+)
                            (period-seconds 0) (update-seconds 1)
                            (application +application-unspecified+)
                            (uncertainty #xFF))
  "The eleven octets of an ES Measurement descriptor (0x290C).

Flags, a sampling function, a measurement period and an update interval as
24-bit second counts, an application, and an uncertainty in half-percent
units where 0xFF means `not known'. The two 24-bit fields are why this is a
function: a three-octet integer is exactly the sort of thing that gets
written as four and shifts everything after it."
  (let ((out (make-array 11 :element-type '(unsigned-byte 8)))
        (i 0))
    (flet ((u16 (v) (setf (aref out i) (ldb (byte 8 0) v)
                          (aref out (1+ i)) (ldb (byte 8 8) v))
                    (incf i 2))
           (u24 (v) (dotimes (k 3) (setf (aref out (+ i k)) (ldb (byte 8 (* 8 k)) v)))
                    (incf i 3))
           (u8 (v) (setf (aref out i) v) (incf i)))
      (u16 0)                           ; flags: none defined for this use
      (u8 sampling)
      (u24 period-seconds)
      (u24 update-seconds)
      (u8 application)
      (u8 uncertainty))
    out))

;;; --- the database -------------------------------------------------------

(defstruct sensor
  "The server and, for each reading, the handle to publish it on.

READINGS is an alist of (NAME . HANDLE) rather than three named slots
because the whole point of the device is that the three temperatures are
indistinguishable except by name."
  server readings)

(defun add-reading (server name value &key (uuid ble:+char-temperature+)
                                           (sampling +sampling-instantaneous+))
  "One characteristic, its CCCD, and the two descriptors that say which it is.

The order is not free. GATT-ADD-CHARACTERISTIC lays down the declaration, the
value and the CCCD; descriptors belong to the characteristic they follow, so
these have to go on before the next characteristic begins."
  (let ((handle (ble:gatt-add-characteristic
                 server :uuid uuid :properties '(:read :notify) :value value)))
    (ble:gatt-add-descriptor server :uuid ble:+descriptor-user-description+
                                    :value name)
    (ble:gatt-add-descriptor server :uuid ble:+descriptor-es-measurement+
                                    :value (es-measurement :sampling sampling))
    (cons name handle)))

(defun build-server ()
  "Generic Access, Generic Attribute, and one Environmental Sensing service
carrying three temperatures, a humidity and a pressure."
  (let ((server (ble:make-gatt-server :mtu 23)))
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
    (ble:gatt-add-service server ble:+service-environmental-sensing+)
    (make-sensor
     :server server
     :readings
     (list
      ;; Three of the same UUID, in one service, on purpose.
      (add-reading server "Indoor"  (temperature 21.5))
      (add-reading server "Outdoor" (temperature 4.25))
      ;; A probe that averages rather than sampling instantaneously, so the
      ;; ES Measurement descriptors are not all identical either.
      (add-reading server "Probe"   (temperature 62.0)
                   :sampling +sampling-arithmetic-mean+)
      (add-reading server "Humidity" (humidity 47.5)
                   :uuid ble:+char-humidity+)
      (add-reading server "Pressure" (pressure 101325)
                   :uuid ble:+char-pressure+)))))

;;; --- running it ---------------------------------------------------------

(defun drifting (start step lo hi)
  "A reading that wanders inside its limits, so a viewer sees it move."
  (let ((v start))
    (lambda ()
      (setf v (max lo (min hi (+ v (* step (- (random 3) 1)))))))))

(defun run (&key (dev nil) (seconds nil) (interval-ms 2000))
  "Advertise as an environmental sensor and update every reading periodically.

Each value is stored on its attribute as well as notified, because unlike a
heart rate or a temperature measurement these characteristics are readable:
an environmental reading is meaningful whenever you ask for it, which is why
the profile permits the read that the other two forbid."
  (let* ((sensor (build-server))
         (server (sensor-server sensor))
         (dev (or dev (ble:default-hci-dev)))
         (sources (list (cons "Indoor"   (drifting 21.5 0.25 18 26))
                        (cons "Outdoor"  (drifting 4.25 0.5 -10 20))
                        (cons "Probe"    (drifting 62.0 1.0 40 80))
                        (cons "Humidity" (drifting 47.5 1.0 20 90))
                        (cons "Pressure" (drifting 101325 25 99000 103000))))
         (next 0))
    (ble:install-adapter-teardown)
    (ble:with-hci-user-socket (sock dev)
      (let ((addr (ble:static-random-address (ble:smp-random-octets sock 6))))
        (ble:set-random-address sock addr)
        (ble:set-adv-parameters sock :adv-type ble:+adv-ind+ :own-addr-type 1)
        (ble:set-adv-data sock (ble:adv-data
                                :flags '(:general-discoverable :no-bredr)
                                :name *name*
                                :services-16 (list ble:+service-environmental-sensing+)))
        (format t "~&~A advertising on hci~D as ~A with ~D reading(s)~%"
                *name* dev (ble:format-mac addr) (length (sensor-readings sensor)))
        (force-output)
        (ble:serve-peripheral
         server sock :seconds seconds
         :on-connect (lambda (conn peer ptype)
                       (declare (ignore conn))
                       (format t "~&connected: ~A (~(~A~))~%"
                               (ble:format-mac peer) ptype)
                       (force-output))
         :on-disconnect (lambda (conn)
                          (declare (ignore conn))
                          (format t "~&disconnected; advertising again~%")
                          (force-output))
         :on-tick
         (lambda (conn request)
           (declare (ignore request))
           (let ((now (get-internal-real-time)))
             (when (> now next)
               (setf next (+ now (round (* interval-ms
                                           internal-time-units-per-second)
                                        1000)))
               (dolist (reading (sensor-readings sensor))
                 (let* ((name (car reading))
                        (handle (cdr reading))
                        (source (cdr (assoc name sources :test #'string=)))
                        (raw (funcall source))
                        (value (cond ((string= name "Humidity") (humidity raw))
                                     ((string= name "Pressure") (pressure raw))
                                     (t (temperature raw)))))
                   ;; Stored as well as notified: these are readable, and a
                   ;; client that reads instead of subscribing must not get
                   ;; the value the device booted with.
                   (ble:gatt-set-value server handle value)
                   (ble:gatt-notify server conn handle value)))
               (format t "~&  updated ~D reading(s)~%"
                       (length (sensor-readings sensor)))
               (force-output)))))))))
