;;;; A Bluetooth heart rate sensor.
;;;;
;;;; Deliberately written in its own package, using only exported symbols. If
;;;; this file needs anything from inside #:ble then the library has not
;;;; finished the job -- a peripheral should be something a consumer can write.

(defpackage #:heart-rate
  (:use #:common-lisp)
  (:export #:heart-rate-measurement #:build-server #:run #:*name*))

(in-package #:heart-rate)

(defparameter *name* "Lisp HRM")

;;; Values the Heart Rate profile defines and the SIG does not name centrally,
;;; so they live here rather than in ble's assigned-numbers file.
(defconstant +location-chest+ #x01)
(defconstant +control-point-reset-energy+ #x01)
(defconstant +control-point-not-supported+ #x80)

;;; --- the measurement ----------------------------------------------------
;;;
;;; Heart Rate Measurement (0x2A37) is a flags octet followed by whatever the
;;; flags say is present. Pure, so it is testable without a radio; the format
;;; is where a sensor is most likely to be quietly wrong.

(defun heart-rate-measurement (bpm &key energy-expended rr-intervals
                                        (sensor-contact :detected))
  "Encode one Heart Rate Measurement.

BPM is carried as one octet when it fits and two when it does not -- flags bit
0 says which, and choosing it automatically means a caller cannot get the pair
out of step. SENSOR-CONTACT is :DETECTED, :NOT-DETECTED, or :UNSUPPORTED;
RR-INTERVALS are in 1/1024 second units, which is what the profile specifies
and not milliseconds."
  (let* ((wide (> bpm 255))
         (flags (logior (if wide 1 0)
                        (ecase sensor-contact
                          (:unsupported  #x00)   ; bits 1-2 both clear
                          (:not-detected #x04)   ; supported, not detected
                          (:detected     #x06))  ; supported, detected
                        (if energy-expended #x08 0)
                        (if rr-intervals #x10 0)))
         (out (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer t)))
    (flet ((u8 (v) (vector-push-extend (logand v #xFF) out))
           (u16 (v) (vector-push-extend (logand v #xFF) out)
                    (vector-push-extend (logand (ash v -8) #xFF) out)))
      (u8 flags)
      (if wide (u16 bpm) (u8 bpm))
      (when energy-expended (u16 energy-expended))
      (dolist (rr rr-intervals) (u16 rr)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;;; --- the database -------------------------------------------------------

(defstruct sensor
  "The server, and the handles a running sensor needs to notify on."
  server measurement-handle (energy-expended 0))

(defun build-server ()
  "Generic Access, Generic Attribute, and the Heart Rate service.

The first two are not optional in practice: iOS reads Device Name and
Appearance the moment it connects, and without Generic Attribute a client has
no way to learn the database changed."
  (let ((server (ble:make-gatt-server :mtu 23))
        (sensor nil))
    (ble:gatt-add-service server ble:+service-generic-access+)
    (ble:gatt-add-characteristic server :uuid ble:+char-device-name+
                                        :properties '(:read) :value *name*)
    (ble:gatt-add-characteristic server :uuid ble:+char-appearance+
                                        :properties '(:read)
                                        :value (ble:appearance
                                                ble:+appearance-heart-rate-belt+))
    (ble:gatt-add-service server ble:+service-generic-attribute+)
    (ble:gatt-add-characteristic server :uuid ble:+char-service-changed+
                                        :properties '(:indicate)
                                        :value (ble:service-changed-range))
    (ble:gatt-add-service server ble:+service-heart-rate+)
    ;; Notify only. The profile forbids reading the measurement: a value read
    ;; at an arbitrary moment says nothing useful about a heart rate.
    (let ((measurement (ble:gatt-add-characteristic
                        server :uuid ble:+char-heart-rate-measurement+
                               :properties '(:notify))))
      (ble:gatt-add-characteristic server :uuid ble:+char-body-sensor-location+
                                          :properties '(:read)
                                          :value (vector +location-chest+))
      (setf sensor (make-sensor :server server :measurement-handle measurement))
      ;; Control point: the only defined opcode resets energy expended, and
      ;; anything else must be refused with 0x80 rather than ignored.
      (ble:gatt-add-characteristic
       server :uuid ble:+char-heart-rate-control-point+ :properties '(:write)
              :on-write (lambda (s a v)
                          (declare (ignore s a))
                          (if (and (plusp (length v))
                                   (= +control-point-reset-energy+ (aref v 0)))
                              (progn (setf (sensor-energy-expended sensor) 0) nil)
                              +control-point-not-supported+)))
      sensor)))

;;; --- running it ---------------------------------------------------------

(defun wandering-bpm ()
  "A plausible resting heart rate that drifts, so a viewer sees it move."
  (let ((bpm 72))
    (lambda ()
      (setf bpm (max 50 (min 110 (+ bpm (- (random 7) 3))))))))

(defun run (&key (dev nil) (seconds nil) (bpm-fn (wandering-bpm))
                 (interval-ms 1000))
  "Advertise as a heart rate sensor and notify a measurement once a second.

DEV defaults to the first USB adapter, since hciN numbering drifts across
reboots and the built-in radio on a Pi cannot do everything a dongle can."
  (let* ((sensor (build-server))
         (dev (or dev (ble:default-hci-dev))))
    (ble:install-adapter-teardown)
    (ble:with-hci-user-socket (sock dev)
      ;; A fresh random address each run. Clients cache a GATT database by
      ;; address and will not re-discover an unbonded peer, so during
      ;; development a new address is the difference between seeing changes
      ;; and seeing a stale copy of the database forever.
      (let ((addr (ble:static-random-address (ble:smp-random-octets sock 6)))
            (next 0) (beats 0))
        (ble:set-random-address sock addr)
        (ble:set-adv-parameters sock :adv-type ble:+adv-ind+ :own-addr-type 1)
        ;; 0x180D in the payload is what makes a heart rate app list this
        ;; device at all, however correct the database behind it.
        (ble:set-adv-data sock (ble:adv-data
                                :flags '(:general-discoverable :no-bredr)
                                :name *name*
                                :services-16 (list ble:+service-heart-rate+)))
        (format t "~&~A advertising on hci~D as ~A~%" *name* dev
                (ble:format-mac addr))
        (force-output)
        (ble:serve-peripheral
         (sensor-server sensor) sock
         :seconds seconds
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
         (lambda (conn)
           (let ((now (get-internal-real-time)))
             (when (> now next)
               (setf next (+ now (round (* interval-ms
                                           internal-time-units-per-second)
                                        1000)))
               (let ((bpm (funcall bpm-fn)))
                 ;; gatt-notify refuses unless the client has subscribed, so
                 ;; there is nothing to gate here.
                 (when (ble:gatt-notify
                        (sensor-server sensor) conn
                        (sensor-measurement-handle sensor)
                        (heart-rate-measurement
                         bpm :energy-expended (sensor-energy-expended sensor)))
                   (incf beats)
                   (incf (sensor-energy-expended sensor))
                   (format t "~&  ~D bpm~:[~; (notified)~]~%" bpm t)
                   (force-output)))))))))))
