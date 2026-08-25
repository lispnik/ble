;;;; Load the thermometer and check its database and encoding, without a radio.
;;;;
;;;; The example cannot be run in CI -- it needs an adapter -- so this at least
;;;; keeps it compiling and asserts what it publishes. An example that has
;;;; quietly stopped building is worse than no example.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename "./")) :ignore-inherited-configuration))
;; Load the SYSTEM, not the file. Loading a file by hand can succeed for the
;; wrong reason -- the reader picks up whatever package is current -- while
;; the system fails for a consumer.
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble/examples))

(defvar *problems* 0)
(defun check (ok fmt &rest args)
  (format t "~&[~A] ~?~%" (if ok " ok " "FAIL") fmt args)
  (unless ok (incf *problems*)))

;; --- IEEE-11073 FLOAT ---------------------------------------------------
;;
;; The encoding this example exists to demonstrate, and the one a thermometer
;; is most likely to be quietly wrong about.
(let ((f (health-thermometer:medfloat 36.85)))
  (check (= 4 (length f)) "a medical FLOAT is four octets")
  (check (= #xFE (aref f 3)) "exponent -2 in the top octet, two's complement")
  (check (= 3685 (+ (aref f 0) (ash (aref f 1) 8) (ash (aref f 2) 16)))
         "36.85 is the integer 3685 scaled by 10^-2, exactly"))

(let ((f (health-thermometer:medfloat -5.5)))
  (check (= #xFE (aref f 3)) "a negative temperature keeps the exponent")
  (check (= (ldb (byte 24 0) -550)
            (+ (aref f 0) (ash (aref f 1) 8) (ash (aref f 2) 16)))
         "and carries a two's complement 24-bit mantissa"))

(let ((f (health-thermometer:medfloat :nan)))
  (check (and (= #xFF (aref f 0)) (= #xFF (aref f 1)) (= #x7F (aref f 2))
              (= 0 (aref f 3)))
         "NaN is the reserved pattern 0x007FFFFF, not a zero"))

;; 8388607 is the largest mantissa there is; one more has to be refused.
(check (health-thermometer:medfloat 8388607 0)
       "the largest representable mantissa is fine")
(check (handler-case (progn (health-thermometer:medfloat 8388608 0) nil)
         (error () t))
       "one more signals rather than truncating into a different temperature")
(check (handler-case (progn (health-thermometer:medfloat 36.85 -100) nil)
         (error () t))
       "and an exponent too large for a signed octet signals too")

;; --- the measurement ----------------------------------------------------
(let ((m (health-thermometer:temperature-measurement 36.85)))
  (check (= 5 (length m)) "flags and a FLOAT, and nothing else by default")
  (check (zerop (logand (aref m 0) 1)) "celsius clears the units bit"))

(let ((m (health-thermometer:temperature-measurement 98.6 :units :fahrenheit)))
  (check (= 1 (logand (aref m 0) 1)) "fahrenheit sets it"))

(let ((m (health-thermometer:temperature-measurement 36.85 :timestamp t)))
  (check (= #x02 (logand (aref m 0) #x02)) "a timestamp sets its flag")
  (check (= 12 (length m)) "and adds seven octets of Date Time"))

(let ((m (health-thermometer:temperature-measurement 36.85 :timestamp t :type 6)))
  (check (= #x06 (logand (aref m 0) #x06)) "type and timestamp flags together")
  (check (= 13 (length m)) "type is one octet, and comes last")
  (check (= 6 (aref m 12)) "and it is the value passed"))

;; --- the database -------------------------------------------------------
(let* ((thermo (health-thermometer:build-server))
       (server (slot-value thermo 'health-thermometer::server))
       (uuids (mapcar #'ble:uuid-string
                      (mapcar #'ble:gatt-service-entry-uuid
                              (reverse (ble:gatt-server-services server))))))
  (check (equal '("1800" "1801" "1809") uuids)
         "Generic Access first, then Generic Attribute, then Health Thermometer")
  (let ((chars (loop for a across (ble:gatt-server-attributes server)
                     collect (ble:uuid-string (ble:gatt-attribute-uuid a)))))
    (check (member "2A1C" chars :test #'string=) "the measurement is present")
    (check (member "2A1D" chars :test #'string=) "and the temperature type")
    (check (member "2A1E" chars :test #'string=) "and the intermediate value")
    (check (member "2A21" chars :test #'string=) "and the measurement interval")
    (check (member "2906" chars :test #'string=)
           "and a Valid Range, which the profile requires once the interval ~
            is writable"))
  ;; The distinction the whole example is about: one indicates, one notifies.
  (let* ((m (slot-value thermo 'health-thermometer::measurement-handle))
         (i (slot-value thermo 'health-thermometer::intermediate-handle))
         (m-attr (ble:gatt-find-attribute server m))
         (i-attr (ble:gatt-find-attribute server i)))
    (check (not (member :read (ble:gatt-attribute-permissions m-attr)))
           "the measurement is indicate-only -- the profile forbids reading it")
    (check (member :indicate (ble:gatt-attribute-permissions m-attr))
           "the settled reading is acknowledged")
    (check (member :notify (ble:gatt-attribute-permissions i-attr))
           "the intermediate reading is not")
    (check (ble:gatt-find-attribute server (1+ m))
           "and each has a CCCD after it to subscribe on")
    (check (ble:gatt-find-attribute server (1+ i)) "both of them"))
  ;; Writing the interval: accepted in range, refused out of it, and refused
  ;; with the profile's own error code rather than a generic one.
  (let* ((h (slot-value thermo 'health-thermometer::interval-handle))
         (attr (ble:gatt-find-attribute server h))
         (write (ble:gatt-attribute-on-write attr))
         (u16 (lambda (n) (let ((v (make-array 2 :element-type '(unsigned-byte 8))))
                            (setf (aref v 0) (ldb (byte 8 0) n)
                                  (aref v 1) (ldb (byte 8 8) n))
                            v))))
    (check (null (funcall write server attr (funcall u16 60)))
           "60 seconds is in range, and accepted")
    (check (= 60 (slot-value thermo 'health-thermometer::interval))
           "and takes effect")
    (check (= #xFF (funcall write server attr (funcall u16 0)))
           "0 is out of range, refused with 0xFF Out of Range")
    (check (= #xFF (funcall write server attr (funcall u16 7200)))
           "and so is two hours")
    (check (= 60 (slot-value thermo 'health-thermometer::interval))
           "a refused write changes nothing")))

(check (find-package :health-thermometer-client)
       "the client half loads too")

(format t "~&~%HEALTH THERMOMETER CHECK: ~:[clean~;~:*~D problem(s)~]~%" *problems*)
(sb-ext:exit :code (if (zerop *problems*) 0 1))
