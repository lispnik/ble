;;;; Load the sensor and check its database, without a radio.
;;;;
;;;; The example cannot be run in CI -- it needs an adapter -- so this at least
;;;; keeps it compiling and asserts the layout it publishes. An example that
;;;; has quietly stopped building is worse than no example.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename "./")) :ignore-inherited-configuration))
;; Load the SYSTEM, not the file. Loading a file by hand can succeed for the
;; wrong reason -- the reader picks up whatever package is current -- while
;; the system fails for a consumer. This is the check that the examples build
;; the way somebody else would build them.
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble/examples))

(defvar *problems* 0)
(defun check (ok fmt &rest args)
  (format t "~&[~A] ~?~%" (if ok " ok " "FAIL") fmt args)
  (unless ok (incf *problems*)))

;; --- the measurement encoding ------------------------------------------
(let ((m (heart-rate:heart-rate-measurement 72)))
  (check (= 2 (length m)) "a narrow measurement is two octets")
  (check (zerop (logand (aref m 0) 1)) "with the uint8 format bit clear")
  (check (= 72 (aref m 1)) "and the rate in the second octet"))

(let ((m (heart-rate:heart-rate-measurement 300)))
  (check (= 1 (logand (aref m 0) 1)) "a rate over 255 sets the uint16 bit")
  (check (= 300 (+ (aref m 1) (ash (aref m 2) 8))) "little-endian, as always"))

(let ((m (heart-rate:heart-rate-measurement 60 :energy-expended 500)))
  (check (= #x08 (logand (aref m 0) #x08)) "energy expended sets its flag")
  (check (= 500 (+ (aref m 2) (ash (aref m 3) 8))) "and follows the rate"))

(let ((m (heart-rate:heart-rate-measurement 60 :rr-intervals '(1024 512))))
  (check (= #x10 (logand (aref m 0) #x10)) "RR intervals set their flag")
  (check (= 6 (length m)) "flags, rate, and two 16-bit intervals")
  (check (= 1024 (+ (aref m 2) (ash (aref m 3) 8))) "in order"))

(let ((m (heart-rate:heart-rate-measurement 60 :sensor-contact :not-detected)))
  (check (= #x04 (logand (aref m 0) #x06))
         "contact supported but not detected is 0b10, not 0b11"))

;; --- the database ------------------------------------------------------
(let* ((sensor (heart-rate:build-server))
       (server (slot-value sensor 'heart-rate::server))
       (uuids (mapcar #'ble:uuid-string
                      (mapcar #'ble:gatt-service-entry-uuid
                              (reverse (ble:gatt-server-services server))))))
  (check (equal '("1800" "1801" "180D") uuids)
         "Generic Access first, then Generic Attribute, then Heart Rate")
  (let ((chars (loop for a across (ble:gatt-server-attributes server)
                     collect (ble:uuid-string (ble:gatt-attribute-uuid a)))))
    (check (member "2A37" chars :test #'string=) "the measurement is present")
    (check (member "2A38" chars :test #'string=) "and the body sensor location")
    (check (member "2A39" chars :test #'string=) "and the control point"))
  (let* ((h (slot-value sensor 'heart-rate::measurement-handle))
         (attr (ble:gatt-find-attribute server h)))
    (check (not (member :read (ble:gatt-attribute-permissions attr)))
           "the measurement is notify-only -- the profile forbids reading it")
    (check (ble:gatt-find-attribute server (1+ h))
           "and has a CCCD after it to subscribe on")))

(check (find-package :heart-rate-client)
       "the client half loads too")

(format t "~&~%HEART RATE CHECK: ~:[clean~;~:*~D problem(s)~]~%" *problems*)
(sb-ext:exit :code (if (zerop *problems*) 0 1))
