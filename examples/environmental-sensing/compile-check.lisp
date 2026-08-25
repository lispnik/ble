;;;; Load the environmental sensor and check its layout, without a radio.
;;;;
;;;; The layout is what this example is about, so it is what gets asserted:
;;;; three characteristics sharing one UUID, each with its own descriptors, in
;;;; an order that decides which descriptor belongs to which.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename "./")) :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble/examples))

(defvar *problems* 0)
(defun check (ok fmt &rest args)
  (format t "~&[~A] ~?~%" (if ok " ok " "FAIL") fmt args)
  (unless ok (incf *problems*)))

;; --- the scaled integers ------------------------------------------------
(let ((v (environmental-sensing:temperature 21.5)))
  (check (= 2 (length v)) "a temperature is two octets")
  (check (= 2150 (+ (aref v 0) (ash (aref v 1) 8)))
         "21.5 C is 2150 hundredths, little-endian"))

(let ((v (environmental-sensing:temperature -12.75)))
  (check (= (ldb (byte 16 0) -1275) (+ (aref v 0) (ash (aref v 1) 8)))
         "and a negative one is two's complement, not a sign bit"))

(let ((v (environmental-sensing:humidity 47.5)))
  (check (= 4750 (+ (aref v 0) (ash (aref v 1) 8))) "47.5% is 4750 hundredths"))

(let ((v (environmental-sensing:pressure 101325)))
  (check (= 4 (length v)) "a pressure is four octets")
  (check (= 1013250 (+ (aref v 0) (ash (aref v 1) 8) (ash (aref v 2) 16)
                       (ash (aref v 3) 24)))
         "standard atmosphere is 1013250 tenths of a pascal"))

(check (handler-case (progn (environmental-sensing:temperature 400) nil)
         (error () t))
       "a temperature past the 16-bit hundredth signals rather than wrapping")
(check (handler-case (progn (environmental-sensing:humidity -1) nil)
         (error () t))
       "and humidity is unsigned, so a negative one is refused")

;; --- the ES Measurement descriptor --------------------------------------
(let ((d (environmental-sensing:es-measurement :period-seconds #x010203
                                               :update-seconds 5)))
  (check (= 11 (length d)) "an ES Measurement descriptor is eleven octets")
  (check (and (zerop (aref d 0)) (zerop (aref d 1))) "flags first, two octets")
  (check (= 1 (aref d 2)) "then the sampling function")
  (check (and (= #x03 (aref d 3)) (= #x02 (aref d 4)) (= #x01 (aref d 5)))
         "then a 24-bit period -- three octets, not four, little-endian")
  (check (and (= 5 (aref d 6)) (zerop (aref d 7)) (zerop (aref d 8)))
         "then a 24-bit update interval")
  (check (= #xFF (aref d 10)) "and an uncertainty of `not known'"))

;; --- the database -------------------------------------------------------
(let* ((sensor (environmental-sensing:build-server))
       (server (slot-value sensor 'environmental-sensing::server))
       (attrs (ble:gatt-server-attributes server))
       (uuids (map 'list (lambda (a) (ble:uuid-string (ble:gatt-attribute-uuid a)))
                   attrs)))
  (check (equal '("1800" "1801" "181A")
                (mapcar #'ble:uuid-string
                        (mapcar #'ble:gatt-service-entry-uuid
                                (reverse (ble:gatt-server-services server)))))
         "Generic Access, Generic Attribute, then Environmental Sensing")
  ;; The property this example exists for.
  (check (= 3 (count "2A6E" uuids :test #'string=))
         "three characteristics share the temperature UUID")
  (check (= 1 (count "2A6F" uuids :test #'string=)) "one humidity")
  (check (= 1 (count "2A6D" uuids :test #'string=)) "one pressure")
  (check (= 5 (count "2901" uuids :test #'string=))
         "and every reading carries a User Description naming it")
  (check (= 5 (count "290C" uuids :test #'string=))
         "and an ES Measurement saying how it samples")

  ;; Descriptor order is not decoration: a descriptor belongs to the
  ;; characteristic it follows, so the four attributes after each value have
  ;; to be the right four, in the right order.
  (let* ((readings (slot-value sensor 'environmental-sensing::readings))
         (first-handle (cdr (first readings))))
    (check (string= "2A6E" (ble:uuid-string
                            (ble:gatt-attribute-uuid
                             (ble:gatt-find-attribute server first-handle))))
           "the first reading's value is a temperature")
    (check (string= "2902" (ble:uuid-string
                            (ble:gatt-attribute-uuid
                             (ble:gatt-find-attribute server (+ first-handle 1)))))
           "its CCCD comes first, laid down with the characteristic")
    (check (string= "2901" (ble:uuid-string
                            (ble:gatt-attribute-uuid
                             (ble:gatt-find-attribute server (+ first-handle 2)))))
           "then the User Description")
    (check (string= "290C" (ble:uuid-string
                            (ble:gatt-attribute-uuid
                             (ble:gatt-find-attribute server (+ first-handle 3)))))
           "then the ES Measurement")
    ;; And the next characteristic's declaration follows immediately, which is
    ;; what a client's range arithmetic depends on.
    (check (string= "2803" (ble:uuid-string
                            (ble:gatt-attribute-uuid
                             (ble:gatt-find-attribute server (+ first-handle 4)))))
           "and then the next characteristic begins"))

  ;; The names really are distinct, which is the only way a client tells the
  ;; three temperatures apart.
  (let ((names (loop for a across attrs
                     when (string= "2901" (ble:uuid-string
                                           (ble:gatt-attribute-uuid a)))
                       collect (map 'string #'code-char
                                    (ble:gatt-attribute-value a)))))
    (check (equal '("Indoor" "Outdoor" "Probe" "Humidity" "Pressure") names)
           "named in order: ~{~A~^, ~}" names)
    (check (= 5 (length (remove-duplicates names :test #'string=)))
           "and no two readings share a name"))

  ;; Readable, unlike the measurements in the other two examples.
  (let ((a (ble:gatt-find-attribute
            server (cdr (first (slot-value sensor
                                           'environmental-sensing::readings))))))
    (check (member :read (ble:gatt-attribute-permissions a))
           "an environmental reading is readable -- it means something at any ~
            moment, which a heart rate does not")
    (check (member :notify (ble:gatt-attribute-permissions a))
           "and notifies as well")))

;; --- what the client would do -------------------------------------------
;;
;; FIND-CHARS-BY-UUID is the library half of this example. Checking it here
;; needs no radio: it is a filter over a list of GATT-CHARs.
(let ((chars (list (ble:make-gatt-char :handle 3 :uuid (ble:uuid16 #x2A6E))
                   (ble:make-gatt-char :handle 7 :uuid (ble:uuid16 #x2A6E))
                   (ble:make-gatt-char :handle 11 :uuid (ble:uuid16 #x2A6F)))))
  (check (= 2 (length (ble:find-chars-by-uuid chars (ble:uuid16 #x2A6E))))
         "find-chars-by-uuid returns every instance")
  (check (= 3 (ble:gatt-char-handle
               (ble:find-char-by-uuid chars (ble:uuid16 #x2A6E))))
         "where the singular one returns only the first, as it says it does")
  (check (null (ble:find-chars-by-uuid chars (ble:uuid16 #x2A6D)))
         "and nothing when there is nothing"))

(check (find-package :environmental-sensing-client) "the client half loads too")

(format t "~&~%ENVIRONMENTAL SENSING CHECK: ~:[clean~;~:*~D problem(s)~]~%"
        *problems*)
(sb-ext:exit :code (if (zerop *problems*) 0 1))
