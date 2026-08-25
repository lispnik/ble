;;;; Load the scanner and check the parts that do not need a radio.
;;;;
;;;; Less is checkable here than in the other examples, and it is worth being
;;;; plain about why rather than padding the file: a scanner is mostly a
;;;; radio. What can be checked without one is the decoding -- the event-type
;;;; bitmap and the service naming -- and that the entry points exist with the
;;;; arguments the README says they take. The scanning itself is verified by
;;;; running it, and cannot be verified any other way.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename "./")) :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble/examples))

(defvar *problems* 0)
(defun check (ok fmt &rest args)
  (format t "~&[~A] ~?~%" (if ok " ok " "FAIL") fmt args)
  (unless ok (incf *problems*)))

;; --- naming services ----------------------------------------------------
(check (string= "Heart Rate" (scanner:service-name ble:+service-heart-rate+))
       "a known service gets its name")
(check (string= "Health Thermometer"
                (scanner:service-name ble:+service-health-thermometer+))
       "including the ones this repository can produce")
(check (string= "Glucose" (scanner:service-name ble:+service-glucose+))
       "and glucose")
(check (string= "Environmental Sensing"
                (scanner:service-name ble:+service-environmental-sensing+))
       "and environmental sensing")
(check (string= "0xFE2C" (scanner:service-name #xFE2C))
       "and an unknown one falls back to hex rather than guessing")

;; --- the event type bitmap ----------------------------------------------
;;
;; Extended reports carry a 16-bit bitmap. Getting this wrong is quiet: a
;; scan response misread as an advertisement looks like a second device.
(let ((names (scanner:event-type-names #x13)))
  ;; connectable | scannable | legacy
  (check (member "connectable" names :test #'string=) "bit 0 is connectable")
  (check (member "scannable" names :test #'string=) "bit 1 is scannable")
  (check (member "legacy-adv" names :test #'string=) "bit 4 says legacy")
  (check (not (member "scan-response" names :test #'string=))
         "and this one is not a scan response"))

(let ((names (scanner:event-type-names #x1B)))
  (check (member "scan-response" names :test #'string=)
         "bit 3 marks the report as a scan response -- the second half of a ~
          device already seen"))

(let ((names (scanner:event-type-names #x01)))
  (check (member "extended-adv" names :test #'string=)
         "and a clear bit 4 means an extended advertisement, not a legacy one")
  (check (= 2 (length names)) "with nothing else claimed"))

;; --- the entry points ---------------------------------------------------
(dolist (name '("SURVEY" "FIND-SERVICE" "FIND-NAMED" "WATCH" "SERVICE-NAME"
                "EVENT-TYPE-NAMES"))
  (check (fboundp (find-symbol name :scanner)) "~A is defined" name))

;; The filters are the reusable part: FIND-SERVICE and FIND-NAMED differ from
;; SURVEY only in the predicate they hand to DISCOVER, and a predicate is
;; checkable without a radio if it is built where it can be reached.
(check (typep (ble:make-discovered :address (ble:parse-mac "AA:BB:CC:DD:EE:FF")
                                   :name "Lisp Thermometer"
                                   :service-uuids (list ble:+service-health-thermometer+))
              'ble:discovered)
       "a DISCOVERED can be built without a radio, so a consumer can test ~
        its own filters")

(format t "~&~%SCANNER CHECK: ~:[clean~;~:*~D problem(s)~]~%" *problems*)
(sb-ext:exit :code (if (zerop *problems*) 0 1))
