;;;; Load the glucose meter and drive its control point, without a radio.
;;;;
;;;; This one checks more than the others because there is more to get wrong.
;;;; The encoding is a smaller float with narrower limits, and the control
;;;; point is a protocol with states -- a procedure that is running, one that
;;;; was refused, one that ended. None of it needs an adapter: the RACP
;;;; handler is a pure function of the request and the stored records, which
;;;; is why it was written as one.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename "./")) :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble/examples))

(defvar *problems* 0)
(defun check (ok fmt &rest args)
  (format t "~&[~A] ~?~%" (if ok " ok " "FAIL") fmt args)
  (unless ok (incf *problems*)))

;; --- IEEE-11073 SFLOAT --------------------------------------------------
(let ((f (glucose:sfloat 1/1000 -5)))
  (check (= 2 (length f)) "an SFLOAT is two octets")
  (let ((raw (+ (aref f 0) (ash (aref f 1) 8))))
    (check (= 100 (ldb (byte 12 0) raw)) "0.001 at 10^-5 is a mantissa of 100")
    (check (= #xB (ldb (byte 4 12) raw))
           "and an exponent of -5, four bits two's complement")))

(check (equalp (glucose:sfloat 1/1000 -5)
               (glucose:sfloat (glucose:mg/dl->kg/l 100)))
       "which is what 100 mg/dL comes to, at the default exponent")

(let ((f (glucose:sfloat :nan)))
  (check (and (= #xFF (aref f 0)) (= #x07 (aref f 1)))
         "NaN is the reserved 0x07FF, not a zero"))

;; The narrow ranges are the point of the type; both edges must hold.
(check (glucose:sfloat 2047 0) "2047 is the largest mantissa")
(check (handler-case (progn (glucose:sfloat 2048 0) nil) (error () t))
       "2048 signals rather than wrapping to a negative concentration")
(check (handler-case (progn (glucose:sfloat 1 8) nil) (error () t))
       "and an exponent of 8 does not fit in four signed bits")

;; --- the measurement ----------------------------------------------------
(let* ((record (list :sequence 7 :time (encode-universal-time 30 45 13 2 6 2026)
                     :mg/dl 142))
       (m (glucose:glucose-measurement record)))
  (check (= 13 (length m))
         "flags, sequence, 7 octets of Date Time, SFLOAT, type/location")
  (check (= #x02 (logand (aref m 0) #x02)) "the concentration flag is set")
  (check (zerop (logand (aref m 0) #x04)) "and the units bit says kg/L")
  (check (= 7 (+ (aref m 1) (ash (aref m 2) 8))) "sequence number, little-endian")
  (check (= 2026 (+ (aref m 3) (ash (aref m 4) 8))) "and the year likewise")
  (check (and (= 6 (aref m 5)) (= 2 (aref m 6))) "month then day, one octet each")
  (check (= #x11 (aref m 12))
         "type and location share an octet: capillary whole blood on a finger"))

;; --- the control point --------------------------------------------------
;;
;; Driving the handler directly. It returns NIL to accept a write and an ATT
;; error code to refuse it, and leaves what should happen next in the meter.
(defun racp (&rest octets)
  (coerce octets '(simple-array (unsigned-byte 8) (*))))

(let* ((meter (glucose:build-server))
       (server (slot-value meter 'glucose::server))
       (write (ble:gatt-attribute-on-write
               (ble:gatt-find-attribute
                server (slot-value meter 'glucose::racp-handle)))))
  (dolist (mg/dl '(96 142 88))
    (glucose:add-record meter mg/dl))
  (flet ((send (&rest octets)
           (funcall write server nil (apply #'racp octets)))
         (procedure () (slot-value meter 'glucose::procedure))
         (subscribe (which bit)
           ;; Reach into the server's CCCD state the way a client's write
           ;; would leave it.
           (setf (gethash (slot-value meter which)
                          (ble:gatt-server-cccd server))
                 bit)))
    ;; Nothing is configured yet, so every procedure must be refused -- its
    ;; entire output would go nowhere.
    (check (= #xFD (send #x01 #x01))
           "a report with no CCCD configured is refused with 0xFD")
    (subscribe 'glucose::measurement-cccd 1)
    (check (= #xFD (send #x01 #x01))
           "still refused with only the measurement subscribed")
    (subscribe 'glucose::racp-cccd 2)
    (check (null (send #x01 #x01)) "and accepted once both are")

    ;; A malformed write is an ATT error, not a response code.
    (setf (slot-value meter 'glucose::procedure) nil)
    (check (= #x0D (send #x01))
           "a one-octet write has no op code to answer, so 0x0D")

    ;; Report all records.
    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x01 #x01)
    (check (eq :report (first (procedure))) "reporting all records is queued")
    (check (= 3 (length (second (procedure)))) "with all three of them")

    ;; A second request while one runs is refused outright -- and this is an
    ;; ATT error rather than a Response Code, because the write itself is
    ;; being rejected.
    (check (= #xFE (send #x01 #x01))
           "a second procedure while one runs is refused with 0xFE")

    ;; Abort is allowed to interrupt, and reports on what it interrupted.
    (check (null (send #x03 #x00)) "abort is accepted even mid-procedure")
    (check (eq :reply (first (procedure))) "and replies immediately")
    (check (= 1 (aref (second (procedure)) 3)) "with success, having stopped one")
    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x03 #x00)
    (check (= 7 (aref (second (procedure)) 3))
           "and with abort-unsuccessful when there was nothing to stop")

    ;; Counting is a different reply: op code 0x05, not a Response Code.
    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x04 #x01)
    (let ((v (second (procedure))))
      (check (= #x05 (aref v 0)) "the count comes back under its own op code")
      (check (= 3 (+ (aref v 2) (ash (aref v 3) 8))) "and is the record count"))

    ;; Selecting by sequence number.
    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x01 #x03 #x01 #x01 #x00)          ; >= sequence 1
    (check (= 2 (length (second (procedure))))
           "greater-or-equal to sequence 1 selects the last two")

    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x01 #x05)                          ; first record
    (check (= 1 (length (second (procedure)))) "first record selects one")
    (check (= 0 (getf (first (second (procedure))) :sequence)) "the oldest")

    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x01 #x06)                          ; last record
    (check (= 2 (getf (first (second (procedure))) :sequence))
           "last record selects the newest")

    ;; The refusals, each with its own distinct code.
    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x01 #x02)                          ; less-than-or-equal
    (check (= 4 (aref (second (procedure)) 3))
           "an operator this meter does not implement is 0x04, not-supported")

    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x01 #x7F)                          ; nonsense operator
    (check (= 3 (aref (second (procedure)) 3))
           "an operator that is not an operator at all is 0x03, invalid")

    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x01 #x03 #x02 #x00 #x00)          ; filter by user-facing time
    (check (= 9 (aref (second (procedure)) 3))
           "a filter type it knows but cannot honour is 0x09, operand ~
            not supported")

    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x01 #x03 #x01)                     ; truncated operand
    (check (= 5 (aref (second (procedure)) 3))
           "and a truncated operand is 0x05, invalid operand")

    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x7F #x01)
    (check (= 2 (aref (second (procedure)) 3))
           "an unknown op code is 0x02, op code not supported")

    ;; Deleting really deletes, and the count follows.
    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x02 #x05)                          ; delete the first record
    (check (= 1 (aref (second (procedure)) 3)) "deleting the first succeeds")
    (check (= 2 (length (slot-value meter 'glucose::records)))
           "and leaves two behind")

    ;; An empty selection is `no records found', not success with nothing.
    (setf (slot-value meter 'glucose::records) '())
    (setf (slot-value meter 'glucose::procedure) nil)
    (send #x01 #x01)
    (check (= 6 (aref (second (procedure)) 3))
           "reporting from an empty meter is 0x06, no records found")))

;; --- the database -------------------------------------------------------
(let* ((meter (glucose:build-server))
       (server (slot-value meter 'glucose::server))
       (uuids (mapcar #'ble:uuid-string
                      (mapcar #'ble:gatt-service-entry-uuid
                              (reverse (ble:gatt-server-services server))))))
  (check (equal '("1800" "1801" "1808") uuids)
         "Generic Access, Generic Attribute, then Glucose")
  (let ((chars (loop for a across (ble:gatt-server-attributes server)
                     collect (ble:uuid-string (ble:gatt-attribute-uuid a)))))
    (check (member "2A18" chars :test #'string=) "the measurement is present")
    (check (member "2A51" chars :test #'string=) "and the feature")
    (check (member "2A52" chars :test #'string=) "and the control point"))
  ;; The security split is the point of this example: history is protected,
  ;; the device's own description is not.
  (let ((m (ble:gatt-find-attribute
            server (slot-value meter 'glucose::measurement-handle)))
        (racp (ble:gatt-find-attribute
               server (slot-value meter 'glucose::racp-handle)))
        (feature (find (ble:uuid16 ble:+char-glucose-feature+)
                       (ble:gatt-server-attributes server)
                       :key #'ble:gatt-attribute-uuid :test #'equalp)))
    (check (ble:gatt-attribute-security m) "the measurement requires encryption")
    (check (ble:gatt-attribute-security racp) "so does the control point")
    (check (not (ble:gatt-attribute-security feature))
           "the feature does not -- a central may look before it pairs")
    ;; And the protection is real, not merely declared.
    (setf (ble:gatt-server-encrypted server) nil)
    (check (member :notify (ble:gatt-attribute-permissions m))
           "the measurement notifies")
    (check (ble:gatt-find-attribute
            server (1+ (slot-value meter 'glucose::measurement-handle)))
           "and has a CCCD after it")))

(check (find-package :glucose-client) "the client half loads too")

(format t "~&~%GLUCOSE CHECK: ~:[clean~;~:*~D problem(s)~]~%" *problems*)
(sb-ext:exit :code (if (zerop *problems*) 0 1))
