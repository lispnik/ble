;;;; A Bluetooth glucose meter.
;;;;
;;;; The third example, and the one that stops being a sensor. A heart rate
;;;; belt and a thermometer both report what is happening now; a glucose meter
;;;; is a database. Its readings were taken hours ago, they are stored on the
;;;; device, and a client asks for them by writing a query.
;;;;
;;;; That query is the Record Access Control Point, and it is a request/
;;;; response protocol layered over GATT rather than a characteristic anyone
;;;; reads. One write starts a procedure; the records come back as a stream of
;;;; notifications on a different characteristic; the procedure ends with an
;;;; indication on the control point saying how it went. Three characteristics
;;;; and two traffic patterns cooperating on one transaction.
;;;;
;;;; It is also the first example that requires an encrypted link. The Glucose
;;;; profile says so, and it is right to: this is medical history. Everything
;;;; the library needs for that was already here and verified against a phone,
;;;; but nothing a consumer could read demonstrated it.
;;;;
;;;; Its own package, using only exported symbols, for the same reason the
;;;; others are.

(defpackage #:glucose
  (:use #:common-lisp)
  (:export #:sfloat #:glucose-measurement #:mg/dl->kg/l
           #:build-server #:add-record #:run #:*name*))

(in-package #:glucose)

(defparameter *name* "Lisp Glucose")

;;; --- what the profile defines -------------------------------------------
;;;
;;; RACP op codes. The control point is written with one of these, an
;;; operator saying which records it applies to, and an operand narrowing it
;;; further.
(defconstant +op-report-records+   #x01)
(defconstant +op-delete-records+   #x02)
(defconstant +op-abort+            #x03)
(defconstant +op-report-count+     #x04)
(defconstant +op-count-response+   #x05)
(defconstant +op-response-code+    #x06)

;;; Operators. NULL is the one that must accompany an op code taking no
;;; record selection -- Abort -- and using it anywhere else is an error the
;;; peer is entitled to be told about.
(defconstant +operator-null+       #x00)
(defconstant +operator-all+        #x01)
(defconstant +operator-lte+        #x02)
(defconstant +operator-gte+        #x03)
(defconstant +operator-range+      #x04)
(defconstant +operator-first+      #x05)
(defconstant +operator-last+       #x06)

;;; The filter type inside an operand. Sequence number is the one every
;;; glucose meter supports; filtering by the user-facing time means trusting
;;; a clock this device does not claim to have.
(defconstant +filter-sequence+     #x01)
(defconstant +filter-user-time+    #x02)

;;; Response codes, carried in a Response Code (0x06) reply.
(defconstant +rsp-success+                #x01)
(defconstant +rsp-op-code-not-supported+  #x02)
(defconstant +rsp-invalid-operator+       #x03)
(defconstant +rsp-operator-not-supported+ #x04)
(defconstant +rsp-invalid-operand+        #x05)
(defconstant +rsp-no-records-found+       #x06)
(defconstant +rsp-abort-unsuccessful+     #x07)
(defconstant +rsp-procedure-not-completed+ #x08)
(defconstant +rsp-operand-not-supported+  #x09)

;;; Common Profile and Service error codes -- the Core specification sets
;;; aside 0xE0-0xFF for these, and both of the ones this service can return
;;; live there. They are ATT errors, refusing the write itself, and are quite
;;; separate from the response codes above, which report on a procedure that
;;; was accepted and then ran.
(defconstant +err-cccd-improperly-configured+ #xFD)
(defconstant +err-procedure-in-progress+      #xFE)

;;; Type is the low nibble of one octet, sample location the high nibble.
(defconstant +type-capillary-whole-blood+ #x01)
(defconstant +type-venous-plasma+         #x04)
(defconstant +location-finger+            #x01)
(defconstant +location-alternate-site+    #x02)
(defconstant +location-not-available+     #x0F)

;;; --- IEEE-11073 SFLOAT --------------------------------------------------
;;;
;;; The thermometer's FLOAT in 16 bits: a signed 4-bit exponent in the top
;;; nibble and a signed 12-bit mantissa under it, value = mantissa x
;;; 10^exponent. Two octets instead of four, because a glucose meter may send
;;; hundreds of stored records in one procedure and half the bytes matters.
;;;
;;; The narrow ranges are the trap. The exponent runs -8 to 7 and the mantissa
;;; -2048 to 2047, so a concentration in kg/L -- which is a very small number
;;; -- has to pick its exponent carefully or lose all its precision.

(defconstant +sfloat-nan+ #x07FF
  "The reserved pattern for `not a number'.")

(defun sfloat (value &optional (exponent -5))
  "Encode VALUE as a 16-bit IEEE-11073 SFLOAT, little-endian.

Pass :NAN for a reading that is not available. Signals rather than truncating
a mantissa that will not fit, because a truncated mantissa is not an error --
it is a different measurement, and this one is clinical."
  (let* ((mantissa (if (eq value :nan)
                       +sfloat-nan+
                       (round (* value (expt 10 (- exponent))))))
         (exponent (if (eq value :nan) 0 exponent)))
    (unless (<= -8 exponent 7)
      (error "sfloat: exponent ~D does not fit in 4 signed bits" exponent))
    (unless (or (eq value :nan) (<= -2048 mantissa 2047))
      (error "sfloat: ~A at 10^~D needs a mantissa of ~D, which does not fit ~
              in 12 signed bits" value exponent mantissa))
    (let ((v (logior (ash (ldb (byte 4 0) exponent) 12)
                     (ldb (byte 12 0) mantissa)))
          (out (make-array 2 :element-type '(unsigned-byte 8))))
      (setf (aref out 0) (ldb (byte 8 0) v)
            (aref out 1) (ldb (byte 8 8) v))
      out)))

(defun mg/dl->kg/l (mg/dl)
  "Convert the number a meter displays into the units the profile sends.

Glucose concentration goes on the wire in kg/L, and no glucose meter in the
world displays kg/L -- the US reads mg/dL, most of Europe mmol/L. 100 mg/dL is
1 g/L is 0.001 kg/L, so the conversion is a factor of 100000. Worth a named
function because getting it wrong by a factor of ten still looks plausible."
  (/ mg/dl 100000))

;;; --- the measurement ----------------------------------------------------

(defun glucose-measurement (record)
  "Encode a Glucose Measurement (0x2A18) from a RECORD plist.

Flags, sequence number, and base time are always present; concentration and
the type/location octet travel together behind one flag, which is why they are
one decision here rather than two."
  (destructuring-bind (&key sequence time mg/dl
                            (type +type-capillary-whole-blood+)
                            (location +location-finger+) &allow-other-keys)
      record
    (let* ((flags (logior (if mg/dl #x02 0)))   ; bit 1: concentration present
                                                ; bit 2 clear: units are kg/L
           (out (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer t)))
      (flet ((u8 (v) (vector-push-extend (logand v #xFF) out))
             (bytes (v) (loop for b across v do (vector-push-extend b out))))
        (u8 flags)
        (u8 (ldb (byte 8 0) sequence))
        (u8 (ldb (byte 8 8) sequence))
        (bytes (ble:date-time time))
        (when mg/dl
          (bytes (sfloat (mg/dl->kg/l mg/dl)))
          ;; One octet, two nibbles: type low, sample location high.
          (u8 (logior (ldb (byte 4 0) type) (ash location 4)))))
      (coerce out '(simple-array (unsigned-byte 8) (*))))))

;;; --- the device ---------------------------------------------------------

(defstruct meter
  "The server, the handles, the stored records, and the running procedure.

PROCEDURE is what makes this a control point rather than a characteristic:
a write starts one, it runs across many ticks, and a second write while it
runs must be refused rather than queued."
  server measurement-handle measurement-cccd racp-handle racp-cccd
  (records '()) (next-sequence 0)
  (procedure nil)       ; NIL, or (list :pending remaining-records request-op)
  (outstanding nil) (sent-at 0))

(defun add-record (meter mg/dl &key (time (get-universal-time)))
  "Store one reading, as taking a measurement would. Returns its sequence
number, which is what a client will later ask for records after."
  (let ((seq (meter-next-sequence meter)))
    (incf (meter-next-sequence meter))
    (setf (meter-records meter)
          (append (meter-records meter)
                  (list (list :sequence seq :time time :mg/dl mg/dl))))
    seq))

;;; --- the control point --------------------------------------------------

(defun %select-records (meter operator operand)
  "The records an operator and operand name, or a response code refusing it.

Returns (VALUES RECORDS NIL) or (VALUES NIL RESPONSE-CODE). Separated from
the request handling because this is the part with the interesting mistakes
in it, and it is pure."
  (let ((records (meter-records meter)))
    (cond
      ((= operator +operator-all+) (values records nil))
      ((= operator +operator-first+)
       (values (when records (list (first records))) nil))
      ((= operator +operator-last+)
       (values (when records (last records)) nil))
      ((= operator +operator-gte+)
       ;; The operand is a filter type and then its value. Anything shorter
       ;; is malformed, and a filter this meter does not implement is a
       ;; different answer from one it does not recognise.
       (cond ((< (length operand) 3) (values nil +rsp-invalid-operand+))
             ((= (aref operand 0) +filter-user-time+)
              (values nil +rsp-operand-not-supported+))
             ((/= (aref operand 0) +filter-sequence+)
              (values nil +rsp-invalid-operand+))
             (t (let ((from (+ (aref operand 1) (ash (aref operand 2) 8))))
                  (values (remove-if (lambda (r) (< (getf r :sequence) from))
                                     records)
                          nil)))))
      ((member operator (list +operator-lte+ +operator-range+))
       (values nil +rsp-operator-not-supported+))
      (t (values nil +rsp-invalid-operator+)))))

(defun %response-code (request-op code)
  "A Response Code (0x06) reply: which request, and how it went."
  (let ((v (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref v 0) +op-response-code+
          (aref v 1) +operator-null+
          (aref v 2) request-op
          (aref v 3) code)
    v))

(defun %count-response (n)
  "A Number of Stored Records response (0x05), which is not a Response Code:
the count comes back in its own op code, and a client decoding one as the
other reads the low octet of the count as a result."
  (let ((v (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref v 0) +op-count-response+
          (aref v 1) +operator-null+
          (aref v 2) (ldb (byte 8 0) n)
          (aref v 3) (ldb (byte 8 8) n))
    v))

(defun handle-racp-write (meter value)
  "Answer a write to the Record Access Control Point.

Returns NIL to accept the write, or an ATT error code to refuse it. Refusing
and failing are different things here and the difference is the whole design:
an ATT error rejects the *write*, so the procedure never starts, while a
Response Code reports on a procedure that did start. A client that cannot
tell them apart will wait forever for a reply that was already refused.

Nothing is sent from here. The reply has to follow the Write Response, and
the records have to follow the reply, so this only decides what will happen;
RUN-PROCEDURE does it."
  ;; An ATT error, not a response code: a write this short has no op code to
  ;; report a response code against, and inventing one would be a reply to a
  ;; request the client never managed to make.
  (when (< (length value) 2)
    (return-from handle-racp-write ble:+att-err-invalid-value-length+))
  (let ((op (aref value 0))
        (operator (aref value 1))
        (operand (subseq value 2)))
    (cond
      ;; Both CCCDs must be configured before any of this means anything:
      ;; the control point indicates its result and the measurements are
      ;; notified, so an unconfigured client would start a procedure whose
      ;; entire output goes nowhere.
      ((not (and (ble:gatt-subscribed-p (meter-server meter)
                                        (meter-racp-cccd meter) :indications t)
                 (ble:gatt-subscribed-p (meter-server meter)
                                        (meter-measurement-cccd meter))))
       +err-cccd-improperly-configured+)
      ;; Abort is answerable at once and is allowed to interrupt.
      ((= op +op-abort+)
       (setf (meter-procedure meter)
             (list :reply (%response-code
                           +op-abort+
                           (if (meter-procedure meter)
                               +rsp-success+ +rsp-abort-unsuccessful+))))
       nil)
      ;; One procedure at a time. This is an ATT error, not a response code:
      ;; the second write is refused outright.
      ((meter-procedure meter) +err-procedure-in-progress+)
      ((= op +op-report-records+)
       (multiple-value-bind (records refusal) (%select-records meter operator operand)
         (setf (meter-procedure meter)
               (cond (refusal (list :reply (%response-code op refusal)))
                     ((null records)
                      (list :reply (%response-code op +rsp-no-records-found+)))
                     (t (list :report records op))))
         nil))
      ((= op +op-report-count+)
       (multiple-value-bind (records refusal) (%select-records meter operator operand)
         (setf (meter-procedure meter)
               (list :reply (if refusal
                                (%response-code op refusal)
                                (%count-response (length records)))))
         nil))
      ((= op +op-delete-records+)
       (multiple-value-bind (records refusal) (%select-records meter operator operand)
         (setf (meter-procedure meter)
               (cond (refusal (list :reply (%response-code op refusal)))
                     ((null records)
                      (list :reply (%response-code op +rsp-no-records-found+)))
                     (t (setf (meter-records meter)
                              (set-difference (meter-records meter) records))
                        (setf (meter-records meter)
                              (sort (meter-records meter) #'<
                                    :key (lambda (r) (getf r :sequence))))
                        (list :reply (%response-code op +rsp-success+)))))
         nil))
      (t (setf (meter-procedure meter)
               (list :reply (%response-code op +rsp-op-code-not-supported+)))
         nil))))

(defun run-procedure (meter conn)
  "Push the running procedure along by one step. Called every tick.

A step at a time rather than a loop, because between any two records the peer
may abort, disconnect, or simply stop confirming, and a loop here would
notice none of those until it had finished."
  (let ((proc (meter-procedure meter)))
    (when (and proc (not (meter-outstanding meter)))
      (ecase (first proc)
        ;; Streaming records: one notification per tick.
        (:report
         (let ((records (second proc)))
           (if records
               (progn
                 (ble:gatt-notify (meter-server meter) conn
                                  (meter-measurement-handle meter)
                                  (glucose-measurement (first records)))
                 (setf (second proc) (rest records)))
               ;; Out of records: the procedure is over, and says so.
               (setf (meter-procedure meter)
                     (list :reply (%response-code (third proc) +rsp-success+))))))
        ;; The terminating indication, which must be confirmed before the
        ;; control point is free again.
        (:reply
         (when (ble:gatt-notify (meter-server meter) conn
                                (meter-racp-handle meter) (second proc)
                                :indications t)
           (setf (meter-outstanding meter) t
                 (meter-sent-at meter) (get-internal-real-time)
                 (meter-procedure meter) nil)))))))

;;; --- the database -------------------------------------------------------

(defun build-server ()
  "Generic Access, Generic Attribute, and the Glucose service.

Every characteristic that carries or selects medical history requires an
encrypted link. Glucose Feature does not: it describes the device rather than
the patient, and a central is entitled to know what it is talking to before
deciding whether to pair."
  (let ((server (ble:make-gatt-server :mtu 23))
        (meter nil))
    (ble:gatt-add-service server ble:+service-generic-access+)
    (ble:gatt-add-characteristic server :uuid ble:+char-device-name+
                                        :properties '(:read) :value *name*)
    (ble:gatt-add-characteristic server :uuid ble:+char-appearance+
                                        :properties '(:read)
                                        :value (ble:appearance
                                                ble:+appearance-generic-glucose-meter+))
    (ble:gatt-add-service server ble:+service-generic-attribute+)
    (ble:gatt-add-characteristic server :uuid ble:+char-service-changed+
                                        :properties '(:indicate)
                                        :value (ble:service-changed-range))
    (ble:gatt-add-service server ble:+service-glucose+)
    (multiple-value-bind (measurement measurement-cccd)
        (ble:gatt-add-characteristic server :uuid ble:+char-glucose-measurement+
                                            :properties '(:notify)
                                            :security :encrypted)
      ;; Read without encryption, deliberately: see the docstring.
      (ble:gatt-add-characteristic
       server :uuid ble:+char-glucose-feature+ :properties '(:read)
              ;; Every feature bit clear. This meter detects no faults and
              ;; claims none -- a device that sets bits it does not implement
              ;; is telling a client to expect annunciations that never come.
              :value #(#x00 #x00))
      (multiple-value-bind (racp racp-cccd)
          (ble:gatt-add-characteristic
           server :uuid ble:+char-record-access-control-point+
                  :properties '(:write :indicate)
                  :security :encrypted
                  :on-write (lambda (s a v)
                              (declare (ignore s a))
                              (handle-racp-write meter v)))
        (setf meter (make-meter :server server
                                :measurement-handle measurement
                                :measurement-cccd measurement-cccd
                                :racp-handle racp
                                :racp-cccd racp-cccd))
        meter))))

;;; --- running it ---------------------------------------------------------

(defconstant +indication-timeout-seconds+ 30
  "The ATT transaction timeout: a peer that has not confirmed in this long is
not going to, and waiting forever is how a device hangs instead of recovering.")

(defun run (&key (dev nil) (seconds nil) (readings '(96 142 88 175 104)))
  "Advertise as a glucose meter holding a few stored readings.

READINGS are in mg/dL and are stored an hour apart ending now, so a client
asking for records after a sequence number gets a meaningful subset rather
than all or nothing."
  (let* ((meter (build-server))
         (dev (or dev (ble:default-hci-dev)))
         (pairing nil))
    (loop for mg/dl in readings
          for i from (length readings) downto 1
          do (add-record meter mg/dl
                         :time (- (get-universal-time) (* i 3600))))
    (ble:install-adapter-teardown)
    (ble:with-hci-user-socket (sock dev)
      ;; The library drives pairing now. This used to be forty-five lines
      ;; here -- request security, answer the controller's key request,
      ;; distribute identity after Encryption Change, store the bond -- and a
      ;; second copy of it lived in tools/pair-with-phone/, already drifting.
      ;; LOCAL-ADDR is not optional: it is bound into the pairing crypto, so
      ;; it has to be the address actually being advertised.
      (setf pairing (ble:make-peripheral-pairing
                     :local-addr (ble:static-random-address
                                  (ble:smp-random-octets sock 6))
                     :irk (ble:smp-random-octets sock 16)
                     :on-paired
                     (lambda (conn session bond)
                       (declare (ignore conn session))
                       (format t "~&*** link encrypted -- stored records are ~
                                  now readable~:[~;, and a bond was stored~]~%"
                               bond)
                       (force-output))))
      (ble:set-random-address sock (ble:peripheral-pairing-local-addr pairing))
      (ble:set-adv-parameters sock :adv-type ble:+adv-ind+ :own-addr-type 1)
      (ble:set-adv-data sock (ble:adv-data
                              :flags '(:general-discoverable :no-bredr)
                              :name *name*
                              :services-16 (list ble:+service-glucose+)))
      (format t "~&~A advertising on hci~D as ~A, holding ~D record(s)~%"
              *name* dev (ble:format-mac
                          (ble:peripheral-pairing-local-addr pairing))
              (length (meter-records meter)))
      (force-output)
      (ble:serve-peripheral
       (meter-server meter) sock
       :seconds seconds
       :pairing pairing
       :on-connect (lambda (conn peer ptype)
                     (declare (ignore conn))
                     (setf (meter-procedure meter) nil
                           (meter-outstanding meter) nil)
                     (format t "~&connected: ~A (~(~A~))~:[~; -- a peer we ~
                                have a bond with~]~%"
                             (ble:format-mac peer) ptype (ble:find-bond peer))
                     (force-output))
       :on-disconnect (lambda (conn)
                        (declare (ignore conn))
                        ;; A procedure belongs to the connection that started
                        ;; it. Leaving one running would have the next client
                        ;; refused with Procedure Already In Progress.
                        (setf (meter-procedure meter) nil
                              (meter-outstanding meter) nil)
                        (format t "~&disconnected; advertising again~%")
                        (force-output))
       :on-tick
       (lambda (conn request)
         (when (eql request ble:+att-handle-value-cfm+)
           (setf (meter-outstanding meter) nil))
         (when (and (meter-outstanding meter)
                    (> (get-internal-real-time)
                       (+ (meter-sent-at meter)
                          (* +indication-timeout-seconds+
                             internal-time-units-per-second))))
           (format t "~&no confirmation in ~Ds; abandoning the procedure~%"
                   +indication-timeout-seconds+)
           (force-output)
           (setf (meter-outstanding meter) nil))
         (run-procedure meter conn))))))
