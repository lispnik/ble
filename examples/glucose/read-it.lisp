;;;; Connect to the glucose meter, pair with it, and download its records.
;;;;
;;;; The client half, and the first one that has to pair. Every characteristic
;;;; worth reading on a glucose meter requires an encrypted link, so a central
;;;; that does not pair gets Insufficient Authentication and nothing else --
;;;; which is not a failure, it is the peripheral telling the central what to
;;;; do about it.
;;;;
;;;; The download itself is the interesting part. Writing one op code to the
;;;; control point starts a procedure; the records arrive as notifications on
;;;; a different characteristic; the procedure ends with an indication on the
;;;; control point. So this waits on two handles at once and stops when the
;;;; control point says the procedure is over, not when it has counted enough
;;;; records -- it does not know how many there are.
(defpackage #:glucose-client (:use #:common-lisp) (:export #:run))
(in-package #:glucose-client)

(defconstant +op-report-records+ #x01)
(defconstant +op-report-count+   #x04)
(defconstant +op-count-response+ #x05)
(defconstant +op-response-code+  #x06)
(defconstant +operator-all+      #x01)
(defconstant +sfloat-nan+        #x07FF)

(defun signed (value bits)
  (if (logbitp (1- bits) value) (- value (ash 1 bits)) value))

(defun decode-sfloat (v i)
  "Decode the 16-bit IEEE-11073 SFLOAT at offset I."
  (let* ((raw (+ (aref v i) (ash (aref v (1+ i)) 8)))
         (mantissa (ldb (byte 12 0) raw))
         (exponent (signed (ldb (byte 4 12) raw) 4)))
    (if (= mantissa +sfloat-nan+)
        :nan
        (* (signed mantissa 12) (expt 10 exponent)))))

(defun response-code-name (code)
  (case code
    (1 :success) (2 :op-code-not-supported) (3 :invalid-operator)
    (4 :operator-not-supported) (5 :invalid-operand) (6 :no-records-found)
    (7 :abort-unsuccessful) (8 :procedure-not-completed)
    (9 :operand-not-supported) (t :unknown)))

(defun decode-measurement (v)
  "Decode a Glucose Measurement (0x2A18) into a plist."
  (let* ((flags (aref v 0))
         (sequence (+ (aref v 1) (ash (aref v 2) 8)))
         (year (+ (aref v 3) (ash (aref v 4) 8)))
         (i 10)
         (offset (when (logbitp 0 flags)
                   (prog1 (signed (+ (aref v i) (ash (aref v (1+ i)) 8)) 16)
                     (incf i 2))))
         (concentration (when (logbitp 1 flags)
                          (prog1 (decode-sfloat v i) (incf i 2)))))
    (list :sequence sequence
          :date (list year (aref v 5) (aref v 6))
          :time (list (aref v 7) (aref v 8) (aref v 9))
          :offset-minutes offset
          ;; kg/L on the wire; nobody's meter displays that.
          :mg/dl (when (and concentration (not (eq concentration :nan)))
                   (round (* concentration 100000)))
          :units (if (logbitp 2 flags) :mol/l :kg/l)
          :type-and-location
          (when (and (logbitp 1 flags) (< i (length v)))
            (list :type (ldb (byte 4 0) (aref v i))
                  :location (ldb (byte 4 4) (aref v i)))))))

(defun racp-write (op operator)
  (let ((v (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref v 0) op (aref v 1) operator)
    v))

(defun run (mac &key (dev 2) (addr-type :random) (timeout-ms 15000))
  (ble:install-adapter-teardown)
  (ble:with-att-channel
      (chan (ble:hci-user-att-connect (ble:parse-mac mac) :addr-type addr-type
                                      :dev dev :init-phys #x01 :timeout 25))
    (format t "~&connected to ~A~%" mac) (force-output)
    (ble:att-exchange-mtu chan 247)
    (let ((gls (ble:att-find-service chan (ble:uuid16 ble:+service-glucose+))))
      (unless gls (error "no glucose service on this device"))
      (let* ((chars (ble:att-discover-characteristics
                     chan :start (ble:gatt-service-start gls)
                          :end (ble:gatt-service-end gls)))
             (m (ble:find-char-by-uuid
                 chars (ble:uuid16 ble:+char-glucose-measurement+)))
             (racp (ble:find-char-by-uuid
                    chars (ble:uuid16 ble:+char-record-access-control-point+)))
             (feature (ble:find-char-by-uuid
                       chars (ble:uuid16 ble:+char-glucose-feature+))))
        (unless (and m racp) (error "not a glucose meter: missing 0x2A18 or 0x2A52"))
        ;; Glucose Feature is readable without encryption, on purpose: a
        ;; central may want to know what it is talking to before it decides
        ;; to pair with it.
        (when feature
          (let ((v (ble:att-read-value chan (ble:gatt-char-handle feature))))
            (format t "~&features: 0x~4,'0X~%" (+ (aref v 0) (ash (aref v 1) 8)))))
        ;; Pair. Everything below here needs it, and attempting the subscribe
        ;; first would simply be refused.
        (let ((local (ble:hci-read-bd-addr :sock (ble:hci-conn-sock chan))))
          (handler-case
              (let ((session (ble:smp-pair chan :role :central
                                                :local-addr local
                                                :local-addr-type :public
                                                :peer-addr (ble:parse-mac mac)
                                                :peer-addr-type addr-type
                                                :io-capability :no-input-no-output)))
                (format t "~&paired; encryption -> ~A~%"
                        (ble:smp-start-encryption session))
                (force-output))
            (ble:smp-error (e)
              (format t "~&pairing failed: ~A~%" e)
              (force-output)
              (return-from run nil))))
        ;; Both CCCDs, before writing the control point: the meter refuses a
        ;; procedure whose output would have nowhere to go, and says so with
        ;; 0xFD rather than by silently doing nothing.
        (ble:att-subscribe chan (ble:att-find-cccd chan (ble:gatt-char-handle m)))
        (ble:att-subscribe chan (ble:att-find-cccd chan (ble:gatt-char-handle racp))
                           :indications t)
        ;; How many are there? A separate procedure, and a different reply --
        ;; op code 0x05 carrying a count, not a Response Code.
        (ble:att-write-value chan (ble:gatt-char-handle racp)
                             (racp-write +op-report-count+ +operator-all+))
        (let ((r (ble:att-next-notification chan (ble:gatt-char-handle racp)
                                            timeout-ms)))
          (if (and r (= (aref r 0) +op-count-response+))
              (format t "~&meter holds ~D record(s)~%"
                      (+ (aref r 2) (ash (aref r 3) 8)))
              (format t "~&unexpected reply to the count: ~A~%" r)))
        (force-output)
        ;; Now the records themselves.
        (ble:att-write-value chan (ble:gatt-char-handle racp)
                             (racp-write +op-report-records+ +operator-all+))
        (format t "~&downloading...~%") (force-output)
        ;; Two handles at once, which is why this is ATT-NEXT-NOTIFICATION-ANY
        ;; rather than the single-handle call the other clients use: waiting on
        ;; the measurement alone would never see the reply that ends the
        ;; procedure, and waiting on the control point alone would drop every
        ;; record on the floor.
        (loop with measurement = (ble:gatt-char-handle m)
              with control = (ble:gatt-char-handle racp)
              do (multiple-value-bind (value handle)
                     (ble:att-next-notification-any chan timeout-ms)
                   (cond
                     ((null value)
                      (format t "~&  (timed out waiting for the meter)~%")
                      (return))
                     ((eql handle measurement)
                      (format t "~&  ~A~%" (decode-measurement value)))
                     ((eql handle control)
                      (format t "~&procedure finished: ~A~%"
                              (if (= (aref value 0) +op-response-code+)
                                  (response-code-name (aref value 3))
                                  value))
                      (return))
                     (t (format t "~&  (unexpected handle ~D)~%" handle)))
                   (force-output)))))))
