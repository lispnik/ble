(in-package #:ble)

;;; ATT/GATT conditions, and the switch that turns sentinel returns into
;;; signals. See src/conditions.lisp for why both styles exist.

(defparameter +att-error-names+
  '((#x01 . "invalid handle")            (#x02 . "read not permitted")
    (#x03 . "write not permitted")       (#x04 . "invalid PDU")
    (#x05 . "insufficient authentication") (#x06 . "request not supported")
    (#x07 . "invalid offset")            (#x08 . "insufficient authorization")
    (#x09 . "prepare queue full")        (#x0A . "attribute not found")
    (#x0B . "attribute not long")        (#x0C . "insufficient encryption key size")
    (#x0D . "invalid attribute value length") (#x0E . "unlikely error")
    (#x0F . "insufficient encryption")   (#x10 . "unsupported group type")
    (#x11 . "insufficient resources")    (#x12 . "database out of sync")
    (#x13 . "value not allowed"))
  "The Core-spec ATT error codes. Only for messages -- a caller branching on a
failure should use ATT-ERROR-CODE, not the wording.")

(defun att-error-name (code)
  "A human-readable name for ATT error CODE, or a hex fallback for the
application-defined range (0x80-0x9F) and anything else unrecognised."
  (or (cdr (assoc code +att-error-names+))
      (format nil "error 0x~2,'0X" code)))

(define-condition att-error (ble-error)
  ((code   :initarg :code   :reader att-error-code)
   (opcode :initarg :opcode :reader att-error-opcode :initform nil)
   (handle :initarg :handle :reader att-error-handle :initform nil))
  (:report (lambda (c s)
             (format s "ATT: ~A" (att-error-name (att-error-code c)))
             (when (att-error-handle c)
               (format s " at handle 0x~4,'0X" (att-error-handle c)))
             (when (att-error-opcode c)
               (format s " (request 0x~2,'0X)" (att-error-opcode c)))))
  (:documentation
   "The peer refused a request with an ATT Error Response. CODE is the raw
error octet; ATT-ERROR-NAME turns it into words."))

(define-condition att-timeout (ble-error)
  ((operation  :initarg :operation  :reader att-timeout-operation :initform nil)
   (timeout-ms :initarg :timeout-ms :reader att-timeout-ms        :initform nil))
  (:report (lambda (c s)
             (format s "ATT: no answer~@[ to ~A~]~@[ within ~Dms~]"
                     (att-timeout-operation c) (att-timeout-ms c))))
  (:documentation
   "The peer did not answer within the deadline. Distinct from ATT-ERROR: a
refusal is an answer, and this is silence."))

(define-condition peer-disconnected (ble-error)
  ((handle :initarg :handle :reader peer-disconnected-handle :initform nil))
  (:report (lambda (c s)
             (format s "the peer disconnected~@[ (handle 0x~4,'0X)~]"
                     (peer-disconnected-handle c))))
  (:documentation "The link dropped while an operation was in flight."))

(define-condition gatt-not-found (ble-error)
  ((what :initarg :what :reader gatt-not-found-what :initform "attribute")
   (uuid :initarg :uuid :reader gatt-not-found-uuid :initform nil))
  (:report (lambda (c s)
             (format s "GATT: no ~A~@[ matching ~A~] on this peer"
                     (gatt-not-found-what c) (gatt-not-found-uuid c))))
  (:documentation
   "Discovery completed and the thing asked for was not in the peer's
database. Not an error from the peer -- it answered, the answer was empty."))

(defvar *att-signal-errors* nil
  "When true, ATT operations signal instead of returning :TIMEOUT,
:DISCONNECTED or a bare error code. Bound by WITH-BLE-CONDITIONS.

Off by default on purpose. A timeout is an ordinary outcome when polling a
radio, and a library that unwinds the stack for one by default is unpleasant
to poll with. Callers doing a *sequence* of GATT operations want the
opposite, and they are the ones who opt in.")

(defmacro with-ble-conditions (&body body)
  "Run BODY with ATT failures signalled as conditions rather than returned as
sentinel values. Every condition inherits from BLE-ERROR:

  (handler-case (ble:with-ble-conditions
                  (ble:att-exchange-mtu chan 247)
                  (ble:att-subscribe chan cccd)
                  (ble:att-write-value chan h payload))
    (ble:att-error (e) (report (ble:att-error-code e)))
    (ble:att-timeout () (give-up)))"
  `(let ((*att-signal-errors* t)) ,@body))

(defun %att-fail (kind &key code opcode handle operation timeout-ms)
  "Signal the condition for KIND when *ATT-SIGNAL-ERRORS* is on; otherwise
return the sentinel the non-signalling API has always returned. Every ATT
operation funnels its failures through here so the two styles cannot drift."
  (ecase kind
    (:error (if *att-signal-errors*
                (error 'att-error :code code :opcode opcode :handle handle)
                code))
    (:timeout (if *att-signal-errors*
                  (error 'att-timeout :operation operation :timeout-ms timeout-ms)
                  :timeout))
    (:disconnected (if *att-signal-errors*
                       (error 'peer-disconnected :handle handle)
                       :disconnected))))
