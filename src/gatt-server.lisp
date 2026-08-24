(in-package #:ble)

;;; A GATT server: an attribute database, and the half of ATT that answers
;;; requests rather than making them.
;;;
;;; The client side of this library has always been able to discover and read
;;; a peer; this is the other direction, so a program using it can *be* the
;;; peripheral. The link layer could already do that -- advertise connectable,
;;; accept a connection, push notifications -- but with nothing above ATT PDUs
;;; there was no attribute database to discover and no way to answer a read.
;;;
;;; Handles are allocated sequentially as the database is built, which is the
;;; ordering GATT requires anyway: a characteristic's declaration must sit
;;; immediately before its value, and its descriptors immediately after.
;;; Building the layout by hand is where a server usually goes wrong, so
;;; GATT-ADD-CHARACTERISTIC lays down all three attributes -- declaration,
;;; value, and a CCCD when the characteristic notifies -- as one operation.

;;; --- characteristic properties -----------------------------------------

(defconstant +char-prop-broadcast+    #x01)
(defconstant +char-prop-read+         #x02)
(defconstant +char-prop-write-nr+     #x04)
(defconstant +char-prop-write+        #x08)
(defconstant +char-prop-notify+       #x10)
(defconstant +char-prop-indicate+     #x20)
(defconstant +char-prop-signed-write+ #x40)
(defconstant +char-prop-extended+     #x80)

(defparameter +char-property-bits+
  `((:broadcast . ,+char-prop-broadcast+)
    (:read . ,+char-prop-read+)
    (:write-without-response . ,+char-prop-write-nr+)
    (:write . ,+char-prop-write+)
    (:notify . ,+char-prop-notify+)
    (:indicate . ,+char-prop-indicate+)
    (:signed-write . ,+char-prop-signed-write+)
    (:extended . ,+char-prop-extended+)))

(defun char-properties-bitmap (properties)
  "Turn a list of property keywords into the declaration's property octet."
  (let ((bits 0))
    (dolist (p properties bits)
      (let ((bit (cdr (assoc p +char-property-bits+))))
        (unless bit (error "unknown characteristic property ~S" p))
        (setf bits (logior bits bit))))))

;;; --- the database ------------------------------------------------------

(defstruct gatt-attribute
  "One attribute: a handle, a type UUID in ATT wire order, and either a static
value or a reader that produces one.

ON-READ lets a value be computed at the moment it is asked for, which is what
a sensor characteristic wants -- a static octet vector would answer with
whatever was true when the database was built. ON-WRITE returns NIL to accept
a write, or an ATT error code to refuse it."
  handle uuid (permissions '(:read)) (value (make-octets 0)) on-read on-write)

(defstruct (gatt-server (:constructor %make-gatt-server))
  "An attribute database and the MTU negotiated for the link it serves."
  (attributes (make-array 0 :adjustable t :fill-pointer t))
  (services '())
  (mtu 23)
  (cccd (make-hash-table :test #'eql))
  (current-service nil))

(defstruct gatt-service-entry start end uuid)

(defun make-gatt-server (&key (mtu 23))
  "An empty database. Add services and characteristics to it in order."
  (%make-gatt-server :mtu mtu))

(defun gatt-attribute-count (server)
  (length (gatt-server-attributes server)))

(defun %next-handle (server)
  (1+ (gatt-attribute-count server)))

(defun %push-attribute (server attr)
  (vector-push-extend attr (gatt-server-attributes server))
  ;; Every attribute added extends the service it lands in. Tracking the end
  ;; handle as we go is what makes Read By Group Type answerable later.
  (let ((svc (gatt-server-current-service server)))
    (when svc (setf (gatt-service-entry-end svc) (gatt-attribute-handle attr))))
  attr)

(defun %as-uuid (uuid)
  "Accept a 16-bit UUID as an integer, or an already-formed ATT-order vector."
  (if (integerp uuid) (uuid16 uuid) (coerce-octets uuid)))

(defun %as-value (value)
  "Accept a string, a list, or an octet vector as an attribute value."
  (cond ((null value) (make-octets 0))
        ((stringp value) (map '(simple-array (unsigned-byte 8) (*))
                              #'char-code value))
        (t (coerce-octets value))))

(defun gatt-add-service (server uuid)
  "Begin a primary service. Characteristics added after this belong to it.
Returns its start handle."
  (let* ((uuid (%as-uuid uuid))
         (handle (%next-handle server))
         (entry (make-gatt-service-entry :start handle :end handle :uuid uuid)))
    (setf (gatt-server-current-service server) entry)
    (push entry (gatt-server-services server))
    (%push-attribute server (make-gatt-attribute
                             :handle handle :uuid (uuid16 +gatt-primary-service+)
                             :value uuid))
    handle))

(defun gatt-add-characteristic (server &key uuid (properties '(:read)) value
                                            on-read on-write)
  "Add a characteristic to the service most recently begun. Lays down the
declaration, the value attribute, and -- when PROPERTIES include :NOTIFY or
:INDICATE -- a Client Characteristic Configuration descriptor.

Returns (VALUES VALUE-HANDLE CCCD-HANDLE); CCCD-HANDLE is NIL when the
characteristic does not notify."
  (unless (gatt-server-current-service server)
    (error "gatt-add-characteristic: no service has been started"))
  (let* ((uuid (%as-uuid uuid))
         (decl-handle (%next-handle server))
         (value-handle (1+ decl-handle))
         (bits (char-properties-bitmap properties))
         (decl (make-octets (+ 3 (length uuid)))))
    (setf (aref decl 0) bits)
    (u16le-put decl 1 value-handle)
    (replace decl uuid :start1 3)
    (%push-attribute server (make-gatt-attribute
                             :handle decl-handle
                             :uuid (uuid16 +gatt-characteristic-decl+)
                             :value decl))
    (%push-attribute server (make-gatt-attribute
                             :handle value-handle :uuid uuid
                             :permissions properties
                             :value (%as-value value)
                             :on-read on-read :on-write on-write))
    (let ((cccd-handle
            (when (intersection properties '(:notify :indicate))
              (let ((h (%next-handle server)))
                (%push-attribute server
                                 (make-gatt-attribute
                                  :handle h :uuid (uuid16 +gatt-cccd+)
                                  :permissions '(:read :write)
                                  :value (make-octets 2)))
                h))))
      (values value-handle cccd-handle))))

(defun gatt-find-attribute (server handle)
  (find handle (gatt-server-attributes server) :key #'gatt-attribute-handle))

(defun gatt-attribute-read (server attr)
  "The current value of ATTR: computed by its reader when it has one."
  (let ((f (gatt-attribute-on-read attr)))
    (if f (%as-value (funcall f server attr)) (gatt-attribute-value attr))))

(defun gatt-set-value (server handle value)
  "Replace the stored value of an attribute. Returns the octets stored."
  (let ((attr (gatt-find-attribute server handle)))
    (unless attr (error 'gatt-not-found :what "attribute"))
    (setf (gatt-attribute-value attr) (%as-value value))))

;;; --- answering requests -------------------------------------------------

(defconstant +att-err-invalid-handle+       #x01)
(defconstant +att-err-read-not-permitted+   #x02)
(defconstant +att-err-write-not-permitted+  #x03)
(defconstant +att-err-request-not-supported+ #x06)
(defconstant +att-err-unsupported-group-type+ #x10)

(defun %send-att-error (chan opcode handle code)
  (let ((pdu (make-octets 5)))
    (setf (aref pdu 0) +att-error-rsp+
          (aref pdu 1) opcode)
    (u16le-put pdu 2 (or handle 0))
    (setf (aref pdu 4) code)
    (att-send chan pdu)))

(defun %attributes-in-range (server start end)
  (sort (remove-if-not (lambda (a)
                         (<= start (gatt-attribute-handle a) end))
                       (coerce (gatt-server-attributes server) 'list))
        #'< :key #'gatt-attribute-handle))

(defun %readable-p (attr)
  (member :read (gatt-attribute-permissions attr)))

(defun %writable-p (attr)
  (intersection '(:write :write-without-response)
                (gatt-attribute-permissions attr)))

(defun %handle-find-info (server chan pdu)
  "Find Information: hand back handle/UUID pairs. One response carries one
UUID format only, so this stops at the first attribute of the other size
rather than mixing them -- a client decoding a mixed response would read
garbage."
  (let* ((start (u16-le pdu 1)) (end (u16-le pdu 3))
         (attrs (%attributes-in-range server start end)))
    (if (null attrs)
        (%send-att-error chan +att-find-info-req+ start +att-err-attr-not-found+)
        (let* ((size (length (gatt-attribute-uuid (first attrs))))
               (fmt (if (= size 2) 1 2))
               (each (+ 2 size))
               (room (floor (- (gatt-server-mtu server) 2) each))
               (take (loop for a in attrs
                           while (and (< (length out) room)
                                      (= (length (gatt-attribute-uuid a)) size))
                           collect a into out
                           finally (return out)))
               (rsp (make-octets (+ 2 (* each (length take))))))
          (setf (aref rsp 0) +att-find-info-rsp+
                (aref rsp 1) fmt)
          (loop for a in take
                for off = 2 then (+ off each)
                do (u16le-put rsp off (gatt-attribute-handle a))
                   (replace rsp (gatt-attribute-uuid a) :start1 (+ off 2)))
          (att-send chan rsp)))))

(defun %handle-find-by-type-value (server chan pdu)
  "Find By Type Value: the single-round-trip way a client asks \"where is the
service with this UUID\", which is what ATT-FIND-SERVICE sends."
  (let* ((start (u16-le pdu 1)) (end (u16-le pdu 3))
         (type (u16-le pdu 5))
         (wanted (subseq pdu 7))
         (hits '()))
    (dolist (svc (gatt-server-services server))
      (when (and (= type +gatt-primary-service+)
                 (<= start (gatt-service-entry-start svc) end)
                 (equalp wanted (gatt-service-entry-uuid svc)))
        (push svc hits)))
    (setf hits (sort hits #'< :key #'gatt-service-entry-start))
    (if (null hits)
        (%send-att-error chan +att-find-by-type-value-req+ start
                         +att-err-attr-not-found+)
        (let ((rsp (make-octets (1+ (* 4 (length hits))))))
          (setf (aref rsp 0) +att-find-by-type-value-rsp+)
          (loop for svc in hits
                for off = 1 then (+ off 4)
                do (u16le-put rsp off (gatt-service-entry-start svc))
                   (u16le-put rsp (+ off 2) (gatt-service-entry-end svc)))
          (att-send chan rsp)))))

(defun %handle-read-by-type (server chan pdu)
  "Read By Type: every attribute of one type in a handle range, which is how a
client discovers characteristics (type 0x2803) and reads a value by UUID.
Entries in one response must all be the same length, so this stops at the
first that differs."
  (let* ((start (u16-le pdu 1)) (end (u16-le pdu 3))
         (type (subseq pdu 5))
         (attrs (remove-if-not (lambda (a) (equalp type (gatt-attribute-uuid a)))
                               (%attributes-in-range server start end))))
    (if (null attrs)
        (%send-att-error chan +att-read-by-type-req+ start +att-err-attr-not-found+)
        (let* ((first-value (gatt-attribute-read server (first attrs)))
               (vlen (min (length first-value) (- (gatt-server-mtu server) 4)))
               (each (+ 2 vlen))
               (room (floor (- (gatt-server-mtu server) 2) each))
               (take (loop for a in attrs
                           for v = (gatt-attribute-read server a)
                           while (and (< (length out) room) (= (length v) vlen))
                           collect a into out
                           finally (return out)))
               (rsp (make-octets (+ 2 (* each (length take))))))
          (setf (aref rsp 0) +att-read-by-type-rsp+
                (aref rsp 1) each)
          (loop for a in take
                for off = 2 then (+ off each)
                do (u16le-put rsp off (gatt-attribute-handle a))
                   (replace rsp (gatt-attribute-read server a)
                            :start1 (+ off 2) :end2 vlen))
          (att-send chan rsp)))))

(defun %handle-read-by-group-type (server chan pdu)
  "Read By Group Type for 0x2800: the service-discovery walk. Only primary
services are a legal group type here; anything else must be refused rather
than answered emptily, or a client cannot tell 'none' from 'not that kind'."
  (let* ((start (u16-le pdu 1)) (end (u16-le pdu 3))
         (type (u16-le pdu 5)))
    (unless (= type +gatt-primary-service+)
      (return-from %handle-read-by-group-type
        (%send-att-error chan +att-read-by-group-type-req+ start
                         +att-err-unsupported-group-type+)))
    (let ((hits (sort (remove-if-not
                       (lambda (s) (<= start (gatt-service-entry-start s) end))
                       (copy-list (gatt-server-services server)))
                      #'< :key #'gatt-service-entry-start)))
      (if (null hits)
          (%send-att-error chan +att-read-by-group-type-req+ start
                           +att-err-attr-not-found+)
          (let* ((ulen (length (gatt-service-entry-uuid (first hits))))
                 (each (+ 4 ulen))
                 (room (floor (- (gatt-server-mtu server) 2) each))
                 (take (loop for s in hits
                             while (and (< (length out) room)
                                        (= (length (gatt-service-entry-uuid s)) ulen))
                             collect s into out
                             finally (return out)))
                 (rsp (make-octets (+ 2 (* each (length take))))))
            (setf (aref rsp 0) +att-read-by-group-type-rsp+
                  (aref rsp 1) each)
            (loop for s in take
                  for off = 2 then (+ off each)
                  do (u16le-put rsp off (gatt-service-entry-start s))
                     (u16le-put rsp (+ off 2) (gatt-service-entry-end s))
                     (replace rsp (gatt-service-entry-uuid s) :start1 (+ off 4)))
            (att-send chan rsp))))))

(defun %handle-read (server chan pdu &key blob)
  (let* ((handle (u16-le pdu 1))
         (offset (if blob (u16-le pdu 3) 0))
         (attr (gatt-find-attribute server handle))
         (opcode (if blob +att-read-blob-req+ +att-read-req+)))
    (cond
      ((null attr) (%send-att-error chan opcode handle +att-err-invalid-handle+))
      ((not (%readable-p attr))
       (%send-att-error chan opcode handle +att-err-read-not-permitted+))
      (t (let ((value (gatt-attribute-read server attr)))
           (if (> offset (length value))
               (%send-att-error chan opcode handle +att-err-invalid-offset+)
               (let* ((chunk (subseq value offset
                                     (min (length value)
                                          (+ offset (- (gatt-server-mtu server) 1)))))
                      (rsp (make-octets (1+ (length chunk)))))
                 (setf (aref rsp 0) (if blob +att-read-blob-rsp+ +att-read-rsp+))
                 (replace rsp chunk :start1 1)
                 (att-send chan rsp))))))))

(defun %handle-read-multiple (server chan pdu)
  (let ((values '()))
    (loop for off from 1 below (length pdu) by 2
          while (<= (+ off 2) (length pdu))
          do (let ((attr (gatt-find-attribute server (u16-le pdu off))))
               (cond ((null attr)
                      (return-from %handle-read-multiple
                        (%send-att-error chan +att-read-multiple-req+
                                         (u16-le pdu off)
                                         +att-err-invalid-handle+)))
                     ((not (%readable-p attr))
                      (return-from %handle-read-multiple
                        (%send-att-error chan +att-read-multiple-req+
                                         (u16-le pdu off)
                                         +att-err-read-not-permitted+)))
                     (t (push (gatt-attribute-read server attr) values)))))
    (let* ((blob (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                        (nreverse values)))
           (chunk (subseq blob 0 (min (length blob) (- (gatt-server-mtu server) 1))))
           (rsp (make-octets (1+ (length chunk)))))
      (setf (aref rsp 0) +att-read-multiple-rsp+)
      (replace rsp chunk :start1 1)
      (att-send chan rsp))))

(defun %handle-write (server chan pdu &key command)
  "Write Request, or Write Command when COMMAND. A command gets no response
whatever happens -- including on refusal, which is exactly why a client that
cares should use a request."
  (let* ((handle (u16-le pdu 1))
         (value (subseq pdu 3))
         (attr (gatt-find-attribute server handle))
         (opcode (if command +att-write-cmd+ +att-write-req+)))
    (flet ((fail (code)
             (unless command (%send-att-error chan opcode handle code))
             (return-from %handle-write nil)))
      (unless attr (fail +att-err-invalid-handle+))
      (unless (or (%writable-p attr)
                  (equalp (gatt-attribute-uuid attr) (uuid16 +gatt-cccd+)))
        (fail +att-err-write-not-permitted+))
      ;; A CCCD write is the subscription itself: record it, because whether a
      ;; notification may be sent later is decided by this value.
      (when (equalp (gatt-attribute-uuid attr) (uuid16 +gatt-cccd+))
        (setf (gethash handle (gatt-server-cccd server))
              (if (>= (length value) 2) (u16-le value 0) 0)))
      (let ((hook (gatt-attribute-on-write attr)))
        (when hook
          (let ((code (funcall hook server attr value)))
            (when (integerp code) (fail code)))))
      (unless (gatt-attribute-on-write attr)
        (setf (gatt-attribute-value attr) (coerce-octets value)))
      (unless command
        (att-send chan (let ((rsp (make-octets 1)))
                         (setf (aref rsp 0) +att-write-rsp+)
                         rsp))))))

(defun %handle-exchange-mtu (server chan pdu)
  (let ((client (u16-le pdu 1))
        (rsp (make-octets 3)))
    (setf (aref rsp 0) +att-exchange-mtu-rsp+)
    (u16le-put rsp 1 *att-rx-mtu*)
    (att-send chan rsp)
    ;; Both ends must settle on the same number, and it is the smaller of the
    ;; two: answering with ours and then using ours would have us emit PDUs
    ;; the client is entitled to drop.
    (setf (gatt-server-mtu server) (max 23 (min client *att-rx-mtu*)))))

(defun gatt-serve-pdu (server chan pdu)
  "Answer one ATT request. Returns the opcode handled, or NIL if PDU was not a
request this server implements -- in which case it has already sent the Error
Response that says so."
  (when (and pdu (vectorp pdu) (plusp (length pdu)))
    (let ((op (aref pdu 0)))
      (macrolet ((need (n) `(unless (>= (length pdu) ,n)
                              (return-from gatt-serve-pdu nil))))
        (cond
          ((= op +att-exchange-mtu-req+) (need 3) (%handle-exchange-mtu server chan pdu) op)
          ((= op +att-find-info-req+) (need 5) (%handle-find-info server chan pdu) op)
          ((= op +att-find-by-type-value-req+) (need 7)
           (%handle-find-by-type-value server chan pdu) op)
          ((= op +att-read-by-type-req+) (need 7) (%handle-read-by-type server chan pdu) op)
          ((= op +att-read-by-group-type-req+) (need 7)
           (%handle-read-by-group-type server chan pdu) op)
          ((= op +att-read-req+) (need 3) (%handle-read server chan pdu) op)
          ((= op +att-read-blob-req+) (need 5) (%handle-read server chan pdu :blob t) op)
          ((= op +att-read-multiple-req+) (need 5) (%handle-read-multiple server chan pdu) op)
          ((= op +att-write-req+) (need 3) (%handle-write server chan pdu) op)
          ((= op +att-write-cmd+) (need 3) (%handle-write server chan pdu :command t) op)
          ((= op +att-handle-value-cfm+) op)  ; our indication was confirmed
          ;; A command carries no response, so an unsupported one is dropped
          ;; rather than refused -- answering would itself be a protocol error.
          ((logbitp 6 op) nil)
          (t (%send-att-error chan op 0 +att-err-request-not-supported+) nil))))))

(defun gatt-serve (server chan &key (timeout-ms 1000))
  "Wait for one request and answer it. Returns the opcode handled, NIL on
timeout, or :DISCONNECTED."
  (let ((pdu (att-recv chan timeout-ms)))
    (cond ((eq pdu :disconnected) :disconnected)
          ((null pdu) nil)
          (t (gatt-serve-pdu server chan pdu)))))

;;; --- server-initiated traffic ------------------------------------------

(defun gatt-subscribed-p (server cccd-handle &key indications)
  "Has the client enabled notifications (or indications) on this CCCD?"
  (let ((v (gethash cccd-handle (gatt-server-cccd server) 0)))
    (logbitp (if indications 1 0) v)))

(defun gatt-notify (server chan value-handle value &key cccd-handle indications force)
  "Send a Handle Value Notification, or an Indication when INDICATIONS.

Refuses unless the client has subscribed, because sending regardless is a
protocol violation and looks to the client like traffic it never asked for.
CCCD-HANDLE defaults to the attribute immediately after the value, which is
where GATT-ADD-CHARACTERISTIC puts it. FORCE skips the check, for testing a
client's handling of unsolicited traffic."
  (let ((cccd (or cccd-handle (1+ value-handle))))
    (when (or force (gatt-subscribed-p server cccd :indications indications))
      (let* ((value (%as-value value))
             (room (max 0 (- (gatt-server-mtu server) 3)))
             (value (if (> (length value) room) (subseq value 0 room) value))
             (pdu (make-octets (+ 3 (length value)))))
        (setf (aref pdu 0) (if indications +att-handle-value-ind+
                               +att-handle-value-ntf+))
        (u16le-put pdu 1 value-handle)
        (replace pdu value :start1 3)
        (att-send chan pdu)
        t))))
