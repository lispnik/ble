;;;; A Bluetooth object server: files over an L2CAP channel.
;;;;
;;;; The sixth example, and the only one whose payload never crosses an
;;;; attribute. Everything else here sends its data as notifications, which
;;;; caps a message at the ATT MTU and makes anything larger somebody's
;;;; chunking scheme. Object Transfer is the SIG's answer to that, and it is
;;;; the only adopted profile that opens an L2CAP connection-oriented channel:
;;;; a real stream, with credit-based flow control, alongside the ATT bearer
;;;; on the same connection.
;;;;
;;;; So the service has two halves that have to agree. GATT carries the
;;;; metadata -- which object is selected, what it is called, how big it is --
;;;; and two control points to navigate and to ask for it. The bytes go over
;;;; the channel. A client that writes `read this object' without having
;;;; opened the channel first is refused with Channel Unavailable, because the
;;;; server has nowhere to put the answer.
;;;;
;;;; Two control points, not one, and the split is not arbitrary. The Object
;;;; List Control Point moves through the list -- first, next, previous --
;;;; and the Object Action Control Point acts on whichever object that landed
;;;; on. Navigating is not an action on an object; it is what decides which
;;;; object the actions will apply to.
;;;;
;;;; Its own package, using only exported symbols, for the same reason the
;;;; others are.

(defpackage #:object-transfer
  (:use #:common-lisp)
  (:export #:build-server #:run #:add-object #:*name* #:+ots-psm+
           #:object-size-value #:object-id-value #:olcp-response #:oacp-response))

(in-package #:object-transfer)

(defparameter *name* "Lisp Objects")

;;; The LE PSM the channel is opened to. The SIG assigns one to Object
;;; Transfer; both halves of this example agree on this value, so the example
;;; works whatever it is, but check it against Assigned Numbers before
;;; pointing somebody else's OTS client at this server -- a wrong PSM is
;;; refused as `SPSM not supported', which at least fails loudly.
(defconstant +ots-psm+ #x0025)

;;; --- what the profile defines -------------------------------------------

;;; Object Action Control Point op codes.
(defconstant +oacp-create+   #x01)
(defconstant +oacp-delete+   #x02)
(defconstant +oacp-checksum+ #x03)
(defconstant +oacp-execute+  #x04)
(defconstant +oacp-read+     #x05)
(defconstant +oacp-write+    #x06)
(defconstant +oacp-abort+    #x07)
(defconstant +oacp-response+ #x60)

;;; ...and its result codes.
(defconstant +oacp-success+              #x01)
(defconstant +oacp-op-code-not-supported+ #x02)
(defconstant +oacp-invalid-parameter+    #x03)
(defconstant +oacp-insufficient-resources+ #x04)
(defconstant +oacp-invalid-object+       #x05)
(defconstant +oacp-channel-unavailable+  #x06)
(defconstant +oacp-unsupported-type+     #x07)
(defconstant +oacp-procedure-not-permitted+ #x08)
(defconstant +oacp-object-locked+        #x09)
(defconstant +oacp-operation-failed+     #x0A)

;;; Object List Control Point op codes.
(defconstant +olcp-first+    #x01)
(defconstant +olcp-last+     #x02)
(defconstant +olcp-previous+ #x03)
(defconstant +olcp-next+     #x04)
(defconstant +olcp-goto+     #x05)
(defconstant +olcp-response+ #x70)

;;; ...and its result codes. Note that these are a different table from the
;;; OACP's above, sharing some numbers with different meanings -- 0x05 is
;;; Invalid Object on one and Out Of Bounds on the other. A client that
;;; decodes a reply against the wrong table gets a plausible wrong answer.
(defconstant +olcp-success+              #x01)
(defconstant +olcp-op-code-not-supported+ #x02)
(defconstant +olcp-invalid-parameter+    #x03)
(defconstant +olcp-operation-failed+     #x04)
(defconstant +olcp-out-of-bounds+        #x05)
(defconstant +olcp-too-many-objects+     #x06)
(defconstant +olcp-no-object+            #x07)
(defconstant +olcp-object-id-not-found+  #x08)

;;; Object Properties bits: what may be done to the selected object.
(defconstant +prop-delete+   #x01)
(defconstant +prop-execute+  #x02)
(defconstant +prop-read+     #x04)
(defconstant +prop-write+    #x08)

;;; OTS Feature bits: what this server implements at all, which is a
;;; different question from what the selected object permits.
(defconstant +feature-oacp-read+ #x00000010)
(defconstant +feature-olcp-goto+ #x00000001)

;;; The first 256 Object IDs are reserved by the profile, so a server's own
;;; objects start above them. Numbering from zero looks fine until a client
;;; that knows the rule refuses the ID.
(defconstant +first-object-id+ #x000000000100)

;;; --- encoding the metadata ----------------------------------------------

(defun u32-octets (n)
  (let ((v (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4 v) (setf (aref v i) (ldb (byte 8 (* 8 i)) n)))))

(defun object-size-value (current allocated)
  "Object Size (0x2AC0): the current size and the allocated size, both 32-bit.

Two numbers, not one. A client reading only the first four octets gets the
right answer for a read and the wrong one for a write."
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (u32-octets current) (u32-octets allocated)))

(defun object-id-value (id)
  "Object ID (0x2AC3): 48 bits, little-endian. Six octets, not eight -- an
ID written as a u64 puts two zeroes where the next field belongs."
  (let ((v (make-array 6 :element-type '(unsigned-byte 8))))
    (dotimes (i 6 v) (setf (aref v i) (ldb (byte 8 (* 8 i)) id)))))

(defun olcp-response (request-op result)
  "An OLCP Response Code: which request, and how it went."
  (let ((v (make-array 3 :element-type '(unsigned-byte 8))))
    (setf (aref v 0) +olcp-response+ (aref v 1) request-op (aref v 2) result)
    v))

(defun oacp-response (request-op result)
  "An OACP Response Code. Same shape as the OLCP's, a different op code, and
result codes from a different table."
  (let ((v (make-array 3 :element-type '(unsigned-byte 8))))
    (setf (aref v 0) +oacp-response+ (aref v 1) request-op (aref v 2) result)
    v))

;;; --- the device ---------------------------------------------------------

(defstruct obj
  "One object: its metadata, and the bytes that never cross an attribute."
  id name type properties content)

(defstruct server-state
  "The GATT server, the object list, and whichever object is selected.

CHANNEL is the open L2CAP channel or NIL, and its being NIL is a protocol
answer rather than an internal detail: an OACP Read with no channel is
refused with Channel Unavailable."
  server objects (current nil) channel
  name-handle type-handle size-handle id-handle props-handle
  oacp-handle oacp-cccd olcp-handle olcp-cccd
  (pending nil) (outstanding nil))

(defun add-object (state name content &key (type #xFFFF)
                                           (properties +prop-read+))
  "Add one object to the list. CONTENT is an octet vector or a string.

TYPE is a UUID naming what kind of thing this is. The SIG defines type UUIDs
for some standard kinds; this example carries its own rather than printing a
number it has not checked, which would be a confident claim about a format."
  (let* ((content (if (stringp content)
                      (map '(simple-array (unsigned-byte 8) (*)) #'char-code content)
                      (coerce content '(simple-array (unsigned-byte 8) (*)))))
         (o (make-obj :id (+ +first-object-id+ (length (server-state-objects state)))
                      :name name :type type :properties properties
                      :content content)))
    (setf (server-state-objects state)
          (append (server-state-objects state) (list o)))
    (unless (server-state-current state)
      (setf (server-state-current state) 0))
    o))

(defun current-object (state)
  (let ((i (server-state-current state)))
    (when i (nth i (server-state-objects state)))))

(defun publish-current (state)
  "Copy the selected object's metadata into the attributes that report it.

This is the half of OTS that is ordinary GATT: navigating with the list
control point does not send the client anything, it changes what these five
characteristics say when read. A server that moves the selection without
republishing leaves a client reading the previous object's name against the
new object's bytes."
  (let ((o (current-object state))
        (server (server-state-server state)))
    (cond
      (o (ble:gatt-set-value server (server-state-name-handle state) (obj-name o))
         (ble:gatt-set-value server (server-state-type-handle state)
                             (ble:uuid16 (obj-type o)))
         (ble:gatt-set-value server (server-state-size-handle state)
                             (object-size-value (length (obj-content o))
                                                (length (obj-content o))))
         (ble:gatt-set-value server (server-state-id-handle state)
                             (object-id-value (obj-id o)))
         (ble:gatt-set-value server (server-state-props-handle state)
                             (u32-octets (obj-properties o))))
      (t
       ;; No object selected. The profile has a defined way to say so -- an
       ;; Object ID of zero -- rather than leaving stale metadata in place.
       (ble:gatt-set-value server (server-state-id-handle state)
                           (object-id-value 0))
       (ble:gatt-set-value server (server-state-name-handle state) "")))))

;;; --- the list control point ---------------------------------------------

(defun handle-olcp-write (state value)
  "Move through the object list. Returns NIL to accept the write.

Every navigation answers with a result code, including the ones that did not
move: running off the end of the list is Out Of Bounds, and an empty list is
No Object. Those are different situations and a client can act on the
difference -- one means stop, the other means there was never anything here."
  (when (< (length value) 1)
    (return-from handle-olcp-write ble:+att-err-invalid-value-length+))
  (unless (ble:gatt-subscribed-p (server-state-server state)
                                 (server-state-olcp-cccd state) :indications t)
    ;; Its result would go nowhere.
    (return-from handle-olcp-write #xFD))
  (let* ((op (aref value 0))
         (objects (server-state-objects state))
         (n (length objects))
         (i (server-state-current state))
         (result
           (cond
             ((zerop n) +olcp-no-object+)
             ((= op +olcp-first+) (setf (server-state-current state) 0) +olcp-success+)
             ((= op +olcp-last+) (setf (server-state-current state) (1- n)) +olcp-success+)
             ((= op +olcp-next+)
              (if (and i (< (1+ i) n))
                  (progn (setf (server-state-current state) (1+ i)) +olcp-success+)
                  +olcp-out-of-bounds+))
             ((= op +olcp-previous+)
              (if (and i (> i 0))
                  (progn (setf (server-state-current state) (1- i)) +olcp-success+)
                  +olcp-out-of-bounds+))
             ((= op +olcp-goto+)
              (if (< (length value) 7)
                  +olcp-invalid-parameter+
                  (let* ((id (loop for k from 0 below 6
                                   sum (ash (aref value (+ 1 k)) (* 8 k))))
                         (pos (position id objects :key #'obj-id)))
                    (if pos
                        (progn (setf (server-state-current state) pos) +olcp-success+)
                        +olcp-object-id-not-found+))))
             (t +olcp-op-code-not-supported+))))
    (publish-current state)
    (setf (server-state-pending state) (list :reply :olcp (olcp-response op result)))
    nil))

;;; --- the action control point -------------------------------------------

(defun handle-oacp-write (state value)
  "Ask for the selected object. Returns NIL to accept the write.

The interesting refusal is Channel Unavailable. A client that has not opened
the L2CAP channel is asking the server to send bytes with nowhere to send
them, and saying so is more use than failing later or transferring into the
void."
  (when (< (length value) 1)
    (return-from handle-oacp-write ble:+att-err-invalid-value-length+))
  (unless (ble:gatt-subscribed-p (server-state-server state)
                                 (server-state-oacp-cccd state) :indications t)
    (return-from handle-oacp-write #xFD))
  (when (server-state-pending state)
    (return-from handle-oacp-write #xFE))     ; procedure already in progress
  (let ((op (aref value 0))
        (o (current-object state)))
    (cond
      ((/= op +oacp-read+)
       (setf (server-state-pending state)
             (list :reply :oacp (oacp-response op +oacp-op-code-not-supported+)))
       nil)
      ;; The request is checked before anything about the server's state.
      ;; Read carries a 32-bit offset and a 32-bit length, and a write too
      ;; short to hold them is malformed however the server is placed --
      ;; answering Channel Unavailable would send the client off to open a
      ;; channel that was never the problem.
      ((< (length value) 9)
       (setf (server-state-pending state)
             (list :reply :oacp (oacp-response op +oacp-invalid-parameter+)))
       nil)
      ((null o)
       (setf (server-state-pending state)
             (list :reply :oacp (oacp-response op +oacp-invalid-object+)))
       nil)
      ((zerop (logand (obj-properties o) +prop-read+))
       ;; The object exists and the server implements Read; this particular
       ;; object simply does not permit it. A different answer from the two
       ;; above, and the client can tell.
       (setf (server-state-pending state)
             (list :reply :oacp (oacp-response op +oacp-procedure-not-permitted+)))
       nil)
      ((null (server-state-channel state))
       (setf (server-state-pending state)
             (list :reply :oacp (oacp-response op +oacp-channel-unavailable+)))
       nil)
      (t
       (let* ((content (obj-content o))
              (offset (loop for k from 0 below 4
                            sum (ash (aref value (+ 1 k)) (* 8 k))))
              (length (loop for k from 0 below 4
                            sum (ash (aref value (+ 5 k)) (* 8 k)))))
         (if (or (> offset (length content))
                 (> (+ offset length) (length content)))
             (setf (server-state-pending state)
                   (list :reply :oacp (oacp-response op +oacp-invalid-parameter+)))
             ;; Accepted: answer first, then transfer. The order matters --
             ;; the client is waiting on the response before it reads the
             ;; channel, and bytes arriving first would look like a stream it
             ;; had not agreed to.
             (setf (server-state-pending state)
                   (list :reply :oacp (oacp-response op +oacp-success+)
                         (subseq content offset (+ offset length)))))
         nil)))))

;;; --- driving the procedure ----------------------------------------------

(defun run-pending (state conn)
  "Send whatever the last control point write decided on. One step per tick."
  (let ((pending (server-state-pending state)))
    (when (and pending (not (server-state-outstanding state)))
      (destructuring-bind (kind which octets &optional payload) pending
        (declare (ignore kind))
        (let ((handle (ecase which
                        (:olcp (server-state-olcp-handle state))
                        (:oacp (server-state-oacp-handle state)))))
          (when (ble:gatt-notify (server-state-server state) conn handle octets
                                 :indications t)
            (setf (server-state-outstanding state) t
                  (server-state-pending state) nil)
            ;; And now the object itself, over the channel rather than the
            ;; attribute bearer. This is the whole reason the service exists.
            (when payload
              (let ((r (ble:l2cap-coc-send (server-state-channel state) payload
                                           :timeout-ms 5000)))
                (format t "~&  sent ~D octet(s) over the channel: ~A~%"
                        (length payload) r)
                (force-output)))))))))

;;; --- the database -------------------------------------------------------

(defun build-server ()
  "Generic Access, Generic Attribute, and the Object Transfer service."
  (let ((server (ble:make-gatt-server :mtu 23))
        (state nil))
    (ble:gatt-add-service server ble:+service-generic-access+)
    (ble:gatt-add-characteristic server :uuid ble:+char-device-name+
                                        :properties '(:read) :value *name*)
    (ble:gatt-add-characteristic server :uuid ble:+char-appearance+
                                        :properties '(:read)
                                        :value (ble:appearance ble:+appearance-generic-tag+))
    (ble:gatt-add-service server ble:+service-generic-attribute+)
    (ble:gatt-add-characteristic server :uuid ble:+char-service-changed+
                                        :properties '(:indicate)
                                        :value (ble:service-changed-range))
    (ble:gatt-add-service server ble:+service-object-transfer+)
    ;; What this server implements: OACP Read and OLCP Goto. Claiming a
    ;; feature bit it does not implement is how a client is told to expect a
    ;; procedure that will be refused.
    (ble:gatt-add-characteristic
     server :uuid ble:+char-ots-feature+ :properties '(:read)
            :value (concatenate '(simple-array (unsigned-byte 8) (*))
                                (u32-octets +feature-oacp-read+)
                                (u32-octets +feature-olcp-goto+)))
    (let ((name-handle (ble:gatt-add-characteristic
                        server :uuid ble:+char-object-name+ :properties '(:read)
                               :value ""))
          (type-handle (ble:gatt-add-characteristic
                        server :uuid ble:+char-object-type+ :properties '(:read)
                               :value (ble:uuid16 #xFFFF)))
          (size-handle (ble:gatt-add-characteristic
                        server :uuid ble:+char-object-size+ :properties '(:read)
                               :value (object-size-value 0 0)))
          (id-handle (ble:gatt-add-characteristic
                      server :uuid ble:+char-object-id+ :properties '(:read)
                             :value (object-id-value 0)))
          (props-handle (ble:gatt-add-characteristic
                         server :uuid ble:+char-object-properties+
                                :properties '(:read) :value (u32-octets 0))))
      (multiple-value-bind (oacp oacp-cccd)
          (ble:gatt-add-characteristic
           server :uuid ble:+char-object-action-control-point+
                  :properties '(:write :indicate)
                  :on-write (lambda (s a v) (declare (ignore s a))
                              (handle-oacp-write state v)))
        (multiple-value-bind (olcp olcp-cccd)
            (ble:gatt-add-characteristic
             server :uuid ble:+char-object-list-control-point+
                    :properties '(:write :indicate)
                    :on-write (lambda (s a v) (declare (ignore s a))
                                (handle-olcp-write state v)))
          (setf state (make-server-state
                       :server server :objects '()
                       :name-handle name-handle :type-handle type-handle
                       :size-handle size-handle :id-handle id-handle
                       :props-handle props-handle
                       :oacp-handle oacp :oacp-cccd oacp-cccd
                       :olcp-handle olcp :olcp-cccd olcp-cccd))
          state)))))

;;; --- running it ---------------------------------------------------------

(defun run (&key (dev nil) (seconds nil))
  "Advertise as an object server holding three small objects."
  (let* ((state (build-server))
         (dev (or dev (ble:default-hci-dev))))
    (add-object state "readme.txt"
                "This object crossed an L2CAP connection-oriented channel,
not a characteristic. That is what Object Transfer is for.")
    (add-object state "reading.csv"
                (format nil "~{~A~%~}"
                        (list "seq,celsius" "0,21.5" "1,21.75" "2,22.0")))
    ;; One object the server will refuse to read, so the difference between
    ;; `cannot' and `may not' is visible from the client.
    (add-object state "private.key" "not for you" :properties 0)
    (publish-current state)
    (ble:install-adapter-teardown)
    (ble:with-hci-user-socket (sock dev)
      (let ((addr (ble:static-random-address (ble:smp-random-octets sock 6))))
        (ble:set-random-address sock addr)
        (ble:set-adv-parameters sock :adv-type ble:+adv-ind+ :own-addr-type 1)
        (ble:set-adv-data sock (ble:adv-data
                                :flags '(:general-discoverable :no-bredr)
                                :name *name*
                                :services-16 (list ble:+service-object-transfer+)))
        (format t "~&~A advertising on hci~D as ~A with ~D object(s)~%"
                *name* dev (ble:format-mac addr) (length (server-state-objects state)))
        (force-output)
        (ble:serve-peripheral
         (server-state-server state) sock
         :seconds seconds
         :on-connect
         (lambda (conn peer ptype)
           (setf (server-state-channel state) nil
                 (server-state-pending state) nil
                 (server-state-outstanding state) nil
                 (server-state-current state) 0)
           (publish-current state)
           ;; Listen before the client asks. A connection request for a PSM
           ;; nobody is listening on is refused immediately, so registering
           ;; late is not a slow path -- it is a failure.
           (ble:l2cap-coc-listen conn +ots-psm+)
           (format t "~&connected: ~A (~(~A~)); listening on PSM 0x~4,'0X~%"
                   (ble:format-mac peer) ptype +ots-psm+)
           (force-output))
         :on-disconnect
         (lambda (conn)
           (declare (ignore conn))
           (setf (server-state-channel state) nil
                 (server-state-pending state) nil
                 (server-state-outstanding state) nil)
           (format t "~&disconnected; advertising again~%")
           (force-output))
         :on-tick
         (lambda (conn request)
           (when (eql request ble:+att-handle-value-cfm+)
             (setf (server-state-outstanding state) nil))
           ;; Timeout 0: SERVE-PERIPHERAL is the only reader of this socket,
           ;; and a blocking accept here would be a second one racing it.
           ;; This only claims channels the receive path has already built.
           (unless (server-state-channel state)
             (let ((coc (ble:l2cap-coc-accept conn :timeout-ms 0)))
               (when (ble:l2cap-coc-p coc)
                 (setf (server-state-channel state) coc)
                 (format t "~&  channel open: mtu ~D, ~D credit(s) to send with~%"
                         (ble:l2cap-coc-peer-mtu coc) (ble:l2cap-coc-tx-credits coc))
                 (force-output))))
           (run-pending state conn)))))))
