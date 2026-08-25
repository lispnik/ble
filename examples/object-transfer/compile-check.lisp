;;;; Load the object server and drive both control points, without a radio.
;;;;
;;;; The channel itself needs two radios and is verified by running it. What
;;;; is checkable here is everything that decides whether the transfer is
;;;; allowed to happen at all: the metadata encodings, the navigation, and the
;;;; four separate reasons an OACP Read can be refused -- which are the part a
;;;; server is most likely to collapse into one.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename "./")) :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble/examples))

(defvar *problems* 0)
(defun check (ok fmt &rest args)
  (format t "~&[~A] ~?~%" (if ok " ok " "FAIL") fmt args)
  (unless ok (incf *problems*)))

;; --- the metadata encodings ---------------------------------------------
(let ((v (object-transfer:object-size-value 300 1024)))
  (check (= 8 (length v)) "Object Size is two 32-bit numbers, not one")
  (check (= 300 (+ (aref v 0) (ash (aref v 1) 8))) "current size first")
  (check (= 1024 (+ (aref v 4) (ash (aref v 5) 8))) "then allocated"))

(let ((v (object-transfer:object-id-value #x0000000001FF)))
  (check (= 6 (length v)) "an Object ID is 48 bits -- six octets, not eight")
  (check (and (= #xFF (aref v 0)) (= #x01 (aref v 1))) "little-endian")
  (check (every #'zerop (subseq v 2)) "and the top octets are clear"))

(let ((r (object-transfer:oacp-response #x05 #x06)))
  (check (= #x60 (aref r 0)) "an OACP reply is op code 0x60")
  (check (= #x05 (aref r 1)) "carrying the request it answers")
  (check (= #x06 (aref r 2)) "and its result"))
(check (= #x70 (aref (object-transfer:olcp-response #x01 #x01) 0))
       "an OLCP reply is 0x70 -- a different op code on a different ~
        characteristic")

;; --- navigation and refusals --------------------------------------------
(defun bytes (&rest octets)
  (coerce octets '(simple-array (unsigned-byte 8) (*))))

(let* ((state (object-transfer:build-server))
       (server (slot-value state 'object-transfer::server))
       (olcp-write (ble:gatt-attribute-on-write
                    (ble:gatt-find-attribute
                     server (slot-value state 'object-transfer::olcp-handle))))
       (oacp-write (ble:gatt-attribute-on-write
                    (ble:gatt-find-attribute
                     server (slot-value state 'object-transfer::oacp-handle)))))
  (flet ((olcp (&rest o) (funcall olcp-write server nil (apply #'bytes o)))
         (oacp (&rest o) (funcall oacp-write server nil (apply #'bytes o)))
         (result () (let ((p (slot-value state 'object-transfer::pending)))
                      (and p (aref (third p) 2))))
         (clear () (setf (slot-value state 'object-transfer::pending) nil))
         (subscribe (which)
           (setf (gethash (slot-value state which) (ble:gatt-server-cccd server)) 2)))
    ;; Nothing is subscribed, so neither control point will accept a write:
    ;; its result would have nowhere to go.
    (check (= #xFD (olcp #x01)) "an unsubscribed list control point is 0xFD")
    (check (= #xFD (oacp #x05)) "and so is the action control point")
    (subscribe 'object-transfer::olcp-cccd)
    (subscribe 'object-transfer::oacp-cccd)

    ;; An empty server: No Object, which is not the same as running off the end.
    (clear)
    (olcp #x01)
    (check (= 7 (result)) "navigating an empty list is No Object, not failure")

    (object-transfer:add-object state "a.txt" "alpha")
    (object-transfer:add-object state "b.txt" "beta")
    (object-transfer:add-object state "secret" "no" :properties 0)

    (clear) (olcp #x01)
    (check (= 1 (result)) "First succeeds once there are objects")
    (check (string= "a.txt" (slot-value (funcall (find-symbol "CURRENT-OBJECT"
                                                              :object-transfer)
                                                 state)
                                        'object-transfer::name))
           "and selects the first")
    (clear) (olcp #x04)
    (check (= 1 (result)) "Next moves on")
    (clear) (olcp #x02)
    (check (= 1 (result)) "Last jumps to the end")
    (clear) (olcp #x04)
    (check (= 5 (result)) "and Next past the end is Out Of Bounds -- a client ~
                           stops here rather than treating it as an error")
    (clear) (olcp #x03)
    (check (= 1 (result)) "Previous comes back")
    (clear) (olcp #x7F)
    (check (= 2 (result)) "an unknown op code is Op Code Not Supported")

    ;; Goto, by 48-bit object ID.
    (clear) (olcp #x05 #x00 #x01 #x00 #x00 #x00 #x00)
    (check (= 1 (result)) "Goto finds the first object by its ID (0x100)")
    (clear) (olcp #x05 #xFF #xFF #x00 #x00 #x00 #x00)
    (check (= 8 (result)) "and an ID that is not there is Object ID Not Found")
    (clear) (olcp #x05 #x00)
    (check (= 3 (result)) "a truncated ID is Invalid Parameter")

    ;; The four reasons a read is refused, each distinct. Collapsing these
    ;; into one code is the mistake this section exists to prevent.
    (clear) (olcp #x01)                        ; select a.txt, readable
    (clear)
    (check (= 6 (progn (oacp #x05 0 0 0 0 5 0 0 0) (result)))
           "readable object, but no channel open: Channel Unavailable")
    (clear) (olcp #x02)                        ; select "secret"
    (clear)
    (check (= 8 (progn (oacp #x05 0 0 0 0 2 0 0 0) (result)))
           "an object that forbids reading: Procedure Not Permitted -- the ~
            server can, this object may not")
    (clear)
    (check (= 2 (progn (oacp #x01) (result)))
           "an op code this server does not implement: Op Code Not Supported")
    (clear) (olcp #x01) (clear)
    (check (= 3 (progn (oacp #x05 0 0) (result)))
           "and Read without its offset and length is Invalid Parameter")

    ;; With a channel, the read is accepted and the payload prepared.
    (clear)
    (setf (slot-value state 'object-transfer::channel) :pretend-channel)
    (oacp #x05 0 0 0 0 5 0 0 0)
    (check (= 1 (result)) "with a channel open the same read succeeds")
    (let ((payload (fourth (slot-value state 'object-transfer::pending))))
      (check (and payload (= 5 (length payload)))
             "and the object's bytes are queued for the channel, not the ~
              attribute")
      (check (string= "alpha" (map 'string #'code-char payload))
             "and they are the right bytes"))))

;; --- the database -------------------------------------------------------
(let* ((state (object-transfer:build-server))
       (server (slot-value state 'object-transfer::server))
       (uuids (loop for a across (ble:gatt-server-attributes server)
                    collect (ble:uuid-string (ble:gatt-attribute-uuid a)))))
  (check (equal '("1800" "1801" "1825")
                (mapcar #'ble:uuid-string
                        (mapcar #'ble:gatt-service-entry-uuid
                                (reverse (ble:gatt-server-services server)))))
         "Generic Access, Generic Attribute, then Object Transfer")
  (dolist (u '("2ABD" "2ABE" "2ABF" "2AC0" "2AC3" "2AC4" "2AC5" "2AC6"))
    (check (member u uuids :test #'string=) "~A is present" u))
  ;; The object's bytes are conspicuously not here, which is the point.
  (check (notany (lambda (a) (> (length (ble:gatt-attribute-value a)) 20))
                 (ble:gatt-server-attributes server))
         "and no attribute holds anything large -- the objects travel by ~
          another road entirely"))

(check (find-package :object-transfer-client) "the client half loads too")

(format t "~&~%OBJECT TRANSFER CHECK: ~:[clean~;~:*~D problem(s)~]~%" *problems*)
(sb-ext:exit :code (if (zerop *problems*) 0 1))
