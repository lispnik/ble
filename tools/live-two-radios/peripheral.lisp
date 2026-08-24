;;;; Dongle A (hci1) as a PERIPHERAL that notifies on TWO handles.
;;;;
;;;; ble has no GATT server, so this is not one: it owns the controller via
;;;; HCI_CHANNEL_USER, advertises connectable, and once a central attaches it
;;;; pushes Handle Value Notifications on two handles and answers any request
;;;; with an ATT Error Response. That is the whole peer surface the client
;;;; side of this test needs, and it is a peer our own code did not write the
;;;; client half against.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))
(in-package #:ble)

(defparameter +dev+ 1)
(defparameter +handle-a+ #x0010)
(defparameter +handle-b+ #x0020)
(defparameter +pairs+ 10)

(defun ntf-pdu (handle payload)
  (let* ((payload (coerce-octets payload))
         (pdu (make-octets (+ 3 (length payload)))))
    (setf (aref pdu 0) #x1B)
    (u16le-put pdu 1 handle)
    (replace pdu payload :start1 3)
    pdu))

(defun await-connection (sock timeout-ms)
  "Wait for an LE Connection Complete and return its handle."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms internal-time-units-per-second) 1000))))
    (loop
      (when (<= (- deadline (get-internal-real-time)) 0) (return nil))
      (let ((pkt (hci-poll-read sock 500)))
        (when (and pkt (>= (length pkt) 7)
                   (= (aref pkt 0) #x04) (= (aref pkt 1) #x3E)
                   (member (aref pkt 3) '(#x01 #x0A)))
          (let ((status (aref pkt 4)))
            (format t "~&[peripheral] connection complete, status ~D~%" status)
            (force-output)
            (return (when (zerop status) (u16-le pkt 5)))))))))

(let ((sock nil))
  (unwind-protect
       (block done
         (multiple-value-bind (s acl-len) (open-hci-user-socket +dev+)
           (setf sock s)
           (format t "~&[peripheral] hci~D owned, acl-len ~D~%" +dev+ acl-len)
           ;; connectable undirected, public own-address, 100 ms
           (set-adv-parameters sock :adv-type +adv-ind+ :own-addr-type 0)
           ;; AD: flags, then the complete local name "TWOHAND"
           (set-adv-data sock (concatenate '(vector (unsigned-byte 8))
                                           (vector 2 1 6 8 9)
                                           (map 'vector #'char-code "TWOHAND")))
           (set-adv-enable sock t)
           (format t "~&[peripheral] advertising as TWOHAND~%") (force-output)
           (let ((handle (await-connection sock 60000)))
             (unless handle
               (format t "~&[peripheral] no central attached~%")
               (return-from done))
             (let ((conn (make-hci-conn :sock sock :handle handle :acl-len acl-len)))
               (format t "~&[peripheral] connected, handle 0x~4,'0X~%" handle)
               (force-output)
               ;; Alternate the two handles. A client that filters on one and
               ;; drops the rest loses exactly half of this.
               (dotimes (i +pairs+)
                 (att-send conn (ntf-pdu +handle-a+ (vector (+ #xA0 i))))
                 (att-send conn (ntf-pdu +handle-b+ (vector (+ #xB0 i))))
                 ;; answer anything the central asks, so the client's error
                 ;; path has a real peer to hear it from
                 (let ((pdu (att-recv conn 120)))
                   (when (and pdu (vectorp pdu) (plusp (length pdu))
                              (member (aref pdu 0) '(#x12 #x0A)))
                     (let ((err (make-octets 5)))
                       (setf (aref err 0) #x01
                             (aref err 1) (aref pdu 0))
                       (u16le-put err 2 (if (>= (length pdu) 3) (u16-le pdu 1) 0))
                       (setf (aref err 4) #x01) ; invalid handle
                       (att-send conn err)
                       (format t "~&[peripheral] refused request 0x~2,'0X~%"
                               (aref pdu 0))
                       (force-output)))))
               (format t "~&[peripheral] sent ~D on each handle~%" +pairs+)
               (force-output)
               ;; Hold the link open so the client can drain its queue --
               ;; and keep refusing requests, so its error path has a live
               ;; peer for the whole run rather than only during the sends.
               (dotimes (i 120)
                 (let ((pdu (att-recv conn 250)))
                   (when (and pdu (vectorp pdu) (plusp (length pdu))
                              (member (aref pdu 0) '(#x12 #x0A)))
                     (let ((err (make-octets 5)))
                       (setf (aref err 0) #x01
                             (aref err 1) (aref pdu 0))
                       (u16le-put err 2 (if (>= (length pdu) 3) (u16-le pdu 1) 0))
                       (setf (aref err 4) #x01)
                       (att-send conn err)
                       (format t "~&[peripheral] refused request 0x~2,'0X~%"
                               (aref pdu 0))
                       (force-output)))))))))
    (when sock
      (ignore-errors (set-adv-enable sock nil))
      (close-hci-user-socket sock)
      (format t "~&[peripheral] adapter handed back~%") (force-output))))
(sb-ext:exit)
