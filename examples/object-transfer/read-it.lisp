;;;; Connect to the object server, walk its list, and download an object.
;;;;
;;;; The client half, and the only one that opens a second L2CAP channel.
;;;; Everything before the download is ordinary GATT -- discover the service,
;;;; navigate with the list control point, read the metadata characteristics
;;;; to see where you landed. The object itself arrives on the channel.
;;;;
;;;; The order matters and is easy to get backwards: open the channel first,
;;;; then ask. A server with nowhere to send bytes refuses the read with
;;;; Channel Unavailable rather than failing halfway through it.
(defpackage #:object-transfer-client (:use #:common-lisp) (:export #:run))
(in-package #:object-transfer-client)

(defconstant +oacp-read+     #x05)
(defconstant +olcp-first+    #x01)
(defconstant +olcp-next+     #x04)
(defconstant +olcp-out-of-bounds+ #x05)

(defun u32-le (v i) (loop for k from 0 below 4 sum (ash (aref v (+ i k)) (* 8 k))))
(defun u48-le (v i) (loop for k from 0 below 6 sum (ash (aref v (+ i k)) (* 8 k))))

(defun oacp-result-name (code)
  (case code
    (1 :success) (2 :op-code-not-supported) (3 :invalid-parameter)
    (4 :insufficient-resources) (5 :invalid-object) (6 :channel-unavailable)
    (7 :unsupported-type) (8 :procedure-not-permitted) (9 :object-locked)
    (10 :operation-failed) (t :unknown)))

(defun olcp-result-name (code)
  ;; A different table from the OACP's, sharing numbers with other meanings.
  (case code
    (1 :success) (2 :op-code-not-supported) (3 :invalid-parameter)
    (4 :operation-failed) (5 :out-of-bounds) (6 :too-many-objects)
    (7 :no-object) (8 :object-id-not-found) (t :unknown)))

(defstruct (found (:conc-name f-)) name size id properties)

(defun read-metadata (chan handles)
  "Read the five characteristics describing whichever object is selected."
  (destructuring-bind (name size id props) handles
    (make-found :name (map 'string #'code-char (ble:att-read-value chan name))
                :size (let ((v (ble:att-read-value chan size))) (u32-le v 0))
                :id (u48-le (ble:att-read-value chan id) 0)
                :properties (u32-le (ble:att-read-value chan props) 0))))

(defun navigate (chan olcp-handle op)
  "Write one op code to the list control point and decode its answer."
  (let ((v (make-array 1 :element-type '(unsigned-byte 8) :initial-element op)))
    (ble:att-write-value chan olcp-handle v))
  (let ((r (ble:att-next-notification chan olcp-handle 10000)))
    (if (and r (>= (length r) 3))
        (olcp-result-name (aref r 2))
        :no-reply)))

(defun run (mac &key (dev 2) (addr-type :random) (want "bulk.dat"))
  "Walk the object list, then download WANT (a name) or the first readable one.

Defaults to the big object, because a rate measured over a few dozen octets
is a measurement of the control point round trip rather than of the channel."
  (ble:install-adapter-teardown)
  (ble:with-att-channel
      (chan (ble:hci-user-att-connect (ble:parse-mac mac) :addr-type addr-type
                                      :dev dev :init-phys #x01 :timeout 25))
    (format t "~&connected to ~A~%" mac) (force-output)
    (ble:att-exchange-mtu chan 247)
    (let ((ots (ble:att-find-service
                chan (ble:uuid16 ble:+service-object-transfer+))))
      (unless ots (error "no object transfer service on this device"))
      (let* ((chars (ble:att-discover-characteristics
                     chan :start (ble:gatt-service-start ots)
                          :end (ble:gatt-service-end ots)))
             (h (lambda (uuid)
                  (let ((c (ble:find-char-by-uuid chars (ble:uuid16 uuid))))
                    (and c (ble:gatt-char-handle c)))))
             (feature (funcall h ble:+char-ots-feature+))
             (meta (list (funcall h ble:+char-object-name+)
                         (funcall h ble:+char-object-size+)
                         (funcall h ble:+char-object-id+)
                         (funcall h ble:+char-object-properties+)))
             (oacp (funcall h ble:+char-object-action-control-point+))
             (olcp (funcall h ble:+char-object-list-control-point+)))
        (unless (and oacp olcp (every #'identity meta))
          (error "this is not a complete object transfer server"))
        (when feature
          (let ((v (ble:att-read-value chan feature)))
            (format t "~&features: oacp=0x~8,'0X olcp=0x~8,'0X~%"
                    (u32-le v 0) (u32-le v 4))))
        ;; Both control points indicate, so both need subscribing before
        ;; either will accept a write.
        (ble:att-subscribe chan (ble:att-find-cccd chan oacp) :indications t)
        (ble:att-subscribe chan (ble:att-find-cccd chan olcp) :indications t)
        ;; Walk the list. There is no `how many objects are there' here --
        ;; you navigate until the server says Out Of Bounds, which is why
        ;; that result is distinct from an outright failure.
        (format t "~&objects:~%")
        (let ((objects '())
              (result (navigate chan olcp +olcp-first+)))
          (loop while (eq result :success)
                do (let ((m (read-metadata chan meta)))
                     (push m objects)
                     (format t "~&  ~20A ~6D octet(s)  id ~D~:[~;  (not readable)~]~%"
                             (f-name m) (f-size m) (f-id m)
                             (zerop (logand (f-properties m) #x04)))
                     (force-output)
                     (setf result (navigate chan olcp +olcp-next+)))
                finally (unless (eq result :out-of-bounds)
                          (format t "~&  (list ended with ~A)~%" result)))
          (setf objects (nreverse objects))
          ;; Select what we want, walking back from wherever Next left us.
          (let* ((target (or (find want objects :key #'f-name :test #'equal)
                             (find-if (lambda (m) (plusp (logand (f-properties m) #x04)))
                                      objects)))
                 (index (position target objects)))
            (unless target
              (format t "~&nothing readable to download~%")
              (return-from run nil))
            (navigate chan olcp +olcp-first+)
            (dotimes (i index) (navigate chan olcp +olcp-next+))
            (format t "~&downloading ~A (~D octets)~%" (f-name target) (f-size target))
            (force-output)
            ;; The channel, before the request. A server asked to read with
            ;; no channel open answers Channel Unavailable.
            (let ((coc (ble:l2cap-coc-connect chan object-transfer:+ots-psm+)))
              (unless (ble:l2cap-coc-p coc)
                (format t "~&could not open the channel: ~A~%" coc)
                (return-from run nil))
              (format t "~&channel open: peer mtu ~D~%" (ble:l2cap-coc-peer-mtu coc))
              (force-output)
              ;; OACP Read: offset 0, the whole object.
              (let ((req (make-array 9 :element-type '(unsigned-byte 8)
                                       :initial-element 0)))
                (setf (aref req 0) +oacp-read+)
                (dotimes (i 4) (setf (aref req (+ 5 i)) (ldb (byte 8 (* 8 i)) (f-size target))))
                (ble:att-write-value chan oacp req))
              (let ((r (ble:att-next-notification chan oacp 10000)))
                (format t "~&server says: ~A~%"
                        (if (and r (>= (length r) 3))
                            (oacp-result-name (aref r 2))
                            :no-reply))
                (force-output)
                (when (and r (>= (length r) 3) (= 1 (aref r 2)))
                  ;; An object arrives as one or more SDUs -- anything past the
                  ;; channel MTU has to. So read until the size the metadata
                  ;; promised, not until the first thing lands.
                  (let ((want (f-size target))
                        (got 0) (sdus 0) (first-at nil) (last-at nil)
                        (sample nil))
                    (loop while (< got want)
                          for sdu = (ble:l2cap-coc-recv coc :timeout-ms 10000)
                          do (cond
                               ((or (null sdu) (eq sdu :disconnected))
                                (format t "~&channel went quiet after ~D of ~D ~
                                           octet(s)~%" got want)
                                (return))
                               (t (unless first-at
                                    (setf first-at (get-internal-real-time)
                                          sample (subseq sdu 0 (min 48 (length sdu)))))
                                  (setf last-at (get-internal-real-time))
                                  (incf got (length sdu))
                                  (incf sdus))))
                    ;; Timed first SDU to last: the gap before the server
                    ;; starts is the control point round trip, not the channel.
                    (let ((secs (if (and first-at last-at (> last-at first-at))
                                    (/ (float (- last-at first-at))
                                       internal-time-units-per-second)
                                    0)))
                      (format t "~&received ~D octet(s) in ~D SDU(s)~@[ in ~,2F s ~
                                 = ~,1F kbit/s~]~%"
                              got sdus (and (plusp secs) secs)
                              (and (plusp secs) (/ (* 8 got) secs 1000.0)))
                      (when (and sample (every (lambda (c) (< 31 c 127)) sample))
                        (format t "~&--- begins ---~%~A~%---~%"
                                (map 'string #'code-char sample)))
                      (force-output)))))
              (ble:l2cap-coc-close coc))))))))
