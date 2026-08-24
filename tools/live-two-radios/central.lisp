;;;; Dongle B (hci2) as the CENTRAL, using the library under test.
;;;;
;;;; The peer notifies on two handles at once. Before the dispatch work, a
;;;; client asking for one handle dropped every notification on the other, so
;;;; the B column below would read 0 no matter how many were sent.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))

(defparameter *peer* "3C:64:CF:2D:55:A3")   ; hci1, the peripheral
(defparameter *dev* 2)                      ; hci2, this side
(defparameter +handle-a+ #x0010)
(defparameter +handle-b+ #x0020)
(defparameter +pairs+ 10)

(ble:install-adapter-teardown)

(let ((chan nil) (fails 0))
  (flet ((check (ok label)
           (format t "~&  [~A] ~A~%" (if ok " ok " "FAIL") label)
           (unless ok (incf fails))
           (force-output)))
    (unwind-protect
         (progn
           (setf chan (ble:hci-user-att-connect
                       (ble:parse-mac *peer*)
                       :addr-type :public :dev *dev* :init-phys #x01
                       :timeout 25 :retries 3))
           (format t "~&[central] connected to ~A on hci~D~%" *peer* *dev*)
           (force-output)

           ;; 1. Ask only for handle A. Every B that arrives meanwhile must be
           ;;    kept, which is the entire point of the change.
           (let ((got-a 0))
             (dotimes (i +pairs+)
               (when (ble:att-next-notification chan +handle-a+ 4000)
                 (incf got-a)))
             (format t "~&[central] handle A received: ~D/~D~%" got-a +pairs+)
             (check (>= got-a (1- +pairs+)) "handle A notifications arrive"))

           (let ((queued (ble:att-pending-notifications chan +handle-b+)))
             (format t "~&[central] handle B queued while we asked for A: ~D~%" queued)
             (check (plusp queued)
                    "handle B was KEPT, not dropped (this is the bug)"))

           ;; 2. The any-handle API, while the queue still holds the B's --
           ;;    it must report WHICH characteristic spoke.
           (multiple-value-bind (value handle) (ble:att-next-notification-any chan 4000)
             (format t "~&[central] next-any: handle 0x~4,'0X value ~A~%"
                     (or handle 0) value)
             (check (and value (member handle (list +handle-a+ +handle-b+)))
                    "att-next-notification-any names its handle"))

           ;; 3. Drain the other handle. These are notifications the old code
           ;;    had already thrown away by this point.
           (let ((got-b 0))
             (loop repeat (* 2 +pairs+)
                   for v = (ble:att-next-notification chan +handle-b+ 1500)
                   while v do (incf got-b))
             (format t "~&[central] handle B received: ~D~%" got-b)
             (check (>= got-b (- +pairs+ 2)) "handle B notifications are claimable"))

           ;; 4. Conditions, against a peer that really refuses the write.
           (let ((r (ble:att-write-value chan #x00FF (vector 1))))
             (format t "~&[central] default-style write returned: ~S~%" r)
             (check (or (integerp r) (eq r :timeout))
                    "default style returns a sentinel, never a condition"))
           ;; Catch the whole hierarchy, then assert on which one arrived --
           ;; that is the property the hierarchy exists for.
           (handler-case
               (ble:with-ble-conditions
                 (ble:att-write-value chan #x00FF (vector 1))
                 (check nil "with-ble-conditions should have signalled"))
             (ble:ble-error (e)
               (format t "~&[central] signalled: ~A (~A)~%" e (type-of e))
               (check t "one handler for BLE-ERROR caught it")
               (when (typep e 'ble:att-error)
                 (check (= 1 (ble:att-error-code e)) "typed att-error, code 0x01")
                 (check (= #x00FF (ble:att-error-handle e))
                        "and it names the handle")))))
      (when chan (ignore-errors (ble:att-channel-close chan)))
      (format t "~&[central] adapter handed back~%")))
  (format t "~&LIVE RESULT: ~:[~D CHECK(S) FAILED~;ALL CHECKS PASSED~]~%"
          (zerop fails) fails)
  (force-output))
(sb-ext:exit)
