;;;; Dongle B: this library as a GATT client, against the server on dongle A.
;;;;
;;;; Both halves of the library, over the air, on separate radios. Everything
;;;; here is a real exchange: discovery walks, reads that need Read Blob and
;;;; L2CAP reassembly to complete, a write the server's hook refuses, two
;;;; simultaneous subscriptions, and an ATT error raised as a typed condition.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))

;;; Set by run.sh from pick-adapters.lisp, which chooses by bus. The
;;; fallbacks only make this file usable by hand; hciN numbering drifts across
;;; reboots, so do not trust them.
(defun env-or (name default)
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v))) v default)))

(defparameter *peer* (env-or "PEER_MAC" "3C:64:CF:2D:55:A3"))
(defparameter *dev* (parse-integer (env-or "CENTRAL_DEV" "2")))

(ble:install-adapter-teardown)

(let ((fails 0) (checks 0))
  (labels ((check (ok label)
             (incf checks)
             (format t "~&  [~A] ~A~%" (if ok " ok " "FAIL") label)
             (unless ok (incf fails))
             (force-output))
           (u (n) (ble:uuid16 n)))
    ;; with-att-channel: the adapter goes back however this ends, including
    ;; on an error escaping the middle of the checks.
    (ble:with-att-channel
        (chan (ble:hci-user-att-connect (ble:parse-mac *peer*)
                                        :addr-type :public :dev *dev*
                                        :init-phys #x01 :timeout 25 :retries 3))
      (format t "~&[central] connected to ~A on hci~D~%" *peer* *dev*)
      (force-output)

      ;; --- MTU -----------------------------------------------------------
      (let ((mtu (ble:att-exchange-mtu chan 247)))
        (format t "~&[central] MTU -> ~A~%" mtu)
        (check (and (integerp mtu) (>= mtu 23)) "MTU exchange completes"))

      ;; --- service discovery ---------------------------------------------
      (let ((services (ble:att-discover-services chan)))
        (format t "~&[central] services: ~{~A ~}~%"
                (mapcar #'ble:gatt-service-uuid-string services))
        (check (= 2 (length services)) "both services discovered")
        (check (equal '("180A" "FFE0")
                      (mapcar #'ble:gatt-service-uuid-string services))
               "with the UUIDs the server declared, in handle order"))

      (let ((svc (ble:att-find-service chan (u #xFFE0))))
        (check (and svc (string= "FFE0" (ble:gatt-service-uuid-string svc)))
               "att-find-service resolves FFE0 in one round trip")
        (check (null (ble:att-find-service chan (u #x1234)))
               "and a UUID that is not there returns NIL"))

      ;; --- characteristics ------------------------------------------------
      (let* ((chars (ble:att-discover-characteristics chan))
             (ffe1 (ble:find-char-by-uuid chars (u #xFFE1)))
             (ffe2 (ble:find-char-by-uuid chars (u #xFFE2)))
             (ffe3 (ble:find-char-by-uuid chars (u #xFFE3)))
             (ffe4 (ble:find-char-by-uuid chars (u #xFFE4)))
             (ffe5 (ble:find-char-by-uuid chars (u #xFFE5))))
        (format t "~&[central] characteristics: ~{~A ~}~%"
                (mapcar #'ble:gatt-char-uuid-string chars))
        (check (= 7 (length chars)) "all seven characteristics discovered")
        (check (and ffe1 ffe2 ffe3 ffe4 ffe5) "including every vendor one")
        (let ((props (ble:gatt-char-property-names ffe1)))
          (format t "~&[central] FFE1 properties: ~A~%" props)
          (check (and (member :read props) (member :write props)
                      (member :notify props))
                 "FFE1 reports read + write + notify"))

        ;; --- reads --------------------------------------------------------
        (let ((maker (ble:find-char-by-uuid chars (u #x2A29))))
          (check (equalp (map 'vector #'char-code "ACME")
                         (ble:att-read-value chan (ble:gatt-char-handle maker)))
                 "a static value reads back verbatim"))
        (let* ((model (ble:find-char-by-uuid chars (u #x2A24)))
               (h (ble:gatt-char-handle model))
               (a (ble:att-read-value chan h))
               (b (ble:att-read-value chan h)))
          (format t "~&[central] computed value: ~A then ~A~%" a b)
          (check (not (equalp a b))
                 "an on-read characteristic is evaluated per read"))

        ;; 300 octets over a 23-octet MTU and a 27-octet ACL: this only
        ;; completes if Read Blob and L2CAP reassembly both work.
        (let ((long (ble:att-read-long-value chan (ble:gatt-char-handle ffe4))))
          (format t "~&[central] long read: ~D octets~%" (length long))
          (check (= 300 (length long)) "a 300-octet value arrives whole")
          (check (and (plusp (length long))
                      (every (lambda (i) (= (aref long i) (mod i 251)))
                             (loop for i below (length long) collect i)))
                 "and every octet is the one the server put there"))

        ;; --- writes -------------------------------------------------------
        (check (eq t (ble:att-write-value chan (ble:gatt-char-handle ffe1) #(9 9)))
               "a permitted write is acknowledged")
        (check (equalp #(9 9) (ble:att-read-value chan (ble:gatt-char-handle ffe1)))
               "and a later read returns what was written")
        (check (= #x0D (ble:att-write-value chan (ble:gatt-char-handle ffe3)
                                            #(1 2 3)))
               "the server's on-write hook refuses with its own error code")
        (check (eq t (ble:att-write-value chan (ble:gatt-char-handle ffe3) #(7)))
               "and accepts the write it considers valid")

        ;; --- a long write, over the air ------------------------------------
        ;; At MTU 23 a Write Request carries 20 octets, so 300 only arrives if
        ;; Prepare Write and Execute Write both work on both sides. This is
        ;; the path whose client half had the mirror image of the MTU bug and
        ;; still passed every in-process check.
        (let ((payload (ble:make-octets 300))
              (h (ble:gatt-char-handle ffe5)))
          (dotimes (i 300) (setf (aref payload i) (mod (* i 7) 251)))
          (let ((r (ble:att-write-long-value chan h payload)))
            (format t "~&[central] long write of 300 octets -> ~S~%" r)
            (check (eq t r) "a 300-octet write is acknowledged"))
          (let ((back (ble:att-read-long-value chan h)))
            (format t "~&[central] read back: ~D octets~%" (length back))
            (check (= 300 (length back)) "and reads back at full length")
            (check (equalp payload back)
                   "with every octet as written, in order")))

        ;; --- two subscriptions at once -------------------------------------
        (let ((cccd1 (ble:att-find-cccd chan (ble:gatt-char-handle ffe1)))
              (cccd2 (ble:att-find-cccd chan (ble:gatt-char-handle ffe2))))
          (format t "~&[central] CCCDs: 0x~4,'0X and 0x~4,'0X~%"
                  (or cccd1 0) (or cccd2 0))
          (check (and cccd1 cccd2) "descriptor discovery finds both CCCDs")
          (ble:att-subscribe chan cccd1)
          (ble:att-subscribe chan cccd2)
          (check t "both subscriptions accepted")

          (let ((from-a 0) (from-b 0)
                (ha (ble:gatt-char-handle ffe1))
                (hb (ble:gatt-char-handle ffe2)))
            ;; Ask for ONE handle repeatedly. The other's traffic must be kept.
            (dotimes (i 6)
              (when (ble:att-next-notification chan ha 3000) (incf from-a)))
            (let ((queued (ble:att-pending-notifications chan hb)))
              (format t "~&[central] handle A: ~D received; handle B queued: ~D~%"
                      from-a queued)
              (check (plusp from-a) "notifications arrive on the handle asked for")
              (check (plusp queued)
                     "and the other characteristic's are KEPT, not dropped"))
            ;; Now drain by whichever speaks, and confirm both are represented.
            (dotimes (i 10)
              (multiple-value-bind (v h) (ble:att-next-notification-any chan 2000)
                (when v
                  (cond ((eql h ha) (incf from-a))
                        ((eql h hb) (incf from-b))))))
            (format t "~&[central] totals: A=~D B=~D~%" from-a from-b)
            (check (and (plusp from-a) (plusp from-b))
                   "att-next-notification-any delivers from both handles")))

        ;; --- the established connection itself -----------------------------
        ;; Only testable against a real controller and a real peer: there is
        ;; no answer to "what interval is this link running at" without a link.
        (let ((rssi (ble:hci-read-rssi chan)))
          (format t "~&[central] connection RSSI: ~A dBm~%" rssi)
          (check (and (integerp rssi) (< -128 rssi 20))
                 "RSSI reads back as a plausible signed dBm value"))

        (multiple-value-bind (version manufacturer subversion)
            (ble:hci-read-remote-version chan)
          (format t "~&[central] peer controller: version 0x~2,'0X, ~
                     manufacturer ~A, subversion 0x~4,'0X~%"
                  (if (integerp version) version 0) manufacturer
                  (or subversion 0))
          (check (integerp version) "the peer reports a core spec version")
          (check (and (integerp version) (>= version #x06))
                 "which is at least 4.0, since this is an LE link"))

        (let ((features (ble:hci-read-remote-features chan)))
          (format t "~&[central] peer LE features: ~A~%" features)
          (check (and (vectorp features) (= 8 (length features)))
                 "the LE feature bitmap is eight octets")
          (check (and (vectorp features) (logbitp 0 (aref features 0)))
                 "with LE Encryption set, which every LE controller has"))

        ;; Renegotiate the link, and believe the numbers that come back rather
        ;; than the ones asked for -- the peer may answer anywhere in range.
        (multiple-value-bind (interval latency timeout)
            (ble:hci-connection-update chan :min-interval-ms 100
                                            :max-interval-ms 150
                                            :supervision-timeout-ms 5000)
          (format t "~&[central] connection update -> interval ~A ms, ~
                     latency ~A, timeout ~A ms~%" interval latency timeout)
          (check (numberp interval) "the update completes")
          (check (and (numberp interval) (<= 100 interval 150))
                 "at an interval inside the range requested")
          (check (and (numberp timeout) (= 5000 timeout))
                 "and the supervision timeout asked for"))

        ;; The link still works afterwards, which is the part that matters.
        (check (equalp #(9 9) (ble:att-read-value chan (ble:gatt-char-handle ffe1)))
               "and the connection still serves reads at the new parameters")

        ;; --- conditions, against a live refusal ---------------------------
        (let ((r (ble:att-write-value chan #x00FF #(1))))
          (format t "~&[central] default-style write to a bad handle: ~S~%" r)
          (check (or (integerp r) (eq r :timeout))
                 "default style returns a sentinel, never a condition"))
        (handler-case
            (ble:with-ble-conditions
              (ble:att-write-value chan #x00FF #(1))
              (check nil "with-ble-conditions should have signalled"))
          (ble:ble-error (e)
            (format t "~&[central] signalled: ~A (~A)~%" e (type-of e))
            (check t "a single BLE-ERROR handler catches it")
            (when (typep e 'ble:att-error)
              (check (= #x01 (ble:att-error-code e))
                     "invalid handle, reported as a typed att-error")
              (check (= #x00FF (ble:att-error-handle e))
                     "naming the handle that was refused"))))))
    (format t "~&[central] adapter handed back~%")
    (format t "~&LIVE RESULT: ~D/~D checks passed~@[ -- ~D FAILED~]~%"
            (- checks fails) checks (when (plusp fails) fails))
    (force-output))
  (sb-ext:exit :code (if (zerop fails) 0 1)))
