;;;; Provision a device: write a configuration too long for one packet.
;;;;
;;;; The client half, and the only one here that writes more than fits in a
;;;; single ATT payload. Three procedures none of the other clients use:
;;;;
;;;;   ATT-WRITE-LONG-VALUE   Prepare Write per fragment, then Execute Write
;;;;   ATT-READ-LONG-VALUE    Read, then Read Blob at rising offsets
;;;;   ATT-READ-MULTIPLE      several short values in one round trip
;;;;
;;;; And the fourth thing, which is not a procedure but the point of them: a
;;;; queue can be abandoned. This demonstrates it deliberately -- queue some
;;;; fragments, cancel, and show the device unchanged -- because a long write
;;;; that cannot be cancelled is just several writes with extra steps.
(defpackage #:provisioning-client (:use #:common-lisp) (:export #:run #:abandon))
(in-package #:provisioning-client)

(defun state-name (n)
  (case n (0 :unconfigured) (1 :configured) (2 :applying) (3 :connected)
        (4 :failed) (t :unknown)))

(defun handles (chan)
  "Discover the provisioning service and return its handles as a plist."
  (let ((svc (ble:att-find-service chan provisioning:+service-uuid+)))
    (unless svc (error "no provisioning service on this device"))
    (let ((chars (ble:att-discover-characteristics
                  chan :start (ble:gatt-service-start svc)
                       :end (ble:gatt-service-end svc))))
      (flet ((h (uuid) (let ((c (ble:find-char-by-uuid chars uuid)))
                         (and c (ble:gatt-char-handle c)))))
        (list :config (h provisioning:+config-uuid+)
              :state  (h provisioning:+state-uuid+)
              :error  (h provisioning:+error-uuid+)
              :rssi   (h provisioning:+rssi-uuid+)
              :apply  (h provisioning:+apply-uuid+))))))

(defun read-status (chan h)
  "State, error and RSSI in one round trip instead of three.

Read Multiple hands back the values concatenated with nothing between them,
so this only works because all three are known to be one octet. A client that
tried it on the configuration characteristic would get a blob it could not
take apart -- which is the limitation Bluetooth 5.2 addressed by adding a
variable-length version of this request."
  (let ((v (ble:att-read-multiple chan (list (getf h :state) (getf h :error)
                                             (getf h :rssi)))))
    (when (>= (length v) 3)
      (list :state (state-name (aref v 0))
            :error (aref v 1)
            :rssi (let ((r (aref v 2))) (if (> r 127) (- r 256) r))))))

(defun abandon (chan h &key (ssid "half-written-network"))
  "Queue fragments of a configuration and then cancel instead of committing.

The device must be untouched afterwards. This is the property the whole
prepare/execute mechanism exists for, and the only way to see it is to do
it: a device that applied fragments as they arrived would now be holding
half a configuration, and would look perfectly healthy while doing so."
  (let* ((before (ble:att-read-long-value chan (getf h :config)))
         (blob (provisioning:encode-config :ssid ssid :psk "12345678"
                                           :url "https://example.invalid/")))
    (format t "~&queueing ~D octet(s), then cancelling~%" (length blob))
    ;; By hand rather than through ATT-WRITE-LONG-VALUE, because that helper
    ;; commits -- and not committing is the entire point here.
    (let ((chunk (max 1 (- (ble:att-mtu chan) 5)))
          (offset 0))
      (loop while (< offset (length blob))
            do (let ((end (min (length blob) (+ offset chunk))))
                 (ble:att-prepare-write chan (getf h :config) offset
                                        (subseq blob offset end))
                 (setf offset end))))
    (ble:att-execute-write chan :cancel t)
    (let ((after (ble:att-read-long-value chan (getf h :config))))
      (format t "~&config before: ~D octet(s), after cancelling: ~D octet(s)~%"
              (length before) (length after))
      (if (equalp before after)
          (format t "~&the abandoned write changed nothing, which is the point~%")
          (format t "~&*** the device kept part of an abandoned write ***~%"))
      (force-output)
      (equalp before after))))

(defun tighten-link (chan ms why)
  "Ask for a shorter connection interval before moving a lot of data.

Credits come back once per connection event, so the event rate is the packet
rate: a link left at whatever the peer's controller chose -- often 45 ms or
more -- moves data about six times slower than one at 7.5 ms. The library has
always had this call and nothing used it, so every example here shipped the
default until it was measured.

Not something to do reflexively. A short interval costs power at both ends,
and a battery sensor sending one notification a second wants the opposite --
which is why the sensor examples in this repository deliberately do not call
this."
  (multiple-value-bind (interval) (ble:hci-connection-update
                                   chan :min-interval-ms ms
                                        :max-interval-ms (* 2 ms))
    (if (numberp interval)
        (format t "~&connection interval now ~,2F ms (~A)~%" interval why)
        (format t "~&connection interval unchanged (~A)~%" interval))
    (force-output)
    interval))

(defun run (mac &key (dev 2) (addr-type :random)
                     (ssid "Kitchen Wi-Fi 2.4GHz")
                     (psk "correct horse battery staple")
                     (url "https://collector.example.invalid/v1/ingest"))
  "Provision the device at MAC, having first proved a cancelled write is safe."
  (ble:install-adapter-teardown)
  (ble:with-att-channel
      (chan (ble:hci-user-att-connect (ble:parse-mac mac) :addr-type addr-type
                                      :dev dev :init-phys #x01 :timeout 25))
    (format t "~&connected to ~A~%" mac) (force-output)
    ;; A bigger MTU means fewer fragments, not fewer procedures: the value is
    ;; still longer than one payload, so the queue is still how it travels.
    (ble:att-exchange-mtu chan 247)
    (let ((h (handles chan)))
      (format t "~&status: ~A~%" (read-status chan h))
      (force-output)
      ;; A queued write is many fragments, each costing a connection event.
      (tighten-link chan 15 "about to write a configuration")
      ;; First, the abandoned write.
      (abandon chan h)
      ;; Then the real one.
      (let ((blob (provisioning:encode-config :ssid ssid :psk psk :url url)))
        (format t "~&writing ~D octet(s) of configuration (MTU ~D, so ~
                   it does not fit in one)~%"
                (length blob) (ble:att-mtu chan))
        (force-output)
        (ble:att-write-long-value chan (getf h :config) blob))
      ;; Read it back the long way, and check the device kept what we sent.
      (let* ((back (ble:att-read-long-value chan (getf h :config)))
             (decoded (provisioning:decode-config back)))
        (format t "~&read back ~D octet(s): ssid ~S, url ~S~%"
                (length back) (getf decoded :ssid) (getf decoded :url))
        (format t "~&round trip is exact: ~A~%"
                (equalp back (provisioning:encode-config
                              :ssid ssid :psk psk :url url)))
        (force-output))
      ;; Apply it, and watch the state move.
      (ble:att-write-value chan (getf h :apply) (vector 1))
      (format t "~&applying...~%") (force-output)
      (dotimes (i 8)
        (let ((status (read-status chan h)))
          (format t "~&  ~A~%" status)
          (force-output)
          (when (eq :connected (getf status :state)) (return))
          (sleep 1))))))
