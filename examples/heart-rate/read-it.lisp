;;;; Connect to the heart rate sensor and print what it sends.
;;;;
;;;; The client half, as a consumer would write it: discover 0x180D, subscribe
;;;; to the measurement, decode. Also in its own package, for the same reason.
(defpackage #:heart-rate-client (:use #:common-lisp) (:export #:run))
(in-package #:heart-rate-client)

(defun decode (v)
  "Decode a Heart Rate Measurement into a plist."
  (let* ((flags (aref v 0))
         (wide (logbitp 0 flags))
         (bpm (if wide (+ (aref v 1) (ash (aref v 2) 8)) (aref v 1)))
         (i (if wide 3 2))
         (energy (when (logbitp 3 flags)
                   (prog1 (+ (aref v i) (ash (aref v (1+ i)) 8))
                     (incf i 2)))))
    (list :bpm bpm
          :contact (case (ldb (byte 2 1) flags)
                     (0 :unsupported) (1 :unsupported)
                     (2 :not-detected) (3 :detected))
          :energy energy
          :rr (loop while (<= (+ i 2) (length v))
                    collect (prog1 (+ (aref v i) (ash (aref v (1+ i)) 8))
                              (incf i 2))))))

(defun run (mac &key (dev 2) (count 5) (addr-type :random))
  (ble:install-adapter-teardown)
  (ble:with-att-channel
      (chan (ble:hci-user-att-connect (ble:parse-mac mac) :addr-type addr-type
                                      :dev dev :init-phys #x01 :timeout 25))
    (format t "~&connected to ~A~%" mac) (force-output)
    (ble:att-exchange-mtu chan 247)
    (let ((hrs (ble:att-find-service chan (ble:uuid16 #x180D))))
      (unless hrs (error "no heart rate service on this device"))
      (format t "~&heart rate service at ~D-~D~%"
              (ble:gatt-service-start hrs) (ble:gatt-service-end hrs))
      (let* ((chars (ble:att-discover-characteristics
                     chan :start (ble:gatt-service-start hrs)
                          :end (ble:gatt-service-end hrs)))
             (m (ble:find-char-by-uuid chars (ble:uuid16 #x2A37)))
             (loc (ble:find-char-by-uuid chars (ble:uuid16 #x2A38))))
        (unless m (error "no measurement characteristic"))
        (when loc
          (format t "~&body sensor location: ~A~%"
                  (case (aref (ble:att-read-value chan (ble:gatt-char-handle loc)) 0)
                    (0 "other") (1 "chest") (2 "wrist") (3 "finger")
                    (t "?"))))
        (let ((cccd (ble:att-find-cccd chan (ble:gatt-char-handle m))))
          (ble:att-subscribe chan cccd)
          (format t "~&subscribed; waiting for ~D measurements~%" count)
          (force-output)
          (dotimes (i count)
            (let ((v (ble:att-next-notification chan (ble:gatt-char-handle m) 6000)))
              (if v
                  (format t "~&  ~A~%" (decode v))
                  (format t "~&  (timed out)~%"))
              (force-output))))))))
