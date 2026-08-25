;;;; Connect to the health thermometer and print what it sends.
;;;;
;;;; The client half, as a consumer would write it: discover 0x1809, subscribe
;;;; to both temperature characteristics, decode. Also in its own package, for
;;;; the same reason.
;;;;
;;;; Subscribing to an indication is the same write as subscribing to a
;;;; notification with a different bit, and the confirmations are sent for us
;;;; by the ATT layer -- which is why this file looks almost identical to the
;;;; heart rate client despite the peripheral working quite differently.
(defpackage #:health-thermometer-client (:use #:common-lisp) (:export #:run))
(in-package #:health-thermometer-client)

(defconstant +medfloat-nan+ #x007FFFFF)

(defun signed (value bits)
  (if (logbitp (1- bits) value) (- value (ash 1 bits)) value))

(defun decode-medfloat (v i)
  "Decode the 32-bit IEEE-11073 FLOAT at offset I: mantissa x 10^exponent."
  (let ((mantissa (logior (aref v i) (ash (aref v (+ i 1)) 8)
                          (ash (aref v (+ i 2)) 16)))
        (exponent (signed (aref v (+ i 3)) 8)))
    (if (= mantissa +medfloat-nan+)
        :nan
        (* (signed mantissa 24) (expt 10 exponent)))))

(defun decode (v)
  "Decode a Temperature Measurement or Intermediate Temperature into a plist."
  (let* ((flags (aref v 0))
         (temp (decode-medfloat v 1))
         (i 5)
         (stamp (when (logbitp 1 flags)
                  (prog1 (list :year (+ (aref v i) (ash (aref v (1+ i)) 8))
                               :month (aref v (+ i 2)) :day (aref v (+ i 3))
                               :time (list (aref v (+ i 4)) (aref v (+ i 5))
                                           (aref v (+ i 6))))
                    (incf i 7)))))
    (list :temperature (if (eq temp :nan) :nan (float temp))
          :units (if (logbitp 0 flags) :fahrenheit :celsius)
          :timestamp stamp
          :type (when (logbitp 2 flags)
                  (case (aref v i)
                    (1 :armpit) (2 :body) (3 :ear) (4 :finger)
                    (5 :gastrointestinal) (6 :mouth) (7 :rectum)
                    (8 :toe) (9 :tympanum) (t :unknown))))))

(defun run (mac &key (dev 2) (count 5) (addr-type :random))
  (ble:install-adapter-teardown)
  (ble:with-att-channel
      (chan (ble:hci-user-att-connect (ble:parse-mac mac) :addr-type addr-type
                                      :dev dev :init-phys #x01 :timeout 25))
    (format t "~&connected to ~A~%" mac) (force-output)
    (ble:att-exchange-mtu chan 247)
    (let ((hts (ble:att-find-service
                chan (ble:uuid16 ble:+service-health-thermometer+))))
      (unless hts (error "no health thermometer service on this device"))
      (format t "~&health thermometer service at ~D-~D~%"
              (ble:gatt-service-start hts) (ble:gatt-service-end hts))
      (let* ((chars (ble:att-discover-characteristics
                     chan :start (ble:gatt-service-start hts)
                          :end (ble:gatt-service-end hts)))
             (m (ble:find-char-by-uuid
                 chars (ble:uuid16 ble:+char-temperature-measurement+)))
             (interval (ble:find-char-by-uuid
                        chars (ble:uuid16 ble:+char-measurement-interval+))))
        (unless m (error "no temperature measurement characteristic"))
        (when interval
          (let ((v (ble:att-read-value chan (ble:gatt-char-handle interval))))
            (format t "~&measurement interval: ~D s~%"
                    (+ (aref v 0) (ash (aref v 1) 8)))))
        ;; Subscribing for indications rather than notifications: the same
        ;; CCCD, bit 1 instead of bit 0. The library sends the confirmation
        ;; for each one, which is what the peripheral is waiting on before it
        ;; will send another.
        (let ((cccd (ble:att-find-cccd chan (ble:gatt-char-handle m))))
          (ble:att-subscribe chan cccd :indications t)
          (format t "~&subscribed; waiting for ~D measurements~%" count)
          (force-output)
          (dotimes (i count)
            (let ((v (ble:att-next-notification
                      chan (ble:gatt-char-handle m) 30000)))
              (if v
                  (format t "~&  ~A~%" (decode v))
                  (format t "~&  (timed out)~%"))
              (force-output))))))))
