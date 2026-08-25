;;;; Throughput harness, central half: time both directions over NUS.
;;;;
;;;; Run tools/nus-throughput/peripheral.lisp on the other dongle first.
;;;; Finds it by name, so no address needs copying between the two.
;;;;
;;;; What is being measured is this stack over this hardware, not Bluetooth.
;;;; The numbers are a property of the library's framing, the MTU that was
;;;; negotiated, the connection interval the peripheral's controller chose,
;;;; and how much the sender can hand the controller before it refuses. All of
;;;; those are worth knowing and none of them are the theoretical PHY rate.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))

(defpackage #:nus-throughput-central (:use #:common-lisp) (:export #:run))
(in-package #:nus-throughput-central)

(defun env (name default)
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v))) (parse-integer v :junk-allowed t) default)))


;;; --- choosing a radio ---------------------------------------------------
;;;
;;; By BD_ADDR, not by index, and not by position in the index order either.
;;;
;;; hciN is not stable on this platform. Every HCI_CHANNEL_USER takeover
;;; releases the device and the kernel re-registers it at the next free index,
;;; so the pair of dongles that were hci1 and hci2 are hci3 and hci4 two runs
;;; later -- and the built-in UART radio moves around them. That is not a
;;; reboot-time drift to be looked up once; it is caused by exactly the
;;; workload this harness creates, between one half starting and the other.
;;;
;;; Ordering by index therefore reshuffles mid-experiment, and the two halves
;;; disagree about whose radio is whose. Observed: the central picked "the
;;; second USB adapter", got the one the peripheral was already holding, and
;;; failed three connection attempts to itself.
;;;
;;; A BD_ADDR is burned into the dongle. Both halves sort by it and reach the
;;; same answer independently, whatever the kernel has renumbered underneath.

(defun address< (a b)
  "Compare BD_ADDRs in display order. Addresses are held on-air, LSB first,
so the comparison runs from the last octet down."
  (cond ((null a) nil)
        ((null b) t)
        (t (loop for i from (1- (length a)) downto 0
                 do (cond ((< (aref a i) (aref b i)) (return t))
                          ((> (aref a i) (aref b i)) (return nil)))
                 finally (return nil)))))

(defun usb-adapters ()
  "The USB adapters, in a stable order: by BD_ADDR."
  (sort (remove-if-not #'ble:hci-adapter-usb-p (ble:list-hci-adapters))
        #'address< :key #'ble:hci-adapter-address))

(defun pick-adapter (nth &optional override)
  "The NTH USB adapter by address, or OVERRIDE when one was given."
  (if override
      override
      (let ((usb (usb-adapters)))
        (when (<= (length usb) nth)
          (error "need at least ~D USB adapter(s); found ~D" (1+ nth) (length usb)))
        (let ((a (nth nth usb)))
          ;; The index is printed because it is what the kernel wants, and the
          ;; address because it is what identifies the thing.
          (format t "~&using hci~D = ~A (~A)~%"
                  (ble:hci-adapter-index a)
                  (if (ble:hci-adapter-address a)
                      (ble:format-mac (ble:hci-adapter-address a))
                      "address unknown")
                  (or (ble:hci-adapter-product a) "USB"))
          (force-output)
          (ble:hci-adapter-index a)))))

(defun kbit/s (octets seconds)
  (if (plusp seconds) (/ (* 8 octets) seconds 1000.0) 0))

(defun elapsed (start)
  (/ (float (- (get-internal-real-time) start))
     internal-time-units-per-second))

(defun measure-uplink (nus seconds)
  "Central -> peripheral: Write Commands, unacknowledged, as fast as we can.

Unacknowledged is the point. A Write Request costs a round trip per payload,
so its rate is a function of the connection interval rather than of anything
this library does; a Write Command is limited by what the controller will
accept, which is what NUS-SEND uses and what a serial link over BLE actually
looks like."
  (let* ((chunk (max 1 (- (ble:nus-mtu nus) 3)))
         (payload (make-array chunk :element-type '(unsigned-byte 8)
                                    :initial-element #xA5))
         (start (get-internal-real-time))
         (deadline (+ start (* seconds internal-time-units-per-second)))
         (sent 0) (stalls 0))
    (loop while (< (get-internal-real-time) deadline)
          do (handler-case
                 (progn (ble:nus-send nus payload) (incf sent chunk))
               ;; The controller refusing the write is this stack's only
               ;; backpressure: there is no ACL credit accounting. Pump once
               ;; to let it drain rather than spinning on a full buffer.
               (error () (incf stalls) (ble:hci-pump (ble:nus-fd nus) 2))))
    (values sent (elapsed start) stalls chunk)))

(defun measure-downlink (nus seconds)
  "Peripheral -> central: notifications, counted as they arrive.

One octet written to RX is the harness's signal to start; the peripheral
blasts for that many seconds. Everything after it is payload."
  (ble:nus-send nus (vector seconds))
  (let* ((start (get-internal-real-time))
         ;; A little longer than the burst, so the tail is not clipped.
         (deadline (+ start (* (+ seconds 1) internal-time-units-per-second)))
         (received 0) (packets 0) (first-at nil) (last-at nil))
    (loop while (< (get-internal-real-time) deadline)
          do (let ((v (ble:nus-recv nus 200)))
               (cond ((or (null v) (eq v :disconnected)))
                     (t (unless first-at (setf first-at (get-internal-real-time)))
                        (setf last-at (get-internal-real-time))
                        (incf received (length v))
                        (incf packets)))))
    ;; Timed from the first notification to the last, not from the request:
    ;; the peripheral's start is not instantaneous and including the gap
    ;; would understate the rate by however long it took to get going.
    (values received
            (if (and first-at last-at (> last-at first-at))
                (/ (float (- last-at first-at)) internal-time-units-per-second)
                (elapsed start))
            packets)))

(defun tighten-interval (nus min-ms max-ms)
  "Ask for a shorter connection interval, and report what we actually got.

This is the one lever that costs nothing to pull: the library has had
HCI-CONNECTION-UPDATE all along and no example or benchmark called it, so
every measurement here was taken at whatever the peripheral's controller
chose -- around 45 ms, inferred from the credit-batch rate.

Credits come back once per connection event, so the event rate is the packet
rate. Everything else about throughput on this stack is downstream of it.

The returned interval is the one in force, which need not be the one asked
for: the peer answers with anything inside the range it likes."
  (multiple-value-bind (interval latency timeout)
      (ble:hci-connection-update (ble:nus-fd nus)
                                 :min-interval-ms min-ms :max-interval-ms max-ms)
    (declare (ignorable latency timeout))
    (if (numberp interval)
        (progn (format t "~&connection interval now ~,2F ms~%" interval) interval)
        (progn (format t "~&connection interval unchanged (~A)~%" interval) nil))))

(defun use-2m (nus)
  "Move the link to the 2M PHY, and report what it actually landed on.

Halves the air time of every packet. On a link whose throughput is bounded by
how many packets fit in a connection event, that is worth close to double --
and it composes with the interval rather than competing with it.

A peer that cannot do 2M answers with the PHY it is already using rather than
an error, so the returned value is the one to believe."
  (multiple-value-bind (tx rx) (ble:hci-set-phy (ble:nus-fd nus) :tx :2m :rx :2m)
    (if (keywordp tx)
        (format t "~&PHY now tx ~(~A~) rx ~(~A~)~%" tx rx)
        (format t "~&PHY unchanged (~A)~%" tx))
    tx))

(defun run (&key (dev (pick-adapter 1 (env "CENTRAL_DEV" nil)))
                 (mtu (env "MTU" 247)) (seconds (env "SECONDS" 5))
                 (interval-ms (env "INTERVAL_MS" nil))
                 (phy-2m (env "PHY_2M" nil)))
  (ble:install-adapter-teardown)
  (let ((found (ble:discover :dev dev :seconds 8
                             :filter (lambda (d)
                                       (let ((n (ble:discovered-name d)))
                                         (and n (search "NUS Bench" n)))))))
    (unless found
      (format t "~&no benchmark peripheral advertising~%")
      (return-from run nil))
    (let ((mac (ble:discovered-address (first found))))
      (format t "~&found it at ~A~%" (ble:format-mac mac))
      (force-output)
      (ble:with-nus-hci (nus mac :dev dev :addr-type :random
                                 :init-phys #x01 :mtu mtu :timeout 25)
        (unless nus (error "could not open NUS"))
        (format t "~&connected, negotiated MTU ~D (~D octet payload per packet)~%"
                (ble:nus-mtu nus) (- (ble:nus-mtu nus) 3))
        (force-output)
        ;; INTERVAL_MS=0 measures the default, for comparison.
        (when (and interval-ms (plusp interval-ms))
          (tighten-interval nus interval-ms (* 2 interval-ms)))
        (when (and phy-2m (plusp phy-2m)) (use-2m nus))
        (format t "~%")
        (force-output)

        (multiple-value-bind (sent secs stalls chunk) (measure-uplink nus seconds)
          (format t "~&UPLINK   central -> peripheral, Write Command~%")
          (format t "~&  ~D octet(s) in ~,2F s = ~,1F kbit/s (~,0F packet/s of ~D)~%"
                  sent secs (kbit/s sent secs) (/ (/ sent chunk) secs) chunk)
          (format t "~&  ~D backpressure stall(s)~%~%" stalls)
          (force-output))

        (multiple-value-bind (got secs packets) (measure-downlink nus seconds)
          (format t "~&DOWNLINK peripheral -> central, notifications~%")
          (format t "~&  ~D octet(s) in ~,2F s = ~,1F kbit/s (~,0F packet/s)~%~%"
                  got secs (kbit/s got secs) (if (plusp secs) (/ packets secs) 0))
          (force-output))))))

(run)
(sb-ext:exit)
