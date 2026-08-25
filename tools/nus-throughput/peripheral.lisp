;;;; Throughput harness, peripheral half: a NUS sink and source.
;;;;
;;;; Runs on one USB dongle while tools/nus-throughput/central.lisp runs on
;;;; the other. Between them they measure what this stack actually moves over
;;;; the Nordic UART Service, which is a different question from whether it
;;;; works and one no test answers.
;;;;
;;;; Two directions, because they are not symmetrical:
;;;;
;;;;   uplink    central writes to RX with Write Commands, we count octets
;;;;   downlink  we notify on TX as fast as the controller will take it
;;;;
;;;; The peripheral is the source for the downlink because that is the
;;;; direction real devices push data, and because it is the one with a
;;;; hazard: this library does no ACL credit accounting -- it defines
;;;; +HCI-NUM-COMPLETED-EVT+ and never reads it -- so nothing stops a sender
;;;; from handing the controller more than its buffer holds. The write then
;;;; fails at the syscall. BLAST below treats that as backpressure rather than
;;;; an error, which is the honest way to measure a stack that has no flow
;;;; control: find the rate at which the controller starts saying no.
;;;;
;;;; Its own package on the public API, like everything else here.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))

(defpackage #:nus-throughput-peripheral (:use #:common-lisp) (:export #:run))
(in-package #:nus-throughput-peripheral)

(defun env (name default)
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v))) (parse-integer v :junk-allowed t) default)))

(defparameter *name* "NUS Bench")

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

(defstruct bench
  "Counters for one run, and the handles to move data on."
  server rx-handle tx-handle tx-cccd
  (received 0)          ; octets written to RX by the central
  (sent 0)              ; octets notified on TX
  (notifications 0)
  (blocked 0)           ; writes the controller refused: backpressure
  (mode nil)            ; NIL, or :blast until BLAST-UNTIL
  (blast-until 0))

(defun build (mtu)
  (let ((server (ble:make-gatt-server :mtu mtu))
        (b nil))
    (ble:gatt-add-service server ble:+service-generic-access+)
    (ble:gatt-add-characteristic server :uuid ble:+char-device-name+
                                        :properties '(:read) :value *name*)
    (ble:gatt-add-service server ble:+nus-service-uuid-le+)
    (let ((rx (ble:gatt-add-characteristic
               server :uuid ble:+nus-rx-uuid-le+
                      :properties '(:write :write-without-response)
                      :on-write
                      (lambda (s a v)
                        (declare (ignore s a))
                        (incf (bench-received b) (length v))
                        ;; A one-octet write is the control channel: how many
                        ;; seconds to blast for. Anything longer is payload
                        ;; and is only counted.
                        (when (= 1 (length v))
                          (let ((seconds (aref v 0)))
                            (setf (bench-mode b) :blast
                                  (bench-sent b) 0
                                  (bench-notifications b) 0
                                  (bench-blocked b) 0
                                  (bench-blast-until b)
                                  (+ (get-internal-real-time)
                                     (* seconds internal-time-units-per-second)))))
                        nil))))
      (multiple-value-bind (tx tx-cccd)
          (ble:gatt-add-characteristic server :uuid ble:+nus-tx-uuid-le+
                                              :properties '(:notify))
        (setf b (make-bench :server server :rx-handle rx
                            :tx-handle tx :tx-cccd tx-cccd))
        b))))

(defun blast (b conn payload)
  "Push notifications until the controller pushes back.

Returns when a write fails, which is this stack's only backpressure signal.
CHECK-SYSCALL turns the failed write into a condition; catching it here is
not ignoring an error, it is reading the one piece of flow-control
information available."
  (loop repeat 64                       ; a bounded burst, so ticks still run
        do (handler-case
               (progn
                 (unless (ble:gatt-notify (bench-server b) conn
                                          (bench-tx-handle b) payload)
                   (return))            ; not subscribed
                 (incf (bench-sent b) (length payload))
                 (incf (bench-notifications b)))
             (error ()
               (incf (bench-blocked b))
               (return)))))

(defun run (&key (dev (pick-adapter 0 (env "PERIPH_DEV" nil)))
                 (mtu (env "MTU" 247)) (minutes (env "MINUTES" 3)))
  (let* ((b (build mtu))
         (payload (make-array (max 1 (- mtu 3))
                              :element-type '(unsigned-byte 8)
                              :initial-element #x5A)))
    (ble:install-adapter-teardown)
    (ble:with-hci-user-socket (sock dev)
      (let ((addr (ble:static-random-address (ble:smp-random-octets sock 6))))
        (ble:set-random-address sock addr)
        (ble:set-adv-parameters sock :adv-type ble:+adv-ind+ :own-addr-type 1)
        (ble:set-adv-data sock (ble:adv-data
                                :flags '(:general-discoverable :no-bredr)
                                :name *name*))
        (format t "~&~A on hci~D as ~A, rx-mtu ~D~%"
                *name* dev (ble:format-mac addr) mtu)
        (force-output)
        (ble:serve-peripheral
         (bench-server b) sock
         :seconds (* 60 minutes)
         ;; A short tick, because the tick is where notifications are sent and
         ;; a 50 ms one would cap the downlink at 20 bursts a second.
         :tick-ms 1
         :on-connect (lambda (conn peer ptype)
                       (declare (ignore conn))
                       (setf (bench-received b) 0 (bench-mode b) nil)
                       (format t "~&connected: ~A (~(~A~)), negotiated mtu ~D~%"
                               (ble:format-mac peer) ptype
                               (ble:gatt-server-mtu (bench-server b)))
                       (force-output))
         :on-disconnect (lambda (conn)
                          (declare (ignore conn))
                          (setf (bench-mode b) nil)
                          (format t "~&disconnected; uplink total ~D octet(s)~%"
                                  (bench-received b))
                          (force-output))
         :on-tick
         (lambda (conn request)
           (declare (ignore request))
           (when (eq (bench-mode b) :blast)
             (if (> (get-internal-real-time) (bench-blast-until b))
                 (progn
                   (setf (bench-mode b) nil)
                   (format t "~&downlink sent ~D octet(s) in ~D notification(s), ~
                              ~D backpressure stall(s)~%"
                           (bench-sent b) (bench-notifications b)
                           (bench-blocked b))
                   (force-output))
                 (blast b conn payload)))))))))

(run)
(sb-ext:exit)
