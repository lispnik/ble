;;;; Dongle A: a real GATT server, on real hardware.
;;;;
;;;; Everything below the ATT layer is this library too -- HCI_CHANNEL_USER for
;;;; the controller, connectable advertising, and the GATT server answering
;;;; discovery, reads, writes and subscriptions. The point of running it on a
;;;; second radio is that central.lisp then talks to it over the air rather
;;;; than through a loopback in one image: the ACL length here is 27 octets, so
;;;; a 300-octet characteristic exercises L2CAP fragmentation and reassembly in
;;;; both directions as a side effect.
;;;;
;;;; In its own package, on the public API. It used to live inside #:ble and
;;;; hand-roll its own accept loop; if it needs a `ble::' symbol again, the API
;;;; has a hole.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))

(defpackage #:two-radios-peripheral (:use #:common-lisp) (:export #:run))
(in-package #:two-radios-peripheral)

(defun env-int (name d)
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v))) (parse-integer v :junk-allowed t) d)))

(defstruct sensor
  server ffe1 cccd1 ffe2 cccd2
  (served 0) (sent 0) (ticks 0) (echoed 0)
  param-ident param-result coc)

(defun build-server ()
  "Two services, and one of every kind of attribute a client might probe."
  (let ((server (ble:make-gatt-server :mtu 23))
        (long (ble:make-octets 300)))
    (dotimes (i 300) (setf (aref long i) (mod i 251)))
    (ble:gatt-add-service server ble:+service-device-information+)
    (ble:gatt-add-characteristic server :uuid ble:+char-manufacturer-name+
                                        :properties '(:read) :value "ACME")
    (ble:gatt-add-characteristic server :uuid ble:+char-model-number+
                                        :properties '(:read)
                                        :on-read (let ((n 0))
                                                   (lambda (s a)
                                                     (declare (ignore s a))
                                                     (format nil "r~D" (incf n)))))
    (ble:gatt-add-service server #xFFE0)
    (multiple-value-bind (ffe1 cccd1)
        (ble:gatt-add-characteristic server :uuid #xFFE1
                                     :properties '(:read :write :notify)
                                     :value #(1 2 3))
      (multiple-value-bind (ffe2 cccd2)
          (ble:gatt-add-characteristic server :uuid #xFFE2
                                       :properties '(:read :notify)
                                       :value #(0))
        (let ((ffe3 (ble:gatt-add-characteristic
                     server :uuid #xFFE3 :properties '(:write)
                            :on-write (lambda (s a v)
                                        (declare (ignore s a))
                                        (unless (= 1 (length v)) #x0D))))
              (ffe4 (ble:gatt-add-characteristic server :uuid #xFFE4
                                                 :properties '(:read)
                                                 :value long))
              ;; Writable and long, so the client's Prepare/Execute path has
              ;; something over the air to write to. Nothing else here has a
              ;; writable attribute bigger than one Write Request.
              (ffe5 (ble:gatt-add-characteristic server :uuid #xFFE5
                                                 :properties '(:read :write)
                                                 :value (ble:make-octets 0))))
          (format t "~&[peripheral] ~D attributes; FFE1 ~D/cccd ~D, FFE2 ~D/cccd ~D, ~
                     FFE3 ~D, FFE4 ~D, FFE5 ~D~%"
                  (ble:gatt-attribute-count server) ffe1 cccd1 ffe2 cccd2
                  ffe3 ffe4 ffe5)
          (make-sensor :server server :ffe1 ffe1 :cccd1 cccd1
                       :ffe2 ffe2 :cccd2 cccd2))))))

(defun on-connect (s conn peer ptype)
  (declare (ignore peer ptype))
  (format t "~&[peripheral] connected, handle 0x~4,'0X~%" (ble:hci-conn-handle conn))
  (force-output)
  ;; Listen for a connection-oriented channel on a free SPSM: 0x0080 and up
  ;; are unassigned, for two devices to agree between themselves.
  ;;
  ;; Two credits, deliberately. A 400-octet SDU at MPS 96 needs five frames,
  ;; so the sender runs out partway through and must wait to be topped up.
  ;; Granting enough up front would leave the flow control -- the whole point
  ;; of this channel type -- never actually exercised.
  (ble:l2cap-coc-listen conn #x0080 :credits 2)
  (setf (sensor-coc s) nil (sensor-ticks s) 0))

(defun on-tick (s conn request)
  (when request (incf (sensor-served s)))
  (let ((i (incf (sensor-ticks s))))
    ;; Once the link is warm, ask the central for a slower interval. A
    ;; peripheral cannot change it itself -- only the central issues LE
    ;; Connection Update -- so this goes over the L2CAP signalling channel.
    ;;
    ;; Send and carry on serving: the answer is picked up by this same loop.
    ;; Blocking here to wait for it would eat the central's requests, which is
    ;; exactly what an earlier version did.
    (when (= i 40)
      (setf (sensor-param-ident s)
            (ble:l2cap-request-conn-params conn :min-interval-ms 200
                                                :max-interval-ms 300
                                                :supervision-timeout-ms 6000))
      (format t "~&[peripheral] asked for a 200-300 ms interval~%") (force-output))
    (when (and (sensor-param-ident s) (null (sensor-param-result s)))
      (let ((r (ble:l2cap-conn-param-result conn (sensor-param-ident s))))
        (when r
          (setf (sensor-param-result s) r)
          (format t "~&[peripheral] parameter update -> ~A~%" r) (force-output))))
    ;; Echo anything on a CoC. The channel is created from the receive path
    ;; when the peer opens it, so this only has to notice it appearing.
    (unless (sensor-coc s)
      (setf (sensor-coc s) (ble:l2cap-coc-accept conn :timeout-ms 0))
      (when (ble:l2cap-coc-p (sensor-coc s))
        (format t "~&[peripheral] CoC opened: peer MTU ~D, MPS ~D~%"
                (ble:l2cap-coc-peer-mtu (sensor-coc s))
                (ble:l2cap-coc-peer-mps (sensor-coc s)))
        (force-output)))
    (when (ble:l2cap-coc-p (sensor-coc s))
      (let ((sdu (ble:l2cap-coc-recv (sensor-coc s) :timeout-ms 0)))
        (when (vectorp sdu)
          (incf (sensor-echoed s))
          (ble:l2cap-coc-send (sensor-coc s) sdu :timeout-ms 3000))))
    ;; Two notifying characteristics on one link is what the client's
    ;; per-handle dispatch has to cope with.
    (when (zerop (mod i 4))
      (when (ble:gatt-notify (sensor-server s) conn (sensor-ffe1 s)
                             (vector #xA0 (mod i 256))
                             :cccd-handle (sensor-cccd1 s))
        (incf (sensor-sent s)))
      (when (ble:gatt-notify (sensor-server s) conn (sensor-ffe2 s)
                             (vector #xB0 (mod i 256))
                             :cccd-handle (sensor-cccd2 s))
        (incf (sensor-sent s))))))

(defun report (s)
  (format t "~&[peripheral] answered ~D requests, sent ~D notifications, ~
             parameter update ~A, CoC echoed ~D~%"
          (sensor-served s) (sensor-sent s)
          (or (sensor-param-result s) :none) (sensor-echoed s))
  (force-output))

(defun run (&key (dev (env-int "PERIPH_DEV" 1))
                 (seconds (env-int "PERIPH_SECONDS" 120)))
  (ble:install-adapter-teardown)
  (let ((s (build-server)))
    (ble:with-hci-user-socket (sock dev)
      (format t "~&[peripheral] hci~D owned~%" dev) (force-output)
      (ble:set-adv-parameters sock :adv-type ble:+adv-ind+ :own-addr-type 0)
      (ble:set-adv-data sock (ble:adv-data
                              :flags '(:general-discoverable :no-bredr)
                              :name "TWOHAND"))
      (format t "~&[peripheral] advertising as TWOHAND~%") (force-output)
      (ble:serve-peripheral
       (sensor-server s) sock :seconds seconds :tick-ms 100
       ;; EATT=1 turns on Enhanced ATT bearers. :INSECURE because this
       ;; harness does not pair -- the encryption gate is real and is covered
       ;; by the unit tests; what the radios are here to prove is that the
       ;; bearers open and carry ATT.
       :eatt (when (plusp (env-int "EATT" 0)) :insecure)
       :accept-timeout-ms (* 1000 (env-int "PERIPH_WAIT" 90))
       :on-connect (lambda (conn peer ptype) (on-connect s conn peer ptype))
       :on-tick (lambda (conn request) (on-tick s conn request))
       :on-disconnect (lambda (conn)
                        (declare (ignore conn))
                        (format t "~&[peripheral] client went away~%")
                        (report s)))
      (report s))))

(run)
(format t "~&[peripheral] adapter handed back~%")
(sb-ext:exit)
