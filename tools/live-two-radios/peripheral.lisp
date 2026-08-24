;;;; Dongle A: a real GATT server, on real hardware.
;;;;
;;;; Everything below the ATT layer is this library too -- HCI_CHANNEL_USER for
;;;; the controller, legacy connectable advertising, and the GATT server from
;;;; src/gatt-server.lisp answering discovery, reads, writes and subscriptions.
;;;; The point of running it on a second radio is that central.lisp then talks
;;;; to it over the air rather than through a loopback in one image: the ACL
;;;; length here is 27 octets, so a 300-octet characteristic exercises L2CAP
;;;; fragmentation and reassembly in both directions as a side effect.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))
(in-package #:ble)

;;; Configuration comes from the environment so the runner can pick adapters
;;; by bus rather than by an index that drifts across reboots. run.sh sets
;;; these; the fallbacks only make the file usable by hand.
(defun env-int (name default)
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v))) (parse-integer v :junk-allowed t) default)))

(defparameter +dev+ (env-int "PERIPH_DEV" 1))
(defparameter +serve-seconds+ (env-int "PERIPH_SECONDS" 120))
(defparameter *counter* 0)

(defun build-server ()
  "Two services, and one of every kind of attribute a client might probe."
  (let ((server (make-gatt-server :mtu 23))
        (long (make-octets 300)))
    (dotimes (i 300) (setf (aref long i) (mod i 251)))
    (gatt-add-service server #x180A)
    (gatt-add-characteristic server :uuid #x2A29 :properties '(:read)
                                    :value "ACME")
    (gatt-add-characteristic server :uuid #x2A24 :properties '(:read)
                                    :on-read (lambda (s a)
                                               (declare (ignore s a))
                                               (format nil "r~D" (incf *counter*))))
    (gatt-add-service server #xFFE0)
    (multiple-value-bind (ffe1 cccd1)
        (gatt-add-characteristic server :uuid #xFFE1
                                 :properties '(:read :write :notify)
                                 :value #(1 2 3))
      (multiple-value-bind (ffe2 cccd2)
          (gatt-add-characteristic server :uuid #xFFE2
                                   :properties '(:read :notify)
                                   :value #(0))
        (let ((ffe3 (gatt-add-characteristic
                     server :uuid #xFFE3 :properties '(:write)
                            :on-write (lambda (s a v)
                                        (declare (ignore s a))
                                        (unless (= 1 (length v)) #x0D))))
              (ffe4 (gatt-add-characteristic server :uuid #xFFE4
                                             :properties '(:read)
                                             :value long))
              ;; Writable and long, so the client's Prepare/Execute path has
              ;; something over the air to write to. Nothing else here has a
              ;; writable attribute bigger than one Write Request.
              (ffe5 (gatt-add-characteristic server :uuid #xFFE5
                                             :properties '(:read :write)
                                             :value (make-octets 0))))
          (format t "~&[peripheral] ~D attributes; FFE1 ~D/cccd ~D, FFE2 ~D/cccd ~D, ~
                     FFE3 ~D, FFE4 ~D, FFE5 ~D~%"
                  (gatt-attribute-count server) ffe1 cccd1 ffe2 cccd2 ffe3 ffe4 ffe5)
          (values server ffe1 cccd1 ffe2 cccd2))))))

(defun await-connection (sock timeout-ms)
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

(multiple-value-bind (server ffe1 cccd1 ffe2 cccd2) (build-server)
  ;; with-hci-user-socket and with-advertising are the point of gap 3: the
  ;; adapter goes back to the kernel and advertising stops however this ends.
  (with-hci-user-socket (sock +dev+)
    (format t "~&[peripheral] hci~D owned~%" +dev+) (force-output)
    (set-adv-parameters sock :adv-type +adv-ind+ :own-addr-type 0)
    (set-adv-data sock (concatenate '(vector (unsigned-byte 8))
                                    (vector 2 1 6 8 9)
                                    (map 'vector #'char-code "TWOHAND")))
    (with-advertising (sock)
      (format t "~&[peripheral] advertising as TWOHAND~%") (force-output)
      (let ((handle (await-connection sock (* 1000 (env-int "PERIPH_WAIT" 90)))))
        (if (null handle)
            (format t "~&[peripheral] no central attached~%")
            (let ((conn (make-hci-conn :sock sock :handle handle
                                       :acl-len (hci-socket-acl-len sock)))
                  (served 0) (sent 0))
              (format t "~&[peripheral] connected, handle 0x~4,'0X~%" handle)
              (force-output)
              ;; Serve until the client leaves or the deadline passes, and
              ;; notify both characteristics once it subscribes -- two
              ;; notifying characteristics on one link is what the client's
              ;; per-handle dispatch has to cope with.
              ;;
              ;; A wall-clock deadline rather than a fixed iteration count:
              ;; how many iterations a run needs depends on how many requests
              ;; the client makes, and counting them meant that adding one
              ;; check to the client could starve the end of its own run.
              (let ((deadline (+ (get-internal-real-time)
                                 (* +serve-seconds+
                                    internal-time-units-per-second)))
                    (i 0))
                (loop
                  (when (> (get-internal-real-time) deadline)
                    (format t "~&[peripheral] deadline reached~%")
                    (return))
                  (let ((op (gatt-serve server conn :timeout-ms 100)))
                    (cond ((eq op :disconnected)
                           (format t "~&[peripheral] client went away~%")
                           (return))
                          (op (incf served))))
                  (when (zerop (mod (incf i) 4))
                    (when (gatt-notify server conn ffe1
                                       (vector #xA0 (mod i 256)) :cccd-handle cccd1)
                      (incf sent))
                    (when (gatt-notify server conn ffe2
                                       (vector #xB0 (mod i 256)) :cccd-handle cccd2)
                      (incf sent)))))
              (format t "~&[peripheral] answered ~D requests, sent ~D notifications~%"
                      served sent)
              (force-output)))))))
(format t "~&[peripheral] adapter handed back~%")
(sb-ext:exit)
