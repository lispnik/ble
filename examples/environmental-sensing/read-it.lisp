;;;; Connect to the environmental sensor and read every reading it has.
;;;;
;;;; The client half, and the one where looking a characteristic up by UUID
;;;; stops working. This service has three 0x2A6E characteristics;
;;;; FIND-CHAR-BY-UUID returns one of them, with no sign the others exist, so
;;;; this uses FIND-CHARS-BY-UUID and then asks each one who it is.
;;;;
;;;; Asking means reading its Characteristic User Description, which is a
;;;; descriptor rather than part of the characteristic -- so the client has to
;;;; work out which handles belong to which characteristic before it can read
;;;; anything meaningful. That range calculation is the actual work here, and
;;;; it is the part a client that assumes one-of-each never has to do.
(defpackage #:environmental-sensing-client (:use #:common-lisp) (:export #:run))
(in-package #:environmental-sensing-client)

(defun signed (value bits)
  (if (logbitp (1- bits) value) (- value (ash 1 bits)) value))

(defun u16 (v) (+ (aref v 0) (ash (aref v 1) 8)))
(defun u32 (v) (+ (aref v 0) (ash (aref v 1) 8) (ash (aref v 2) 16)
                  (ash (aref v 3) 24)))

(defun decode (uuid16 value)
  "Each characteristic has its own fixed scale; there is no exponent on the
wire to check against, so this table is the only thing keeping the client and
the sensor in agreement."
  (case uuid16
    (#x2A6E (/ (signed (u16 value) 16) 100))    ; hundredths of a degree
    (#x2A6F (/ (u16 value) 100))                ; hundredths of a percent
    (#x2A6D (/ (u32 value) 10))                 ; tenths of a pascal
    (t value)))

(defun uuid16-of (char)
  (let ((u (ble:gatt-char-uuid char)))
    (when (= 2 (length u)) (+ (aref u 0) (ash (aref u 1) 8)))))

(defun descriptor-ranges (chars service-end)
  "For each characteristic, the handle range its descriptors live in.

A characteristic owns everything from just after its value handle up to the
next characteristic's declaration, and the declaration sits one handle before
the value. There is no field anywhere saying where a characteristic ends --
this arithmetic is the only thing that decides which descriptor belongs to
which, and getting it wrong reads the neighbour's name."
  (let ((sorted (sort (copy-list chars) #'< :key #'ble:gatt-char-handle)))
    (loop for (this next) on sorted
          collect (list this
                        (1+ (ble:gatt-char-handle this))
                        (if next
                            (- (ble:gatt-char-handle next) 2)
                            service-end)))))

(defun read-user-description (chan start end)
  "The Characteristic User Description in [START, END], as a string, or NIL."
  (when (<= start end)
    (let ((found (find (ble:uuid16 ble:+descriptor-user-description+)
                       (ble:att-discover-descriptors chan start end)
                       :key #'cdr :test #'equalp)))
      (when found
        (map 'string #'code-char (ble:att-read-value chan (car found)))))))

(defun run (mac &key (dev 2) (addr-type :random))
  (ble:install-adapter-teardown)
  (ble:with-att-channel
      (chan (ble:hci-user-att-connect (ble:parse-mac mac) :addr-type addr-type
                                      :dev dev :init-phys #x01 :timeout 25))
    (format t "~&connected to ~A~%" mac) (force-output)
    (ble:att-exchange-mtu chan 247)
    (let ((ess (ble:att-find-service
                chan (ble:uuid16 ble:+service-environmental-sensing+))))
      (unless ess (error "no environmental sensing service on this device"))
      (let* ((service-end (ble:gatt-service-end ess))
             (chars (ble:att-discover-characteristics
                     chan :start (ble:gatt-service-start ess) :end service-end))
             (temperatures (ble:find-chars-by-uuid
                            chars (ble:uuid16 ble:+char-temperature+))))
        (format t "~&~D characteristic(s), of which ~D are temperature~%"
                (length chars) (length temperatures))
        ;; The point of the example, stated where it can be seen:
        (when (> (length temperatures) 1)
          (format t "~&FIND-CHAR-BY-UUID would have returned handle ~D and ~
                     hidden the other ~D~%"
                  (ble:gatt-char-handle
                   (ble:find-char-by-uuid chars (ble:uuid16 ble:+char-temperature+)))
                  (1- (length temperatures))))
        (force-output)
        (dolist (entry (descriptor-ranges chars service-end))
          (destructuring-bind (char start end) entry
            (let* ((uuid16 (uuid16-of char))
                   (name (read-user-description chan start end))
                   (value (ble:att-read-value chan (ble:gatt-char-handle char))))
              (format t "~&  ~:[(unnamed)~;~:*~A~]~20T0x~4,'0X  ~A~%"
                      name uuid16 (float (decode uuid16 value)))
              (force-output))))))))
