;;;; Find out what is advertising nearby.
;;;;
;;;; The other examples are all peripherals, and their clients each take a MAC
;;;; address you have to have got from somewhere else. This is that somewhere
;;;; else, and it is the first thing anybody actually does with a BLE stack.
;;;;
;;;; It is also the only example that scans, which means it is the only one
;;;; that touches the Coded PHY -- START-EXTENDED-SCAN listens on 1M and Coded
;;;; together, and a device advertising long-range is invisible to anything
;;;; that does not. That is not a detail: a whole fleet can be sitting in the
;;;; room, advertising steadily, and a legacy scan will report an empty world.
;;;;
;;;; Two things worth knowing before reading the output. A report is not a
;;;; device: one device produces many, and the name usually arrives in a scan
;;;; response while the service UUIDs arrive in the advertisement, so anything
;;;; that treats reports as devices sees each device repeatedly, differently,
;;;; and never completely. DISCOVER merges them by address, which is what makes
;;;; its result a list of devices. And an address is not an identity -- most
;;;; phones and many sensors rotate a random address every fifteen minutes, so
;;;; the same hardware appears as a stranger later on.
;;;;
;;;; Its own package, using only exported symbols, for the same reason the
;;;; others are.

(defpackage #:scanner
  (:use #:common-lisp)
  (:export #:survey #:find-service #:find-named #:watch #:service-name
           #:event-type-names))

(in-package #:scanner)

;;; --- naming what we find ------------------------------------------------

(defparameter *known-services*
  ;; Deliberately the ones this repository can produce, plus the handful any
  ;; room is full of. A scanner that prints raw 16-bit numbers makes the
  ;; reader look them up; one that pretends to know every assigned number
  ;; would be a copy of a registry, going stale from the day it was written.
  (list (cons ble:+service-generic-access+      "Generic Access")
        (cons ble:+service-generic-attribute+   "Generic Attribute")
        (cons ble:+service-device-information+  "Device Information")
        (cons ble:+service-battery+             "Battery")
        (cons ble:+service-heart-rate+          "Heart Rate")
        (cons ble:+service-health-thermometer+  "Health Thermometer")
        (cons ble:+service-glucose+             "Glucose")
        (cons ble:+service-environmental-sensing+ "Environmental Sensing"))
  "16-bit service UUIDs this scanner can put a name to.")

(defun service-name (uuid16)
  "A readable name for a 16-bit service UUID, or its hex if we have none."
  (or (cdr (assoc uuid16 *known-services*))
      (format nil "0x~4,'0X" uuid16)))

;;; --- the survey ---------------------------------------------------------

(defun survey (&key (dev nil) (seconds 8) named-only)
  "Scan for SECONDS and print what is out there, strongest signal first.

NAMED-ONLY drops devices that never gave a name, which in a populated room is
most of them -- an advertisement is not obliged to carry one, and a device
that only responds to an active scan will not have been asked.

Returns the DISCOVERED list, so this is usable from a REPL as well as read
as output."
  (let* ((dev (or dev (ble:default-hci-dev)))
         (found (ble:discover :dev dev :seconds seconds :extended t)))
    (when named-only
      (setf found (remove-if-not (lambda (d)
                                   (let ((n (ble:discovered-name d)))
                                     (and n (plusp (length n)))))
                                 found)))
    (format t "~&~D device(s) on hci~D in ~D second(s)~%~%" (length found) dev seconds)
    (format t "~&~17A ~6A ~4A  ~A~%" "ADDRESS" "TYPE" "RSSI" "NAME / SERVICES")
    (dolist (d found)
      (format t "~&~17A ~6A ~4@A  ~A~%"
              (ble:format-mac (ble:discovered-address d))
              (case (ble:discovered-addr-type d)
                (0 "public") (1 "random") (t "?"))
              (or (ble:discovered-rssi d) "?")
              (or (ble:discovered-name d) "")))
    (dolist (d found)
      (let ((services (ble:discovered-service-uuids d)))
        (when services
          (format t "~&~17A        ~{~A~^, ~}~%"
                  (ble:format-mac (ble:discovered-address d))
                  (mapcar #'service-name services)))))
    (force-output)
    found))

;;; --- finding one thing --------------------------------------------------

(defun find-service (uuid16 &key (dev nil) (seconds 8))
  "Every device advertising the 16-bit service UUID16, strongest first.

This is what makes the other examples usable without going outside for an
address. A service UUID in the advertisement is the only way to recognise a
device before connecting to it -- everything else about a peripheral lives
behind a connection.

Note what it can and cannot see: a peripheral that has the service in its
GATT database but not in its advertising payload will not be found, because
nothing has looked in its database yet. That is a mistake the examples in
this repository deliberately do not make."
  (ble:discover :dev (or dev (ble:default-hci-dev)) :seconds seconds
                :filter (lambda (d) (member uuid16 (ble:discovered-service-uuids d)))))

(defun find-named (substring &key (dev nil) (seconds 8))
  "Every device whose advertised name contains SUBSTRING, case-sensitively."
  (ble:discover :dev (or dev (ble:default-hci-dev)) :seconds seconds
                :filter (lambda (d)
                          (let ((n (ble:discovered-name d)))
                            (and n (search substring n))))))

;;; --- watching the raw reports -------------------------------------------

(defun event-type-names (bits)
  "Decode an extended advertising report's event type.

Extended reports carry a 16-bit bitmap; legacy reports carry a small enum in
one octet, and the two are not interchangeable. Bit 4 says which kind of
advertisement produced this report, and bit 3 says the report is a scan
response -- the second half of a device whose first half you already saw."
  (remove nil
          (list (when (logbitp 0 bits) "connectable")
                (when (logbitp 1 bits) "scannable")
                (when (logbitp 2 bits) "directed")
                (when (logbitp 3 bits) "scan-response")
                (if (logbitp 4 bits) "legacy-adv" "extended-adv"))))

(defun watch (&key (dev nil) (seconds 15) address)
  "Print every advertising report as it arrives, unmerged.

The counterpart to SURVEY, and worth running once to see why merging exists:
the same device appears again and again, its name in one report and its
services in another. ADDRESS, a display-order MAC string, narrows it to one
device, which is the readable way to watch a single peripheral advertise."
  (let ((want (when address (ble:parse-mac address)))
        (count 0))
    (ble:scan-reports
     (lambda (r)
       (when (or (null want) (equalp want (ble:adv-report-address r)))
         (incf count)
         (format t "~&~17A ~4@A dBm  ~{~A~^ ~}~@[  name=~S~]~@[  services=~{~A~^,~}~]~%"
                 (ble:format-mac (ble:adv-report-address r))
                 (or (ble:adv-report-rssi r) "?")
                 (event-type-names (ble:adv-report-event-type r))
                 (let ((n (ble:adv-local-name (ble:adv-report-data r))))
                   (when (and n (plusp (length n))) n))
                 (mapcar #'service-name
                         (ble:adv-service-uuids-16 (ble:adv-report-data r))))
         (force-output)))
     :dev (or dev (ble:default-hci-dev)) :seconds seconds :extended t)
    (format t "~&~D report(s)~%" count)
    (force-output)
    count))
