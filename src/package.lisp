;;; The #:ble package -- generic Bluetooth Low Energy, no vendor protocol.
;;;
;;; This library used to be one package, #:bledecode, holding both the BLE
;;; plumbing and the Swift Sensors protocol built on top of it. They are now
;;; separate:
;;;
;;;   #:ble        HCI sockets, LE scanning, advertising, GATT/NUS, adapters,
;;;                MAC byte order, octet primitives. Nothing here knows what
;;;                a Swift sensor is.
;;;   #:ble.swift  the Swift Sensors protocol: packet parsing, decryption,
;;;                sample decoding, model and error tables, and the live glue
;;;                that drives #:ble to scan for and advertise as Swift
;;;                devices. See src/swift/package.lisp.
;;;
;;; The split is by domain. It cuts across the older split by platform, which
;;; still matters just as much and is preserved: the ble/core system defined
;;; here (this file, src/octets.lisp, src/mac.lisp) has no dependencies at
;;; all, so ble/swift can be portable -- the Swift protocol and its test
;;; suite still load on a machine with no BLE stack, which is the whole
;;; reason `make test` runs on a Mac.
;;;
;;; The symbols below are ble/core's. The I/O layer adds its own exports in
;;; src/ble-package.lisp, which loads with the Linux-only ble system.

(defpackage #:ble
  (:use #:cl)
  (:export
   ;; Octet vectors and little-endian integers (src/octets.lisp)
   #:octet
   #:octets
   #:make-octets
   #:coerce-octets
   #:u16-le
   #:s16-le
   #:u32-le
   #:s32-le
   #:u16le-put
   #:u24le-put
   ;; Advertising-data records (src/ad.lisp)
   #:+ad-type-manufacturer-specific-data+
   #:extract-manufacturer-data
   #:map-ad-records
   #:adv-local-name
   #:adv-service-uuids-16
   ;; MAC addresses (src/mac.lisp). Held on-air, LSB first, everywhere;
   ;; these two are the only place the display order exists.
   #:parse-mac
   #:format-mac
   #:invalid-mac
   #:invalid-mac-string))
