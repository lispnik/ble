;;; The #:ble package -- generic Bluetooth Low Energy.
;;;
;;; Nothing in this library knows what kind of device it is talking to. It
;;; carries HCI sockets, adapter enumeration, LE scanning, extended
;;; advertising, advertising-data parsing, and an ATT/GATT client; what a
;;; given vendor puts inside an advertising payload is a consumer's business.
;;;
;;; Two systems, split by platform:
;;;
;;;   ble/core  this file, src/octets.lisp, src/ad.lisp, src/mac.lisp.
;;;             Octet vectors, little-endian integers, advertising-data
;;;             records, MAC byte order. NO DEPENDENCIES AT ALL, which is
;;;             load-bearing rather than tidy: it lets a consumer's protocol
;;;             code -- and its test suite -- share this library while still
;;;             loading on a machine with no Bluetooth stack.
;;;   ble       the I/O layer. Adds cffi, and Linux/BlueZ. Its exports are
;;;             declared in src/ble-package.lisp so that loading ble/core on
;;;             a machine without a radio does not advertise a scanner that
;;;             cannot run there.
;;;
;;; A hazard worth knowing if you USE this package: a DEFUN of a name #:ble
;;; already exports does not shadow it, it redefines it. A wrapper that then
;;; delegates to that symbol calls itself forever, and it presents as a slow
;;; compile rather than an error. Give a specialised version its own name.
;;;
;;; The symbols below are ble/core's.

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
