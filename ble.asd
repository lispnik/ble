;;; A Bluetooth Low Energy library for SBCL on Linux.
;;;
;;; Split by platform, which is the seam that matters for a library like this:
;;;
;;;   ble/core        octet vectors, little-endian integers, advertising-data
;;;                   records, MAC byte order. No dependencies whatsoever.
;;;   ble             HCI sockets, adapter enumeration, LE scan, extended
;;;                   advertising, the NUS GATT client, and LE connections
;;;                   over HCI_CHANNEL_USER. Adds cffi; Linux/BlueZ only.
;;;
;;; ble/core being genuinely dependency-free is load-bearing rather than
;;; tidy: it lets a consumer's protocol code -- and its test suite -- stay
;;; portable while still sharing this library's octet handling, MAC byte
;;; order and advertising-data parsing. Its consumers rely on that to run
;;; their test suites on machines with no Bluetooth. See the README for who
;;; those are; the library itself has no business knowing.

(asdf:defsystem #:ble/core
  :description "Portable BLE primitives: octets, advertising-data records, MAC byte order."
  :license     "MIT"
  :version     "0.2.0"
  :depends-on  ()
  :components ((:module "src"
                :components ((:file "package")
                             (:file "conditions" :depends-on ("package"))
                             (:file "octets"     :depends-on ("package"))
                             (:file "ad"         :depends-on ("octets"))
                             (:file "mac"        :depends-on ("conditions"))))))

(asdf:defsystem #:ble
  :description "Generic Bluetooth Low Energy over raw BlueZ sockets (Linux only)."
  :license     "MIT"
  :version     "0.2.0"
  :depends-on  (#:ble/core #:cffi)
  :components ((:module "src"
                :components ((:file "ble-package")
                             (:file "ffi"        :depends-on ("ble-package"))
                             (:file "att-conditions" :depends-on ("ble-package"))
                             (:file "hci"        :depends-on ("ffi"))
                             (:file "advertiser" :depends-on ("hci"))
                             (:file "nus"        :depends-on ("hci" "att-conditions"))
                             (:file "hci-conn"   :depends-on ("nus"))
                             (:file "teardown"   :depends-on ("hci-conn"))))))

(asdf:defsystem #:ble/io-tests
  :description "Tests for the parts of the I/O layer that need no radio."
  :depends-on  (#:ble #:ble/tests)
  :components ((:module "tests"
                :components ((:file "io-tests"))))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call :ble/tests :run-io-tests)
               (error "ble I/O test suite failed"))))

(asdf:defsystem #:ble/tests
  :description "Test suite for the portable parts of ble."
  :depends-on  (#:ble/core #:fiveam)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "core-tests"))))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call :ble/tests :run-tests)
               (error "ble test suite failed"))))
