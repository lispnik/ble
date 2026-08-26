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
                             (:file "uuids"      :depends-on ("octets"))
                             (:file "mac"        :depends-on ("conditions"))))))

(asdf:defsystem #:ble
  :description "Generic Bluetooth Low Energy over raw BlueZ sockets (Linux only)."
  :license     "MIT"
  :version     "0.2.0"
  ;; ironclad is for the Security Manager: AES-CMAC, and P-256 in software
  ;; because the controllers here answer LE Generate DHKey with garbage.
  ;; ble/core stays dependency-free, which is what keeps consumers' protocol
  ;; tests runnable on a machine with no Bluetooth.
  :depends-on  (#:ble/core #:cffi #:ironclad #:sb-posix)
  :components ((:module "src"
                :components ((:file "ble-package")
                             (:file "ffi"        :depends-on ("ble-package"))
                             (:file "att-conditions" :depends-on ("ble-package"))
                             (:file "hci"        :depends-on ("ffi"))
                             (:file "advertiser" :depends-on ("hci"))
                             (:file "att"        :depends-on ("hci" "att-conditions"))
                             (:file "gatt-server" :depends-on ("att"))
                             (:file "hci-conn"   :depends-on ("att"))
                             (:file "conn-params" :depends-on ("hci-conn"))
                             (:file "l2cap-signalling" :depends-on ("conn-params"))
                             (:file "l2cap-coc" :depends-on ("l2cap-signalling"))
                             (:file "smp-crypto" :depends-on ("ble-package"))
                             (:file "smp" :depends-on ("smp-crypto" "hci-conn"))
                             (:file "bonds" :depends-on ("smp"))
                             ;; smp and bonds because SERVE-PERIPHERAL can
                             ;; drive pairing; without them here ASDF is free
                             ;; to compile this first and the SMP calls become
                             ;; forward references.
                             (:file "peripheral" :depends-on ("gatt-server"
                                                              "hci-conn"
                                                              "advertiser"
                                                              "smp" "bonds"))
                             (:file "nus"        :depends-on ("att" "hci-conn"))
                             (:file "teardown"   :depends-on ("hci-conn"))
                             ;; last: wraps acquire/release pairs from all of
                             ;; the above
                             (:file "with"       :depends-on ("hci-conn" "advertiser" "nus"))))))

(asdf:defsystem #:ble/examples
  :description "Worked examples: peripherals and clients built on ble."
  :license     "MIT"
  :version     "0.2.0"
  :depends-on  (#:ble)
  ;; Each example lives in its own package and uses only exported symbols.
  ;; That is the point of them being a system rather than loose scripts: if an
  ;; example needs a `ble::' symbol, the API has a hole, and loading this is
  ;; what makes that fail loudly instead of quietly working because the file
  ;; happened to be read inside the right package.
  :components ((:module "examples"
                :components ((:module "scanner"
                              :components ((:file "scanner")))
                             (:module "heart-rate"
                              :components ((:file "heart-rate")
                                           (:file "read-it")))
                             (:module "health-thermometer"
                              :components ((:file "health-thermometer")
                                           (:file "read-it")))
                             (:module "glucose"
                              :components ((:file "glucose")
                                           (:file "read-it")))
                             (:module "environmental-sensing"
                              :components ((:file "environmental-sensing")
                                           (:file "read-it")))
                             (:module "object-transfer"
                              :components ((:file "object-transfer")
                                           (:file "read-it")))
                             (:module "nordic-uart"
                              :components ((:file "nordic-uart")
                                           (:file "read-it")))
                             (:module "broadcaster"
                              :components ((:file "broadcaster")))
                             (:module "provisioning"
                              :components ((:file "provisioning")
                                           (:file "read-it")))
                             (:module "lisp-repl"
                              :components ((:file "lisp-repl")
                                           (:file "read-it")))
                             (:module "nus-pty"
                              :components ((:file "nus-pty")))))))

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
