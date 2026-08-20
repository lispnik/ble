(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(defpackage #:ble/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests #:run-io-tests #:ble-core #:ble-io))

(in-package #:ble/tests)

(def-suite ble-core
  :description "The portable half of ble: octets, advertising-data records,
and MAC byte order. Nothing here opens a socket, which is the point -- these
run on any machine, including one with no Bluetooth at all.")

(defun run-tests ()
  (let ((results (run 'ble-core)))
    (explain! results)
    (results-status results)))

(defun octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8)
                             :initial-contents bytes))

(defun hex->octets (&rest strings)
  "Concatenate STRINGS and read them as hex. Variadic so a test can spell an
advertising payload out record by record, which is how you read one."
  (let* ((clean (remove-if-not (lambda (c) (digit-char-p c 16))
                               (apply #'concatenate 'string strings)))
         (out (make-array (floor (length clean) 2) :element-type '(unsigned-byte 8))))
    (dotimes (i (length out) out)
      (setf (aref out i)
            (parse-integer clean :start (* i 2) :end (+ (* i 2) 2) :radix 16)))))
