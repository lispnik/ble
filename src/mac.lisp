(in-package #:ble)

;;; BLE address byte order: on the wire an address is little-endian (LSB
;;; first); the conventional display form is big-endian (MSB first). Getting
;;; this backwards silently targets the wrong device, so parsing and
;;; formatting live together here as one source of truth (used by the scanner,
;;; the advertiser, and the CLI), with round-trip tests in the core suite.
;;;
;;; Internally bledecode always holds addresses in ON-AIR (LSB-first) order --
;;; that is what the crypto keystream and the HCI connection code expect.

(defun format-mac (bytes)
  "Format a 6-byte BLE address in ON-AIR (LSB-first) order as a display-order
MAC string \"AA:BB:CC:DD:EE:FF\" (MSB first). Inverse of PARSE-MAC."
  (format nil "~{~2,'0X~^:~}" (reverse (coerce bytes 'list))))

(define-condition invalid-mac (error)
  ((string :initarg :string :reader invalid-mac-string))
  (:report (lambda (c s)
             (format s "invalid MAC ~S: expected six hex octets separated by ':' or '-'"
                     (invalid-mac-string c))))
  (:documentation
   "PARSE-MAC was handed something that is not an address. A condition of its
own rather than a SIMPLE-ERROR because callers do act on it -- a CLI wants to
tell the user their --mac argument is malformed, distinctly from the device
being unreachable."))

(defun parse-mac (string)
  "Parse a display-order MAC \"AA:BB:CC:DD:EE:FF\" (MSB first) into a 6-byte
octet vector in ON-AIR (LSB-first) byte order. Inverse of FORMAT-MAC.

Accepts ':' or '-' as the separator, and either case. Signals INVALID-MAC
when the result is not six octets -- silently returning something shorter
would target a nonexistent device rather than complaining, which is the
failure mode this whole file exists to prevent."
  (let ((parts (remove "" (uiop:split-string string :separator ":-") :test #'string=)))
    (unless (= (length parts) 6)
      (error 'invalid-mac :string string))
    (handler-case
        (nreverse (map '(simple-array (unsigned-byte 8) (*))
                       (lambda (p) (parse-integer p :radix 16)) parts))
      (parse-error () (error 'invalid-mac :string string))
      (type-error  () (error 'invalid-mac :string string)))))
