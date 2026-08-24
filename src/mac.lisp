(in-package #:ble)

;;; BLE address byte order: on the wire an address is little-endian (LSB
;;; first); the conventional display form is big-endian (MSB first). Getting
;;; this backwards silently targets the wrong device, so parsing and
;;; formatting live together here as one source of truth, with round-trip tests
;;; in the core suite.
;;;
;;; Internally this library always holds addresses in ON-AIR (LSB-first)
;;; order -- that is what HCI and the connection code expect.

(defun format-mac (bytes)
  "Format a 6-byte BLE address in ON-AIR (LSB-first) order as a display-order
MAC string \"AA:BB:CC:DD:EE:FF\" (MSB first). Inverse of PARSE-MAC."
  (format nil "~{~2,'0X~^:~}" (reverse (coerce bytes 'list))))

(define-condition invalid-mac (ble-error)
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

(defun static-random-address (octets)
  "Turn six random octets into a valid static random address.

The two most significant bits must both be 1; the rest is free, and must not
be all-zero or all-one across the remaining 46. On-air order is LSB first, so
the most significant octet is the last one."
  (let ((a (coerce-octets octets)))
    (setf (aref a 5) (logior (aref a 5) #xC0))
    a))

(defun static-random-address-p (mac)
  "Is MAC a valid static random address? On-air order."
  (and (= 6 (length mac))
       (= #xC0 (logand (aref mac 5) #xC0))
       ;; all-zero and all-one are reserved
       (not (every #'zerop (list (logand (aref mac 5) #x3F) (aref mac 4)
                                 (aref mac 3) (aref mac 2) (aref mac 1)
                                 (aref mac 0))))))

;;; --- resolvable private addresses ---------------------------------------
;;;
;;; On air, LSB first, a resolvable address is hash(3) then prand(3), with the
;;; top two bits of the most significant octet set to 01. Everything here
;;; takes and returns on-air order, like every other address in this library,
;;; and converts at the crypto boundary.

(defun resolvable-address-p (mac)
  "Is MAC a resolvable private address? On-air order."
  (and (= 6 (length mac)) (= #x40 (logand (aref mac 5) #xC0))))

(defun address-prand (mac)
  "The random part of a resolvable address, in on-air order."
  (subseq (coerce-octets mac) 3 6))

(defun address-hash (mac)
  "The hash part of a resolvable address, in on-air order."
  (subseq (coerce-octets mac) 0 3))
