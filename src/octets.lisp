(in-package #:ble)

;;; Octet vectors and little-endian integer readers.
;;;
;;; These sat in the Swift crypto file, which was fine while there was one
;;; package but is not once the library is split: the HCI layer needs them
;;; too, and a generic BLE system that reached into a vendor protocol file
;;; for MAKE-OCTETS would have the dependency exactly backwards.
;;;
;;; Portable. This file and src/mac.lisp are the whole of ble/core, which is
;;; what lets the Swift protocol code -- and its test suite -- load with no
;;; foreign dependencies at all.

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(simple-array octet (*)))

(defun make-octets (length &key (initial-element 0))
  (make-array length :element-type 'octet :initial-element initial-element))

(defun coerce-octets (sequence)
  "Copy SEQUENCE into a fresh (simple-array (unsigned-byte 8) (*))."
  (let* ((len (length sequence))
         (out (make-octets len)))
    (replace out sequence)
    out))

;;; Little-endian integer readers -----------------------------------------

(declaim (inline u16-le s16-le u32-le s32-le))

(defun u16-le (vec offset)
  (logior (aref vec offset)
          (ash (aref vec (+ offset 1)) 8)))

(defun s16-le (vec offset)
  (let ((u (u16-le vec offset)))
    (if (logbitp 15 u) (- u #x10000) u)))

(defun u32-le (vec offset)
  (logior (aref vec offset)
          (ash (aref vec (+ offset 1)) 8)
          (ash (aref vec (+ offset 2)) 16)
          (ash (aref vec (+ offset 3)) 24)))

(defun s32-le (vec offset)
  (let ((u (u32-le vec offset)))
    (if (logbitp 31 u) (- u #x100000000) u)))

;;; Encryption -------------------------------------------------------------
;;;
;;; The Swift Sensors firmware (packetizer.c) leaves the first three bytes
;;; of the advertising payload (size, version, sequence) as plaintext and
;;; XORs every following byte with a keystream. The keystream is built one

;;; Little-endian writers ---------------------------------------------------

(defun u16le-put (vec offset value)
  "Store VALUE as a 16-bit little-endian integer at OFFSET."
  (setf (aref vec offset)      (logand value #xFF)
        (aref vec (1+ offset)) (logand (ash value -8) #xFF))
  vec)

(defun u24le-put (vec offset value)
  "Store VALUE as a 24-bit little-endian integer at OFFSET."
  (setf (aref vec offset)       (logand value #xFF)
        (aref vec (+ offset 1)) (logand (ash value -8) #xFF)
        (aref vec (+ offset 2)) (logand (ash value -16) #xFF))
  vec)
