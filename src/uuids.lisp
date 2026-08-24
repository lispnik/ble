(in-package #:ble)

;;; Assigned numbers.
;;;
;;; None of these are ours: they are published by the Bluetooth SIG, and a
;;; peripheral that wants to be recognised has to use exactly them. That is
;;; precisely why they belong in named constants rather than as literals at the
;;; point of use -- #x2A37 is not a choice anyone made, it is a fact to be
;;; looked up, and a reader who has to look it up is a reader who cannot tell a
;;; typo from a decision.
;;;
;;; Only the ones a peripheral commonly needs are here. This is not an attempt
;;; to transcribe the whole registry, which is thousands of entries and would
;;; rot; it is the handful that appear in the examples and in any device that
;;; wants a phone to draw the right icon.
;;;
;;; ble/core, because they are just numbers -- a consumer parsing a capture
;;; file needs them as much as one driving a radio does.

;;; --- services -----------------------------------------------------------

(defconstant +service-generic-access+     #x1800)
(defconstant +service-generic-attribute+  #x1801)
(defconstant +service-device-information+ #x180A)
(defconstant +service-battery+            #x180F)
(defconstant +service-heart-rate+         #x180D)

;;; Vendor services are deliberately absent. The Nordic UART Service, for one,
;;; is a 128-bit UUID and has no 16-bit form -- writing a short one here would
;;; be inventing a number, not recording one.

;;; --- characteristics ----------------------------------------------------

;; Generic Access
(defconstant +char-device-name+           #x2A00)
(defconstant +char-appearance+            #x2A01)
;; Generic Attribute
(defconstant +char-service-changed+       #x2A05)
;; Device Information
(defconstant +char-manufacturer-name+     #x2A29)
(defconstant +char-model-number+          #x2A24)
(defconstant +char-serial-number+         #x2A25)
(defconstant +char-firmware-revision+     #x2A26)
;; Battery
(defconstant +char-battery-level+         #x2A19)
;; Heart Rate
(defconstant +char-heart-rate-measurement+ #x2A37)
(defconstant +char-body-sensor-location+   #x2A38)
(defconstant +char-heart-rate-control-point+ #x2A39)

;;; Descriptors are not here either: the CCCD already has a name in this
;;; library, +GATT-CCCD+, and two names for one number is the drift that named
;;; constants exist to prevent.

;;; --- appearance ---------------------------------------------------------
;;;
;;; A 16-bit value split into a 10-bit category and a 6-bit subtype. It is what
;;; decides the icon a phone draws beside the device, which is the only part of
;;; a peripheral most people ever see.

(defconstant +appearance-unknown+           #x0000)
(defconstant +appearance-generic-phone+     #x0040)
(defconstant +appearance-generic-tag+       #x0200)
(defconstant +appearance-generic-thermometer+ #x0300)
(defconstant +appearance-heart-rate-sensor+ #x0340)
(defconstant +appearance-heart-rate-belt+   #x0341)
(defconstant +appearance-generic-sensor+    #x0540)

(defun appearance (value)
  "The two octets an Appearance characteristic holds, little-endian.

A helper rather than a literal because the encoding is where this goes wrong:
0x0341 written out by hand is #(#x41 #x03), and the reversed form is a
different, valid appearance that simply draws the wrong icon."
  (let ((v (make-octets 2)))
    (u16le-put v 0 value)
    v))

;;; --- service changed ----------------------------------------------------

(defun service-changed-range (&optional (start #x0001) (end #xFFFF))
  "The four octets of a Service Changed value: the handle range that moved.

Defaults to the whole database, which is what a peripheral means when it
cannot say anything more precise. Note this value is only meaningful inside
the indication that announces a change -- nothing reads it from the attribute
-- so the stored value is a placeholder rather than a statement."
  (let ((v (make-octets 4)))
    (u16le-put v 0 start)
    (u16le-put v 2 end)
    v))
