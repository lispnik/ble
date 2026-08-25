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
(defconstant +service-health-thermometer+ #x1809)
(defconstant +service-glucose+            #x1808)
(defconstant +service-environmental-sensing+ #x181A)
(defconstant +service-object-transfer+    #x1825)

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
;; Object Transfer. The metadata of whichever object is currently selected;
;; the object's own bytes never cross an attribute, they go over an L2CAP
;; connection-oriented channel.
(defconstant +char-ots-feature+                    #x2ABD)
(defconstant +char-object-name+                    #x2ABE)
(defconstant +char-object-type+                    #x2ABF)
(defconstant +char-object-size+                    #x2AC0)
(defconstant +char-object-id+                      #x2AC3)
(defconstant +char-object-properties+              #x2AC4)
(defconstant +char-object-action-control-point+    #x2AC5)
(defconstant +char-object-list-control-point+      #x2AC6)
;; Environmental Sensing. Any of these may appear more than once in one
;; service -- three Temperatures for indoor, outdoor and a probe is the
;; ordinary case -- so a client cannot look them up by UUID and stop.
(defconstant +char-temperature+ #x2A6E)
(defconstant +char-humidity+    #x2A6F)
(defconstant +char-pressure+    #x2A6D)
;; Glucose
(defconstant +char-glucose-measurement+         #x2A18)
(defconstant +char-glucose-measurement-context+ #x2A34)
(defconstant +char-glucose-feature+             #x2A51)
;; Not a glucose characteristic as such -- the Record Access Control Point is
;; the SIG's generic stored-records protocol, and Continuous Glucose
;; Monitoring and Insulin Delivery use the same number for the same thing.
(defconstant +char-record-access-control-point+ #x2A52)
;; Health Thermometer
(defconstant +char-temperature-measurement+  #x2A1C)
(defconstant +char-temperature-type+         #x2A1D)
(defconstant +char-intermediate-temperature+ #x2A1E)
(defconstant +char-measurement-interval+     #x2A21)
;; Heart Rate
(defconstant +char-heart-rate-measurement+ #x2A37)
(defconstant +char-body-sensor-location+   #x2A38)
(defconstant +char-heart-rate-control-point+ #x2A39)

;;; --- descriptors --------------------------------------------------------
;;;
;;; The CCCD is deliberately absent: it already has a name in this library,
;;; +GATT-CCCD+, and two names for one number is the drift that named
;;; constants exist to prevent.

(defconstant +descriptor-user-description+ #x2901)
(defconstant +descriptor-presentation-format+ #x2904)
(defconstant +descriptor-valid-range+ #x2906)
(defconstant +descriptor-es-measurement+ #x290C)

;;; --- appearance ---------------------------------------------------------
;;;
;;; A 16-bit value split into a 10-bit category and a 6-bit subtype. It is what
;;; decides the icon a phone draws beside the device, which is the only part of
;;; a peripheral most people ever see.

(defconstant +appearance-unknown+           #x0000)
(defconstant +appearance-generic-phone+     #x0040)
(defconstant +appearance-generic-tag+       #x0200)
(defconstant +appearance-generic-thermometer+ #x0300)
;; Category 16 of 64 values each: 16 * 64 = 0x0400. The thermometer
;; above is category 12 and the heart rate sensor 13, which is the
;; arithmetic to check this against.
(defconstant +appearance-generic-glucose-meter+ #x0400)
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

;;; --- date time ----------------------------------------------------------

(defun date-time (&optional (universal-time (get-universal-time)))
  "The seven octets of a Date Time (0x2A08): year, month, day, h, m, s.

Little-endian year, every other field one octet. A zero year, month or day
means `unknown', which is what a device without a real clock should send
rather than a plausible-looking lie.

Here rather than in the profile that needs it because several do -- Health
Thermometer stamps a measurement with it, Glucose stamps a stored record --
and it is the SIG's structure in all of them."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (let ((v (make-octets 7)))
      (u16le-put v 0 year)
      (setf (aref v 2) month
            (aref v 3) day
            (aref v 4) hour
            (aref v 5) min
            (aref v 6) sec)
      v)))

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
