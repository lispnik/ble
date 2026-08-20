(in-package #:ble)

;;; Advertising side of the HCI controller.
;;;
;;; Four LE Controller commands (OGF=0x08) drive an advertising set:
;;;   0x0035  LE Set Advertising Set Random Address
;;;   0x0036  LE Set Extended Advertising Parameters
;;;   0x0037  LE Set Extended Advertising Data
;;;   0x0039  LE Set Extended Advertising Enable

;;; --- HCI command OCFs ------------------------------------------------

(defconstant +ocf-le-set-adv-set-random-address+   #x0035)
(defconstant +ocf-le-set-extended-adv-parameters+  #x0036)
(defconstant +ocf-le-set-extended-adv-data+        #x0037)
(defconstant +ocf-le-set-extended-adv-enable+      #x0039)

;;; --- Helpers ---------------------------------------------------------

;;; u16le-put and u24le-put live in octets.lisp.

;;; --- HCI advertising commands ---------------------------------------

(defun set-adv-set-random-address (sock handle mac)
  "Bind a 6-byte random BD_ADDR to advertising set HANDLE. The high two
bits of MAC[5] should be 11 (static random) for compatibility."
  (assert (= (length mac) 6) (mac) "MAC must be 6 bytes, got ~D" (length mac))
  (let ((params (make-octets 7)))
    (setf (aref params 0) (logand handle #xFF))
    (replace params mac :start1 1)
    (send-hci-command sock +ogf-le+ +ocf-le-set-adv-set-random-address+ params)))

(defun set-extended-adv-parameters
    (sock handle &key (interval-ms 5000) (phy :1m) (tx-power 8))
  "Configure HANDLE for non-connectable, non-scannable, non-directed
advertising at INTERVAL-MS and the requested PHY (:1m or :coded).
TX-POWER is signed dBm in the range -127..+20; controllers may clamp.
Pass NIL to mean 'host has no preference' (controller picks; often the
lowest power)."
  (let* ((interval-units (max 32 (min #xFFFFFF (round (* interval-ms 1.6)))))
         (phy-id (ecase phy (:1m 1) (:coded 3)))
         (txp    (cond ((null tx-power) #x7F)
                       ((minusp tx-power) (logand (+ tx-power 256) #xFF))
                       (t (logand tx-power #xFF))))
         (params (make-octets 25)))
    (setf (aref params 0) (logand handle #xFF))
    ;; bytes 1-2: event_props = 0  (none of connectable/scannable/directed/legacy)
    (u24le-put params 3 interval-units)        ; primary interval min
    (u24le-put params 6 interval-units)        ; primary interval max
    (setf (aref params 9)  #x07                ; channel map: ch37+ch38+ch39
          (aref params 10) 1                   ; own addr type: random
          (aref params 11) 0                   ; peer addr type: public
          ;; bytes 12-17: peer address = zeros (non-directed)
          (aref params 18) 0                   ; filter policy: any
          (aref params 19) txp                 ; TX power
          (aref params 20) phy-id              ; primary PHY
          (aref params 21) 0                   ; secondary max skip
          (aref params 22) phy-id              ; secondary PHY
          (aref params 23) 0                   ; SID
          (aref params 24) 0)                  ; scan request notification disabled
    (send-hci-command sock +ogf-le+ +ocf-le-set-extended-adv-parameters+ params)))

(defun set-extended-adv-data (sock handle data)
  "Push DATA as the complete advertising-data blob for HANDLE."
  (assert (<= (length data) 251))
  (let ((params (make-octets (+ 4 (length data)))))
    (setf (aref params 0) (logand handle #xFF)
          (aref params 1) 3                    ; operation: complete
          (aref params 2) 1                    ; fragment preference: should not fragment
          (aref params 3) (length data))
    (replace params data :start1 4)
    (send-hci-command sock +ogf-le+ +ocf-le-set-extended-adv-data+ params)))

(defun set-extended-adv-enable (sock handle enable)
  "Enable or disable advertising for HANDLE (continuous, no time cap)."
  (let ((params (make-octets 6)))
    (setf (aref params 0) (if enable 1 0)
          (aref params 1) 1                    ; one set
          (aref params 2) (logand handle #xFF))
    ;; bytes 3-4: duration = 0 (continuous), byte 5: max events = 0
    (send-hci-command sock +ogf-le+ +ocf-le-set-extended-adv-enable+ params)))
