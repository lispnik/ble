(in-package #:ble)

;;; Advertising side of the HCI controller, in both forms.
;;;
;;; Extended (5.0), which drives an advertising set and is the only way to
;;; transmit on the Coded PHY:
;;;   0x0035  LE Set Advertising Set Random Address
;;;   0x0036  LE Set Extended Advertising Parameters
;;;   0x0037  LE Set Extended Advertising Data
;;;   0x0039  LE Set Extended Advertising Enable
;;;
;;; Legacy (4.0), implemented by every LE controller ever made:
;;;   0x0006  LE Set Advertising Parameters
;;;   0x0008  LE Set Advertising Data
;;;   0x0009  LE Set Scan Response Data
;;;   0x000A  LE Set Advertising Enable
;;;
;;; Both are here for the same reason both scan paths are: extended reaches
;;; further, legacy reaches controllers that never implemented extended. A
;;; library that offered only extended could scan on a radio it could not
;;; advertise from, which was the state of this file until now.

;;; --- HCI command OCFs ------------------------------------------------

(defconstant +ocf-le-set-random-address+          #x0005)
(defconstant +ocf-le-set-adv-set-random-address+   #x0035)
(defconstant +ocf-le-set-extended-adv-parameters+  #x0036)
(defconstant +ocf-le-set-extended-adv-data+        #x0037)
(defconstant +ocf-le-set-extended-adv-enable+      #x0039)

(defconstant +ocf-le-set-adv-parameters+          #x0006)
(defconstant +ocf-le-set-adv-data+                #x0008)
(defconstant +ocf-le-set-scan-response-data+      #x0009)
(defconstant +ocf-le-set-adv-enable+              #x000A)

;;; Legacy advertising types (LE Set Advertising Parameters, adv_type).
(defconstant +adv-ind+          #x00 "Connectable and scannable undirected.")
(defconstant +adv-scan-ind+     #x02 "Scannable undirected; not connectable.")
(defconstant +adv-nonconn-ind+  #x03 "Neither connectable nor scannable.")

;;; --- Helpers ---------------------------------------------------------

;;; u16le-put and u24le-put live in octets.lisp.

;;; --- HCI advertising commands ---------------------------------------

(defun set-random-address (sock mac)
  "LE Set Random Address: the address legacy advertising uses when
OWN-ADDR-TYPE is 1. MAC is in on-air order.

Distinct from SET-ADV-SET-RANDOM-ADDRESS, which sets one advertising set\'s
address under the extended commands. This is the controller-wide one, and it
is what a legacy advertiser needs.

Changing address makes a peripheral a different device to anything that
remembers the old one -- which is occasionally exactly what you want. Clients
cache a GATT database by address and will not re-discover it; iOS honours
Service Changed only from a bonded peer, so an unbonded peripheral that gains
a characteristic is invisible in that respect until its address changes."
  (send-hci-command sock +ogf-le+ +ocf-le-set-random-address+ (coerce-octets mac)))

(defun set-adv-set-random-address (sock handle mac)
  "Bind a 6-byte random BD_ADDR to advertising set HANDLE. The high two
bits of MAC[5] should be 11 (static random) for compatibility.

CALL SET-EXTENDED-ADV-PARAMETERS FOR HANDLE FIRST. That command is what
creates the advertising set, and a controller is required to answer Unknown
Advertising Identifier (0x42) here if the set does not exist yet. Some
controllers create it implicitly and accept this in either order, which is
how the wrong order survives testing on one dongle and fails on the next."
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

;;; --- legacy advertising ------------------------------------------------

(defun %adv-parameters-params (interval-min interval-max adv-type
                               own-addr-type peer-addr-type peer-addr
                               channel-map filter-policy)
  "The 15-octet parameter block for LE Set Advertising Parameters."
  (let ((params (make-octets 15)))
    (u16le-put params 0 interval-min)          ; units of 0.625 ms
    (u16le-put params 2 interval-max)
    (setf (aref params 4) adv-type
          (aref params 5) own-addr-type
          (aref params 6) peer-addr-type)
    (replace params (coerce-octets peer-addr) :start1 7 :end1 13)
    (setf (aref params 13) channel-map
          (aref params 14) filter-policy)
    params))

(defun set-adv-parameters (sock &key (interval-min #x00A0) (interval-max #x00A0)
                                     (adv-type +adv-ind+) (own-addr-type 0)
                                     (peer-addr-type 0) (peer-addr #(0 0 0 0 0 0))
                                     (channel-map #x07) (filter-policy 0))
  "LE Set Advertising Parameters (legacy).

Intervals are in units of 0.625 ms; the default 0x00A0 is 100 ms. CHANNEL-MAP
defaults to all three primary advertising channels (0x07) -- narrowing it is
occasionally useful for testing but mostly just makes you harder to find.

Must be issued while advertising is disabled; the controller rejects it
otherwise, which is the usual reason this returns a non-zero status."
  (send-hci-command sock +ogf-le+ +ocf-le-set-adv-parameters+
                    (%adv-parameters-params interval-min interval-max adv-type
                                            own-addr-type peer-addr-type peer-addr
                                            channel-map filter-policy)))

(defun %adv-data-params (data)
  "The 32-octet block both LE Set Advertising Data and LE Set Scan Response
Data take: a significant-length byte, then 31 octets, zero-padded.

The fixed width is the point -- the command is always 32 octets on the wire
however little of it means anything, and a short buffer is rejected rather
than padded for you."
  (let ((data (coerce-octets data))
        (params (make-octets 32)))
    (when (> (length data) 31)
      (error "advertising data is ~D octets; the legacy limit is 31"
             (length data)))
    (setf (aref params 0) (length data))
    (replace params data :start1 1)
    params))

(defun set-adv-data (sock data)
  "LE Set Advertising Data (legacy). DATA is up to 31 octets of AD records."
  (send-hci-command sock +ogf-le+ +ocf-le-set-adv-data+ (%adv-data-params data)))

(defun set-scan-response-data (sock data)
  "LE Set Scan Response Data (legacy): what an active scanner gets back when
it asks. Up to 31 octets, and a second budget of them -- putting the name
here rather than in the advertisement is how devices fit both."
  (send-hci-command sock +ogf-le+ +ocf-le-set-scan-response-data+
                    (%adv-data-params data)))

(defun set-adv-enable (sock enable)
  "LE Set Advertising Enable (legacy)."
  (let ((params (make-octets 1)))
    (setf (aref params 0) (if enable 1 0))
    (send-hci-command sock +ogf-le+ +ocf-le-set-adv-enable+ params)))
