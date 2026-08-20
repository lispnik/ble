(in-package #:ble/tests)

;;; Tests for the `ble' system rather than ble/core -- the parts of the I/O
;;; layer that are pure enough to exercise without a radio.
;;;
;;; A separate system (ble/io-tests) from the portable suite, because loading
;;; `ble' pulls in cffi. It loads anywhere -- the foreign bindings are plain
;;; libc and only fail when CALLED -- but ble/tests stays free of it so the
;;; core suite keeps running on a machine with nothing installed.

(def-suite ble-io
  :description "The parts of the I/O layer that need no radio.")

(defun run-io-tests ()
  (let ((results (run 'ble-io)))
    (explain! results)
    (results-status results)))

(in-suite ble-io)

;;; --- the open-channel registry ----------------------------------------
;;;
;;; This is what stands between an interrupted process and an adapter left
;;; captured machine-wide, so it is worth more than a glance.

(test registry-tracks-channels-and-is-idempotent
  (let ((ble:*open-att-channels* nil))
    (ble:register-att-channel 7)
    (ble:register-att-channel 8)
    (is (equal '(8 7) ble:*open-att-channels*))
    (ble:register-att-channel 7)
    (is (equal '(8 7) ble:*open-att-channels*)
        "registering twice must not double-enter -- close-all would then \
close a stale fd number, which by then may belong to something else")
    (ble:unregister-att-channel 7)
    (is (equal '(8) ble:*open-att-channels*))
    (ble:unregister-att-channel 7)
    (is (equal '(8) ble:*open-att-channels*) "unregistering twice is harmless")))

(test close-all-cannot-signal-and-always-empties
  "It runs from an exit hook and a signal handler, so it must not signal even
when a channel is nonsense, and must leave nothing behind either way."
  (let ((ble:*open-att-channels* (list :not-a-channel "neither is this")))
    (finishes (ble:close-all-att-channels))
    (is (null ble:*open-att-channels*)))
  (let ((ble:*open-att-channels* nil))
    (is (= 0 (ble:close-all-att-channels)))))

;;; --- UUIDs -------------------------------------------------------------

(test uuid16-is-att-wire-order
  (is (equalp (octets #xE1 #xFF) (ble:uuid16 #xFFE1))
      "ATT puts UUIDs little-endian; FFE1 goes out as E1 FF")
  (is (string= "FFE1" (ble:uuid-string (ble:uuid16 #xFFE1)))
      "and reads back big-endian for humans"))

(test uuid-string-renders-the-128-bit-form
  ;; the Nordic UART service, ATT order (reversed)
  (let ((nus (make-array 16 :element-type '(unsigned-byte 8)
                            :initial-contents
                            '(#x9E #xCA #xDC #x24 #x0E #xE5 #xA9 #xE0
                              #x93 #xF3 #xA3 #xB5 #x01 #x00 #x40 #x6E))))
    (is (string= "6E400001-B5A3-F393-E0A9-E50E24DCCA9E" (ble:uuid-string nus)))))

;;; --- characteristic properties -----------------------------------------

(test property-bits-decode-to-names
  (flet ((names (bits) (ble:gatt-char-property-names
                        (ble::make-gatt-char :handle 1 :properties bits :uuid (ble:uuid16 1)))))
    ;; 0x1C = write-without-response | write | notify -- the bitmap a
    ;; serial-over-GATT bridge presents, and the reason discovery keeps it
    (is (equal '(:write-without-response :write :notify) (names #x1C)))
    (is (equal '(:read) (names #x02)))
    (is (equal '(:indicate) (names #x20)))
    (is (null (names #x00)))))

;;; --- advertising-report parsing ----------------------------------------
;;;
;;; Offset arithmetic over data that arrives off a radio, which is exactly
;;; where a parser goes wrong and nothing tells you.

(test legacy-adv-report-parses-address-data-and-trailing-rssi
  ;; num=1, event 0x00, addr type 0x01, addr, len 3, data, then RSSI last
  (let* ((params (hex->octets "01" "00" "01" "D5D4D3D2D1D0" "03" "020106" "C6"))
         (reports (ble::parse-le-adv-report params)))
    (is (= 1 (length reports)))
    (let ((r (first reports)))
      (is (string= "D0:D1:D2:D3:D4:D5" (ble:format-mac (ble:adv-report-address r))))
      (is (= 1 (ble:adv-report-addr-type r)))
      (is (equalp (hex->octets "020106") (ble:adv-report-data r)))
      (is (= -58 (ble:adv-report-rssi r)) "0xC6 is -58 dBm"))))

(test extended-adv-report-carries-rssi-inline
  ;; num=1, then 24 fixed bytes (RSSI at offset 13), then data
  (let* ((params (hex->octets "01" "0000" "00" "D5D4D3D2D1D0"
                              "0000" "00" "00"        ; phys, sid, tx power
                              "C6"                     ; RSSI at offset 13
                              "0000" "0000" "00" "000000000000"
                              "03" "020106"))
         (reports (ble::parse-le-ext-adv-report params)))
    (is (= 1 (length reports)))
    (is (= -58 (ble:adv-report-rssi (first reports))))
    (is (string= "D0:D1:D2:D3:D4:D5"
                 (ble:format-mac (ble:adv-report-address (first reports)))))))

;;; --- connection parameters ---------------------------------------------

(test legacy-and-extended-create-conn-params-have-the-spec-lengths
  "25 octets for LE Create Connection, and 10 + 16 per initiating PHY for the
extended form. A wrong length is rejected by the controller with a status
byte that says nothing about which field moved."
  (let ((mac (ble:parse-mac "D0:D1:D2:D3:D4:D5")))
    (is (= 25 (length (ble::%legacy-create-conn-params mac 0))))
    (is (= 26 (length (ble::%extended-create-conn-params mac 0 #x01)))
        "one PHY")
    (is (= 42 (length (ble::%extended-create-conn-params mac 0 #x05)))
        "1M + Coded")
    ;; the peer address must land where the spec says, or we connect to
    ;; whatever that byte pattern happens to name
    (is (equalp mac (subseq (ble::%legacy-create-conn-params mac 0) 6 12)))
    (is (equalp mac (subseq (ble::%extended-create-conn-params mac 0 #x01) 3 9)))))

;;; --- reading -----------------------------------------------------------

(test read-request-pdus-are-spec-shaped
  (is (equalp (hex->octets "0A" "0C00") (ble::%read-req-pdu #x000C))
      "Read Request: opcode, then the handle little-endian")
  (is (equalp (hex->octets "0C" "0C00" "1400") (ble::%read-blob-req-pdu #x000C 20))
      "Read Blob adds a 16-bit offset"))

(test truncation-is-only-ever-a-suspicion
  "ATT has no length field and no more-data flag. A response that exactly
fills the MTU is the only hint the value continues -- and it is ambiguous,
which is why a long read costs a round trip that usually finds nothing."
  (let ((mtu 23))
    (is-true  (ble:value-may-be-truncated-p (ble:make-octets 22) mtu)
              "22 = MTU-1, so it might continue")
    (is-false (ble:value-may-be-truncated-p (ble:make-octets 21) mtu)
              "21 leaves room, so it certainly ended")
    (is-false (ble:value-may-be-truncated-p (ble:make-octets 0) mtu)))
  (is-true (ble:value-may-be-truncated-p (ble:make-octets 246) 247)))

(test read-characteristic-reports-a-missing-uuid-distinctly
  "Not found is not a read failure, and a caller that cannot tell them apart
will retry a device that was never going to have the characteristic."
  (multiple-value-bind (value error)
      (ble:att-read-characteristic nil '() (ble:uuid16 #xFFE1))
    (is (null value))
    (is (eq :not-found error))))

;;; --- RSSI and adapter labels -------------------------------------------
;;;
;;; These were in bledecode's CLI suite, testing this library's code from a
;;; consumer, because at the time ble had no suite that could load ble. It
;;; does now.

(test rssi-not-available-sentinel
  "0x7F is the spec's \"RSSI is not available\". Taken literally it renders as
a nonsensical +127 dBm, and every consumer already handles a missing RSSI --
a replayed capture has none either."
  (is (null (ble::decode-rssi #x7F)))
  (is (= -55 (ble::decode-rssi (+ 256 -55))))
  (is (= -1 (ble::decode-rssi #xFF)))
  (is (= -128 (ble::decode-rssi #x80)))
  (is (= 0 (ble::decode-rssi 0)))
  (is (= 20 (ble::decode-rssi 20))))

(test adapter-label-can-omit-the-address
  (let ((a (ble::make-hci-adapter
            :index 1 :bus :usb :product "TP-TP+ Bluetooth USB Adapter"
            :address (ble:parse-mac "A1:B2:C3:D4:E5:F6"))))
    (is (search "A1:B2:C3:D4:E5:F6" (ble:hci-adapter-label a)))
    (is (not (search "A1:B2" (ble:hci-adapter-label a :address nil))))
    (is (search "hci1" (ble:hci-adapter-label a :address nil)))
    (is (search "(USB)" (ble:hci-adapter-label a :address nil)))))

(test adapter-label-names-the-built-in-radio
  (let ((a (ble::make-hci-adapter :index 0 :bus :serial :product nil :address nil)))
    (is (search "built-in UART radio" (ble:hci-adapter-label a)))
    (is (search "(UART)" (ble:hci-adapter-label a)))))
