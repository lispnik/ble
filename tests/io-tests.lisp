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

;;; --- legacy advertising parameters -------------------------------------

(test adv-parameter-block-is-15-octets-with-fields-in-place
  "A wrong length or a shifted field is rejected by the controller with a
status byte that says nothing about which one moved."
  (let* ((peer (ble:parse-mac "D0:D1:D2:D3:D4:D5"))
         (p (ble::%adv-parameters-params #x00A0 #x00B0 ble:+adv-nonconn-ind+
                                         1 0 peer #x07 0)))
    (is (= 15 (length p)))
    (is (= #x00A0 (ble:u16-le p 0)) "interval min, little-endian")
    (is (= #x00B0 (ble:u16-le p 2)) "interval max")
    (is (= #x03 (aref p 4)) "adv type")
    (is (= 1 (aref p 5)) "own address type")
    (is (equalp peer (subseq p 7 13)) "peer address, on-air order")
    (is (= #x07 (aref p 13)) "all three primary channels")))

(test adv-data-block-is-always-32-octets-and-zero-padded
  "The command is fixed width however little of it means anything."
  (let ((p (ble::%adv-data-params (hex->octets "020106"))))
    (is (= 32 (length p)))
    (is (= 3 (aref p 0)) "significant length comes first")
    (is (equalp (hex->octets "020106") (subseq p 1 4)))
    (is (every #'zerop (subseq p 4)) "the rest is padding"))
  (let ((p (ble::%adv-data-params (ble:make-octets 31))))
    (is (= 32 (length p)))
    (is (= 31 (aref p 0)) "31 octets is the legal maximum")))

(test adv-data-over-31-octets-is-refused-not-truncated
  "Silently dropping the tail would put a malformed AD record on the air,
which a scanner reports as a device with no name rather than as an error."
  (signals error (ble::%adv-data-params (ble:make-octets 32))))

;;; --- service discovery --------------------------------------------------

(test service-uuid-string-round-trips
  (let ((s (ble::make-gatt-service :start 1 :end 9 :uuid (ble:uuid16 #x1800))))
    (is (= 1 (ble:gatt-service-start s)))
    (is (= 9 (ble:gatt-service-end s)))
    (is (string= "1800" (ble:gatt-service-uuid-string s)))))

(test find-service-by-uuid-matches-on-wire-order
  (let* ((gap (ble::make-gatt-service :start 1 :end 9 :uuid (ble:uuid16 #x1800)))
         (ffe0 (ble::make-gatt-service :start 10 :end 14 :uuid (ble:uuid16 #xFFE0)))
         (services (list gap ffe0)))
    (is (eq ffe0 (ble:find-service-by-uuid services (ble:uuid16 #xFFE0))))
    (is (eq gap (ble:find-service-by-uuid services (ble:uuid16 #x1800))))
    (is (null (ble:find-service-by-uuid services (ble:uuid16 #x180F))))))

;;; --- the ATT loops, driven by a scripted peer --------------------------
;;;
;;; What follows is the part that needed a radio until the library grew
;;; ATT-TEST-CHANNEL. These are walks with termination conditions -- stop at
;;; 0xFFFF, stop on a short response, stop when the peer errors -- and a walk
;;; that fails to terminate hangs rather than fails.

(defun scripted (&rest exchanges)
  "A channel whose responder answers by opcode. EXCHANGES are (OPCODE . FN)
where FN takes the request PDU and returns the reply."
  (ble:make-att-test-channel
   :responder (lambda (pdu)
                (let ((entry (assoc (aref pdu 0) exchanges)))
                  (when entry (funcall (cdr entry) pdu))))))

(test service-discovery-walks-until-the-handles-run-out
  (let* ((round 0)
         (chan (scripted
                (cons #x10
                      (lambda (pdu)
                        (declare (ignore pdu))
                        (incf round)
                        (case round
                          ;; each-len 6: start(2) end(2) uuid16(2)
                          (1 (hex->octets "11" "06" "0100" "0900" "0018"
                                                    "0A00" "0E00" "E0FF"))
                          ;; second round, and this one ends at FFFF
                          (2 (hex->octets "11" "06" "0F00" "FFFF" "0F18"))
                          (t (hex->octets "01" "10" "0000" "0A")))))))
         (services (ble:att-discover-services chan)))
    (is (= 3 (length services)))
    (is (equal '(1 10 15) (mapcar #'ble:gatt-service-start services)))
    (is (string= "FFE0" (ble:gatt-service-uuid-string (second services))))
    (is (= 2 round)
        "must stop after the group ending at FFFF -- asking again would wrap ~
         START to 0 and walk forever")))

(test service-discovery-stops-when-the-peer-says-not-found
  (let ((chan (scripted (cons #x10 (lambda (pdu) (declare (ignore pdu))
                                     ;; Attribute Not Found ends a walk
                                     (hex->octets "01" "10" "0100" "0A"))))))
    (is (null (ble:att-discover-services chan)))))

(test characteristic-discovery-asks-over-the-range-it-was-given
  "The only place scoping is observable is the request itself."
  (let* ((chan (scripted (cons #x08 (lambda (pdu) (declare (ignore pdu))
                                      (hex->octets "01" "08" "0A00" "0A"))))))
    (ble:att-discover-characteristics chan :start #x000A :end #x000E)
    (let ((req (first (ble:att-test-channel-sent-pdus chan))))
      (is (= #x000A (ble:u16-le req 1)) "start handle")
      (is (= #x000E (ble:u16-le req 3)) "end handle, not FFFF"))))

(test characteristic-discovery-reads-properties-and-stops-on-not-found
  (let* ((n 0)
         (chan (scripted
                (cons #x08 (lambda (pdu)
                             (declare (ignore pdu))
                             (incf n)
                             (if (= n 1)
                                 ;; each-len 7: decl(2) props(1) value(2) uuid16(2)
                                 (hex->octets "09" "07" "0B00" "1C" "0C00" "E1FF")
                                 (hex->octets "01" "08" "0C00" "0A")))))))
    (let ((chars (ble:att-discover-characteristics chan :start 10 :end 20)))
      (is (= 1 (length chars)))
      (is (= #x000C (ble:gatt-char-handle (first chars))))
      (is (equal '(:write-without-response :write :notify)
                 (ble:gatt-char-property-names (first chars)))
          "0x1C is the bitmap a serial-over-GATT bridge presents"))))

(test a-peer-that-repeats-itself-does-not-hang-the-walk
  "Found by a stub that returned the same reply every time: the walk advanced
START from the handles in the response, so a response that did not move
forward meant the next request was identical, and so was the next answer.
That is an infinite loop, and an infinite loop in a discovery walk presents
as a hung process rather than as an error."
  (let ((chan (scripted
               ;; always the same record, whatever we ask
               (cons #x08 (lambda (pdu) (declare (ignore pdu))
                            (hex->octets "09" "07" "0B00" "1C" "0C00" "E1FF")))
               (cons #x10 (lambda (pdu) (declare (ignore pdu))
                            (hex->octets "11" "06" "0100" "0900" "0018"))))))
    (finishes (ble:att-discover-characteristics chan :start 20 :end 40))
    (finishes (ble:att-discover-services chan))))

(test long-read-continues-with-blobs-until-a-short-response
  (let* ((mtu 8)                      ; a response carries at most 7 octets
         (chan (scripted
                (cons #x0A (lambda (pdu) (declare (ignore pdu))
                             ;; opcode + 7 octets = exactly full, so ambiguous
                             (hex->octets "0B" "01020304050607")))
                (cons #x0C (lambda (pdu)
                             (let ((offset (ble:u16-le pdu 3)))
                               (is (= 7 offset) "blob must resume where we stopped")
                               ;; short: 3 octets, so the value ends here
                               (hex->octets "0D" "080910")))))))
    (multiple-value-bind (value error) (ble:att-read-long-value chan 12 :mtu mtu)
      (is (null error))
      (is (equalp (hex->octets "01020304050607080910") value)))))

(test long-read-treats-attribute-not-long-as-the-end
  "A peer answering Attribute Not Long is saying the value ended exactly on
the boundary. That is a complete read, not a failure."
  (let ((chan (scripted
               ;; 21 octets = MTU-1 at MTU 22, so it looks like it continues
               (cons #x0A (lambda (pdu) (declare (ignore pdu))
                            (hex->octets "0B" "0102030405060708090A0B0C0D0E0F10"
                                              "1112131415")))
               (cons #x0C (lambda (pdu) (declare (ignore pdu))
                            (hex->octets "01" "0C" "0C00" "0B"))))))
    (multiple-value-bind (value error) (ble:att-read-long-value chan 12 :mtu 22)
      (is (null error))
      (is (= 21 (length value))))))

(test read-reports-an-att-error-rather-than-inventing-a-value
  (let ((chan (scripted (cons #x0A (lambda (pdu) (declare (ignore pdu))
                                     ;; Read Not Permitted
                                     (hex->octets "01" "0A" "0C00" "02"))))))
    (multiple-value-bind (value error) (ble:att-read-value chan 12)
      (is (null value))
      (is (= 2 error)))))

(test an-indication-is-confirmed-before-its-value-is-returned
  "Without the confirmation the peer sends one indication and then waits
forever, which looks like a device that stopped talking."
  (let ((chan (ble:make-att-test-channel :responder (constantly nil))))
    (setf (ble::att-test-channel-inbox chan)
          (list (hex->octets "1D" "0C00" "DEAD")))    ; indication on handle 0x0C
    (is (equalp (hex->octets "DEAD") (ble:att-next-notification chan #x000C)))
    (is (equalp (list (hex->octets "1E"))
                (ble:att-test-channel-sent-pdus chan))
        "a bare Handle Value Confirmation, and nothing else")))

(test a-notification-needs-no-confirmation
  (let ((chan (ble:make-att-test-channel :responder (constantly nil))))
    (setf (ble::att-test-channel-inbox chan)
          (list (hex->octets "1B" "0C00" "BEEF")))
    (is (equalp (hex->octets "BEEF") (ble:att-next-notification chan #x000C)))
    (is (null (ble:att-test-channel-sent-pdus chan)))))

(test att-request-answers-a-peer-mtu-request-mid-exchange
  "Devices send their own Exchange MTU Request right after connect. Ignoring
it desyncs every later request/response pair."
  (let* ((chan (scripted
                (cons #x0A (lambda (pdu) (declare (ignore pdu))
                             (list (hex->octets "02" "F700")     ; peer asks first
                                   (hex->octets "0B" "2A"))))))) ; then answers
    (multiple-value-bind (value error) (ble:att-read-value chan 12)
      (is (null error))
      (is (equalp (hex->octets "2A") value)))
    (is (find #x03 (ble:att-test-channel-sent-pdus chan) :key (lambda (p) (aref p 0)))
        "we must have replied with an Exchange MTU Response")))

;;; --- remaining pure buffers --------------------------------------------

(test hci-opcode-packs-ogf-and-ocf
  ;; LE Set Scan Enable: OGF 0x08, OCF 0x000C -> 0x200C
  (is (= #x200C (ble::hci-opcode #x08 #x000C)))
  (is (= #x0C03 (ble::hci-opcode #x03 #x0003)) "HCI Reset"))

(test hci-filter-is-16-octets-with-two-of-padding
  "The kernel sizes struct hci_filter at 16 and rejects anything else with
EINVAL, so the padding is load-bearing."
  (let ((b (ble::%hci-filter-bytes)))
    (is (= 16 (length b)))
    (is (every (lambda (x) (= x #xFF)) (subseq b 0 12)) "type and event masks")
    (is (every #'zerop (subseq b 12)) "opcode and tail padding")))

(test sockaddr-l2-lays-out-family-cid-and-address
  "14 octets: family(2) psm(2) bdaddr(6) cid(2) type(1) + pad. A field in the
wrong place connects to a different device, or to nothing."
  (let ((mac (ble:parse-mac "D0:D1:D2:D3:D4:D5")))
    (cffi:with-foreign-object (sa :unsigned-char 14)
      (ble::%fill-sockaddr-l2 sa :bdaddr mac :bdaddr-type 1)
      (let ((b (ble::foreign-to-bytes sa 14)))
        (is (= 31 (ble:u16-le b 0)) "AF_BLUETOOTH")
        (is (= 0 (ble:u16-le b 2)) "PSM is 0 for a fixed channel")
        (is (equalp mac (subseq b 4 10)) "address, on-air order")
        (is (= 4 (ble:u16-le b 10)) "ATT is CID 0x0004")
        (is (= 1 (aref b 12)) "address type")))))

;;; --- read multiple and long writes -------------------------------------

(test read-multiple-packs-handles-and-returns-values-concatenated
  "ATT sends the values back with no lengths and no delimiters, which is the
whole caveat on this operation."
  (let ((chan (scripted
               (cons #x0E (lambda (pdu)
                            (is (= 5 (length pdu)) "opcode plus two handles")
                            (is (= #x000C (ble:u16-le pdu 1)))
                            (is (= #x000E (ble:u16-le pdu 3)))
                            (hex->octets "0F" "AABB" "CC"))))))
    (multiple-value-bind (value error) (ble:att-read-multiple chan '(#x0C #x0E))
      (is (null error))
      (is (equalp (hex->octets "AABBCC") value)))))

(test long-write-chunks-by-mtu-and-offsets-each-part
  (let* ((prepared '())
         (executed nil)
         (chan (scripted
                (cons #x16 (lambda (pdu)
                             (push (cons (ble:u16-le pdu 3) (subseq pdu 5)) prepared)
                             ;; echo handle, offset and part back
                             (concatenate '(vector (unsigned-byte 8))
                                          (hex->octets "17") (subseq pdu 1))))
                (cons #x18 (lambda (pdu)
                             (setf executed (aref pdu 1))
                             (hex->octets "19"))))))
    ;; MTU 9 leaves 4 octets per part (opcode + handle + offset = 5)
    (is (eq t (ble:att-write-long-value chan #x000C (hex->octets "0102030405060708090A")
                                        :mtu 9)))
    (setf prepared (reverse prepared))
    (is (equal '(0 4 8) (mapcar #'car prepared)) "offsets advance by the chunk size")
    (is (equalp (hex->octets "01020304") (cdr (first prepared))))
    (is (equalp (hex->octets "090A") (cdr (third prepared))) "short final part")
    (is (= 1 executed) "flags = 1 commits the queue")))

(test a-failed-long-write-cancels-the-queue-it-left-behind
  "Leaving prepared writes queued hands the next client a half-written value
to commit, so a failure has to clean up after itself."
  (let* ((executed nil)
         (chan (scripted
                (cons #x16 (lambda (pdu) (declare (ignore pdu))
                             ;; Invalid Offset on the very first part
                             (hex->octets "01" "16" "0C00" "07")))
                (cons #x18 (lambda (pdu)
                             (setf executed (aref pdu 1))
                             (hex->octets "19"))))))
    (is (= 7 (ble:att-write-long-value chan #x000C (hex->octets "0102030405")
                                       :mtu 9))
        "the ATT error code is returned, not swallowed")
    (is (= 0 executed) "flags = 0 cancels")))

(test execute-write-can-cancel-explicitly
  (let ((chan (scripted (cons #x18 (lambda (pdu)
                                     (is (= 0 (aref pdu 1)))
                                     (hex->octets "19"))))))
    (is (eq t (ble:att-execute-write chan :cancel t)))))

;;; --- regressions found by review ---------------------------------------

(test characteristic-walk-rejects-an-impossible-record-length
  "A characteristic record is decl(2) props(1) value(2) uuid(2+), so seven
octets is the floor. Without a guard, each-len 0 spins forever and 1..6 walks
off the end of the response. The service walk had this check; the
characteristic walk did not."
  (dolist (each '(0 1 6))
    (let ((chan (scripted
                 (cons #x08 (lambda (pdu)
                              (declare (ignore pdu))
                              (let ((rsp (ble:make-octets 8)))
                                (setf (aref rsp 0) #x09
                                      (aref rsp 1) each)
                                rsp))))))
      (finishes (ble:att-discover-characteristics chan))
      (is (null (ble:att-discover-characteristics chan))
          "each-len ~D must yield nothing, not a hang or a bounds error" each))))

(test closing-a-nus-drops-it-from-the-open-channel-registry
  "NUS-CLOSE used to close the fd directly, leaving a stale entry behind. An
fd number gets reused, so the exit hook would later close whatever had since
been handed that number."
  (let* ((ble:*open-att-channels* nil)
         (chan (ble:register-att-channel 4242))
         (nus (ble::make-nus :fd chan :mtu 23)))
    (is (equal '(4242) ble:*open-att-channels*))
    ;; %close on a bogus fd fails harmlessly; the unregister is the point
    (ignore-errors (ble:nus-close nus))
    (is (null ble:*open-att-channels*)
        "the channel must be gone from the registry, not merely closed")))

(test the-mtu-we-advertise-is-the-mtu-we-answer-with
  "A caller that asks for 247 and then answers a peer's own Exchange MTU
Request with 23 leaves the two ends disagreeing about ATT_MTU, and the larger
writes it believes are legal get silently dropped."
  (let ((ble:*att-rx-mtu* 23))
    (let ((chan (scripted (cons #x02 (lambda (pdu) (declare (ignore pdu))
                                       (hex->octets "03" "F700"))))))
      (ble:att-exchange-mtu chan 247)
      (is (= 247 ble:*att-rx-mtu*) "we must remember what we advertised"))
    ;; now a peer asks us, mid-exchange, and must hear the same number
    (let ((chan (scripted (cons #x0A (lambda (pdu) (declare (ignore pdu))
                                       (list (hex->octets "02" "F700")
                                             (hex->octets "0B" "2A")))))))
      (ble:att-read-value chan 12)
      (let ((answer (find #x03 (ble:att-test-channel-sent-pdus chan)
                          :key (lambda (p) (aref p 0)))))
        (is-true answer "we should have answered the peer's MTU request")
        (is (= 247 (ble:u16-le answer 1))
            "and with the MTU we advertised, not the default")))))

;;; --- L2CAP reassembly ---------------------------------------------------
;;;
;;; These drive %DRAIN-L2CAP-FRAMES directly. It is the half of
;;; HCI-ACL-RECV-ATT that does not need a socket, and it is where the
;;; interesting mistakes are: what happens to a second frame, and to a partial
;;; one. The bug these pin down dropped every frame after the first in a read.

(defun l2cap-frame (cid payload-hex)
  "One L2CAP B-frame: 2-octet length, 2-octet CID, payload."
  (let* ((payload (hex->octets payload-hex))
         (out (ble:make-octets (+ 4 (length payload)))))
    (ble:u16le-put out 0 (length payload))
    (ble:u16le-put out 2 cid)
    (replace out payload :start1 4)
    out))

(defun drained (&rest frames)
  "Feed FRAMES to a fresh conn's buffer, drain, and return (VALUES PENDING REMAINDER)."
  (let ((conn (ble::make-hci-conn
               :rxbuf (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                             frames))))
    (ble::%drain-l2cap-frames conn)
    (values (ble::hci-conn-pending conn) (ble::hci-conn-rxbuf conn))))

(test two-att-frames-in-one-read-both-survive
  "A reply to a command and a periodic notification can complete in the same
read. Taking one frame and clearing the buffer threw the other away."
  (multiple-value-bind (pending remainder)
      (drained (l2cap-frame 4 "1B0C00AA") (l2cap-frame 4 "1B0C00BB"))
    (is (= 2 (length pending)) "both frames must be queued")
    (is (equalp (hex->octets "1B0C00AA") (first pending)))
    (is (equalp (hex->octets "1B0C00BB") (second pending))
        "and in arrival order")
    (is (zerop (length remainder)) "nothing left over")))

(test a-non-att-frame-does-not-swallow-the-att-frame-behind-it
  "Draining must skip a frame on another CID and keep going, not stop."
  (multiple-value-bind (pending) (drained (l2cap-frame 5 "DEADBEEF")
                                          (l2cap-frame 4 "1B0C0042"))
    (is (= 1 (length pending)) "the signalling frame is not an ATT PDU")
    (is (equalp (hex->octets "1B0C0042") (first pending))
        "but the ATT frame behind it must still arrive")))

(test a-partial-frame-is-kept-for-the-next-fragment
  "The remainder is what the continuation fragment gets appended to."
  (let ((whole (l2cap-frame 4 "1B0C00AA")))
    (multiple-value-bind (pending remainder)
        (drained (l2cap-frame 4 "1B0C00BB") (subseq whole 0 3))
      (is (= 1 (length pending)) "only the complete frame comes out")
      (is (equalp (subseq whole 0 3) remainder)
          "the partial one stays buffered, byte for byte"))))

(test draining-an-empty-or-header-only-buffer-is-harmless
  (multiple-value-bind (pending remainder) (drained (ble:make-octets 0))
    (is (null pending))
    (is (zerop (length remainder))))
  ;; three octets cannot even carry an L2CAP header
  (multiple-value-bind (pending remainder) (drained (hex->octets "040000"))
    (is (null pending))
    (is (= 3 (length remainder)) "kept until the rest of the header arrives")))

;;; --- notification dispatch across several handles -----------------------
;;;
;;; The bug these pin down: ATT-NEXT-NOTIFICATION filtered on one handle and
;;; dropped every PDU that did not match, and ATT-REQUEST dropped stray
;;; notifications outright. Between them, a peer notifying on two
;;; characteristics lost whichever one the caller was not asking for at that
;;; instant -- silently, and indistinguishably from a peer gone quiet.

(defun ntf (handle payload-hex)
  "A Handle Value Notification PDU."
  (let* ((payload (hex->octets payload-hex))
         (pdu (ble:make-octets (+ 3 (length payload)))))
    (setf (aref pdu 0) #x1B)
    (ble:u16le-put pdu 1 handle)
    (replace pdu payload :start1 3)
    pdu))

(defun inbox-channel (&rest pdus)
  "A channel that simply hands back PDUS, in order, with no responder."
  (let ((chan (ble:make-att-test-channel)))
    (setf (ble:att-test-channel-inbox chan) (mapcar #'ble:coerce-octets pdus))
    chan))

(test a-notification-on-another-handle-is-kept-not-dropped
  (let ((chan (inbox-channel (ntf #x0020 "BB") (ntf #x0010 "AA"))))
    (ble:att-clear-notifications chan)
    (is (equalp (hex->octets "AA") (ble:att-next-notification chan #x0010 50))
        "the handle we asked for must arrive, past the one we did not")
    (is (= 1 (ble:att-pending-notifications chan #x0020))
        "and the other handle's notification must still be there")
    (is (equalp (hex->octets "BB") (ble:att-next-notification chan #x0020 50))
        "claimable afterwards, which is the whole point")))

(test next-notification-any-reports-which-handle-it-came-from
  (let ((chan (inbox-channel (ntf #x0020 "BB") (ntf #x0010 "AA"))))
    (ble:att-clear-notifications chan)
    (multiple-value-bind (value handle) (ble:att-next-notification-any chan 50)
      (is (equalp (hex->octets "BB") value) "oldest first")
      (is (= #x0020 handle) "and the caller is told which characteristic"))
    (multiple-value-bind (value handle) (ble:att-next-notification-any chan 50)
      (is (equalp (hex->octets "AA") value))
      (is (= #x0010 handle)))))

(test a-request-does-not-eat-notifications-that-arrive-during-it
  "A read that runs while the peer is notifying must not cost the subscriber
its readings -- this is the common case on a device that notifies at 1 Hz."
  (let ((chan (ble:make-att-test-channel
               :responder (lambda (pdu)
                            (when (= (aref pdu 0) #x0A)
                              ;; a stray notification arrives first, then the
                              ;; read response we are actually waiting for
                              (list (ntf #x0010 "AA")
                                    (hex->octets "0B" "DEAD")))))))
    (ble:att-clear-notifications chan)
    (is (equalp (hex->octets "DEAD") (ble:att-read-value chan #x000C))
        "the read still returns its own answer")
    (is (= 1 (ble:att-pending-notifications chan #x0010))
        "and the notification it stepped over was kept")))

(test an-indication-is-confirmed-when-received-not-when-claimed
  "The peer may send no further indication until confirmed, so deferring the
confirmation until someone claims the value would stall the link."
  (let ((chan (inbox-channel (let ((p (ntf #x0010 "AA")))
                               (setf (aref p 0) #x1D) ; indication
                               p))))
    (ble:att-clear-notifications chan)
    (is (equalp (hex->octets "AA") (ble:att-next-notification chan #x0010 50)))
    (is (find #x1E (ble:att-test-channel-sent-pdus chan)
              :key (lambda (p) (aref p 0)))
        "a Handle Value Confirmation must have gone back")))

(test the-notification-queue-is-bounded
  "A peer notifying faster than anyone reads must not grow this without end."
  (let ((chan (ble:make-att-test-channel))
        (ble:*att-notification-queue-limit* 4))
    (ble:att-clear-notifications chan)
    (setf (ble:att-test-channel-inbox chan)
          (loop for i below 10 collect (ntf #x0010 "AA")))
    ;; drain what will fit; the excess is dropped from the front
    (loop repeat 10 while (ble:att-next-notification chan #x0099 1))
    (is (<= (ble:att-pending-notifications chan) 4)
        "the bound must hold")))

(test closing-a-channel-forgets-its-queue
  "fd numbers get reused; a new channel must not inherit the old one's backlog."
  (let ((chan (inbox-channel (ntf #x0010 "AA"))))
    (ble:att-clear-notifications chan)
    (ble:att-next-notification chan #x0099 1)   ; queues it, claims nothing
    (is (= 1 (ble:att-pending-notifications chan)))
    (ble:att-clear-notifications chan)
    (is (zerop (ble:att-pending-notifications chan)))))

;;; --- conditions ---------------------------------------------------------

(test sentinels-are-still-the-default
  "Turning every timeout into a stack unwind would make this unpleasant to
poll with, so the old return values stand unless a caller opts in."
  (let ((chan (scripted (cons #x12 (lambda (pdu) (declare (ignore pdu))
                                     (hex->octets "01" "12" "0C00" "03"))))))
    (is (= 3 (ble:att-write-value chan #x000C (hex->octets "AA")))
        "an ATT error code comes back as an integer, as it always has")))

(test with-ble-conditions-signals-a-typed-att-error
  (let ((chan (scripted (cons #x12 (lambda (pdu) (declare (ignore pdu))
                                     (hex->octets "01" "12" "0C00" "03"))))))
    (handler-case (ble:with-ble-conditions
                    (ble:att-write-value chan #x000C (hex->octets "AA"))
                    (is nil "should have signalled"))
      (ble:att-error (e)
        (is (= 3 (ble:att-error-code e)) "write not permitted")
        (is (= #x000C (ble:att-error-handle e)) "and it names the handle")
        (is (string= "write not permitted" (ble:att-error-name 3)))))))

(test every-condition-descends-from-ble-error
  "The point of the hierarchy: one handler for anything Bluetooth-related."
  (dolist (type '(ble:att-error ble:att-timeout ble:peer-disconnected
                  ble:gatt-not-found ble:syscall-error ble:invalid-mac))
    (is (subtypep type 'ble:ble-error)
        (format nil "~A must inherit BLE-ERROR" type))))

(test a-failed-subscribe-always-signals
  "No sentinel here in either style: a CCCD write that fails means no
notification will ever arrive, and a caller carrying on would wait out its
timeouts against a peer that was never going to speak."
  (let ((chan (scripted (cons #x12 (lambda (pdu) (declare (ignore pdu))
                                     (hex->octets "01" "12" "0D00" "02"))))))
    (handler-case (progn (ble:att-subscribe chan #x000D)
                         (is nil "should have signalled"))
      (ble:att-error (e) (is (= 2 (ble:att-error-code e)))))))

;;; --- the GATT server, driven by this library's own client ---------------
;;;
;;; These wire the two halves together in one image: the client's att-send is
;;; handed to the server, and whatever the server sends back becomes the
;;; client's inbox. No radio, and every request the client knows how to make
;;; gets answered by the server under test -- which is a far better exercise
;;; of both than a hand-written list of expected octets, because the two sides
;;; were written to the spec independently rather than to each other.

(defstruct (loopback (:constructor %make-loopback))
  client server-chan server)

(defun make-loopback (server)
  (let ((srv (ble:make-att-test-channel)))
    (%make-loopback
     :server server :server-chan srv
     :client (ble:make-att-test-channel
              :responder
              (lambda (pdu)
                (setf (ble:att-test-channel-sent srv) '())
                (ble:gatt-serve-pdu server srv pdu)
                (reverse (ble:att-test-channel-sent srv)))))))

(defun deliver-server-traffic (lb)
  "Move anything the server sent unprompted (notifications) to the client."
  (let ((srv (loopback-server-chan lb)))
    (setf (ble:att-test-channel-inbox (loopback-client lb))
          (append (ble:att-test-channel-inbox (loopback-client lb))
                  (reverse (ble:att-test-channel-sent srv)))
          (ble:att-test-channel-sent srv) '())))

(defun demo-server ()
  "Two services, a static value, a computed one, a writable one, and a
notifying characteristic -- i.e. one of each thing a server has to get right."
  (let ((server (ble:make-gatt-server :mtu 247))
        (counter 0))
    (ble:gatt-add-service server #x180A)                 ; Device Information
    (ble:gatt-add-characteristic server :uuid #x2A29 :properties '(:read)
                                        :value "ACME")   ; manufacturer
    (ble:gatt-add-characteristic server :uuid #x2A24 :properties '(:read)
                                        :on-read (lambda (s a)
                                                   (declare (ignore s a))
                                                   (format nil "n=~D" (incf counter))))
    (ble:gatt-add-service server #xFFE0)                 ; a vendor service
    (multiple-value-bind (value-handle cccd-handle)
        (ble:gatt-add-characteristic server :uuid #xFFE1
                                     :properties '(:read :write :notify)
                                     :value #(1 2 3))
      ;; Return the handles the server assigned rather than deriving them --
      ;; a characteristic occupies two attributes or three depending on
      ;; whether it notifies, so arithmetic on a neighbour's handle addresses
      ;; a declaration about as often as it addresses a value.
      (let ((ffe2 (ble:gatt-add-characteristic
                   server :uuid #xFFE2 :properties '(:write)
                          :on-write (lambda (s a v)
                                      (declare (ignore s a))
                                      ;; refuse anything but one octet
                                      (unless (= 1 (length v)) #x0D))))
            (ffe3 (ble:gatt-add-characteristic
                   server :uuid #xFFE3 :properties '())))
        (values server value-handle cccd-handle ffe2 ffe3)))))

(test the-server-lays-out-handles-the-way-gatt-requires
  "A characteristic's declaration must sit immediately before its value, and
its CCCD immediately after. Getting this wrong makes discovery return a
database that reads plausibly and points at the wrong attributes."
  (multiple-value-bind (server value-handle cccd-handle) (demo-server)
    (let ((decl (ble:gatt-find-attribute server (1- value-handle))))
      (is (equalp (ble:uuid16 #x2803) (ble:gatt-attribute-uuid decl))
          "the attribute before the value is the declaration")
      (is (= value-handle (ble:u16-le (ble:gatt-attribute-value decl) 1))
          "and the declaration points at the value handle"))
    (is (= cccd-handle (1+ value-handle)) "the CCCD follows the value")
    (is (equalp (ble:uuid16 #x2902)
                (ble:gatt-attribute-uuid (ble:gatt-find-attribute server cccd-handle))))))

(test the-client-discovers-the-services-the-server-declares
  (let* ((lb (make-loopback (demo-server)))
         (services (ble:att-discover-services (loopback-client lb))))
    (is (= 2 (length services)) "both services found")
    (is (equalp '("180A" "FFE0")
                (mapcar #'ble:gatt-service-uuid-string services))
        "with the UUIDs declared, in handle order")
    ;; the second service's range must cover its characteristics
    (let ((vendor (second services)))
      (is (> (ble:gatt-service-end vendor) (ble:gatt-service-start vendor))
          "a service with characteristics spans more than one handle"))))

(test find-service-answers-in-one-round-trip
  "Find By Type Value is the single-exchange form; ATT-FIND-SERVICE uses it."
  (let ((lb (make-loopback (demo-server))))
    (let ((svc (ble:att-find-service (loopback-client lb) (ble:uuid16 #xFFE0))))
      (is-true svc "the vendor service is found by UUID")
      (is (equalp "FFE0" (ble:gatt-service-uuid-string svc))))
    (is (null (ble:att-find-service (loopback-client lb) (ble:uuid16 #x1234)))
        "and a UUID that is not there returns NIL, not a wrong service")))

(test the-client-discovers-characteristics-and-their-properties
  (multiple-value-bind (server value-handle) (demo-server)
    (let* ((lb (make-loopback server))
           (chars (ble:att-discover-characteristics (loopback-client lb))))
      (is (= 5 (length chars)) "every characteristic in the database")
      (let ((ffe1 (ble:find-char-by-uuid chars (ble:uuid16 #xFFE1))))
        (is-true ffe1 "FFE1 is discoverable")
        (is (= value-handle (ble:gatt-char-handle ffe1))
            "and its value handle is the one the server assigned")
        (let ((props (ble:gatt-char-property-names ffe1)))
          (is (and (member :read props) (member :write props)
                   (member :notify props))
              "with the properties it was declared with"))))))

(test descriptor-discovery-finds-the-cccd
  (multiple-value-bind (server value-handle cccd-handle) (demo-server)
    (let* ((lb (make-loopback server))
           (found (ble:att-find-cccd (loopback-client lb) value-handle)))
      (is (= cccd-handle found)
          "att-find-cccd must land on the descriptor the server laid down"))))

(test reads-static-computed-and-refused
  (multiple-value-bind (server value-handle cccd ffe2 ffe3) (demo-server)
    (declare (ignore cccd ffe2))
    (let ((lb (make-loopback server)))
      (is (equalp (map 'vector #'char-code "ACME")
                  (ble:att-read-value (loopback-client lb) 3))
          "a static value comes back verbatim")
      (is (equalp #(1 2 3) (ble:att-read-value (loopback-client lb) value-handle)))
      ;; the computed one increments every time it is read
      (let ((a (ble:att-read-value (loopback-client lb) 5))
            (b (ble:att-read-value (loopback-client lb) 5)))
        (is (not (equalp a b))
            "an on-read characteristic is evaluated per read, not once"))
      ;; FFE3 was declared with no properties at all
      (multiple-value-bind (value err)
          (ble:att-read-value (loopback-client lb) ffe3)
        (is (null value) "a read of an unreadable attribute yields no value")
        (is (= #x02 err) "and reports read-not-permitted")))))

(test an-invalid-handle-is-reported-as-such
  (let ((lb (make-loopback (demo-server))))
    (multiple-value-bind (value err)
        (ble:att-read-value (loopback-client lb) #x0FFF)
      (is (null value))
      (is (= #x01 err) "invalid handle, not attribute-not-found"))))

(test writes-are-stored-refused-or-vetoed-by-the-hook
  (multiple-value-bind (server value-handle cccd ffe2) (demo-server)
    (declare (ignore cccd))
    (let ((lb (make-loopback server)))
      (is (eq t (ble:att-write-value (loopback-client lb) value-handle #(9 9)))
          "a permitted write is acknowledged")
      (is (equalp #(9 9) (ble:att-read-value (loopback-client lb) value-handle))
          "and the value is what a later read returns")
      ;; FFE2 has an on-write hook that refuses anything but one octet
      (is (= #x0D (ble:att-write-value (loopback-client lb) ffe2 #(1 2 3)))
          "the hook's error code is what the client sees")
      (is (eq t (ble:att-write-value (loopback-client lb) ffe2 #(7)))
          "and a write it accepts is acknowledged")
      ;; a read-only attribute
      (is (= #x03 (ble:att-write-value (loopback-client lb) 3 #(1)))
          "write-not-permitted on a read-only characteristic"))))

(test a-write-command-stores-without-answering
  (multiple-value-bind (server value-handle) (demo-server)
    (let ((lb (make-loopback server)))
      (ble:att-write-command (loopback-client lb) value-handle #(4 2))
      (is (equalp #(4 2) (ble:att-read-value (loopback-client lb) value-handle))
          "the value must land even though nothing was sent back"))))

(test mtu-exchange-settles-on-the-smaller-of-the-two
  (let* ((server (ble:make-gatt-server :mtu 247))
         (lb (make-loopback server))
         (ble:*att-rx-mtu* 100))
    (ble:att-exchange-mtu (loopback-client lb) 60)
    (is (= 60 (ble:gatt-server-mtu server))
        "the server must use the client's smaller number, not its own")))

(test an-unsupported-request-is-refused-not-ignored
  (let* ((lb (make-loopback (demo-server)))
         (chan (loopback-client lb)))
    ;; 0x20 Read Multiple Variable Length Request (BT 5.2): a real request
    ;; this server does not implement, so it must be refused rather than
    ;; ignored -- a client is waiting for an answer to it.
    (let ((rsp (ble:att-request chan (hex->octets "20" "0300" "0500"))))
      (is-true (ble:att-error-p rsp) "an Error Response must come back")
      (is (= #x06 (aref rsp 4)) "request not supported"))))

(test the-wrong-group-type-is-refused-rather-than-answered-empty
  "A client must be able to tell 'no services there' from 'that is not a
group type', which an empty answer cannot express."
  (let* ((lb (make-loopback (demo-server)))
         (req (ble:make-octets 7)))
    (setf (aref req 0) #x10)
    (ble:u16le-put req 1 1)
    (ble:u16le-put req 3 #xFFFF)
    (ble:u16le-put req 5 #x2803)          ; not a group type
    (let ((rsp (ble:att-request (loopback-client lb) req)))
      (is-true (ble:att-error-p rsp))
      (is (= #x10 (aref rsp 4)) "unsupported group type"))))

(test notifications-only-after-the-client-subscribes
  (multiple-value-bind (server value-handle cccd-handle) (demo-server)
    (let* ((lb (make-loopback server))
           (chan (loopback-client lb))
           (srv (loopback-server-chan lb)))
      (is (null (ble:gatt-notify server srv value-handle #(1)))
          "sending before subscription is a protocol violation, so refuse it")
      (is-false (ble:gatt-subscribed-p server cccd-handle))
      (ble:att-subscribe chan cccd-handle)
      (is-true (ble:gatt-subscribed-p server cccd-handle)
               "the CCCD write is the subscription")
      (is-true (ble:gatt-notify server srv value-handle #(7 7))
               "and now a notification is allowed")
      (deliver-server-traffic lb)
      (is (equalp #(7 7) (ble:att-next-notification chan value-handle 50))
          "which the client receives on the value handle"))))

(test indications-need-their-own-bit
  (multiple-value-bind (server value-handle cccd-handle) (demo-server)
    (let* ((lb (make-loopback server))
           (chan (loopback-client lb))
           (srv (loopback-server-chan lb)))
      (ble:att-subscribe chan cccd-handle)          ; notifications only
      (is-false (ble:gatt-subscribed-p server cccd-handle :indications t)
                "subscribing to notifications must not enable indications")
      (ble:att-subscribe chan cccd-handle :indications t)
      (is-true (ble:gatt-subscribed-p server cccd-handle :indications t))
      (is-true (ble:gatt-notify server srv value-handle #(5) :indications t))
      (deliver-server-traffic lb)
      (is (equalp #(5) (ble:att-next-notification chan value-handle 50))
          "an indication reaches the client through the same call")
      (is (find #x1E (ble:att-test-channel-sent-pdus chan)
                :key (lambda (p) (aref p 0)))
          "and the client confirms it"))))

(test a-long-value-survives-the-round-trip
  "Read Blob plus reassembly: the server must serve offsets and the client
must stitch them, and a small MTU is what forces both."
  (let* ((server (ble:make-gatt-server :mtu 23))
         (blob (ble:make-octets 60)))
    (dotimes (i 60) (setf (aref blob i) i))
    (ble:gatt-add-service server #xFFE0)
    (let ((h (ble:gatt-add-characteristic server :uuid #xFFE1
                                                 :properties '(:read)
                                                 :value blob)))
      (let ((lb (make-loopback server)))
        (is (equalp blob (ble:att-read-long-value (loopback-client lb) h))
            "every octet, in order, across several reads")))))

(test read-multiple-concatenates-in-request-order
  (multiple-value-bind (server value-handle) (demo-server)
    (let ((lb (make-loopback server)))
      (is (equalp (concatenate 'vector (map 'vector #'char-code "ACME") #(1 2 3))
                  (ble:att-read-multiple (loopback-client lb)
                                         (list 3 value-handle)))
          "values arrive back to back, in the order asked for"))))

;;; --- resource-safe wrappers ---------------------------------------------

(test with-att-channel-closes-on-a-normal-exit
  (let ((chan (inbox-channel (ntf #x0010 "AA"))))
    (ble:with-att-channel (c chan)
      (ble:att-next-notification c #x0099 1))       ; queues, claims nothing
    (is (zerop (ble:att-pending-notifications chan))
        "closing must drop the channel's notification backlog")))

(test with-att-channel-closes-on-a-nonlocal-exit
  "The reason these macros exist: an HCI_CHANNEL_USER socket that escapes
cleanup holds the adapter away from the whole machine."
  (let ((chan (inbox-channel (ntf #x0010 "AA"))))
    (ignore-errors
     (ble:with-att-channel (c chan)
       (ble:att-next-notification c #x0099 1)
       (error "boom")))
    (is (zerop (ble:att-pending-notifications chan))
        "cleanup must have run even though the body threw")))

(test the-server-never-answers-a-command-even-when-refusing-it
  "A Write Command carries no response, so refusing one with an Error Response
would itself be a protocol error -- the client is not waiting for anything and
would read the error as the answer to whatever it asked next."
  (multiple-value-bind (server) (demo-server)
    (let* ((srv (ble:make-att-test-channel))
           ;; handle 3 is read-only, so this write must be refused
           (cmd (let ((p (ble:make-octets 4)))
                  (setf (aref p 0) #x52)
                  (ble:u16le-put p 1 3)
                  (setf (aref p 3) #xAA)
                  p)))
      (ble:gatt-serve-pdu server srv cmd)
      (is (null (ble:att-test-channel-sent srv))
          "nothing at all may go back for a command"))))

(test a-signed-write-is-dropped-rather-than-refused
  "Also a command by opcode. Unimplemented, but silence is the correct way to
not implement it."
  (multiple-value-bind (server) (demo-server)
    (let ((srv (ble:make-att-test-channel))
          (pdu (ble:make-octets 16)))
      (setf (aref pdu 0) #xD2)            ; Signed Write Command
      (ble:u16le-put pdu 1 3)
      (ble:gatt-serve-pdu server srv pdu)
      (is (null (ble:att-test-channel-sent srv))
          "no Error Response for an opcode with the command bit set"))))

(test the-server-truncates-a-value-to-the-negotiated-mtu
  "Answering with more than the client agreed to receive gets the response
dropped by the client, which looks like a server that did not answer."
  (let ((server (ble:make-gatt-server :mtu 23))
        (blob (ble:make-octets 100)))
    (ble:gatt-add-service server #xFFE0)
    (let ((h (ble:gatt-add-characteristic server :uuid #xFFE1
                                                 :properties '(:read)
                                                 :value blob)))
      (let* ((lb (make-loopback server))
             (value (ble:att-read-value (loopback-client lb) h)))
        (is (= 22 (length value))
            "a plain read yields at most MTU-1 octets")
        (is (ble:value-may-be-truncated-p value 23)
            "and the client can tell that it was truncated")))))

(test a-long-read-uses-the-negotiated-mtu-not-the-advertised-one
  "Found over the air, not here: the suite's long-read test never called
ATT-EXCHANGE-MTU, so the client's idea of the MTU happened to match the
server's. Against a peer that answers 23 while we ask for 247, sizing by what
we advertised makes the first 22-octet response look short of the 246 that
would suggest more to come -- so the read stopped, returned 22 octets of a
300-octet attribute, and reported no error at all."
  (let ((server (ble:make-gatt-server :mtu 23))
        (blob (ble:make-octets 300))
        (ble:*att-rx-mtu* 23))
    (dotimes (i 300) (setf (aref blob i) (mod i 251)))
    (ble:gatt-add-service server #xFFE0)
    (let* ((h (ble:gatt-add-characteristic server :uuid #xFFE1
                                                  :properties '(:read)
                                                  :value blob))
           (lb (make-loopback server))
           (chan (loopback-client lb)))
      ;; ask for far more than the server will agree to
      (let ((agreed (ble:att-exchange-mtu chan 247)))
        (is (= 23 agreed) "the peer's smaller number is what is agreed")
        (is (= 23 (ble:att-mtu chan))
            "and it is remembered against the channel")
        (is (= 247 ble:*att-rx-mtu*)
            "while the advertised value stays what we advertised"))
      (is (equalp blob (ble:att-read-long-value chan h))
          "the whole 300 octets, sized by the agreed MTU"))))

(test the-negotiated-mtu-is-per-channel-and-dropped-on-close
  (let* ((server (ble:make-gatt-server :mtu 23))
         (lb (make-loopback server))
         (chan (loopback-client lb)))
    (ble:gatt-add-service server #xFFE0)
    (is (= 23 (ble:att-mtu chan))
        "23 until an exchange says otherwise, as ATT requires")
    (ble:att-exchange-mtu chan 247)
    (is (= 23 (ble:att-mtu chan)))
    (ble:att-forget-mtu chan)
    (is (= 23 (ble:att-mtu chan))
        "and back to the default once forgotten")))

;;; --- long writes: Prepare/Execute on the server -------------------------

(defun long-write-server (&key (mtu 23) on-write)
  (let ((server (ble:make-gatt-server :mtu mtu)))
    (ble:gatt-add-service server #xFFE0)
    (values server
            (ble:gatt-add-characteristic server :uuid #xFFE5
                                                :properties '(:read :write)
                                                :value (ble:make-octets 0)
                                                :on-write on-write))))

(test a-long-write-arrives-whole
  "300 octets through Prepare Write and Execute Write, then read back. At MTU
23 a plain Write Request carries 20, so this only completes if both ends
fragment and reassemble correctly."
  (multiple-value-bind (server h) (long-write-server)
    (let* ((lb (make-loopback server))
           (chan (loopback-client lb))
           (payload (ble:make-octets 300)))
      (dotimes (i 300) (setf (aref payload i) (mod (* i 7) 251)))
      (ble:att-exchange-mtu chan 247)
      (is (eq t (ble:att-write-long-value chan h payload))
          "the execute is acknowledged")
      (is (equalp payload (ble:att-read-long-value chan h))
          "and every octet of it is what a later read returns"))))

(test nothing-takes-effect-until-the-execute
  "The point of the queue: a client that gives up midway must leave the
attribute exactly as it was."
  (multiple-value-bind (server h) (long-write-server)
    (let* ((lb (make-loopback server))
           (chan (loopback-client lb)))
      (ble:gatt-set-value server h #(1 2 3))
      (ble:att-prepare-write chan h 0 #(9 9 9))
      (ble:att-prepare-write chan h 3 #(8 8 8))
      (is (equalp #(1 2 3)
                  (ble:gatt-attribute-value (ble:gatt-find-attribute server h)))
          "queued fragments must not be visible")
      (ble:att-execute-write chan :cancel t)
      (is (equalp #(1 2 3)
                  (ble:gatt-attribute-value (ble:gatt-find-attribute server h)))
          "and a cancelled queue must leave it untouched"))))

(test a-cancelled-queue-does-not-leak-into-the-next-write
  (multiple-value-bind (server h) (long-write-server)
    (let* ((lb (make-loopback server))
           (chan (loopback-client lb)))
      (ble:att-prepare-write chan h 0 #(9 9 9))
      (ble:att-execute-write chan :cancel t)
      (ble:att-prepare-write chan h 0 #(4 4))
      (ble:att-execute-write chan)                        ; commit
      (is (equalp #(4 4)
                  (ble:gatt-attribute-value (ble:gatt-find-attribute server h)))
          "only the second queue may be committed"))))

(test a-hook-sees-the-whole-value-not-a-fragment
  "A characteristic that validates its value has to be shown all of it, or it
would refuse a perfectly good long write on the strength of its first 20
octets."
  (let ((seen '()))
    (multiple-value-bind (server h)
        (long-write-server :on-write (lambda (s a v)
                                       (declare (ignore s a))
                                       (push (length v) seen)
                                       (if (= 100 (length v)) nil #x0D)))
      (let* ((lb (make-loopback server))
             (chan (loopback-client lb))
             (payload (ble:make-octets 100)))
        (ble:att-exchange-mtu chan 247)
        (is (eq t (ble:att-write-long-value chan h payload))
            "the hook accepts the assembled value")
        (is (equal '(100) seen)
            "and was called exactly once, with the whole 100 octets")))))

(test a-refused-long-write-leaves-the-attribute-alone
  (multiple-value-bind (server h)
      (long-write-server :on-write (lambda (s a v) (declare (ignore s a v)) #x0D))
    (let* ((lb (make-loopback server))
           (chan (loopback-client lb)))
      (ble:gatt-set-value server h #(7 7))
      (ble:att-exchange-mtu chan 247)
      (let ((r (ble:att-write-long-value chan h (ble:make-octets 60))))
        (is (= #x0D r) "the hook's code reaches the client"))
      (is (equalp #(7 7)
                  (ble:gatt-attribute-value (ble:gatt-find-attribute server h)))
          "and the refused value was never committed"))))

(test the-prepare-queue-is-bounded
  (multiple-value-bind (server h) (long-write-server)
    (let* ((lb (make-loopback server))
           (chan (loopback-client lb))
           (ble:*max-prepared-writes* 3)
           (codes '()))
      (dotimes (i 5)
        (push (nth-value 1 (ble:att-prepare-write chan h (* i 2) #(1 2))) codes))
      (is (member #x09 codes)
          "a queue that will not fit must answer Prepare Queue Full"))))

(test a-write-past-the-maximum-attribute-length-is-refused
  (multiple-value-bind (server h) (long-write-server)
    (let* ((lb (make-loopback server))
           (chan (loopback-client lb)))
      (is (= #x0D (nth-value 1 (ble:att-prepare-write chan h 510 #(1 2 3 4 5))))
          "512 is the ATT maximum; a fragment ending past it is invalid"))))

(test prepare-write-echoes-the-fragment-back
  "The echo is what lets a client verify a reliable write fragment by
fragment, so it has to be the value, not just an acknowledgement."
  (multiple-value-bind (server h) (long-write-server)
    (let* ((lb (make-loopback server))
           (chan (loopback-client lb))
           (echo (ble:att-prepare-write chan h 4 #(#xAA #xBB))))
      (is (= h (ble:u16-le echo 0)) "the echo names the handle")
      (is (= 4 (ble:u16-le echo 2)) "and the offset")
      (is (equalp #(#xAA #xBB) (subseq echo 4))
          "and the queued octets come back verbatim"))))

(test an-unwritable-attribute-refuses-a-prepare
  (multiple-value-bind (server) (demo-server)
    (let* ((lb (make-loopback server))
           (chan (loopback-client lb)))
      ;; handle 3 is the read-only manufacturer string
      (is (= #x03 (nth-value 1 (ble:att-prepare-write chan 3 0 #(1))))
          "write-not-permitted, at prepare time rather than at execute"))))

;;; --- connection parameter units -----------------------------------------
;;;
;;; The controller carries intervals in 1.25 ms units and supervision timeouts
;;; in 10 ms ones. Mixing them up produces a link that works but runs an order
;;; of magnitude away from what was asked for, which is invisible until you
;;; measure it -- so the conversions get their own checks.

(test connection-interval-units-round-trip
  (is (= 24 (ble:ms-to-interval-units 30)) "30 ms is 24 units, not 30")
  (is (= 30 (ble:interval-units-to-ms 24)) "and back again")
  (is (= 6 (ble:ms-to-interval-units 1))
      "clamped up to the 7.5 ms minimum the spec allows")
  (is (= #x0C80 (ble:ms-to-interval-units 100000))
      "and down to the maximum")
  (is (= 40 (ble:ms-to-interval-units 50))))

(test supervision-timeout-units-round-trip
  (is (= 400 (ble:ms-to-timeout-units 4000)) "4 s is 400 units of 10 ms")
  (is (= 4000 (ble:timeout-units-to-ms 400)))
  (is (= 5000 (ble:timeout-units-to-ms 500))
      "500 units is five seconds, which is the number that trips people up")
  (is (= 10 (ble:ms-to-timeout-units 1)) "clamped to the minimum"))

;;; --- L2CAP signalling ---------------------------------------------------
;;;
;;; The wire format is pure, so it gets checked here; the exchange itself
;;; needs two radios and lives in tools/live-two-radios/.

(test a-connection-parameter-request-decodes-to-milliseconds
  "The request carries intervals in 1.25 ms units and the timeout in 10 ms
ones, which is the same trap as everywhere else on this boundary."
  (let ((frame (ble:make-octets 12)))
    (setf (aref frame 0) #x12)          ; Connection Parameter Update Request
    (setf (aref frame 1) 7)             ; identifier
    (ble:u16le-put frame 2 8)           ; length
    (ble:u16le-put frame 4 (ble:ms-to-interval-units 100))
    (ble:u16le-put frame 6 (ble:ms-to-interval-units 200))
    (ble:u16le-put frame 8 4)           ; latency
    (ble:u16le-put frame 10 (ble:ms-to-timeout-units 6000))
    (multiple-value-bind (min-ms max-ms latency timeout-ms)
        (ble:parse-conn-param-request frame)
      (is (= 100 min-ms))
      (is (= 200 max-ms))
      (is (= 4 latency))
      (is (= 6000 timeout-ms) "six seconds, not six hundred"))))

(test a-frame-that-is-not-a-parameter-request-is-not-decoded-as-one
  (let ((reject (ble:make-octets 12)))
    (setf (aref reject 0) #x01)         ; Command Reject
    (is (null (ble:parse-conn-param-request reject))
        "only the request opcode may be parsed as a request"))
  (let ((truncated (ble:make-octets 6)))
    (setf (aref truncated 0) #x12)
    (is (null (ble:parse-conn-param-request truncated))
        "and a frame too short to hold four parameters is refused")))

;;; --- connection-oriented channels: SDU reassembly -----------------------
;;;
;;; The framing, without a radio. Credits start high here so the automatic
;;; replenish (which would try to transmit) stays out of the way; the exchange
;;; itself is verified over two radios.

(defun coc-fixture (&key (credits 100))
  "A connection with one channel on it, and no socket."
  (let* ((conn (ble::make-hci-conn :handle 1 :acl-len 27))
         (coc (ble::%make-l2cap-coc :conn conn :scid #x0040 :dcid #x0041
                                    :peer-mtu 512 :peer-mps 96
                                    :tx-credits 10 :rx-credits credits)))
    (push (cons #x0040 coc) (ble::hci-conn-coc-channels conn))
    (values conn coc)))

(defun k-frame (payload &key sdu-length)
  "A K-frame: the first of an SDU carries its total length, the rest do not."
  (let ((payload (ble:coerce-octets payload)))
    (if sdu-length
        (let ((f (ble:make-octets (+ 2 (length payload)))))
          (ble:u16le-put f 0 sdu-length)
          (replace f payload :start1 2)
          f)
        payload)))

(test a-single-frame-sdu-is-delivered-whole
  (multiple-value-bind (conn coc) (coc-fixture)
    (ble::%coc-note-frame conn #x0040 (k-frame #(1 2 3) :sdu-length 3))
    (is (equalp #(1 2 3) (ble:l2cap-coc-recv coc :timeout-ms 0))
        "the payload, without its length prefix")))

(test an-sdu-split-across-frames-is-reassembled
  "Only the first frame carries the length; the rest are payload, and the
receiver knows it is done by counting rather than by any end marker."
  (multiple-value-bind (conn coc) (coc-fixture)
    (ble::%coc-note-frame conn #x0040 (k-frame #(1 2 3 4) :sdu-length 10))
    (is (null (ble:l2cap-coc-recv coc :timeout-ms 0))
        "an incomplete SDU must not be delivered")
    (ble::%coc-note-frame conn #x0040 (k-frame #(5 6 7 8)))
    (is (null (ble:l2cap-coc-recv coc :timeout-ms 0)))
    (ble::%coc-note-frame conn #x0040 (k-frame #(9 10)))
    (is (equalp #(1 2 3 4 5 6 7 8 9 10) (ble:l2cap-coc-recv coc :timeout-ms 0))
        "and all ten octets arrive in order once the last frame lands")))

(test two-sdus-back-to-back-do-not-run-together
  (multiple-value-bind (conn coc) (coc-fixture)
    (ble::%coc-note-frame conn #x0040 (k-frame #(1 1) :sdu-length 2))
    (ble::%coc-note-frame conn #x0040 (k-frame #(2 2 2) :sdu-length 3))
    (is (equalp #(1 1) (ble:l2cap-coc-recv coc :timeout-ms 0)))
    (is (equalp #(2 2 2) (ble:l2cap-coc-recv coc :timeout-ms 0))
        "the second SDU starts a fresh length, not a continuation")))

(test every-received-frame-costs-a-credit
  "Credits are the whole flow-control mechanism: one frame, one credit."
  (multiple-value-bind (conn coc) (coc-fixture :credits 100)
    (ble::%coc-note-frame conn #x0040 (k-frame #(1 2 3) :sdu-length 3))
    (is (= 99 (ble:l2cap-coc-rx-credits coc)))
    (ble::%coc-note-frame conn #x0040 (k-frame #(4) :sdu-length 1))
    (is (= 98 (ble:l2cap-coc-rx-credits coc))
        "each frame, not each SDU")))

(test a-frame-for-an-unknown-channel-is-ignored
  "A CID we never allocated is not ours; treating it as a channel would put
somebody else's octets into our stream."
  (multiple-value-bind (conn coc) (coc-fixture)
    (ble::%coc-note-frame conn #x007E (k-frame #(9 9) :sdu-length 2))
    (is (null (ble:l2cap-coc-recv coc :timeout-ms 0)))
    (is (= 100 (ble:l2cap-coc-rx-credits coc))
        "and costs no credit on a channel it does not belong to")))

(test an-sdu-larger-than-the-peer-said-it-would-take-is-refused
  (multiple-value-bind (conn coc) (coc-fixture)
    (declare (ignore conn))
    (is (eq :too-large
            (ble:l2cap-coc-send coc (ble:make-octets 513) :timeout-ms 0))
        "the peer advertised an MTU of 512; sending more is a protocol error,
not something to discover from the far end going quiet")))

;;; --- the Security Manager's crypto --------------------------------------

(test aes-cmac-matches-the-published-vectors
  "RFC 4493. Everything SMP derives is AES-CMAC underneath, so if this is
wrong nothing above it can be right -- and a wrong CMAC still produces
plausible-looking 16-octet values that agree with themselves."
  (let ((key (hex->octets "2b7e151628aed2a6abf7158809cf4f3c")))
    (is (equalp (hex->octets "bb1d6929e95937287fa37d129b756746")
                (ble:aes-cmac key #()))
        "the empty message")
    (is (equalp (hex->octets "070a16b46b4d4144f79bdd9dd04a287c")
                (ble:aes-cmac key (hex->octets "6bc1bee22e409f96e93d7e117393172a")))
        "one full block")
    (is (equalp (hex->octets "dfa66747de9ae63030ca32611497c827")
                (ble:aes-cmac key (hex->octets "6bc1bee22e409f96e93d7e117393172a~
                                                ae2d8a571e03ac9c9eb76fac45af8e51~
                                                30c81c46a35ce411")))
        "a message needing padding")))

(test msb-reverses-exactly-once
  "Wire order and crypto order differ, and every value crosses that boundary
once. Applying the reversal twice is the same as not applying it, which is
the shape the bug takes."
  (let ((v (hex->octets "0102030405")))
    (is (equalp (hex->octets "0504030201") (ble:msb v)))
    (is (equalp v (ble:msb (ble:msb v))) "twice is identity")))

(test f4-depends-on-every-input
  "Structural, not a published vector: each argument must reach the result,
because a confirm value that ignores one of them would still match on both
sides and still be wrong."
  (let* ((u (ble:make-octets 32)) (v (ble:make-octets 32))
         (x (ble:make-octets 16))
         (base (ble:smp-f4 u v x 0)))
    (is (= 16 (length base)) "a confirm value is 16 octets")
    (is (equalp base (ble:smp-f4 u v x 0)) "and is deterministic")
    (let ((u2 (copy-seq u))) (setf (aref u2 0) 1)
      (is (not (equalp base (ble:smp-f4 u2 v x 0))) "U reaches the result"))
    (let ((v2 (copy-seq v))) (setf (aref v2 31) 1)
      (is (not (equalp base (ble:smp-f4 u v2 x 0))) "V reaches the result"))
    (let ((x2 (copy-seq x))) (setf (aref x2 0) 1)
      (is (not (equalp base (ble:smp-f4 u v x2 0))) "the nonce reaches it"))
    (is (not (equalp base (ble:smp-f4 u v x 1))) "and so does Z")))

(test f5-derives-two-different-keys-from-one-secret
  "The counter octet is what separates the MacKey from the LTK. Without it
they would be identical, and the check values would be computed with the
very key they are supposed to be protecting."
  (let ((dh (ble:make-octets 32)) (n1 (ble:make-octets 16))
        (n2 (ble:make-octets 16)) (a1 (ble:make-octets 7))
        (a2 (ble:make-octets 7)))
    (multiple-value-bind (mackey ltk) (ble:smp-f5 dh n1 n2 a1 a2)
      (is (= 16 (length mackey)))
      (is (= 16 (length ltk)))
      (is (not (equalp mackey ltk)) "they must not be the same key"))
    ;; the addresses are bound into the key, which is what stops one derived
    ;; on one pair of devices being replayed against another
    (let ((a1b (copy-seq a1)))
      (setf (aref a1b 3) #xAA)
      (is (not (equalp (nth-value 1 (ble:smp-f5 dh n1 n2 a1 a2))
                       (nth-value 1 (ble:smp-f5 dh n1 n2 a1b a2))))
          "changing an address changes the LTK"))))

(test f5-is-not-symmetric-in-its-nonces
  "Na and Nb are not interchangeable; if they were, a reflected transcript
would derive the same key."
  (let ((dh (ble:make-octets 32)) (a (ble:make-octets 7)))
    (let ((n1 (ble:make-octets 16)) (n2 (ble:make-octets 16)))
      (setf (aref n1 0) 1 (aref n2 0) 2)
      (is (not (equalp (nth-value 1 (ble:smp-f5 dh n1 n2 a a))
                       (nth-value 1 (ble:smp-f5 dh n2 n1 a a))))))))

(test f6-and-g2-produce-the-right-shapes
  (let ((k (ble:make-octets 16)) (n (ble:make-octets 16))
        (r (ble:make-octets 16)) (io (ble:make-octets 3))
        (a (ble:make-octets 7)) (u (ble:make-octets 32)))
    (is (= 16 (length (ble:smp-f6 k n n r io a a))) "a check value is 16 octets")
    (let ((digits (ble:smp-g2 u u n n)))
      (is (integerp digits))
      (is (< digits 1000000) "numeric comparison shows six digits"))))

(test p256-shared-secrets-agree-from-both-sides
  "The property that makes ECDH ECDH. Done in software because the RTL8761B
controllers here answer LE Generate DHKey with a fixed public key and a
shared secret that is neither correct nor stable."
  (multiple-value-bind (a-priv a-x a-y) (ble:smp-generate-keypair)
    (multiple-value-bind (b-priv b-x b-y) (ble:smp-generate-keypair)
      (is (= 32 (length a-x)) "coordinates are 32 octets")
      (is (not (equalp a-x b-x)) "and two keypairs differ")
      (is (equalp (ble:smp-dhkey a-priv b-x b-y)
                  (ble:smp-dhkey b-priv a-x a-y))
          "each side computes the same shared secret from the other's key"))))
