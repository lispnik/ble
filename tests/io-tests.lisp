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

(test characteristic-discovery-defaults-to-the-whole-handle-space
  "Membership is the handle range and nothing else -- ATT has no other notion
of which service a characteristic belongs to, so the range has to reach the
request. Defaulting to 1..FFFF keeps the common call unchanged."
  (let ((ll (sb-introspect:function-lambda-list 'ble:att-discover-characteristics)))
    (is (equal '(fd &key (start 1) (end 65535))
               (mapcar (lambda (x) (if (consp x) (list (intern (string (first x)))
                                                       (second x))
                                       (intern (string x))))
                       ll))
        "signature drifted: ~A" ll)))

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
