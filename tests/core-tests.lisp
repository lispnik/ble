(in-package #:ble/tests)

(in-suite ble-core)

;;; --- octets ------------------------------------------------------------

(test coerce-octets-always-returns-a-fresh-simple-vector
  "The contract is a copy every time, even when the input is already an octet
vector. That costs an allocation on a path that could alias instead, and it
buys the guarantee callers actually rely on: what you get back is yours to
mutate and nobody else holds a reference to it."
  (let ((already (octets 1 2 3)))
    (is (equalp already (ble:coerce-octets already)))
    (is (not (eq already (ble:coerce-octets already)))
        "a copy, not the same vector")
    (let ((copy (ble:coerce-octets already)))
      (setf (aref copy 0) 99)
      (is (= 1 (aref already 0)) "mutating the copy must not touch the original")))
  (is (equalp (octets 1 2 3) (ble:coerce-octets '(1 2 3))))
  (is (typep (ble:coerce-octets '(1 2 3)) 'ble:octets)))

(test little-endian-readers-and-writers-round-trip
  (let ((v (ble:make-octets 4)))
    (ble:u16le-put v 0 #xBEEF)
    (is (= #xBEEF (ble:u16-le v 0)))
    (is (equalp (octets #xEF #xBE) (subseq v 0 2))
        "low byte first"))
  (let ((v (ble:make-octets 3)))
    (ble:u24le-put v 0 #x123456)
    (is (equalp (octets #x56 #x34 #x12) v)))
  (is (= -2 (ble:s16-le (octets #xFE #xFF) 0))
      "signed reader should sign-extend"))

;;; --- advertising-data records -----------------------------------------

(test extract-manufacturer-data-matches-any-vendor-by-default
  ;; one AD record: length 5, type FF, company 004C (Apple), payload DE AD
  (let ((blob (hex->octets "05FF4C00DEAD")))
    (is (equalp (hex->octets "DEAD") (ble:extract-manufacturer-data blob)))
    (is (equalp (hex->octets "DEAD")
                (ble:extract-manufacturer-data blob :company-id #x004C)))
    (is (null (ble:extract-manufacturer-data blob :company-id #x0292))
        "a different company id must not match")))

(test extract-manufacturer-data-walks-past-other-records
  ;; flags record first, then the manufacturer one
  (let ((blob (hex->octets "020106" "05FF9202BEEF")))
    (is (equalp (hex->octets "BEEF")
                (ble:extract-manufacturer-data blob :company-id #x0292)))))

(test extract-manufacturer-data-survives-malformed-input
  (is (null (ble:extract-manufacturer-data (hex->octets "00"))) "zero length terminates")
  (is (null (ble:extract-manufacturer-data (hex->octets "FFFF")))
      "a record claiming more bytes than exist must not read past the end")
  (is (null (ble:extract-manufacturer-data (ble:make-octets 0)))))

(test local-name-prefers-nothing-and-accepts-either-type
  ;; 0x09 complete local name "NODE"
  (is (string= "NODE" (ble:adv-local-name (hex->octets "0509" "4E4F4445"))))
  ;; 0x08 shortened
  (is (string= "NO" (ble:adv-local-name (hex->octets "0308" "4E4F"))))
  (is (null (ble:adv-local-name (hex->octets "020106")))))

(test service-uuids-are-read-little-endian
  ;; 0x03 complete list of 16-bit UUIDs: FFE0
  (is (equal '(#xFFE0) (ble:adv-service-uuids-16 (hex->octets "0303" "E0FF"))))
  (is (equal '(#xFFE0 #x180A)
             (ble:adv-service-uuids-16 (hex->octets "0503" "E0FF" "0A18")))))

;;; --- MAC byte order ----------------------------------------------------
;;;
;;; Display order is MSB-first; the wire is LSB-first. Getting it backwards
;;; silently targets a different device rather than failing, so the round trip
;;; is worth asserting in both directions.

(test parse-mac-on-air-order
  ;; display MSB-first -> on-air LSB-first (the reverse). This is the exact
  ;; case that caused real confusion: the display address CD:52:C6:52:53:D1
  ;; goes on the wire as D1:53:52:C6:52:CD.
  (is (equalp (octets #xD1 #x53 #x52 #xC6 #x52 #xCD)
              (ble:parse-mac "CD:52:C6:52:53:D1")))
  (is (equalp (octets #x66 #x55 #x44 #x33 #x22 #x11)
              (ble:parse-mac "11:22:33:44:55:66"))))

(test format-mac-display-order
  ;; on-air LSB-first -> display MSB-first, upper-case, colon-separated
  (is (string= "CD:52:C6:52:53:D1"
               (ble:format-mac (octets #xD1 #x53 #x52 #xC6 #x52 #xCD))))
  (is (string= "11:22:33:44:55:66"
               (ble:format-mac (octets #x66 #x55 #x44 #x33 #x22 #x11))))
  ;; lower-case / single-digit octets are zero-padded and upcased
  (is (string= "0A:0B:0C:0D:0E:0F"
               (ble:format-mac (octets #x0F #x0E #x0D #x0C #x0B #x0A)))))

(test mac-round-trip
  ;; format . parse = identity (display), parse . format = identity (bytes)
  (dolist (s '("CD:52:C6:52:53:D1" "11:22:33:44:55:66" "AA:BB:CC:DD:EE:FF"
               "00:00:00:00:00:00" "0A:0B:0C:0D:0E:0F"))
    (is (string= s (ble:format-mac (ble:parse-mac s)))))
  (dolist (b (list (octets #xD1 #x53 #x52 #xC6 #x52 #xCD)
                   (octets 1 2 3 4 5 6)
                   (octets #xFF #xFF #xFF #xFF #xFF #xFF)))
    (is (equalp b (ble:parse-mac (ble:format-mac b))))))

(test parse-mac-rejects-bad-input
  (signals error (ble:parse-mac "11:22:33:44:55"))       ; too few
  (signals error (ble:parse-mac "11:22:33:44:55:66:77")) ; too many
  (signals error (ble:parse-mac "zz:22:33:44:55:66")))   ; non-hex

(test signed-octets-read-as-signed
  "RSSI and transmit power are signed. Reading one unsigned is the difference
between -60 dBm and a cheerful 196."
  (is (= 0 (ble:s8 #(0) 0)))
  (is (= 127 (ble:s8 #(127) 0)) "the top of the positive range")
  (is (= -128 (ble:s8 #(128) 0)) "and the bottom of the negative one")
  (is (= -1 (ble:s8 #(255) 0)))
  (is (= -60 (ble:s8 #(0 #xC4) 1)) "a plausible connection RSSI"))

(test a-static-random-address-has-both-top-bits-set
  "The two most significant bits identify the address as static random, and
on-air order puts the most significant octet last -- the usual place to get
this wrong."
  (let ((a (ble:static-random-address #(1 2 3 4 5 6))))
    (is (= #xC6 (aref a 5)) "the top two bits are forced on, the rest kept")
    (is (equalp #(1 2 3 4 5) (subseq a 0 5)) "and nothing else is touched")
    (is-true (ble:static-random-address-p a)))
  (is-false (ble:static-random-address-p #(1 2 3 4 5 6))
            "an address without them is not one")
  (is-false (ble:static-random-address-p (ble:static-random-address
                                          (ble:make-octets 6)))
            "all-zero is reserved, even with the bits set"))

;;; --- building advertising payloads --------------------------------------
;;;
;;; Round-tripped through the parsers in the same file. That symmetry is the
;;; reason the builder lives here: if the two disagree, one of them is wrong,
;;; and a payload that parses back to what went in is the strongest statement
;;; either can make without a radio.

(test an-advertising-payload-parses-back-to-what-went-in
  (let ((data (ble:adv-data :flags '(:general-discoverable :no-bredr)
                            :name "HR Sensor"
                            :services-16 '(#x180D))))
    (is (string= "HR Sensor" (ble:adv-local-name data))
        "the name survives the round trip")
    (is (equal '(#x180D) (ble:adv-service-uuids-16 data))
        "and so does the service UUID, which is what makes it findable")))

(test several-service-uuids-go-in-one-record
  (let ((data (ble:adv-data :services-16 '(#x180D #x180A #x1801))))
    (is (equal '(#x180D #x180A #x1801) (ble:adv-service-uuids-16 data)))
    (is (= 8 (length data)) "one record: length, type, and three UUIDs")))

(test each-record-is-length-prefixed-and-counts-its-own-type
  "The length octet covers the type as well as the value -- off by one here
produces a payload that parses as garbage from the second record onward."
  (let ((data (ble:adv-data :flags :general-discoverable)))
    (is (= 3 (length data)))
    (is (= 2 (aref data 0)) "length is 2: the type octet plus one flag octet")
    (is (= #x01 (aref data 1)) "AD type Flags")
    (is (= #x02 (aref data 2)) "general discoverable")))

(test a-payload-that-will-not-fit-is-refused
  "31 octets is not much, and an overrun is silent on the wire: the device
simply becomes one nobody can see."
  (signals error (ble:adv-data :name "a name far too long to fit in a legal
                                      advertising payload alongside anything"))
  ;; the same content fits when the caller knows it is going in a scan response
  (is-true (ble:adv-data :name "0123456789012345678901234567890123456789"
                         :max-length 62)))

(test manufacturer-data-needs-a-company-id
  (signals error (ble:adv-data :manufacturer #(1 2 3)))
  (let ((data (ble:adv-data :manufacturer #(#xDE #xAD) :company-id #x004C)))
    (is (equalp #(#xDE #xAD) (ble:extract-manufacturer-data data))
        "and parses back through the manufacturer-data reader")))

(test appearance-is-little-endian-like-everything-else
  (let ((data (ble:adv-data :appearance #x0341)))
    (is (= #x19 (aref data 1)) "AD type Appearance")
    (is (= #x41 (aref data 2)))
    (is (= #x03 (aref data 3)) "0x0341, low octet first")))

(test an-unknown-flag-is-an-error-rather-than-a-silent-zero
  (signals error (ble:adv-data :flags '(:general-discoverable :nonsense))))
