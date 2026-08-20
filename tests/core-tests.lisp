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
  ;; 0x09 complete local name "UD18"
  (is (string= "UD18" (ble:adv-local-name (hex->octets "0509" "55443138"))))
  ;; 0x08 shortened
  (is (string= "UD" (ble:adv-local-name (hex->octets "0308" "5544"))))
  (is (null (ble:adv-local-name (hex->octets "020106")))))

(test service-uuids-are-read-little-endian
  ;; 0x03 complete list of 16-bit UUIDs: FFE0
  (is (equal '(#xFFE0) (ble:adv-service-uuids-16 (hex->octets "0303" "E0FF"))))
  (is (equal '(#xFFE0 #x180A)
             (ble:adv-service-uuids-16 (hex->octets "0503" "E0FF" "0A18")))))

;;; --- MAC byte order ----------------------------------------------------
;;;
;;; Moved here from bledecode when ble became its own library: parse/format
;;; are this library's, so their tests are too.

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
