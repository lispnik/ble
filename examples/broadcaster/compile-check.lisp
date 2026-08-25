;;;; Load the beacon and check its payload, without a radio.
;;;;
;;;; The payload is the whole device -- there is no database and no
;;;; conversation -- so it is entirely checkable here, which makes this the
;;;; example with the largest fraction of itself under test and the smallest
;;;; amount of it proven by running. What running proves is the part no test
;;;; can: that a Coded PHY advertisement is actually heard.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename "./")) :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble/examples))

(defvar *problems* 0)
(defun check (ok fmt &rest args)
  (format t "~&[~A] ~?~%" (if ok " ok " "FAIL") fmt args)
  (unless ok (incf *problems*)))

;; --- the 31-octet limit, asserted rather than described -----------------
(check (handler-case (progn (broadcaster:legacy-payload) nil) (error () t))
       "this payload does NOT fit in a legacy advertisement")

(let ((p (broadcaster:payload)))
  (check (> (length p) 31)
         "which is unsurprising: it is ~D octets, and legacy has 31"
         (length p))
  (check (<= (length p) 251) "and it does fit an extended set's 251"))

;; What costs what, so the arithmetic in the header comment is checked and
;; not merely asserted.
(let ((with (length (ble:adv-data :flags '(:general-discoverable)
                                  :services-128 (list ble:+nus-service-uuid-le+)
                                  :max-length 251)))
      (without (length (ble:adv-data :flags '(:general-discoverable)
                                     :max-length 251))))
  (check (= 18 (- with without))
         "one 128-bit UUID costs 18 octets: sixteen of UUID and two of header")
  (check (> (+ without 18 2 (length "Lisp UART")) 31)
         "so a vendor UUID and a name together cannot fit in a legacy ~
          advertisement -- which is why the NUS example is found by name"))

;; --- the records are the ones claimed -----------------------------------
(let* ((p (broadcaster:payload :counter #x01020304))
       (records '())
       (i 0))
  ;; Walk the AD structure: length, type, value.
  (loop while (< i (length p))
        for len = (aref p i)
        while (plusp len)
        do (push (cons (aref p (1+ i)) (subseq p (+ i 2) (+ i 1 len))) records)
           (incf i (1+ len)))
  (setf records (nreverse records))
  (check (= i (length p)) "the payload parses exactly, with nothing left over")
  (check (assoc #x01 records) "there is a flags record")
  (check (assoc #x09 records) "and a complete local name")
  (check (assoc #x07 records)
         "and a COMPLETE list of 128-bit UUIDs (0x07), not the incomplete ~
          0x06 -- the two mean different things to a scanner")
  (check (equalp ble:+nus-service-uuid-le+ (cdr (assoc #x07 records)))
         "carrying the NUS service UUID, in wire order")
  (let ((m (cdr (assoc #xFF records))))
    (check m "and a manufacturer-specific record")
    (check (and (= #xFF (aref m 0)) (= #xFF (aref m 1)))
           "under 0xFFFF, the identifier the SIG reserves for testing -- not ~
            somebody else's company")
    (check (= #x01020304 (+ (aref m 2) (ash (aref m 3) 8)
                            (ash (aref m 4) 16) (ash (aref m 5) 24)))
           "with the counter little-endian behind it")))

;; The counter is what makes a watcher able to tell a live beacon from a
;; stale report, so it had better actually change the bytes.
(check (not (equalp (broadcaster:payload :counter 1)
                    (broadcaster:payload :counter 2)))
       "two counter values produce two different payloads")

;; --- the library half ---------------------------------------------------
(check (= #x07 ble:+ad-type-complete-uuids-128+)
       "the AD type is 0x07, checked against the SIG's ad_types registry")
(check (handler-case
           (progn (ble:adv-data :services-128 (list ble:+nus-service-uuid-le+)
                                :name "a name long enough to overflow the limit")
                  nil)
         (error () t))
       "adv-data still refuses to overrun 31 octets by default -- the extended ~
        payload is opt-in, so no existing peripheral silently grew one")

(format t "~&~%BROADCASTER CHECK: ~:[clean~;~:*~D problem(s)~]~%" *problems*)
(sb-ext:exit :code (if (zerop *problems*) 0 1))
