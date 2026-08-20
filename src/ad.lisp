(in-package #:ble)

;;; Advertising-data records.
;;;
;;; An advertising payload is a stream of [length][type][data] records. That
;;; container is generic BLE; what a vendor puts inside one of them is not.
;;;
;;; Portable, and deliberately so. This is arithmetic over an octet vector
;;; with no I/O in it, and a consumer's packet parser needs it -- so if it
;;; lived with the HCI socket code, portable protocol code would have to
;;; depend on the Linux-only system just to read a packet out of a capture
;;; file. It belongs in ble/core.

(defconstant +ad-type-manufacturer-specific-data+ #xFF)

(defun extract-manufacturer-data
    (adv-data &key company-id)
  "Walk a BLE advertising data blob -- a stream of AD records of the form
[length][type][...data...] -- and return the first manufacturer-specific
payload, or NIL if there is none. The returned octet vector starts
immediately after the 16-bit company identifier.

COMPANY-ID, when given, restricts the search to that vendor. When NIL any
manufacturer record matches, which is what you want when you do not yet know
whose device you are looking at."
  (let ((data (coerce-octets adv-data))
        (i 0))
    (loop while (< i (length data)) do
      (let ((rec-length (aref data i)))
        (when (zerop rec-length)
          (return-from extract-manufacturer-data nil))
        (let* ((payload-start (+ i 2))
               (payload-end (+ i 1 rec-length)))
          (when (> payload-end (length data))
            (return-from extract-manufacturer-data nil))
          (when (and (= (aref data (1+ i)) +ad-type-manufacturer-specific-data+)
                     (>= rec-length 3))
            (let ((cid (logior (aref data payload-start)
                               (ash (aref data (1+ payload-start)) 8))))
              (when (or (null company-id) (= cid company-id))
                (return-from extract-manufacturer-data
                  (subseq data (+ payload-start 2) payload-end)))))
          (setf i payload-end))))
    nil))

(defun map-ad-records (data fn)
  "Call FN with (TYPE VALUE-OCTETS) for each AD record in DATA."
  (loop with i = 0
        while (< i (length data))
        for len = (aref data i)
        while (and (plusp len) (<= (+ i 1 len) (length data)))
        do (funcall fn (aref data (1+ i)) (subseq data (+ i 2) (+ i 1 len)))
           (incf i (1+ len))))

(defconstant +ad-type-shortened-local-name+ #x08)
(defconstant +ad-type-complete-local-name+  #x09)
(defconstant +ad-type-incomplete-uuids-16+  #x02)
(defconstant +ad-type-complete-uuids-16+    #x03)

(defun adv-local-name (data)
  "The complete (0x09) or shortened (0x08) local name in DATA, or NIL."
  (let (name)
    (map-ad-records data
                    (lambda (type value)
                      (when (and (member type (list +ad-type-shortened-local-name+
                                                    +ad-type-complete-local-name+))
                                 (null name))
                        (setf name (map 'string #'code-char value)))))
    name))

(defun adv-service-uuids-16 (data)
  "The 16-bit service UUIDs advertised in DATA (AD types 0x02 / 0x03)."
  (let (uuids)
    (map-ad-records data
                    (lambda (type value)
                      (when (member type (list +ad-type-incomplete-uuids-16+
                                               +ad-type-complete-uuids-16+))
                        (loop for i from 0 below (1- (length value)) by 2
                              do (push (u16-le value i) uuids)))))
    (nreverse uuids)))
