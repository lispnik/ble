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
(defconstant +ad-type-complete-uuids-128+   #x07)

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

;;; --- building an advertising payload ------------------------------------

(defconstant +ad-type-flags+       #x01)
(defconstant +ad-type-appearance+  #x19)

(defparameter +ad-flags+
  '((:limited-discoverable . #x01)
    (:general-discoverable . #x02)
    (:no-bredr             . #x04)   ; LE only, which is nearly always right
    (:le-bredr-controller  . #x08)
    (:le-bredr-host        . #x10)))

(defun %ad-record (type value)
  "One [length][type][value] record."
  (let* ((value (coerce-octets value))
         (out (make-octets (+ 2 (length value)))))
    (setf (aref out 0) (1+ (length value))     ; length covers type + value
          (aref out 1) type)
    (replace out value :start1 2)
    out))

(defun %ad-flag-bits (flags)
  (let ((bits 0))
    (dolist (f (if (listp flags) flags (list flags)) bits)
      (let ((bit (cdr (assoc f +ad-flags+))))
        (unless bit (error "unknown advertising flag ~S" f))
        (setf bits (logior bits bit))))))

(defun adv-data (&key flags name (name-complete t) services-16 services-128
                      appearance manufacturer company-id (max-length 31))
  "Build an advertising payload from the records a peripheral usually wants.

  (adv-data :flags '(:general-discoverable :no-bredr)
            :name \"HR Sensor\"
            :services-16 '(#x180D))

Signals if the result will not fit. That limit is the reason this exists as a
function rather than as a literal vector in each peripheral: 31 octets is not
much, the overrun is silent on the wire, and the failure looks like a device
nobody can see rather than like a payload one record too long.

SERVICES-128 takes UUIDs in ATT wire order -- BLE:UUID128 produces them --
and is where the 31-octet limit bites: one 128-bit UUID costs eighteen
octets, so a vendor service and a name together do not fit. Raise MAX-LENGTH
to 251 when advertising it through an extended advertising set.

Advertising the service UUID is what makes a peripheral findable by an app
that filters for it -- a heart rate monitor that omits 0x180D is invisible to
every heart rate app, however correct its GATT database."
  (let ((records '()))
    (when flags
      (push (%ad-record +ad-type-flags+ (vector (%ad-flag-bits flags))) records))
    (when services-16
      (let ((v (make-octets (* 2 (length services-16)))))
        (loop for uuid in services-16
              for i from 0 by 2
              do (u16le-put v i uuid))
        (push (%ad-record +ad-type-complete-uuids-16+ v) records)))
    (when services-128
      ;; Sixteen octets each, plus two of header. One of these and a name is
      ;; already most of a legacy advertisement, which is why vendor devices
      ;; are usually found by name and why MAX-LENGTH is worth raising when
      ;; the controller supports extended advertising.
      (push (%ad-record +ad-type-complete-uuids-128+
                        (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                               (mapcar #'coerce-octets services-128)))
            records))
    (when appearance
      (let ((v (make-octets 2)))
        (u16le-put v 0 appearance)
        (push (%ad-record +ad-type-appearance+ v) records)))
    (when name
      (push (%ad-record (if name-complete
                            +ad-type-complete-local-name+
                            +ad-type-shortened-local-name+)
                        (map '(simple-array (unsigned-byte 8) (*)) #'char-code name))
            records))
    (when manufacturer
      (let* ((data (coerce-octets manufacturer))
             (v (make-octets (+ 2 (length data)))))
        (unless company-id
          (error "adv-data: manufacturer data needs a company-id"))
        (u16le-put v 0 company-id)
        (replace v data :start1 2)
        (push (%ad-record +ad-type-manufacturer-specific-data+ v) records)))
    (let ((out (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                      (nreverse records))))
      (when (> (length out) max-length)
        (error "advertising data is ~D octets, which will not fit in ~D"
               (length out) max-length))
      out)))
