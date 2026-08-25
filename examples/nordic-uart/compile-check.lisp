;;;; Load the UART device and check it, without a radio.
;;;;
;;;; The 128-bit UUIDs are what this example is for, so they are what gets
;;;; asserted hardest: written one way, sent the other, and a transposition
;;;; between the two is invisible to every other kind of test.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename "./")) :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble/examples))

(defvar *problems* 0)
(defun check (ok fmt &rest args)
  (format t "~&[~A] ~?~%" (if ok " ok " "FAIL") fmt args)
  (unless ok (incf *problems*)))

;; --- 128-bit UUIDs ------------------------------------------------------
;;
;; The wire form is the reverse of the written form. Getting that backwards
;; produces a service nobody can find, and looks perfectly fine in a hex dump.
(let ((u (ble:uuid128 "6E400001-B5A3-F393-E0A9-E50E24DCCA9E")))
  (check (= 16 (length u)) "a 128-bit UUID is sixteen octets")
  (check (= #x9E (aref u 0))
         "the LAST octet written is the FIRST on the wire")
  (check (= #x6E (aref u 15)) "and the first written is the last")
  (check (string= "6E400001-B5A3-F393-E0A9-E50E24DCCA9E" (ble:uuid-string u))
         "and UUID-STRING is its exact inverse"))

(check (equalp (ble:uuid128 "6E400002B5A3F393E0A9E50E24DCCA9E")
               (ble:uuid128 "6E400002-B5A3-F393-E0A9-E50E24DCCA9E"))
       "the dashes are optional, as they are in every document that has them")
(check (handler-case (progn (ble:uuid128 "6E400002") nil) (error () t))
       "and a short one signals rather than padding into a different UUID")

;; The library's own NUS constants, which the device below is built from.
(check (= #x02 (aref ble:+nus-rx-uuid-le+ 12))
       "NUS RX is ...0002... -- byte 12 on the wire, being little-endian")
(check (= #x03 (aref ble:+nus-tx-uuid-le+ 12)) "and TX is ...0003...")
(check (= #x01 (aref ble:+nus-service-uuid-le+ 12))
       "and the service itself is ...0001...")
(check (and (equalp (subseq ble:+nus-rx-uuid-le+ 0 12)
                    (subseq ble:+nus-tx-uuid-le+ 0 12))
            (equalp (subseq ble:+nus-rx-uuid-le+ 13)
                    (subseq ble:+nus-tx-uuid-le+ 13)))
       "the three differ in exactly one octet, which is the whole family")

;; --- the terminal -------------------------------------------------------
(check (search "help" (nordic-uart:handle-line "help"))
       "help lists itself")
(check (string= "hello" (nordic-uart:handle-line "echo hello"))
       "echo returns what follows it")
(check (string= "" (nordic-uart:handle-line "   "))
       "blank input says nothing rather than complaining")
(check (search "no such command" (nordic-uart:handle-line "wat"))
       "and an unknown command says so")
(check (string= "hello" (nordic-uart:handle-line "  echo hello  "))
       "leading and trailing whitespace is trimmed -- a terminal peer sends ~
        line endings and they are not part of the command")
(check (search "5 second" (nordic-uart:handle-line "uptime" :uptime 5))
       "uptime reports what it is given")

;; --- the database -------------------------------------------------------
(let* ((term (nordic-uart:build-server))
       (server (slot-value term 'nordic-uart::server))
       (services (mapcar #'ble:uuid-string
                         (mapcar #'ble:gatt-service-entry-uuid
                                 (reverse (ble:gatt-server-services server))))))
  (check (equal '("1800" "1801" "6E400001-B5A3-F393-E0A9-E50E24DCCA9E") services)
         "two 16-bit services and one 128-bit one, in that order")
  (let ((uuids (loop for a across (ble:gatt-server-attributes server)
                     collect (ble:uuid-string (ble:gatt-attribute-uuid a)))))
    (check (member "6E400002-B5A3-F393-E0A9-E50E24DCCA9E" uuids :test #'string=)
           "RX is present")
    (check (member "6E400003-B5A3-F393-E0A9-E50E24DCCA9E" uuids :test #'string=)
           "and TX"))
  ;; The direction that catches everybody: RX is what the DEVICE receives.
  (let ((rx (ble:gatt-find-attribute
             server (slot-value term 'nordic-uart::rx-handle)))
        (tx (ble:gatt-find-attribute
             server (slot-value term 'nordic-uart::tx-handle))))
    (check (member :write (ble:gatt-attribute-permissions rx))
           "the host WRITES to RX, because RX is what the device receives")
    (check (member :write-without-response (ble:gatt-attribute-permissions rx))
           "and may do so unacknowledged, which is what makes it feel serial")
    (check (not (member :notify (ble:gatt-attribute-permissions rx)))
           "RX does not notify")
    (check (member :notify (ble:gatt-attribute-permissions tx))
           "TX notifies, because TX is what the device transmits")
    (check (not (intersection '(:write :write-without-response)
                              (ble:gatt-attribute-permissions tx)))
           "and is not written to")
    (check (ble:gatt-find-attribute
            server (1+ (slot-value term 'nordic-uart::tx-handle)))
           "with a CCCD after it to subscribe on"))

  ;; A write leaves the line for the tick to answer rather than replying from
  ;; inside the handler.
  (let* ((rx (ble:gatt-find-attribute
              server (slot-value term 'nordic-uart::rx-handle)))
         (write (ble:gatt-attribute-on-write rx)))
    (check (null (funcall write server rx
                          (map '(simple-array (unsigned-byte 8) (*))
                               #'char-code "help")))
           "a write is accepted with no error")
    (check (equal '("help") (slot-value term 'nordic-uart::pending))
           "and the line is queued for the tick, not answered in the handler")))

(check (find-package :nordic-uart-client) "the client half loads too")

(format t "~&~%NORDIC UART CHECK: ~:[clean~;~:*~D problem(s)~]~%" *problems*)
(sb-ext:exit :code (if (zerop *problems*) 0 1))
