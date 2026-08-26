;;;; Read Multiple Variable Length (ATT 0x20) over the air, as the client.
;;;;
;;;; Run peripheral.lisp beside it and point this at its address:
;;;;
;;;;   PEER_MAC=CC:BA:... CENTRAL_DEV=2 sbcl --load read-multiple-variable.lisp
;;;;
;;;; Its Device Information service carries two readable strings of DIFFERENT
;;;; lengths, which is the only shape that shows what 0x20 is for. Observed:
;;;;
;;;;   0x20 returned 2 value(s): "ACME" "r1"
;;;;   0x0E returned 6 octet(s): "ACMEr2"
;;;;
;;;; Six octets, and nothing in them says where the first value stopped. That
;;;; is the whole argument for the newer opcode, in one line of output.
;;;;
;;;; WORTH RUNNING ON THE OLD DONGLES. 0x20 arrived in Bluetooth 5.2 and this
;;;; passes between two 5.1 controllers, because ATT is a host protocol: the
;;;; opcode rides in ACL payload the controller never inspects. The README
;;;; claimed for a long time that this could not be tested without newer
;;;; radios. It runs here on both, and that claim was simply wrong.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))
(defun env-or (n d) (let ((v (sb-ext:posix-getenv n)))
                      (if (and v (plusp (length v))) v d)))
(defun as-text (v) (map 'string #'code-char v))
(let ((peer (env-or "PEER_MAC" ""))
      (dev (parse-integer (env-or "CENTRAL_DEV" "1")))
      (fails 0) (checks 0))
  (flet ((check (ok fmt &rest args)
           (incf checks)
           (format t "~&  [~A] ~?~%" (if ok " ok " "FAIL") fmt args)
           (unless ok (incf fails))
           (force-output)))
    (ble:install-adapter-teardown)
    (ble:with-att-channel
        (chan (ble:hci-user-att-connect (ble:parse-mac peer) :addr-type :public
                                        :dev dev :init-phys #x01
                                        :timeout 25 :retries 3))
      (format t "~&connected to ~A on hci~D~%" peer dev)
      (let ((mtu (ble:att-exchange-mtu chan 247)))
        (format t "~&MTU ~A~%" mtu))
      (let* ((chars (ble:att-discover-characteristics chan))
             (mfr (ble:char-handle-by-uuid chars (ble:uuid16 ble:+char-manufacturer-name+)))
             (mdl (ble:char-handle-by-uuid chars (ble:uuid16 ble:+char-model-number+))))
        (format t "~&manufacturer handle ~A, model handle ~A~%" mfr mdl)
        (force-output)
        (check (and mfr mdl) "both characteristics discovered")
        (when (and mfr mdl)
          ;; --- the new opcode ------------------------------------------
          (multiple-value-bind (values err)
              (ble:att-read-multiple-variable chan (list mfr mdl))
            (cond
              ((eql err #x06)
               (check nil "peer refused 0x20 as Request Not Supported"))
              (err (check nil "0x20 failed: ~A" err))
              (t
               (format t "~&0x20 returned ~D value(s): ~{~S~^ ~}~%"
                       (length values) (mapcar #'as-text values))
               (check (= 2 (length values))
                      "two attributes came back as TWO values")
               (check (string= "ACME" (as-text (first values)))
                      "the first is ~S, whole and on its own"
                      (as-text (first values)))
               (check (and (>= (length (second values)) 2)
                           (char= #\r (char (as-text (second values)) 0)))
                      "the second is ~S -- a DIFFERENT length, kept separate"
                      (as-text (second values))))))
          ;; --- the old one, for contrast --------------------------------
          (multiple-value-bind (blob err)
              (ble:att-read-multiple chan (list mfr mdl))
            (cond
              (err (check nil "0x0E failed: ~A" err))
              (t
               (format t "~&0x0E returned ~D octet(s): ~S~%"
                       (length blob) (as-text blob))
               (check (> (length blob) 4)
                      "the same read through 0x0E is one run-together blob ~
                       (~S) with no boundary in it"
                      (as-text blob))))))))
    (format t "~&~%~D/~D checks passed~%" (- checks fails) checks)
    (force-output)
    (sb-ext:exit :code (if (zerop fails) 0 1))))
