;;;; Drive the provisioning device's long writes, without a radio.
;;;;
;;;; This check does something the others do not: it feeds real ATT PDUs into
;;;; the real server through a test channel, so the prepare/execute path is
;;;; exercised end to end rather than by calling the write hook directly. That
;;;; matters here because the thing being tested is not the hook -- it is when
;;;; the hook is called, and whether the attribute changes when it is not.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename "./")) :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble/examples))

(defvar *problems* 0)
(defun check (ok fmt &rest args)
  (format t "~&[~A] ~?~%" (if ok " ok " "FAIL") fmt args)
  (unless ok (incf *problems*)))

;; --- the blob format ----------------------------------------------------
(let* ((blob (provisioning:encode-config :ssid "net" :psk "12345678"
                                         :url "http://x/")))
  (check (= 1 (aref blob 0)) "a format version leads, so a later one can differ")
  (check (= 3 (aref blob 1)) "then each field is length-prefixed")
  (let ((back (provisioning:decode-config blob)))
    (check (string= "net" (getf back :ssid)) "and decoding returns the ssid")
    (check (string= "12345678" (getf back :psk)) "the passphrase")
    (check (string= "http://x/" (getf back :url)) "and the url")))

;; Length-prefixed, not delimited: a passphrase may contain anything.
(let* ((nasty (format nil "pa~Cs~Cwd" #\Nul #\Newline))
       (blob (provisioning:encode-config :ssid "n" :psk nasty :url "")))
  (check (string= nasty (getf (provisioning:decode-config blob) :psk))
         "a passphrase containing NUL and a newline survives exactly -- ~
          which a delimited format could not carry"))

(check (null (provisioning:decode-config #(9 1 65)))
       "an unknown format version decodes to NIL rather than being guessed at")
(check (null (provisioning:decode-config #(1 5 65)))
       "a field claiming more octets than are present is refused")
(check (null (provisioning:decode-config #(1 1 65 1 66 1 67 99)))
       "and trailing rubbish after the last field is refused too")
(check (null (provisioning:decode-config #())) "as is nothing at all")

;; --- driving the real server -------------------------------------------
(defun pdu (&rest octets)
  (coerce octets '(simple-array (unsigned-byte 8) (*))))

(defun u16 (n) (list (ldb (byte 8 0) n) (ldb (byte 8 8) n)))

(defun prepare (server chan handle offset bytes)
  (ble:gatt-serve-pdu server chan
                      (apply #'pdu #x16 (append (u16 handle) (u16 offset)
                                                (coerce bytes 'list)))))

(defun execute (server chan &key (commit t))
  (ble:gatt-serve-pdu server chan (pdu #x18 (if commit 1 0))))

(defun last-pdu (chan)
  (car (last (ble:att-test-channel-sent-pdus chan))))

(defun value-of (dev)
  (ble:gatt-attribute-value
   (ble:gatt-find-attribute (slot-value dev 'provisioning::server)
                            (slot-value dev 'provisioning::config-handle))))

(let* ((dev (provisioning:build-server))
       (server (slot-value dev 'provisioning::server))
       (handle (slot-value dev 'provisioning::config-handle))
       (chan (ble:make-att-test-channel))
       (blob (provisioning:encode-config :ssid "Kitchen Wi-Fi"
                                         :psk "correct horse battery staple"
                                         :url "https://example.invalid/ingest")))
  (check (> (length blob) 20)
         "the configuration is ~D octets, which does not fit one default ~
          payload -- this example would be pointless otherwise" (length blob))

  ;; A queued write, committed.
  (let ((offset 0) (chunk 15))
    (loop while (< offset (length blob))
          do (let ((end (min (length blob) (+ offset chunk))))
               (prepare server chan handle offset (subseq blob offset end))
               (setf offset end))))
  (check (= #x17 (aref (last-pdu chan) 0))
         "every fragment is acknowledged with a Prepare Write Response")
  (check (zerop (length (value-of dev)))
         "and NONE of them has reached the attribute yet -- the queue is not ~
          the value")
  (execute server chan)
  (check (= #x19 (aref (last-pdu chan) 0)) "Execute Write is accepted")
  (check (equalp blob (value-of dev)) "and now the whole value has arrived")
  (check (string= "Kitchen Wi-Fi" (getf (slot-value dev 'provisioning::config) :ssid))
         "and was parsed once, whole")
  (check (= 1 (slot-value dev 'provisioning::commits))
         "the hook ran exactly once, not once per fragment"))

;; The property the example exists for: a cancelled queue changes nothing.
(let* ((dev (provisioning:build-server))
       (server (slot-value dev 'provisioning::server))
       (handle (slot-value dev 'provisioning::config-handle))
       (chan (ble:make-att-test-channel))
       (good (provisioning:encode-config :ssid "Original" :psk "12345678" :url ""))
       (other (provisioning:encode-config :ssid "Replacement" :psk "87654321" :url "")))
  ;; Establish a configuration.
  (prepare server chan handle 0 good)
  (execute server chan)
  (check (equalp good (value-of dev)) "a configuration is in place")
  ;; Queue a different one and abandon it.
  (let ((offset 0) (chunk 10))
    (loop while (< offset (length other))
          do (let ((end (min (length other) (+ offset chunk))))
               (prepare server chan handle offset (subseq other offset end))
               (setf offset end))))
  (execute server chan :commit nil)
  (check (= #x19 (aref (last-pdu chan) 0)) "cancelling is acknowledged too")
  (check (equalp good (value-of dev))
         "and the abandoned write changed NOTHING -- the device still holds ~
          the configuration it had")
  (check (string= "Original" (getf (slot-value dev 'provisioning::config) :ssid))
         "including the parsed form of it")
  (check (= 1 (slot-value dev 'provisioning::commits))
         "and the hook never ran for the abandoned one")
  ;; A queue is abandoned by disconnecting too, which is the same thing from
  ;; the server's point of view: no Execute Write ever arrives.
  (prepare server chan handle 0 other)
  (check (equalp good (value-of dev))
         "a peer that queues and then vanishes leaves the value untouched, ~
          because nothing is applied until Execute Write says so"))

;; A refused commit leaves the old value in place rather than a broken one.
(let* ((dev (provisioning:build-server))
       (server (slot-value dev 'provisioning::server))
       (handle (slot-value dev 'provisioning::config-handle))
       (chan (ble:make-att-test-channel))
       (good (provisioning:encode-config :ssid "Original" :psk "12345678" :url "")))
  (prepare server chan handle 0 good)
  (execute server chan)
  ;; Now queue something well-formed but unacceptable: a four-character
  ;; passphrase, which WPA will not take.
  (prepare server chan handle 0
           (provisioning:encode-config :ssid "Other" :psk "abcd" :url ""))
  (execute server chan)
  (let ((rsp (last-pdu chan)))
    (check (= #x01 (aref rsp 0)) "an unacceptable configuration is refused")
    (check (= #x18 (aref rsp 1)) "against the Execute Write that committed it")
    (check (= #x80 (aref rsp 4))
           "with an application error, not an ATT one -- nothing about the ~
            length or offset was wrong"))
  (check (equalp good (value-of dev))
         "and the previous configuration survives the refusal")
  (check (= 1 (slot-value dev 'provisioning::commits)) "with one commit")
  (check (= 1 (slot-value dev 'provisioning::rejections)) "and one rejection"))

;; --- what the device will not accept ------------------------------------
(flet ((complaint (&rest args)
         (funcall (find-symbol "CONFIG-COMPLAINT" :provisioning)
                  (apply #'provisioning:decode-config
                         (list (apply #'provisioning:encode-config args))))))
  (check (null (complaint :ssid "n" :psk "12345678" :url ""))
         "a sound configuration draws no complaint")
  (check (null (complaint :ssid "open network" :psk "" :url ""))
         "an empty passphrase is fine -- open networks exist")
  (check (complaint :ssid "" :psk "12345678" :url "") "an empty SSID is not")
  (check (complaint :ssid "n" :psk "short" :url "")
         "nor is a passphrase WPA would refuse")
  (check (complaint :ssid (make-string 33 :initial-element #\x)
                    :psk "12345678" :url "")
         "nor an SSID past 32 octets"))

;; --- the database -------------------------------------------------------
(let* ((dev (provisioning:build-server))
       (server (slot-value dev 'provisioning::server)))
  (check (equal (list "1800" "1801" (ble:uuid-string provisioning:+service-uuid+))
                (mapcar #'ble:uuid-string
                        (mapcar #'ble:gatt-service-entry-uuid
                                (reverse (ble:gatt-server-services server)))))
         "a 128-bit vendor service, because provisioning has no assigned number")
  (let ((cfg (ble:gatt-find-attribute
              server (slot-value dev 'provisioning::config-handle))))
    (check (member :read (ble:gatt-attribute-permissions cfg))
           "the configuration is readable, so a client can Read Blob it back")
    (check (member :write (ble:gatt-attribute-permissions cfg))
           "and writable")
    (check (not (ble:gatt-attribute-security cfg))
           "and unprotected by default, which the file says plainly is not ~
            what a product should do")))

(let* ((dev (provisioning:build-server :secure t))
       (server (slot-value dev 'provisioning::server)))
  (check (ble:gatt-attribute-security
          (ble:gatt-find-attribute server (slot-value dev 'provisioning::config-handle)))
         ":secure t protects the credentials")
  (check (ble:gatt-attribute-security
          (ble:gatt-find-attribute server (slot-value dev 'provisioning::apply-handle)))
         "and the control point with them"))

(check (find-package :provisioning-client) "the client half loads too")

(format t "~&~%PROVISIONING CHECK: ~:[clean~;~:*~D problem(s)~]~%" *problems*)
(sb-ext:exit :code (if (zerop *problems*) 0 1))
