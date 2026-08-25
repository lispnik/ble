;;;; Provisioning: writing a configuration that does not fit in one packet.
;;;;
;;;; The ninth example, and the one about long writes. Every other example
;;;; sends values that fit in a single ATT payload -- twenty octets on a
;;;; default MTU -- because that is what a measurement is. A configuration is
;;;; not. An SSID is up to 32 octets, a WPA passphrase up to 63, and a server
;;;; URL beside them; there is no arranging that into twenty.
;;;;
;;;; So it goes as a queued write: Prepare Write puts fragments in a queue
;;;; with their offsets, and Execute Write commits the lot. And the part worth
;;;; the example is that Execute Write can also CANCEL, discarding the queue
;;;; untouched -- because a queued write is not several writes, it is one
;;;; write that happens to arrive in pieces. A peer that disconnects halfway
;;;; through configuring this device must not leave it holding half of a new
;;;; SSID and all of an old passphrase. Nothing else in this repository
;;;; demonstrates that, and it is the property the whole procedure exists for.
;;;;
;;;; Reading it back is the same problem mirrored: the value is longer than
;;;; the MTU, so a client has to ask for it in pieces with Read Blob.
;;;;
;;;; A NOTE ON SECURITY, because this example writes credentials. The
;;;; characteristic is unprotected by default so that the example stays about
;;;; long writes and its client stays readable. That is not what a product
;;;; should do: a passphrase written over an unencrypted link is a passphrase
;;;; broadcast to the room. BUILD-SERVER takes :SECURE T to require an
;;;; encrypted link, and examples/glucose/ shows a peripheral driving the
;;;; pairing that makes it possible.
;;;;
;;;; Its own package, using only exported symbols, for the same reason the
;;;; others are.

(defpackage #:provisioning
  (:use #:common-lisp)
  (:export #:encode-config #:decode-config #:build-server #:run #:*name*
           #:+service-uuid+ #:+config-uuid+ #:+state-uuid+ #:+error-uuid+
           #:+rssi-uuid+ #:+apply-uuid+))

(in-package #:provisioning)

(defparameter *name* "Lisp Provision")

;;; --- the service --------------------------------------------------------
;;;
;;; These UUIDs are this project's own. There is no SIG service for
;;; provisioning -- Mesh Provisioning (0x1827) is Bluetooth Mesh, which is a
;;; different thing entirely -- and a made-up 16-bit UUID would be squatting
;;; on the registry, since every one of those belongs to somebody. A 128-bit
;;; UUID is the correct way to be unassigned: there are enough of them that
;;; picking one at random is safe, which is the whole point of their size.
;;;
;;; Laid out like Nordic's, one field apart, so the family is legible.

(defparameter +service-uuid+ (ble:uuid128 "9A7B2C10-3D4E-4F58-9A6B-1C2D3E4F5A60"))
(defparameter +config-uuid+  (ble:uuid128 "9A7B2C11-3D4E-4F58-9A6B-1C2D3E4F5A60"))
(defparameter +state-uuid+   (ble:uuid128 "9A7B2C12-3D4E-4F58-9A6B-1C2D3E4F5A60"))
(defparameter +error-uuid+   (ble:uuid128 "9A7B2C13-3D4E-4F58-9A6B-1C2D3E4F5A60"))
(defparameter +rssi-uuid+    (ble:uuid128 "9A7B2C14-3D4E-4F58-9A6B-1C2D3E4F5A60"))
(defparameter +apply-uuid+   (ble:uuid128 "9A7B2C15-3D4E-4F58-9A6B-1C2D3E4F5A60"))

;;; State, as reported by the state characteristic.
(defconstant +state-unconfigured+ 0)
(defconstant +state-configured+   1)
(defconstant +state-applying+     2)
(defconstant +state-connected+    3)
(defconstant +state-failed+       4)

;;; Errors, as reported by the error characteristic.
(defconstant +error-none+          0)
(defconstant +error-bad-password+  1)
(defconstant +error-no-such-network+ 2)

;;; An application error code. 0x80-0x9F is the range the Core specification
;;; leaves to a profile, so a service that needs to say something the ATT
;;; codes cannot say has somewhere to say it. `The value you assembled is not
;;; a configuration' is exactly that: nothing about its length or offset was
;;; wrong, so Invalid Value Length would be a lie.
(defconstant +err-malformed-config+ #x80)

;;; What this device will accept. The limits are the protocol's, not ours:
;;; an SSID is at most 32 octets, and a WPA-PSK passphrase is 8 to 63.
(defconstant +max-ssid+ 32)
(defconstant +min-psk+ 8)
(defconstant +max-psk+ 63)
(defconstant +max-url+ 128)

;;; --- the configuration blob ---------------------------------------------

(defun encode-config (&key (ssid "") (psk "") (url ""))
  "Encode a configuration: a version octet, then each field length-prefixed.

Length-prefixed rather than delimited, because a passphrase may contain any
octet at all -- including whatever separator seemed safe -- and a format that
cannot represent a legal password is a format that fails on exactly the
accounts whose owners were careful."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer t)))
    (flet ((field (s)
             (let ((bytes (map 'list #'char-code s)))
               (vector-push-extend (length bytes) out)
               (dolist (b bytes) (vector-push-extend b out)))))
      (vector-push-extend 1 out)        ; format version
      (field ssid)
      (field psk)
      (field url))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun decode-config (octets)
  "Decode a configuration blob. Returns a plist, or NIL if it is not one.

NIL rather than a signal: this parses something a peer sent, and a peer is
entitled to send nonsense. The caller turns that into an ATT error, which is
the only thing the peer can act on."
  (let ((i 0) (n (length octets)) (fields '()))
    (when (or (zerop n) (/= 1 (aref octets 0)))
      (return-from decode-config nil))   ; unknown format version
    (incf i)
    (dotimes (k 3)
      (when (>= i n) (return-from decode-config nil))
      (let ((len (aref octets i)))
        (incf i)
        (when (> (+ i len) n) (return-from decode-config nil))
        (push (map 'string #'code-char (subseq octets i (+ i len))) fields)
        (incf i len)))
    ;; Trailing octets mean this is not the message it claims to be.
    (unless (= i n) (return-from decode-config nil))
    (destructuring-bind (url psk ssid) fields
      (list :ssid ssid :psk psk :url url))))

(defun config-complaint (config)
  "NIL if CONFIG is acceptable, or a string saying what is wrong with it.

Separate from decoding because `well-formed' and `acceptable' are different
questions, and a peer that sent a syntactically perfect four-character
passphrase deserves a different answer from one that sent rubbish."
  (let ((ssid (getf config :ssid))
        (psk (getf config :psk))
        (url (getf config :url)))
    (cond ((zerop (length ssid)) "the SSID is empty")
          ((> (length ssid) +max-ssid+)
           (format nil "the SSID is ~D octets; the limit is ~D"
                   (length ssid) +max-ssid+))
          ;; An open network is legitimate, so an empty passphrase is allowed;
          ;; a short one is not, because WPA will not take it.
          ((and (plusp (length psk)) (< (length psk) +min-psk+))
           (format nil "the passphrase is ~D octets; WPA needs at least ~D"
                   (length psk) +min-psk+))
          ((> (length psk) +max-psk+)
           (format nil "the passphrase is ~D octets; the limit is ~D"
                   (length psk) +max-psk+))
          ((> (length url) +max-url+)
           (format nil "the URL is ~D octets; the limit is ~D"
                   (length url) +max-url+))
          (t nil))))

;;; --- the device ---------------------------------------------------------

(defstruct device
  "The server, the handles, and the configuration currently held."
  server config-handle state-handle state-cccd error-handle rssi-handle
  apply-handle
  (config nil) (state +state-unconfigured+) (last-error +error-none+)
  (commits 0) (rejections 0))

(defun set-state (dev state)
  (setf (device-state dev) state)
  (ble:gatt-set-value (device-server dev) (device-state-handle dev)
                      (vector state)))

(defun handle-config-write (dev value)
  "Commit a configuration. Called once with the whole assembled value.

This is what makes the queued write atomic. The server lays the fragments
into a copy, so by the time this runs it holds the complete value or the
write never happened at all -- there is no state in which half of it has been
accepted. Returning an error code here leaves the attribute exactly as it
was, which is why validation belongs in this hook and not in the fragments."
  (let ((config (decode-config value)))
    (cond
      ((null config)
       (incf (device-rejections dev))
       +err-malformed-config+)
      ((config-complaint config)
       (incf (device-rejections dev))
       +err-malformed-config+)
      (t
       (incf (device-commits dev))
       (setf (device-config dev) config)
       ;; The hook owns the value: a characteristic with an ON-WRITE is not
       ;; stored by the server, because a hook that refuses must be able to
       ;; leave the old one in place.
       (ble:gatt-set-value (device-server dev) (device-config-handle dev) value)
       (set-state dev +state-configured+)
       (setf (device-last-error dev) +error-none+)
       (ble:gatt-set-value (device-server dev) (device-error-handle dev)
                           (vector +error-none+))
       nil))))

(defun handle-apply-write (dev value)
  "The control point: 1 applies the held configuration, 2 clears it."
  (if (zerop (length value))
      ble:+att-err-invalid-value-length+
      (case (aref value 0)
        (1 (if (device-config dev)
               (progn (set-state dev +state-applying+) nil)
               ;; Nothing to apply. Not malformed -- the request was fine,
               ;; the device simply has no configuration yet.
               +err-malformed-config+))
        (2 (setf (device-config dev) nil)
           (ble:gatt-set-value (device-server dev) (device-config-handle dev) #())
           (set-state dev +state-unconfigured+)
           nil)
        (t +err-malformed-config+))))

(defun build-server (&key secure)
  "Generic Access, Generic Attribute, and the provisioning service.

SECURE requires an encrypted link for the configuration and the control
point, which is what a product must do and what this example does not do by
default -- see the note at the top of the file."
  (let ((server (ble:make-gatt-server :mtu 23))
        (dev nil))
    (ble:gatt-add-service server ble:+service-generic-access+)
    (ble:gatt-add-characteristic server :uuid ble:+char-device-name+
                                        :properties '(:read) :value *name*)
    (ble:gatt-add-characteristic server :uuid ble:+char-appearance+
                                        :properties '(:read)
                                        :value (ble:appearance
                                                ble:+appearance-generic-tag+))
    (ble:gatt-add-service server ble:+service-generic-attribute+)
    (ble:gatt-add-characteristic server :uuid ble:+char-service-changed+
                                        :properties '(:indicate)
                                        :value (ble:service-changed-range))
    (ble:gatt-add-service server +service-uuid+)
    ;; The long one. Readable as well as writable so the client has something
    ;; to Read Blob; a real device would very likely refuse to hand the
    ;; passphrase back, and would be right to.
    (let ((config (ble:gatt-add-characteristic
                   server :uuid +config-uuid+
                          :properties '(:read :write)
                          :security (and secure :encrypted)
                          :on-write (lambda (s a v)
                                      (declare (ignore s a))
                                      (handle-config-write dev v)))))
      (multiple-value-bind (state state-cccd)
          (ble:gatt-add-characteristic server :uuid +state-uuid+
                                              :properties '(:read :notify)
                                              :value (vector +state-unconfigured+))
        ;; Three short characteristics in a row, which is what makes Read
        ;; Multiple worth using: one round trip instead of three. Note the
        ;; catch -- Read Multiple concatenates the values with no lengths
        ;; between them, so a client can only take them apart if it already
        ;; knows how long each is. That is why these three are one octet each
        ;; and fixed, and why Bluetooth 5.2 added a variable-length version.
        (let ((err (ble:gatt-add-characteristic
                    server :uuid +error-uuid+ :properties '(:read)
                           :value (vector +error-none+)))
              (rssi (ble:gatt-add-characteristic
                     server :uuid +rssi-uuid+ :properties '(:read)
                            :value (vector 0)))
              (apply-h (ble:gatt-add-characteristic
                        server :uuid +apply-uuid+ :properties '(:write)
                               :security (and secure :encrypted)
                               :on-write (lambda (s a v)
                                           (declare (ignore s a))
                                           (handle-apply-write dev v)))))
          (setf dev (make-device :server server :config-handle config
                                 :state-handle state :state-cccd state-cccd
                                 :error-handle err :rssi-handle rssi
                                 :apply-handle apply-h))
          dev)))))

;;; --- running it ---------------------------------------------------------

(defun run (&key (dev nil) (seconds nil) secure)
  "Advertise as a provisionable device and accept a configuration."
  (let* ((device (build-server :secure secure))
         (adapter (or dev (ble:default-hci-dev)))
         (applying-since nil))
    (ble:install-adapter-teardown)
    (ble:with-hci-user-socket (sock adapter)
      (let ((addr (ble:static-random-address (ble:smp-random-octets sock 6))))
        (ble:set-random-address sock addr)
        (ble:set-adv-parameters sock :adv-type ble:+adv-ind+ :own-addr-type 1)
        ;; Found by name: the service UUID is 128-bit and does not fit beside
        ;; a name in a legacy advertisement, as examples/nordic-uart/ explains
        ;; and examples/broadcaster/ gets around.
        (ble:set-adv-data sock (ble:adv-data
                                :flags '(:general-discoverable :no-bredr)
                                :name *name*))
        (format t "~&~A advertising on hci~D as ~A~:[~; (encrypted link required)~]~%"
                *name* adapter (ble:format-mac addr) secure)
        (force-output)
        (ble:serve-peripheral
         (device-server device) sock
         :seconds seconds
         :on-connect (lambda (conn peer ptype)
                       (declare (ignore conn))
                       (format t "~&connected: ~A (~(~A~))~%"
                               (ble:format-mac peer) ptype)
                       (force-output))
         :on-disconnect (lambda (conn)
                          (declare (ignore conn))
                          ;; The prepared-write queue belongs to the
                          ;; connection. The server clears it on execute, and
                          ;; a peer that vanished mid-write never executed --
                          ;; so nothing it half-sent has been committed, which
                          ;; is the guarantee this example is about.
                          (format t "~&disconnected; ~D commit(s), ~D ~
                                     rejection(s), config is ~:[unset~;set~]~%"
                                  (device-commits device)
                                  (device-rejections device)
                                  (device-config device))
                          (force-output))
         :on-tick
         (lambda (conn request)
           (declare (ignore request))
           ;; Pretend applying takes a moment, so a client watching the state
           ;; characteristic sees it move rather than jumping.
           (cond
             ((and (= (device-state device) +state-applying+) (null applying-since))
              (setf applying-since (get-internal-real-time)))
             ((and (= (device-state device) +state-applying+)
                   (> (- (get-internal-real-time) applying-since)
                      (* 3 internal-time-units-per-second)))
              (setf applying-since nil)
              (set-state device +state-connected+)
              (ble:gatt-set-value (device-server device)
                                  (device-rssi-handle device)
                                  (vector (logand -55 #xFF)))
              (format t "~&  applied: ~A~%" (getf (device-config device) :ssid))
              (force-output)
              (ble:gatt-notify (device-server device) conn
                               (device-state-handle device)
                               (vector +state-connected+)))
             ((/= (device-state device) +state-applying+)
              (setf applying-since nil)))))))))
