(in-package #:ble)

;;; The Security Manager Protocol: LE Secure Connections, Just Works.
;;;
;;; What this closes: a peer that requires a bonded link was simply
;;; unreachable before, however correct everything above ATT was.
;;;
;;; Scope, stated plainly because the omissions matter:
;;;
;;;   * LE Secure Connections only. Legacy pairing (4.0/4.1) is not here. Its
;;;     Just Works variant offers no protection against a passive eavesdropper
;;;     at all, and adding it would mean shipping something whose security
;;;     properties are hard to describe honestly.
;;;   * Just Works association only -- no passkey entry, no OOB. The exchange
;;;     is identical for numeric comparison; what is missing is a way to show
;;;     a user six digits and ask, which is a UI question this library has no
;;;     business answering. SMP-G2 is implemented, so that path is short.
;;;   * No key distribution. Secure Connections derives the LTK on both sides,
;;;     so nothing has to be sent for encryption to work; IRK and CSRK
;;;     distribution (for private addresses and signed writes) is not done.
;;;
;;; Just Works gives no MITM protection -- an attacker present for the pairing
;;; can be both ends of it. It protects against passive eavesdropping only.
;;; That is a real limit, not a footnote, and a caller who needs more needs
;;; passkey entry.

(defconstant +smp-cid+ #x0006)

(defconstant +smp-pairing-request+     #x01)
(defconstant +smp-pairing-response+    #x02)
(defconstant +smp-pairing-confirm+     #x03)
(defconstant +smp-pairing-random+      #x04)
(defconstant +smp-pairing-failed+      #x05)
(defconstant +smp-pairing-public-key+  #x0C)
(defconstant +smp-pairing-dhkey-check+ #x0D)

(defconstant +smp-io-no-input-no-output+ #x03)
(defconstant +smp-auth-bonding+ #x01)
(defconstant +smp-auth-sc+      #x08)

(defparameter +smp-failure-reasons+
  '((#x01 . "passkey entry failed") (#x02 . "OOB not available")
    (#x03 . "authentication requirements") (#x04 . "confirm value failed")
    (#x05 . "pairing not supported")       (#x06 . "encryption key size")
    (#x07 . "command not supported")       (#x08 . "unspecified reason")
    (#x09 . "repeated attempts")           (#x0A . "invalid parameters")
    (#x0B . "DHKey check failed")          (#x0C . "numeric comparison failed")))

(define-condition smp-error (ble-error)
  ((reason :initarg :reason :reader smp-error-reason))
  (:report (lambda (c s)
             (format s "pairing failed: ~A"
                     (or (cdr (assoc (smp-error-reason c) +smp-failure-reasons+))
                         (format nil "reason 0x~2,'0X" (smp-error-reason c))))))
  (:documentation "The peer refused to pair, with an SMP reason code."))

(defun smp-random (conn n)
  "N random octets from the controller's generator.

The controller's rather than the Lisp runtime's: these nonces are what stop a
pairing transcript being replayed, and MAKE-RANDOM-STATE is not a source to
stake that on."
  (let ((out (make-octets n)))
    (loop for off from 0 below n by 8
          do (let ((r (hci-do-command (hci-conn-sock conn) +ogf-le+ #x0018 #()
                                      :name "LE Rand")))
               (replace out r :start1 off :end1 (min n (+ off 8)))))
    out))

(defstruct smp-session
  "One pairing in progress, and the keys it produced."
  conn role local-priv local-x local-y peer-x peer-y
  na nb local-addr peer-addr local-io peer-io ltk mackey)

;;; --- addresses ----------------------------------------------------------

(defun %smp-addr (mac type)
  "The seven octets f5 and f6 want: address type, then the address, in crypto
order. TYPE is :PUBLIC or :RANDOM."
  (cat (vector (ecase type (:public 0) (:random 1))) (msb mac)))

;;; --- the channel --------------------------------------------------------

(defun smp-send (conn opcode payload)
  (hci-acl-send-l2cap conn +smp-cid+ (cat (vector opcode) payload)))

(defun smp-next (conn &key (timeout-ms 10000) expect)
  "The next SMP PDU, or NIL. Signals SMP-ERROR on Pairing Failed.

Frames for other channels keep flowing while this waits: the ATT layer's PDUs
stay in PENDING and the signalling channel is served as usual. Pairing does
not stop the rest of the link."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms internal-time-units-per-second) 1000))))
    (loop
      (let ((hit (find-if (lambda (f)
                            (and (plusp (length f))
                                 (or (null expect) (= (aref f 0) expect)
                                     (= (aref f 0) +smp-pairing-failed+))))
                          (hci-conn-smp-pending conn))))
        (when hit
          (setf (hci-conn-smp-pending conn)
                (remove hit (hci-conn-smp-pending conn) :count 1))
          (when (= (aref hit 0) +smp-pairing-failed+)
            (error 'smp-error :reason (if (>= (length hit) 2) (aref hit 1) #x08)))
          (return hit)))
      (when (<= (- deadline (get-internal-real-time)) 0) (return nil))
      (when (eq :disconnected (%pump conn 200)) (return nil)))))

(defun %smp-note-frame (conn frame)
  (setf (hci-conn-smp-pending conn)
        (nconc (hci-conn-smp-pending conn) (list frame))))

(setf *l2cap-smp-frame-handler* #'%smp-note-frame)

(defun smp-fail (conn reason)
  (ignore-errors (smp-send conn +smp-pairing-failed+ (vector reason)))
  (error 'smp-error :reason reason))

;;; --- pairing ------------------------------------------------------------

(defun %pairing-params (io)
  "The three octets of a Pairing Request or Response that feed f6, plus the
key-distribution octets. Secure Connections and bonding are both requested;
without the SC bit the peer would answer with legacy pairing, which this does
not implement."
  (vector io                                   ; IO capability
          #x00                                 ; OOB not present
          (logior +smp-auth-bonding+ +smp-auth-sc+)))

(defun %send-pairing (conn opcode io)
  (let ((p (cat (%pairing-params io)
                (vector 16       ; max encryption key size
                        #x00     ; initiator key distribution: none
                        #x00)))) ; responder key distribution: none
    (smp-send conn opcode p)
    ;; f6 is fed the first three octets together with the opcode.
    (cat (vector opcode) (%pairing-params io))))

(defun smp-pair (conn &key (role :central) local-addr local-addr-type
                           peer-addr peer-addr-type (timeout-ms 20000))
  "Pair over an established connection and return an SMP-SESSION whose LTK is
ready to encrypt with.

ROLE :CENTRAL initiates. Addresses are in on-air order, as everywhere else in
this library; both are needed because f5 and f6 bind the keys to them, which
is what stops a derived key being replayed against a different pair of
devices."
  (let* ((initiator (eq role :central))
         (io +smp-io-no-input-no-output+)
         (session (make-smp-session
                   :conn conn :role role
                   :local-addr (%smp-addr local-addr local-addr-type)
                   :peer-addr (%smp-addr peer-addr peer-addr-type))))
    ;; 1. feature exchange
    (if initiator
        (progn
          (setf (smp-session-local-io session)
                (%send-pairing conn +smp-pairing-request+ io))
          (let ((rsp (smp-next conn :expect +smp-pairing-response+
                                    :timeout-ms timeout-ms)))
            (unless rsp (smp-fail conn #x08))
            (setf (smp-session-peer-io session) (subseq rsp 0 4))
            (unless (logtest +smp-auth-sc+ (aref rsp 3))
              ;; The peer wants legacy pairing. Refusing is the honest answer:
              ;; continuing would mean pretending to a security level we do
              ;; not implement.
              (smp-fail conn #x03))))
        (let ((req (smp-next conn :expect +smp-pairing-request+
                                  :timeout-ms timeout-ms)))
          (unless req (smp-fail conn #x08))
          (setf (smp-session-peer-io session) (subseq req 0 4))
          (unless (logtest +smp-auth-sc+ (aref req 3)) (smp-fail conn #x03))
          (setf (smp-session-local-io session)
                (%send-pairing conn +smp-pairing-response+ io))))

    ;; 2. public keys
    (multiple-value-bind (priv x y) (smp-generate-keypair)
      (setf (smp-session-local-priv session) priv
            (smp-session-local-x session) x
            (smp-session-local-y session) y))
    (flet ((send-key ()
             ;; On the wire, little-endian; in the crypto, big-endian.
             (smp-send conn +smp-pairing-public-key+
                       (cat (msb (smp-session-local-x session))
                            (msb (smp-session-local-y session)))))
           (recv-key ()
             (let ((pk (smp-next conn :expect +smp-pairing-public-key+
                                      :timeout-ms timeout-ms)))
               (unless (and pk (>= (length pk) 65)) (smp-fail conn #x0A))
               (setf (smp-session-peer-x session) (msb (subseq pk 1 33))
                     (smp-session-peer-y session) (msb (subseq pk 33 65))))))
      (if initiator (progn (send-key) (recv-key)) (progn (recv-key) (send-key))))

    ;; 3. confirm and nonces. Only the responder commits to a confirm value;
    ;;    the initiator reveals its nonce first and the responder's confirm is
    ;;    what binds it to a nonce it chose before seeing Na.
    (let ((nonce (smp-random conn 16)))
      (if initiator
          (setf (smp-session-na session) nonce)
          (setf (smp-session-nb session) nonce)))
    (if initiator
        (let ((cb (smp-next conn :expect +smp-pairing-confirm+
                                 :timeout-ms timeout-ms)))
          (unless cb (smp-fail conn #x08))
          (smp-send conn +smp-pairing-random+ (msb (smp-session-na session)))
          (let ((rb (smp-next conn :expect +smp-pairing-random+
                                   :timeout-ms timeout-ms)))
            (unless rb (smp-fail conn #x08))
            (setf (smp-session-nb session) (msb (subseq rb 1 17)))
            ;; Check the responder kept its word.
            (let ((expected (smp-f4 (smp-session-peer-x session)
                                    (smp-session-local-x session)
                                    (smp-session-nb session) 0)))
              (unless (equalp expected (msb (subseq cb 1 17)))
                (smp-fail conn #x04)))))
        (progn
          (smp-send conn +smp-pairing-confirm+
                    (msb (smp-f4 (smp-session-local-x session)
                                 (smp-session-peer-x session)
                                 (smp-session-nb session) 0)))
          (let ((ra (smp-next conn :expect +smp-pairing-random+
                                   :timeout-ms timeout-ms)))
            (unless ra (smp-fail conn #x08))
            (setf (smp-session-na session) (msb (subseq ra 1 17))))
          (smp-send conn +smp-pairing-random+ (msb (smp-session-nb session)))))

    ;; 4. shared secret and keys
    (let* ((dhkey (smp-dhkey (smp-session-local-priv session)
                             (smp-session-peer-x session)
                             (smp-session-peer-y session)))
           (a1 (if initiator (smp-session-local-addr session)
                   (smp-session-peer-addr session)))
           (a2 (if initiator (smp-session-peer-addr session)
                   (smp-session-local-addr session))))
      (multiple-value-bind (mackey ltk)
          (smp-f5 dhkey (smp-session-na session) (smp-session-nb session) a1 a2)
        (setf (smp-session-mackey session) mackey
              (smp-session-ltk session) ltk)

        ;; 5. check values, each computed over the other side's IO capability
        ;;    and address, so agreeing proves both derived the same key from
        ;;    the same transcript.
        (let* ((zero (make-octets 16))
               (ea (smp-f6 mackey (smp-session-na session) (smp-session-nb session)
                           zero (if initiator (smp-session-local-io session)
                                    (smp-session-peer-io session))
                           a1 a2))
               (eb (smp-f6 mackey (smp-session-nb session) (smp-session-na session)
                           zero (if initiator (smp-session-peer-io session)
                                    (smp-session-local-io session))
                           a2 a1)))
          (if initiator
              (progn
                (smp-send conn +smp-pairing-dhkey-check+ (msb ea))
                (let ((got (smp-next conn :expect +smp-pairing-dhkey-check+
                                          :timeout-ms timeout-ms)))
                  (unless got (smp-fail conn #x08))
                  (unless (equalp eb (msb (subseq got 1 17)))
                    (smp-fail conn #x0B))))
              (progn
                (let ((got (smp-next conn :expect +smp-pairing-dhkey-check+
                                          :timeout-ms timeout-ms)))
                  (unless got (smp-fail conn #x08))
                  (unless (equalp ea (msb (subseq got 1 17)))
                    (smp-fail conn #x0B)))
                (smp-send conn +smp-pairing-dhkey-check+ (msb eb)))))))
    session))



;;; --- turning the LTK into an encrypted link ------------------------------

(defun smp-start-encryption (session &key (timeout-ms 10000))
  "Encrypt the link with the LTK just derived. Central only -- a peripheral
cannot start encryption, it can only answer the request.

Returns T once the controller reports encryption enabled, or the HCI status.
EDIV and Rand are zero: they exist to identify a stored key from legacy
pairing, and Secure Connections has no such indirection."
  (let* ((conn (smp-session-conn session))
         (params (make-octets 28)))
    (u16le-put params 0 (hci-conn-handle conn))
    (replace params (msb (smp-session-ltk session)) :start1 12)
    (send-hci-command (hci-conn-sock conn) +ogf-le+ #x0019 params)
    (let ((evt (%await-hci-event conn :event #x08 :timeout-ms timeout-ms)))
      (cond ((null evt) :timeout)
            ((eq evt :disconnected) :disconnected)
            ((< (length evt) 7) :timeout)
            ((/= (aref evt 3) 0) (aref evt 3))
            (t (= 1 (aref evt 6)))))))

(defun smp-answer-ltk-request (session event)
  "Answer an LE Long Term Key Request with the paired key. Peripheral side.

A key that is not ours gets a negative reply rather than silence: the peer is
waiting, and refusing tells it to start a fresh pairing instead of timing
out."
  (let* ((conn (smp-session-conn session))
         (sock (hci-conn-sock conn))
         (handle (u16-le event 4)))
    (if (and session (smp-session-ltk session))
        (let ((params (make-octets 18)))
          (u16le-put params 0 handle)
          (replace params (msb (smp-session-ltk session)) :start1 2)
          (hci-do-command sock +ogf-le+ #x001A params :name "LTK Request Reply")
          t)
        (let ((params (make-octets 2)))
          (u16le-put params 0 handle)
          (hci-do-command sock +ogf-le+ #x001B params
                          :name "LTK Request Negative Reply")
          nil))))

(defun smp-await-encryption (conn &key (timeout-ms 10000))
  "Wait for the controller to report the link encrypted. Either side."
  (let ((evt (%await-hci-event conn :event #x08 :timeout-ms timeout-ms)))
    (cond ((null evt) :timeout)
          ((eq evt :disconnected) :disconnected)
          ((< (length evt) 7) :timeout)
          ((/= (aref evt 3) 0) (aref evt 3))
          (t (= 1 (aref evt 6))))))
