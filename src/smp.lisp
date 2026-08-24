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
(defconstant +smp-security-request+    #x0B)

(defconstant +smp-io-display-only+       #x00)
(defconstant +smp-io-display-yes-no+     #x01)
(defconstant +smp-io-keyboard-only+      #x02)
(defconstant +smp-io-no-input-no-output+ #x03)
(defconstant +smp-io-keyboard-display+   #x04)

(defparameter +smp-io-capabilities+
  `((:display-only      . ,+smp-io-display-only+)
    (:display-yes-no    . ,+smp-io-display-yes-no+)
    (:keyboard-only     . ,+smp-io-keyboard-only+)
    (:no-input-no-output . ,+smp-io-no-input-no-output+)
    (:keyboard-display  . ,+smp-io-keyboard-display+)))

(defconstant +smp-auth-bonding+ #x01)
(defconstant +smp-auth-mitm+    #x04)
(defconstant +smp-auth-sc+      #x08)

(defun io-capability-code (io)
  (if (integerp io)
      io
      (or (cdr (assoc io +smp-io-capabilities+))
          (error "unknown IO capability ~S" io))))

;;; The association model is not chosen; it falls out of what both ends can
;;; do. Core spec Vol 3, Part H, Table 2.8. Two rules carry most of it: if
;;; either side cannot both show and accept a number the result is Just Works,
;;; and if neither side asked for MITM protection it is Just Works regardless
;;; of what they could have managed.

(defun smp-association-model (initiator-io responder-io initiator-auth responder-auth)
  "Which model this pair of devices will use: :JUST-WORKS, :PASSKEY-ENTRY or
:NUMERIC-COMPARISON.

Returned rather than requested, because a caller who insists on passkey entry
against a peer with no keyboard is asking for something that cannot happen --
better to report what will happen than to fail obscurely inside the exchange."
  (if (not (or (logtest +smp-auth-mitm+ initiator-auth)
               (logtest +smp-auth-mitm+ responder-auth)))
      :just-works
      (let ((i initiator-io) (r responder-io))
        (cond
          ;; Neither end has any way to involve a person.
          ((or (= i +smp-io-no-input-no-output+) (= r +smp-io-no-input-no-output+))
           :just-works)
          ;; Both can show a number and answer yes or no.
          ((and (member i (list +smp-io-display-yes-no+ +smp-io-keyboard-display+))
                (member r (list +smp-io-display-yes-no+ +smp-io-keyboard-display+)))
           :numeric-comparison)
          ;; A display facing a keyboard, either way round.
          ((or (and (member i (list +smp-io-display-only+ +smp-io-display-yes-no+
                                    +smp-io-keyboard-display+))
                    (member r (list +smp-io-keyboard-only+ +smp-io-keyboard-display+)))
               (and (member i (list +smp-io-keyboard-only+ +smp-io-keyboard-display+))
                    (member r (list +smp-io-display-only+ +smp-io-display-yes-no+
                                    +smp-io-keyboard-display+))))
           :passkey-entry)
          (t :just-works)))))

(defun passkey-octets (passkey)
  "A six-digit passkey as the 128-bit value f6 takes, big-endian."
  (let ((out (make-octets 16)))
    (loop for i from 15 downto 0
          for v = passkey then (ash v -8)
          do (setf (aref out i) (logand v #xFF)))
    out))

(defun passkey-bit (passkey i)
  "Bit I of the passkey, as the Z octet f4 takes during passkey entry: 0x80
with the bit in its low position. Twenty of these, one per round, is what
makes passkey entry twenty exchanges rather than one."
  (logior #x80 (ldb (byte 1 i) passkey)))

(defparameter +smp-failure-reasons+
  '((#x01 . "passkey entry failed") (#x02 . "OOB not available")
    (#x03 . "authentication requirements") (#x04 . "confirm value failed")
    (#x05 . "pairing not supported")       (#x06 . "encryption key size")
    (#x07 . "command not supported")       (#x08 . "unspecified reason")
    (#x09 . "repeated attempts")           (#x0A . "invalid parameters")
    (#x0B . "DHKey check failed")          (#x0C . "numeric comparison failed")))

(define-condition smp-error (ble-error)
  ((reason :initarg :reason :reader smp-error-reason)
   (source :initarg :source :initform :peer :reader smp-error-source))
  (:report (lambda (c s)
             (if (eq (smp-error-source c) :disconnected)
                 (format s "pairing failed: the link dropped before it finished")
                 (format s "pairing failed (~A): ~A"
                         (ecase (smp-error-source c)
                           (:peer "the peer rejected us")
                           (:local "we rejected the peer"))
                         (or (cdr (assoc (smp-error-reason c)
                                         +smp-failure-reasons+))
                             (format nil "reason 0x~2,'0X"
                                     (smp-error-reason c)))))))
  (:documentation
   "Pairing did not complete. SOURCE says which end refused, which is the
first question worth answering: the same reason code means something quite
different depending on who produced it."))

(defvar *smp-trace* nil
  "When true, print the pairing transcript's intermediate values.

Pairing fails opaquely -- a reason code and nothing else -- and the values
that went into a mismatched check value are exactly what is needed to tell a
byte-order mistake from a wiring one.")

(defun %trace-value (label octets)
  (when *smp-trace*
    (format *trace-output* "~&  smp ~12A ~{~2,'0X~}~%" label
            (coerce octets 'list))
    (force-output *trace-output*)))

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
  "The next SMP PDU, or NIL on timeout. Signals SMP-ERROR on Pairing Failed,
and on the link dropping.

A dropped link is reported as itself rather than folded into the timeout. It
used to return NIL, which callers turned into `we rejected the peer:
unspecified reason' -- wrong twice over, since nothing was rejected here and
the cause was not unspecified. It also spent the whole timeout to say so.

Frames for other channels keep flowing while this waits: the ATT layer's PDUs
stay in PENDING and the signalling channel is served as usual. Pairing does
not stop the rest of the link."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms internal-time-units-per-second) 1000))))
    (flet ((claim ()
             (let ((hit (find-if (lambda (f)
                                   (and (plusp (length f))
                                        (or (null expect) (= (aref f 0) expect)
                                            (= (aref f 0) +smp-pairing-failed+))))
                                 (hci-conn-smp-pending conn))))
               (when hit
                 (setf (hci-conn-smp-pending conn)
                       (remove hit (hci-conn-smp-pending conn) :count 1))
                 (when (= (aref hit 0) +smp-pairing-failed+)
                   (error 'smp-error :source :peer
                                     :reason (if (>= (length hit) 2)
                                                 (aref hit 1) #x08)))
                 hit))))
      (loop
        (let ((hit (claim))) (when hit (return hit)))
        (when (<= (- deadline (get-internal-real-time)) 0) (return nil))
        (when (eq :disconnected (%pump conn 200))
          ;; A peer that refuses usually sends Pairing Failed and drops
          ;; immediately after, so the frame and the disconnect can surface
          ;; from the same read. Look once more before blaming the link: the
          ;; reason it gave is worth more than the fact that it left.
          (let ((hit (claim))) (when hit (return hit)))
          (error 'smp-error :reason #x08 :source :disconnected))))))

(defun %smp-note-frame (conn frame)
  (setf (hci-conn-smp-pending conn)
        (nconc (hci-conn-smp-pending conn) (list frame))))

(setf *l2cap-smp-frame-handler* #'%smp-note-frame)

(defun smp-fail (conn reason)
  (ignore-errors (smp-send conn +smp-pairing-failed+ (vector reason)))
  (error 'smp-error :reason reason :source :local))

;;; --- pairing ------------------------------------------------------------

(defun %pairing-params (io &key mitm)
  "The three octets of a Pairing Request or Response that feed f6, plus the
key-distribution octets. Secure Connections and bonding are both requested;
without the SC bit the peer would answer with legacy pairing, which this does
not implement."
  (vector io                                   ; IO capability
          #x00                                 ; OOB not present
          (logior +smp-auth-bonding+ +smp-auth-sc+
                  (if mitm +smp-auth-mitm+ 0))))

(defun %send-pairing (conn opcode io &key mitm)
  (let ((p (cat (%pairing-params io :mitm mitm)
                (vector 16       ; max encryption key size
                        #x00     ; initiator key distribution: none
                        #x00)))) ; responder key distribution: none
    (smp-send conn opcode p)
    ;; What f6 wants is NOT what went on the wire. The wire order of a
    ;; Pairing Request is IO capability, OOB flag, AuthReq; f6 takes the same
    ;; three fields in the opposite order, AuthReq first, and without the
    ;; opcode. Feeding it the wire bytes produces a check value that is wrong
    ;; in a way both ends of a self-test share -- so they agree with each
    ;; other and fail against anything else. A phone rejected exactly this
    ;; with "DHKey check failed".
    (%iocap-triple (%pairing-params io :mitm mitm))))

(defun %iocap-triple (wire-params)
  "The three octets f6 wants, from a Pairing Request/Response's three: the
same fields, reversed."
  (vector (aref wire-params 2)      ; AuthReq
          (aref wire-params 1)      ; OOB data flag
          (aref wire-params 0)))    ; IO capability

(defun %peer-iocap (pdu)
  "The peer's IOcap triple, from the Pairing Request or Response it sent.
PDU includes the opcode, so the fields start at index 1."
  (vector (aref pdu 3)              ; AuthReq
          (aref pdu 2)              ; OOB data flag
          (aref pdu 1)))            ; IO capability

(defun %single-nonce-round (session conn initiator timeout-ms)
  "Just Works and numeric comparison: one exchange. Only the responder commits
to a confirm value -- the initiator reveals its nonce first, and the
responder\'s confirm is what binds it to a nonce chosen before it saw Na."
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
          (unless (equalp (smp-f4 (smp-session-peer-x session)
                                  (smp-session-local-x session)
                                  (smp-session-nb session) 0)
                          (msb (subseq cb 1 17)))
            (smp-fail conn #x04))))
      (progn
        (smp-send conn +smp-pairing-confirm+
                  (msb (smp-f4 (smp-session-local-x session)
                               (smp-session-peer-x session)
                               (smp-session-nb session) 0)))
        (let ((ra (smp-next conn :expect +smp-pairing-random+
                                 :timeout-ms timeout-ms)))
          (unless ra (smp-fail conn #x08))
          (setf (smp-session-na session) (msb (subseq ra 1 17))))
        (smp-send conn +smp-pairing-random+ (msb (smp-session-nb session))))))

(defun %passkey-rounds (session conn initiator passkey timeout-ms)
  "Twenty exchanges, one per bit of the passkey.

Each round commits both ends to a fresh nonce before either reveals it, with
the round\'s passkey bit mixed into the confirm. That is what makes passkey
entry resistant to a man in the middle: an attacker who guesses wrong on any
bit is caught on that round, and cannot go back. It is also why this is twenty
round trips rather than one -- the cost of leaking the passkey one bit at a
time instead of all at once.

The nonces from the LAST round are the Na and Nb that go on to f5 and f6."
  (dotimes (i 20)
    (let ((z (passkey-bit passkey i))
          (nonce (smp-random conn 16)))
      (if initiator
          (setf (smp-session-na session) nonce)
          (setf (smp-session-nb session) nonce))
      (flet ((my-confirm ()
               (msb (smp-f4 (smp-session-local-x session)
                            (smp-session-peer-x session)
                            (if initiator (smp-session-na session)
                                (smp-session-nb session))
                            z)))
             (check-peer (confirm peer-nonce)
               (unless (equalp (smp-f4 (smp-session-peer-x session)
                                       (smp-session-local-x session)
                                       peer-nonce z)
                               (msb (subseq confirm 1 17)))
                 (smp-fail conn #x04))))
        (if initiator
            (progn
              (smp-send conn +smp-pairing-confirm+ (my-confirm))
              (let ((cb (smp-next conn :expect +smp-pairing-confirm+
                                       :timeout-ms timeout-ms)))
                (unless cb (smp-fail conn #x08))
                (smp-send conn +smp-pairing-random+
                          (msb (smp-session-na session)))
                (let ((rb (smp-next conn :expect +smp-pairing-random+
                                         :timeout-ms timeout-ms)))
                  (unless rb (smp-fail conn #x08))
                  (setf (smp-session-nb session) (msb (subseq rb 1 17)))
                  (check-peer cb (smp-session-nb session)))))
            (let ((ca (smp-next conn :expect +smp-pairing-confirm+
                                     :timeout-ms timeout-ms)))
              (unless ca (smp-fail conn #x08))
              (smp-send conn +smp-pairing-confirm+ (my-confirm))
              (let ((ra (smp-next conn :expect +smp-pairing-random+
                                       :timeout-ms timeout-ms)))
                (unless ra (smp-fail conn #x08))
                (setf (smp-session-na session) (msb (subseq ra 1 17)))
                (check-peer ca (smp-session-na session))
                (smp-send conn +smp-pairing-random+
                          (msb (smp-session-nb session))))))))))

(defun smp-pair (conn &key (role :central) local-addr local-addr-type
                           peer-addr peer-addr-type (timeout-ms 20000)
                           (io-capability :no-input-no-output)
                           passkey passkey-fn confirm-fn)
  "Pair over an established connection and return an SMP-SESSION whose LTK is
ready to encrypt with.

ROLE :CENTRAL initiates. Addresses are in on-air order, as everywhere else in
this library; both are needed because f5 and f6 bind the keys to them, which
is what stops a derived key being replayed against a different pair of
devices."
  (let* ((initiator (eq role :central))
         (io (io-capability-code io-capability))
         ;; Asking for MITM protection only means anything if this device can
         ;; actually take part in it; claiming it with no input and no output
         ;; would negotiate a model that then falls back to Just Works anyway.
         (mitm (/= io +smp-io-no-input-no-output+))
         (model :just-works)
         (session (make-smp-session
                   :conn conn :role role
                   :local-addr (%smp-addr local-addr local-addr-type)
                   :peer-addr (%smp-addr peer-addr peer-addr-type))))
    ;; 1. feature exchange
    (if initiator
        (progn
          (setf (smp-session-local-io session)
                (%send-pairing conn +smp-pairing-request+ io :mitm mitm))
          (let ((rsp (smp-next conn :expect +smp-pairing-response+
                                    :timeout-ms timeout-ms)))
            (unless rsp (smp-fail conn #x08))
            (setf (smp-session-peer-io session) (%peer-iocap rsp))
            (unless (logtest +smp-auth-sc+ (aref rsp 3))
              ;; The peer wants legacy pairing. Refusing is the honest answer:
              ;; continuing would mean pretending to a security level we do
              ;; not implement.
              (smp-fail conn #x03))))
        (let ((req (smp-next conn :expect +smp-pairing-request+
                                  :timeout-ms timeout-ms)))
          (unless req (smp-fail conn #x08))
          (setf (smp-session-peer-io session) (%peer-iocap req))
          (unless (logtest +smp-auth-sc+ (aref req 3)) (smp-fail conn #x03))
          (setf (smp-session-local-io session)
                (%send-pairing conn +smp-pairing-response+ io :mitm mitm))))

    ;; The model falls out of both sides' capabilities; neither end chooses
    ;; it alone. Both compute it from the same two triples and must agree, or
    ;; one would be running twenty rounds while the other ran one.
    (let ((mine (smp-session-local-io session))
          (theirs (smp-session-peer-io session)))
      (setf model (if initiator
                      (smp-association-model (aref mine 2) (aref theirs 2)
                                             (aref mine 0) (aref theirs 0))
                      (smp-association-model (aref theirs 2) (aref mine 2)
                                             (aref theirs 0) (aref mine 0)))))
    (%trace-value "model" (vector (position model '(:just-works :passkey-entry
                                                    :numeric-comparison))))
    (when (eq model :passkey-entry)
      (setf passkey (or passkey
                        (and passkey-fn (funcall passkey-fn model))
                        (smp-fail conn #x01)))
      (unless (and (integerp passkey) (<= 0 passkey 999999))
        (smp-fail conn #x01)))

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

    ;; 3. confirm and nonces -- the one phase the association model changes.
    (when (eq model :passkey-entry)
      (%passkey-rounds session conn initiator passkey timeout-ms))
    (unless (eq model :passkey-entry)
      (%single-nonce-round session conn initiator timeout-ms))
    ;; 4. shared secret and keys
    (let* ((dhkey (smp-dhkey (smp-session-local-priv session)
                             (smp-session-peer-x session)
                             (smp-session-peer-y session)))
           (a1 (if initiator (smp-session-local-addr session)
                   (smp-session-peer-addr session)))
           (a2 (if initiator (smp-session-peer-addr session)
                   (smp-session-local-addr session))))
      (%trace-value "dhkey" dhkey)
      (%trace-value "Na" (smp-session-na session))
      (%trace-value "Nb" (smp-session-nb session))
      (%trace-value "A (init)" a1)
      (%trace-value "B (resp)" a2)
      (%trace-value "IOcap loc" (smp-session-local-io session))
      (%trace-value "IOcap peer" (smp-session-peer-io session))
      (multiple-value-bind (mackey ltk)
          (smp-f5 dhkey (smp-session-na session) (smp-session-nb session) a1 a2)
        (setf (smp-session-mackey session) mackey
              (smp-session-ltk session) ltk)
        (%trace-value "mackey" mackey)
        (%trace-value "ltk" ltk)

        ;; 5. check values, each computed over the other side's IO capability
        ;;    and address, so agreeing proves both derived the same key from
        ;;    the same transcript.
        (when (eq model :numeric-comparison)
          (let ((digits (smp-g2 (if initiator (smp-session-local-x session)
                                    (smp-session-peer-x session))
                                (if initiator (smp-session-peer-x session)
                                    (smp-session-local-x session))
                                (smp-session-na session)
                                (smp-session-nb session))))
            ;; Both ends show the same six digits; a person says whether they
            ;; match. Refusing is what stops a man in the middle, so a caller
            ;; with no way to ask must not silently accept.
            (unless (and confirm-fn (funcall confirm-fn digits))
              (smp-fail conn #x0C))))
        (let* ((zero (if (eq model :passkey-entry)
                         (passkey-octets passkey)
                         (make-octets 16)))
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
                  (%trace-value "Ea wanted" ea)
                  (%trace-value "Ea got" (msb (subseq got 1 17)))
                  (unless (equalp ea (msb (subseq got 1 17)))
                    (smp-fail conn #x0B)))
                (%trace-value "Eb sent" eb)
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

(defun smp-request-security (conn &key (bonding t))
  "Ask the central to start pairing. Peripheral only.

A peripheral cannot initiate pairing -- only the central sends a Pairing
Request -- so this is how one says it would like the link secured. It is what
makes a phone offer to pair rather than sit connected and unbonded: without
it, the central has no reason to think anything is wanted."
  (smp-send conn +smp-security-request+
            (vector (logior (if bonding +smp-auth-bonding+ 0) +smp-auth-sc+))))

(defun smp-peer-addr-type (connection-complete-event)
  "The peer's address type from an LE Connection Complete event.

Worth reading rather than assuming: phones connect from a resolvable private
address, so a peripheral that assumes :PUBLIC feeds the wrong type octet into
f5 and derives a key the peer will not agree with -- and the failure surfaces
as a check-value mismatch, which reads like a crypto bug rather than a
one-octet mistake."
  (if (and (>= (length connection-complete-event) 9)
           (= 1 (aref connection-complete-event 8)))
      :random :public))
