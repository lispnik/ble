(in-package #:ble)

;;; Enhanced ATT bearers.
;;;
;;; Ordinary ATT gets one channel per connection -- fixed CID 0x0004 -- and one
;;; outstanding request on it. A slow read blocks everything behind it,
;;; including notifications, because there is nowhere else for them to go.
;;; EATT gives ATT several channels instead, each carrying its own
;;; transaction, so a long operation on one no longer stops the others.
;;;
;;; WHAT THIS ACTUALLY BUYS, stated plainly because it is easy to oversell:
;;; interop, and independence. Android 12 and recent iOS open EATT when they
;;; see the PSM, so a peripheral that offers it talks to them the way they
;;; expect. And a notification can go out while a long read is in flight. It
;;; does NOT buy throughput -- this library is single threaded, so the bearers
;;; are independent, not parallel. Nothing here runs at the same time as
;;; anything else.
;;;
;;; IT DOES NOT NEED BLUETOOTH 5.2 HARDWARE, whatever the version it was
;;; introduced in suggests. EATT is L2CAP, which is host layer: the frames are
;;; ACL payload and the controller never looks inside them. The tests here run
;;; between two Bluetooth 5.1 dongles. The README claimed otherwise for a long
;;; time and was wrong.
;;;
;;; The transport underneath is Enhanced Credit Based Flow Control mode, which
;;; is the same K-frame format as the LE credit-based channels in
;;; l2cap-coc.lisp -- two octets of SDU length on the first frame, credits
;;; granted by the receiver -- so all of that is reused rather than rewritten.
;;; One SDU is exactly one ATT PDU, which is what makes the fit so clean: ATT
;;; needs message boundaries and a credit-based SDU already is one.
;;;
;;; What is genuinely new is the signalling that opens several channels in a
;;; single exchange, and PSM 0x0027.

;;; --- the wire ----------------------------------------------------------
;;;
;;; Field layouts taken from a shipping implementation rather than from
;;; memory (Linux, include/net/bluetooth/l2cap.h), because two of them are not
;;; what you would guess. See the note on Reconfigure below.

(defconstant +l2cap-ecred-conn-req+   #x17)
(defconstant +l2cap-ecred-conn-rsp+   #x18)
(defconstant +l2cap-ecred-reconf-req+ #x19)
(defconstant +l2cap-ecred-reconf-rsp+ #x1A)

(defconstant +psm-eatt+ #x0027
  "The SPSM Enhanced ATT bearers are opened to.")

(defconstant +ecred-min-mtu+ 64
  "An enhanced bearer may not be narrower than this, unlike the fixed ATT
channel, whose floor is 23.")
(defconstant +ecred-min-mps+ 64)
(defconstant +ecred-max-cid+ 5
  "Channels per connection request. Not a limit on bearers in total, but a
second request is a second round trip.")

;;; Connection response results. The distinction that matters when reading
;;; them: `all refused' is a flat no, while `some refused' means the response
;;; still carries destination CIDs and some channels really did open. Treating
;;; the second as failure throws away working bearers.
(defconstant +ecred-success+                     #x0000)
(defconstant +ecred-spsm-not-supported+          #x0002)
(defconstant +ecred-some-refused-no-resources+   #x0004)
(defconstant +ecred-insufficient-authentication+ #x0005)
(defconstant +ecred-insufficient-authorization+  #x0006)
(defconstant +ecred-key-size-too-short+          #x0007)
(defconstant +ecred-insufficient-encryption+     #x0008)
(defconstant +ecred-invalid-source-cid+          #x0009)
(defconstant +ecred-source-cid-allocated+        #x000A)
(defconstant +ecred-unacceptable-parameters+     #x000B)

(defconstant +reconf-success+        #x0000)
(defconstant +reconf-invalid-mtu+    #x0001)
(defconstant +reconf-invalid-mps+    #x0002)
(defconstant +reconf-invalid-cid+    #x0003)
(defconstant +reconf-invalid-params+ #x0004)

(defparameter *eatt-default-mtu* 517
  "The largest ATT PDU worth carrying: a 512-octet value plus its header.")
(defparameter *eatt-default-mps* 247)
(defparameter *eatt-default-credits* 10)
(defparameter *eatt-default-bearers* 2
  "Two is enough to get the property people want -- something else can proceed
while one bearer is busy -- without spending five CIDs to get it.")

(defstruct (eatt-bearer (:constructor %make-eatt-bearer (coc)))
  "One Enhanced ATT bearer.

A thin wrapper over the credit-based channel rather than a flag on it: the
CoC layer has no business knowing that some of its channels carry ATT, and
ATT-SEND needs something unambiguous to dispatch on."
  coc)

(defun eatt-bearer-mtu (bearer)
  "The ATT MTU for this bearer.

The smaller of the two directions, because a PDU has to be both sendable and
receivable. Note what is NOT involved: Exchange MTU. On an enhanced bearer
the MTU is settled by L2CAP when the channel opens, and the ATT PDU that
would renegotiate it is prohibited."
  (let ((coc (eatt-bearer-coc bearer)))
    (min (l2cap-coc-mtu coc) (l2cap-coc-peer-mtu coc))))

(defun eatt-bearer-open-p (bearer)
  (and bearer (not (l2cap-coc-closed (eatt-bearer-coc bearer)))))

(defun %eatt-register (conn coc)
  "Put a new bearer on both lists it has to be on.

COC-CHANNELS is what routes incoming frames to it -- %COC-NOTE-FRAME finds
channels there and nowhere else -- and EATT-BEARERS is what serving walks, so
that answering ATT does not mean walking every credit-based channel on the
link looking for the ones that happen to carry it."
  (let ((bearer (%make-eatt-bearer coc)))
    (push (cons (l2cap-coc-scid coc) coc) (hci-conn-coc-channels conn))
    (setf (hci-conn-eatt-bearers conn)
          (nconc (hci-conn-eatt-bearers conn) (list bearer)))
    ;; Seed the negotiated MTU so ATT-MTU answers for this bearer without an
    ;; exchange ever happening on it.
    (setf (gethash bearer *att-negotiated-mtu*) (eatt-bearer-mtu bearer))
    bearer))

;;; --- opening, as the initiator ------------------------------------------

(defun eatt-connect (conn &key (count *eatt-default-bearers*)
                               (mtu *eatt-default-mtu*)
                               (mps *eatt-default-mps*)
                               (credits *eatt-default-credits*)
                               (timeout-ms 8000))
  "Open COUNT Enhanced ATT bearers to the peer. Returns a list of
EATT-BEARER, or a keyword: :TIMEOUT, :DISCONNECTED, or :REFUSED-<n>.

A partial success is a success. When the peer accepts some of the requested
channels and refuses the rest for want of resources, the ones it accepted are
returned and the rest are simply not there -- so a caller asking for four and
getting two carries on with two, rather than treating the whole thing as a
failure and falling back to the fixed channel it did not need to."
  (assert (<= 1 count +ecred-max-cid+) (count)
          "COUNT must be 1..~D, got ~D" +ecred-max-cid+ count)
  (assert (>= mtu +ecred-min-mtu+) (mtu)
          "an enhanced bearer's MTU floor is ~D, got ~D" +ecred-min-mtu+ mtu)
  (assert (>= mps +ecred-min-mps+) (mps)
          "an enhanced bearer's MPS floor is ~D, got ~D" +ecred-min-mps+ mps)
  (let* ((scids (loop repeat count collect (%coc-alloc-cid conn)))
         (ident (%next-sig-ident))
         (d (make-octets (+ 8 (* 2 count)))))
    (u16le-put d 0 +psm-eatt+)
    (u16le-put d 2 mtu)
    (u16le-put d 4 mps)
    (u16le-put d 6 credits)
    (loop for cid in scids
          for off from 8 by 2
          do (u16le-put d off cid))
    ;; Recorded before sending, not after: the collision we are looking for is
    ;; the peer's request arriving while ours is in flight, and that can
    ;; happen before this call gets its next chance to run.
    (setf (hci-conn-eatt-pending conn) ident)
    (unwind-protect
         (progn
           (%l2cap-send-sig conn +l2cap-ecred-conn-req+ ident d)
           (let ((deadline (+ (get-internal-real-time)
                              (round (* timeout-ms internal-time-units-per-second)
                                     1000))))
             (loop
               (let ((hit (find-if (lambda (f)
                                     (and (>= (length f) 12)
                                          (= (aref f 0) +l2cap-ecred-conn-rsp+)
                                          (= (aref f 1) ident)))
                                   (hci-conn-sig-pending conn))))
                 (when hit
                   (setf (hci-conn-sig-pending conn)
                         (remove hit (hci-conn-sig-pending conn) :count 1))
                   (return (%eatt-finish-connect conn hit scids mtu mps credits))))
               (when (<= (- deadline (get-internal-real-time)) 0) (return :timeout))
               (when (eq :disconnected (hci-pump conn 200)) (return :disconnected)))))
      (setf (hci-conn-eatt-pending conn) nil))))

(defun %eatt-finish-connect (conn rsp scids mtu mps credits)
  "Turn a Connection Response into bearers.

The destination CIDs are positional: the Nth answers the Nth source CID we
sent. A zero in that position means that particular channel was refused, and
the response can carry a mix -- which is why this walks them in step with our
own CIDs rather than counting how many came back."
  (let* ((peer-mtu (u16-le rsp 4))
         (peer-mps (u16-le rsp 6))
         (peer-credits (u16-le rsp 8))
         (result (u16-le rsp 10))
         (n (floor (- (length rsp) 12) 2))
         (bearers '()))
    (when (and (/= result +ecred-success+)
               (/= result +ecred-some-refused-no-resources+)
               (/= result +ecred-invalid-source-cid+)
               (/= result +ecred-source-cid-allocated+))
      (return-from %eatt-finish-connect
        (intern (format nil "REFUSED-~D" result) :keyword)))
    (loop for cid in scids
          for i from 0
          while (< i n)
          do (let ((dcid (u16-le rsp (+ 12 (* 2 i)))))
               (when (plusp dcid)
                 (push (%eatt-register
                        conn
                        (%make-l2cap-coc :conn conn :scid cid :dcid dcid
                                         :peer-mtu peer-mtu :peer-mps peer-mps
                                         :mtu mtu :mps mps
                                         :tx-credits peer-credits
                                         :rx-credits credits))
                       bearers))))
    (or (nreverse bearers)
        (intern (format nil "REFUSED-~D" result) :keyword))))

;;; --- opening, as the responder ------------------------------------------

(defstruct (eatt-listener (:constructor %make-eatt-listener))
  "What to do when a peer asks for bearers. ENCRYPTED-P is a thunk rather than
a flag because L2CAP cannot see the link's security state -- the same reason
GATT-SERVER carries ENCRYPTED and is told about it from outside."
  (mtu *eatt-default-mtu*) (mps *eatt-default-mps*)
  (credits *eatt-default-credits*) (max-bearers +ecred-max-cid+)
  (encrypted-p nil))

(defun eatt-listen (conn &key (mtu *eatt-default-mtu*)
                              (mps *eatt-default-mps*)
                              (credits *eatt-default-credits*)
                              (max-bearers +ecred-max-cid+)
                              encrypted-p)
  "Accept Enhanced ATT bearers on this connection.

ENCRYPTED-P, when given, is called with no arguments and must answer whether
the link is encrypted; bearers are refused until it does. EATT is expected to
run over an encrypted link, and a peripheral that hands out bearers before
pairing has quietly moved its whole attribute database onto a channel it
never secured."
  (setf (hci-conn-eatt-listener conn)
        (%make-eatt-listener :mtu mtu :mps mps :credits credits
                             :max-bearers max-bearers
                             :encrypted-p encrypted-p))
  conn)

(defun %eatt-refuse (conn ident result)
  (let ((rsp (make-octets 8)))
    (u16le-put rsp 6 result)
    (%l2cap-send-sig conn +l2cap-ecred-conn-rsp+ ident rsp)))

(defun %eatt-yields-on-collision-p (conn)
  "Whether we abandon our own request when the peer's arrives at the same time.

THE ONE RULE HERE THAT IS NOT CITED. Both peers opening bearers at once needs
a tie-break, or each refuses the other and neither gets any. Resolving it by
role is the obvious answer and matches how L2CAP breaks ties elsewhere, so
the Peripheral yields; but the Linux kernel implements credit-based channels
without implementing EATT itself -- that lives in the host profile -- so
there was no shipping implementation here to read it off, and it is written
from reasoning rather than from the specification text.

Kept as its own function so that being wrong costs one line. What is not at
risk either way is CID bookkeeping: whichever side yields still frees its own
CIDs, so a wrong guess costs a round trip, not a leak."
  (eq :peripheral (hci-conn-role conn)))

(defun %eatt-handle-connect-request (conn frame)
  (let* ((ident (aref frame 1))
         (psm (u16-le frame 4))
         (peer-mtu (u16-le frame 6))
         (peer-mps (u16-le frame 8))
         (peer-credits (u16-le frame 10))
         (n (floor (- (length frame) 12) 2))
         (peer-cids (loop for i below n collect (u16-le frame (+ 12 (* 2 i)))))
         (listener (hci-conn-eatt-listener conn)))
    (cond
      ((/= psm +psm-eatt+)
       (%eatt-refuse conn ident +ecred-spsm-not-supported+))
      ((null listener)
       (%eatt-refuse conn ident +ecred-spsm-not-supported+))
      ((or (< peer-mtu +ecred-min-mtu+) (< peer-mps +ecred-min-mps+)
           (zerop n) (> n +ecred-max-cid+)
           (some #'zerop peer-cids))
       (%eatt-refuse conn ident +ecred-unacceptable-parameters+))
      ;; A CID the peer is already using for something else would leave two
      ;; channels indistinguishable on receive.
      ((some (lambda (c) (find c (hci-conn-coc-channels conn)
                               :key (lambda (e) (l2cap-coc-dcid (cdr e)))))
             peer-cids)
       (%eatt-refuse conn ident +ecred-source-cid-allocated+))
      ;; Both of us asked at once. One side has to give way.
      ((and (hci-conn-eatt-pending conn)
            (not (%eatt-yields-on-collision-p conn)))
       (%eatt-refuse conn ident +ecred-unacceptable-parameters+))
      ((let ((p (eatt-listener-encrypted-p listener)))
         (and p (not (funcall p))))
       (%eatt-refuse conn ident +ecred-insufficient-authentication+))
      (t
       ;; If we were mid-request ourselves, this is the collision and we are
       ;; the one yielding: drop our request on the floor so its response,
       ;; when it comes, finds nobody waiting.
       (setf (hci-conn-eatt-pending conn) nil)
       (let* ((room (max 0 (- (eatt-listener-max-bearers listener)
                              (length (hci-conn-eatt-bearers conn)))))
              (take (min (length peer-cids) room))
              (rsp (make-octets (+ 8 (* 2 (length peer-cids)))))
              (mtu (eatt-listener-mtu listener))
              (mps (eatt-listener-mps listener))
              (credits (eatt-listener-credits listener)))
         (u16le-put rsp 0 mtu)
         (u16le-put rsp 2 mps)
         (u16le-put rsp 4 credits)
         (u16le-put rsp 6 (if (= take (length peer-cids))
                              +ecred-success+
                              +ecred-some-refused-no-resources+))
         (loop for peer-cid in peer-cids
               for i from 0
               do (if (< i take)
                      (let* ((scid (%coc-alloc-cid conn))
                             (coc (%make-l2cap-coc
                                   :conn conn :scid scid :dcid peer-cid
                                   :peer-mtu peer-mtu :peer-mps peer-mps
                                   :mtu mtu :mps mps
                                   :tx-credits peer-credits
                                   :rx-credits credits))
                             (bearer (%eatt-register conn coc)))
                        (setf (hci-conn-eatt-incoming conn)
                              (nconc (hci-conn-eatt-incoming conn) (list bearer)))
                        (u16le-put rsp (+ 8 (* 2 i)) scid))
                      ;; Zero in this slot: that one was refused.
                      (u16le-put rsp (+ 8 (* 2 i)) 0)))
         (%l2cap-send-sig conn +l2cap-ecred-conn-rsp+ ident rsp))))))

(defun eatt-accept (conn &key (timeout-ms 10000))
  "Hand over a bearer a peer has opened, or NIL on timeout.

The bearers themselves are created on the receive path as the request
arrives; this only collects them, the same division as L2CAP-COC-ACCEPT."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms internal-time-units-per-second) 1000))))
    (loop
      (when (hci-conn-eatt-incoming conn)
        (return (pop (hci-conn-eatt-incoming conn))))
      (when (<= (- deadline (get-internal-real-time)) 0) (return nil))
      (when (eq :disconnected (hci-pump conn 200)) (return :disconnected)))))

;;; --- reconfiguring ------------------------------------------------------
;;;
;;; THE CID CONVENTION IS BACKWARDS HERE, and it is the one thing in EATT most
;;; likely to be got wrong silently. Every other L2CAP command that names a
;;; channel uses Destination CID for `the CID at the other end' --
;;; Disconnection Request does, and l2cap-coc-close sends it that way. The
;;; Reconfigure Request instead carries the SENDER's own source CIDs, and the
;;; receiver resolves them against the CIDs it sends to. Linux does exactly
;;; this: __l2cap_get_chan_by_dcid(conn, scid), in l2cap_ecred_reconf_req.
;;;
;;; So: sending, we put our SCIDs in. Receiving, we match against our DCIDs.
;;; Reversed, every CID fails to resolve and the peer answers `invalid CID'
;;; for channels that are plainly open.

(defun eatt-reconfigure (bearers &key mtu mps (timeout-ms 8000))
  "Raise the MTU, and optionally the MPS, of bearers already open.

Returns T, or a keyword: :TIMEOUT, :DISCONNECTED, or :REFUSED-<n>.

Only ever an increase for MTU -- the peer is required to refuse a reduction,
because it may already have sent a PDU sized by the old value. MPS may be
reduced, but only when exactly one bearer is being reconfigured; with more
than one the peer must refuse that too."
  (let* ((bearers (remove-if-not #'eatt-bearer-open-p bearers))
         (cocs (mapcar #'eatt-bearer-coc bearers)))
    (when (null cocs) (return-from eatt-reconfigure t))
    (let ((mtu (or mtu (reduce #'max cocs :key #'l2cap-coc-mtu)))
          (mps (or mps (reduce #'max cocs :key #'l2cap-coc-mps))))
      (assert (>= mtu +ecred-min-mtu+) (mtu))
      (assert (>= mps +ecred-min-mps+) (mps))
      (when (some (lambda (c) (> (l2cap-coc-mtu c) mtu)) cocs)
        (return-from eatt-reconfigure
          (intern (format nil "REFUSED-~D" +reconf-invalid-mtu+) :keyword)))
      (when (and (> (length cocs) 1)
                 (some (lambda (c) (> (l2cap-coc-mps c) mps)) cocs))
        (return-from eatt-reconfigure
          (intern (format nil "REFUSED-~D" +reconf-invalid-mps+) :keyword)))
      (let* ((conn (l2cap-coc-conn (first cocs)))
             (ident (%next-sig-ident))
             (d (make-octets (+ 4 (* 2 (length cocs))))))
        (u16le-put d 0 mtu)
        (u16le-put d 2 mps)
        (loop for c in cocs
              for off from 4 by 2
              do (u16le-put d off (l2cap-coc-scid c)))   ; ours, see above
        (%l2cap-send-sig conn +l2cap-ecred-reconf-req+ ident d)
        (let ((deadline (+ (get-internal-real-time)
                           (round (* timeout-ms internal-time-units-per-second)
                                  1000))))
          (loop
            (let ((hit (find-if (lambda (f)
                                  (and (>= (length f) 6)
                                       (= (aref f 0) +l2cap-ecred-reconf-rsp+)
                                       (= (aref f 1) ident)))
                                (hci-conn-sig-pending conn))))
              (when hit
                (setf (hci-conn-sig-pending conn)
                      (remove hit (hci-conn-sig-pending conn) :count 1))
                (let ((result (u16-le hit 4)))
                  (return
                    (cond
                      ((/= result +reconf-success+)
                       (intern (format nil "REFUSED-~D" result) :keyword))
                      (t
                       (dolist (b bearers t)
                         (let ((c (eatt-bearer-coc b)))
                           (setf (l2cap-coc-mtu c) mtu
                                 (l2cap-coc-mps c) mps
                                 (gethash b *att-negotiated-mtu*)
                                 (eatt-bearer-mtu b))))))))))
            (when (<= (- deadline (get-internal-real-time)) 0) (return :timeout))
            (when (eq :disconnected (hci-pump conn 200)) (return :disconnected))))))))

(defun %eatt-handle-reconfigure-request (conn frame)
  (let* ((ident (aref frame 1))
         (mtu (u16-le frame 4))
         (mps (u16-le frame 6))
         (n (floor (- (length frame) 8) 2))
         (cids (loop for i below n collect (u16-le frame (+ 8 (* 2 i)))))
         (result +reconf-success+)
         (cocs '()))
    (cond
      ((or (< mtu +ecred-min-mtu+) (< mps +ecred-min-mps+)
           (zerop n) (> n +ecred-max-cid+))
       (setf result +reconf-invalid-params+))
      (t
       (loop for cid in cids
             for i from 0
             do (let ((coc (find cid (hci-conn-coc-channels conn)
                                 :key (lambda (e) (l2cap-coc-dcid (cdr e))))))
                  (cond
                    ((or (zerop cid) (null coc))
                     (setf result +reconf-invalid-cid+) (return))
                    ;; Never narrower than it already is: we may already have
                    ;; sent something sized by the old value.
                    ((> (l2cap-coc-peer-mtu (cdr coc)) mtu)
                     (setf result +reconf-invalid-mtu+) (return))
                    ;; One channel may shrink its MPS; several may not.
                    ((and (plusp i) (> (l2cap-coc-peer-mps (cdr coc)) mps))
                     (setf result +reconf-invalid-mps+) (return))
                    (t (push (cdr coc) cocs)))))))
    ;; Commit only once every channel has passed, so a request that is refused
    ;; leaves nothing half-applied.
    (when (= result +reconf-success+)
      (dolist (c cocs)
        (setf (l2cap-coc-peer-mtu c) mtu
              (l2cap-coc-peer-mps c) mps))
      (dolist (b (hci-conn-eatt-bearers conn))
        (when (member (eatt-bearer-coc b) cocs)
          (setf (gethash b *att-negotiated-mtu*) (eatt-bearer-mtu b)))))
    (let ((rsp (make-octets 2)))
      (u16le-put rsp 0 result)
      (%l2cap-send-sig conn +l2cap-ecred-reconf-rsp+ ident rsp))))

;;; --- closing and serving ------------------------------------------------

(defun eatt-close (bearer)
  "Close one bearer and tell the peer."
  (when bearer
    (let ((conn (l2cap-coc-conn (eatt-bearer-coc bearer))))
      (l2cap-coc-close (eatt-bearer-coc bearer))
      (setf (hci-conn-eatt-bearers conn)
            (remove bearer (hci-conn-eatt-bearers conn)))
      (remhash bearer *att-negotiated-mtu*))
    t))

(defun eatt-close-all (conn)
  (mapc #'eatt-close (copy-list (hci-conn-eatt-bearers conn)))
  (setf (hci-conn-eatt-bearers conn) '()
        (hci-conn-eatt-incoming conn) '())
  t)

(defun eatt-pending-pdu-p (bearer)
  "Whether a whole ATT PDU is already waiting on this bearer.

Asked rather than read because serving several bearers must not block on any
one of them: L2CAP-COC-RECV waits out its timeout, which would let the first
idle bearer starve the rest."
  (and (eatt-bearer-open-p bearer)
       (l2cap-coc-sdus (eatt-bearer-coc bearer))))

(defun eatt-serve (server conn)
  "Answer one waiting request on each bearer that has one. Returns how many.

Never blocks. Bearers with nothing pending are skipped, so this is safe to
call from a tick loop beside the fixed channel's own GATT-SERVE."
  (let ((handled 0))
    (dolist (bearer (copy-list (hci-conn-eatt-bearers conn)) handled)
      (when (eatt-pending-pdu-p bearer)
        (let ((pdu (pop (l2cap-coc-sdus (eatt-bearer-coc bearer)))))
          ;; Bound for the whole of serving: every response this request
          ;; produces is sized by THIS bearer's MTU, not the server's, which
          ;; belongs to the fixed channel and is very likely a different
          ;; number.
          (let ((*att-bearer-mtu* (eatt-bearer-mtu bearer)))
            (gatt-serve-pdu server bearer pdu))
          (incf handled))))))

;;; --- signalling ---------------------------------------------------------

(defun %eatt-handle-signalling (conn frame)
  "The EATT half of the signalling channel. Returns T if FRAME was ours."
  (let ((code (aref frame 0)))
    (cond
      ((= code +l2cap-ecred-conn-req+)
       (when (>= (length frame) 14) (%eatt-handle-connect-request conn frame)) t)
      ((= code +l2cap-ecred-reconf-req+)
       (when (>= (length frame) 10) (%eatt-handle-reconfigure-request conn frame)) t)
      ;; Responses belong to whoever is blocked waiting for them, and that is
      ;; not this call. Claim them so nothing else answers, and ask for them
      ;; to be put back -- the same contract the CoC handler uses.
      ((= code +l2cap-ecred-conn-rsp+) :leave)
      ((= code +l2cap-ecred-reconf-rsp+) :leave)
      (t nil))))

(setf *l2cap-eatt-signalling-handler* #'%eatt-handle-signalling)
