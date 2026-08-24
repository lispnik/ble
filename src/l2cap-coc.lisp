(in-package #:ble)

;;; L2CAP connection-oriented channels: LE Credit Based Flow Control mode.
;;;
;;; A stream between two devices that is not GATT. ATT moves a value at a time
;;; and pays a round trip for each; a CoC carries an SDU of up to 64 KiB and is
;;; the right shape for bulk -- a firmware image, a log download, an
;;; Object Transfer profile.
;;;
;;; The flow control is the substance of it, and it runs the opposite way from
;;; most protocols: a sender may transmit exactly as many frames as the
;;; receiver has granted it credits for, and not one more. That makes overrun
;;; structurally impossible rather than merely unlikely -- there is no window
;;; to misjudge and no rate to tune -- but it does mean a sender with no
;;; credits must stop, and a receiver that forgets to replenish silently
;;; wedges the channel. Replenishing is therefore done here, on receipt,
;;; rather than left to the caller to remember.
;;;
;;; Two sizes, easily confused: MTU is the largest SDU the peer will accept,
;;; MPS the largest single frame. One SDU is split across as many frames as
;;; MPS requires, and the first carries a 2-octet length so the far end knows
;;; when it has the whole thing.

(defconstant +l2cap-le-credit-conn-req+     #x14)
(defconstant +l2cap-le-credit-conn-rsp+     #x15)
(defconstant +l2cap-flow-control-credit+    #x16)
(defconstant +l2cap-disconnection-req+      #x06)
(defconstant +l2cap-disconnection-rsp+      #x07)

(defconstant +coc-success+              #x0000)
(defconstant +coc-spsm-not-supported+   #x0002)
(defconstant +coc-no-resources+         #x0004)

(defconstant +coc-cid-min+ #x0040)
(defconstant +coc-cid-max+ #x007F)

(defparameter *coc-default-mtu* 512)
(defparameter *coc-default-mps* 96
  "Largest single frame. Kept under a typical ACL length so one frame is one
ACL packet; larger works, and simply costs fragmentation underneath.")
(defparameter *coc-default-credits* 10)

(defstruct (l2cap-coc (:constructor %make-l2cap-coc))
  "One connection-oriented channel.

TX-CREDITS is what the peer has granted us and is the hard limit on what we
may send; RX-CREDITS is what we have granted it. RXBUF and RX-SDU-LEN hold a
partially received SDU between frames."
  conn scid dcid peer-mtu peer-mps (mtu *coc-default-mtu*) (mps *coc-default-mps*)
  (tx-credits 0) (rx-credits 0)
  (rxbuf (make-octets 0)) (rx-sdu-len nil) (sdus '()) (closed nil))

(defun %coc-alloc-cid (conn)
  (let ((cid (hci-conn-coc-next-cid conn)))
    (when (> cid +coc-cid-max+)
      (error 'ble-error))
    (setf (hci-conn-coc-next-cid conn) (1+ cid))
    cid))

(defun %coc-find (conn cid)
  (cdr (assoc cid (hci-conn-coc-channels conn))))

;;; --- pumping the transport ---------------------------------------------

(defun %pump (conn timeout-ms)
  "Read one packet and route it, without consuming anything a caller may be
waiting for.

The distinction that matters: ATT PDUs are left in PENDING for the ATT layer,
not swallowed. Every bug this file's neighbours have had came from a helper
that needed the transport, took it over, and discarded what it was not itself
looking for."
  (let ((pkt (hci-poll-read (hci-conn-sock conn) timeout-ms)))
    (cond
      ((null pkt) nil)
      ((and (>= (length pkt) 2) (= (aref pkt 0) #x04)
            (= (aref pkt 1) +hci-disconn-complete-evt+))
       :disconnected)
      ((and (= (aref pkt 0) #x02) (>= (length pkt) 5))
       (let* ((flags (u16-le pkt 1))
              (pb (logand (ash flags -12) #x3))
              (acl-len (u16-le pkt 3))
              (data (subseq pkt 5 (min (length pkt) (+ 5 acl-len)))))
         (setf (hci-conn-rxbuf conn)
               (if (= pb #x01)
                   (concatenate '(simple-array (unsigned-byte 8) (*))
                                (hci-conn-rxbuf conn) data)
                   (coerce-octets data)))
         (%drain-l2cap-frames conn)
         (%maybe-serve-signalling conn)
         t))
      (t t))))

;;; --- data ---------------------------------------------------------------

(defun %coc-note-frame (conn cid frame)
  "One K-frame has arrived on CID. Reassemble, and pay a credit back."
  (let ((coc (%coc-find conn cid)))
    (when coc
      (decf (l2cap-coc-rx-credits coc))
      (if (null (l2cap-coc-rx-sdu-len coc))
          ;; First frame of an SDU: it carries the total length ahead of the
          ;; payload.
          (when (>= (length frame) 2)
            (setf (l2cap-coc-rx-sdu-len coc) (u16-le frame 0)
                  (l2cap-coc-rxbuf coc) (subseq frame 2)))
          (setf (l2cap-coc-rxbuf coc)
                (concatenate '(simple-array (unsigned-byte 8) (*))
                             (l2cap-coc-rxbuf coc) frame)))
      (let ((want (l2cap-coc-rx-sdu-len coc)))
        (when (and want (>= (length (l2cap-coc-rxbuf coc)) want))
          (setf (l2cap-coc-sdus coc)
                (nconc (l2cap-coc-sdus coc)
                       (list (subseq (l2cap-coc-rxbuf coc) 0 want)))
                (l2cap-coc-rxbuf coc) (make-octets 0)
                (l2cap-coc-rx-sdu-len coc) nil)))
      ;; Replenish before the peer runs dry. A receiver that lets credits
      ;; reach zero stops the sender dead, and nothing reports it -- the
      ;; channel simply goes quiet, which is indistinguishable from a peer
      ;; with nothing to say.
      (when (<= (l2cap-coc-rx-credits coc) (floor *coc-default-credits* 2))
        (l2cap-coc-give-credits coc *coc-default-credits*)))))

(setf *l2cap-coc-frame-handler* #'%coc-note-frame)

(defun l2cap-coc-give-credits (coc n)
  "Grant the peer N more frames' worth of sending."
  (let ((d (make-octets 4)))
    (u16le-put d 0 (l2cap-coc-scid coc))
    (u16le-put d 2 n)
    (%l2cap-send-sig (l2cap-coc-conn coc) +l2cap-flow-control-credit+
                     (%next-sig-ident) d)
    (incf (l2cap-coc-rx-credits coc) n)))

(defun l2cap-coc-send (coc sdu &key (timeout-ms 5000))
  "Send one SDU. Returns T, :NO-CREDITS on timeout waiting for them,
:DISCONNECTED, or :TOO-LARGE if the peer said it would not accept this much.

Blocks only for credits, and pumps the transport while it waits rather than
sleeping: the credits it is waiting for arrive on that transport."
  (let* ((sdu (coerce-octets sdu))
         (conn (l2cap-coc-conn coc))
         (mps (l2cap-coc-peer-mps coc))
         (deadline (+ (get-internal-real-time)
                      (round (* timeout-ms internal-time-units-per-second) 1000))))
    (when (> (length sdu) (l2cap-coc-peer-mtu coc))
      (return-from l2cap-coc-send :too-large))
    ;; The first frame carries the SDU length; the rest are payload only.
    (let ((first (make-octets (+ 2 (min (- mps 2) (length sdu))))))
      (u16le-put first 0 (length sdu))
      (replace first sdu :start1 2 :end2 (- (length first) 2))
      (let ((frames (list first))
            (off (- (length first) 2)))
        (loop while (< off (length sdu))
              do (let ((end (min (length sdu) (+ off mps))))
                   (push (subseq sdu off end) frames)
                   (setf off end)))
        (dolist (f (nreverse frames) t)
          (loop while (<= (l2cap-coc-tx-credits coc) 0)
                do (when (<= (- deadline (get-internal-real-time)) 0)
                     (return-from l2cap-coc-send :no-credits))
                   (when (eq :disconnected (%pump conn 200))
                     (return-from l2cap-coc-send :disconnected)))
          (hci-acl-send-l2cap conn (l2cap-coc-dcid coc) f)
          (decf (l2cap-coc-tx-credits coc)))))))

(defun l2cap-coc-recv (coc &key (timeout-ms 5000))
  "The next complete SDU, or NIL on timeout, or :DISCONNECTED."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms internal-time-units-per-second) 1000))))
    (loop
      (when (l2cap-coc-sdus coc) (return (pop (l2cap-coc-sdus coc))))
      (when (<= (- deadline (get-internal-real-time)) 0) (return nil))
      (when (eq :disconnected (%pump (l2cap-coc-conn coc) 200))
        (return :disconnected)))))

;;; --- opening and closing ------------------------------------------------

(defun l2cap-coc-connect (conn spsm &key (mtu *coc-default-mtu*)
                                         (mps *coc-default-mps*)
                                         (credits *coc-default-credits*)
                                         (timeout-ms 8000))
  "Open a channel to the peer's SPSM. Returns an L2CAP-COC, or a keyword:
:TIMEOUT, :DISCONNECTED, or :REFUSED-<n> carrying the peer's result code.

SPSM is the simplified protocol/service multiplexer -- the LE equivalent of a
port number. Below 0x0080 they are assigned by the SIG; 0x0080 and up are free
for whatever two devices agree between themselves."
  (let* ((scid (%coc-alloc-cid conn))
         (ident (%next-sig-ident))
         (d (make-octets 10)))
    (u16le-put d 0 spsm)
    (u16le-put d 2 scid)
    (u16le-put d 4 mtu)
    (u16le-put d 6 mps)
    (u16le-put d 8 credits)
    (%l2cap-send-sig conn +l2cap-le-credit-conn-req+ ident d)
    (let ((deadline (+ (get-internal-real-time)
                       (round (* timeout-ms internal-time-units-per-second) 1000))))
      (loop
        (let ((hit (find-if (lambda (f)
                              (and (>= (length f) 14)
                                   (= (aref f 0) +l2cap-le-credit-conn-rsp+)
                                   (= (aref f 1) ident)))
                            (hci-conn-sig-pending conn))))
          (when hit
            (setf (hci-conn-sig-pending conn)
                  (remove hit (hci-conn-sig-pending conn) :count 1))
            (let ((result (u16-le hit 12)))
              (return
                (if (/= result +coc-success+)
                    (intern (format nil "REFUSED-~D" result) :keyword)
                    (let ((coc (%make-l2cap-coc
                                :conn conn :scid scid :dcid (u16-le hit 4)
                                :peer-mtu (u16-le hit 6) :peer-mps (u16-le hit 8)
                                :mtu mtu :mps mps
                                :tx-credits (u16-le hit 10)
                                :rx-credits credits)))
                      (push (cons scid coc) (hci-conn-coc-channels conn))
                      coc))))))
        (when (<= (- deadline (get-internal-real-time)) 0) (return :timeout))
        (when (eq :disconnected (%pump conn 200)) (return :disconnected))))))

(defun l2cap-coc-listen (conn spsm &key (mtu *coc-default-mtu*)
                                        (mps *coc-default-mps*)
                                        (credits *coc-default-credits*))
  "Accept channels opened to SPSM. A request for an SPSM nobody is listening
on is refused rather than dropped, so the peer learns immediately instead of
waiting out a timeout."
  (push (list spsm mtu mps credits) (hci-conn-coc-listeners conn))
  spsm)

(defun %coc-handle-connect-request (conn frame)
  (let* ((ident (aref frame 1))
         (spsm (u16-le frame 4))
         (peer-cid (u16-le frame 6))
         (peer-mtu (u16-le frame 8))
         (peer-mps (u16-le frame 10))
         (peer-credits (u16-le frame 12))
         (listener (find spsm (hci-conn-coc-listeners conn) :key #'first))
         (rsp (make-octets 10)))
    (cond
      ((null listener)
       (u16le-put rsp 8 +coc-spsm-not-supported+)
       (%l2cap-send-sig conn +l2cap-le-credit-conn-rsp+ ident rsp))
      (t
       (destructuring-bind (s mtu mps credits) listener
         (declare (ignore s))
         (let* ((scid (%coc-alloc-cid conn))
                (coc (%make-l2cap-coc
                      :conn conn :scid scid :dcid peer-cid
                      :peer-mtu peer-mtu :peer-mps peer-mps
                      :mtu mtu :mps mps
                      :tx-credits peer-credits :rx-credits credits)))
           (push (cons scid coc) (hci-conn-coc-channels conn))
           (setf (hci-conn-coc-incoming conn)
                 (nconc (hci-conn-coc-incoming conn) (list coc)))
           (u16le-put rsp 0 scid)
           (u16le-put rsp 2 mtu)
           (u16le-put rsp 4 mps)
           (u16le-put rsp 6 credits)
           (u16le-put rsp 8 +coc-success+)
           (%l2cap-send-sig conn +l2cap-le-credit-conn-rsp+ ident rsp)))))))

(defun l2cap-coc-accept (conn &key (timeout-ms 10000))
  "Wait for a peer to open a channel to one of our SPSMs. The channel itself
is created when the request arrives, from the receive path; this only hands
over the ones that have appeared."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms internal-time-units-per-second) 1000))))
    (loop
      (when (hci-conn-coc-incoming conn)
        (return (pop (hci-conn-coc-incoming conn))))
      (when (<= (- deadline (get-internal-real-time)) 0) (return nil))
      (when (eq :disconnected (%pump conn 200)) (return :disconnected)))))

(defun l2cap-coc-close (coc)
  "Close the channel. The peer is told, so it can free its own end."
  (unless (l2cap-coc-closed coc)
    (setf (l2cap-coc-closed coc) t)
    (let ((conn (l2cap-coc-conn coc))
          (d (make-octets 4)))
      (u16le-put d 0 (l2cap-coc-dcid coc))
      (u16le-put d 2 (l2cap-coc-scid coc))
      (ignore-errors
       (%l2cap-send-sig conn +l2cap-disconnection-req+ (%next-sig-ident) d))
      (setf (hci-conn-coc-channels conn)
            (remove (l2cap-coc-scid coc) (hci-conn-coc-channels conn) :key #'car)))
    t))

(defun %coc-handle-signalling (conn frame)
  "The CoC half of the signalling channel. Returns T if FRAME was ours."
  (let ((code (aref frame 0)))
    (cond
      ((= code +l2cap-le-credit-conn-req+)
       (when (>= (length frame) 14) (%coc-handle-connect-request conn frame)) t)
      ((= code +l2cap-flow-control-credit+)
       (when (>= (length frame) 8)
         (let ((coc (find (u16-le frame 4) (hci-conn-coc-channels conn)
                          :key (lambda (e) (l2cap-coc-dcid (cdr e))))))
           (when coc (incf (l2cap-coc-tx-credits (cdr coc)) (u16-le frame 6)))))
       t)
      ((= code +l2cap-disconnection-req+)
       (when (>= (length frame) 8)
         (let* ((dcid (u16-le frame 4))
                (entry (assoc dcid (hci-conn-coc-channels conn)))
                (rsp (make-octets 4)))
           (when entry (setf (l2cap-coc-closed (cdr entry)) t))
           (u16le-put rsp 0 dcid)
           (u16le-put rsp 2 (u16-le frame 6))
           (%l2cap-send-sig conn +l2cap-disconnection-rsp+ (aref frame 1) rsp)
           (setf (hci-conn-coc-channels conn)
                 (remove dcid (hci-conn-coc-channels conn) :key #'car))))
       t)
      ((= code +l2cap-disconnection-rsp+) t)
      ;; The opener is blocked on this one, and it is not here. Claim it so
      ;; nothing else answers it, but ask to have it put back.
      ((= code +l2cap-le-credit-conn-rsp+) :leave)
      (t nil))))

(setf *l2cap-coc-signalling-handler* #'%coc-handle-signalling)
