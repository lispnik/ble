(in-package #:ble)

;;; The LE L2CAP signalling channel (CID 0x0005).
;;;
;;; One command on it matters for this library: Connection Parameter Update
;;; Request. A peripheral cannot change the connection interval itself -- only
;;; the central issues LE Connection Update -- so this is how it *asks*. That
;;; asymmetry is the whole reason the command exists, and it is why a
;;; peripheral that wants a slower link to save power has to be polite about
;;; it rather than simply setting one.
;;;
;;; Both directions live here. A peripheral built on this library can make the
;;; request; a central built on it answers one, and answering is not optional:
;;; a request that goes unanswered leaves the peer waiting out its timeout,
;;; and before these frames were kept they were dropped with every other
;;; non-ATT CID, which looks from the far end exactly like that.

(defconstant +l2cap-command-reject+          #x01)
(defconstant +l2cap-conn-param-update-req+   #x12)
(defconstant +l2cap-conn-param-update-rsp+   #x13)

(defconstant +l2cap-param-accepted+ #x0000)
(defconstant +l2cap-param-rejected+ #x0001)

(defvar *l2cap-coc-signalling-handler* nil
  "Called with (CONN FRAME) before the parameter-update handling, returning T
if it claimed the frame. Set by src/l2cap-coc.lisp.")

(defvar *l2cap-eatt-signalling-handler* nil
  "The same contract, consulted after the CoC handler. Set by src/eatt.lisp.

Two hooks rather than one list because the order is meaningful and worth being
able to read: connection-oriented channels answer first, Enhanced ATT bearers
second, and neither file has to know the other exists.")

(defvar *l2cap-sig-ident* 0
  "Rolling identifier for outgoing signalling commands. A response carries the
identifier of the request it answers, which is how the two are paired.")

(defun %next-sig-ident ()
  ;; 0 is reserved, so the range is 1..255.
  (setf *l2cap-sig-ident* (1+ (mod *l2cap-sig-ident* 255))))

(defun %l2cap-sig-pdu (code ident data)
  (let* ((data (coerce-octets data))
         (pdu (make-octets (+ 4 (length data)))))
    (setf (aref pdu 0) code
          (aref pdu 1) ident)
    (u16le-put pdu 2 (length data))
    (replace pdu data :start1 4)
    pdu))

(defvar *l2cap-sig-sink* nil
  "When bound to a function, it is called with (CODE IDENT DATA) and nothing
is transmitted.

A deliberately narrow test seam. Signalling is the half of L2CAP that decides
things -- which channels open and at what MTU, whether a reconfiguration is
allowed, what a refusal says -- and every one of those decisions used to be
reachable only with two radios, because it ends in a write to a socket. The
transport is worth proving over the air; the decisions are not, and this lets
them be checked on a machine with no Bluetooth at all.")

(defun %l2cap-send-sig (conn code ident data)
  (if *l2cap-sig-sink*
      (funcall *l2cap-sig-sink* code ident (coerce-octets data))
      (hci-acl-send-l2cap conn +l2cap-sig-cid+ (%l2cap-sig-pdu code ident data))))

(defun parse-conn-param-request (frame)
  "The four parameters out of a Connection Parameter Update Request, in
milliseconds. (VALUES MIN-MS MAX-MS LATENCY TIMEOUT-MS) or NIL.

Pure, so the wire format can be checked without a radio."
  (when (and frame (>= (length frame) 12)
             (= (aref frame 0) +l2cap-conn-param-update-req+))
    (values (interval-units-to-ms (u16-le frame 4))
            (interval-units-to-ms (u16-le frame 6))
            (u16-le frame 8)
            (timeout-units-to-ms (u16-le frame 10)))))

(defvar *l2cap-accept-conn-param-updates* t
  "Whether a central answers a peer's parameter request by accepting it and
performing the update.

On by default: a peripheral asks because it knows something the central does
not -- how much power it has left, how often it has anything to say. Refusing
by default would make the feature useless. Bind to NIL where the central has
its own reason to hold the link at a particular interval.")

(defun l2cap-answer-conn-param-request (conn frame &key (accept *l2cap-accept-conn-param-updates*))
  "Answer one Connection Parameter Update Request, and carry it out.

Returns (VALUES RESULT INTERVAL-MS) where RESULT is :ACCEPTED or :REJECTED.
Accepting means actually issuing the LE Connection Update -- the response
alone changes nothing on the link, and a central that answered `accepted' and
then did not update would be lying to the peer in a way it cannot detect."
  (multiple-value-bind (min-ms max-ms latency timeout-ms)
      (parse-conn-param-request frame)
    (unless min-ms (return-from l2cap-answer-conn-param-request nil))
    (let ((ident (aref frame 1)))
      (cond
        (accept
         (%l2cap-send-sig conn +l2cap-conn-param-update-rsp+ ident
                          (let ((d (make-octets 2)))
                            (u16le-put d 0 +l2cap-param-accepted+)
                            d))
         ;; Issue the update without waiting for its completion event: this
         ;; runs inside somebody's read loop, and blocking here stops
         ;; everything else on the link -- including the response to a request
         ;; that is already in flight.
         (hci-connection-update conn :min-interval-ms min-ms
                                     :max-interval-ms max-ms
                                     :latency latency
                                     :supervision-timeout-ms timeout-ms
                                     :await nil)
         (values :accepted nil))
        (t
         (%l2cap-send-sig conn +l2cap-conn-param-update-rsp+ ident
                          (let ((d (make-octets 2)))
                            (u16le-put d 0 +l2cap-param-rejected+)
                            d))
         (values :rejected nil))))))

(defun l2cap-serve-signalling (conn &key (accept *l2cap-accept-conn-param-updates*))
  "Answer any signalling frames that have arrived. Returns the number handled.

Unknown commands get a Command Reject rather than silence: the peer is waiting
for something, and 'not understood' is an answer it can act on."
  (let ((handled 0) (leave '()))
    (loop for frame = (pop (hci-conn-sig-pending conn))
          while frame
          do (incf handled)
             (let* ((code (aref frame 0))
                    (claim (or (and *l2cap-coc-signalling-handler*
                                    (funcall *l2cap-coc-signalling-handler*
                                             conn frame))
                               (and *l2cap-eatt-signalling-handler*
                                    (funcall *l2cap-eatt-signalling-handler*
                                             conn frame)))))
               (cond
                 ;; A response somebody else is blocked on. Draining it here
                 ;; would leave them waiting out a timeout for a frame that
                 ;; had already arrived -- so put it back.
                 ((eq claim :leave) (push frame leave) (decf handled))
                 ;; Connection-oriented channels take their own commands
                 ;; first; the hook keeps this file independent of that one.
                 (claim nil)
                 ((= code +l2cap-conn-param-update-req+)
                  (l2cap-answer-conn-param-request conn frame :accept accept))
                 ((= code +l2cap-conn-param-update-rsp+)
                  ;; Ours, from a request we made. Record it for whoever asked
                  ;; -- they are not in this call, they are somewhere in their
                  ;; own loop.
                  (when (>= (length frame) 6)
                    (push (cons (aref frame 1)
                                (if (= +l2cap-param-accepted+ (u16-le frame 4))
                                    :accepted :rejected))
                          (hci-conn-sig-results conn))))
                 ((= code +l2cap-command-reject+) nil)
                 (t (%l2cap-send-sig conn +l2cap-command-reject+ (aref frame 1)
                                     (let ((d (make-octets 2)))
                                       (u16le-put d 0 0) ; command not understood
                                       d))))))
    (setf (hci-conn-sig-pending conn)
          (nconc (nreverse leave) (hci-conn-sig-pending conn)))
    handled))

(defun l2cap-request-conn-params (conn &key (min-interval-ms 30) (max-interval-ms 50)
                                            (latency 0) (supervision-timeout-ms 4000))
  "Ask the central to change the connection parameters. For use by a
peripheral, which cannot change them itself. Returns the identifier to read
the answer with, via L2CAP-CONN-PARAM-RESULT.

This sends and returns; it does not wait. Waiting would mean reading from the
transport, and on a peripheral the PDUs that arrive are the central\'s
requests -- so a blocking version swallows the very traffic the caller is
there to serve. It did, and it broke a long write that happened to overlap the
request. Whoever owns the read loop keeps owning it; the response is picked up
by L2CAP-SERVE-SIGNALLING from that loop like anything else."
  (let ((ident (%next-sig-ident))
        (data (make-octets 8)))
    (u16le-put data 0 (ms-to-interval-units min-interval-ms))
    (u16le-put data 2 (ms-to-interval-units max-interval-ms))
    (u16le-put data 4 latency)
    (u16le-put data 6 (ms-to-timeout-units supervision-timeout-ms))
    (%l2cap-send-sig conn +l2cap-conn-param-update-req+ ident data)
    ident))

(defun l2cap-conn-param-result (conn ident &key peek)
  "The answer to the request IDENT, or NIL if none has arrived yet.
:ACCEPTED or :REJECTED. Consumed unless PEEK."
  (let ((hit (assoc ident (hci-conn-sig-results conn))))
    (when hit
      (unless peek
        (setf (hci-conn-sig-results conn)
              (remove hit (hci-conn-sig-results conn) :count 1)))
      (cdr hit))))

;;; Answer the signalling channel from the ordinary receive path, so a peer's
;;; request is handled by any program using this library rather than only by
;;; one that remembered to poll for it.
(setf *l2cap-signalling-handler* #'l2cap-serve-signalling)
