(in-package #:ble)

;;; LE connection over an HCI_CHANNEL_USER socket.
;;;
;;; The kernel-assisted L2CAP-socket path (see nus.lisp) does not reliably
;;; initiate LE connections on this hardware, and never lets us pick the
;;; initiating PHY. To reach a peripheral that advertises on the Coded PHY
;;; only, we take an adapter away from the kernel entirely:
;;;
;;;   HCIDEVDOWN dev  ->  bind a HCI_CHANNEL_USER socket  ->  we ARE the host
;;;
;;; Now nothing else (bluetoothd, the kernel BLE stack) touches the
;;; controller, and we drive it directly over HCI:
;;;   - initialize it (Reset, event masks, LE Read Buffer Size)
;;;   - LE Extended Create Connection with an explicit Coded initiating PHY
;;;   - carry ATT PDUs ourselves inside L2CAP B-frames inside HCI ACL data
;;;
;;; The ATT protocol layer (MTU exchange, discovery, CCCD subscribe,
;;; notifications) is reused from nus.lisp: ATT-SEND / ATT-RECV dispatch on
;;; the channel type, so an HCI-CONN is a drop-in transport for a NUS.
;;;
;;; Exclusive control is reversible: closing the socket and HCIDEVUP hands
;;; the adapter back to the kernel. If the process dies mid-session the
;;; adapter is left down -- recover with `hciconfig hciN up`.
;;;
;;; Needs CAP_NET_RAW + CAP_NET_ADMIN, same as the scanner.

;;; ioctl numbers (Linux): _IOW('H', 201/202, int)
(defconstant +hcidevup+   #x400448C9)
(defconstant +hcidevdown+ #x400448CA)


;;; HCI opcodes / events used here
(defconstant +ogf-host-ctl+            #x03)
(defconstant +ocf-reset+               #x0003)
(defconstant +ocf-set-event-mask+      #x0001)
(defconstant +ocf-le-set-event-mask+   #x0001)
(defconstant +ocf-le-read-buffer-size+ #x0002)
(defconstant +ocf-le-ext-create-conn+  #x0043)
(defconstant +ocf-le-create-conn+      #x000D)
(defconstant +ocf-le-create-conn-cancel+ #x000E)
(defconstant +ocf-disconnect+          #x0006)
(defconstant +ogf-link-ctl+            #x01)

(defconstant +hci-disconn-complete-evt+ #x05)
(defconstant +hci-num-completed-evt+   #x13)
(defconstant +sub-le-conn-complete+     #x01)
(defconstant +sub-le-enh-conn-complete+ #x0A)

(defconstant +acl-default-len+ 27)

;;; --- low-level socket I/O ---------------------------------------------

(defun hci-write-raw (sock bytes)
  "Write a raw framed HCI packet (incl. the leading type byte) to SOCK."
  (let ((bytes (coerce-octets bytes)))
    (cffi:with-foreign-object (buf :unsigned-char (max 1 (length bytes)))
      (bytes-to-foreign bytes buf)
      (check-syscall (%write (hci-socket-fd sock) buf (length bytes)) "hci write"))))


(defun hci-poll-read (sock timeout-ms)
  "Read one HCI packet, waiting at most TIMEOUT-MS. NIL on timeout.

An EINTR return is reported as a timeout, not an error: unlike read(2),
poll(2) is not restarted by SA_RESTART, so any signal the runtime happens to
deliver (GC, thread interruption, a timer) surfaces here. Callers already loop
on NIL, so treating it as 'nothing yet' is both correct and what keeps a
long-running scan from dying at a random moment."
  (cffi:with-foreign-object (pfd :unsigned-char 8)
    (dotimes (i 8) (setf (cffi:mem-aref pfd :unsigned-char i) 0))
    (setf (cffi:mem-aref pfd :int 0) (hci-socket-fd sock)
          (cffi:mem-aref pfd :short 2) +pollin+)
    (let ((pr (%poll pfd 1 timeout-ms)))
      (cond ((zerop pr) nil)
            ((and (< pr 0) (= (errno) +eintr+)) nil)
            ((< pr 0) (check-syscall pr "poll"))
            (t (read-hci-packet sock))))))

;;; --- controller setup --------------------------------------------------

(defun open-hci-user-socket (dev)
  "Take exclusive control of hci<DEV>: bring it down, bind a
HCI_CHANNEL_USER socket, and initialize the controller. Returns an
HCI-SOCKET. Hand it back with CLOSE-HCI-USER-SOCKET."
  (let ((fd (check-syscall
             (%socket +af-bluetooth+ +sock-raw+ +btproto-hci+) "socket(HCI)")))
    (handler-case
        (progn
          ;; Down the device (ignore failure -- it may already be down) so
          ;; the kernel will grant exclusive USER-channel access.
          (%ioctl fd +hcidevdown+ dev)
          (cffi:with-foreign-object (sa :unsigned-char 8)
            (dotimes (i 8) (setf (cffi:mem-aref sa :unsigned-char i) 0))
            (setf (cffi:mem-aref sa :unsigned-char 0) (logand +af-bluetooth+ #xFF)
                  (cffi:mem-aref sa :unsigned-char 1) (logand (ash +af-bluetooth+ -8) #xFF)
                  (cffi:mem-aref sa :unsigned-char 2) (logand dev #xFF)
                  (cffi:mem-aref sa :unsigned-char 3) (logand (ash dev -8) #xFF)
                  (cffi:mem-aref sa :unsigned-char 4) +hci-channel-user+)
            (check-syscall (%bind fd sa 6) "bind(HCI_CHANNEL_USER)"))
          (let ((sock (make-hci-socket :fd fd :dev dev)))
            ;; Returns (values SOCK LE-ACL-DATA-PACKET-LENGTH).
            (let ((acl-len (hci-init-controller sock)))
              (setf (hci-socket-acl-len sock) acl-len)
              (values sock acl-len))))
      (error (c)
        (%close fd)
        ;; HCIDEVDOWN already happened, so an error after it leaves the
        ;; adapter down and unusable by anything else on the machine. Put it
        ;; back before rethrowing.
        (ignore-errors (hci-device-up dev))
        (error c)))))

(defun hci-device-up (dev)
  "HCIDEVUP hci<DEV>, handing it back to the kernel.

Its own function because there are two paths that owe the adapter back: a
normal close, and a takeover that failed after the HCIDEVDOWN succeeded. The
second one used to just close the socket and leave the radio down."
  (let ((tmp (%socket +af-bluetooth+ +sock-raw+ +btproto-hci+)))
    (when (>= tmp 0)
      (%ioctl tmp +hcidevup+ dev)
      (%close tmp))))

(defun close-hci-user-socket (sock)
  "Release exclusive control and hand the adapter back to the kernel."
  (when (hci-socket-fd sock)
    (%close (hci-socket-fd sock))
    (setf (hci-socket-fd sock) nil)
    (hci-device-up (hci-socket-dev sock))))

(defun hci-do-command (sock ogf ocf params &key (timeout-ms 3000) name)
  "Send an HCI command and return its return parameters, after the status.

A thin wrapper now: SEND-HCI-COMMAND waits for the answer and signals on a
bad status, which is what this function used to be for. It stays because
`return the parameters, and I know there are some\' is a different intent from
`send this\', and because it names the command in the error."
  (let ((answer (send-hci-command sock ogf ocf params
                                  :timeout-ms timeout-ms :name name)))
    (unless answer
      (error "HCI ~A: no Command Complete" (or name (hci-opcode ogf ocf))))
    (or (command-return-params answer) #())))

(defun hci-init-controller (sock)
  "Minimal post-takeover init: reset, enable all (incl. LE meta) events,
and learn the LE ACL buffer size. Returns the LE ACL data packet length."
  (hci-do-command sock +ogf-host-ctl+ +ocf-reset+ #() :name "Reset")
  (hci-do-command sock +ogf-host-ctl+ +ocf-set-event-mask+
                  (make-array 8 :initial-element #xFF) :name "Set Event Mask")
  (hci-do-command sock +ogf-le+ +ocf-le-set-event-mask+
                  (make-array 8 :initial-element #xFF) :name "LE Set Event Mask")
  (let* ((rsp (hci-do-command sock +ogf-le+ +ocf-le-read-buffer-size+ #()
                              :name "LE Read Buffer Size"))
         (le-acl-len (if (>= (length rsp) 2) (u16-le rsp 0) 0)))
    (if (plusp le-acl-len) le-acl-len +acl-default-len+)))

;;; --- LE connection -----------------------------------------------------

(defstruct hci-conn
  "An LE connection we own via an HCI_CHANNEL_USER socket. CHAN slot in a
NUS holds one of these for the HCI transport; RXBUF accumulates ACL
fragments during L2CAP reassembly, and PENDING holds ATT PDUs that were
reassembled but not yet asked for -- a single read can complete more than
one, and the surplus has to survive until the next call. SIG-PENDING is the
same for the L2CAP signalling channel, whose frames used to be dropped on the
floor along with every other non-ATT CID."
  sock handle acl-len (rxbuf (make-octets 0)) (pending '()) (sig-pending '())
  (sig-results '())
  ;; Connection-oriented channels, keyed by the CID we allocated for each,
  ;; plus the SPSMs we accept and the channels a peer has opened to them.
  (coc-channels '()) (coc-listeners '()) (coc-incoming '()) (coc-next-cid #x0040)
  (smp-pending '())
  ;; HCI events nobody has claimed yet. Events are not addressed to a
  ;; particular reader, so a reader that is not looking for one must not be
  ;; able to destroy it.
  (events '()))

(defun %await-le-connection (sock opcode timeout-ms)
  "Wait up to TIMEOUT-MS (a wall-clock deadline) for the (Enhanced)
Connection Complete after a create-connection. Returns the connection
handle on success, or :TIMEOUT / :FAILED. Intervening events (the command
status, advertising reports) are consumed."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms internal-time-units-per-second) 1000))))
    (loop
      (let ((remaining (- deadline (get-internal-real-time))))
        (when (<= remaining 0) (return :timeout))
        (let ((pkt (hci-poll-read
                    sock (max 1 (round (* remaining 1000)
                                       internal-time-units-per-second)))))
          (unless pkt (return :timeout))
          (cond
            ;; Command Status for our create-connection opcode
            ((and (>= (length pkt) 7)
                  (= (aref pkt 0) +hci-event-pkt+)
                  (= (aref pkt 1) +hci-cmd-status-evt+)
                  (= (u16-le pkt 5) opcode))
             (unless (zerop (aref pkt 3)) (return :failed)))
            ;; (Enhanced) Connection Complete LE meta sub-event
            ((and (>= (length pkt) 7)
                  (= (aref pkt 0) +hci-event-pkt+)
                  (= (aref pkt 1) +hci-le-meta-evt+)
                  (member (aref pkt 3) (list +sub-le-conn-complete+
                                             +sub-le-enh-conn-complete+)))
             (return (if (zerop (aref pkt 4))
                         (logand (u16-le pkt 5) #x0FFF)
                         :failed)))))))))

(defun %cancel-le-connection (sock)
  "Cancel an in-flight create-connection and drain events so a fresh one
can be issued (re-issuing while a create is pending is Command Disallowed)."
  (ignore-errors
    ;; :CHECK NIL. Cancelling when nothing is in flight is Command Disallowed,
    ;; which is a normal answer here rather than a fault -- and a signal would
    ;; skip the drain below, which is what this function is actually for.
    (send-hci-command sock +ogf-le+ +ocf-le-create-conn-cancel+ #() :check nil)
    ;; Read until the cancellation's Connection Complete (status 0x02).
    (loop repeat 40
          for pkt = (hci-poll-read sock 800)
          while pkt
          until (and (>= (length pkt) 4)
                     (= (aref pkt 0) +hci-event-pkt+)
                     (= (aref pkt 1) +hci-le-meta-evt+)
                     (member (aref pkt 3) (list +sub-le-conn-complete+
                                                +sub-le-enh-conn-complete+))))))

(defun %extended-create-conn-params (peer-mac peer-type init-phys)
  (let* ((nphys (logcount init-phys))
         (params (make-octets (+ 10 (* 16 nphys)))))
    (setf (aref params 0) 0           ; initiator_filter_policy = peer addr
          (aref params 1) 0           ; own_address_type = public
          (aref params 2) peer-type)  ; peer_address_type
    (replace params (coerce-octets peer-mac) :start1 3 :end1 9)
    (setf (aref params 9) init-phys)
    (dotimes (i nphys)
      (let ((off (+ 10 (* i 16))))
        (u16le-put params off        #x0060)  ; scan interval (60 ms)
        (u16le-put params (+ off 2)  #x0060)  ; scan window   (60 ms, continuous)
        (u16le-put params (+ off 4)  #x0018)  ; conn interval min (30 ms)
        (u16le-put params (+ off 6)  #x0028)  ; conn interval max (50 ms)
        (u16le-put params (+ off 8)  #x0000)  ; latency
        (u16le-put params (+ off 10) #x01F4)  ; supervision timeout (5 s)
        (u16le-put params (+ off 12) #x0000)  ; min CE length
        (u16le-put params (+ off 14) #x0000))); max CE length
    params))

(defun %legacy-create-conn-params (peer-mac peer-type)
  "Parameters for LE Create Connection (OCF 0x000D), 25 octets."
  (let ((params (make-octets 25)))
    (u16le-put params 0 #x0060)          ; scan interval    60 ms
    (u16le-put params 2 #x0060)          ; scan window      60 ms
    (setf (aref params 4) 0              ; initiator filter: use peer address
          (aref params 5) peer-type)
    (replace params (coerce-octets peer-mac) :start1 6 :end1 12)
    (setf (aref params 12) 0)            ; own address type: public
    (u16le-put params 13 #x0018)         ; conn interval min 30 ms
    (u16le-put params 15 #x0028)         ; conn interval max 50 ms
    (u16le-put params 17 #x0000)         ; latency
    (u16le-put params 19 #x01F4)         ; supervision timeout 5 s
    (u16le-put params 21 #x0000)         ; min CE length
    (u16le-put params 23 #x0000)         ; max CE length
    params))

(defun hci-le-create-connection (sock peer-mac peer-type
                                 &key (init-phys #x05) (timeout-ms 12000) (retries 2)
                                      (command :extended))
  "Connect to PEER-MAC (6 octets, on-air order) and wait for the link.
PEER-TYPE is the LE address type (0 public, 1 random). Returns the connection
handle, or signals after all attempts fail.

COMMAND selects which HCI command does the work, and it is not a detail:

  :EXTENDED  LE Extended Create Connection (5.0). The only way to name the
             initiating PHY, so the only way to reach a Coded-PHY-only peer.
             INIT-PHYS is the bitmask (bit0 = 1M, bit2 = Coded).
  :LEGACY    LE Create Connection (4.0). Implemented by every LE controller
             ever made, including built-in radios that do not implement the
             extended command at all. 1M PHY only, which is all an ordinary
             legacy advertiser needs.

Connectable adverts can be sparse, so an attempt may not catch one within
TIMEOUT-MS; this retries up to RETRIES extra times, cancelling the in-flight
attempt between tries."
  (let* ((extended (ecase command (:extended t) (:legacy nil)))
         (ocf (if extended +ocf-le-ext-create-conn+ +ocf-le-create-conn+))
         (params (if extended
                     (%extended-create-conn-params peer-mac peer-type init-phys)
                     (%legacy-create-conn-params peer-mac peer-type)))
         (opcode (hci-opcode +ogf-le+ ocf)))
    (dotimes (attempt (1+ retries))
      ;; :CHECK NIL, alone among the command sites here, because
      ;; %AWAIT-LE-CONNECTION already reads this command's Command Status and
      ;; turns a refusal into :FAILED -- which this loop retries. Letting
      ;; SEND-HCI-COMMAND consume it instead would signal out of a path whose
      ;; whole purpose is to try again.
      (send-hci-command sock +ogf-le+ ocf params :check nil)
      (let ((outcome (%await-le-connection sock opcode timeout-ms)))
        (when (integerp outcome)
          (return-from hci-le-create-connection outcome))
        ;; :timeout / :failed -- cancel the in-flight attempt before retrying
        (%cancel-le-connection sock)
        (when (< attempt retries)
          (format *error-output* "~&  connect attempt ~D/~D (~(~A~)); retrying...~%"
                  (1+ attempt) (1+ retries) outcome)
          (force-output *error-output*))))
    (error "LE create connection: failed after ~D attempt(s)" (1+ retries))))

(defun hci-disconnect (sock handle &optional (reason #x13))
  "Disconnect HANDLE (reason 0x13 = remote user terminated). Best effort."
  (ignore-errors
   (let ((p (make-octets 3)))
     (u16le-put p 0 handle)
     (setf (aref p 2) reason)
     ;; Best effort, as the docstring says: this is called from teardown
     ;; paths where the peer may already be gone, and Command Disallowed is
     ;; then the expected answer rather than a problem to raise. The
     ;; IGNORE-ERRORS around it would swallow the condition anyway; saying so
     ;; here is cheaper than leaving the next reader to work out why.
     (send-hci-command sock +ogf-link-ctl+ +ocf-disconnect+ p
                       :name "Disconnect"))))

;;; --- ACL / L2CAP transport (ATT lives on CID 0x0004) -------------------

(defun hci-acl-send-l2cap (conn cid pdu)
  "Wrap PDU in an L2CAP B-frame on CID and send it as one or more HCI ACL data
packets. ATT is one channel among several -- the signalling channel carries
connection parameter requests on CID 0x0005 -- so the framing is shared."
  (let* ((pdu (coerce-octets pdu))
         (l2cap (make-octets (+ 4 (length pdu))))
         (handle (hci-conn-handle conn))
         (maxseg (max 1 (hci-conn-acl-len conn)))
         (total (+ 4 (length pdu))))
    (u16le-put l2cap 0 (length pdu))   ; L2CAP length
    (u16le-put l2cap 2 cid)            ; channel id
    (replace l2cap pdu :start1 4)
    (loop with off = 0
          for first = t then nil
          for end = (min total (+ off maxseg))
          for seg = (subseq l2cap off end)
          for pb = (if first #x00 #x01)        ; first / continuation
          for flags = (logior (logand handle #x0FFF) (ash pb 12))
          for acl = (let ((a (make-octets (+ 5 (length seg)))))
                      (setf (aref a 0) #x02)   ; HCI ACL data packet type
                      (u16le-put a 1 flags)
                      (u16le-put a 3 (length seg))
                      (replace a seg :start1 5)
                      a)
          do (hci-write-raw (hci-conn-sock conn) acl)
             (setf off end)
          while (< off total))))

(defun hci-acl-send-att (conn pdu)
  "Send an ATT PDU on the ATT CID."
  (hci-acl-send-l2cap conn +att-cid+ pdu))

(defvar *l2cap-smp-frame-handler* nil
  "Called with (CONN FRAME) for Security Manager PDUs on CID 0x0006. Set by
src/smp.lisp.")

(defvar *l2cap-coc-frame-handler* nil
  "Called with (CONN CID FRAME) for data on a dynamic CID. Set by
src/l2cap-coc.lisp; a hook for the same reason the signalling one is.")

(defvar *l2cap-signalling-handler* nil
  "Called with the connection after frames are reassembled, to answer anything
that arrived on the signalling channel. Set by src/l2cap-signalling.lisp.

A hook rather than a direct call because this file loads first, and a variable
also lets a caller wrap it to observe what was handled, or bind it to NIL to
take over the channel entirely. Answering is the sane default: a peer that
asked for something is waiting, and silence costs it a timeout.")

(defvar *in-l2cap-serve* nil
  "Guards against re-entry. Answering a parameter request performs an LE
Connection Update, which waits for an event, which reassembles more frames --
without this, a second request arriving in that window would recurse.")

(defun %maybe-serve-signalling (conn)
  (when (and *l2cap-signalling-handler* (not *in-l2cap-serve*)
             (hci-conn-sig-pending conn))
    (let ((*in-l2cap-serve* t))
      (funcall *l2cap-signalling-handler* conn))))

(defun %drain-l2cap-frames (conn)
  "Pull every complete L2CAP frame out of CONN's reassembly buffer, queueing
the ATT ones on PENDING and leaving any partial frame behind.

Draining in a loop, and keeping the remainder, is load-bearing rather than
tidy. This used to take one frame per read and then clear the buffer, so a
read that completed two frames silently dropped the second -- and two at once
is not exotic here: it is what happens when a reply to a command lands in the
same instant as one of the peer's periodic notifications, which is precisely
when losing it costs the most."
  (loop
    (let ((buf (hci-conn-rxbuf conn)))
      (when (< (length buf) 4) (return))
      (let ((l2len (u16-le buf 0)))
        (when (< (length buf) (+ 4 l2len)) (return))
        (let ((cid (u16-le buf 2))
              (frame (subseq buf 4 (+ 4 l2len))))
          (setf (hci-conn-rxbuf conn) (subseq buf (+ 4 l2len)))
          (cond
            ((= cid +att-cid+)
             (setf (hci-conn-pending conn)
                   (nconc (hci-conn-pending conn) (list frame))))
            ;; The peer's own parameter request arrives here. Dropping it, as
            ;; every non-ATT CID used to be dropped, leaves a peripheral
            ;; waiting for an answer that will never come.
            ((= cid +l2cap-sig-cid+)
             (setf (hci-conn-sig-pending conn)
                   (nconc (hci-conn-sig-pending conn) (list frame))))
            ((and *l2cap-smp-frame-handler* (= cid #x0006))
             (funcall *l2cap-smp-frame-handler* conn frame))
            ;; A dynamic CID belongs to a connection-oriented channel. The
            ;; hook keeps this file from having to know how one works.
            ((and *l2cap-coc-frame-handler* (>= cid #x0040))
             (funcall *l2cap-coc-frame-handler* conn cid frame))))))))

(defun hci-pump (conn &optional (timeout-ms 200))
  "Read at most one HCI packet and route it. THE ONLY PLACE THAT READS FROM
THE SOCKET.

Returns NIL if nothing arrived, :DISCONNECTED if the link dropped, :DATA if
ACL data was filed, or the event packet itself for anything else -- events are
not queued anywhere, so handing them back is the only way a caller can match
one.

It never returns ACL payload and it never discards any. Reassembled frames are
filed where their owners will look: ATT PDUs into PENDING, signalling frames
answered, connection-oriented channels handed to theirs.

That invariant is the point of this function existing. Five separate defects
in this library were one shape -- a helper that needed the transport took it
over and threw away whatever it was not itself looking for. The L2CAP
reassembly kept one frame per read and dropped the rest; notification dispatch
dropped every handle but the one being waited on; a blocking parameter request
swallowed the peer's ATT requests; an event wait filed every ATT PDU as a
notification, losing the response to a request already in flight; and the
signalling server consumed a response another caller was blocked on. Each
surfaced as an unrelated timeout somewhere else, and four of the five were
invisible to the test suite. Everything that needs the transport now goes
through here, so a sixth is harder to write than to avoid."
  (let ((pkt (hci-poll-read (hci-conn-sock conn) timeout-ms)))
    (cond
      ((null pkt) nil)
      ((and (>= (length pkt) 2) (= (aref pkt 0) #x04))    ; HCI event
       (if (= (aref pkt 1) +hci-disconn-complete-evt+)
           :disconnected
           (progn
             ;; File it as well as returning it. An event arrives whenever the
             ;; controller has something to say -- a Long Term Key Request in
             ;; the middle of serving GATT, say -- and the reader that happens
             ;; to be at the socket is usually not the one who wants it.
             ;; Returning it only to whoever pumped meant it was discarded by
             ;; every caller that was looking for something else, which is how
             ;; a phone's request for the key went unanswered until it gave up.
             (setf (hci-conn-events conn)
                   (nconc (hci-conn-events conn) (list pkt)))
             (loop while (> (length (hci-conn-events conn)) 64)
                   do (pop (hci-conn-events conn)))
             pkt)))
      ((and (= (aref pkt 0) #x02) (>= (length pkt) 5))    ; ACL data
       (let* ((flags (u16-le pkt 1))
              (pb (logand (ash flags -12) #x3))
              (acl-len (u16-le pkt 3))
              (data (subseq pkt 5 (min (length pkt) (+ 5 acl-len)))))
         ;; pb #x01 continues the frame in progress; anything else starts a
         ;; new one, and whatever was buffered was a partial we will never be
         ;; able to complete.
         (setf (hci-conn-rxbuf conn)
               (if (= pb #x01)
                   (concatenate '(simple-array (unsigned-byte 8) (*))
                                (hci-conn-rxbuf conn) data)
                   (coerce-octets data)))
         (%drain-l2cap-frames conn)
         (%maybe-serve-signalling conn)
         :data))
      (t :data))))

(defun hci-take-event (conn &key event subevent)
  "Claim a queued HCI event matching EVENT (and SUBEVENT), or NIL.

Events are filed by HCI-PUMP rather than handed to whoever happened to be
reading, so a caller waiting on one can find it even though another part of
the program pulled it off the socket."
  (let ((hit (find-if (lambda (p)
                        (and (>= (length p) 2)
                             (or (null event) (= (aref p 1) event))
                             (or (null subevent)
                                 (and (>= (length p) 4) (= (aref p 3) subevent)))))
                      (hci-conn-events conn))))
    (when hit
      (setf (hci-conn-events conn)
            (remove hit (hci-conn-events conn) :count 1))
      hit)))

(defun hci-drop-events (conn &key event subevent)
  "Discard queued events of this type.

Called before issuing a command whose completion will be awaited. Queuing
events makes them survive an uninterested reader, but it also means an event
from an *earlier* exchange is still sitting there when the next command goes
out -- and it will be claimed as that command\'s answer. A stale Connection
Update Complete answered a later update with the previous update\'s
parameters, which is worse than losing it: the caller is told the link is
running at something it is not."
  (setf (hci-conn-events conn)
        (remove-if (lambda (p)
                     (and (>= (length p) 2)
                          (or (null event) (= (aref p 1) event))
                          (or (null subevent)
                              (and (>= (length p) 4) (= (aref p 3) subevent)))))
                   (hci-conn-events conn))))

(defun hci-acl-recv-att (conn timeout-ms)
  "The next ATT PDU: octets, NIL on timeout, or :DISCONNECTED.

Only takes from the queue HCI-PUMP fills, so an ATT PDU cannot be lost to
whatever else happened to arrive alongside it."
  (when (hci-conn-pending conn)
    (return-from hci-acl-recv-att (pop (hci-conn-pending conn))))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms internal-time-units-per-second) 1000))))
    (loop
      (let ((remaining (- deadline (get-internal-real-time))))
        (when (<= remaining 0) (return nil))
        (let ((r (hci-pump conn (max 1 (round (* remaining 1000)
                                              internal-time-units-per-second)))))
          (when (eq r :disconnected) (return :disconnected))
          (when (hci-conn-pending conn)
            (return (pop (hci-conn-pending conn))))
          (when (null r) (return nil)))))))

;;; --- top-level: NUS over the HCI_CHANNEL_USER transport ----------------

(defun hci-user-att-connect (mac &key (addr-type :random) (init-phys #x05)
                                     (dev 0) (timeout 20) (retries 2)
                                     (command :extended))
  "Take exclusive control of hci<DEV>, connect to MAC, and return an HCI-CONN
usable anywhere an ATT channel is expected.

The transport on its own, with no GATT profile attached. NUS-CONNECT-HCI is
this plus the Nordic service discovery; anything else that wants to drive its
own GATT over an adapter we own starts here."
  (let ((peer-type (ecase addr-type (:public 0) (:random 1))))
    (multiple-value-bind (sock acl-len) (open-hci-user-socket dev)
      (handler-case
          (let ((handle (hci-le-create-connection
                         sock (coerce-octets mac) peer-type
                         :init-phys init-phys :command command
                         :timeout-ms (round (* 1000 timeout)) :retries retries)))
            (register-att-channel
             (make-hci-conn :sock sock :handle handle :acl-len acl-len)))
        (error (c)
          (ignore-errors (close-hci-user-socket sock))
          (error c))))))

(defun hci-conn-close (conn)
  "Disconnect and hand the adapter back to the kernel."
  (when (and conn (hci-conn-sock conn))
    (hci-disconnect (hci-conn-sock conn) (hci-conn-handle conn))
    (close-hci-user-socket (hci-conn-sock conn))
    (setf (hci-conn-sock conn) nil)))
