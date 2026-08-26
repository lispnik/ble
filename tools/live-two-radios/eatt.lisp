;;;; Enhanced ATT bearers, over the air, as the client.
;;;;
;;;; Run peripheral.lisp beside it with EATT=1 and point this at its address:
;;;;
;;;;   EATT=1 PERIPH_DEV=7 sbcl --load peripheral.lisp &
;;;;   PEER_MAC=CC:BA:... CENTRAL_DEV=8 sbcl --load eatt.lisp
;;;;
;;;; WORTH RUNNING ON THE OLD DONGLES, which is half the point. EATT arrived
;;;; in Bluetooth 5.2 and this passes between two 5.1 controllers, because
;;;; L2CAP is a host protocol: the frames are ACL payload and the controller
;;;; never looks inside them. The README claimed for a long time that EATT
;;;; needed newer radios. It does not.
;;;;
;;;; What this proves, and what it does not. It proves bearers open, that each
;;;; carries ATT independently, and that each is sized by its own MTU rather
;;;; than the fixed channel's. It does NOT prove interoperability: the peer is
;;;; this same library, so any misreading of the specification is shared by
;;;; both ends and a green run says only that we are self-consistent. The
;;;; simultaneous-open tie-break in particular is written from reasoning
;;;; rather than from a shipping implementation. A phone would settle it.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))

(defun env-or (n d) (let ((v (sb-ext:posix-getenv n)))
                      (if (and v (plusp (length v))) v d)))
(defun as-text (v) (map 'string #'code-char v))

(let ((peer (env-or "PEER_MAC" ""))
      (dev (parse-integer (env-or "CENTRAL_DEV" "2")))
      (want (parse-integer (env-or "BEARERS" "3")))
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
      (format t "~&fixed-channel MTU ~A~%" (ble:att-exchange-mtu chan 247))
      (force-output)

      ;; --- open the bearers ---------------------------------------------
      ;; Opened deliberately NARROW so the reconfigure below has somewhere to
      ;; go. Asking for the maximum and then "raising" it to the same number
      ;; proves nothing, which is what this test did until the run showed
      ;; 517 -> 517 and gave the game away.
      (let ((bearers (ble:eatt-connect chan :count want :mtu 128)))
        (cond
          ((not (listp bearers))
           (check nil "eatt-connect refused: ~A" bearers))
          (t
           (format t "~&opened ~D bearer(s), MTU ~{~A~^ ~}~%"
                   (length bearers) (mapcar #'ble:eatt-bearer-mtu bearers))
           (force-output)
           (check (plusp (length bearers)) "bearers opened")
           (check (every (lambda (b) (>= (ble:eatt-bearer-mtu b) 64)) bearers)
                  "each is at or above the enhanced floor of 64")

           ;; --- ATT works on every one of them ----------------------------
           ;;
           ;; Discovery over a bearer, not over the fixed channel: if the
           ;; bearer were merely open but not wired to ATT this is where it
           ;; would show, and it would show as a timeout rather than an error.
           (let ((handles '()))
             (dolist (b bearers)
               (let* ((chars (ble:att-discover-characteristics b))
                      (mfr (ble:char-handle-by-uuid
                            chars (ble:uuid16 ble:+char-manufacturer-name+))))
                 (push mfr handles)
                 (check (and mfr (plusp mfr))
                        "discovery over bearer ~D found the manufacturer ~
                         characteristic at handle ~A"
                        (position b bearers) mfr)))
             (setf handles (nreverse handles))

             ;; --- and each returns the value ------------------------------
             (loop for b in bearers
                   for h in handles
                   for i from 0
                   do (when h
                        (multiple-value-bind (v err) (ble:att-read-value b h)
                          (check (and (null err) v (plusp (length v)))
                                 "bearer ~D read ~S" i (and v (as-text v))))))

             ;; --- they are independent ------------------------------------
             ;;
             ;; The property EATT exists for. Two requests are outstanding on
             ;; two bearers at once; the second must not be waiting behind the
             ;; first. Sent before either is collected, which a single channel
             ;; cannot do at all -- ATT permits exactly one outstanding
             ;; request per bearer.
             (when (and (>= (length bearers) 2) (first handles) (second handles))
               (let ((b1 (first bearers)) (b2 (second bearers))
                     (r1 (ble:make-octets 3)) (r2 (ble:make-octets 3)))
                 (setf (aref r1 0) #x0A (aref r2 0) #x0A)
                 (ble:u16le-put r1 1 (first handles))
                 (ble:u16le-put r2 1 (second handles))
                 (ble:att-send b1 r1)
                 (ble:att-send b2 r2)
                 (let ((a (ble:att-recv b1 4000))
                       (c (ble:att-recv b2 4000)))
                   (check (and a c (= #x0B (aref a 0)) (= #x0B (aref c 0)))
                          "two requests in flight at once, both answered ~
                           (~S and ~S)"
                          (and a (as-text (subseq a 1)))
                          (and c (as-text (subseq c 1)))))))

             ;; --- Exchange MTU is prohibited here -------------------------
             (check (handler-case (progn (ble:att-exchange-mtu (first bearers) 247)
                                         nil)
                      (error () t))
                    "Exchange MTU on a bearer is refused before it is sent")

             ;; --- reconfigure ---------------------------------------------
             ;;
             ;; Raising the MTU on bearers already carrying traffic. The peer
             ;; here is this same library, so this shows the exchange
             ;; completes, not that a third-party stack would agree with it.
             (let* ((before (ble:eatt-bearer-mtu (first bearers)))
                    (target 512)
                    (r (ble:eatt-reconfigure bearers :mtu target)))
               (check (< before target)
                      "the bearers start narrow (~D), so raising them means ~
                       something" before)
               (check (eq t r) "reconfigure to MTU ~D accepted (~A)" target r)
               (when (eq t r)
                 (check (= target (ble:eatt-bearer-mtu (first bearers)))
                        "and the new MTU took effect: ~D -> ~D"
                        before (ble:eatt-bearer-mtu (first bearers)))
                 ;; It has to be true of every bearer named, not just the one
                 ;; that happened to be checked.
                 (check (every (lambda (b) (= target (ble:eatt-bearer-mtu b)))
                               bearers)
                        "on all ~D bearers, not just the first"
                        (length bearers))))

             ;; A reduction must be refused by our own peer, which is the
             ;; rule most likely to be got wrong in a first implementation.
             (check (not (eq t (ble:eatt-reconfigure bearers :mtu 64)))
                    "a reduction in MTU is refused")

             ;; --- and they close -------------------------------------------
             (dolist (b bearers) (ble:eatt-close b))
             (check t "bearers closed"))))))

    (format t "~&~%~D/~D checks passed~%" (- checks fails) checks)
    (force-output)
    (sb-ext:exit :code (if (zerop fails) 0 1))))
