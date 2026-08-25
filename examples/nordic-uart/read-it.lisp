;;;; Talk to a Nordic UART device.
;;;;
;;;; The client half, and the shortest one here, because the library already
;;;; knows this profile: BLE:WITH-NUS-HCI connects, discovers RX and TX,
;;;; subscribes, and hands back something with SEND and RECV on it. The other
;;;; clients each do that discovery themselves because their profiles are
;;;; theirs; this one is a worked example of the profile support paying off.
;;;;
;;;; Which is also the honest reason NUS is in the library at all. It is not a
;;;; SIG profile and it has no assigned number -- it is in there because so
;;;; many vendor devices speak it that having the forty lines ready saves
;;;; writing them again.
(defpackage #:nordic-uart-client (:use #:common-lisp) (:export #:run #:converse))
(in-package #:nordic-uart-client)

(defun send-line (nus text &key (timeout-ms 5000))
  "Write one line and collect what comes back.

A reply may arrive as several notifications: the device answers with whatever
fits its MTU, and a terminal that stopped after the first would silently
truncate. So this reads until the device goes quiet rather than until it has
one answer -- there is no end-of-message marker in NUS, which is the price of
a protocol that is only a pipe."
  (ble:nus-send nus (map '(simple-array (unsigned-byte 8) (*)) #'char-code text))
  (let ((parts '()))
    (loop for first = t then nil
          for chunk = (ble:nus-recv nus :timeout-ms (if first timeout-ms 300))
          while (and chunk (not (eq chunk :disconnected)))
          do (push (map 'string #'code-char chunk) parts))
    (apply #'concatenate 'string (nreverse parts))))

(defun converse (nus lines)
  "Send each line and print the answer. Separated from RUN so the transport
and the conversation are not tangled: this works over either NUS transport."
  (dolist (line lines)
    (format t "~&-> ~A~%" line)
    (force-output)
    (let ((reply (send-line nus line)))
      (format t "~&<- ~A~%" (if (plusp (length reply)) reply "(nothing)"))
      (force-output))))

(defun run (mac &key (dev 2) (addr-type :random)
                     (lines '("help" "time" "echo hello from a lisp central"
                              "nonsense")))
  "Connect to the UART device at MAC and hold a short conversation.

Over the HCI-CHANNEL-USER transport, because that is the one that can choose
its initiating PHY. WITH-NUS is the same thing over a kernel L2CAP socket
where that does not matter."
  (ble:install-adapter-teardown)
  (ble:with-nus-hci (nus mac :dev dev :addr-type addr-type :timeout 25)
    (unless nus (error "could not open a NUS connection to ~A" mac))
    (format t "~&connected to ~A, MTU ~D~%" mac (ble:nus-mtu nus))
    (format t "~&rx handle ~D, tx handle ~D, cccd ~D~%"
            (ble:nus-rx-handle nus) (ble:nus-tx-handle nus)
            (ble:nus-cccd-handle nus))
    (force-output)
    (converse nus lines)))
