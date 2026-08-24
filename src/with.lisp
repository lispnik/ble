(in-package #:ble)

;;; Resource-safe wrappers.
;;;
;;; Every one of these pairs an acquire with a release that runs on *every*
;;; exit path, including a nonlocal one. That is not a convenience here the way
;;; WITH-OPEN-FILE is a convenience: an HCI_CHANNEL_USER socket holds the
;;; adapter away from the kernel for as long as it is open, so leaking one
;;; takes the radio out of service for the whole machine -- not just for the
;;; process that leaked it, and not just until it exits, since a process killed
;;; outright never runs its cleanup at all. Recovering from that needs
;;; `hciconfig hciN up' as root.
;;;
;;; Both consumers of this library had written their own version of these
;;; before they lived here, which is the usual sign that they belong in the
;;; library.

(defmacro with-hci-socket ((var &key (dev 0) (channel '+hci-channel-raw+))
                           &body body)
  "Bind VAR to a raw HCI socket on hci<DEV> for the extent of BODY."
  `(let ((,var (open-hci-socket :dev ,dev :channel ,channel)))
     (unwind-protect (progn ,@body)
       (close-hci-socket ,var))))

(defmacro with-hci-user-socket ((var dev) &body body)
  "Take exclusive control of hci<DEV> for the extent of BODY, and hand it back
afterwards however BODY ends.

This is the one that matters most. Between the bind and the release the kernel
has no access to that controller, so an escaping error that skipped the
release would leave the adapter down for every other program on the machine."
  `(let ((,var (open-hci-user-socket ,dev)))
     (unwind-protect (progn ,@body)
       (close-hci-user-socket ,var))))

(defmacro with-att-channel ((var form) &body body)
  "Bind VAR to the ATT channel FORM produces and close it after BODY.

Closes whichever transport it turns out to be -- a kernel L2CAP socket or an
HCI-CONN whose adapter has to be handed back -- and drops the channel's
notification backlog, which ATT-CHANNEL-CLOSE does for us."
  `(let ((,var ,form))
     (unwind-protect (progn ,@body)
       (when ,var (ignore-errors (att-channel-close ,var))))))

(defmacro with-nus ((var mac &rest args) &body body)
  "Connect to the Nordic UART Service at MAC, bind VAR to it, and close on
exit. ARGS are NUS-CONNECT's keywords."
  `(let ((,var (nus-connect ,mac ,@args)))
     (unwind-protect (progn ,@body)
       (when ,var (ignore-errors (nus-close ,var))))))

(defmacro with-nus-hci ((var mac &rest args) &body body)
  "As WITH-NUS, over an HCI_CHANNEL_USER connection. ARGS are
NUS-CONNECT-HCI's keywords."
  `(let ((,var (nus-connect-hci ,mac ,@args)))
     (unwind-protect (progn ,@body)
       (when ,var (ignore-errors (nus-close ,var))))))

(defmacro with-extended-scan ((sock) &body body)
  "Run BODY with extended scanning enabled on SOCK, and stop it afterwards.
A scan left running keeps the controller busy and floods any later reader of
the socket with reports it did not ask for."
  `(progn
     (start-extended-scan ,sock)
     (unwind-protect (progn ,@body)
       (ignore-errors (stop-extended-scan ,sock)))))

(defmacro with-advertising ((sock) &body body)
  "Enable legacy advertising for the extent of BODY and disable it after.
Advertising outlives the process that started it, so a peripheral that exits
without this stays discoverable and connectable with nothing behind it."
  `(progn
     (set-adv-enable ,sock t)
     (unwind-protect (progn ,@body)
       (ignore-errors (set-adv-enable ,sock nil)))))
