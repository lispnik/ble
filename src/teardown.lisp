(in-package #:ble)

;;; Giving the adapter back.
;;;
;;; The HCI_CHANNEL_USER transport takes an adapter away from the kernel, and
;;; while it holds one, nothing else on the machine can use that radio -- not
;;; bluetoothd, and not `hciconfig hciN down' as root, which returns EBUSY.
;;; The adapter comes back on every ordinary exit path, including errors,
;;; because those unwind. A signal does not unwind.
;;;
;;; That asymmetry is a library-created hazard, so the remedy belongs here
;;; rather than in each consumer. It bit during development: a `timeout 60'
;;; around a scan left the process alive holding hci0, and it took a SIGKILL
;;; and a manual down/up to clear -- SBCL does not exit on SIGTERM by default.
;;;
;;; Channels register themselves as they open and drop out as they close, so
;;; CLOSE-ALL-ATT-CHANNELS has something to close. INSTALL-ADAPTER-TEARDOWN
;;; is opt-in: installing signal handlers behind an application's back is not
;;; a library's business, but offering it in one line is.

(defvar *open-att-channels* nil
  "Every ATT channel currently open, newest first.

Bookkeeping for process-wide teardown, not a connection pool -- and
deliberately not thread-safe. A registry that took a lock would still be
useless from a signal handler, which is where it actually gets read.")

(defun register-att-channel (chan)
  "Record CHAN as open. Called for you by the connect functions."
  (pushnew chan *open-att-channels* :test #'eq)
  chan)

(defun unregister-att-channel (chan)
  "Forget CHAN. Called for you by ATT-CHANNEL-CLOSE."
  (setf *open-att-channels* (remove chan *open-att-channels* :test #'eq))
  chan)

(defun close-all-att-channels ()
  "Close every open channel, handing back any adapter we took.

Safe to call from an exit hook or a signal handler: it cannot signal, and it
is idempotent. Returns how many it closed."
  (let ((n 0))
    (dolist (chan *open-att-channels*)
      (when (ignore-errors (att-channel-close chan) t) (incf n)))
    (setf *open-att-channels* nil)
    n))

(defun install-adapter-teardown (&key (signals (list sb-unix:sigterm sb-unix:sighup))
                                      (report t))
  "Make an interrupted process still give the adapter back.

Registers an exit hook that closes every open channel, and turns each of
SIGNALS into an orderly exit rather than the default. SBCL does not terminate
on SIGTERM on its own, so without this a `timeout' or a service stop leaves
the process running and the radio captured.

Opt-in, and worth saying why: a library that installed signal handlers on
load would be taking a decision that belongs to whoever writes main. Call
this from yours.

SIGKILL cannot be caught. If that happens, `sudo hciconfig hciN up' restores
the adapter."
  (push (lambda () (ignore-errors (close-all-att-channels))) sb-ext:*exit-hooks*)
  (dolist (signal signals)
    (sb-sys:enable-interrupt
     signal
     (lambda (&rest args)
       (declare (ignore args))
       (when report
         (ignore-errors
          (format *error-output* "~&Terminated; releasing the adapter.~%")
          (force-output *error-output*)))
       ;; :abort NIL so the exit hook above actually runs.
       (sb-ext:exit :code 143 :abort nil))))
  t)
