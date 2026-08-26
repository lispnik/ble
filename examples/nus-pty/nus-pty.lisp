;;;; A serial port that is really a Bluetooth device.
;;;;
;;;; The eleventh example, and the only one whose output is a file rather than
;;;; a printout. It opens a pseudo-terminal, pumps bytes between it and a
;;;; Nordic UART peripheral, and prints the device node:
;;;;
;;;;     (nus-pty:bridge "AA:BB:CC:DD:EE:FF")
;;;;     => /dev/pts/3 is now that device
;;;;
;;;; After which `screen /dev/pts/3', `cat > /dev/pts/3', minicom, or any
;;;; program that expects a serial port works unmodified and never learns that
;;;; Bluetooth was involved. It is the equivalent of BlueZ's `rfcomm bind',
;;;; which is how the Serial Port Profile is normally consumed -- and it is
;;;; the honest answer to wanting SPP, since SPP itself is a BR/EDR protocol
;;;; this library does not speak and this delivers what SPP is wanted FOR.
;;;;
;;;; A PTY is a kernel-provided pair. Whatever is written to the master
;;;; appears as input on the slave and the other way round; a terminal
;;;; emulator holds the master while a shell runs on the slave believing it
;;;; has hardware. Here the master is held by this process and the far end of
;;;; the pipe is a radio.
;;;;
;;;; TWO THINGS THAT BITE. A PTY is a *terminal*, so the kernel's line
;;;; discipline sits in the middle echoing input, buffering by lines, and
;;;; turning ^C into a signal. None of that is wanted for a byte pipe, so the
;;;; slave is put in raw mode before anything is bridged. And the device
;;;; number is allocated when it is opened, so it changes every run: it is
;;;; printed rather than promised, and anything scripted has to read it.
;;;;
;;;; TRANSPARENCY CUTS BOTH WAYS. This bridge adds no framing, which is the
;;;; point -- but it means the peripheral's own framing limits show through
;;;; undisguised. Driving examples/nordic-uart/ through it, observed:
;;;;
;;;;   -> time                         <- 20:27:45
;;;;   -> help                         <- commands: help, time
;;;;   -> echo through a device node   <- through a devicno such command: e n
;;;;
;;;; The first is right. The second is the reply truncated at a 23-octet MTU,
;;;; because GATT-NOTIFY cuts to the MTU rather than chunking. The third is a
;;;; 27-octet line arriving as two ATT writes, each of which that peripheral
;;;; treats as a whole command -- which is exactly the caveat its own
;;;; docstring gives, seen from the outside for the first time.
;;;;
;;;; None of that is the bridge misbehaving. A serial port has no message
;;;; boundaries either, and a device that needs them has to impose them. It is
;;;; recorded here because a bridge is precisely the thing that will expose
;;;; such a bug in somebody else's peripheral, and knowing what it looks like
;;;; is worth more than a paragraph of theory.
;;;;
;;;; Its own package. It uses CFFI directly for the pty calls, which is not a
;;;; violation of the rule that examples use only exported BLE symbols -- ble
;;;; has no business knowing what a terminal is.

(defpackage #:nus-pty
  (:use #:common-lisp)
  (:export #:open-pty #:close-pty #:bridge #:pty-name))

(in-package #:nus-pty)

;;; --- the pseudo-terminal -------------------------------------------------
;;;
;;; Four calls, in this order, and the order is the whole ritual: open a
;;; master, grant the slave to this user, unlock it, then ask what it is
;;; called. Skipping GRANTPT or UNLOCKPT leaves a node that exists and cannot
;;; be opened, which presents as a permissions problem rather than as a
;;; missing step.

(cffi:defcfun ("posix_openpt" %posix-openpt) :int (flags :int))
(cffi:defcfun ("grantpt"      %grantpt)      :int (fd :int))
(cffi:defcfun ("unlockpt"     %unlockpt)     :int (fd :int))
(cffi:defcfun ("ptsname"      %ptsname)      :string (fd :int))
(cffi:defcfun ("read"         %read)         :long
  (fd :int) (buf :pointer) (count :unsigned-long))
(cffi:defcfun ("write"        %write)        :long
  (fd :int) (buf :pointer) (count :unsigned-long))
(cffi:defcfun ("close"        %close)        :int (fd :int))

(defconstant +o-rdwr+   #x0002)
(defconstant +o-noctty+ #x0100
  "Do not make this our controlling terminal. Without it a ^C typed at the
far end could signal this process instead of travelling over the radio.")

(defstruct (pty (:constructor %make-pty (fd name)))
  "An open pseudo-terminal: the master file descriptor we hold, and the name
of the slave that everything else in the system sees."
  fd name)

(defun set-raw (name)
  "Put the slave in raw mode: no echo, no line buffering, no signals.

Done by running stty rather than through termios, deliberately. The termios
struct is a pile of platform-specific layout that would have to be marshalled
by hand and got right per architecture, and this example is about the bridge,
not about tcsetattr. The cost is one process; the benefit is that a reader
can see what was asked for."
  ;; -F on GNU, -f on BSD and macOS, tried in that order. The FLAG COMES
  ;; FIRST, before the settings: BSD stty wants the device named before it is
  ;; told what to do to it, and settings-first is accepted by GNU and quietly
  ;; ignored elsewhere. Getting that wrong leaves the terminal in canonical
  ;; mode, where a read returns only complete lines -- so a byte pipe carrying
  ;; anything without a newline in it simply stops, which is what it did.
  (dolist (flag '("-F" "-f") nil)
    (let ((p (ignore-errors
              (sb-ext:run-program "/bin/stty" (list flag name "raw" "-echo")
                                  :search nil :wait t
                                  :output nil :error nil))))
      (when (and p (zerop (sb-ext:process-exit-code p)))
        (return t)))))

(defun open-pty (&key (raw t))
  "Open a pseudo-terminal and return a PTY. Signals if any step fails."
  (let ((fd (%posix-openpt (logior +o-rdwr+ +o-noctty+))))
    (when (minusp fd) (error "posix_openpt failed"))
    (handler-case
        (progn
          (unless (zerop (%grantpt fd)) (error "grantpt failed"))
          (unless (zerop (%unlockpt fd)) (error "unlockpt failed"))
          (let ((name (%ptsname fd)))
            (unless name (error "ptsname returned nothing"))
            ;; Warned about rather than ignored. A terminal that stayed in
            ;; canonical mode looks like a working bridge until the first
            ;; message without a newline in it, and then never delivers it.
            (when (and raw (not (set-raw name)))
              (warn "could not put ~A in raw mode; it will echo and buffer by ~
                     line, which is not what a byte pipe wants" name))
            (%make-pty fd name)))
      (error (c) (%close fd) (error c)))))

(defun close-pty (pty)
  (when (and pty (pty-fd pty))
    (%close (pty-fd pty))
    (setf (pty-fd pty) nil)))

(defun pty-read (pty &optional (limit 512))
  "Whatever is waiting on the master, or NIL.

Polled before reading, and that is not an optimisation. The master is an
ordinary blocking descriptor: a bare read on it waits until somebody types,
which in a bridge means the radio half of the loop stops running and anything
the peer sends is never delivered. Nothing about a serial port suggests that
would happen, and the symptom is a device that works in one direction.

BLE:FD-READABLE-P is the library's own poll, used here rather than setting
O_NONBLOCK because the answer wanted is `is there anything', not `fail if
not'.

A master whose slave nobody has opened yet reads EIO rather than returning
zero. That is not an error either -- it is the normal state of a bridge
waiting for somebody to run screen -- so it too is reported as nothing."
  (unless (ble:fd-readable-p (pty-fd pty) 0)
    (return-from pty-read nil))
  (cffi:with-foreign-object (buf :unsigned-char limit)
    (let ((n (%read (pty-fd pty) buf limit)))
      (when (plusp n)
        (let ((out (make-array n :element-type '(unsigned-byte 8))))
          (dotimes (i n out)
            (setf (aref out i) (cffi:mem-aref buf :unsigned-char i))))))))

(defun pty-write (pty octets)
  (let ((octets (coerce octets '(simple-array (unsigned-byte 8) (*)))))
    (cffi:with-foreign-object (buf :unsigned-char (max 1 (length octets)))
      (dotimes (i (length octets))
        (setf (cffi:mem-aref buf :unsigned-char i) (aref octets i)))
      (%write (pty-fd pty) buf (length octets)))))

;;; --- the bridge ----------------------------------------------------------

(defun bridge (mac &key (dev 2) (addr-type :random) (seconds nil) (raw t))
  "Present the NUS peripheral at MAC as a serial device, and pump between them.

Prints the device node and then runs until SECONDS elapses, or forever when
SECONDS is NIL. Point anything at the node that expects a serial port.

The loop is deliberately symmetric and deliberately small: poll each side,
and whatever arrives on one goes out the other. There is no framing in either
direction because neither end has any -- a PTY is a byte stream and NUS is a
byte stream, and the whole value of this example is that they are the same
shape."
  (ble:install-adapter-teardown)
  (let ((pty (open-pty :raw raw)))
    (unwind-protect
         (ble:with-nus-hci (nus (ble:parse-mac mac) :dev dev :addr-type addr-type
                                                    :init-phys #x01 :mtu 247
                                                    :timeout 25)
           (unless nus (error "could not open a NUS connection to ~A" mac))
           ;; A serial link is interactive: every keystroke is its own round
           ;; trip, so the interval matters more than the throughput.
           (ble:hci-connection-update (ble:nus-fd nus)
                                      :min-interval-ms 15 :max-interval-ms 30)
           (format t "~&~A is now ~A~%~
                      (screen ~:*~A, or point anything expecting a serial ~
                       port at it)~%"
                   (pty-name pty) mac)
           (force-output)
           (let ((deadline (and seconds (+ (get-internal-real-time)
                                           (* seconds internal-time-units-per-second))))
                 (up 0) (down 0))
             (loop
               (when (and deadline (> (get-internal-real-time) deadline))
                 (format t "~&~D octet(s) out, ~D in~%" up down)
                 (force-output)
                 (return))
               ;; Terminal -> radio.
               (let ((typed (pty-read pty)))
                 (when typed
                   (ble:nus-send nus typed)
                   (incf up (length typed))))
               ;; Radio -> terminal. A short wait rather than none: this is
               ;; the only thing in the loop that sleeps, and without it the
               ;; bridge spins a core doing nothing.
               (let ((got (ble:nus-recv nus 20)))
                 (cond
                   ((eq got :disconnected)
                    (format t "~&peer disconnected~%") (force-output)
                    (return))
                   (got (pty-write pty got) (incf down (length got))))))))
      (close-pty pty))))
