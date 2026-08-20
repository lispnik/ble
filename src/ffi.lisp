(in-package #:ble)

;;; libc / syscall bindings used by the live BLE scanner. Targets Linux:
;;; the AF_BLUETOOTH socket family and BlueZ raw HCI sockets only exist
;;; there. The bindings load fine on other platforms but the corresponding
;;; syscalls will fail at runtime.

(cffi:defctype socklen :unsigned-int)

(cffi:defcfun ("socket"     %socket)     :int
  (domain :int) (type :int) (protocol :int))
(cffi:defcfun ("bind"       %bind)       :int
  (fd :int) (addr :pointer) (addrlen socklen))
(cffi:defcfun ("connect"    %connect)    :int
  (fd :int) (addr :pointer) (addrlen socklen))
(cffi:defcfun ("close"      %close)      :int
  (fd :int))
(cffi:defcfun ("write"      %write)      :long
  (fd :int) (buf :pointer) (count :unsigned-long))
(cffi:defcfun ("read"       %read)       :long
  (fd :int) (buf :pointer) (count :unsigned-long))
(cffi:defcfun ("setsockopt" %setsockopt) :int
  (fd :int) (level :int) (optname :int) (optval :pointer) (optlen socklen))
(cffi:defcfun ("getsockopt" %getsockopt) :int
  (fd :int) (level :int) (optname :int) (optval :pointer) (optlen :pointer))
(cffi:defcfun ("fcntl"      %fcntl)      :int
  (fd :int) (cmd :int) (arg :int))
(cffi:defcfun ("poll"       %poll)       :int
  (fds :pointer) (nfds :unsigned-long) (timeout :int))
(cffi:defcfun ("ioctl"      %ioctl)      :int
  (fd :int) (request :unsigned-long) (arg :long))
(cffi:defcfun ("strerror"   %strerror)   :string
  (errnum :int))

(defun errno ()
  #+sbcl (sb-alien:get-errno)
  #-sbcl 0)

(define-condition syscall-error (error)
  ((label :initarg :label :reader syscall-error-label)
   (code  :initarg :code  :reader syscall-error-code))
  (:report (lambda (c s)
             (format s "~A failed: ~A (errno ~D)"
                     (syscall-error-label c)
                     (%strerror (syscall-error-code c))
                     (syscall-error-code c))))
  (:documentation
   "A libc call returned a negative value. Carries the errno rather than only
a formatted string, so callers can act on it -- a CLI that sees EPERM here can
tell the user about setcap instead of making them read the number."))

(defun check-syscall (result label)
  "Signal SYSCALL-ERROR if a libc call returned a negative value."
  (when (< result 0)
    (error 'syscall-error :label label :code (errno)))
  result)

;;; Socket, fcntl and poll constants ---------------------------------------
;;;
;;; Gathered here rather than left beside their first use. They were spread
;;; across three files, which is how the same dozen constants ended up being
;;; defined twice in two projects.

(defconstant +af-bluetooth+     31)
(defconstant +sock-raw+          3)
(defconstant +sock-seqpacket+    5)   ; each send/recv is one ATT PDU
(defconstant +btproto-l2cap+     0)
(defconstant +btproto-hci+       1)

(defconstant +f-getfl+           3)
(defconstant +f-setfl+           4)
(defconstant +o-nonblock+  #o4000)    ; 0x800 on Linux
(defconstant +pollin+       #x0001)
(defconstant +pollout+      #x0004)
(defconstant +sol-socket+        1)
(defconstant +so-error+          4)
(defconstant +einprogress+     115)
(defconstant +eintr+             4)

;;; Waiting on a descriptor ------------------------------------------------

(defun fd-readable-p (fd timeout-ms)
  "Poll FD for readability. T when readable, NIL on timeout.

EINTR is reported as a timeout rather than an error. poll(2), unlike read(2),
is not restarted by SA_RESTART, so any signal the runtime happens to deliver
-- a GC, a timer, a thread interruption -- surfaces right here. Every caller
already loops on NIL, so treating it as \"nothing yet\" is both correct and
what stops a long scan dying at an arbitrary moment."
  (cffi:with-foreign-object (pfd :unsigned-char 8)
    (dotimes (i 8) (setf (cffi:mem-aref pfd :unsigned-char i) 0))
    (setf (cffi:mem-aref pfd :int 0) fd
          (cffi:mem-aref pfd :short 2) +pollin+)
    (let ((rc (%poll pfd 1 timeout-ms)))
      (cond ((zerop rc) nil)
            ((and (< rc 0) (= (errno) +eintr+)) nil)
            ((< rc 0) (check-syscall rc "poll") nil)
            (t t)))))

;;; Octet <-> foreign-buffer helpers --------------------------------------

(defun bytes-to-foreign (octets buf)
  "Copy a Lisp octet vector into a CFFI foreign buffer of unsigned chars."
  (loop for i below (length octets)
        do (setf (cffi:mem-aref buf :unsigned-char i) (aref octets i))))

(defun foreign-to-bytes (buf len)
  "Copy LEN unsigned chars out of a CFFI foreign buffer into a fresh octet
vector."
  (let ((out (make-octets len)))
    (loop for i below len
          do (setf (aref out i) (cffi:mem-aref buf :unsigned-char i)))
    out))
