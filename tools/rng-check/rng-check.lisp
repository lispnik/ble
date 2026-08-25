;;;; Is this controller's LE Rand worth trusting?
;;;;
;;;; The Bluetooth specification says LE Rand returns a random number. On the
;;;; RTL8761B dongles used to develop this library, a substantial fraction of
;;;; the output repeats across a controller reset -- measured at 39 of 100
;;;; 32-bit words shared between two consecutive takeovers, where chance would
;;;; give 0.000002. Within a single takeover the values are distinct, so
;;;; nothing looks wrong until you restart the process and draw again.
;;;;
;;;; That is why SMP-RANDOM XORs the controller's output with /dev/urandom.
;;;; This tool is here so anyone can find out what their own part does rather
;;;; than inherit an assumption: run it against each adapter.
;;;;
;;;;   sudo sbcl --non-interactive --load tools/rng-check/rng-check.lisp
;;;;   DEV=2 sudo -E sbcl --non-interactive --load tools/rng-check/rng-check.lisp
;;;;
;;;; Any shared word at all is a finding. It does not mean the keys derived
;;;; from it are recoverable -- that would take real analysis -- but it does
;;;; mean the generator is not doing what its specification claims, and key
;;;; material should not rest on it alone.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (truename "../../"))
       :ignore-inherited-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :ble))
(in-package #:ble)
(defun x (v) (format nil "~{~2,'0X~}" (coerce v 'list)))

(defun words-from (dev n)
  "N draws from a fresh takeover, as 32-bit words."
  (with-hci-user-socket (sock dev)
    (loop repeat n
          for d = (subseq (hci-do-command sock +ogf-le+ #x0018 #() :name "LE Rand") 0 8)
          append (list (x (subseq d 0 4)) (x (subseq d 4 8))))))

(let* ((dev (parse-integer (or (sb-ext:posix-getenv "DEV") "1")))
       (n 50)
       (a (words-from dev n))
       (b (words-from dev n))
       (c (words-from dev n))
       (shared-ab (intersection a b :test #'string=))
       (shared-ac (intersection a c :test #'string=))
       (shared-bc (intersection b c :test #'string=)))
  (format t "~&hci~D: three separate takeovers, ~D words each~%" dev (length a))
  (format t "~&  shared A-B: ~D~@[  ~{~A ~}~]~%" (length shared-ab) shared-ab)
  (format t "~&  shared A-C: ~D~@[  ~{~A ~}~]~%" (length shared-ac) shared-ac)
  (format t "~&  shared B-C: ~D~@[  ~{~A ~}~]~%" (length shared-bc) shared-bc)
  (format t "~&  expected by chance for a sound generator: ~,6F~%"
          (/ (* (length a) (length a)) (expt 2.0 32)))
  (format t "~&  VERDICT: ~:[independent across takeovers~;OVERLAP -- state survives a reset~]~%"
          (or shared-ab shared-ac shared-bc)))
(sb-ext:exit)
