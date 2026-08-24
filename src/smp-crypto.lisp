(in-package #:ble)

;;; The Security Manager's cryptographic toolbox: f4, f5, f6, g2.
;;;
;;; All four are AES-CMAC underneath, and all four are defined on big-endian
;;; (most-significant-octet-first) values while SMP puts everything on the
;;; wire little-endian. That mismatch is the single largest source of bugs in
;;; an SMP implementation: every value crosses the boundary exactly once, and
;;; a reversal missed or applied twice produces a confirm value that is wrong
;;; in a way no error message describes -- the peer simply says pairing failed.
;;;
;;; The convention here: everything in this file works in the crypto order
;;; (big-endian). Conversion happens at the wire, in src/smp.lisp, and nowhere
;;; else.

(defun msb (octets)
  "Reverse between wire order and crypto order. Its own function because the
reversal must happen exactly once per value, and a named call is easier to
audit than a REVERSE buried in an expression."
  (reverse (coerce-octets octets)))

(defun aes-cmac (key message)
  "AES-CMAC-128. KEY is 16 octets; both are in crypto order."
  (let ((mac (ironclad:make-mac :cmac (coerce-octets key) :aes)))
    (ironclad:update-mac mac (coerce-octets message))
    (ironclad:produce-mac mac)))

(defun cat (&rest parts)
  (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
         (mapcar #'coerce-octets parts)))

(defun smp-f4 (u v x z)
  "The confirm value. U and V are the two public keys' X coordinates (32
octets each), X the nonce, Z a single octet -- zero for Just Works and
numeric comparison, and a passkey bit otherwise."
  (aes-cmac x (cat u v (vector z))))

(defparameter +f5-salt+
  (coerce-octets #(#x6C #x88 #x83 #x91 #xAA #xF5 #xA5 #x38
                   #x60 #x37 #x0B #xDB #x5A #x60 #x83 #xBE))
  "The fixed salt f5 uses to turn the DHKey into a key-generation key.

Checked against the published vectors, and not from memory: an earlier value
here agreed with the real one for three octets and then diverged, which is
precisely the kind of constant that cannot be reasoned about. It produced a
MacKey and LTK that were wrong but perfectly self-consistent, so two
implementations sharing the mistake pair happily with each other and with
nothing else.")

(defparameter +f5-key-id+ (coerce-octets #(#x62 #x74 #x6C #x65))  ; "btle"
  "Key ID, literally the ASCII for `btle'.")

(defun smp-f5 (dhkey n1 n2 a1 a2)
  "Derive (VALUES MACKEY LTK) from the shared secret.

A1 and A2 are seven octets each: the address type followed by the address, in
crypto order. Counter 0 gives the MacKey and counter 1 the LTK -- the same
input otherwise, which is why the counter octet is not optional decoration."
  (let ((tk (aes-cmac +f5-salt+ dhkey)))
    (flet ((round-with (counter)
             (aes-cmac tk (cat (vector counter) +f5-key-id+ n1 n2 a1 a2
                               (vector #x01 #x00)))))   ; length = 256 bits
      (values (round-with 0) (round-with 1)))))

(defun smp-f6 (mackey n1 n2 r io-cap a1 a2)
  "The check value each side sends so the other knows it derived the same
key. R is 16 octets -- zero for Just Works -- and IO-CAP is the three octets
of the sender's pairing request or response."
  (aes-cmac mackey (cat n1 n2 r io-cap a1 a2)))

(defun smp-g2 (u v x y)
  "The six digits a user compares in numeric comparison. Not used by Just
Works, which is the same protocol without a human in it, but it is what makes
the difference between the two only an association model rather than a
different exchange."
  (let ((mac (aes-cmac x (cat u v y))))
    (mod (reduce (lambda (acc b) (+ (* acc 256) b))
                 (coerce (subseq mac 12 16) 'list) :initial-value 0)
         1000000)))

;;; --- P-256 ---------------------------------------------------------------
;;;
;;; In software, not in the controller. The RTL8761B dongles here answer LE
;;; Generate DHKey with a fixed public key and a shared secret that is neither
;;; correct nor even stable -- the first call returns something that is
;;; visibly not a random 32-octet value, and subsequent calls return nothing
;;; at all. A silently wrong shared secret is the worst possible failure mode
;;; for pairing, so this does not use the controller for it.

(defun smp-generate-keypair ()
  "(VALUES PRIVATE PUBLIC-X PUBLIC-Y), coordinates in crypto order."
  (multiple-value-bind (private public) (ironclad:generate-key-pair :secp256r1)
    (let ((point (ironclad:destructure-public-key public)))
      (let ((y (getf point :y)))
        ;; Uncompressed point: 0x04 || X (32) || Y (32)
        (values private (subseq y 1 33) (subseq y 33 65))))))

(defparameter +p256-p+
  (- (expt 2 256) (expt 2 224) (- (expt 2 192)) (- (expt 2 96)) 1)
  "The P-256 prime, 2^256 - 2^224 + 2^192 + 2^96 - 1.")

(defparameter +p256-b+
  #x5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B)

(defun %octets-to-int (v)
  (reduce (lambda (acc b) (+ (* acc 256) b)) (coerce v 'list) :initial-value 0))

(defun smp-public-key-valid-p (x y)
  "Is (X, Y) actually on P-256?

Required, not defensive. A peer that sends a point off the curve can, against
an implementation that multiplies by it anyway, learn the private scalar --
the invalid-curve attack. Refusing is the only correct response.

It also catches the mundane version of the same thing: a coordinate assembled
in the wrong byte order is almost never on the curve, and without this check
that surfaces as a shared secret which is merely *different* from the peer\'s,
which in turn surfaces as a check value mismatch several steps later with
nothing pointing at the cause."
  (let ((xi (%octets-to-int x))
        (yi (%octets-to-int y))
        (p +p256-p+))
    (and (< 0 xi p) (< 0 yi p)
         ;; y^2 = x^3 - 3x + b  (mod p)
         (= (mod (* yi yi) p)
            (mod (+ (* xi xi xi) (* -3 xi) +p256-b+) p)))))

(defun smp-dhkey (private peer-x peer-y)
  "The X coordinate of the shared point: 32 octets, crypto order."
  (let* ((point (cat (vector #x04) peer-x peer-y))
         (peer (ironclad:make-public-key :secp256r1 :y point))
         (shared (ironclad:diffie-hellman private peer)))
    ;; DIFFIE-HELLMAN hands back an uncompressed point; SMP wants only X.
    (subseq shared 1 33)))
