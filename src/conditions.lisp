(in-package #:ble)

;;; The condition hierarchy.
;;;
;;; Everything this library signals inherits from BLE-ERROR, so a consumer can
;;; wrap a block of Bluetooth work in one handler instead of checking a return
;;; value at every step. That was the gap: the low-level operations report
;;; failure by returning a sentinel -- :TIMEOUT, :DISCONNECTED, or a bare
;;; integer ATT error code -- which is right for a caller driving one PDU at a
;;; time and useless for a caller doing a sequence of them, who then has to
;;; hand-match keywords at every call. Both consumers of this library ended up
;;; doing exactly that.
;;;
;;; The sentinels are still the default, because a timeout is an ordinary
;;; outcome when you are polling a radio and turning it into a stack unwind by
;;; default would be worse. Signalling is opt-in per dynamic extent, through
;;; *ATT-SIGNAL-ERRORS* or the WITH-BLE-CONDITIONS macro that binds it. The
;;; conditions themselves are always defined, so a consumer can name them in a
;;; handler without caring which style produced them.
;;;
;;; This file is part of ble/core so that the portable half -- MAC parsing --
;;; can hang its condition off the same root as the I/O layer's.

(define-condition ble-error (error) ()
  (:documentation
   "Base of every error this library signals. Trap this to catch anything
Bluetooth-related without enumerating the specific failures."))
