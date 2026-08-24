(in-package #:ble)

;;; Bonds that outlive the process.
;;;
;;; Pairing is expensive and, with Just Works, is also the moment an attacker
;;; would have to be present. Doing it once and remembering the result is the
;;; entire point of bonding -- a device that re-pairs on every connection has
;;; the security properties of one that never pairs at all.
;;;
;;; The hard part is not storage, it is identity. A phone connects from a
;;; resolvable private address that changes every few minutes, so the address
;;; a bond was made at is not the address the peer comes back on. What makes a
;;; bond findable again is the identity key it distributed: FIND-BOND tries
;;; every stored IRK against the address in front of it.

(defstruct bond
  "One paired peer. IDENTITY-ADDR is what the peer calls itself, which is not
necessarily the address it connected from."
  identity-addr (identity-addr-type :public) irk ltk
  (authenticated nil)          ; was there MITM protection when this was made?
  (secure-connections t))

(defvar *bonds* '()
  "Bonds held in memory, most recently stored first.")

(defvar *bond-file* nil
  "Where bonds persist, or NIL to keep them in memory only.")

(defun %octets-to-list (v) (coerce v 'list))

(defun bond-to-form (b)
  (list :identity-addr (%octets-to-list (bond-identity-addr b))
        :identity-addr-type (bond-identity-addr-type b)
        :irk (and (bond-irk b) (%octets-to-list (bond-irk b)))
        :ltk (%octets-to-list (bond-ltk b))
        :authenticated (bond-authenticated b)
        :secure-connections (bond-secure-connections b)))

(defun bond-from-form (f)
  (make-bond :identity-addr (coerce-octets (getf f :identity-addr))
             :identity-addr-type (getf f :identity-addr-type :public)
             :irk (let ((k (getf f :irk))) (and k (coerce-octets k)))
             :ltk (coerce-octets (getf f :ltk))
             :authenticated (getf f :authenticated)
             :secure-connections (getf f :secure-connections t)))

(defun save-bonds (&optional (path *bond-file*))
  "Write the bonds to PATH. Returns how many were written, or NIL if there is
nowhere to write them.

These are long-term keys in plain text. The file is created readable only by
its owner, which is the least that can be done about that; anything stronger
means a passphrase this library has no way to ask for."
  (when path
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (let ((*print-readably* nil) (*print-pretty* nil))
        (format out ";;; ble bonds -- long-term keys, keep private~%")
        (dolist (b (reverse *bonds*))
          (prin1 (bond-to-form b) out)
          (terpri out))))
    #+sbcl (ignore-errors (sb-posix:chmod (namestring path) #o600))
    (length *bonds*)))

(defun load-bonds (&optional (path *bond-file*))
  "Replace the in-memory bonds with those in PATH. Missing file is not an
error -- it just means nothing has been paired yet."
  (setf *bonds* '())
  (when (and path (probe-file path))
    (with-open-file (in path)
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            do (push (bond-from-form form) *bonds*))))
  (length *bonds*))

(defun store-bond (bond &key (path *bond-file*))
  "Remember BOND, replacing any earlier one for the same identity, and
persist. Re-pairing with a peer should not leave the old keys behind: they no
longer work, and a stale entry that still matches an address would send us
looking for a key the peer has forgotten."
  (setf *bonds* (remove-if (lambda (b)
                             (and (equalp (bond-identity-addr b)
                                          (bond-identity-addr bond))
                                  (eq (bond-identity-addr-type b)
                                      (bond-identity-addr-type bond))))
                           *bonds*))
  (push bond *bonds*)
  (save-bonds path)
  bond)

(defun forget-bond (identity-addr &key (path *bond-file*))
  "Drop the bond for this identity."
  (let ((before (length *bonds*)))
    (setf *bonds* (remove-if (lambda (b)
                               (equalp (bond-identity-addr b) identity-addr))
                             *bonds*))
    (save-bonds path)
    (/= before (length *bonds*))))

(defun find-bond (mac)
  "The bond for the peer at MAC, or NIL. MAC is on-air order.

Tries the identity addresses first, then resolves MAC against every stored
IRK. The second is what matters in practice: a peer using resolvable private
addresses is at a different address every time, and the only thing that links
them is a key it gave us once."
  (or (find-if (lambda (b) (equalp (bond-identity-addr b) mac)) *bonds*)
      (find-if (lambda (b)
                 (and (bond-irk b) (resolve-address mac (bond-irk b))))
               *bonds*)))

(defun bond-from-session (session &key irk identity-addr identity-addr-type)
  "Make a bond from a completed pairing.

Prefers the identity the peer distributed over the address it happened to
connect from -- storing the connection address would key the bond to something
that has already stopped being true by the next connection."
  (make-bond :identity-addr (or (smp-session-peer-identity-addr session)
                                identity-addr)
             :identity-addr-type (or (smp-session-peer-identity-addr-type session)
                                     identity-addr-type
                                     :public)
             :irk (or (smp-session-peer-irk session) irk)
             :ltk (smp-session-ltk session)
             :authenticated nil
             :secure-connections t))
