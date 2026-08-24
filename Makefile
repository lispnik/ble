# Build and test the ble library.
#
#   make test   -- run the portable suite (needs no Bluetooth)
#   make check  -- also compile the Linux-only I/O layer
#   make deploy -- copy this tree to a Linux box with radios and rebuild
#   make clean  -- drop this tree's SBCL fasl cache
#
# There is no binary to build: this is a library. Its consumers embed it.

SBCL       ?= sbcl
SBCL_FLAGS := --non-interactive --no-userinit --no-sysinit

# Hermetic: this tree and its vendored ocicl/ deps only. If something is
# missing we want a loud failure, not a neighbour's copy.
BOOT := --eval "(require :asdf)" \
        --eval "(asdf:initialize-source-registry \`(:source-registry (:tree ,(truename \"./\")) :ignore-inherited-configuration))"

# Where the radios are. Overridable: make deploy HOST=pi@other
HOST ?= pi@rpi4
DEST ?= ~/ble/

.PHONY: test check deploy clean help
.DEFAULT_GOAL := test

# ble/core has no dependencies at all, so this runs anywhere.
test:
	$(SBCL) $(SBCL_FLAGS) $(BOOT) \
	  --eval "(asdf:test-system :ble/tests)" --eval "(sb-ext:exit)"

# Also the I/O layer: its pure parts (registry, UUIDs, report parsers,
# connection parameters) are testable without a radio, and `ble' loads on
# macOS anyway -- the bindings are plain libc and only fail when CALLED.
check: test
	$(SBCL) $(SBCL_FLAGS) $(BOOT) \
	  --eval "(asdf:test-system :ble/io-tests)" \
	  --eval "(format t \"~&ble loaded: ~D exported symbols~%\" \
	            (let ((n 0)) (do-external-symbols (s :ble) (declare (ignore s)) (incf n)) n))" \
	  --eval "(sb-ext:exit)"

# Copy the tree, then DROP THE REMOTE FASL CACHE.
#
# That second step is not tidiness. rsync -a preserves this machine's
# mtimes, and against the target's cached build times they can look older --
# so ASDF sees no reason to recompile and silently keeps the previous build.
# It cost two rounds of live testing against code that was never running: the
# source on the target was right, the fasls were not, and the only clue was a
# log message printing in an older format. Never let a deploy be able to do
# that again.
deploy:
	rsync -a --delete --exclude .git --exclude ocicl ./ $(HOST):$(DEST)
	rsync -a --exclude .git ./ocicl/ $(HOST):$(DEST)ocicl/
	@# sudo for both: running sbcl under `sudo -E' leaves HOME as the user's,
	@# so root-owned fasls land in the user's cache and a plain rm cannot
	@# shift them. Verified rather than assumed -- a cache that silently
	@# survives is exactly the failure this target exists to prevent.
	ssh $(HOST) 'sudo -n rm -rf ~/.cache/common-lisp /root/.cache/common-lisp'
	ssh $(HOST) 'test -z "$$(find ~/.cache/common-lisp -name "*.fasl" 2>/dev/null | head -1)"' \
	  && echo "==> deployed to $(HOST):$(DEST), remote fasl cache verified empty" \
	  || (echo "deploy: remote fasl cache NOT cleared" >&2; exit 1)

clean:
	rm -rf $(HOME)/.cache/common-lisp/*/$(subst /,_,$(CURDIR))

help:
	@grep -E '^#   ' Makefile | sed 's/^#   //'
