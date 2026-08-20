# Build and test the ble library.
#
#   make test   -- run the portable suite (needs no Bluetooth)
#   make check  -- also compile the Linux-only I/O layer
#   make clean  -- drop this tree's SBCL fasl cache
#
# There is no binary to build: this is a library. Its consumers embed it.

SBCL       ?= sbcl
SBCL_FLAGS := --non-interactive --no-userinit --no-sysinit

# Hermetic: this tree and its vendored ocicl/ deps only. If something is
# missing we want a loud failure, not a neighbour's copy.
BOOT := --eval "(require :asdf)" \
        --eval "(asdf:initialize-source-registry \`(:source-registry (:tree ,(truename \"./\")) :ignore-inherited-configuration))"

.PHONY: test check clean help
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

clean:
	rm -rf $(HOME)/.cache/common-lisp/*/$(subst /,_,$(CURDIR))

help:
	@grep -E '^#   ' Makefile | sed 's/^#   //'
