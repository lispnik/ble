#!/usr/bin/env bash
#
# Install what is needed and run the suites. Used identically by every job in
# the matrix, so a failure on one architecture is comparable to the other
# rather than an artefact of how it was invoked.
#
# The last line is `make check' -- the same command a developer runs. That is
# the point of restoring dependencies with ocicl rather than reaching for
# Quicklisp: ocicl.csv pins every system by OCI digest, so CI resolves to the
# same bytes as a workstation, and the hermetic source registry in the
# Makefile stays hermetic instead of needing a CI-shaped exception.
#
# Runs both as an unprivileged CI user and as root in a container, so it asks
# for sudo only when it is neither root nor already supplied with sbcl.
set -euo pipefail

export DEBIAN_FRONTEND=noninteractive
export PATH="$HOME/.local/bin:$PATH"

if ! command -v sbcl >/dev/null 2>&1; then
  SUDO=""
  [ "$(id -u)" -eq 0 ] || SUDO="sudo"
  $SUDO apt-get update -qq
  $SUDO apt-get install -y -qq --no-install-recommends \
    sbcl git curl ca-certificates make
fi

echo "==> $(sbcl --version) on $(uname -m)"

# ocicl builds itself with sbcl and installs to ~/.local/bin. Cached between
# runs; this only pays out on a cold cache.
if ! command -v ocicl >/dev/null 2>&1; then
  echo "==> building ocicl"
  rm -rf /tmp/ocicl-src
  git clone --depth 1 --quiet https://github.com/ocicl/ocicl /tmp/ocicl-src
  ( cd /tmp/ocicl-src && sbcl --non-interactive --load setup.lisp >/dev/null )
  ocicl setup >/dev/null
fi

echo "==> ocicl $(ocicl version 2>/dev/null | head -1)"

# Restores exactly what ocicl.csv pins, by digest.
echo "==> restoring dependencies"
ocicl install

echo "==> make check"
make check
