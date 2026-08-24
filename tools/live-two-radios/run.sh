#!/usr/bin/env bash
#
# Run the two-radio live verification, start to finish, repeatably.
#
#   sudo ./run.sh
#
# Needs two USB Bluetooth adapters and root (HCI_CHANNEL_USER means
# CAP_NET_ADMIN, and the peripheral takes a controller away from the kernel).
#
# What makes it rerunnable rather than merely runnable:
#
#   * Adapters are chosen by bus, not by index. hciN numbering drifts across
#     reboots, and hardcoding it is how the second run of the day targets the
#     built-in radio and sees nothing.
#   * It recovers from the previous run first. A peripheral killed mid-session
#     leaves its adapter DOWN and unusable by anything on the machine, so this
#     ups every adapter and kills stale harness processes before starting --
#     otherwise one crash makes every later run fail for a reason that has
#     nothing to do with the code under test.
#   * It waits for the peripheral to actually advertise rather than sleeping a
#     guessed interval, and gives up with the log if it never does.
#   * The peripheral is killed on every exit path, including interrupts, so a
#     failed run does not leave a radio held.
set -uo pipefail

cd "$(dirname "$0")"
LOG="${LOG:-/tmp/live-two-radios-peripheral.log}"
READY_TIMEOUT="${READY_TIMEOUT:-90}"
SBCL="${SBCL:-sbcl}"
run_lisp() { "$SBCL" --non-interactive --no-userinit --no-sysinit --load "$1"; }

PERIPH_PID=""
cleanup() {
  if [ -n "$PERIPH_PID" ]; then
    kill "$PERIPH_PID" 2>/dev/null
    # Give it a moment to hand the adapter back itself -- its own unwind is
    # cleaner than ours -- then force the issue.
    for _ in 1 2 3 4 5 6 7 8 9 10; do
      kill -0 "$PERIPH_PID" 2>/dev/null || break
      sleep 0.5
    done
    kill -9 "$PERIPH_PID" 2>/dev/null
  fi
  ups
}
ups() {
  for d in /sys/class/bluetooth/hci*; do
    [ -e "$d" ] || continue
    hciconfig "$(basename "$d")" up >/dev/null 2>&1
  done
}
trap cleanup EXIT INT TERM

if [ "$(id -u)" -ne 0 ]; then
  echo "run.sh: must be root (HCI_CHANNEL_USER needs CAP_NET_ADMIN)" >&2
  exit 2
fi

echo "==> recovering from any previous run"
pkill -f 'sbcl.*(peripheral|central)\.lisp' 2>/dev/null
ups

echo "==> choosing adapters"
PICK="$(run_lisp ./pick-adapters.lisp 2>/tmp/live-two-radios-pick.err)"
PICK_STATUS=$?
cat /tmp/live-two-radios-pick.err >&2
if [ $PICK_STATUS -ne 0 ] || [ -z "$PICK" ]; then
  echo "run.sh: could not choose adapters" >&2
  exit 2
fi
# Only well-formed assignments, so a stray line in the output cannot be run.
eval "$(printf '%s\n' "$PICK" | grep -E '^(PERIPH_DEV|CENTRAL_DEV|PEER_MAC)=[0-9A-Fa-f:]+$')"
: "${PERIPH_DEV:?}" "${CENTRAL_DEV:?}" "${PEER_MAC:?}"
export PERIPH_DEV CENTRAL_DEV PEER_MAC

echo "==> starting the peripheral on hci$PERIPH_DEV (log: $LOG)"
: > "$LOG"
run_lisp ./peripheral.lisp > "$LOG" 2>&1 &
PERIPH_PID=$!

# Wait for it to advertise. Polling the log beats sleeping a guess: the first
# run after a boot pays for compiling the whole system, later ones do not.
ready=""
for _ in $(seq 1 $((READY_TIMEOUT * 2))); do
  if grep -aq "advertising as" "$LOG"; then ready=1; break; fi
  if ! kill -0 "$PERIPH_PID" 2>/dev/null; then
    echo "run.sh: the peripheral exited before it advertised:" >&2
    tail -20 "$LOG" >&2
    exit 2
  fi
  sleep 0.5
done
if [ -z "$ready" ]; then
  echo "run.sh: the peripheral never advertised within ${READY_TIMEOUT}s:" >&2
  tail -20 "$LOG" >&2
  exit 2
fi

echo "==> running the central on hci$CENTRAL_DEV against $PEER_MAC"
run_lisp ./central.lisp
STATUS=$?

echo
echo "==> peripheral"
grep -a '\[peripheral\]' "$LOG" | tail -5

exit $STATUS
