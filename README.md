# ble

[![CI](https://github.com/lispnik/ble/actions/workflows/ci.yml/badge.svg)](https://github.com/lispnik/ble/actions/workflows/ci.yml)

A Bluetooth Low Energy library for SBCL on Linux: HCI sockets, adapter
enumeration, LE scanning, extended advertising, and an ATT/GATT client that
speaks over either of two transports.

## Systems

| System | Depends on | Contents |
|---|---|---|
| `ble/core` | **nothing** | octet vectors, little-endian integers, advertising-data records, MAC byte order |
| `ble` | `ble/core`, `cffi` | HCI sockets, adapters, LE scan, extended advertising, ATT/GATT, LE connections |
| `ble/tests` | `ble/core`, `fiveam` | the portable suite |

`ble/core` having **no dependencies at all** protocol code — and its test suite
— stay portable while still sharing this library's octet handling and AD
parsing. Both current consumers rely on that to run their tests on a machine
with no Bluetooth.

```sh
make test    # portable suite (37 checks); runs anywhere, including macOS
make check   # also the I/O layer, driven by a scripted peer (104 more)
```

## What it gives you

**Advertising.** Both forms, for the same reason both scan paths exist:
`set-adv-parameters` / `set-adv-data` / `set-scan-response-data` /
`set-adv-enable` are the legacy 4.0 commands every controller implements, and
`set-extended-adv-*` are the 5.0 ones that can reach the Coded PHY. Offering
only extended meant you could scan on a radio you could not advertise from.

**Scanning.** `scan-reports` is the vendor-neutral loop — every advertising
report to a callback, bounded by seconds or count. `discover` merges reports
per address into `discovered` records, which matters more than it sounds: a
device's name arrives in a scan response and its service UUIDs in the
advertisement, so anything treating reports as devices sees each device
repeatedly, differently, and never whole.

Both the legacy 4.0 scan commands and the 5.0 extended ones are here, and you
need both. Extended is the only way to reach the Coded PHY; legacy is the only
thing some controllers implement.

**GATT discovery.** `att-discover-services` walks the primary services;
`att-find-service` asks the peer for one UUID's handle range in a single round
trip. `att-discover-characteristics` takes `:start`/`:end`, because a
service's handle range *is* the membership test — ATT has no other notion of
which service a characteristic belongs to. It returns `gatt-char` records
carrying the property bitmap, which is how you tell a device's command channel
from its data channel without guessing. `att-discover-descriptors` gives the
raw attribute list over a range.

Reading: `att-read-value`, `att-read-long-value` (Read Blob continuation), and
`att-read-characteristic` to read by UUID rather than by a handle number that
means nothing at the call site. Note that ATT gives no length field and no
more-data flag, so a response that exactly fills the MTU is the only hint a
value continues — and it is ambiguous. `value-may-be-truncated-p` is that test
spelled out; the long read pays a round trip that usually finds nothing.

Writing and subscribing: `att-write-value` (Write Request),
`att-write-command` (fire-and-forget), `att-subscribe`,
`att-next-notification`. Indications are supported properly — they arrive
through the same call as notifications and the required confirmation is sent
on receipt. Without that a peer sends one indication and then waits forever,
which presents as a device that stopped talking.

**Several notifying characteristics at once.** Notifications are queued per
channel as they arrive, so traffic on a handle you are not currently asking
for is kept rather than dropped. `att-next-notification` takes one handle;
`att-next-notification-any` returns `(values value handle)` for whichever
speaks first and is the call to reach for with more than one subscription.
`att-pending-notifications` reports the backlog, which is bounded by
`*att-notification-queue-limit*` — a peer notifying faster than anyone reads
drops its oldest rather than growing without limit.

This matters more than it sounds. Notifications arrive whenever the peer feels
like it, including in the middle of a read, a write, or service discovery, and
every one of those paths used to discard them. A subscriber could lose an
arbitrary number of readings to an unrelated request running concurrently, and
nothing reported it.

## Being the peripheral: the GATT server

`src/gatt-server.lisp` is the other direction — an attribute database, and the
half of ATT that answers requests rather than making them.

```lisp
(let ((server (ble:make-gatt-server)))
  (ble:gatt-add-service server #x180A)
  (ble:gatt-add-characteristic server :uuid #x2A29 :properties '(:read)
                                      :value "ACME")
  (ble:gatt-add-service server #xFFE0)
  (multiple-value-bind (value-handle cccd-handle)
      (ble:gatt-add-characteristic server :uuid #xFFE1
                                   :properties '(:read :write :notify)
                                   :on-read (lambda (s a) (declare (ignore s a))
                                              (current-reading)))
    (loop (ble:gatt-serve server chan :timeout-ms 100)
          (ble:gatt-notify server chan value-handle (current-reading)
                           :cccd-handle cccd-handle))))
```

Handles are allocated as the database is built, which is the order GATT
requires anyway: a characteristic's declaration must sit immediately before
its value and its descriptors immediately after. Laying that out by hand is
where a server usually goes wrong, so `gatt-add-characteristic` writes all
three attributes — declaration, value, and a CCCD when the characteristic
notifies — and hands back the handles it assigned rather than making you
derive them.

It answers Exchange MTU, Find Information, Find By Type Value, Read By Type,
Read By Group Type, Read, Read Blob, Read Multiple, Write, Write Command, and
Prepare/Execute Write, and refuses anything else with `request not supported`.

Long writes go through the prepare queue, so nothing takes effect until the
Execute: a client that gives up midway leaves the attribute exactly as it was,
and a cancelled queue cannot leak into the next write. An `on-write` hook is
called once with the **assembled** value rather than per fragment — a
characteristic that validates its contents would otherwise refuse a perfectly
good long write on the strength of its first twenty octets — and a refusal
leaves the old value in place. The queue is bounded by
`*max-prepared-writes*`, since a client could otherwise queue fragments
forever and never execute them. `on-read` computes a
value at the moment it is asked for, which is what a sensor characteristic
wants; `on-write` returns an ATT error code to refuse a write. `gatt-notify`
declines to send unless the client has actually subscribed — sending anyway is
a protocol violation, and it reaches the client as traffic it never asked for.

## The established connection

`hci-connection-update`, `hci-read-rssi`, `hci-read-remote-features` and
`hci-read-remote-version` drive the controller directly, so they need an
`hci-conn` — with the kernel L2CAP path we do not own the controller and
cannot issue commands on it.

The connection interval is the one knob that trades latency against power on a
live link. A peripheral asks for what it wants when it advertises, but the
central sets it, so this is how you shorten a link that answers too slowly or
lengthen one draining a battery. Intervals are carried in 1.25 ms units and
supervision timeouts in 10 ms ones; these take milliseconds and convert, so
the units live in one place rather than at every call site.

`hci-connection-update` returns the parameters **in force**, which need not be
the ones asked for — the peer may answer anywhere inside the range. Using the
requested numbers afterwards is how a link ends up being driven at an interval
it is not running at.

## Resource-safe wrappers

`with-hci-socket`, `with-hci-user-socket`, `with-att-channel`, `with-nus`,
`with-nus-hci`, `with-extended-scan`, `with-advertising`. Each releases on
every exit path, including a nonlocal one.

`with-hci-user-socket` is the one that earns its keep. Between the bind and
the release the kernel has no access to that controller, so an escaping error
that skipped the release leaves the adapter down for *every* program on the
machine — recoverable only with `hciconfig hciN up` as root. Both consumers
had written their own version of these before they lived here.

## Conditions

Everything this library signals inherits from `ble-error`, so one handler
covers anything Bluetooth-related: `att-error` (with `att-error-code`,
`att-error-handle`, and `att-error-name` for the spec wording), `att-timeout`,
`peer-disconnected`, `gatt-not-found`, plus `syscall-error` and `invalid-mac`.

The low-level operations still *return* `:timeout`, `:disconnected` or a bare
error code by default, because a timeout is an ordinary outcome when you are
polling a radio and unwinding the stack for one would make this unpleasant to
poll with. Callers running a *sequence* of GATT operations want the opposite,
and they opt in per dynamic extent:

```lisp
(handler-case
    (ble:with-ble-conditions
      (ble:att-exchange-mtu chan 247)
      (ble:att-subscribe chan cccd)
      (ble:att-write-value chan handle payload))
  (ble:att-error (e) (report (ble:att-error-name (ble:att-error-code e))))
  (ble:att-timeout () (give-up)))
```

`att-subscribe` signals in both styles: a CCCD write that fails means no
notification will ever arrive, so a caller that carried on would wait out its
timeouts against a peer that was never going to speak.

Long and batched access: `att-read-long-value` and `att-read-multiple`,
`att-prepare-write` / `att-execute-write`, and `att-write-long-value` for a
value larger than MTU−3. Sized by `att-mtu` — the MTU *agreed* with the peer,
which is not the same number as `*att-rx-mtu*`, what we advertise we can
receive. They differ whenever the peer offers less, and sizing by the wrong
one truncates a long read silently. The read side is exercised over real
radios by `tools/live-two-radios/`, whose server offers a 300-octet
characteristic behind a 23-octet MTU; the long *write* is still suite-only,
and the long *write* by the same harness, whose server now offers a writable
300-octet characteristic — at MTU 23 a Write Request carries 20 octets, so it
completes only if Prepare and Execute work on both sides.

Not implemented:

- **SMP** — no pairing, bonding or encryption, so this cannot talk to a peer
  that requires a bonded link. The largest remaining gap by a distance.
- **LE privacy** (resolving list, RPA) and the controller filter-accept-list.
- **Read Multiple Variable Length** (0x20, from 5.2), refused as unsupported.
- **L2CAP connection-oriented channels**, so no high-throughput transfers.
- **Signed Writes.** Verifying one needs a CSRK, which is distributed by
  bonding, which needs SMP. Until that exists the only honest thing to do with
  a signed write is ignore it — and since it is a command, silence is also the
  protocol-correct way to not support it. Accepting one unverified would be
  worse than refusing.

**Two transports, one ATT layer.** `att-send` and `att-recv` dispatch on
whether the channel is an integer fd (a kernel L2CAP socket) or an `hci-conn`,
so every line of ATT protocol is shared:

- `l2cap-att-connect` — ask the kernel to connect. Less invasive, needs no
  `CAP_NET_ADMIN`, leaves the adapter with the kernel.
- `hci-user-att-connect` — `HCIDEVDOWN`, then bind an `HCI_CHANNEL_USER`
  socket and *be* the host. Needed when the kernel path will not initiate, and
  the only way to choose the initiating PHY.

The second one borrows the adapter and must give it back. Every exit path
does, including errors — but a process killed outright leaves the adapter
down, and until it is up again *nothing* else on the machine can use that
radio, not even `hciconfig hciN down` as root. Consumers should trap SIGTERM.

## Capabilities

```sh
sudo setcap 'cap_net_raw,cap_net_admin+eip' <your binary>
```

`CAP_NET_ADMIN` is only needed for the `hci-user` transport, which downs and
re-ups the adapter.

## Choosing an adapter

`default-hci-dev` takes a `prefer` argument because there is no single right
answer. `:usb` picks the first USB dongle — correct when you need Coded PHY,
which built-in radios generally cannot receive. `:lowest` picks the lowest
index — correct for an ordinary 1M peripheral, and on at least one development
Pi the built-in radio is the *only* one that hears the device while both
dongles report nothing. `hciN` numbering also drifts across reboots, so
resolve by bus and capability rather than assuming an index.

## Testing against it, with two radios

`tools/live-two-radios/` is the end-to-end check that needs hardware: two USB
dongles on one machine, one driving each side of a real link.
`peripheral.lisp` takes a controller with `HCI_CHANNEL_USER`, advertises
connectable, and once a central attaches it notifies on **two** handles at
once and refuses any request with an ATT Error Response. `central.lisp` is the
library under test.

Two radios rather than one because the interesting behaviour is only visible
against a peer that does something no consumer device here does. Both
consumers of this library have exactly one notifying characteristic, so the
multi-handle dispatch has no natural peer to be tested against — and a peer
built from the same code you are testing proves less.

```sh
sudo tools/live-two-radios/run.sh
```

That is the whole invocation, and it is meant to be run repeatedly. It picks
the two adapters **by bus** — `hciN` numbering drifts across reboots, and on
this machine the built-in UART radio holds hci0 while the dongles land at 1
and 2 — reads the peripheral's address before anything takes a controller
away, waits for the peripheral to actually advertise rather than sleeping a
guessed interval, and exits with the central's status.

It also recovers from the previous run before starting: killing a peripheral
mid-session leaves its adapter DOWN and unusable by anything on the machine,
so one crash would otherwise make every later run fail for a reason unrelated
to the code under test. `run.sh` ups every adapter and clears stale harness
processes first, and kills the peripheral on every exit path including
interrupts. Verified by doing it: three consecutive runs, then a run started
from a deliberately downed adapter, all 25/25.

`PERIPH_DEV`, `CENTRAL_DEV` and `PEER_MAC` override the choice if you need a
particular pairing; `PERIPH_SECONDS` bounds how long the peripheral serves.

`compile-check.lisp` beside them needs no hardware at all: it loads every
definition, compiles the forms that drive the radios without running them, and
asserts on the attribute layout the server builds. CI runs it on both
architectures, because a harness that cannot be exercised is a harness that
quietly stops compiling.

Adjust the adapter indices and the peer address at the top of each file;
`hciN` numbering drifts across reboots, so check with `ble:list-hci-adapters`
rather than assuming. Each side hands its adapter back on every exit path,
including errors — a process killed outright leaves one down.

## Testing against it, without a radio

`make-att-test-channel` is a third kind of ATT channel, alongside the kernel
L2CAP socket and the `hci-conn`: one backed by a function instead of a
socket. `att-send` and `att-recv` dispatch on it like any other, so every line
of ATT protocol runs unchanged against a scripted peer.

```lisp
(let ((chan (ble:make-att-test-channel
             :responder (lambda (pdu)
                          (when (= (aref pdu 0) #x0A)          ; Read Request
                            (ble:coerce-octets #(#x0B #x2A))))))) ; Read Response
  (ble:att-read-value chan #x000C))          ;  => #(42), NIL
```

`att-test-channel-sent-pdus` returns what went out, which for something like
a range-scoped discovery is the only place the behaviour is visible at all.

It ships in the library rather than the test suite because a consumer building
its own GATT profile has the same problem. It also earns its keep: writing
tests through it immediately found a walk that never terminated when a peer
repeated itself, and an indication path that had been reported as implemented
while half of the edit had silently failed to apply.

## A trap worth knowing

If your package `:use`s `#:ble`, a `defun` of a name `#:ble` already exports
does **not** shadow it — it redefines the inherited function. A wrapper that
then delegates to that symbol calls itself forever, and it does not look like
an error, it looks like a slow compile. Give your specialised version its own
name.

## Consumers

- [ud18](https://github.com/lispnik/ud18) — ATORCH UD18 power meter
