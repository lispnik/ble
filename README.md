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
value larger than MTU−3. These are exercised by the test suite but not against
hardware — neither consumer has an attribute long enough to need them.

Not implemented:

- **SMP** — no pairing, bonding or encryption, so this cannot talk to a peer
  that requires a bonded link. The largest single gap.
- **A GATT server.** There is no attribute database, so this cannot answer
  discovery or serve characteristics. It *can* be a peripheral at the link
  layer — advertise connectable, accept a connection, and push notifications —
  which is what `tools/live-two-radios/` does; what is missing is everything
  above ATT PDUs.
- **Connection parameter update**, and the LE privacy features (resolving
  list, RPA) and controller filter-accept-list.

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
sudo sbcl --non-interactive --load tools/live-two-radios/peripheral.lisp &
sudo sbcl --non-interactive --load tools/live-two-radios/central.lisp
```

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
