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

## Pairing and encryption

`smp-pair` runs LE Secure Connections over an established link and returns a
session whose LTK is ready to use; `smp-start-encryption` (central) and
`smp-answer-ltk-request` (peripheral) turn that key into an encrypted link.
Both roles are implemented.

**All three association models.** Which one runs is not chosen — it falls out
of both ends' IO capabilities and whether either asked for MITM protection
(`smp-association-model` computes it, and both ends must reach the same answer
or one would run twenty rounds while the other ran one):

- **Just Works** — no MITM protection at all. An attacker present during
  pairing can be both ends of it; it defends against passive eavesdropping and
  nothing more. That is a limit, not a footnote.
- **Passkey entry** — pass `:io-capability :display-only` (or
  `:keyboard-only`) and a `:passkey`, or a `:passkey-fn` to be asked for one.
  Twenty exchanges, one per bit, each committing both ends to a fresh nonce
  before either reveals it. That is what resists a man in the middle: an
  attacker who guesses wrong on any bit is caught on that round and cannot go
  back. It is also why it costs twenty round trips — the passkey leaks one bit
  at a time instead of all at once.
- **Numeric comparison** — `:io-capability :display-yes-no` and a
  `:confirm-fn` that is shown six digits and returns whether they matched. A
  caller with no way to ask must not silently accept, so a missing
  `confirm-fn` refuses.

**Scope.** Secure Connections only — legacy (4.0/4.1) pairing is not here, and
its Just Works variant offers no protection against a passive eavesdropper at
all. No key distribution: Secure Connections derives the LTK on both sides so
nothing needs sending for encryption, but IRK and CSRK are not exchanged,
which is why a bonded peer using a resolvable private address cannot yet be
recognised across rotations.

**The ECDH is in software, and not by preference.** These RTL8761B controllers
answer `LE Read Local P-256 Public Key` with the *same* key across separate
processes, and `LE Generate DHKey` with a value that is visibly not a random
32 octets — then return nothing at all on the second and third call. A
silently wrong shared secret is the worst failure mode pairing has, so this
uses ironclad's P-256 rather than the controller. AES-CMAC is ironclad's too;
the controller's AES-128 was verified correct against the FIPS-197 vector, but
having both in one place is worth more than saving the round trips.

**What is verified, and what is not.** RFC 4493 vectors cover AES-CMAC, and
`smp-dhkey` is checked for the property that makes ECDH work — both sides
reaching the same secret. Over two radios, an initiator and a responder in
separate processes derive a byte-identical LTK and the controllers report the
link encrypted — with a *different* LTK each run, and with passkey entry a
deliberately mismatched passkey is rejected rather than quietly falling back
to Just Works, each end naming the same cause from its own side: *the peer
rejected us: confirm value failed* against *we rejected the peer: confirm
value failed*. But **both ends are this code**, so agreement demonstrates
internal consistency rather than conformance: a systematically wrong `f5`
would agree with itself. Interop against an independent stack is the proof
that is missing. It was attempted against BlueZ three ways and none completed
— the Pi's built-in radio fails to scan with an I/O error, and both
`bluetoothctl` and `btmgmt` runs died without connecting. **Treat spec
conformance as unproven until a third-party peer pairs with this.**

## Streams: connection-oriented channels

`l2cap-coc-connect`, `l2cap-coc-listen` / `l2cap-coc-accept`,
`l2cap-coc-send`, `l2cap-coc-recv`, `l2cap-coc-close`. A stream between two
devices that is not GATT: ATT moves one value at a time and pays a round trip
for each, while a CoC carries an SDU of up to 64 KiB — the right shape for a
firmware image or a log download.

The flow control is the substance, and it runs opposite to most protocols: a
sender may transmit exactly as many frames as the receiver has granted credits
for, and not one more. Overrun is structurally impossible rather than merely
unlikely — no window to misjudge, no rate to tune. The cost is that a sender
with no credits must stop, and a receiver that forgets to replenish silently
wedges the channel, which looks exactly like a peer with nothing to say. So
replenishing happens here, on receipt, rather than being left to the caller.

Two sizes, easily confused: **MTU** is the largest SDU the peer will accept,
**MPS** the largest single frame. One SDU is split across as many frames as
MPS requires, and only the first carries a 2-octet length, so the receiver
knows it is done by counting rather than by any end marker.

`SPSM` is the LE equivalent of a port number. Below 0x0080 the SIG assigns
them; 0x0080 and up are free for two devices to agree between themselves.

The live harness runs 400 octets at MPS 96 against a listener granting **two**
credits, so the sender runs dry after two frames and has to wait to be topped
up mid-SDU. Granting enough up front would leave the flow control — the entire
point of the channel type — never actually exercised.

## When the peripheral wants different parameters

Only the central issues LE Connection Update, so a peripheral that wants a
slower link has to ask. That request goes over the L2CAP signalling channel
(CID 0x0005), not HCI: `l2cap-request-conn-params` sends one and returns an
identifier, and `l2cap-conn-param-result` collects the answer.

It sends without waiting, deliberately. Waiting means reading from the
transport, and on a peripheral the PDUs arriving are the central's requests —
so a blocking version swallows the very traffic the caller exists to serve.
Mine did, and it broke a long write that happened to overlap the request.
Whoever owns the read loop keeps owning it.

The answering side needs nothing: incoming signalling frames are handled from
the ordinary receive path via `*l2cap-signalling-handler*`, so a program using
this library answers a peer's request whether or not it knows the channel
exists. That matters because silence is indistinguishable, from the far end,
from the frames being dropped — which is what happened to every non-ATT CID
before this. Bind `*l2cap-accept-conn-param-updates*` to NIL to refuse, or the
handler itself to NIL to take the channel over.

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

This library is Linux and raw HCI, deliberately — not a BlueZ D-Bus client and
not portable to CoreBluetooth. Owning the controller is what makes the
initiating PHY selectable and `HCI_CHANNEL_USER` possible, and those are the
capabilities its consumers exist for. The cost is `CAP_NET_ADMIN` and a Linux
box to run on, which is the trade being made rather than a gap to close.

Not implemented:

- **LE privacy** (resolving list, RPA) and the controller filter-accept-list.
- **Read Multiple Variable Length** (0x20, from 5.2), refused as unsupported.
  Older Read Multiple returns values concatenated with no delimiters, so a
  client cannot split them without already knowing each length; 0x20 prefixes
  each with its length and fixes that. Low priority: anything it does can be
  done with separate reads, and **it cannot be tested here** — see below.

- **Signed Writes.** Verifying one needs a CSRK, which is distributed by
  bonding, which needs SMP. Until that exists the only honest thing to do with
  a signed write is ignore it — and since it is a command, silence is also the
  protocol-correct way to not support it. Accepting one unverified would be
  worse than refusing.

**One reader.** `hci-pump` is the only thing that reads from a connection's
socket. It routes and never consumes: ATT PDUs are filed in the connection's
pending queue, signalling frames answered, connection-oriented channels handed
to theirs, and HCI events returned to the caller that asked. Everything that
needs the transport — the ATT layer, an event wait, a CoC send waiting on
credits, the Security Manager — goes through it.

That is not an abstraction for its own sake. Five separate defects here were
one shape: a helper that needed the transport took it over and discarded
whatever it was not itself looking for. Reassembly kept one frame per read;
notification dispatch dropped every handle but the awaited one; a blocking
parameter request swallowed the peer's ATT requests; an event wait filed every
ATT PDU as a notification and lost an in-flight response; the signalling
server consumed a response another caller was blocked on. Each surfaced as an
unrelated timeout elsewhere, and four of the five were invisible to the suite.

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

**The dongles are Bluetooth 5.1.** Both report core version `0x0A`,
manufacturer 93 — Realtek, the RTL8761B in a TP-Link UB500. So nothing from
5.2 or later can be exercised here at all, whatever the code does: Read
Multiple Variable Length (0x20), Enhanced ATT bearers, LE Power Control, and
the 5.2 parts of the feature bitmap are all out of reach. **Testing those
needs two dongles newer than 5.2**, and until there are some, treat any claim
about them as unverified — the live harness is the only thing here that has
ever caught a protocol bug the suite missed, and it cannot catch these.

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
