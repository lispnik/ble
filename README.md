# ble

[![CI](https://github.com/lispnik/ble/actions/workflows/ci.yml/badge.svg)](https://github.com/lispnik/ble/actions/workflows/ci.yml)

A Bluetooth Low Energy library for SBCL on Linux: HCI sockets, adapter
enumeration, LE scanning, extended advertising, and an ATT/GATT client that
speaks over either of two transports.

## Trippy Demo 🍄

`examples/nus-pty/` is the practical one: it bridges a Nordic UART peripheral
to a pseudo-terminal, so a BLE device appears as `/dev/pts/N` and `screen`,
`minicom` or a shell script can drive it with no idea a radio is involved.
That is the honest answer to wanting the Serial Port Profile — SPP is a BR/EDR
protocol this library does not speak, and this delivers what SPP is wanted
*for*. Verified by talking to a peripheral with nothing but `printf` and `dd`.

`examples/lisp-repl/` is a Common Lisp REPL reachable over Bluetooth, and
`(lisp-repl-client:demo "AA:BB:...")` runs a scripted tour of it. What follows
is a real transcript between two USB dongles on a Raspberry Pi 4, not a
mock-up — the addresses, temperatures and signal strengths are whatever they
were at the time.

```
connected to FE:D5:8B:E1:76:7D
paired; encryption -> T
NUS attached, MTU 247

1. Where am I? Evaluated on the Pi, over the air.
   (machine-instance)
   => "rpi4"

2. And what is running there.
   (list (lisp-implementation-type) (lisp-implementation-version))
   => ("SBCL" "2.5.2.debian")

3. A real sensor reading -- no characteristic was defined for this, and none had to be.
   (with-open-file (s "/sys/class/thermal/thermal_zone0/temp") (/ (read s) 1000.0))
   => 58.426

4. Define a function in the running image. Note what this does NOT do: it adds nothing to the GATT database, and no client is told anything.
   (defun celsius->f (c) (+ 32 (* 9/5 c)))
   => CELSIUS->F

5. ...and call it anyway. It is reachable because the REPL evaluates whatever arrives -- not because anything was wired up.
   (celsius->f 21)
   => 349/5

6. Move into the library's own package.
   (in-package :ble)
   => #<PACKAGE "BLE">

7. Ask the Bluetooth stack, over Bluetooth, what radios it has.
   (mapcar (lambda (a) (list (hci-adapter-index a) (hci-adapter-bus a))) (list-hci-adapters))
   => ((0 :SERIAL) (1 :USB) (2 :USB))

8. Now the self-referential part: the flow-control window of the very link this answer is travelling on.
   (hci-socket-acl-credits (hci-conn-sock lisp-repl:*connection*))
   => 8

9. And its signal strength, in dBm. The device is telling us how well it can hear us, using the link it is telling us over.
   (hci-read-rssi lisp-repl:*connection*)
   => -19

10. Errors come back as values rather than taking the device down.
   (/ 1 0)
   => ; DIVISION-BY-ZERO: arithmetic error DIVISION-BY-ZERO signalled
Operation was (/ 1 0).

11. And the reader is not a back door: #. is refused, not computed.
   #.(+ 1 2)
   => ; SIMPLE-READER-ERROR: can't read #. while *READ-EVAL* is NIL

                         Stream: #<dynamic-extent STRING-INPUT-STREAM (unavailable) from "#.(+ 1 2)">
```

Steps 4 and 5 are worth pausing on, and worth being precise about. `defun`
puts a function in the running image; it wires nothing into GATT, adds no
characteristic, and notifies no client. The database is exactly what it was.
Step 5 reaches it anyway, because the one characteristic that *is* in the
database evaluates whatever arrives.

That is the whole trick, and it is smaller than it looks: a profile can only
answer questions somebody anticipated when they defined the characteristics,
and this sidesteps that by defining a characteristic whose meaning is
"evaluate this" — which is also precisely why it needs an encrypted link.

Steps 8 and 9 are the reason `lisp-repl:*connection*` exists. It is bound to
the live connection while a session is up, so evaluated code can interrogate
the link it is arriving over: step 8 reports the ACL flow-control window of
that link, and step 9 asks the device how well it can hear you — answering
over the very link it is reporting on.

Step 5 returning `349/5` rather than `69.8` is not a rounding failure. It is
exact rational arithmetic surviving a round trip that a byte-oriented protocol
would have flattened to a float.

And it is encrypted: the REPL's characteristics require a paired link by
default, because an unauthenticated remote evaluator is precisely a remote
shell. `#.` is refused at read time for the same reason — see step 11.

### A Bluetooth device that is also a serial port

`examples/nus-pty/` opens a pseudo-terminal and pumps bytes between it and a
Nordic UART peripheral. One terminal runs the bridge:

```
/dev/pts/0 is now D9:47:7B:29:36:7E
(screen /dev/pts/0, or point anything expecting a serial port at it)
37 octet(s) out, 63 in
```

Another drives that node with nothing but `printf` and `dd` — no Lisp, no
Bluetooth library, no idea a radio is involved:

```
/dev/pts/0 is a Bluetooth device. Talking to it with printf and dd:
  -> help                         <- commands: help, time
  -> time                         <- 23:27:37
  -> echo through a device node   <- through a devicno such command: e n
```

`time` is right. The other two are the peripheral's limits showing through,
and they are worth looking at, because a transparent bridge is exactly what
exposes them: `help`'s reply is **truncated at a 23-octet MTU**, because
`gatt-notify` cuts to the MTU rather than chunking; and the 27-octet line
arrived as **two ATT writes**, each of which `examples/nordic-uart/` answers
as a separate command — the caveat its own docstring gives, seen from the
outside.

Neither is the bridge misbehaving. A serial port has no message boundaries
either, and a device that needs them has to impose them. This is the honest
answer to wanting the Serial Port Profile: SPP is a BR/EDR protocol this
library does not speak, and what people want it *for* is a device node.

### Growing a thermometer while somebody is connected

Step 4 above adds nothing to GATT. This does. `(lisp-repl-client:grow-a-thermometer
"AA:BB:...")` builds a real Health Thermometer service on a live peripheral,
from the other end of the link, and then reads the Pi's CPU temperature back
through it:

```
connected to DE:A5:AD:56:D7:2A
paired; encryption -> T
subscribed to Service Changed at handle 6
before: no health thermometer service
   (in-package :ble)
   => #<PACKAGE "BLE">
   (gatt-add-service lisp-repl:*server* +service-health-thermometer+)
   => 14
   (defparameter cl-user::*t* (multiple-value-list (gatt-add-characteristic lisp-repl:*server* :uuid +char-temperature-measurement+ :properties (list :indicate))))
   => COMMON-LISP-USER::*T*
Service Changed arrived: handles 1..65535 moved
after:  a health thermometer service
0x2A1C says: 57.450 C
```

The attribute vector is adjustable and handles keep counting, so growing a
database is allowed. What makes it *usable* is **Service Changed** — a client
discovers a database once and caches it, and has no other way to be told it
moved. Four examples in this repository declare that characteristic; this is
the only one that ever indicates on it.

There is a condition worth knowing: the specification only requires a peer to
honour Service Changed when it is **bonded**. An unbonded client caches by
address and will keep the stale database however loudly you indicate. The REPL
pairs and bonds on the way in, which is why it works here and would not on
most of the other examples.

`0x2A1C` is indicated rather than read, as the real profile requires — see
`examples/health-thermometer/` for why that distinction matters.

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

`ble/examples` holds the worked examples; it depends on `ble` and nothing
else depends on it.

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

## Writing a peripheral

`ble/examples` is a system of worked ones.

`examples/scanner/` comes first because it is what you do first. Every other
client here takes a MAC address, and this is where the address comes from —
`(scanner:find-service ble:+service-health-thermometer+)` hands back the
thermometer. It is also the only example that scans, and so the only one that
touches the **Coded PHY**: `start-extended-scan` listens on 1M and Coded
together, and a fleet advertising long-range is simply invisible to anything
that does not. Its `watch` prints reports unmerged, which is worth running
once to see why merging exists — the name arrives in a scan response and the
service UUIDs in the advertisement, so a report is not a device.

`examples/heart-rate/` is a
complete Bluetooth heart rate sensor in about 150 lines, plus the client that
reads it — each in its own package, using only exported symbols.

`examples/health-thermometer/` is the companion, and exists for what it does
differently. A heart rate sensor notifies; a thermometer **indicates**, and an
indication is acknowledged — only one may be outstanding at a time, so a
peripheral that sends the next before the confirmation arrives is violating
GATT however well it appears to work. It carries both characteristics on
purpose: Temperature Measurement indicates the settled reading, Intermediate
Temperature notifies the reading on its way there, and the difference sits in
one file. Temperatures are also IEEE-11073 FLOATs rather than integers —
decimal, so 36.85 °C is exactly 3685 × 10⁻², which binary floating point
cannot say.

`examples/glucose/` stops being a sensor. A meter is a database: its readings
were taken hours ago, and a client asks for them by writing a query to the
Record Access Control Point — one write starts a procedure, the records stream
back as notifications on a *different* characteristic, and an indication ends
it. The distinction it is built around is that refusing a write and failing a
procedure are different things, and a client that confuses them waits forever
for a reply that was already refused. It is also the first example requiring
an encrypted link, on both sides: the client pairs as a central before it can
subscribe to anything.

`examples/environmental-sensing/` is the smallest, and exists for one property
the others do not have — a service carrying the same characteristic more than
once. Three Temperatures, all `0x2A6E`, told apart only by their descriptors.
Look one up by UUID and you have silently picked one of three.

`examples/object-transfer/` is the only one whose payload never crosses an
attribute. Everything else notifies, which caps a message at the ATT MTU;
Object Transfer is the SIG's answer to that, and the only adopted profile that
opens an **L2CAP connection-oriented channel** — a real stream with
credit-based flow control, alongside the ATT bearer on the same connection. So
it has two halves that must agree: GATT carries the metadata and two control
points, and the bytes go over the channel. Ask to read without opening the
channel first and you are refused with Channel Unavailable, because the server
has nowhere to put the answer.

`examples/nordic-uart/` is a serial port, and the first example that is not a
SIG profile at all — NUS is Nordic's, and it is here because so many vendor
devices speak it. It is also the first with **128-bit UUIDs**, which is what
`ble:uuid128` exists for: the wire form is the reverse of the written form, so
a hand-reversed sixteen-octet literal cannot be reviewed. The naming trap gets
its own assertions — a host *writes* to RX and *reads* notifications from TX,
because the names are the device's point of view.

`examples/broadcaster/` never accepts a connection at all. The advertisement
*is* the message, and it uses **extended advertising** to get past two legacy
limits: 31 octets of payload (it sends 42, including the full NUS UUID that
the UART example cannot fit) and 1M-only (it can advertise on the **Coded
PHY**). `examples/scanner/` is the other half and hears both.

`examples/provisioning/` is about **long writes**. Every other example sends
values that fit one ATT payload, because that is what a measurement is; a
configuration is not — an SSID is up to 32 octets, a WPA passphrase up to 63,
and a server URL beside them. So it travels as a queued write: Prepare Write
puts fragments in a queue and Execute Write commits them. The point of the
example is that Execute Write can also **cancel**, discarding the queue
untouched, because a queued write is one write that arrives in pieces rather
than several writes — a peer that disconnects halfway through configuring a
device must not leave it holding half a new SSID and all of an old passphrase.
Reading it back is the same problem mirrored, through Read Blob.

That is why they are a system rather than loose scripts. Loading a file by
hand can succeed for the wrong reason, since the reader picks up whatever
package happens to be current; loading the system fails the way it would fail
for a consumer. Writing the heart rate example found two holes in the API that the
library's own tests could not: an unexported `gatt-server-attributes`, and a
peripheral having to rummage in an internal frame queue to ask whether the
peer wanted to pair — now `smp-pairing-requested-p`. The thermometer found two
more: there was no way to add a descriptor other than a CCCD, which Health
Thermometer requires (`gatt-add-descriptor`), and no way for a peripheral to
notice its indication had been confirmed (`+att-handle-value-cfm+`). The
glucose meter found that an `on-write` handler had no name for the ATT error
it returns, and the weather station that `find-char-by-uuid` quietly returns
the first of several — now `find-chars-by-uuid`, with the singular one's
docstring saying what it does.

```sh
make examples        # build them, and check every database
```

```lisp
(asdf:load-system :ble/examples)
(scanner:survey :dev 1)                          ; what is out there?
(scanner:find-service ble:+service-heart-rate+)  ; ...and where is the belt
(heart-rate:run :dev 1)                          ; be a heart rate sensor
(heart-rate-client:run "DC:95:E3:F9:9E:58")      ; read one
(health-thermometer:run :dev 1)                  ; be a thermometer
(health-thermometer-client:run "DC:95:E3:F9:9E:58")
(glucose:run :dev 1)                             ; be a glucose meter
(environmental-sensing:run :dev 1)               ; be a weather station
(object-transfer:run :dev 1)                     ; serve objects over L2CAP
(nordic-uart:run :dev 1)                         ; a serial port over BLE
(broadcaster:run :dev 1 :phy :coded)             ; a long-range beacon
(provisioning:run :dev 1)                        ; take a configuration
(lisp-repl:run :dev 1)                           ; a REPL, over the air
(lisp-repl-client:demo "C7:06:A2:90:43:53")      ; ...and a narrated tour of it
```

```lisp
(ble:with-hci-user-socket (sock (ble:default-hci-dev))
  (ble:set-adv-data sock (ble:adv-data :flags '(:general-discoverable :no-bredr)
                                       :name "Lisp HRM"
                                       :services-16 '(#x180D)))
  (ble:serve-peripheral server sock
                        :on-connect (lambda (conn peer type) ...)
                        :on-tick    (lambda (conn)
                                      (ble:gatt-notify server conn handle value))))
```

`serve-peripheral` is the loop every peripheral needs, and it exists because
two things about it are less obvious than they look:

- **Advertising stops the moment a central connects.** That is the
  specification, not a fault. A peripheral that does not re-enable it after a
  disconnect silently vanishes: the process looks healthy, the adapter is up,
  and the device is simply not there. This cost two debugging sessions before
  it was understood.
- **A disconnect is not a queued event.** `hci-pump` reports it as
  `:DISCONNECTED` and files nothing, so a loop that ignores the return value
  waits forever for a client that left.

`peripheral-accept` handles the other trap: the connection arrives as either
the plain or the Enhanced Connection Complete subevent depending on the
controller's event mask, and handling only the first presents as a peripheral
nobody can connect to.

`adv-data` builds the payload. It signals rather than truncating when the
result will not fit in 31 octets, because the overrun is silent on the wire and
looks like a device nobody can see. Advertising the service UUID matters more
than it appears: a heart rate sensor that omits `0x180D` is invisible to every
heart rate app, however correct the database behind it.

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
Read By Group Type, Read, Read Blob, Read Multiple, Read Multiple Variable
Length, Write, Write Command, and Prepare/Execute Write, and refuses anything
else with `request not supported`.

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

**Verified against an independent implementation.** f4, f5, f6 and g2 all
match the Core spec's published Secure Connections vectors, and ironclad's
P-256 matches a from-scratch implementation of the curve arithmetic. Over two
radios an initiator and a responder derive a byte-identical LTK and the
controllers report the link encrypted, with a different LTK each run; with
passkey entry a mismatched passkey is rejected rather than falling back to
Just Works.

**And against a phone**, which matters more than all of it. An iPhone pairs
with this library and encrypts the link: the check values agree exactly, and
the controllers report encryption enabled with the key both ends derived.
That is the only test that ever found anything: f5's salt was written here
from memory, agreed with the real value for three octets, and then diverged.
A wrong salt yields a MacKey and LTK that are wrong but perfectly
self-consistent — so two implementations sharing the mistake pair happily with
each other and with nothing else. The two-radio test passed throughout,
including every passkey run. Only a peer that did not share the error could
show it, and one did.

The published vectors are now in the suite. A constant that cannot be reasoned
about has to be checked against a source, not recalled.

## Pairing, from a peripheral

`serve-peripheral` takes a `:pairing`, and drives the exchange itself:

```lisp
(let ((pairing (ble:make-peripheral-pairing
                :local-addr addr        ; the address being advertised
                :irk (ble:smp-random-octets sock 16)
                :on-paired (lambda (conn session bond) ...))))
  (ble:serve-peripheral server sock :pairing pairing ...))
```

`local-addr` is not optional: it is bound into the pairing crypto, so a
peripheral that pairs as one address while advertising another produces
confirm values the peer cannot verify.

The sequencing it removes from consumers is short but exact, and each step
fails as something else when it is wrong. The Long Term Key Request comes from
the *controller*, not the peer, and must be answered from the session just
negotiated or from a stored bond — unanswered, the link never encrypts and the
central gives up silently. Keys are distributed *over* the encrypted link, so
the identity exchange belongs after Encryption Change and not when `smp-pair`
returns; collecting them earlier stores a bond against an address that expires
within minutes.

This was written out by hand in `examples/glucose/` and again in
`tools/pair-with-phone/`, and the second copy had already drifted from the
first.

## Bonds, and peers that change address

`smp-pair` gives you keys; a bond is what makes them worth having. Without one
a device re-pairs on every connection, which has the security properties of
never pairing at all.

The hard part is not storage, it is identity. A phone connects from a
resolvable private address that changes every few minutes, so the address a
bond was made at is not the address the peer returns on. What links them is
the identity key the peer distributes: `find-bond` tries every stored IRK
against the address in front of it, using `resolve-address` (the spec's `ah`,
checked against its published vector).

Two orderings matter and both were got wrong first:

- **Keys are distributed over the encrypted link**, not as part of pairing.
  Asking as soon as pairing completes collects nothing, and the bond then gets
  stored against whatever address the peer happened to use — an address that
  stops being true within minutes.
- **`smp-answer-ltk-request` takes the connection, not the session.** The
  interesting case is a returning peer, where there is no session at all; it
  answers from a stored bond instead, and sends a *negative* reply when it has
  neither, so a peer holding a bond we have forgotten starts a fresh pairing
  rather than waiting out a timeout.

Bonds persist through `*bond-file*` at mode 0600. They are long-term keys in
plain text; anything stronger needs a passphrase this library has no way to
ask for.

Verified against a phone: it pairs once, and on later connections arrives at
an address never seen before, is resolved to its identity through the IRK it
distributed, and encrypts from the stored key with no pairing prompt.

**A note on randomness, which turned out to matter.** `smp-random` XORs the
controller's `LE Rand` with `/dev/urandom`, because the controller's output is
measurably not random across a reset.

Measured, not assumed: on these RTL8761B dongles, **39 of 100** 32-bit words
drawn after one controller takeover reappeared after the next. Chance would
give 0.000002. Within a single takeover every value is distinct, so nothing
looks wrong until the process restarts and draws again — which is exactly how
it surfaced, as a peripheral advertising the same "random" address five runs
running. The same part also returns a fixed P-256 public key across separate
processes, and answers `LE Generate DHKey` with a value that is visibly not a
shared secret.

`tools/rng-check/` measures it, so anyone can find out what their own part does
rather than inherit the assumption. Any shared word at all is a finding. It
does not follow that keys derived from it are recoverable — that would take
real analysis — but it does mean the generator is not doing what its
specification claims, and key material must not rest on it alone.

XOR is the right combiner: the result is as unpredictable as the better of the
two sources, so this is no worse than either alone, where choosing one would
have been a bet.

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

Long and batched access: `att-read-long-value`, `att-read-multiple` and
`att-read-multiple-variable`,
`att-prepare-write` / `att-execute-write`, and `att-write-long-value` for a
value larger than MTU−3. Sized by `att-mtu` — the MTU *agreed* with the peer,
which is not the same number as `*att-rx-mtu*`, what we advertise we can
receive. Prefer `att-read-multiple-variable` over `att-read-multiple` where
the peer supports it: the older opcode returns the values concatenated with
nothing between them, so a client that does not already know every width
cannot tell where one ends, and a value truncated by the MTU is
indistinguishable from a short one. The newer opcode length-prefixes each
value, which makes both problems go away — and a peer that has not
implemented it says so with `request not supported`. They differ whenever the peer offers less, and sizing by the wrong
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

**Two transports, one ATT layer** (`src/att.lisp`). `att-send` and `att-recv` dispatch on
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

**The dongles are Bluetooth 5.1.** Both TP-Link UB500s report core version
`0x0A`, manufacturer 93 — Realtek RTL8761B. A third dongle, a Barrot part,
reports core version `0x0D` (5.4), but reporting it is all it does: its
`LE Read Local Supported Features` comes back `FF F9 01 00 00 00 00 00`, every
bit from 24 up clear, so it has no 5.2 or 5.3 LE feature either. It does carry
LL Privacy and Periodic Advertising, which the Realteks do not. **A version
byte is a claim; the feature bitmap is the capability, and they disagree
here.**

So the genuinely controller-side 5.2 features — LE Power Control, path loss
monitoring, the 5.2 parts of the feature bitmap — remain untestable on this
hardware, and any claim about them should be treated as unverified.

**But two things once listed here as blocked were never blocked by hardware at
all**, and saying so was a mistake worth undoing. Read Multiple Variable
Length is an *ATT* opcode and Enhanced ATT bearers are an *L2CAP* mode: both
are host-layer, carried as ordinary ACL payload the controller never inspects.

Read Multiple Variable Length is now implemented, and it settled the argument
by running. `tools/live-two-radios/read-multiple-variable.lisp` passes 5/5
**between two of the 5.1 Realteks** — the radios this section called too old
for it — as well as with the 5.4 Barrot as the client. What was blocking it
was that nobody had written it, which is a different and much cheaper problem
than needing new radios.

EATT is now implemented too, and likewise runs between the 5.1 dongles:
`tools/live-two-radios/eatt.lisp` passes 16/16 there, opening three bearers,
serving GATT on each, and raising their MTU from 128 to 512 in one
reconfiguration. See "Enhanced ATT bearers" below for what that does and does
not demonstrate.

`compile-check.lisp` beside them needs no hardware at all: it loads every
definition, compiles the forms that drive the radios without running them, and
asserts on the attribute layout the server builds. CI runs it on both
architectures, because a harness that cannot be exercised is a harness that
quietly stops compiling.

Adjust the adapter indices and the peer address at the top of each file;
`hciN` numbering drifts across reboots, so check with `ble:list-hci-adapters`
rather than assuming. Each side hands its adapter back on every exit path,
including errors — a process killed outright leaves one down.

## The assigned numbers

`src/uuids.lisp` holds numbers that are not ours — the SIG publishes them, and
a peripheral that wants to be recognised must use exactly them. A wrong one is
invisible: the device advertises correctly, connects correctly, and is simply
never recognised by the app looking for it.

```sh
python3 tools/check-uuids/check-uuids.py src/uuids.lisp
```

checks every constant against the SIG's own machine-readable Assigned Numbers
at `bitbucket.org/bluetooth-SIG/public`, which is the registry rather than
somebody's copy of it, and runs in CI. What it cannot check is anything
defined in the GATT Specification Supplement instead — the Environmental
Sensing application codes, for one — because that is published only as a PDF.
That is why `examples/environmental-sensing/` sends Unspecified rather than a
guessed placement code.

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

## Flow control, and how the library used to wedge its host

A controller has a small number of buffers for outbound data — on the dongles
this was written against, **eight packets of 27 octets**. It reports that count
in `LE Read Buffer Size`, spends one per ACL packet, and hands them back in
`HCI_Number_Of_Completed_Packets` as the radio drains them. That window is the
only thing making a host wait for a radio three orders of magnitude slower
than it.

This library read the packet *length* from that response and discarded the
*count*, and never read the returning event at all. So `hci-acl-send-l2cap`
just wrote, forever. Under sustained sending the surplus queued as pending USB
transfers, each holding a swiotlb bounce-buffer slot, until that **machine-wide**
pool was exhausted — at which point Bluetooth URBs failed with `EAGAIN` and the
SDIO Wi-Fi driver, needing buffers from the same pool, blocked forever and took
the host's network down with it. Twice.

It now keeps the credit window. Sending blocks when it is empty, pumping while
it waits, because the event that refunds a credit arrives on the transport
being pumped. `tools/nus-throughput/` is what found this, and measures what is
left once the sender is made to wait:

| path | default interval | at 7.5 ms |
|---|---|---|
| NUS, central → peripheral (Write Command) | 34.9 kbit/s | **204.8 kbit/s** |
| NUS, peripheral → central (notifications) | 26.4 kbit/s | **154.5 kbit/s** |
| L2CAP CoC, 32 KB object (`examples/object-transfer/`) | 25.2 kbit/s | **75.6 kbit/s** (at 15 ms) |

Zero backpressure stalls throughout, and in every case the two ends'
independent byte counts agree.

**The connection interval is the whole game.** Credits come back once per
connection event, so the event rate *is* the packet rate and everything else
is downstream of it. The peripheral's controller was choosing about 45 ms;
asking for 7.5 ms with `hci-connection-update` — which this library has always
had, and which no example or benchmark called — multiplied both directions by
5.9, against a predicted 6.

What is left is worth knowing for what it is not. The connection-oriented
channel is *not* faster than notifications (25 against 26 kbit/s at the same
interval): a 512-octet SDU is six L2CAP frames and about twenty ACL packets
before it reaches the air, so CoC buys arbitrary-length SDUs and its own flow
control, not speed. Beyond the interval there is less left than it looks. The **2M PHY** is now
implemented (`hci-set-phy`, on a live connection rather than the preference
`hci-set-default-phy` states) and makes no measurable difference here — 204.1
against 203.8 kbit/s — because air time is not the constraint. The controller
has eight ACL buffers of 27 octets and returns credits once per connection
event, which puts the ceiling at

    133 events/s × 8 packets × 27 octets ≈ 230 kbit/s of L2CAP payload

and the measured 204 is 95% of that after fragmentation overhead. Halving the
time a packet spends in the air does nothing when only eight may be in flight
per event and the interval is already at the specification minimum.

That leaves **Data Length Extension** (`LE Set Data Length`, not implemented)
as the only remaining lever, and its value here is genuinely uncertain: these
dongles report the specification-minimum 27-octet LE ACL buffer, so the host
cannot hand over more per buffer whatever the air PDU becomes.

The bulk-transfer example clients ask for a shorter interval; the sensor
examples deliberately do not, because a battery peripheral notifying once a
second wants the opposite trade.

## Enhanced ATT bearers

Ordinary ATT gets one channel per connection, fixed CID `0x0004`, and one
outstanding request on it. A slow read blocks everything behind it, including
notifications, because there is nowhere else for them to go. EATT gives ATT
several channels instead, each carrying its own transaction.

```lisp
;; Server: off by default, so nothing that worked before changes.
(ble:serve-peripheral server sock :eatt t)

;; Client:
(let ((bearers (ble:eatt-connect conn :count 3 :mtu 128)))
  (ble:att-read-value (first bearers) handle)      ; ATT, on a bearer
  (ble:eatt-reconfigure bearers :mtu 512)          ; wider, once open
  (mapc #'ble:eatt-close bearers))
```

**What it buys, and what it does not.** Interop, and independence: Android 12
and recent iOS open EATT when they see the PSM, and a notification can go out
while a long read is in flight. It does **not** buy throughput. This library
is single threaded, so the bearers are independent, not parallel — nothing
here runs at the same time as anything else, and a benchmark will not show a
bearer count in it.

**It needs no 5.2 hardware.** EATT is L2CAP, which is host layer: the frames
are ACL payload and the controller never looks inside them. The live test runs
between two Bluetooth 5.1 dongles.

Three things differ from the fixed channel and are worth knowing:

- **Each bearer has its own MTU**, settled by L2CAP when it opens, with a
  floor of 64 rather than 23. `gatt-server-mtu` therefore stops being the
  single number that sizes every response; `*att-bearer-mtu*` is bound while a
  bearer is served and takes precedence.
- **Exchange MTU is prohibited** on a bearer, and refused in both directions —
  the client will not send one, the server answers Request Not Supported.
- **Bearers are refused until the link is encrypted.** `:eatt :insecure` lifts
  that for a bench, and is spelled that way so it cannot happen by accident.

### What the tests here do not prove

The unit tests cover the decisions — which channels open, at what MTU, what a
refusal says, the reconfiguration rules — and the live test covers the
transport. Neither covers **interoperability**, and it is worth being exact
about why: both radios run this library, so any misreading of the
specification is shared by both ends and a green run says only that we are
self-consistent.

### Tested against an iPhone, which declined

Offered to an iPhone through `tools/pair-with-phone/`, with the PSM open and
Server Supported Features (`0x2B3A`) bit 0 set, over a paired and encrypted
link. **The phone never asked for a bearer.** It discovered the
characteristics, did not read `0x2B3A`, never wrote its own Client Supported
Features, and never touched the PSM — across a first pairing, a fresh pairing
after forgetting the device, and a reconnection from cache.

Our side was eliminated rather than assumed: the attribute table has `0x2B3A`
at handle 11 with the EATT bit set and readable, and the same code passes
16/16 over the air between two dongles. The first attempt did find a real
omission — the characteristic was missing entirely, so a conforming client had
no way to learn EATT was on offer and correctly did not try — but adding it
did not change the outcome.

So EATT interop is **untested, not disproved**. What the same session did
confirm is worth recording, because it is the same kind of evidence and it
came out positive: LE Secure Connections pairing with matching `Ea`, LTK and
IRK exchange, bond storage, and — on reconnection behind a different
resolvable private address — resolution back to the bonded identity and
re-encryption from the stored LTK, with no re-pairing. Those are Apple's
implementation agreeing with ours, which is exactly what the two-radio tests
cannot show.

Android 12 and later are reported to be readier to open EATT and are the
obvious next probe. Apple's PacketLogger, with the Bluetooth debug profile
installed on the phone, would show iOS's side of the decision directly rather
than leaving it to be inferred from what did not arrive.

Two things are specifically unconfirmed. The simultaneous-open tie-break —
when both peers ask for bearers at once, somebody must yield, and this yields
by role — is written from reasoning rather than from a shipping
implementation, because Linux implements the credit-based channels underneath
EATT without implementing EATT itself. And Reconfigure initiation has only
ever been answered by our own responder. A phone would settle both in minutes
and is the obvious next step.

The wire formats themselves are not guesswork: they were read off
`include/net/bluetooth/l2cap.h`. That mattered, because Reconfigure Request
carries the **sender's** source CIDs, the opposite convention from
Disconnection Request, and getting it backwards makes every channel look
unknown while looking entirely reasonable in the code.

## A controller quirk worth knowing about

After a session with an encrypted link, the kernel logs this and — on some
adapters — resets the dongle over USB:

```
Bluetooth: hciN: command tx timeout
Bluetooth: hciN: Resetting usb device.
```

The device then re-enumerates at a *new* `hciN` index. That is why nothing in
this repository should select an adapter by index and expect it to keep:
`tools/nus-throughput/` sorts by BD_ADDR instead, and the comment there
explains why. That part has never been in doubt and is the only part that
affects how you write code against this library.

**The attribution below was wrong once, and the correction is the useful
part.** This section previously said the cause was Realtek firmware failing to
answer the kernel's shutdown Reset, that it was therefore unavoidable from
userspace, and that a non-Realtek adapter would be the test. The test arrived,
in the form of a Barrot BT5.4 dongle, and it refuted the claim: **a
non-Realtek controller produces `command tx timeout` too.**

What survived the correction, and what did not:

- **Not chipset-specific.** Both a Realtek RTL8761B and a Barrot controller
  produce the timeout.
- **The USB reset *is* chipset-specific**, which is what made the two look like
  one thing. `hci_cmd_timeout` (`net/bluetooth/hci_core.c`) ends with
  `if (hdev->reset) hdev->reset(hdev);` — and `btusb.c` assigns `hdev->reset`
  for Intel, MediaTek, Qualcomm and Realtek only. So on Realtek a timeout
  becomes a USB reset and a new index; on the Barrot the same timeout is logged
  and nothing else happens. The index churn is a Realtek amplifier on a general
  problem.
- **Not deterministic, and workload-dependent.** The earlier "every time,
  exactly twice" was measured on one heavy workload — a full `serve-peripheral`
  session with pairing, GATT, NUS and a scripted conversation over it. A
  minimal pair-and-encrypt session on the *same two dongles* produces **zero**
  timeouts across repeated runs, and even the heavy workload sometimes comes up
  clean. Anything claiming an invariant here should be measured over several
  runs before it is believed; this section previously was not.
- **It is not the shutdown Reset going unanswered.** The message we get is the
  bare `command tx timeout` with **no opcode in it**. Reading `hci_cmd_timeout`
  again, that is the `else` branch — the one taken when `hdev->req_skb` is
  NULL, meaning the command timer fired with *no command outstanding*. A
  controller ignoring a Reset would name the opcode. So the thing to chase is a
  timer left armed, not a firmware that will not answer.

What was tried and did not help: disconnecting before releasing the adapter,
waiting for the Disconnection Complete before releasing it, and resetting the
controller ourselves with a longer timeout than the kernel allows itself. The
first two are right on their own terms and were kept; the third was reverted.

So this is open, with a much better lead than it had. The practical
consequence is unchanged — look adapters up by address, never by index — and
any *other* socket open on a dongle that gets USB-reset dies with it, which is
what makes a second session immediately after a first one fail unless the
indices are looked up again.

## Commands the controller refused

`send-hci-command` waits for the Command Complete or Command Status and
signals `hci-command-error` on a non-zero status. It did not always: for a
long time it wrote the command and walked away, so a controller refusing one —
an out-of-range parameter, an unsupported command, an adapter in the wrong
state — was completely silent, and the symptom was a device that configured
itself successfully and then did nothing.

Two call sites pass `:check nil` deliberately, both because something else was
already reading that answer: the create-connection retry loop, whose
`%await-le-connection` turns a refusal into `:failed` and tries again, and the
connection-cancel path, where Command Disallowed is the expected reply and a
signal would skip the event drain that is the point of the call.

Checking has to read the socket, and on a live connection `hci-pump` is the
only reader — so anything read while looking for the answer is put back on the
socket's pending queue rather than dropped, and `read-hci-packet` hands those
out first. Discarding them instead would be the same two-readers bug this
library has had in several other guises.

## A trap worth knowing

If your package `:use`s `#:ble`, a `defun` of a name `#:ble` already exports
does **not** shadow it — it redefines the inherited function. A wrapper that
then delegates to that symbol calls itself forever, and it does not look like
an error, it looks like a slow compile. Give your specialised version its own
name.

## Consumers

- [ud18](https://github.com/lispnik/ud18) — ATORCH UD18 power meter
