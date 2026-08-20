# ble

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
make check   # also the I/O layer's radio-free parts (47 more)
```

## What it gives you

**Scanning.** `scan-reports` is the vendor-neutral loop — every advertising
report to a callback, bounded by seconds or count. `discover` merges reports
per address into `discovered` records, which matters more than it sounds: a
device's name arrives in a scan response and its service UUIDs in the
advertisement, so anything treating reports as devices sees each device
repeatedly, differently, and never whole.

Both the legacy 4.0 scan commands and the 5.0 extended ones are here, and you
need both. Extended is the only way to reach the Coded PHY; legacy is the only
thing some controllers implement.

**GATT.** `att-discover-characteristics` returns `gatt-char` records carrying
the property bitmap, which is how you tell a device's command channel from its
data channel without guessing. `uuid16`, `find-char-by-uuid`.

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
for you. Without that a peer sends one indication and then waits forever,
which presents as a device that stopped talking.

Not implemented: Read Multiple, Prepare/Execute Write (so no write longer than
MTU−3 to a single attribute), service discovery by group type, and SMP — this
cannot talk to a peer that requires a bonded link.

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

## A trap worth knowing

If your package `:use`s `#:ble`, a `defun` of a name `#:ble` already exports
does **not** shadow it — it redefines the inherited function. A wrapper that
then delegates to that symbol calls itself forever, and it does not look like
an error, it looks like a slow compile. Give your specialised version its own
name.

## Consumers

- [ud18](https://github.com/lispnik/ud18) — ATORCH UD18 power meter
