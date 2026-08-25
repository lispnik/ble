#!/usr/bin/env python3
"""Check every assigned number in src/uuids.lisp against the Bluetooth SIG.

src/uuids.lisp says its constants are "not ours: they are published by the
Bluetooth SIG, and a peripheral that wants to be recognised has to use exactly
them".  That is a claim, and until this script existed nothing checked it --
the numbers were transcribed by hand, and a wrong one is invisible: a
peripheral with a mistyped service UUID advertises correctly, connects
correctly, and is simply never recognised by the app looking for it.

The source is the SIG's own machine-readable Assigned Numbers, published at

    https://bitbucket.org/bluetooth-SIG/public/src/main/assigned_numbers/

which is the registry itself rather than somebody's copy of it.  Community
databases are convenient and mostly right; "mostly" is the problem.

Not everything is there.  Values defined in the GATT Specification Supplement
-- the Environmental Sensing application and sampling-function enumerations,
for instance -- are published only as a PDF, so this script cannot check them
and does not pretend to.  That is exactly why examples/environmental-sensing/
sends Unspecified rather than a guessed placement code.

Usage:  python3 tools/check-uuids/check-uuids.py [path/to/uuids.lisp]
Exits non-zero if any constant disagrees with the registry.

Stdlib only, so it runs anywhere without a virtualenv.  It needs network
access; with none it says so and exits 0, because a check that could not run
is not a failure of the thing being checked.
"""

import re
import ssl
import sys
import urllib.error
import urllib.request

BASE = "https://bitbucket.org/bluetooth-SIG/public/raw/main/assigned_numbers"

# Which registry file backs which family of constant in uuids.lisp.
SOURCES = {
    "service":    f"{BASE}/uuids/service_uuids.yaml",
    "char":       f"{BASE}/uuids/characteristic_uuids.yaml",
    "descriptor": f"{BASE}/uuids/descriptors.yaml",
    "object":     f"{BASE}/uuids/object_types.yaml",
}


def fetch(url):
    ctx = ssl.create_default_context()
    with urllib.request.urlopen(url, timeout=30, context=ctx) as r:
        return r.read().decode("utf-8", "replace")


def parse_uuid_yaml(text):
    """{number: name} from a SIG uuids YAML file.

    Deliberately not a YAML parser.  These files are a flat list of
    `- uuid:`/`name:` pairs, and the point is to avoid a dependency for a job
    this small; a real parser would be more correct and less likely to be run.
    """
    out, current = {}, None
    for line in text.splitlines():
        m = re.match(r"\s*-?\s*uuid:\s*(0x[0-9A-Fa-f]+)", line)
        if m:
            current = int(m.group(1), 16)
            continue
        m = re.match(r"\s*name:\s*(.+?)\s*$", line)
        if m and current is not None:
            out[current] = m.group(1).strip("\"'")
            current = None
    return out


def parse_appearance_yaml(text):
    """{value: name} for appearance.

    A value is category * 64 + subtype, and both levels matter: 0x0340 is a
    generic Heart Rate Sensor and 0x0341 is a Heart Rate Belt.  Reading only
    the categories -- which this did at first -- reports every subtype as
    missing from a registry that has it.
    """
    out, category, category_name, sub_value = {}, None, None, None
    for line in text.splitlines():
        m = re.match(r"\s*-?\s*category:\s*(0x[0-9A-Fa-f]+)", line)
        if m:
            category = int(m.group(1), 16)
            category_name, sub_value = None, None
            continue
        m = re.match(r"\s*-?\s*value:\s*(0x[0-9A-Fa-f]+)", line)
        if m:
            sub_value = int(m.group(1), 16)
            continue
        m = re.match(r"\s*name:\s*(.+?)\s*$", line)
        if m and category is not None:
            name = m.group(1).strip("\"'")
            if category_name is None:
                category_name = name
                # The category itself is the generic value: subtype zero.
                out[category << 6] = "Generic " + name
            elif sub_value is not None:
                out[(category << 6) | sub_value] = name
                sub_value = None
    return out


def parse_psm_yaml(text):
    out, name = {}, None
    for line in text.splitlines():
        m = re.match(r"\s*-?\s*name:\s*(.+?)\s*$", line)
        if m:
            name = m.group(1).strip("\"'")
            continue
        m = re.match(r"\s*psm:\s*(0x[0-9A-Fa-f]+)", line)
        if m and name:
            out[int(m.group(1), 16)] = name
            name = None
    return out


def constants(path):
    """[(constant-name, value)] for every defconstant in uuids.lisp."""
    found = []
    for line in open(path, encoding="utf-8"):
        m = re.match(r"\(defconstant\s+(\+[a-z0-9-]+\+)\s+#x([0-9A-Fa-f]+)\)", line)
        if m:
            found.append((m.group(1), int(m.group(2), 16)))
    return found


def words(s):
    """Comparable token set.

    Case, punctuation and noise words differ between a Lisp constant and a
    registry entry without either being wrong: `+service-object-transfer+' and
    `Object Transfer Service' are the same thing.
    """
    s = s.strip("+").replace("-", " ").replace("_", " ").lower()
    # "string" because the registry says Manufacturer Name String where this
    # library says manufacturer-name, and the extra word carries nothing.
    drop = {"service", "characteristic", "descriptor", "char", "appearance",
            "generic", "object", "type", "types", "string"}
    return set(w for w in re.split(r"[^a-z0-9]+", s) if w and w not in drop)


# Places where this library's name is deliberately not the registry's, and
# both are right.  Listed rather than pattern-matched: an alias is a decision
# about one constant, and a rule that made these pass would also hide a real
# mismatch somewhere else.
ALIASES = {
    "+service-generic-access+": "GAP",
    "+service-generic-attribute+": "GATT",
    "+descriptor-es-measurement+": "Environmental Sensing Measurement",
}


def family(name):
    # object-type first: it would not be matched by any of the others, but the
    # order makes that a decision rather than an accident.
    for prefix, fam in (("object-type", "object"), ("service", "service"),
                        ("char", "char"), ("descriptor", "descriptor"),
                        ("appearance", "appearance")):
        if name.startswith("+" + prefix + "-"):
            return fam
    return None


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else "src/uuids.lisp"
    try:
        tables = {k: parse_uuid_yaml(fetch(u)) for k, u in SOURCES.items()}
        tables["appearance"] = parse_appearance_yaml(
            fetch(f"{BASE}/core/appearance_values.yaml"))
        psms = parse_psm_yaml(fetch(f"{BASE}/core/psm.yaml"))
    except (urllib.error.URLError, OSError) as e:
        print(f"cannot reach the SIG registry ({e}); nothing checked")
        return 0

    print(f"registry: {len(tables['service'])} services, "
          f"{len(tables['char'])} characteristics, "
          f"{len(tables['descriptor'])} descriptors, "
          f"{len(tables['appearance'])} appearance categories, "
          f"{len(psms)} PSMs\n")

    problems = 0
    for name, value in constants(path):
        fam = family(name)
        if fam is None:
            continue
        # A characteristic constant may name an object type (OTS carries both
        # kinds), so fall back rather than reporting a false absence.
        official = tables[fam].get(value) or tables["object"].get(value)
        if official is None:
            print(f"  MISSING  {name} = 0x{value:04X} is in no SIG table")
            problems += 1
        elif ALIASES.get(name) == official:
            print(f"  ok       {name:<46} 0x{value:04X}  {official} (alias)")
        elif words(name) != words(official):
            print(f"  NAME     {name} = 0x{value:04X} "
                  f"but the SIG calls it {official!r}")
            problems += 1
        else:
            print(f"  ok       {name:<46} 0x{value:04X}  {official}")

    # The OTS example's PSM is not in uuids.lisp, but it is an assigned number
    # and a wrong one is just as invisible there.
    ots = [n for n, v in psms.items() if v.upper() == "OTS"]
    if ots:
        print(f"\n  ok       {'OTS LE PSM':<46} 0x{ots[0]:04X}")
        if ots[0] != 0x0025:
            print("  NOTE: examples/object-transfer/ hardcodes 0x0025")
            problems += 1

    print(f"\n{problems} problem(s)")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
