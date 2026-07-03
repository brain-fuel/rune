# Script Ergonomics Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make "read a float from the command line and double it" a 7-line Rune program that runs byte-identically on all 9 backends.

**Architecture:** Three new host ops (`parseFloat`/`getFloat`/`printFloat`) join the existing `ioPrims` family; the CLI grows mix-style compilation sets (multiple files/dirs, one session, topo-sorted); `import`/`alias` land as contextual keywords desugaring to name rewrites; the REPL prelude becomes a shared package loaded by the CLI by default, with a reachability pass (tree-shake) so unused prelude defs never reach emitted output. No kernel changes anywhere.

**Tech Stack:** Go (compiler), 9 backends (js/py/go/rs/erl/jvm/c/ll/wasm), existing harness conformance-test pattern.

**Spec:** `docs/superpowers/specs/2026-07-03-script-ergonomics-design.md`.
Sequencing deviation from spec: datatypes/foreigns-in-modules (Task 9) moved BEFORE the prelude task (Task 10), because `import Std.Float` requires `foreign` declarations inside a module block.

## Global Constraints

- Kernel frozen: no changes under `core/`, `equality/`, `store/`.
- Repo: `/home/brainfuel/matt/goforge.dev/rune`. Latest tag `v3.361.0`; check `git tag | sort -V | tail -1` before tagging (another agent may claim numbers).
- Multiple agents may share this repo: work on branch `feat/script-ergonomics` in a dedicated worktree; commit with explicit pathspecs.
- Every task ends with `go test ./harness/... ./surface/... ./internal/... ./cmd/... ./codegen/...` green for the touched packages; Task 12 runs the full `go test ./...`.
- No em-dashes or en-dashes in any prose you write (docs, comments, commit messages).
- Commit messages: conventional commits, normal prose.
- Canonical float formatting (used by `printFloat` on every backend): the
  ECMAScript `Number::toString(10)` algorithm. Precisely: take the SHORTEST
  round-trip decimal digits `d1..dn` and decimal exponent `k` (the value is
  `0.d1..dn * 10^k`), then:
  - NaN prints `NaN`; positive infinity prints `Infinity`; negative infinity
    prints `-Infinity`; negative zero prints `0`.
  - if `n <= k <= 21`: digits followed by `k-n` zeros (e.g. 1e7 prints
    `10000000`).
  - if `0 < k <= 21` and `k < n`: digits with a `.` after position `k`
    (e.g. `6.28`).
  - if `-6 < k <= 0`: `0.` then `-k` zeros then digits (e.g. 1e-6 prints
    `0.000001`).
  - otherwise exponent form: `d1` (`.d2..dn` if n > 1) then `e` then
    explicit sign then `k-1` in decimal, no zero padding (1e21 prints
    `1e+21`, 1e-7 prints `1e-7`, 1.5e-9 prints `1.5e-9`).
  - Negative values prefix `-`.
- `parseFloat` accepts exactly: optional `-` or `+`, then (`digits`,
  `digits.digits`, `digits.`, or `.digits`), then optional exponent
  (`e`/`E`, optional sign, digits). Nothing else (no `nan`, `inf`, spaces,
  underscores, hex). Reject means `none`.

---

### Task 1: Float IO host ops on the JS backend

**Files:**
- Modify: `codegen/ioprims.go` (declare 3 prims near the fadd/fsqrt block, ~line 55-71)
- Modify: `codegen/js.go` (implement, near fsqrt ~line 275 and printNat ~line 59)
- Create: `listings/ch566_float_io.rune`
- Test: `harness/io_float_stdin_test.go`

**Interfaces:**
- Produces prim contracts every later backend task mirrors exactly:
  - `parseFloat : Nat -> Option Float` (packed-string bytes in, `none` on reject; Option encoding: tag 0 `none` args `[null]`, tag 1 `some` args `[null, value]`, same as `jsonStrField`)
  - `getFloat : IO Float` (read one stdin line, parse, garbage yields `0.0`)
  - `printFloat : Float -> IO Float` (canonical format + `\n` to stdout, returns the argument, mirroring `printNat`)
- Produces the listing `ch566_float_io.rune` and harness test all later backend tasks extend.

- [ ] **Step 1: Declare the prims**

In `codegen/ioprims.go`, in the float block (after `fpow`):

```go
	"parseFloat": true, // parseFloat s : Nat -> Option Float (packed string -> float; none on reject)
	"getFloat":   true, // getFloat      : IO Float (read stdin line, parse; garbage -> 0.0)
	"printFloat": true, // printFloat x  : Float -> IO Float (canonical ECMAScript Number::toString + \n; returns x)
```

- [ ] **Step 2: Write the listing**

Create `listings/ch566_float_io.rune`. Self-contained (no prelude yet), modeled on ch211/ch213. It reads one float from stdin, doubles it, prints it, and also exercises parseFloat's none path on a garbage constant string:

```
-- Chapter 566: float IO at the boundary (parseFloat/getFloat/printFloat).
-- Feed "3.14" on stdin: prints 6.28 then 999 (the none-branch probe) then
-- the shown IO result. Canonical float formatting is the ECMAScript
-- Number::toString algorithm on every backend (the divergence lock).

data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ

data Option : U -> U is
  none : (A : U) -> Option A
| some : (A : U) -> A -> Option A
end

foreign Float : U end
foreign fromNat : Nat -> Float end
foreign fmul : Float -> Float -> Float end
foreign parseFloat : Nat -> Option Float end
foreign getFloat : IO Float end
foreign printFloat : Float -> IO Float end
foreign printNat : Nat -> IO Nat end

-- "abc" packed: bytes least-significant first with the 0x01 sentinel on top.
-- 'a'=97 'b'=98 'c'=99 -> 0x01636261 = 23290465
probe : IO Nat is
  case parseFloat 23290465 of
  | none A -> printNat 999
  | some A x -> printNat 111
  end
end

main : IO Nat is
  bindIO Float Nat getFloat
    (fn (x : Float) is
      bindIO Float Nat (printFloat (fmul x (fromNat 2)))
        (fn (_y : Float) is probe end)
    end)
end
```

Note: if elaboration rejects anything here (e.g. `case` on a foreign-typed
scrutinee is fine, but check `bindIO`'s availability as in ch211: it is a
kernel IO former, used exactly as ch211 uses it), fix the LISTING to match
the working idiom of `listings/ch211_io_stdin.rune` and
`listings/ch212_io_error.rune`; the prim contracts must not change.

- [ ] **Step 3: Write the failing harness test**

Create `harness/io_float_stdin_test.go`, copying the shape of
`TestIOFloatBlasConformance` (`harness/io_os_test.go:1707`), js backend only
for now. `runIOListing(t, bk, file, main, stdin)` already supports stdin:

```go
package harness

import (
	"os/exec"
	"testing"
)

// Task 1: js only. Later tasks append backends to floatIOBackends.
func floatIOBackends() []ioBackend {
	return []ioBackend{ioCLIBackends[0]} // js
}

func TestIOFloatStdinConformance(t *testing.T) {
	const want = "6.28\n999\n999"
	for _, bk := range floatIOBackends() {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch566_float_io.rune", "main", "3.14\n"); got != want {
				t.Errorf("[%s] float stdin run gave %q, want %q", bk.name, got, want)
			}
		})
	}
}
```

Before writing, open `harness/io_os_test.go` and confirm the element type and
order of `ioCLIBackends` (index 0 must be js; if not, select js by name) and
the exact show format of a returned IO Nat (see the ch211-based test for
whether the tail is `999` or `unit` or similar). Set `want` to match the
harness's real convention; the doubled value line MUST be exactly `6.28`.

- [ ] **Step 4: Run test, verify failure**

Run: `go test ./harness/ -run TestIOFloatStdinConformance -v`
Expected: FAIL (js emission does not know the three prims yet; error or empty output).

- [ ] **Step 5: Implement on js**

In `codegen/js.go`, next to the existing float prims (~line 275), add. The
formatter is just `String(x)` because the canonical algorithm IS ECMAScript's:

```javascript
const __fmtf = x => Object.is(x, -0) ? "0" : String(x);
const __parsef = s => /^[+-]?((\d+(\.\d*)?)|(\.\d+))([eE][+-]?\d+)?$/.test(s) ? Number(s) : null;
const parseFloat_ = () => s => { const v = __parsef(__h2s(s)); return v === null ? {tag:0,name:"none",args:[null]} : {tag:1,name:"some",args:[null,v]}; };
const getFloat = () => () => { const v = __parsef(__readLine()); return v === null ? 0.0 : v; };
const printFloat = () => x => () => { console.log(__fmtf(x)); return x; };
```

Adapt mechanically to the file's real conventions: how `getNat` reads a line
(reuse its line-reading helper instead of inventing `__readLine`), how
packed strings decode (`__h2s` per the `jsonStrField` implementation at
js.go:142), how IO thunks are shaped (mirror `printNat` at js.go:59), and
how the emitter gates prim bodies on `usesForeign`. The rune-side name is
`parseFloat`; if a bare `parseFloat` identifier collides with the js global,
follow the file's existing collision convention (the emitted binding name
must be whatever the emitter's name-mangling produces anyway).

- [ ] **Step 6: Run test, verify pass**

Run: `go test ./harness/ -run TestIOFloatStdinConformance -v`
Expected: PASS (js).

- [ ] **Step 7: Verify listing gate + commit**

Run: `go test ./harness/ -run TestListingsElaborateAndCheck/ch566 -v`
Expected: PASS.

```bash
git add codegen/ioprims.go codegen/js.go listings/ch566_float_io.rune harness/io_float_stdin_test.go
git commit -m "feat(io): parseFloat/getFloat/printFloat host ops, js backend" -- codegen/ioprims.go codegen/js.go listings/ch566_float_io.rune harness/io_float_stdin_test.go
```

---

### Task 2: Float IO on py and go backends

**Files:**
- Modify: `codegen/py.go` (near fsqrt ~line 240, printNat ~line 41)
- Modify: `codegen/golang.go` (near fsqrt ~line 337, printNat ~line 135, jsonStrField ~line 202)
- Modify: `harness/io_float_stdin_test.go` (extend backend list)

**Interfaces:**
- Consumes the Task 1 prim contracts and canonical-format algorithm (Global Constraints) verbatim.
- Produces `__fmtf`-equivalent helpers on py and go that later corpus tests (Task 4) rely on.

- [ ] **Step 1: Extend the test to py and go**

In `harness/io_float_stdin_test.go` change `floatIOBackends` to return js, py, go (select from `ioCLIBackends` the way Task 1 did for js).

- [ ] **Step 2: Run test, verify py/go fail**

Run: `go test ./harness/ -run TestIOFloatStdinConformance -v`
Expected: js PASS, py FAIL, go FAIL.

- [ ] **Step 3: Implement on py**

Python `repr` gives shortest round-trip digits but the WRONG dressing
(`repr(2.0)` is `2.0`, `repr(1e16)` is `1e+16`, exponent is zero-padded).
Implement the canonical formatter from shortest digits. In `codegen/py.go`
next to the float prims:

```python
def __fmtf(x):
    import math
    if math.isnan(x): return "NaN"
    if math.isinf(x): return "Infinity" if x > 0 else "-Infinity"
    if x == 0: return "0"
    s = repr(abs(x))
    if 'e' in s or 'E' in s:
        m, _, e = s.lower().partition('e')
        k = int(e) + 1
    else:
        m = s; k = 0
        if '.' in m:
            i, _, f = m.partition('.')
            if i == '0':
                f2 = f.lstrip('0'); k = -(len(f) - len(f2)); m = f2
            else:
                k = len(i); m = i + f
        else:
            k = len(m)
    d = m.replace('.', '').rstrip('0') or '0'
    n = len(d)
    if 'e' in s or 'E' in s:
        # normalize k for the mantissa form d1.d2..: k above already counts
        # digits before the point, recompute from mantissa shape
        pass
    sign = '-' if x < 0 else ''
    if n <= k <= 21: out = d + '0' * (k - n)
    elif 0 < k <= 21: out = d[:k] + '.' + d[k:]
    elif -6 < k <= 0: out = '0.' + '0' * (-k) + d
    else:
        out = d[0] + ('.' + d[1:] if n > 1 else '') + 'e' + ('+' if k - 1 >= 0 else '-') + str(abs(k - 1))
    return sign + out
```

The digit/exponent extraction above is subtle: WRITE UNIT-LEVEL PROBES FIRST.
Before wiring into the backend, run the formatter standalone
(`python3 -c ...`) against this table and fix until all match:

| input | expected |
|---|---|
| 0.0 | 0 |
| -0.0 | 0 |
| 2.0 | 2 |
| 6.28 | 6.28 |
| -0.5 | -0.5 |
| 1e7 | 10000000 |
| 1e20 | 100000000000000000000 |
| 1e21 | 1e+21 |
| 1e-6 | 0.000001 |
| 1e-7 | 1e-7 |
| 1.5e-9 | 1.5e-9 |
| 0.1 | 0.1 |
| float('inf') | Infinity |
| float('nan') | NaN |

Then implement the three prims mirroring `printNat`/`getNat`/`jsonStrField`
py conventions, with the same accept-regex as js
(`^[+-]?((\d+(\.\d*)?)|(\.\d+))([eE][+-]?\d+)?$`, via `re`), `getFloat`
garbage -> 0.0, and Option tagged-record encoding from `jsonStrField`.

- [ ] **Step 4: Implement on go**

Same structure in `codegen/golang.go`. Shortest digits come from
`strconv.FormatFloat(x, 'e', -1, 64)` (mantissa + exponent, always exponent
form, easiest to re-dress): parse `d.ddddde±XX`, set digits `d`, `k = exp+1`,
then apply the identical dressing rules as the py helper. Accept-regexp via
`regexp.MustCompile` (hoisted, not per-call). Option encoding is the
`map[string]any{"tag": ..., "name": ..., "args": ...}` shape from
`jsonStrField` (golang.go:202). Emitted Go must compile with no unused
imports (see the ioprims.go:97 comment for the existing convention).

- [ ] **Step 5: Run test, verify 3-way pass**

Run: `go test ./harness/ -run TestIOFloatStdinConformance -v`
Expected: PASS on js, py, go, byte-identical.

- [ ] **Step 6: Commit**

```bash
git add codegen/py.go codegen/golang.go harness/io_float_stdin_test.go
git commit -m "feat(io): float IO host ops on py and go backends" -- codegen/py.go codegen/golang.go harness/io_float_stdin_test.go
```

---

### Task 3: Float IO on rust, beam, jvm backends

**Files:**
- Modify: `codegen/rust.go` (near fsqrt ~line 137), `codegen/beam.go` (near fsqrt ~line 256), `codegen/jvm.go` (near fsqrt ~line 133)
- Modify: `harness/io_float_stdin_test.go` (extend list; note `TestIOFloatBlasConformance` appends rust as `ioOSBackends[3]`, follow that for whichever of the three are not in `ioCLIBackends`)

**Interfaces:**
- Consumes Task 1 contracts + Global Constraints formatter spec.

- [ ] **Step 1: Extend test to rust/beam/jvm; run; verify the three fail**

Run: `go test ./harness/ -run TestIOFloatStdinConformance -v`

- [ ] **Step 2: Implement on rust**

Rust `format!("{}", x)` never uses exponent notation, so extract shortest
digits from `format!("{:e}", x)` (gives `d.ddde±k` shortest? VERIFY: if
`{:e}` is not shortest round-trip on the toolchain, obtain shortest digits
by the precision-search loop: for p in 1..=17, `format!("{:.*e}", p, x)`,
accept the first that reparses equal). Then apply the canonical dressing
(same rules as py Task 2 table, same 14-row probe table must pass; probe
with a scratch `rustc` one-file program before wiring in). Values are
`Rc<V>` with `V::Float` (see fsqrt at rust.go:137); Option encoding mirrors
whatever `jsonStrField` does on rust (find it in rust.go; if rust lacks
jsonStrField, mirror the constructor-record convention the rust emitter
uses for `case`-visible datatypes: tag + args vector).

- [ ] **Step 3: Implement on beam**

Erlang: `float_to_binary(X, [short])` (OTP 25+) gives shortest digits in
Erlang's own dressing (`"6.28"`, `"1.0e21"`); re-dress to canonical with the
same rules (probe table via `erl -eval` or `escript` scratch file first).
Prims are `ff_`-prefixed funs (see `ff_fsqrt` at beam.go:256); `getFloat`
reads a line like the beam `getNat` does; remember `$A` is a char literal in
Erlang, not a variable (past gotcha).

- [ ] **Step 4: Implement on jvm**

Java `Double.toString` is shortest round-trip with wrong dressing
(`2.0`, `1.0E7`): parse its output into digits + exponent, re-dress
canonically (same probe table via `jshell` or a scratch Main first). Values
are `V`/`VFloat` (jvm.go:133); IO and stdin conventions mirror the jvm
`printNat`/`getNat`.

- [ ] **Step 5: Run test, verify 6-way pass; commit**

Run: `go test ./harness/ -run TestIOFloatStdinConformance -v`
Expected: PASS js/py/go/rs/erl/jvm byte-identical (skip = missing toolchain only).

```bash
git add codegen/rust.go codegen/beam.go codegen/jvm.go harness/io_float_stdin_test.go
git commit -m "feat(io): float IO host ops on rust, beam, jvm backends" -- codegen/rust.go codegen/beam.go codegen/jvm.go harness/io_float_stdin_test.go
```

---

### Task 4: Float IO on c, ll, wasm + 9-way format-corpus divergence lock

**Files:**
- Modify: `codegen/c.go` (near fsqrt ~line 1060, jsonStrField ~line 720), `codegen/ll.go` (near line 555; C and LL share the runtime, mirror c.go), `codegen/wasm.go` (near `emitFloatPrimsWasm` ~line 2279)
- Create: `listings/ch567_float_format.rune`
- Modify: `harness/io_float_stdin_test.go` (all 9 backends + corpus test)

**Interfaces:**
- Consumes Task 1 contracts.
- Produces `TestIOFloatFormatConformance`, the divergence-lock family member the spec requires.

- [ ] **Step 1: Write the corpus listing**

Create `listings/ch567_float_format.rune`: same header pattern as ch566
(Nat, Option, foreigns for Float/parseFloat/printFloat/printNat), plus
packed-string constants for each corpus input. Compute packed constants
with the bytes-LSB-first + 0x01-sentinel rule (e.g. "0" = 0x0130 = 304;
compute each in a scratch Go snippet, do not guess). The program printFloats,
in order, the parse of: "0", "2", "3.14", "-0.5", "1e7", "1e20", "1e21",
"1e-6", "1e-7", "1.5e-9", "0.1", then probes THREE parse rejects (printNat
999 on none, 111 on some): "..", "1e", "5 5".

AMENDMENT (2026-07-03, during Task 3): the original corpus produced
Infinity/-Infinity/NaN via fdiv. Erlang floats cannot represent IEEE
inf/NaN (arithmetic raises badarith), so those three rows are
unrepresentable on the beam backend and are replaced with parse-reject
rows. The formatter's NaN/Infinity branches remain on the backends whose
floats can hold them, covered by each language's standalone probe table,
not by this cross-backend corpus.

Expected stdout (before the harness's shown-result tail):

```
0
2
3.14
-0.5
10000000
100000000000000000000
1e+21
0.000001
1e-7
1.5e-9
0.1
999
999
999
```

- [ ] **Step 2: Add `TestIOFloatFormatConformance` over the 6 working backends; verify pass**

Same shape as `TestIOFloatStdinConformance`, no stdin, `want` = the block
above plus the harness result tail. Run:
`go test ./harness/ -run TestIOFloatFormatConformance -v`
Expected: PASS 6-way. Fix any backend whose dressing diverges NOW, before
the native tier (this is the whole point of the corpus).

- [ ] **Step 3: Implement on c (and ll via the shared runtime)**

C has no shortest-float primitive. Use the precision-search loop:

```c
static void __fmtf(double x, char *out /* >= 32 bytes */) {
  if (x != x) { strcpy(out, "NaN"); return; }
  if (x > 1.7976931348623157e308) { strcpy(out, "Infinity"); return; }
  if (x < -1.7976931348623157e308) { strcpy(out, "-Infinity"); return; }
  if (x == 0) { strcpy(out, "0"); return; }
  char buf[40]; int p;
  for (p = 1; p <= 17; p++) {
    snprintf(buf, sizeof buf, "%.*e", p - 1, x);
    if (strtod(buf, NULL) == x) break;
  }
  /* buf is now d.dddde[+-]XX with n=p shortest digits: re-dress canonically */
  ...
}
```

Fill the re-dress from the same rules as every other backend (digits d, n,
k = exponent+1; the four dressing branches from Global Constraints). Probe
table first as a standalone C file compiled with `cc -lm`. Mirror
`jsonStrField_c2` (c.go:720) for the Option construction (`mkcon`), and the
c `printNat`/`getNat` for IO and stdin. Then mirror the whole set into
`codegen/ll.go` the same way ll mirrors c for fsqrt (ll.go:555, shared
runtime; likely the identical C text).

- [ ] **Step 4: Implement on wasm**

Read `codegen/wasm.go` around `emitFloatPrimsWasm` (~line 2279) and the wasm
`printNat`/`getNat`/string machinery first. Implement the same three prims
in the file's WAT-emission style. The precision-search trick is unavailable
in raw WAT; two acceptable routes, pick whichever the file's architecture
makes cheaper: (a) implement the digit-extraction via the same
`%.*e`-equivalent using wasmtime WASI host calls if the backend already
shells text formatting out to the host, or (b) implement Grisu-lite:
extract digits by repeated multiply/divide on the f64 against powers of 10
with a 17-digit cap and round-trip check via reparse inside wasm. Gate with
the corpus test; byte-identical output is the acceptance, the internal
route is free.

- [ ] **Step 5: Extend both tests to all 9 backends; run; verify**

Run: `go test ./harness/ -run 'TestIOFloatStdinConformance|TestIOFloatFormatConformance' -v`
Expected: PASS 9-way byte-identical (toolchain-missing skips allowed).

- [ ] **Step 6: Listings gate + commit**

Run: `go test ./harness/ -run TestListingsElaborateAndCheck -v` (all pass)

```bash
git add codegen/c.go codegen/ll.go codegen/wasm.go listings/ch567_float_format.rune harness/io_float_stdin_test.go
git commit -m "feat(io): float IO on native c/ll/wasm; 9-way format divergence lock" -- codegen/c.go codegen/ll.go codegen/wasm.go listings/ch567_float_format.rune harness/io_float_stdin_test.go
```

---

### Task 5: REPL coverage for float IO

**Files:**
- Test: `internal/repl/repl_test.go` (extend)

**Interfaces:**
- Consumes: the `:run` command (repl.go:345, emits js + executes node) and Task 1 prims.

- [ ] **Step 1: Write the failing REPL test**

Standing rule: a feature is not done until it works in `rune repl`. Host ops
run in the REPL via `:run`. Find an existing `:run`-based test in
`internal/repl/repl_test.go` and copy its harness. New test: a session that
enters the ch566 definitions (minus the stdin read: use
`parseFloat <packed "3.14">` instead of `getFloat`, then
`printFloat (fmul x (fromNat 2))` in the some-branch), then
`:run main`, asserting `6.28` in the output. Skip when node is absent
(match the existing test's skip convention).

- [ ] **Step 2: Run, expect it to pass already (prims are codegen-side); if it fails, fix the REPL wiring until green**

Run: `go test ./internal/repl/ -run TestRepl -v` (narrow to the new test name)

- [ ] **Step 3: Commit**

```bash
git add internal/repl/repl_test.go
git commit -m "test(repl): float IO prims covered via :run" -- internal/repl/repl_test.go
```

---

### Task 6: Compilation sets (multi-file, dirs, topo-sort, cycle errors)

**Files:**
- Create: `internal/session/compset.go`
- Test: `internal/session/compset_test.go`
- Modify: `cmd/rune/main.go` (`parseEmitArgs` ~line 293, `programFor` ~line 360, usage text ~line 277)

**Interfaces:**
- Consumes: `surface.ParseProgram(src) ([]Item, error)`, `Session.LoadSource(src) ([]string, error)` (repeatable, latest-wins).
- Produces: `session.LoadSet(s *Session, sources []NamedSource) error` with `type NamedSource struct { Name, Src string }`; topo-sorted load; `parseEmitArgs` returns `files []string` (plural). Task 10 consumes both.

- [ ] **Step 1: Write failing tests**

`internal/session/compset_test.go`:

```go
package session

import (
	"strings"
	"testing"
)

const modMath = `
module Math is
  three : (n : Nat) -> Nat is fn (n : Nat) is succ (succ (succ zero)) end end
end
`

const usesMath = `
data Nat : U is zero : Nat | succ : Nat -> Nat end
answer : Nat is Math.three zero end
`

// Nat must load before Math (Math.three mentions Nat's constructors), and
// Math before answer. Feed the files in the WRONG order; LoadSet must sort.
func TestLoadSetTopoSorts(t *testing.T) {
	s := New()
	err := LoadSet(s, []NamedSource{
		{Name: "math.rune", Src: modMath},
		{Name: "main.rune", Src: usesMath},
	})
	if err != nil {
		t.Fatalf("LoadSet: %v", err)
	}
	if _, err := s.NormalizeExpr("answer"); err != nil {
		t.Fatalf("answer did not elaborate: %v", err)
	}
}

func TestLoadSetCycleError(t *testing.T) {
	a := `x : Nat is y end` // references y
	b := `data Nat : U is zero : Nat | succ : Nat -> Nat end
y : Nat is x end` // references x
	s := New()
	err := LoadSet(s, []NamedSource{{Name: "a.rune", Src: a}, {Name: "b.rune", Src: b}})
	if err == nil {
		t.Fatal("want cycle error, got nil")
	}
	if !strings.Contains(err.Error(), "a.rune") || !strings.Contains(err.Error(), "b.rune") {
		t.Errorf("cycle error should name both files, got: %v", err)
	}
}
```

Adjust `NormalizeExpr` to whatever public Session method evaluates a name
(find the one the REPL uses at repl.go:221); the assertion is only "answer
resolved". If `usesMath`'s split (Nat in the second file, used by the first)
makes the test awkward, restructure the fixtures, but keep the property:
files fed in an order that FAILS with sequential LoadSource must succeed
with LoadSet, and a true cycle must produce an error naming the cycle files.

- [ ] **Step 2: Run tests, verify failure (LoadSet undefined)**

Run: `go test ./internal/session/ -run TestLoadSet -v`

- [ ] **Step 3: Implement LoadSet**

`internal/session/compset.go`:

```go
package session

import (
	"fmt"
	"strings"

	"goforge.dev/rune/v3/surface"
)

type NamedSource struct{ Name, Src string }

// LoadSet parses every source, topologically sorts the files by
// defined-name -> referenced-name edges, and loads them in order.
// Module blocks already qualify their defs at parse time, so "defined
// names" fall out of the parsed items directly.
func LoadSet(s *Session, sources []NamedSource) error {
	type parsed struct {
		src   NamedSource
		items []surface.Item
		defs  map[string]bool
		refs  map[string]bool
	}
	ps := make([]*parsed, 0, len(sources))
	owner := map[string]int{} // defined name -> file index
	for i, ns := range sources {
		items, err := surface.ParseProgram(ns.Src)
		if err != nil {
			return fmt.Errorf("%s: %s", ns.Name, surface.RenderParseError(ns.Src, err))
		}
		p := &parsed{src: ns, items: items, defs: definedNames(items), refs: referencedNames(items)}
		for d := range p.defs {
			owner[d] = i
		}
		ps = append(ps, p)
	}
	// edges: file i depends on file j when i references a name j defines
	adj := make([][]int, len(ps))
	indeg := make([]int, len(ps))
	for i, p := range ps {
		seen := map[int]bool{}
		for r := range p.refs {
			if j, ok := owner[r]; ok && j != i && !seen[j] {
				seen[j] = true
				adj[j] = append(adj[j], i)
				indeg[i]++
			}
		}
	}
	var order []int
	queue := []int{}
	for i := range ps {
		if indeg[i] == 0 {
			queue = append(queue, i)
		}
	}
	for len(queue) > 0 {
		i := queue[0]
		queue = queue[1:]
		order = append(order, i)
		for _, j := range adj[i] {
			indeg[j]--
			if indeg[j] == 0 {
				queue = append(queue, j)
			}
		}
	}
	if len(order) != len(ps) {
		var cyc []string
		for i := range ps {
			if indeg[i] > 0 {
				cyc = append(cyc, ps[i].src.Name)
			}
		}
		return fmt.Errorf("import cycle between files: %s", strings.Join(cyc, " -> "))
	}
	for _, i := range order {
		if _, err := s.LoadSource(ps[i].src.Src); err != nil {
			return fmt.Errorf("%s: %w", ps[i].src.Name, err)
		}
	}
	return nil
}
```

`definedNames(items)`: walk the `[]surface.Item`; collect `Def.Name`,
`DataDef.Name` + every `Ctor` name, group members, and the builtin-decl
names. `referencedNames(items)`: walk every `Def.Ty`/`Def.Body` expression
tree collecting `EVar` names (and case-pattern constructor names). Write
both in compset.go; the surface AST types are in `surface/ast.go` (see the
Item/Def table in the exploration notes: Def, DataDef, DefGroup, DataGroup,
BuiltinNat, BuiltinNatOp, BuiltinNumInj). If the expression walk needs a
visitor the surface package lacks, add an exported `surface.WalkExp(e Exp,
fn func(Exp))` in a new `surface/walk.go` with its own small test rather
than reflecting from session.

Note: parsing each file twice (once here, once inside LoadSource) is
accepted for now; do not restructure LoadSource in this task.

- [ ] **Step 4: Run tests, verify pass**

Run: `go test ./internal/session/ ./surface/ -v -run 'TestLoadSet|TestWalkExp'`

- [ ] **Step 5: Wire the CLI**

In `cmd/rune/main.go`:
- `parseEmitArgs`: collect ALL positional args; the LAST positional is the
  main name IFF at least one earlier positional names an existing file or
  directory and the last does not (preserve today's `<file> <name>` and
  `<file>` forms exactly; add `<path...> <name>`). A directory path expands
  to its `*.rune` files sorted by name, non-recursive.
- `programFor(src, main)` gains a sibling `programForSet(sources []session.NamedSource, main string)` calling `session.LoadSet`; `emitFor`/`runTarget` route through it when more than one source is in play (single file keeps the exact old path).
- Usage text: `rune (emit|run) <path...> [name]` with one line noting dirs expand to their .rune files.

Add a CLI-level test in `cmd/rune` (find the existing test file for main;
if commands are tested via helper functions, test `parseEmitArgs`
directly): two files in a temp dir, `rune run <dir> main` equivalent path
returns the doubled output. If no test scaffolding for full runs exists,
test `parseEmitArgs` multi-path parsing + the dir expansion helper only.

- [ ] **Step 6: Run package tests + commit**

Run: `go test ./cmd/... ./internal/session/ ./surface/`

```bash
git add internal/session/compset.go internal/session/compset_test.go surface/walk.go surface/walk_test.go cmd/rune/main.go
git commit -m "feat(cli): compilation sets: multi-file/dir args, topo-sorted load, cycle errors" -- internal/session/compset.go internal/session/compset_test.go surface/walk.go surface/walk_test.go cmd/rune/main.go
```

(Adjust the pathspec if Step 5 touched a cmd test file; include it.)

---

### Task 7: `import` / `alias` parsing (contextual keywords)

**Files:**
- Modify: `surface/ast.go` (two new Item types), `surface/parser.go` (top-level loop ~line 107, next to the protocol/postulate contextual blocks)
- Test: `surface/parser_import_test.go`

**Interfaces:**
- Produces AST items Task 8 consumes:
  - `type Import struct { Module string; Pos int }`
  - `type Alias struct { Module string; As string; Pos int }` (`As` is the last dotted segment of Module when no `as` clause)
- Both satisfy the `Item` interface the same way `Def` does (find the marker method in ast.go and implement it).

- [ ] **Step 1: Write failing parser tests**

`surface/parser_import_test.go`:

```go
package surface

import "testing"

func TestParseImport(t *testing.T) {
	items, err := ParseProgram("import Std.Float\n")
	if err != nil {
		t.Fatal(err)
	}
	imp, ok := items[0].(Import)
	if !ok || imp.Module != "Std.Float" {
		t.Fatalf("got %#v", items[0])
	}
}

func TestParseAlias(t *testing.T) {
	items, err := ParseProgram("alias Math.Geometry\nalias Math.Geometry as G\n")
	if err != nil {
		t.Fatal(err)
	}
	a := items[0].(Alias)
	if a.Module != "Math.Geometry" || a.As != "Geometry" {
		t.Fatalf("bare alias: %#v", a)
	}
	b := items[1].(Alias)
	if b.Module != "Math.Geometry" || b.As != "G" {
		t.Fatalf("as-alias: %#v", b)
	}
}

// Contextual: `import` used as an ordinary definition name keeps parsing.
func TestImportStaysContextual(t *testing.T) {
	src := "import : Nat -> Nat is fn (n : Nat) is n end end\n"
	items, err := ParseProgram(src)
	if err != nil {
		t.Fatalf("contextual fallthrough broke: %v", err)
	}
	d, ok := items[0].(Def)
	if !ok || d.Name != "import" {
		t.Fatalf("got %#v", items[0])
	}
}
```

- [ ] **Step 2: Run, verify failure**

Run: `go test ./surface/ -run 'TestParseImport|TestParseAlias|TestImportStaysContextual' -v`

- [ ] **Step 3: Implement**

In `ParseProgram`'s top-level loop, copy the `protocol`/`postulate`
contextual pattern (parser.go ~line 118): trigger only on
`tIdent "import"` followed by `tIdent` (and NOT followed by `:` which
would make it a def; the def form is `name : ...`, so the second token
being tIdent already excludes it, but mirror whatever extra lookahead
protocol uses). Same for `alias`, with an optional `as <ident>` tail
(`as` is also contextual: only meaningful right after the alias module
ident). Dotted module names arrive as one tIdent token (lexer absorbs
dots). Populate `As` with the segment after the last `.` when the `as`
clause is absent.

- [ ] **Step 4: Run tests + full surface package; commit**

Run: `go test ./surface/ -v` (all green, listings unaffected)

```bash
git add surface/ast.go surface/parser.go surface/parser_import_test.go
git commit -m "feat(surface): contextual import/alias items" -- surface/ast.go surface/parser.go surface/parser_import_test.go
```

---

### Task 8: `import` / `alias` resolution in the session

**Files:**
- Modify: `internal/session/session.go` (`LoadSource` ~line 479)
- Create: `internal/session/imports.go`
- Test: `internal/session/imports_test.go`

**Interfaces:**
- Consumes: Task 7's `surface.Import`/`surface.Alias` items, Task 6's `surface.WalkExp` (add a transforming sibling `surface.MapExpNames(e Exp, ren func(string) string) Exp` if walking cannot rewrite in place).
- Produces: unqualified and aliased references resolve; ambiguity and unknown-module errors. Task 10's demo relies on `import Std.Float` working.

- [ ] **Step 1: Write failing tests**

`internal/session/imports_test.go`, table of three sessions built with
`LoadSource` (single-source is fine; imports are per-source scoped):

```go
package session

import (
	"strings"
	"testing"
)

const geo = `
data Nat : U is zero : Nat | succ : Nat -> Nat end
module Math.Geometry is
  three : Nat is succ (succ (succ zero)) end
end
`

func TestImportUnqualifies(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(geo); err != nil {
		t.Fatal(err)
	}
	src := "import Math.Geometry\nanswer : Nat is three end\n"
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("import did not resolve three -> Math.Geometry.three: %v", err)
	}
}

func TestAliasShortens(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(geo); err != nil {
		t.Fatal(err)
	}
	src := "alias Math.Geometry as G\nanswer : Nat is G.three end\n"
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("alias G did not resolve G.three: %v", err)
	}
}

func TestImportAmbiguityErrors(t *testing.T) {
	s := New()
	two := geo + `
module Other is
  three : Nat is zero end
end
`
	if _, err := s.LoadSource(two); err != nil {
		t.Fatal(err)
	}
	src := "import Math.Geometry\nimport Other\nanswer : Nat is three end\n"
	_, err := s.LoadSource(src)
	if err == nil {
		t.Fatal("want ambiguity error")
	}
	msg := err.Error()
	if !strings.Contains(msg, "Math.Geometry.three") || !strings.Contains(msg, "Other.three") {
		t.Errorf("ambiguity error should list both candidates, got: %v", msg)
	}
}

func TestImportUnknownModuleErrors(t *testing.T) {
	s := New()
	_, err := s.LoadSource("import No.Such.Module\n")
	if err == nil || !strings.Contains(err.Error(), "No.Such.Module") {
		t.Fatalf("want unknown-module error naming it, got: %v", err)
	}
}
```

- [ ] **Step 2: Run, verify failure**

Run: `go test ./internal/session/ -run 'TestImport|TestAlias' -v`

- [ ] **Step 3: Implement**

In `LoadSource`'s item loop: `surface.Import`/`surface.Alias` items update a
per-call scope (imports reset per LoadSource call; they are file directives,
not session state). Validate at the directive: an imported/aliased module
must have at least one definition in the session whose name starts with
`Module + "."` OR one defined later in this same source's parsed items
(collect this source's defined names up front, reusing `definedNames` from
compset.go); otherwise error naming the module.

Before each `AddDef`/`AddData` in this call, rewrite names throughout the
item's expressions (types, bodies, case patterns) in `imports.go`:

```go
// resolveName maps a surface reference through the active alias/import
// scope. Local (this-source) and session-global names win untouched;
// alias prefixes rewrite; otherwise a unique import match qualifies it.
func (sc *importScope) resolveName(n string) (string, error) {
	if sc.local[n] || sc.sess.has(n) {
		return n, nil
	}
	if i := strings.IndexByte(n, '.'); i > 0 {
		if full, ok := sc.aliases[n[:i]]; ok {
			return full + n[i:], nil
		}
	}
	var hits []string
	for _, m := range sc.imports {
		q := m + "." + n
		if sc.sess.has(q) || sc.local[q] {
			hits = append(hits, q)
		}
	}
	switch len(hits) {
	case 0:
		return n, nil // leave it; elaboration reports unknown names as today
	case 1:
		return hits[0], nil
	default:
		return "", fmt.Errorf("%q is ambiguous: could be %s", n, strings.Join(hits, " or "))
	}
}
```

`sc.sess.has(name)` checks `s.refs` (and the data/constructor table; find
where constructor names live when `AddData` runs and check both). Apply via
`surface.MapExpNames` over `Def.Ty` and `Def.Body`, plus the constructor
names inside case patterns (extend MapExpNames to visit pattern heads).
Route the returned error into the session's Diagnostic rendering the same
way LoadSource errors render today.

- [ ] **Step 4: Run tests; run full surface+session+harness; commit**

Run: `go test ./internal/session/ ./surface/ ./harness/ -run 'TestImport|TestAlias|TestListings'`

```bash
git add internal/session/session.go internal/session/imports.go internal/session/imports_test.go surface/walk.go
git commit -m "feat(session): import/alias name resolution with ambiguity diagnostics" -- internal/session/session.go internal/session/imports.go internal/session/imports_test.go surface/walk.go
```

---

### Task 9: `data` / `foreign` / `builtin` inside module blocks

**Files:**
- Modify: `surface/parser.go` (`parseModule` ~line 357), `codegen/ioprims.go` (`usesForeign` ~line 258: prim identity becomes the LAST dotted segment)
- Test: `surface/parser_module_test.go` (extend or create), `internal/session/imports_test.go` (extend)

**Interfaces:**
- Consumes: `parseItem` (parser.go:388) which already dispatches tForeign/tData/tBuiltin/tMutual.
- Produces: qualified datatypes and foreigns; `primName(name) = segment after last '.'` helper in ioprims.go that every backend's prim gate uses. Task 10's `module Std.Float is foreign getFloat ... end` relies on this.

- [ ] **Step 1: Write failing tests**

Parser test: a module containing a datatype and a foreign qualifies all names:

```go
func TestModuleQualifiesDataAndForeign(t *testing.T) {
	src := `module M is
data Flag : U is on : Flag | off : Flag end
foreign mystery : Flag end
lit : Flag is on end
end
`
	items, err := ParseProgram(src)
	if err != nil {
		t.Fatal(err)
	}
	// Expect: DataDef "M.Flag" with ctors "M.on"/"M.off"; Def "M.mystery"
	// (foreign); Def "M.lit". Inner references inside the module body
	// (`on` in lit) are NOT rewritten by the parser; resolution happens in
	// the session via an implicit self-import of M (next test).
	...assert names...
}
```

Session test (extends imports_test.go): loading that module then
`import M` + `probe : M.Flag is case M.mystery of | on -> on | off -> off end`
style use elaborates; also that within the module, `lit : Flag is on end`
resolved (module bodies get an implicit `import <their own module>` in the
session scope from Task 8's machinery).

ioprims test: `usesForeign` on a program whose foreign is named
`Std.Float.getFloat` still reports the `getFloat` prim as used (test file
next to ioprims.go if none exists; otherwise the harness demo in Task 10
covers it and this unit test is optional but preferred).

- [ ] **Step 2: Run, verify failures**

- [ ] **Step 3: Implement**

- `parseModule`: replace the `p.parseDef()` call with `p.parseItem()` and
  qualify each resulting item's names (Def.Name; DataDef.Name and every
  Ctor name; group members; BuiltinNat/NumInj type+function name fields
  reference names DEFINED elsewhere, qualify only when they refer to
  module-local names: simplest correct rule is to qualify the referenced
  name iff the module defines it, which requires a first pass over the
  module's items to collect local names). Record the module name on the
  items (add a `Module string` field to the Item types or return it
  alongside) so the session can install the implicit self-import.
- Session: when items carry a module name, add that module to the
  importScope for those items (self-import), so unqualified in-module
  references resolve module-first (a local name beats an import per Task
  8's precedence: adjust precedence so self-import beats other imports and
  ambiguity with the outer world is impossible inside your own module).
- `codegen/ioprims.go`: add `func primName(foreign string) string` (last
  dot segment) and use it in `usesForeign` and wherever backends look up
  prim identity by foreign name (grep each backend for the map/switch that
  turns an `IForeign` name into an emitted body; thread primName through).

- [ ] **Step 4: Run the full listings gate + touched packages; commit**

Run: `go test ./surface/ ./internal/session/ ./codegen/ ./harness/ -run 'TestModule|TestImport|TestListings|TestUsesForeign'`

```bash
git add surface/parser.go surface/ast.go surface/parser_module_test.go codegen/ioprims.go internal/session/imports.go internal/session/imports_test.go
git commit -m "feat(surface): data/foreign/builtin inside modules; prim identity = last segment" -- surface/parser.go surface/ast.go surface/parser_module_test.go codegen/ioprims.go internal/session/imports.go internal/session/imports_test.go
```

---

### Task 10: Shared prelude package, always-on in the CLI, Std.Float, the 7-line demo

**Files:**
- Create: `internal/prelude/prelude.go` (embed), move `internal/repl/prelude.rune` -> `internal/prelude/prelude.rune`
- Modify: `internal/repl/prelude.go` (load from the new package), `internal/prelude/prelude.rune` (append Option + Std.Float), `cmd/rune/main.go` (load prelude first; `--no-prelude` flag on emit/run)
- Create: `examples/double.rune`
- Test: `harness/io_float_stdin_test.go` (extend), `cmd/rune` test file from Task 6

**Interfaces:**
- Consumes: Tasks 6 (LoadSet), 8 (import), 9 (foreign-in-module).
- Produces: `prelude.Source() string`; the demo below compiles from `examples/double.rune`.

- [ ] **Step 1: Extract the prelude package**

`internal/prelude/prelude.go`:

```go
// Package prelude ships the standard prelude source. It is ordinary
// surface rune with no special status; the REPL and the CLI both load it
// through the same pipeline as any user file.
package prelude

import _ "embed"

//go:embed prelude.rune
var src string

func Source() string { return src }
```

`git mv internal/repl/prelude.rune internal/prelude/prelude.rune`; point
`internal/repl/prelude.go`'s loadPrelude at `prelude.Source()` and delete
its embed. Run: `go test ./internal/repl/` (green before proceeding).

- [ ] **Step 2: Append Option and Std.Float to the prelude**

At the end of `internal/prelude/prelude.rune` (verify first with grep that
the prelude does not already define `Option`, `Float`, or any of these
names; if Option exists, reuse it):

```
data Option : U -> U is
  none : (A : U) -> Option A
| some : (A : U) -> A -> Option A
end

module Std.Float is
  foreign Float : U end
  foreign fromNat : Nat -> Float end
  foreign fadd : Float -> Float -> Float end
  foreign fsub : Float -> Float -> Float end
  foreign fmul : Float -> Float -> Float end
  foreign fdiv : Float -> Float -> Float end
  foreign parseFloat : Nat -> Option Float end
  foreign getFloat : IO Float end
  foreign printFloat : Float -> IO Float end
end
```

Caution: the prelude's Nat is named `Whole` with a counting-Nat layered on
top (`Nat : U is ...` around prelude line 328); check which type `fromNat`'s
`Nat ->` should name in prelude context (the builtin-nat type is `Whole`).
Use the builtin-nat type so numerals work: `fromNat : Whole -> Float` if
`Whole` carries `builtin nat`. The demo then reads `fromNat 2` with 2
elaborating at Whole. Adjust the module accordingly and keep the demo
working; the LISTING ch566 (bare, own Nat) is unaffected.

Run: `go test ./internal/repl/` (prelude still loads).

- [ ] **Step 3: CLI always-on prelude**

In `cmd/rune/main.go`: emit/run parse a `--no-prelude` flag (same style as
`--target`). Unless set, the compilation set is
`[{Name: "prelude", Src: prelude.Source()}] + user sources`, loaded via
LoadSet with the prelude pinned FIRST (exempt the prelude from topo
ordering: load it before sorting the user files; latest-wins shadowing then
lets user files redefine prelude names). REPL behavior unchanged.

- [ ] **Step 4: The demo, failing test first**

Create `examples/double.rune`:

```
import Std.Float

main : IO Float is
  bindIO Float Float getFloat
    (fn (x : Float) is printFloat (fmul x (fromNat 2)) end)
end
```

Harness test (extend `harness/io_float_stdin_test.go`): run
`examples/double.rune` with the prelude through the same 9 backends,
stdin `3.14\n`, expect `6.28` + result tail, byte-identical. The harness
loads listings via bare sessions; this test instead builds the session the
CLI way (prelude first + LoadSet) then reuses the emit/execute half of
`runIOListing` (factor a helper if needed).

Run: `go test ./harness/ -run TestIOFloatDoubleDemo -v` -> verify FAIL
before Step 3's wiring is complete, then PASS after.

- [ ] **Step 5: Old-listing safety + commit**

Run: `go build ./cmd/rune && ./rune run listings/ch211_io_stdin.rune main --target js <<< "7"`
Expected: still `7\n7` (or its historical output): the listing redefines
Nat etc. and latest-wins shadowing must keep it working under the always-on
prelude. If shadowing trips on datatype collisions in practice, the
fallback is: CLI auto-disables the prelude when the user set defines
`builtin nat` itself, and this rule gets a test. Then full gate:

Run: `go test ./harness/ ./cmd/... ./internal/...`

```bash
git add internal/prelude/ internal/repl/prelude.go cmd/rune/main.go examples/double.rune harness/io_float_stdin_test.go
git commit -m "feat(cli): always-on shared prelude, Std.Float module, 7-line double demo" -- internal/prelude/ internal/repl/prelude.go cmd/rune/main.go examples/double.rune harness/io_float_stdin_test.go
```

---

### Task 11: Tree-shaking (reachability pass at emit)

**Files:**
- Create: `codegen/shake.go`
- Test: `codegen/shake_test.go`
- Modify: `internal/session/session.go` (`EmitProgram` ~line 1090 calls the pass when Main is set)

**Interfaces:**
- Consumes: `codegen.Program{Datas, Defs, Nat, Main, IOMain, Partials, PartialGroups}`, `DefSpec{Name, Body Ir, Arity}`, IR global refs are `IGlobal{Name string}` (ir.go:24).
- Produces: `codegen.Shake(p Program) Program` keeping only defs reachable from Main.

- [ ] **Step 1: Write failing test**

`codegen/shake_test.go`:

```go
package codegen

import "testing"

func TestShakeDropsUnreachable(t *testing.T) {
	p := Program{
		Main: "main",
		Defs: []DefSpec{
			{Name: "main", Body: IGlobal{Name: "helper"}},
			{Name: "helper", Body: IUnit{}},
			{Name: "unused", Body: IGlobal{Name: "alsoUnused"}},
			{Name: "alsoUnused", Body: IUnit{}},
		},
	}
	q := Shake(p)
	names := map[string]bool{}
	for _, d := range q.Defs {
		names[d.Name] = true
	}
	if !names["main"] || !names["helper"] {
		t.Fatalf("reachable defs dropped: %v", names)
	}
	if names["unused"] || names["alsoUnused"] {
		t.Fatalf("unreachable defs kept: %v", names)
	}
}

func TestShakeNoMainIsIdentity(t *testing.T) {
	p := Program{Defs: []DefSpec{{Name: "a", Body: IUnit{}}}}
	if got := Shake(p); len(got.Defs) != 1 {
		t.Fatal("no-Main shake must keep everything")
	}
}
```

Replace `IUnit{}` with the simplest real leaf Ir node (check ir.go; use
whatever a trivial body is). Add a walker case list by reading the full Ir
node set in ir.go: EVERY node type that can contain a nested Ir or a global
name must be visited; a missed case is a silent correctness bug, so write
the walker as an exhaustive type switch that panics on an unknown node
type (the test suite will catch a panic immediately, silence never).

- [ ] **Step 2: Run, verify failure. Implement**

`codegen/shake.go`: worklist from `p.Main` over `IGlobal` names; keep
reachable Defs (preserve original order); keep `Datas`, `Nat`, and prune
`Partials`/`PartialGroups` to surviving names. Do not prune `Datas` in this
task (constructor tags may be referenced positionally; out of scope, note
it in a comment).

- [ ] **Step 3: Wire into EmitProgram**

At the end of `EmitProgram` (after `p.Main` is set): `p = codegen.Shake(p)`.

- [ ] **Step 4: The acceptance test from the spec**

Extend `codegen/shake_test.go` or the Task 10 demo harness test: emit the
`examples/double.rune` + prelude program for js and assert the output text
does NOT contain a known prelude-only symbol that the demo never uses:
`lebEquiv` (prelude line 205) is a good probe. Assert it DOES contain the
mangled `printFloat` body marker.

- [ ] **Step 5: Full harness gate (conformance + listings + frontier) + commit**

Run: `go test ./codegen/ ./harness/ ./internal/...`
Frontier/seq-io tests exercise emitted programs end to end; shake must not
change any of their outputs.

```bash
git add codegen/shake.go codegen/shake_test.go internal/session/session.go
git commit -m "feat(codegen): tree-shake emitted programs to the Main-reachable set" -- codegen/shake.go codegen/shake_test.go internal/session/session.go
```

---

### Task 12: Diagnostics polish, full suite, docs, finish

**Files:**
- Modify: `surface/diagnostics.go` (only if Task 7/8 errors are not yet Diagnostic-grade), `README.md` (usage), `CLAUDE.md` (one-paragraph feature note where the other feature families are chronicled)
- Test: whatever gaps the sweep exposes

**Interfaces:**
- Consumes everything above.

- [ ] **Step 1: Diagnostic sweep**

Trigger each new error by hand with `./rune run` and check it renders with
the same quality bar as the existing 11 diagnostic families (carets, human
phrasing): unknown module on import, ambiguous name (must list both
candidates), import cycle (must name the file path), malformed
`import`/`alias` parse. Fix any that print raw fmt.Errorf text without the
Diagnostic dressing; add one test per fixed family next to the existing
diagnostics tests.

- [ ] **Step 2: Full test suite**

Run: `go test ./...`
Expected: green. Budget several minutes. Fix anything red before
proceeding; no green, no tag.

- [ ] **Step 3: Docs**

README: the compilation-set usage line, `import`/`alias` one-liner, the
7-line demo. CLAUDE.md: append the feature-family paragraph (follow the
house style of the D3/R-FFI entries). No em-dashes.

- [ ] **Step 4: Finish per the standing branch-finish default**

```bash
git tag | sort -V | tail -3   # confirm the next free minor
git checkout main && git merge --ff-only feat/script-ergonomics
git tag v3.36X.0              # next free minor
git push origin main v3.36X.0
```

(Only after Step 2's full green run. If ff-only fails because main moved,
rebase the branch, rerun `go test ./...`, then merge.)

---

## Self-Review Notes

- Spec coverage: compilation set (T6), import/alias (T7+T8), datatypes in
  modules (T9), prelude always-on (T10), tree-shake (T11), float IO 9-way +
  divergence lock (T1-T4), REPL rule (T5), Diagnostic-grade errors (T8+T12),
  demo acceptance (T10), unused-symbol assertion (T11 step 4). Spec's
  "harness --no-prelude for listings" turned out unnecessary (listings tests
  use bare sessions; CLI shadowing covered in T10 step 5 with a tested
  fallback rule).
- Sequencing deviation from spec (T9 before prelude) is stated up top.
- Types: `NamedSource{Name, Src}` (T6) used by T10; `Import{Module, Pos}` /
  `Alias{Module, As, Pos}` (T7) consumed in T8; `primName` (T9) consumed by
  T10's Std.Float foreigns; `Shake(Program) Program` (T11) wired in
  EmitProgram.
- Known intentional roughness: exact harness `want` strings (result-tail
  format) are verified against ch211 precedent at implementation time, and
  the py formatter sketch in T2 is a starting point that MUST pass the
  14-row probe table before wiring in.
