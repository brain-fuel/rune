# v4 Float Track B: 9-Backend Display Parity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A raw `Float` displays byte-identically on all 9 backends (js/py/go/rust/beam/jvm/c/ll/wasm), matching ECMAScript `Number::toString(10)`, for EVERY finite double (subnormals through ~1.8e308) plus -0/Infinity/-Infinity/NaN, with no documented residue.

**Architecture:** Codegen shadow only (no core, no hash bump). Two native fixes route `show()`'s float case through the existing ECMAScript `__fmtf` (moved to the base runtime); two WASM fixes replace the precision-losing `(f64)N` reconstruction with an exact Dragon4 formatter and the `|k|>22` atof with a correctly-rounded bignum path, both over the WASM runtime's existing base-1e9 limb bignums; a 9-way conformance gate proves parity over a broad edge corpus.

**Tech Stack:** Go (codegen emitters + harness tests), C (native runtime), LLVM-IR runtime.c, WAT (WASM text). Go `strconv` as the numeric oracle.

## Global Constraints

- KERNEL FROZEN: do NOT modify core/, store/, elaborate/. This is codegen + harness only (Rule 4: mutate the shadow).
- No new core constructor, NO hash-format bump (stays 0x06).
- No FloatLit: display-parity corpus values are built arithmetically (fromNat + fadd/fsub/fmul/fdiv), never from a float literal.
- NO em-dashes or en-dashes anywhere in added content (comments, chapter narration). Hyphens only. Dash-scan every changed file before commit.
- Conventional Commits; end EVERY commit message with:
  Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>
- Commit with explicit pathspecs only.
- ECMAScript `Number::toString` is the canonical rendering on ALL backends. The four dressing cases (k<=n<=21 integer; 0<n<k point; -5<=n<=0 leading zeros; else exponential) are SHARED and must not change; only WASM digit-generation and parse precision are corrected.
- A rune feature is not done until the full `go test -timeout 30m ./...` is green.

---

## File structure

- `codegen/c.go` -- move `__fmtf` from `emitFloatIOPrimsC` into the base C runtime (unconditional, dependency-free); route `show()` K_FLOAT through it.
- `codegen/ll_runtime.go` -- mirror: `__fmtf` in the base LL runtime; route `show()` K_FLOAT through it.
- `codegen/wasm_float.go` -- replace the `(f64)N` reconstruction in `$flt_fmt` with an exact Dragon4 digit generator over the runtime bignums; add a correctly-rounded bignum atof slow path.
- `codegen/wasm_float_display_test.go` (new) -- WASM differential unit tests vs a Go ECMAScript oracle (format) and Go `strconv.ParseFloat` (atof), over a wide random + edge corpus.
- `codegen/c_float_show_test.go` (new) -- native C show-of-Float emits ECMAScript, not %g.
- `codegen/ll_float_show_test.go` (new) -- native LL mirror.
- `harness/float_display_parity_test.go` (new) -- the 9-way display-parity gate.

Reference (read before starting): `docs/superpowers/specs/2026-07-10-v4-float-track-b-display.md`.

---

## Task 1: Native C -- show routes float through __fmtf

**Files:**
- Modify: `codegen/c.go` (base runtime string assembly ~line 120-145; `emitFloatIOPrimsC` ~1095-1128 remove the `__fmtf` definition; `show()` K_FLOAT case ~1972)
- Test: `codegen/c_float_show_test.go` (new)

**Interfaces:**
- Consumes: `usesForeign(p Program, name string) bool` (codegen/ioprims.go:385); `codegen.C{}` backend `Emit(Program) (string, error)`.
- Produces: a base-runtime C function `static void __fmtf(double x, char* out)` present in EVERY emitted C program; `show()`'s K_FLOAT case calling it. (Task 5 relies on native C float display matching ECMAScript.)

**Background:** `__fmtf` (ECMAScript shortest-round-trip, c.go:1111) is currently defined inside `emitFloatIOPrimsC` and only emitted under `usesFloatIO`. `show()` (c.go:1972) renders K_FLOAT with `printf("%g", ...)`, which uses 6 significant digits by default and diverges (e.g. `1234567.0` -> `"1.23457e+06"` vs ECMAScript `"1234567"`). `__fmtf` depends only on `<stdio.h>/<string.h>/<stdlib.h>` (already included), so it is safe to define unconditionally in the base runtime. A non-float program then carries an unused `__fmtf` (harmless; the C runtime already defines many always-present helpers; the build does not use -Werror).

- [ ] **Step 1: Write the failing test**

Create `codegen/c_float_show_test.go`. It emits a Float-valued `main` with NO float IO and runs the compiled C, asserting the ECMAScript string (which `%g` would get wrong):

```go
package codegen

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// A Float-valued main (no printFloat/getFloat/parseFloat) reaches the runtime
// show() K_FLOAT case. %g would render 1234567.0 as "1.23457e+06"; ECMAScript
// Number::toString renders "1234567". This pins native C show to ECMAScript.
func TestCShowFloatIsEcmascript(t *testing.T) {
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skip("cc not in PATH")
	}
	// main : Float = fromNat 1234567  (no float IO)
	src := `data Nat is zero | succ (n : Nat) end
foreign Float : U end
foreign fromNat : Nat -> Float end
n7 : Nat is succ zero end
` // built up below to 1234567 without a literal is impractical; use the helper.
	_ = src
	prog := floatShowProgram(t, 1234567) // helper builds `main : Float = fromNat <1234567>`
	out := emitAndRunC(t, prog)
	if strings.TrimSpace(out) != "1234567" {
		t.Fatalf("C show of 1234567.0 = %q, want \"1234567\" (got %%g divergence?)", out)
	}
}
```

Because building 1234567 as a `succ`-chain is impractical, use the builtin-nat literal path the corpus already supports. Implement the two helpers in the test file using the existing session+backend pipeline (mirror `internal/session/float_guarded_test.go`'s emit+run helper, which builds a Program from source and runs a chosen backend). Concretely:

```go
// floatShowProgram builds a checked Program whose `main : Float` is `fromNat k`,
// using the builtin nat literal so k need not be a succ-chain. Mirror the
// session-to-Program construction in internal/session/float_guarded_test.go.
func floatShowProgram(t *testing.T, k int) Program { /* build via session.New(); LoadSource prelude; AddDef main = fromNat <k>; EmitProgram */ }

// emitAndRunC emits C, writes main.c, compiles with cc, runs, returns stdout.
func emitAndRunC(t *testing.T, p Program) string {
	dir := t.TempDir()
	src, err := (C{}).Emit(p)
	if err != nil { t.Fatalf("emit: %v", err) }
	cpath := filepath.Join(dir, "main.c")
	if err := os.WriteFile(cpath, []byte(src), 0o644); err != nil { t.Fatal(err) }
	bin := filepath.Join(dir, "a.out")
	if out, err := exec.Command("cc", "-o", bin, cpath).CombinedOutput(); err != nil {
		t.Fatalf("cc: %v\n%s", err, out)
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil { t.Fatalf("run: %v\n%s", err, out) }
	return string(out)
}
```

Look at `internal/session/float_guarded_test.go` for the exact `session.New()` -> `LoadSource(prelude.Source())` -> add a `main` def -> `EmitProgram` sequence and reuse it verbatim for `floatShowProgram`. If `fromNat` needs `Std.Float.fromNat`, qualify accordingly. The `main` value should be `Std.Float.fromNat` applied to the nat literal `1234567`.

- [ ] **Step 2: Run test to verify it fails**

Run: `cd <worktree> && go test ./codegen/ -run TestCShowFloatIsEcmascript -v`
Expected: FAIL -- output is `1.23457e+06` (the `%g` rendering), not `1234567`.

- [ ] **Step 3: Move __fmtf to the base runtime and route show through it**

In `codegen/c.go`:
1. Cut the `__fmtf` definition string (the `b.WriteString("static void __fmtf(double x, char* out) { ... }\n")` at ~1111) OUT of `emitFloatIOPrimsC`.
2. Add it to the base runtime assembly (near the other always-emitted `static` helpers, e.g. just before `show_paren`/`show` are defined, so it is in scope for `show`). Keep the exact same body string and the explanatory comment. It must be emitted UNCONDITIONALLY (no `usesFloatIO` guard).
3. In `show()` (c.go ~1972) replace:
   `case K_FLOAT: printf("%g", o->dval); return;`
   with:
   `case K_FLOAT: { char __fb[64]; __fmtf(o->dval, __fb); printf("%s", __fb); return; }`
   (No trailing newline; show does not add one.)
4. Confirm `printFloat_c2` still calls `__fmtf` (it does, unchanged) and now references the base-runtime definition -- so there is exactly ONE `__fmtf` in the emitted program.

- [ ] **Step 4: Run the test to verify it passes**

Run: `go test ./codegen/ -run TestCShowFloatIsEcmascript -v`
Expected: PASS (`1234567`).

Also run the float IO conformance to prove printFloat still works (single definition, no duplicate-symbol error):
Run: `go test ./harness/ -run 'TestIOFloat' -count=1`
Expected: PASS.

- [ ] **Step 5: Dash-scan and commit**

```bash
cd <worktree>
git diff -- codegen/c.go codegen/c_float_show_test.go | grep -nP '[\x{2013}\x{2014}]' && echo "DASH FOUND -- fix" || echo "dash-clean"
git add codegen/c.go codegen/c_float_show_test.go
git commit -m "fix(codegen/c): render shown Float via __fmtf not %g (ECMAScript parity)

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 2: Native LL -- show routes float through __fmtf

**Files:**
- Modify: `codegen/ll_runtime.go` (`show()` K_FLOAT case ~501; `__fmtf` location -- currently in `emitFloatIOPrimsLL` per the comment at ll_runtime.go:620)
- Test: `codegen/ll_float_show_test.go` (new)

**Interfaces:**
- Consumes: `codegen.LL{}` backend; its `EmitRuntimeFor` / base `EmitRuntime` split (base omits float/BLAS to avoid -lopenblas; `__fmtf` has no such dep and moves to base).
- Produces: `__fmtf` in the base LL runtime.c; `show()` K_FLOAT routed through it.

**Background:** identical divergence to Task 1 on the LLVM backend. The LL runtime.c base (`EmitRuntime`) deliberately omits OpenBLAS/CPython-dependent float bodies, but `__fmtf` depends only on libc, so it belongs in the base like `show`/`big_print`.

- [ ] **Step 1: Write the failing test**

Create `codegen/ll_float_show_test.go`, mirroring Task 1 but for the LL backend. Reuse the same `floatShowProgram` helper (it is in the `codegen` package from Task 1). Emit via `LL{}.Emit`, compile the emitted `.ll` + its runtime the same way the existing LL tests do (grep `codegen/ll_*_test.go` for the clang/llc + runtime.c compile-and-run helper -- e.g. `TestLLConformsToC` or the LL ARC tests -- and reuse that harness). Assert `1234567`.

```go
func TestLLShowFloatIsEcmascript(t *testing.T) {
	// skip if clang/llc absent (match the existing LL test skip guard)
	prog := floatShowProgram(t, 1234567)
	out := emitAndRunLL(t, prog) // reuse the existing LL emit+compile+run helper
	if strings.TrimSpace(out) != "1234567" {
		t.Fatalf("LL show of 1234567.0 = %q, want \"1234567\"", out)
	}
}
```

Before writing, grep for the LL compile-run helper and reuse it rather than re-deriving the clang invocation:
`grep -rn 'func.*LL.*Run\|llc\|clang\|runtime.c' codegen/ll_*_test.go`

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./codegen/ -run TestLLShowFloatIsEcmascript -v`
Expected: FAIL -- `1.23457e+06`.

- [ ] **Step 3: Move __fmtf to base LL runtime and route show**

In `codegen/ll_runtime.go`:
1. Locate the `__fmtf` definition (emitted by `emitFloatIOPrimsLL`; the runtime.c string). Move that exact `static void __fmtf(...)` C body into the BASE runtime.c string (`EmitRuntime`), positioned before `show` is defined.
2. Remove it from `emitFloatIOPrimsLL` (printFloat there now calls the base definition).
3. In `show()` (ll_runtime.go ~501) replace `case K_FLOAT: printf("%g", o->dval); return;` with `case K_FLOAT: { char __fb[64]; __fmtf(o->dval, __fb); printf("%s", __fb); return; }`.
4. Ensure the base runtime.c still compiles WITHOUT -lopenblas for a non-float program (\_\_fmtf has no BLAS dep).

- [ ] **Step 4: Run tests to verify pass**

Run: `go test ./codegen/ -run 'TestLLShowFloatIsEcmascript|TestLLConformsToC' -count=1 -v`
Expected: PASS. Then `go test ./harness/ -run TestD3OpenBLAS -count=1` (LL float path still links) -- PASS or Skip (if no clang/openblas).

- [ ] **Step 5: Dash-scan and commit**

```bash
git diff -- codegen/ll_runtime.go codegen/ll_float_show_test.go | grep -nP '[\x{2013}\x{2014}]' && echo "DASH" || echo "clean"
git add codegen/ll_runtime.go codegen/ll_float_show_test.go
git commit -m "fix(codegen/ll): render shown Float via __fmtf not %g (ECMAScript parity)

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 3: WASM -- correctly-rounded atof (remove the |k|>22 residue)

**Files:**
- Modify: `codegen/wasm_float.go` (the atof function; see wasm_float.go:32-36 comment for the fast-path contract)
- Test: `codegen/wasm_float_display_test.go` (new; the atof half)

**Interfaces:**
- Consumes: the WASM runtime bignum ops in `codegen/wasm_runtime.go`: `$big_alloc`, `$big_from_u32`/limb setters, `$big_add`, `$big_sub`, `$big_mul`, `$big_cmp`, `$big_divmod`, `$big_divmod_small`, `$big_norm`, `$big_nlimbs`, `$big_limb`. Confirm exact names by grepping `codegen/wasm_runtime.go`.
- Produces: an atof (`$flt_atof` or the existing name -- grep wasm_float.go for the current parse function) that is correctly rounded for ALL decimal inputs, used by parseFloat/getFloat.

**Background:** the current WASM atof is correctly rounded only for D < 2^53 and |k| <= 22 (an exact i64 significand times an exact power-of-ten). Outside that, `(f64)D * 10^k` double-rounds. This task adds a bignum slow path. Display parity does not strictly require it, but the user opted to make WASM float fully residue-free; the parse fix also de-risks any future round-trip use.

Algorithm (correctly-rounded decimal D * 10^k -> f64, nearest-even):
- Keep the existing i64 fast path when `ndigits(D) <= 15 && -22 <= k <= 22`.
- Else: let the exact value be `D * 10^k`. Build numerator NUM and denominator DEN as bignums so that value = NUM/DEN with DEN a power of ten (if k<0, DEN = 10^-k, NUM = D; if k>=0, NUM = D * 10^k, DEN = 1).
- Find the binary exponent: estimate `e2` from `floor(log2(value))` (via the bit length of NUM/DEN), form the candidate mantissa `m = round(value * 2^(52-e2))` by the bignum division `NUM * 2^(52-e2) / DEN` (or `NUM / (DEN * 2^(e2-52))`), taking the quotient and using the remainder for the round-to-nearest-even decision (compare `2*rem` to `DEN'` via `$big_cmp`; tie -> even). Assemble the f64 from `(m, e2)` with `f64.reinterpret_i64` (handle the implicit bit, subnormals when `e2` below the min exponent, and overflow to +inf).
- Overflow (value >= 2^1024) -> inf; underflow (value < 2^-1075) -> 0. Sign handled by the caller.

This is a standard bignum decimal parser; correctness is proven by the differential test below, not by inspection.

- [ ] **Step 1: Write the failing differential test**

Create `codegen/wasm_float_display_test.go` with the atof half. It emits a tiny WASM program that reads a decimal string on stdin (via getFloat/parseFloat) and prints the resulting f64 BITS (as a decimal u64 via a helper, so the test compares exact doubles, not re-formatted strings), for a corpus of hard inputs, and compares against Go `strconv.ParseFloat(s, 64)` reinterpreted to bits.

Concretely, the cleanest exact channel: build `main` = `parseFloat <input>` and print `floatToNat`-of-the-bits is not available; instead print the value with the (to-be-fixed) formatter would be circular. So compare BITS: add a test-only path that emits a program computing `fleqN x y`-style equality against a Go-provided expected double is also awkward. Simplest robust channel: the test emits, for each input string s, a program that parses s to a Float and prints it with the EXISTING (Task 4-fixed) formatter -- but Task 4 is separate. To keep Task 3 independently testable, use this channel: emit a program `parseFloat s` then compare to a second parse of Go's canonical shortest string for the SAME double; if atof is correctly rounded, `parseFloat s` and `parseFloat (canonical s)` yield the same double, checkable by `feq`. That still needs a value channel.

Use the direct bits channel instead. Add a test-only foreign is overkill. Decision: drive the atof test THROUGH the round-trip: for each hard input string `s`, compute `want = strconv.ParseFloat(s)`; emit a program `main : Bool = feq (parseFloat-or-0 s) (<arith that builds want>)`. Building `want` arithmetically is the same corpus-construction problem as Task 5. Therefore FOLD the atof correctness check into the display gate (Task 5) via inputs whose parsed value is then displayed, and make THIS task's unit test a Go-level test of the algorithm ported to Go:

Write, in `wasm_float_display_test.go`, a Go reference implementation `atofBig(D *big.Int, k int) float64` that is the SAME bignum algorithm the WAT will implement, and assert it matches `strconv.ParseFloat` over a wide corpus. This pins the ALGORITHM. The WAT implementation is then verified end-to-end by Task 5 (inputs displayed) plus the existing float IO conformance.

```go
func TestAtofBigMatchesStrconv(t *testing.T) {
	cases := []string{
		"1e309", "1e-320", "1e-323", "4.9e-324", "1.7976931348623157e308",
		"2.2250738585072014e-308", "1.1e22", "1.1e23", "9007199254740993",
		"123456789012345678e-5", "0.30000000000000004", "1e1000", "1e-1000",
	}
	for _, s := range cases {
		want, _ := strconv.ParseFloat(s, 64)
		got := atofRef(s) // Go port of the WAT bignum algorithm
		if math.Float64bits(got) != math.Float64bits(want) {
			t.Fatalf("atofRef(%s) bits=%x want %x", s, math.Float64bits(got), math.Float64bits(want))
		}
	}
	// plus a large random sweep
	r := newRand(t) // deterministic seed
	for i := 0; i < 20000; i++ {
		s := randDecimalString(r)
		want, err := strconv.ParseFloat(s, 64)
		if err != nil { continue }
		if math.Float64bits(atofRef(s)) != math.Float64bits(want) {
			t.Fatalf("atofRef(%s) mismatch", s)
		}
	}
}
```

`atofRef` is a Go implementation using `math/big` mirroring the WAT algorithm exactly (same estimate, same round-to-even via remainder comparison). This is the algorithm's executable spec.

- [ ] **Step 2: Run to verify it fails**

Run: `go test ./codegen/ -run TestAtofBigMatchesStrconv -v`
Expected: FAIL until `atofRef` is written correctly.

- [ ] **Step 3: Implement atofRef (Go) then port to WAT**

First get `atofRef` correct (pure Go, `math/big`). THEN port the identical algorithm into `codegen/wasm_float.go`'s atof as the slow-path branch, using the runtime `$big_*` ops. Keep the i64 fast path for the common case.

- [ ] **Step 4: Verify**

Run: `go test ./codegen/ -run TestAtofBigMatchesStrconv -v` -> PASS.
Run the WASM float IO conformance to prove the WAT port did not regress the fast path:
`go test ./harness/ -run 'TestIOFloat|getFloat' -count=1` (wasmtime-gated) -> PASS.

- [ ] **Step 5: Dash-scan and commit**

```bash
git diff -- codegen/wasm_float.go codegen/wasm_float_display_test.go | grep -nP '[\x{2013}\x{2014}]' && echo DASH || echo clean
git add codegen/wasm_float.go codegen/wasm_float_display_test.go
git commit -m "fix(codegen/wasm): correctly-rounded bignum atof for extreme exponents

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 4: WASM -- exact Dragon4 shortest formatter (remove the p>=16 residue)

**Files:**
- Modify: `codegen/wasm_float.go` (`$flt_fmt` -- the shortest-round-trip formatter; the digit-generation section documented at wasm_float.go:22-30)
- Test: `codegen/wasm_float_display_test.go` (extend with the format half)

**Interfaces:**
- Consumes: the same `$big_*` runtime ops as Task 3; the existing four ECMAScript dressing cases in `$flt_fmt` (reuse them verbatim -- only digit generation changes); the IEEE bit-decomposition of a double.
- Produces: `$flt_fmt` emitting the exact shortest correctly-rounded ECMAScript digits + decimal-point position for EVERY finite double incl subnormals and 16/17-sig-digit values.

**Background:** the current formatter scales by `10^(E-p+1)` and rounds to an f64 integer significand `N`, then does `(f64)N * 10^k` to round-trip-test. For p >= 16, `(f64)N` loses precision (wasm_float.go:29-30). Replace the digit generation with Dragon4 (Steele and White FPP2 free-format), which is exact via bignums.

Dragon4 (free-format, shortest, correctly rounded):
- Decompose finite nonzero |x| = f * 2^e: read `bits = i64.reinterpret_f64(x)`; `mant = bits & (2^52-1)`, `expo = (bits >> 52) & 0x7ff`. If `expo == 0` (subnormal): `f = mant`, `e = -1074`. Else: `f = mant | 2^52`, `e = expo - 1075`. (Sign stripped earlier; specials handled before.)
- Set up bignums R, S, M+ , M- (the Steele-White scaled-value/denominator/high-margin/low-margin):
  - if e >= 0: `be = 2^e`; if `f != 2^52` (not a power-of-two boundary): `R = f*be*2`, `S = 2`, `Mp = be`, `Mm = be`. else (unequal gaps at the boundary): `R = f*be*4`, `S = 4`, `Mp = be*2`, `Mm = be`.
  - if e < 0: if `expo <= 1 (subnormal or min normal)` or `f != 2^52`: `R = f*2`, `S = 2^(1-e)`, `Mp = 1`, `Mm = 1`. else: `R = f*4`, `S = 2^(2-e)`, `Mp = 2`, `Mm = 1`.
    (These are the standard Dragon4 initial conditions; `2^n` built by shifting a bignum or repeated `$big_mul` by 2, `f*be` by `$big_mul`.)
- Scale into range: find k such that the first digit is in [1,10): estimate `k = ceil(log10(x))`, then multiply S by 10^k (k>0) or R,Mp,Mm by 10^-k (k<0) using `$big_mul` by 10^9 limbs / small powers, and adjust by one if `R + Mp > S` fails / `R + Mp <= S`. (Standard fixup loop.)
- Generate digits: loop -- `d = (R*10) / S` (bignum `$big_mul` by 10 then `$big_divmod` by S), `R = (R*10) mod S`, `Mp *= 10`, `Mm *= 10`; low = `R < Mm`, high = `R + Mp > S` (bignum compares); if low or high, terminate: emit `d` (or `d+1` if high and (not low or `2*R >= S`)), apply round-to-even at exact ties. Record digit count and the decimal exponent so the dressing cases can place the point.
- Feed the digit string + exponent into the EXISTING four ECMAScript dressing cases unchanged.
- Zero, -0, Inf, NaN: keep the existing direct special strings (do not enter Dragon4).

- [ ] **Step 1: Write the failing differential test (format half)**

Extend `codegen/wasm_float_display_test.go` with a Go ECMAScript-render oracle and a Go port `dragon4Ref(x float64) string` (same algorithm as the WAT), asserting both match over a wide corpus. The oracle derives the ECMAScript string from Go's shortest digits:

```go
// ecmaFloat renders x as ECMAScript Number::toString(10) would, using Go's
// shortest-decimal DIGITS + the four ECMAScript dressing cases. This is the
// cross-backend canonical string every backend must produce.
func ecmaFloat(x float64) string {
	if x != x { return "NaN" }
	if math.IsInf(x, 1) { return "Infinity" }
	if math.IsInf(x, -1) { return "-Infinity" }
	if x == 0 { return "0" } // ECMAScript: -0 renders "0"
	neg := math.Signbit(x)
	if neg { x = -x }
	// shortest significant digits + exponent via strconv 'e', -1
	b := strconv.AppendFloat(nil, x, 'e', -1, 64) // e.g. "1.234e+06"
	s, n := parseEcmaDigits(b) // s = digit string (no point), n = position: value = s * 10^(n-len(s))
	out := dressEcma(s, n)     // the four cases: k<=n<=21 | 0<n<k | -5<=n<=0 | else exp
	if neg { out = "-" + out }
	return out
}

func TestDragon4MatchesEcma(t *testing.T) {
	edge := []float64{
		1234567, 100, 1e21, 1e-7, 5e-324, 2.2250738585072014e-308,
		9007199254740992, 9007199254740994, 0.1, 1.0/3.0, 2.0/3.0,
		1.7976931348623157e308, 123456789012345.6, 0.30000000000000004,
	}
	for _, x := range edge {
		if got := dragon4Ref(x); got != ecmaFloat(x) {
			t.Fatalf("dragon4Ref(%v)=%q want %q", x, got, ecmaFloat(x))
		}
	}
	r := newRand(t)
	for i := 0; i < 50000; i++ {
		x := randFloat64(r) // full range incl subnormals, via Float64frombits of random u64, skip NaN/Inf
		if got := dragon4Ref(x); got != ecmaFloat(x) {
			t.Fatalf("dragon4Ref(%x)=%q want %q", math.Float64bits(x), got, ecmaFloat(x))
		}
	}
}
```

Implement `parseEcmaDigits`, `dressEcma`, `ecmaFloat`, and `dragon4Ref` (the Go port of the WAT Dragon4) in the test file. `dragon4Ref` must NOT delegate to strconv -- it is the executable spec of the WAT; `ecmaFloat` (which uses strconv for digits) is the independent oracle. They agreeing over 50k randoms is the correctness proof.

- [ ] **Step 2: Run to verify it fails**

Run: `go test ./codegen/ -run 'TestDragon4MatchesEcma' -v`
Expected: FAIL until `dragon4Ref` is correct.

- [ ] **Step 3: Get dragon4Ref correct, then port to WAT**

Iterate `dragon4Ref` (Go, `math/big`) until the 50k sweep is green. THEN port the identical loop into `$flt_fmt` in `codegen/wasm_float.go`, replacing the `(f64)N` reconstruction, reusing the four dressing cases. Keep the WAT memory-window discipline (the format buffers documented at wasm_float.go:38-43); the bignums allocate on the heap via `$big_alloc` and are transient.

- [ ] **Step 4: Verify Go + WASM**

Run: `go test ./codegen/ -run 'TestDragon4MatchesEcma|TestAtofBigMatchesStrconv' -v` -> PASS.
Run the WASM float IO + printFloat conformance (wasmtime-gated):
`go test ./harness/ -run 'TestIOFloat|printFloat' -count=1` -> PASS (proves the WAT port renders the existing corpus unchanged).

- [ ] **Step 5: Dash-scan and commit**

```bash
git diff -- codegen/wasm_float.go codegen/wasm_float_display_test.go | grep -nP '[\x{2013}\x{2014}]' && echo DASH || echo clean
git add codegen/wasm_float.go codegen/wasm_float_display_test.go
git commit -m "fix(codegen/wasm): exact Dragon4 shortest float formatter (all magnitudes)

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 5: The 9-way display-parity conformance gate

**Files:**
- Create: `harness/float_display_parity_test.go`

**Interfaces:**
- Consumes: `bibleBackends()` (the 9-backend list, harness/bible_conformance_test.go:29-89); the emit-and-run helper the other conformance gates use (grep `harness/comparison_conformance_test.go` / `harness/numeric_parity_test.go` for the per-backend run helper -- reuse it); `prelude.Source()`.
- Produces: `TestFloatDisplayParity` asserting byte-identical float display 9-way.

**Background:** Track A's `numeric_parity_test.go` deliberately avoided PRINTING floats (verified via `feq`) because display was not unified. This gate is the opposite: it PRINTS floats two ways and asserts all 9 backends agree, with the expected string from the Go `ecmaFloat` oracle (copy it into the harness package, or export a shared helper). A `main : Float` program reaches the runtime `show()` path (the one that used `%g`); a `printFloat` program reaches the IO path. Both must be exercised.

Corpus: each value built arithmetically (no FloatLit). Use `Std.Float.fromNat`, `fadd`, `fsub`, `fmul`, `fdiv`. Each witness targets a named formatter branch; document the branch in a comment. Build values like:
- integer/trailing-zero: `fromNat 1234567` (6-sig %g divergence branch), `fromNat 100`, `fromNat 1000000`
- point case: `fdiv (fromNat 1) (fromNat 2)` = 0.5, `fdiv (fromNat 1) (fromNat 8)` = 0.125
- 17-sig-digit: `fdiv (fromNat 1) (fromNat 3)` (0.333...), `fdiv (fromNat 2) (fromNat 3)`
- leading-zeros case (-5<=n<=0): `fdiv (fromNat 1) (fromNat 1000000)` = 1e-6 (renders "0.000001")
- exponential case: `fdiv (fromNat 1) (<10^7>)` = 1e-7 (renders "1e-7"); a large one via repeated fmul to ~1e21
- specials: `fdiv (fromNat 1) (fromNat 0)` = Infinity, `fsub (fromNat 0) (fdiv (fromNat 1)(fromNat 0))` = -Infinity, `fdiv (fromNat 0)(fromNat 0)` = NaN
- negative: `fsub (fromNat 0) (fromNat 1234567)` = -1234567
- subnormal / 1.8e308 boundary: if not exactly constructible from small-nat arithmetic, construct the nearest witness reaching the same branch (e.g. a very small value via many `fdiv`), and DOCUMENT it; a subnormal is reachable by dividing 5 by a power of ten large enough to underflow into subnormal range (e.g. repeated `fdiv` by `1e300` twice). Every formatter branch MUST have >= 1 witness; if a branch cannot be reached arithmetically, note it explicitly (it will be covered once FloatLit lands).

For each corpus entry, build TWO programs: `mainShow` (`main : Float = <expr>`, exercises show) and `mainPrint` (`main = printFloat <expr>`, exercises the IO path). Run both on all 9 backends; assert stdout (trimmed) equals `ecmaFloat(expected)`, where `expected` is the Go `float64` computed by the SAME arithmetic (compute it in Go alongside).

- [ ] **Step 1: Write the gate**

```go
package harness

import (
	"strings"
	"testing"
)

type floatWitness struct {
	name     string  // branch documented
	buildExpr string // rune expr producing the Float, e.g. "Std.Float.fdiv (Std.Float.fromNat 1) (Std.Float.fromNat 2)"
	expected float64 // the SAME value computed in Go
}

func TestFloatDisplayParity(t *testing.T) {
	ws := floatWitnesses() // returns the corpus above, expected computed in Go
	for _, w := range ws {
		w := w
		t.Run(w.name, func(t *testing.T) {
			want := ecmaFloat(w.expected) // the harness copy of the oracle
			// show path: main : Float
			srcShow := preludeAnd(t, "main : Float is "+w.buildExpr+" end")
			assertFloatDisplayAgree(t, srcShow, "main", want)
			// print path: main = printFloat <expr>
			srcPrint := preludeAnd(t, "main is Std.Float.printFloat ("+w.buildExpr+") end")
			assertFloatDisplayAgree(t, srcPrint, "main", want)
		})
	}
}
```

`assertFloatDisplayAgree` mirrors `assertNumericParityAgree` in numeric_parity_test.go: for each `bibleBackends()` entry (skip on missing toolchain by the established convention), emit + run, trim stdout, and assert it equals `want` (a FIXED string from the Go oracle, not another backend's output). WASM is INCLUDED. If a witness value cannot be represented (should not happen for these), the test fails loudly rather than skipping.

- [ ] **Step 2: Run to verify it fails (or reveals a residual gap)**

Run: `go test ./harness/ -run TestFloatDisplayParity -count=1 -v`
Expected: FAIL on any backend still diverging (this is the acceptance check that Tasks 1-4 actually closed every gap; a green run here is the deliverable). If a SOURCE backend (go/rust/jvm) diverges on some branch, that is a newly-surfaced bug to fix in that backend's float show, same pattern as the native fix -- fix it and note it.

- [ ] **Step 3: Fix any residual divergence surfaced**

If the gate is red on a specific backend/branch, fix that backend's float rendering to ECMAScript (the same move as Tasks 1-4). Re-run until byte-identical 9-way.

- [ ] **Step 4: Full green**

Run: `go test ./harness/ -run TestFloatDisplayParity -count=1 -v`
Expected: PASS, all 9 backends, both paths, every witness.

- [ ] **Step 5: Dash-scan and commit**

```bash
git diff -- harness/float_display_parity_test.go | grep -nP '[\x{2013}\x{2014}]' && echo DASH || echo clean
git add harness/float_display_parity_test.go
git commit -m "test(harness): 9-backend float display parity gate (printFloat + show)

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Self-review notes (for the executor)

- Tasks 1-2 are mechanical and low-risk (route show through an existing function). Cheap-model implementers.
- Tasks 3-4 are the high-bug-surface numeric kernels. The Go reference (`atofRef`/`dragon4Ref`) is the executable spec; it MUST pass a >= 20k random differential sweep vs `strconv` before the WAT port. Adversarial review mandatory; the reviewer should re-run the sweep with a different seed and spot-check subnormal + 17-digit + boundary values.
- Task 5 is the integration acceptance; a green run is the whole Track's deliverable. It may surface a source-backend gap (go/rust/jvm float show) -- fix in place.
- No REPL pin task: Float is host-opaque in NbE, so the REPL cannot compute a float to display; display is a COMPILED-backend property, covered by Task 5. (Track A already established the host-opaque REPL story.)
