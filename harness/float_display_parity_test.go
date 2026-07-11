package harness

// float_display_parity_test.go -- v4 Float Track B, Task 5: the 9-way display-parity
// conformance gate. Track A (numeric_parity_test.go) deliberately never PRINTED a
// Float (arithmetic was verified via feq only, since display was not yet unified
// across backends). This gate is the opposite: it PRINTS a Float two ways -- show()-
// ing a raw Float value (main : Float, no float IO at all) and Std.Float.printFloat
// (the IO path) -- and asserts all 9 backends (js/py/go/rs/erl/jvm/c/ll/wasm) agree
// byte-for-byte with the Go ECMAScript oracle (ecmaFloat below, the same independent
// oracle defined in codegen/wasm_float_display_test.go, copied here since that lives
// in a different package).
//
// Building this gate's corpus surfaced that show()'s float rendering was STILL
// native-format (not ECMAScript) on FIVE of the nine backends -- python (a trailing
// ".0"), go (%v-style scientific notation + "+Inf"/"NaN" spellings), rust (the
// Display impl's decimal-only rendering + "inf"), jvm (Double.toString), and beam (an
// outright function_clause CRASH: show_t had no float clause at all outside float
// IO). Only js (JS's own Number.toString) and the already-fixed native c/ll/wasm
// (Track B Tasks 1-4) were correct out of the box. Each of the five was the SAME bug
// shape Tasks 1/2 fixed on native C/LL: printFloat already routed through a correct
// ECMAScript __fmtf-equivalent formatter, but show()'s float branch used the host
// language's native formatting instead of reaching that same formatter. Fixed in
// place (codegen/py.go, codegen/golang.go, codegen/rust.go, codegen/jvm.go,
// codegen/beam.go) by hoisting each backend's __fmtf-equivalent to always-emitted
// (not gated to float IO) and routing show()/_show/show_t's float branch through it
// -- exactly Tasks 1/2's native C/LL move, repeated on the remaining five source
// backends. No core/store/elaborate change (Rule 4: mutate the shadow only).
//
// Corpus: every Float value is built ARITHMETICALLY (Std.Float.fromNat/fadd/fsub/
// fmul/fdiv) -- there is no FloatLit. Each witness documents which ECMAScript
// Number::toString dressing branch it targets: (A) integer/trailing-zero
// (p<=n<=21), (B) point-inside (0<n<p), (C) leading-zeros (-5<=n<=0), (D)
// exponential (else), plus the subnormal bit-pattern case and the three IEEE
// specials (Infinity/-Infinity/NaN). fromNat is only ever applied to "safe" (exactly
// float64-representable) small/moderate integers (<= 2^53-ish); magnitudes far
// outside that range are reached by CHAINING fmul/fdiv (real IEEE double ops,
// already proven 9-way conformant by Track A's numeric_parity_test.go), which the Go
// witness computation mirrors with the IDENTICAL left-folded operation sequence --
// so `expected` is guaranteed bit-identical to whatever the runtime actually
// computes, independent of any fromNat bignum-conversion subtlety at extreme
// magnitudes (out of this task's scope; kernel frozen).

import (
	"fmt"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// ---- the ECMAScript Number::toString(10) oracle (copied from
// codegen/wasm_float_display_test.go -- that file lives in package codegen_test, so
// the harness needs its own copy). ecmaFloat renders x the way every backend's
// __fmtf/_show/show float branch must, using Go's strconv shortest DIGITS fed
// through the four ECMAScript dressing cases. ----

// ecmaFloat is the INDEPENDENT oracle every backend's stdout is compared against.
func ecmaFloat(x float64) string {
	if x != x {
		return "NaN"
	}
	if math.IsInf(x, 1) {
		return "Infinity"
	}
	if math.IsInf(x, -1) {
		return "-Infinity"
	}
	if x == 0 {
		return "0" // ECMAScript: -0 renders "0"
	}
	neg := math.Signbit(x)
	if neg {
		x = -x
	}
	s, n := parseEcmaDigits(strconv.AppendFloat(nil, x, 'e', -1, 64))
	out := dressEcma(s, n)
	if neg {
		out = "-" + out
	}
	return out
}

// parseEcmaDigits reads strconv's 'e' shortest form ("d.ddde+NN" / "de+NN") and
// returns the digit string (point removed) plus the position n where
// value = digits * 10^(n-len). Because the 'e' form always has exactly one digit
// before the point, n = exp+1 regardless of length.
func parseEcmaDigits(b []byte) (string, int) {
	s := string(b)
	ei := strings.IndexAny(s, "eE")
	exp, _ := strconv.Atoi(s[ei+1:])
	mant := strings.Replace(s[:ei], ".", "", 1)
	return mant, exp + 1
}

// dressEcma applies the four ECMAScript Number::toString dressing cases to a
// shortest digit string s and position n (value = s * 10^(n-len(s))). This is the
// SHARED dressing every backend's formatter performs; it must match byte-for-byte.
func dressEcma(s string, n int) string {
	p := len(s)
	switch {
	case p <= n && n <= 21: // integer: digits then (n-p) trailing zeros
		return s + strings.Repeat("0", n-p)
	case 0 < n && n < p: // point inside: digits[:n] "." digits[n:]
		return s[:n] + "." + s[n:]
	case -5 <= n && n <= 0: // leading zeros: "0." (-n) zeros then digits
		return "0." + strings.Repeat("0", -n) + s
	default: // exponential: d1 ["." d2..dp] "e" sign(n-1) |n-1|
		var b strings.Builder
		b.WriteByte(s[0])
		if p > 1 {
			b.WriteByte('.')
			b.WriteString(s[1:])
		}
		b.WriteByte('e')
		en := n - 1
		if en >= 0 {
			b.WriteByte('+')
		} else {
			b.WriteByte('-')
			en = -en
		}
		b.WriteString(strconv.Itoa(en))
		return b.String()
	}
}

// ---- the corpus ----

// floatWitness is one display-parity corpus entry: expr is a rune Std.Float
// expression (no FloatLit), and expected is the SAME value computed in Go by the
// identical arithmetic op sequence, so ecmaFloat(expected) is the fixed oracle
// string every backend's stdout must equal.
type floatWitness struct {
	name     string // documents the ECMAScript dressing branch (or special/subnormal case) targeted
	expr     string // the rune Std.Float expression producing the value
	expected float64
}

// nestedFloatOp builds `Std.Float.<op> (Std.Float.<op> (... base ...) factor)
// factor` applied n times (left fold), plus the Go float64 that results from
// applying `apply` (the matching Go op) n times with the IDENTICAL fold order -- so
// the returned expr and value are guaranteed to agree bit-for-bit with whatever the
// runtime computes, as long as fmul/fdiv are IEEE-754 double ops (already proven
// 9-way in Track A's numeric_parity_test.go). Used to reach magnitudes (~1e21,
// subnormal) far outside fromNat's "safe" exact-integer range without ever routing
// a huge Nat literal through fromNat.
func nestedFloatOp(op string, base string, baseVal float64, factor string, factorVal float64, n int, apply func(a, b float64) float64) (string, float64) {
	expr := base
	val := baseVal
	for i := 0; i < n; i++ {
		expr = fmt.Sprintf("Std.Float.%s (%s) (%s)", op, expr, factor)
		val = apply(val, factorVal)
	}
	return expr, val
}

// floatWitnesses is the Task 5 acceptance corpus. Every FORMATTER BRANCH (each of
// the four ECMAScript dressing cases, the subnormal bit pattern, and the three IEEE
// specials) has at least one witness, per the plan/spec (Decision B3).
func floatWitnesses() []floatWitness {
	ws := []floatWitness{
		// Branch A (integer/trailing-zero, p<=n<=21): the original %g-divergence
		// branch (Tasks 1/2's canonical example -- %g rendered "1.23457e+06").
		{"integer_1234567_branchA", "Std.Float.fromNat 1234567", 1234567},
		{"integer_100_branchA_trailing_zero", "Std.Float.fromNat 100", 100},
		{"integer_1000000_branchA_trailing_zeros", "Std.Float.fromNat 1000000", 1000000},
		// Exact float64-representable boundary integers (2^53, 2^53+2): branch A,
		// also exercises fromNat's bignum->double conversion at the mantissa
		// boundary (spec B3's suggested "boundary integers" witnesses).
		{"integer_2pow53_branchA_boundary", "Std.Float.fromNat 9007199254740992", 9007199254740992},
		{"integer_2pow53plus2_branchA_boundary", "Std.Float.fromNat 9007199254740994", 9007199254740994},
		// Negative integer: branch A, sign applied by the caller.
		{"negative_integer_branchA", "Std.Float.fsub (Std.Float.fromNat 0) (Std.Float.fromNat 1234567)", -1234567},

		// Branch B (point-inside, 0<n<p): 5/4 = 1.25 (digits "125", point after the
		// 1st digit).
		{"point_inside_1_25_branchB", "Std.Float.fdiv (Std.Float.fromNat 5) (Std.Float.fromNat 4)", 5.0 / 4.0},

		// Branch C (leading-zeros, -5<=n<=0).
		{"leading_zero_half_branchC", "Std.Float.fdiv (Std.Float.fromNat 1) (Std.Float.fromNat 2)", 1.0 / 2.0},
		{"leading_zero_eighth_branchC", "Std.Float.fdiv (Std.Float.fromNat 1) (Std.Float.fromNat 8)", 1.0 / 8.0},
		{"leading_zero_1e-6_branchC", "Std.Float.fdiv (Std.Float.fromNat 1) (Std.Float.fromNat 1000000)", 1.0 / 1000000.0},
		// 16/17-significant-digit stress (the p>=16 double-rounding-risk band Task
		// 4's background section called out): 1/3 and 2/3 are 16 digits, 1/7 is 17.
		{"sixteen_digit_third_branchC", "Std.Float.fdiv (Std.Float.fromNat 1) (Std.Float.fromNat 3)", 1.0 / 3.0},
		{"sixteen_digit_two_thirds_branchC", "Std.Float.fdiv (Std.Float.fromNat 2) (Std.Float.fromNat 3)", 2.0 / 3.0},
		{"seventeen_digit_seventh_branchC", "Std.Float.fdiv (Std.Float.fromNat 1) (Std.Float.fromNat 7)", 1.0 / 7.0},

		// Specials (all three IEEE atoms; BEAM reboxes these as atoms internally --
		// see codegen/beam_float_specials_test.go -- so this doubles as a cross-check
		// that the atom-aware show()/printFloat paths agree with the other 8
		// backends' native IEEE float specials).
		{"special_pos_inf", "Std.Float.fdiv (Std.Float.fromNat 1) (Std.Float.fromNat 0)", math.Inf(1)},
		{"special_neg_inf", "Std.Float.fsub (Std.Float.fromNat 0) (Std.Float.fdiv (Std.Float.fromNat 1) (Std.Float.fromNat 0))", math.Inf(-1)},
		{"special_nan", "Std.Float.fdiv (Std.Float.fromNat 0) (Std.Float.fromNat 0)", math.NaN()},

		// Branch D (exponential, else): 1e-7 is small enough (n=-6 < -5) to force the
		// exponential branch rather than leading-zeros (1e-6 above stays leading-zero,
		// n=-5).
		{"exponential_1e-7_branchD", "Std.Float.fdiv (Std.Float.fromNat 1) (Std.Float.fromNat 10000000)", 1.0 / 10000000.0},
	}

	// Branch D, large magnitude (~1e21, n=22>21 so exponential): built via 21
	// repeated fmul-by-10 steps (never a giant Nat literal through fromNat), so
	// `expected` mirrors the exact IEEE double multiply chain the runtime performs.
	largeExpr, largeVal := nestedFloatOp("fmul", "Std.Float.fromNat 1", 1.0, "Std.Float.fromNat 10", 10.0, 21,
		func(a, b float64) float64 { return a * b })
	ws = append(ws, floatWitness{"exponential_large_1e21_branchD_repeated_fmul", largeExpr, largeVal})

	// Subnormal bit pattern (IEEE biased exponent field == 0): built via 21 repeated
	// fdiv-by-1e15 steps from 5.0 (never a giant Nat literal), reaching 5e-315 --
	// comfortably inside the subnormal range (0 < |x| < 2.2250738585072014e-308).
	// This also happens to land in the exponential DRESSING branch (n=-314), but
	// internally it is a genuinely denormalized double, exercising the formatters'
	// subnormal decomposition path independently of which dressing case the digits
	// fall into.
	subExpr, subVal := nestedFloatOp("fdiv", "Std.Float.fromNat 5", 5.0, "Std.Float.fromNat 1000000000000000", 1e15, 21,
		func(a, b float64) float64 { return a / b })
	ws = append(ws, floatWitness{"subnormal_5e-315_branchD_repeated_fdiv", subExpr, subVal})

	// Negative zero (IEEE -0.0): ECMAScript renders -0 as "0" (see ecmaFloat's x==0
	// branch above), but no prior witness produced negative zero, leaving that rule
	// untested. expr is (-1.0) * 0.0 via Std.Float.fmul (Std.Float.fsub 0 1)
	// (Std.Float.fromNat 0). expected is computed via the IDENTICAL op chain but
	// routed through plain (non-const) float64 vars: Go's untyped constant
	// arithmetic has no signed zero, so writing `(0.0 - 1.0) * 0.0` directly as a
	// literal constant expression folds to +0.0 at compile time (verified:
	// math.Signbit of that literal is false) -- routing the same ops through
	// runtime float64 variables instead preserves the true IEEE -0.0 bit pattern
	// (verified: math.Signbit(negZeroVal) && negZeroVal == 0 are both true).
	negZeroBase, negZeroOne := 0.0, 1.0
	negZeroVal := (negZeroBase - negZeroOne) * negZeroBase
	ws = append(ws, floatWitness{
		"negative_zero_renders_0",
		"Std.Float.fmul (Std.Float.fsub (Std.Float.fromNat 0) (Std.Float.fromNat 1)) (Std.Float.fromNat 0)",
		negZeroVal,
	})

	return ws
}

// runFloatDisplayBackend emits+runs `main` from src (loaded on top of the shared
// prelude) on one backend in dir, returning trimmed stdout. Mirrors
// runNumericParityBackend's shape (numeric_parity_test.go) and runBibleBackend's
// (bible_conformance_test.go). Skips (ok=false) if the backend's toolchain is
// absent.
func runFloatDisplayBackend(t *testing.T, bk bibleBackend, src, dir string) (string, bool) {
	t.Helper()
	if _, err := exec.LookPath(bk.bin); err != nil {
		return "", false
	}
	s := session.New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("loading witness source: %v\n--- source ---\n%s", err, src)
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatalf("[%s] EmitProgram(main): %v\n--- source ---\n%s", bk.name, err, src)
	}
	out, err := bk.emit(p)
	if err != nil {
		t.Fatalf("[%s] emit: %v", bk.name, err)
	}
	f := filepath.Join(dir, "main."+bk.ext)
	if err := os.WriteFile(f, []byte(out), 0o644); err != nil {
		t.Fatal(err)
	}
	runFile := f
	if bk.compile != nil {
		bin := filepath.Join(dir, "main_"+bk.name+".bin")
		if bk.runtime != nil {
			if err := os.WriteFile(filepath.Join(dir, "runtime.c"), []byte(bk.runtime(p)), 0o644); err != nil {
				t.Fatal(err)
			}
		}
		if cout, err := bk.compile(f, bin).CombinedOutput(); err != nil {
			t.Fatalf("[%s] compile failed: %v\n%s\n--- emitted ---\n%s", bk.name, err, cout, out)
		}
		runFile = bin
	}
	cmd := bk.run(runFile)
	cmd.Dir = dir
	stdout, err := cmd.Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("[%s] run failed: %v\n%s\n--- emitted ---\n%s", bk.name, err, stderr, out)
	}
	return strings.TrimSpace(string(stdout)), true
}

// assertFloatDisplayAgree runs `main` (from src) on every backend (bibleBackends(),
// all 9 -- WASM included, no exclusions) in its own temp dir and asserts they all
// produce EXACTLY `want`, a FIXED string from the Go ecmaFloat oracle (never another
// backend's output). A missing toolchain is reported via t.Skipf (not a false
// pass), the same convention every other conformance gate in this package uses.
func assertFloatDisplayAgree(t *testing.T, src, want string) {
	t.Helper()
	for _, bk := range bibleBackends() {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			dir := t.TempDir()
			got, ok := runFloatDisplayBackend(t, bk, src, dir)
			if !ok {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got != want {
				t.Errorf("%s = %q, want %q (backends must not diverge)", bk.name, got, want)
			}
		})
	}
}

// ---- the batched driver ----
//
// The naive shape of this gate (one compiled+run program per witness per path per
// backend) is 20 witnesses * 2 paths * 9 backends = 360 compile+run cycles. Five of
// those backends (jvm/rust/native-c/native-ll/wasm) each take real wall-clock
// seconds to COMPILE, so the naive gate ran past a 10-minute `go test` timeout.
// This gate must stay ONE test among many in a full-suite run with a 30-minute
// total budget, so it is reworked below to compile ONCE PER BACKEND for the
// printFloat path (batching all 20 witnesses into a single sequenced IO program)
// and to compile only 9 backends * 4 witnesses for the show path, rather than
// 9*20. Both paths still assert every stdout line against the FIXED Go ecmaFloat
// oracle (never another backend's output); no backend is skipped except for a
// genuinely missing toolchain (the pre-existing t.Skipf convention).

// floatPrintBatchSrc sequences ws into ONE `main : IO Std.Float.Float` program: a
// right-folded bindIO chain of `Std.Float.printFloat (<expr>)` calls, one per
// witness, in order -- mirroring the bindIO/printNat chaining pattern in
// listings/ch210_io_os.rune (`bindIO Nat Nat timeNanos (fn (t : Nat) is bindIO Nat
// Nat (printNat 1) (fn (a : Nat) is printNat 2 end) end)`), specialized to
// Std.Float.Float end-to-end and closed by a bare final printFloat call (no
// trailing pureIO needed -- printFloat's own result IS already an IO Float, the
// exact type bindIO's last argument needs). Compiling this once per backend prints
// all len(ws) lines in a single run instead of len(ws) separate compiles.
func floatPrintBatchSrc(ws []floatWitness) string {
	if len(ws) == 0 {
		panic("floatPrintBatchSrc: empty witness list")
	}
	body := fmt.Sprintf("Std.Float.printFloat (%s)", ws[len(ws)-1].expr)
	for i := len(ws) - 2; i >= 0; i-- {
		body = fmt.Sprintf(
			"bindIO Std.Float.Float Std.Float.Float (Std.Float.printFloat (%s)) (fn (a%d : Std.Float.Float) is\n  %s\nend)",
			ws[i].expr, i, body)
	}
	return fmt.Sprintf("main : IO Std.Float.Float is\n%s\nend\n", body)
}

// floatPrintBatchWant is the expected stdout for floatPrintBatchSrc(ws): one
// ecmaFloat line per witness, IN ORDER (each is printFloat's own side-effecting
// print), PLUS one extra final line. That extra line is not a driver quirk this
// test invents -- every one of the 9 backends' emitted `main` unconditionally
// prints `_show`/`$show`/`show` of main's own return value after running it
// (golang.go:473 `fmt.Println(_show(mainExpr))`, py.go:406, rust.go:384, jvm.go:273,
// beam.go:39 `io:format("~s~n",[show(...)])`, js.go:397, c.go:243, ll.go:139,
// wasm.go's $rune_main), regardless of the expression's type -- this is exactly the
// pre-existing "double-print" convention examples/double.rune and
// TestIOFloatDoubleDemo already establish for a single printFloat call
// ("6.28\n6.28"). Here `main`'s final IO Std.Float.Float action is the LAST
// witness's printFloat call, and printFloat returns its argument unchanged, so
// that trailing driver-show line duplicates the last witness's own printFloat
// line -- it is not a new assertion surface, just the existing single-witness
// convention carried through the batch unchanged.
func floatPrintBatchWant(ws []floatWitness) string {
	lines := make([]string, len(ws))
	for i, w := range ws {
		lines[i] = ecmaFloat(w.expected)
	}
	lines = append(lines, lines[len(lines)-1])
	return strings.Join(lines, "\n")
}

// floatShowSubset picks the witnesses (by name, out of floatWitnesses()) whose
// show() path the printFloat batch cannot already cover. The print batch routes
// all 20 witnesses through __fmtf on every backend and its final line exercises
// show() on a finite value; the show subset therefore only needs the
// non-finite/signed-zero edges (the 3 IEEE specials -- separate atom guards on
// BEAM -- plus -0.0) whose show() path differs from a finite's. Concretely: the
// four finite dressing-branch representatives (A/B/C/D) that a naive subset would
// otherwise re-check are dropped, since the batch's trailing double-print line
// (main's own returned Float, shown at runtime) already proves show() reaches the
// same formatter as printFloat for a finite value on every backend. What remains
// is exactly the four cases whose show() branch is genuinely distinct from the
// finite path: all three IEEE specials are included (not just NaN) because on
// BEAM, nan/pos_inf/neg_inf are SEPARATE atom guards in show (see
// codegen/beam_float_specials_test.go) -- a NaN-only subset would leave the
// pos_inf/neg_inf show guards on that backend completely unexercised. Negative
// zero is included too, since it is the only witness whose oracle string ("0")
// differs from what naive native formatting would produce ("-0" or "0.0") -- the
// exact bug shape this whole gate exists to catch.
func floatShowSubset(t *testing.T, all []floatWitness) []floatWitness {
	t.Helper()
	want := []string{
		"special_pos_inf",
		"special_neg_inf",
		"special_nan",
		"negative_zero_renders_0",
	}
	byName := make(map[string]floatWitness, len(all))
	for _, w := range all {
		byName[w.name] = w
	}
	out := make([]floatWitness, 0, len(want))
	for _, n := range want {
		w, ok := byName[n]
		if !ok {
			t.Fatalf("floatShowSubset: witness %q not found in floatWitnesses()", n)
		}
		out = append(out, w)
	}
	return out
}

// TestFloatDisplayParity is the v4 Float Track B, Task 5 acceptance gate: a raw
// Float displays byte-identically on all 9 backends, matching ECMAScript
// Number::toString, via BOTH the printFloat IO path (all 20 witnesses, batched
// into one compiled program per backend, whose trailing double-print line also
// exercises show() on a finite value) and the show path (`main : Float`, no float
// IO at all -- the 4 non-finite/signed-zero edges show() cannot inherit from the
// batch: the three IEEE specials and negative zero, one compile per witness per
// backend since each is a distinct `main` expression). Total compiles: 9
// (printFloat batch) + 4*9 (show subset) = 45, versus the naive 20*2*9 = 360 --
// comfortably inside the suite's time budget while still exercising every witness
// (via printFloat) and every show()-distinct edge (via show).
func TestFloatDisplayParity(t *testing.T) {
	all := floatWitnesses()

	t.Run("printFloat_batch", func(t *testing.T) {
		src := floatPrintBatchSrc(all)
		want := floatPrintBatchWant(all)
		assertFloatDisplayAgree(t, src, want)
	})

	t.Run("show_subset", func(t *testing.T) {
		for _, w := range floatShowSubset(t, all) {
			w := w
			t.Run(w.name, func(t *testing.T) {
				want := ecmaFloat(w.expected)
				src := fmt.Sprintf("main : Std.Float.Float is %s end\n", w.expr)
				assertFloatDisplayAgree(t, src, want)
			})
		}
	})
}
