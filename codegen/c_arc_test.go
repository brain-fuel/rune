package codegen_test

// c_arc_test.go -- native ARC balance gates for the C backend (Plan 6e Task 2).
//
// buildAndRunCWithReport compiles the C shadow with -DRUNE_ARC_REPORT (so the
// atexit hook prints "rt_live=N" to stderr) and returns (stdout, rt_live). The
// steady-loop test asserts that the retained heap does NOT scale with the work
// done -- Perceus releases fire, so a fold over n=50 and n=5000 leave the same
// (constant) number of live objects. Task 4 reuses this helper for the balance
// family. Modeled on codegen_test.go's runC (cc lookup + skip + temp dir + build).

import (
	"fmt"
	"math/big"
	"math/rand"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"testing"
	"time"

	"goforge.dev/rune/v3/codegen"
)

// TestCARCNatDispatchWrappedStep guards peelToMkClosure (closure.go): after the C backend
// runs Perceus, an IH-ignoring NatElim step whose body is COMPOUND (a `let`) has its outer
// step closure wrapped as CDrop/CLet{$own, innerMk, CDup{0,0}} -- the bare
// `outer.Body.(MkClosure)` assertion in StepIgnoresIH then fails and the eliminator falls
// back to the eager b3 fold. For the lambda-wrapped beqNat-shape parser guards (ch521) that
// b3 fold re-runs the recursive descent |byte-delim| times per level -> 44^depth blowup
// (10 GB OOM). This pins that natDispatch's constant-time `_case` still fires on the wrapped
// step: the emitted C must contain a `_case(` call, and the program must still be correct.
func TestCARCNatDispatchWrappedStep(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
dbl : Nat -> Nat is fn (n : Nat) is succ (succ n) end end
guard : Nat -> Nat is
  fn (n : Nat) is NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is let d = dbl k in d end) n end
end
main : Nat is guard 7 end`
	csrc := emitWith(t, codegen.C{}, src, "main")
	// natDispatch fired -> a `<elim>_case(` CALL appears (excluding the `static Value ..._case(`
	// definition). If peelToMkClosure regressed, StepIgnoresIH would fail and no call is emitted.
	calls := 0
	for _, line := range strings.Split(csrc, "\n") {
		if strings.Contains(line, "_case(") && !strings.Contains(line, "static Value") {
			calls++
		}
	}
	if calls == 0 {
		t.Fatalf("natDispatch _case did not fire on a wrapped IH-ignoring step (peelToMkClosure regressed); StepIgnoresIH fell back to the eager fold")
	}
	// guard 7 = dbl 6 = 8 (IH ignored; step returns dbl of the predecessor).
	if out, _ := buildAndRunCWithReport(t, src, "main"); out != "8" {
		t.Fatalf("guard 7 = %q, want 8", out)
	}
}

// TestCARCFoldPrimBalance is the Task-3 prim-body gate: `foldLines` over a temp file
// is the highest-risk foreign shape (apply-in-loop). The world step (foldLines_c5)
// borrows `step` (env[0]) and threads the accumulator; each line the emitted apply chain
// `apply(apply(apply(step, s), line), UNIT)` produces two fresh intermediate closures
// that NOTHING releases unless the prim body releases them, so a pre-sweep body leaks two
// K_CLO per line -> the retained heap scales with line count. It also returns the
// accumulator without retaining it (a borrowed env slot handed back as owned), which
// double-frees / reads-freed once Perceus releases the fold's IO result. Both are fixed
// by the sweep; this asserts (a) the count is correct and (b) rt_live does NOT scale with
// the number of lines.
func TestCARCFoldPrimBalance(t *testing.T) {
	srcTmpl := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end
foreign foldLines : (S : U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign printNat  : Nat -> IO Nat end
step : Nat -> Nat -> IO Nat is
  fn (count : Nat) (line : Nat) is pureIO Nat (succ count) end
end
main : IO Nat is
  bindIO Nat Nat (foldLines Nat (codeOf "%s") step zero)
    (fn (n : Nat) is printNat n end)
end`
	mkfile := func(n int) string {
		dir := t.TempDir()
		p := dir + "/lines.txt"
		var b strings.Builder
		for i := 0; i < n; i++ {
			b.WriteString("line\n")
		}
		if err := os.WriteFile(p, []byte(b.String()), 0o644); err != nil {
			t.Fatal(err)
		}
		return p
	}
	pSmall := mkfile(5)
	pBig := mkfile(2000)
	outSmall, liveSmall := buildAndRunCWithReport(t, fmt.Sprintf(srcTmpl, pSmall), "main")
	outBig, liveBig := buildAndRunCWithReport(t, fmt.Sprintf(srcTmpl, pBig), "main")
	// main : IO Nat prints the count (printNat) then show() prints the returned nat.
	if outSmall != "5\n5" || outBig != "2000\n2000" {
		t.Fatalf("foldLines count wrong: %q (5 lines), %q (2000 lines)", outSmall, outBig)
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("foldLines retained heap scales with line count: rt_live %d (5 lines) vs %d (2000 lines) -- prim apply-in-loop leaks its per-line intermediates", liveSmall, liveBig)
	}
}

// TestCARCTerminalPrimArgBalance is the terminal-prim-arg gate (the Task-3 fixup):
// apply CONSUMES its argument, so a TERMINAL pure prim body (byteLen_c1 here) OWNS its
// direct arg and must rt_release it when it neither stores nor returns it -- exactly
// what every WASM twin does (byteLen_c1 wasm.go:1262 "the owned $arg is consumed
// (released)"; splitOn/jsonStrField/sqlQuote; fromNat/fmul_c2 in wasm_float.go). The
// step routes each boxed line (a fresh owned K_BIG foldLines builds once per line)
// through byteLen: a body that fails to release its consumed arg leaks one packed
// bignum per line, so rt_live scales with line count. env[i] slots stay BORROWED --
// only the direct arg of the terminal step is owned.
func TestCARCTerminalPrimArgBalance(t *testing.T) {
	srcTmpl := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end
foreign foldLines : (S : U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign byteLen   : Nat -> Nat end
foreign printNat  : Nat -> IO Nat end
step : Nat -> Nat -> IO Nat is
  fn (total : Nat) (line : Nat) is pureIO Nat (byteLen line) end
end
main : IO Nat is
  bindIO Nat Nat (foldLines Nat (codeOf "%s") step zero)
    (fn (n : Nat) is printNat n end)
end`
	mkfile := func(n int) string {
		dir := t.TempDir()
		p := dir + "/lines.txt"
		var b strings.Builder
		for i := 0; i < n; i++ {
			b.WriteString("word\n")
		}
		if err := os.WriteFile(p, []byte(b.String()), 0o644); err != nil {
			t.Fatal(err)
		}
		return p
	}
	pSmall := mkfile(5)
	pBig := mkfile(2000)
	outSmall, liveSmall := buildAndRunCWithReport(t, fmt.Sprintf(srcTmpl, pSmall), "main")
	outBig, liveBig := buildAndRunCWithReport(t, fmt.Sprintf(srcTmpl, pBig), "main")
	// Every line is "word" (4 bytes); the step yields byteLen of the current line, so
	// the fold's final value is 4. printNat prints it, then show() prints it again.
	if outSmall != "4\n4" || outBig != "4\n4" {
		t.Fatalf("byteLen fold wrong: %q (5 lines), %q (2000 lines), want 4\\n4", outSmall, outBig)
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("terminal prim leaks its owned direct arg: rt_live %d (5 lines) vs %d (2000 lines) -- byteLen_c1 must release the consumed line", liveSmall, liveBig)
	}
}

// buildAndRunCWithReport emits the C backend for `main`, compiles it with
// -DRUNE_ARC_REPORT, runs it, and returns (trimmed stdout, rt_live). It skips
// gracefully when no C compiler is on PATH.
func buildAndRunCWithReport(t *testing.T, src, mainName string) (string, int64) {
	t.Helper()
	cc := ""
	for _, cand := range []string{"cc", "gcc", "clang"} {
		if _, err := exec.LookPath(cand); err == nil {
			cc = cand
			break
		}
	}
	if cc == "" {
		t.Skip("no C compiler (cc/gcc/clang) in PATH")
	}
	csrc := emitWith(t, codegen.C{}, src, mainName)
	dir := t.TempDir()
	f := dir + "/main.c"
	bin := dir + "/main"
	if err := os.WriteFile(f, []byte(csrc), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command(cc, "-DRUNE_ARC_REPORT", "-o", bin, f).CombinedOutput(); err != nil {
		t.Fatalf("%s: %v\n%s\n--- emitted ---\n%s", cc, err, out, csrc)
	}
	cmd := exec.Command(bin)
	var stdout, stderr strings.Builder
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run: %v\nstderr=%s", err, stderr.String())
	}
	// Parse the last "rt_live=N" line from stderr.
	var live int64 = -1
	for _, line := range strings.Split(stderr.String(), "\n") {
		if i := strings.Index(line, "rt_live="); i >= 0 {
			if v, err := strconv.ParseInt(strings.TrimSpace(line[i+len("rt_live="):]), 10, 64); err == nil {
				live = v
			}
		}
	}
	if live < 0 {
		t.Fatalf("no rt_live= line in stderr:\n%s", stderr.String())
	}
	return strings.TrimSpace(stdout.String()), live
}

// TestCARCSteadyLoop builds a datatype-eliminator fold (NatElim over builtin nat,
// the step USES its induction hypothesis so it is NOT accel-folded -- it runs the
// b3 fold loop) at two very different depths and checks the retained heap does not
// scale with the work done: releases fire, so the per-iteration counter / succ /
// step-closure temporaries do not accumulate. A leak in the fold would make the
// n=5000 run's rt_live grow ~100x over the n=50 run.
func TestCARCSteadyLoop(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
double : Nat -> Nat is
  fn (n : Nat) is case n of | zero -> zero | succ k with ih -> succ (succ ih) end end
end
main : Nat is double %d end`
	_, liveSmall := buildAndRunCWithReport(t, fmt.Sprintf(src, 50), "main")
	_, liveBig := buildAndRunCWithReport(t, fmt.Sprintf(src, 5000), "main")
	if liveBig-liveSmall > 64 {
		t.Fatalf("retained heap scales with work: rt_live %d (n=50) vs %d (n=5000) - releases not firing", liveSmall, liveBig)
	}
}

// TestCARCNatDispatchSteady guards the IH-ignoring one-peel dispatch (natDispatch /
// <elim>_case) -- a native-only path (WASM has no _case; its ownership must be balanced
// inside the _case helper). `pred` uses an IH-IGNORING NatElim step (returns k, drops
// ih), so it lowers via _case; the retained heap must not scale with n.
func TestCARCNatDispatchSteady(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
pred : Nat -> Nat is
  fn (n : Nat) is NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is k end) n end
end
main : Nat is pred %d end`
	outSmall, liveSmall := buildAndRunCWithReport(t, fmt.Sprintf(src, 50), "main")
	outBig, liveBig := buildAndRunCWithReport(t, fmt.Sprintf(src, 5000), "main")
	if outSmall != "49" || outBig != "4999" {
		t.Fatalf("pred wrong: %q (n=50), %q (n=5000)", outSmall, outBig)
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("natDispatch _case leak scales: rt_live %d (n=50) vs %d (n=5000)", liveSmall, liveBig)
	}
}

// ============================================================================
// Task 4: the native ARC gate family -- twins of wasm_arc_test.go's shapes as
// size-invariance assertions (rt_live at a small vs a large size), plus the
// ARC pressure gate that replaces the retired tiny-heap mark-sweep tests, plus
// an ASAN variant. Every test below reuses buildAndRunCWithReport (Task 2).
// ============================================================================

// TestCARCRecursiveRelease twins TestARCRecursiveRelease (wasm_arc_test.go): a
// constructor CHAIN (a List Nat built to length n, each cons cell holding a
// bignum) must have its ENTIRE chain recursively released when consumed --
// not just its own cell. `length` walks the whole chain via ListElim; if the
// C backend's K_CON release only freed the immediate cell (not recursing into
// the `rest` field), every cons cell and its held Nat would leak and rt_live
// would scale with n. Twinned as size-invariance (n=50 vs n=5000) rather than
// wasm's single manual build+release, matching this file's established style.
func TestCARCRecursiveRelease(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
replicate : Nat -> List Nat is
  fn (n : Nat) is
    NatElim (fn (x : Nat) is List Nat end) (nil Nat)
      (fn (k : Nat) (ih : List Nat) is cons Nat k ih end) n
  end
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end) zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end) xs
  end
end
main : Nat is length Nat (replicate %d) end`
	outSmall, liveSmall := buildAndRunCWithReport(t, fmt.Sprintf(src, 50), "main")
	outBig, liveBig := buildAndRunCWithReport(t, fmt.Sprintf(src, 5000), "main")
	if outSmall != "50" || outBig != "5000" {
		t.Fatalf("length(replicate n) wrong: %q (n=50), %q (n=5000)", outSmall, outBig)
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("recursive release through a cons chain leaks: rt_live %d (n=50) vs %d (n=5000) -- each cons cell (and its held bignum) must be freed as the chain is consumed", liveSmall, liveBig)
	}
}

// TestCARCPairBalance twins TestARCPairRelease (wasm_arc_test.go): a Sig pair
// (the outer core Σ, C1/R-SUM) whose two halves hold live values must have BOTH
// halves released when the pair is released. Each fold iteration builds a fresh
// K_PAIR holding two references to the loop counter k and passes it to `dropP`,
// whose body ignores it -- Perceus CDrops the dead owned arg, so the K_PAIR
// release branch must recursively free both slots or the per-iteration pairs
// (and the counter references they pin) accumulate and rt_live scales with n.
//
// KNOWN RESIDUAL (documented, not gated here): a runtime PROJECTION of a pair
// (`Fst p` / `Snd p`, whether p is a fresh `Pair …` or an owned function arg)
// leaks the projected-from pair -- Perceus classifies CFst/CSnd as borrowed
// aliases and only its CLet-bound pair-projection machinery releases the dying
// pair. The shape leaks identically on WASM (shared Perceus + a plain
// $rt_pair_fst alias), so the fix is a Perceus extension owned jointly with the
// frozen WASM emission, out of this plan's C-only scope. This test pins the
// walker (release of a whole dead pair), which is what TestARCPairRelease pins.
func TestCARCPairBalance(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
dropP : Sig Nat (fn (x : Nat) is Nat end) -> Nat -> Nat is
  fn (p : Sig Nat (fn (x : Nat) is Nat end)) (b : Nat) is b end
end
step : Nat -> Nat -> Nat is
  fn (k : Nat) (acc : Nat) is
    dropP (Pair Nat (fn (x : Nat) is Nat end) k k) (succ acc)
  end
end
main : Nat is
  NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is step k ih end) %d
end`
	outSmall, liveSmall := buildAndRunCWithReport(t, fmt.Sprintf(src, 50), "main")
	outBig, liveBig := buildAndRunCWithReport(t, fmt.Sprintf(src, 5000), "main")
	if outSmall != "50" || outBig != "5000" {
		t.Fatalf("pair-fold wrong: %q (n=50), %q (n=5000)", outSmall, outBig)
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("pair release leaks: rt_live %d (n=50) vs %d (n=5000) -- releasing a dead K_PAIR must free both slots", liveSmall, liveBig)
	}
}

// TestCARCClosureEnvBalance twins TestARCClosureRelease (wasm_arc_test.go): a
// closure capturing a bignum in its environment must have that env slot
// released when the closure is released. `mkAdder k` builds a fresh K_CLO
// capturing the loop counter k (a bignum) each iteration; the closure is
// applied once and then dead -- if K_CLO's env-release loop skipped a slot,
// each iteration's captured k would leak and rt_live would scale with n.
// Correctness is the running sum 0+1+...+(n-1) = n(n-1)/2, an independent
// check that the closures actually ran (not just built and dropped unused).
func TestCARCClosureEnvBalance(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
addN : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) n (fn (k : Nat) (ih : Nat) is succ ih end) m
  end
end
mkAdder : Nat -> Nat -> Nat is
  fn (x : Nat) is fn (y : Nat) is addN x y end end
end
step : Nat -> Nat -> Nat is
  fn (k : Nat) (acc : Nat) is (mkAdder k) acc end
end
main : Nat is
  NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is step k ih end) %d
end`
	sumBelow := func(n int64) string {
		return new(big.Int).Div(new(big.Int).Mul(big.NewInt(n), big.NewInt(n-1)), big.NewInt(2)).String()
	}
	outSmall, liveSmall := buildAndRunCWithReport(t, fmt.Sprintf(src, 50), "main")
	outBig, liveBig := buildAndRunCWithReport(t, fmt.Sprintf(src, 5000), "main")
	if outSmall != sumBelow(50) || outBig != sumBelow(5000) {
		t.Fatalf("closure-env fold wrong: %q (n=50, want %s), %q (n=5000, want %s)", outSmall, sumBelow(50), outBig, sumBelow(5000))
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("closure env release leaks a captured bignum: rt_live %d (n=50) vs %d (n=5000) -- releasing a K_CLO must free every env slot", liveSmall, liveBig)
	}
}

// TestCARCBignumBalance exercises GENUINE multi-limb bignum arithmetic (K_BIG
// is base-1e9 per limb -- see c.go BIG_BASE -- so a value at or above 1e18
// needs 3+ limbs, past anything an int64 fast path could hold) at scale: a
// seed of 1e18 added n times via the kernel-accelerated natAdd (registered so
// the call dispatches to real big_add carry-chain arithmetic, not an O(seed)
// eliminator unfold). rt_live must stay flat between n=50 and n=5000 even
// though the intermediate/final bignums genuinely grow into extra limbs, and
// the decimal result (checked against math/big) proves the arithmetic itself
// -- not just its ARC bookkeeping -- is correct at both scales.
func TestCARCBignumBalance(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
addN : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) n (fn (k : Nat) (ih : Nat) is succ ih end) m
  end
end
builtin natAdd addN
seed : Nat is 1000000000000000000 end
step : Nat -> Nat -> Nat is
  fn (k : Nat) (acc : Nat) is addN seed acc end
end
main : Nat is
  NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is step k ih end) %d
end`
	seed, _ := new(big.Int).SetString("1000000000000000000", 10)
	expect := func(n int64) string { return new(big.Int).Mul(seed, big.NewInt(n)).String() }
	outSmall, liveSmall := buildAndRunCWithReport(t, fmt.Sprintf(src, 50), "main")
	outBig, liveBig := buildAndRunCWithReport(t, fmt.Sprintf(src, 5000), "main")
	if outSmall != expect(50) || outBig != expect(5000) {
		t.Fatalf("bignum fold wrong: %q (n=50, want %s), %q (n=5000, want %s)", outSmall, expect(50), outBig, expect(5000))
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("multi-limb bignum arithmetic leaks: rt_live %d (n=50) vs %d (n=5000)", liveSmall, liveBig)
	}
}

// TestCARCBytesBalance twins TestARCBinLeafRelease (wasm_arc_test.go): K_BYTES
// (the Phase-0 `Bin` real-byte string) is a LEAF for the free walker (its
// bytes are inline words, not child Values), so its balance risk is pure
// alloc/free churn rather than recursive release. `binCons` prepends a byte
// by allocating a FRESH, one-byte-longer Bin and releasing the old one (c.go
// emitBinPrimsC) -- building a Bin of length n via n chained binCons calls
// must leave only the FINAL Bin live, not one per intermediate length. rt_live
// must stay flat between n=50 and n=5000; binLen confirms the byte count
// (i.e. the round trip, not just the alloc/free balance) is correct too.
func TestCARCBytesBalance(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
foreign Bin      : U end
foreign binEmpty : Bin end
foreign binCons  : Nat -> Bin -> Bin end
foreign binLen   : Bin -> Nat end
buildBin : Nat -> Bin is
  fn (n : Nat) is
    NatElim (fn (x : Nat) is Bin end) binEmpty (fn (k : Nat) (ih : Bin) is binCons k ih end) n
  end
end
main : Nat is binLen (buildBin %d) end`
	outSmall, liveSmall := buildAndRunCWithReport(t, fmt.Sprintf(src, 50), "main")
	outBig, liveBig := buildAndRunCWithReport(t, fmt.Sprintf(src, 5000), "main")
	if outSmall != "50" || outBig != "5000" {
		t.Fatalf("binLen(buildBin n) wrong: %q (n=50), %q (n=5000)", outSmall, outBig)
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("Bin churn leaks intermediate buffers: rt_live %d (n=50) vs %d (n=5000) -- binCons must release the buffer it grows past", liveSmall, liveBig)
	}
}

// pressureChurnSrc (Task 4 Step 2) is a list-build-and-discard loop: the outer
// NatElim b3 fold (an IH-USING step, so it compiles to the constant-stack C
// fold LOOP -- the same driver TestCARCSteadyLoop pins) runs n iterations, and
// EACH iteration builds a fresh 8-element list (8 saturated cons applications
// -> 8 direct mkcon K_CONs via satCtorDispatch), folds it back down with the
// eliminator-lowered `case … with ih` (~a dozen closure/bignum temporaries per
// element), and abandons the whole structure. Measured with gprof: ~83 heap
// objects allocated AND discarded per iteration, so the n=100000 run churns
// ~8.3e6 objects -- a leak-vs-release differential of >8e6 objects (~500MB if
// nothing released; the pre-fix emitter retained 1.6e6 objects at this size)
// while a correct walker keeps rt_live at a single-digit constant.
//
// Why not ONE deep list built and summed (the naive "build 1e6 cells then fold
// them")? Two lowering properties of this backend, not ARC properties: (a) a
// datatype `case` lowers through the ELIMINATOR, whose lowered cons arm
// computes the recursive IH eagerly -- combining that with a `partial`
// self-call in the arm doubles the work per level (2^n; the rune-strict-case
// hazard), and (b) eliminator recursion is not tail-call-optimized, so folding
// a 1e6-deep list overflows the native C stack. The loop-churn shape reaches
// the same >1e6-object pressure with O(1) stack and linear time.
const pressureChurnSrc = `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
addN : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) n (fn (k : Nat) (ih : Nat) is succ ih end) m
  end
end
builtin natAdd addN
mk8 : Nat -> List Nat is
  fn (k : Nat) is
    cons Nat k (cons Nat k (cons Nat k (cons Nat k
      (cons Nat k (cons Nat k (cons Nat k (cons Nat k (nil Nat))))))))
  end
end
len8 : List Nat -> Nat is
  fn (xs : List Nat) is
    case xs of | nil -> zero | cons x rest with ih -> succ ih end
  end
end
main : Nat is
  NatElim (fn (x : Nat) is Nat end) zero
    (fn (k : Nat) (ih : Nat) is addN (len8 (mk8 k)) ih end)
    %d
end
`

// TestCARCPressure is the pressure gate that REPLACES the retired tiny-heap
// mark-sweep tests (TestCGCForcesCollection / TestCGCConformanceUnderTinyHeap,
// deleted from codegen_test.go in this same change): under mark-sweep, forcing
// collection needed a tiny -DRUNE_GC_THRESHOLD; under ARC the release walker
// IS the collector, so the gate is simply: build-and-discard enough cells that
// leak-vs-release would differ by well over a million objects, and check the
// process completes with a flat, size-invariant rt_live. See pressureChurnSrc's
// doc comment for the object-count accounting (~8.3e6 objects churned at
// n=100000). Result = 8n (each iteration contributes len8(mk8 k) = 8).
func TestCARCPressure(t *testing.T) {
	outSmall, liveSmall := buildAndRunCWithReport(t, fmt.Sprintf(pressureChurnSrc, 2000), "main")
	outBig, liveBig := buildAndRunCWithReport(t, fmt.Sprintf(pressureChurnSrc, 100000), "main")
	if outSmall != "16000" || outBig != "800000" {
		t.Fatalf("churn fold wrong: %q (n=2000, want 16000), %q (n=100000, want 800000)", outSmall, outBig)
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("build-and-discard leaks under pressure: rt_live %d (n=2000) vs %d (n=100000) -- releases not firing at scale (a leaking run retains millions of objects)", liveSmall, liveBig)
	}
}

// cASANSupported probes the toolchain for AddressSanitizer support by
// compiling and running a trivial 3-line program with -fsanitize=address,
// per the Task 4 brief (t.Skip if unsupported rather than failing the suite
// on a toolchain that lacks the sanitizer runtime).
func cASANSupported(t *testing.T, cc string) bool {
	t.Helper()
	dir := t.TempDir()
	f := dir + "/probe.c"
	bin := dir + "/probe"
	if err := os.WriteFile(f, []byte("int main(void) {\n  return 0;\n}\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command(cc, "-fsanitize=address", "-o", bin, f).CombinedOutput(); err != nil {
		t.Logf("ASAN probe compile failed (toolchain lacks ASAN): %v\n%s", err, out)
		return false
	}
	if out, err := exec.Command(bin).CombinedOutput(); err != nil {
		t.Logf("ASAN probe run failed (toolchain lacks a working ASAN runtime): %v\n%s", err, out)
		return false
	}
	return true
}

// buildAndRunCWithASAN emits the C backend for `main`, compiles it with
// -fsanitize=address (no -DRUNE_ARC_REPORT -- ASAN's own instrumentation is
// the gate here, not the rt_live counter), runs it, and fails loudly on any
// sanitizer report (a double-free or use-after-free anywhere in the ARC
// walker/ownership protocol aborts the process with an "ERROR:
// AddressSanitizer" report on stderr). Returns trimmed stdout for a
// correctness check alongside the memory-safety one.
//
// LeakSanitizer is disabled (ASAN_OPTIONS=detect_leaks=0): leak BALANCE is
// pinned by the rt_live invariance gates above (which distinguish scaling
// leaks from constants), while LSan flags the deliberate constant end-of-
// process residuals -- e.g. the program's final result value, shown and then
// abandoned at exit -- as "leaks". UAF/double-free detection is unaffected:
// those abort mid-run regardless of the leak phase.
func buildAndRunCWithASAN(t *testing.T, cc, src, mainName string) string {
	t.Helper()
	csrc := emitWith(t, codegen.C{}, src, mainName)
	dir := t.TempDir()
	f := dir + "/main.c"
	bin := dir + "/main"
	if err := os.WriteFile(f, []byte(csrc), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command(cc, "-fsanitize=address", "-g", "-o", bin, f).CombinedOutput(); err != nil {
		t.Fatalf("%s -fsanitize=address: %v\n%s\n--- emitted ---\n%s", cc, err, out, csrc)
	}
	cmd := exec.Command(bin)
	cmd.Env = append(os.Environ(), "ASAN_OPTIONS=detect_leaks=0")
	var stdout, stderr strings.Builder
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	runErr := cmd.Run()
	if runErr != nil || strings.Contains(stderr.String(), "AddressSanitizer") ||
		strings.Contains(stderr.String(), "runtime error") {
		t.Fatalf("ASAN reported a memory error (run err=%v):\nstderr=%s", runErr, stderr.String())
	}
	return strings.TrimSpace(stdout.String())
}

// TestCARCUnderASAN (Task 4 Step 3) recompiles the pressure shape and the
// Task-3 fold-prim shape (the highest-risk apply-in-loop foreign body) under
// AddressSanitizer. rt_live invariance can only prove balanced COUNTS; ASAN
// independently proves every retain/release pairing in the walker and the
// prim bodies is memory-SAFE (no double-free, no use-after-free) by aborting
// the process the instant one occurs, rather than merely miscounting. Scaled
// down from the steady-state sizes (ASAN's instrumentation overhead is large)
// since this gate is about catching a memory error, not about measuring scale.
func TestCARCUnderASAN(t *testing.T) {
	cc := ""
	for _, cand := range []string{"cc", "gcc", "clang"} {
		if _, err := exec.LookPath(cand); err == nil {
			cc = cand
			break
		}
	}
	if cc == "" {
		t.Skip("no C compiler (cc/gcc/clang) in PATH")
	}
	if !cASANSupported(t, cc) {
		t.Skip("toolchain lacks AddressSanitizer support")
	}

	t.Run("pressure", func(t *testing.T) {
		out := buildAndRunCWithASAN(t, cc, fmt.Sprintf(pressureChurnSrc, 5000), "main")
		if out != "40000" {
			t.Fatalf("pressure shape under ASAN: got %q, want 40000", out)
		}
	})

	t.Run("foldLines", func(t *testing.T) {
		src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end
foreign foldLines : (S : U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign printNat  : Nat -> IO Nat end
step : Nat -> Nat -> IO Nat is
  fn (count : Nat) (line : Nat) is pureIO Nat (succ count) end
end
main : IO Nat is
  bindIO Nat Nat (foldLines Nat (codeOf "%s") step zero)
    (fn (n : Nat) is printNat n end)
end`
		dir := t.TempDir()
		p := dir + "/lines.txt"
		var b strings.Builder
		for i := 0; i < 50; i++ {
			b.WriteString("line\n")
		}
		if err := os.WriteFile(p, []byte(b.String()), 0o644); err != nil {
			t.Fatal(err)
		}
		out := buildAndRunCWithASAN(t, cc, fmt.Sprintf(src, p), "main")
		if out != "50\n50" {
			t.Fatalf("foldLines shape under ASAN: got %q, want \"50\\n50\"", out)
		}
	})
}

// packedDecodeSrc builds a String literal of the given repeated payload and
// returns its byte length via the d6 `byteLen` host op. byteLen decodes the
// packed-String bignum by repeated division-by-256 (d6_s2h) -- the exact
// small-divisor workload the fast path accelerates.
func packedDecodeSrc(payload string) string {
	return fmt.Sprintf(`data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data List : U -> U is nil : (A : U) -> List A | cons : (A : U) -> A -> List A -> List A end
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end
foreign byteLen : Nat -> Nat end
main : Nat is byteLen (codeOf "%s") end`, payload)
}

// runGoStdout emits the Go backend for `main`, runs it under `go run`, and
// returns trimmed stdout. It is the correctness oracle for the native backends:
// the go big.Int divmod is unaffected by the C small-divisor specialization, so
// its output is the reference the C/LL stdout must byte-match.
func runGoStdout(t *testing.T, src, mainName string) string {
	t.Helper()
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	gosrc := emitWith(t, codegen.Go{}, src, mainName)
	dir := t.TempDir()
	f := dir + "/main.go"
	if err := os.WriteFile(f, []byte(gosrc), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("go", "run", f).CombinedOutput()
	if err != nil {
		t.Fatalf("go run: %v\n%s", err, out)
	}
	return strings.TrimSpace(string(out))
}

// TestCARCPackedDecodeFast pins the small-divisor division specialization:
// decoding a ~1KB packed-String payload must complete in seconds, not the
// tens of seconds the general binary-search divmod takes (the v3.371.0
// scale-gate miss, profiled to d6_s2h -> big_divmod). Correctness is pinned
// by comparing the program's stdout against the go backend's for the same
// source; the ceiling pins the specialization.
func TestCARCPackedDecodeFast(t *testing.T) {
	const payloadBytes = 6000
	payload := strings.Repeat("abcdefgh", payloadBytes/8)
	src := packedDecodeSrc(payload)

	start := time.Now()
	got, _ := buildAndRunCWithReport(t, src, "main")
	elapsed := time.Since(start)
	t.Logf("packed-decode C build+run of %d-byte payload: %s, stdout=%q", len(payload), elapsed, got)

	if want := runGoStdout(t, src, "main"); got != want {
		t.Fatalf("c stdout %q != go stdout %q (byteLen mismatch)", got, want)
	}
	if elapsed > 10*time.Second {
		t.Fatalf("packed decode too slow: %s (> 10s ceiling) for a %d-byte payload -- the small-divisor divmod fast path is missing", elapsed, len(payload))
	}
}

// divmodCase is one (a, d) probe for the small-divisor divmod property test.
type divmodCase struct {
	a *big.Int // dividend, up to ~40 base-1e9 limbs
	d int64    // divisor, single base-1e9 limb: [1, 999999999]
}

// divmodSmallCases returns the fixed-seed case set for the divmod-small property
// tests: explicit limb-boundary edges (a = 0, a < d, a = d, a = 1e9^k exactly, the
// max single limb, d = 1, d = BIG_BASE-1) plus seeded-random multi-limb dividends
// spanning 1..40 limbs. Fixed seed => deterministic program + expected output.
func divmodSmallCases() []divmodCase {
	bigBase := big.NewInt(1000000000)
	pow := func(k int) *big.Int { return new(big.Int).Exp(bigBase, big.NewInt(int64(k)), nil) }
	cases := []divmodCase{
		{big.NewInt(0), 7},                 // a = 0
		{big.NewInt(5), 999999999},         // a < d (q=0, r=a)
		{big.NewInt(123456789), 123456789}, // a = d (q=1, r=0)
		{pow(1), 256},                      // a = 1e9 exactly (2-limb boundary)
		{pow(4), 1},                        // d = 1 (q=a, r=0), 5-limb boundary
		{new(big.Int).Sub(pow(2), big.NewInt(1)), 999999999}, // all-nines 2 limbs, max d
	}
	rng := rand.New(rand.NewSource(0x5eed))
	for i := 0; i < 6; i++ {
		nl := 1 + rng.Intn(40) // limb count 1..40
		a := big.NewInt(0)
		for j := 0; j < nl; j++ {
			limb := int64(rng.Intn(1000000000))
			if j == nl-1 && limb == 0 {
				limb = 1 // keep the top limb non-zero so a really spans nl limbs
			}
			a.Mul(a, bigBase)
			a.Add(a, big.NewInt(limb))
		}
		d := int64(1 + rng.Intn(999999999))
		cases = append(cases, divmodCase{a, d})
	}
	return cases
}

// divmodSmallPropertySrc builds ONE rune program (a single compile per backend)
// that prints divN a d and modN a d for every case, with divN/modN registered as
// the natDiv/natMod builtins (the ch501 shape) so each call accel-dispatches to
// the runtime's nat_div/nat_mod -> big_divmod -> the small-divisor fast path
// (every d here is a single base-1e9 limb). The fuel-bounded Euclidean fallback
// bodies exist only to type the builtins; the accel replaces them.
func divmodSmallPropertySrc(cases []divmodCase) (src, want string) {
	var b strings.Builder
	b.WriteString(`data Bool : U is false : Bool | true : Bool end
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
foreign printNat : Nat -> IO Nat end
pred : Nat -> Nat is fn (n : Nat) is case n of | zero -> zero | succ m -> m end end end
addN : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a end
end
mulN : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is addN b ih end) a end
end
monus : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat -> Nat end) (fn (x : Nat) is x end)
      (fn (k : Nat) (ih : Nat -> Nat) is fn (x : Nat) is pred (ih x) end end) b a
  end
end
leb : Nat -> Nat -> Bool is fn (a : Nat) (b : Nat) is case monus a b of | zero -> true | succ k -> false end end end
divModGo : Nat -> Nat -> Nat -> Nat is
  fn (fuel : Nat) is
    NatElim (fn (x : Nat) is Nat -> Nat -> Nat end) (fn (a : Nat) (b : Nat) is 0 end)
      (fn (f : Nat) (ih : Nat -> Nat -> Nat) is
         fn (a : Nat) (b : Nat) is case leb b a of | true -> succ (ih (monus a b) b) | false -> 0 end end
      end)
      fuel
  end
end
divN : Nat -> Nat -> Nat is fn (a : Nat) (b : Nat) is case b of | zero -> 0 | succ k -> divModGo a a b end end end
modN : Nat -> Nat -> Nat is fn (a : Nat) (b : Nat) is monus a (mulN (divN a b) b) end end
builtin natAdd addN
builtin natMul mulN
builtin natMonus monus
builtin natDiv divN
builtin natMod modN

main : IO Nat is
`)
	// One bindIO chain printing q then r per case; the last print is the chain tail.
	var lines []string
	depth := 0
	emit := func(expr string, last bool) {
		if last {
			b.WriteString(strings.Repeat(" ", depth+2) + "printNat (" + expr + ")\n")
			return
		}
		b.WriteString(strings.Repeat(" ", depth+2) + fmt.Sprintf("bindIO Nat Nat (printNat (%s)) (fn (x%d : Nat) is\n", expr, depth))
		depth++
	}
	for i, c := range cases {
		q, r := new(big.Int).QuoRem(c.a, big.NewInt(c.d), new(big.Int))
		// The oracle: q*d + r == a and 0 <= r < d hold for big.Int QuoRem by
		// construction; matching the printed q and r against it pins the runtime.
		lines = append(lines, q.String(), r.String())
		emit(fmt.Sprintf("divN %s %d", c.a.String(), c.d), false)
		emit(fmt.Sprintf("modN %s %d", c.a.String(), c.d), i == len(cases)-1)
	}
	for ; depth > 0; depth-- {
		b.WriteString(strings.Repeat(" ", depth+1) + "end)\n")
	}
	b.WriteString("end\n")
	// The runtime shows main's final value after the IO chain runs (the same
	// convention as the foldLines gate's "50\n50"), so the last modN result
	// appears once from its printNat and once as the program value.
	lines = append(lines, lines[len(lines)-1])
	return b.String(), strings.Join(lines, "\n")
}

// TestCARCDivmodSmallProperty is the randomized invariant gate for the new
// small-divisor big_divmod fast path: 12 fixed-seed cases (multi-limb dividends
// spanning 1..40 base-1e9 limbs incl exact limb-boundary values, single-limb
// divisors in [1, 999999999]) compiled into ONE C program, its printed q/r
// checked against Go's math/big QuoRem oracle (which guarantees q*d + r == a
// and r < d). Divisors are all <= 1 limb, so every division takes the new path.
func TestCARCDivmodSmallProperty(t *testing.T) {
	src, want := divmodSmallPropertySrc(divmodSmallCases())
	got, _ := buildAndRunCWithReport(t, src, "main")
	if got != want {
		t.Fatalf("c divmod-small property mismatch vs math/big oracle:\ngot:\n%s\nwant:\n%s", got, want)
	}
}
