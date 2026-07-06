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
	"os"
	"os/exec"
	"strconv"
	"strings"
	"testing"

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
