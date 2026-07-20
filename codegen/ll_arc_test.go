package codegen_test

// ll_arc_test.go -- native ARC balance gates for the LLVM backend (Plan 6e Task 2,
// the LL mirror of c_arc_test.go). buildAndRunLLWithReport compiles the emitted .ll
// + the linked C runtime with clang under -DRUNE_ARC_REPORT (so the atexit hook
// prints "rt_live=N" to stderr) and returns (stdout, rt_live). The balance tests
// assert the retained heap does NOT scale with the work done: Perceus releases fire
// through the shared annotated IR, so a fold / trampoline over a small vs a large
// size leaves the same (constant) number of live objects. Task 3 reuses the helper.
// Modeled on ll_test.go's runLL (clang lookup + skip + temp dir + link) and
// c_arc_test.go's buildAndRunCWithReport (the -DRUNE_ARC_REPORT + rt_live parse).

import (
	"fmt"
	"math/big"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"testing"
	"time"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/session"
)

// llProgramFor loads `src` and emits the codegen.Program for `mainName` -- the LL
// twin needs the actual Program (not just the emitted .ll text) so it can call
// EmitRuntimeFor(p), which gates in the foreign-prim host bodies (foldLines/byteLen/
// Bin/…) the emitWith+EmitRuntime() combo omits. Mirrors harness's loadListing +
// s.EmitProgram pattern (e.g. harness/bytes_test.go:20).
func llProgramFor(t *testing.T, src, mainName string) codegen.Program {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	p, err := s.EmitProgram(mainName)
	if err != nil {
		t.Fatal(err)
	}
	return p
}

// buildAndRunLLWithReport emits the LLVM backend for `main`, links the emitted .ll
// with the C runtime (EmitRuntimeFor, so foreign-prim host bodies a shape needs --
// foldLines/byteLen/Bin/etc -- are included exactly when the program uses them)
// under -DRUNE_ARC_REPORT, runs the binary, and returns (trimmed stdout, rt_live).
// It skips gracefully when clang is absent. The .ll itself carries no preprocessor
// macros (it is IR); -DRUNE_ARC_REPORT applies to the runtime.c TU, which is where
// arc_report + the atexit hook live (llRuntimeC / llRuntimeMain, all EmitRuntimeFor
// main variants register it).
func buildAndRunLLWithReport(t *testing.T, src, mainName string) (string, int64) {
	t.Helper()
	if _, err := exec.LookPath("clang"); err != nil {
		t.Skip("clang not in PATH")
	}
	p := llProgramFor(t, src, mainName)
	llOut, err := codegen.LL{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	llsrc := string(llOut)
	dir := t.TempDir()
	llf := dir + "/program.ll"
	rtf := dir + "/runtime.c"
	bin := dir + "/main"
	if err := os.WriteFile(llf, []byte(llsrc), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(rtf, []byte(codegen.LL{}.EmitRuntimeFor(p)), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command("clang", "-DRUNE_ARC_REPORT", llf, rtf, "-o", bin).CombinedOutput(); err != nil {
		t.Fatalf("clang: %v\n%s\n--- emitted .ll ---\n%s", err, out, llsrc)
	}
	cmd := exec.Command(bin)
	var stdout, stderr strings.Builder
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run: %v\nstderr=%s", err, stderr.String())
	}
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

// TestLLARCSteadyLoop is the LL twin of TestCARCSteadyLoop: a datatype-eliminator
// fold (NatElim over builtin nat whose step USES its induction hypothesis, so it runs
// the b3 fold LOOP -- not the accel one-peel) at two very different depths, checking
// the retained heap does not scale with the work done. The b3 fold retains its
// accumulator and releases the per-iteration counter / succ / step-closure
// temporaries; a leak would make the n=5000 run's rt_live grow ~100x over the n=50
// run. Fails before the Perceus wiring (nothing annotated -> pure leak, the interim
// ladder), passes once the pass + the b3 ARC protocol are wired.
func TestLLARCSteadyLoop(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
double : Nat -> Nat is
  fn (n : Nat) is case n of | zero -> zero | succ k with ih -> succ (succ ih) end end
end
main : Nat is double %d end`
	_, liveSmall := buildAndRunLLWithReport(t, fmt.Sprintf(src, 50), "main")
	_, liveBig := buildAndRunLLWithReport(t, fmt.Sprintf(src, 5000), "main")
	if liveBig-liveSmall > 64 {
		t.Fatalf("retained heap scales with work: rt_live %d (n=50) vs %d (n=5000) - releases not firing", liveSmall, liveBig)
	}
}

// TestLLARCPartialTrampolineSteady is the reviewer-mandated LL trampoline balance
// gate (Task 1's LL coverage rode only on ch39 conformance): the ch39-style
// partial-recursive countdown recurses via CBounce through the native trampoline
// (rt_tramp). Under ARC the bounce OWNS its args, rt_tramp shell-frees each spent
// bounce and releases every intermediate apply-closure, and the cached _step head is
// borrowed -- so driving a countdown of depth n must leave rt_live flat regardless of
// n. A tramp that leaked its per-iteration intermediates (Task 1's leak-only rt_tramp)
// or double-freed the borrowed step would fail this at scale. countdown n = zero.
func TestLLARCPartialTrampolineSteady(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
partial countdown : Nat -> Nat is
  fn (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero
      (fn (k : Nat) (ih : Nat) is countdown k end)
      n
  end
end
main : Nat is countdown %d end`
	outSmall, liveSmall := buildAndRunLLWithReport(t, fmt.Sprintf(src, 50), "main")
	outBig, liveBig := buildAndRunLLWithReport(t, fmt.Sprintf(src, 5000), "main")
	// countdown n = zero; the builtin-nat rep shows zero as the bignum "0".
	if outSmall != "0" || outBig != "0" {
		t.Fatalf("countdown wrong: %q (n=50), %q (n=5000), want 0", outSmall, outBig)
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("trampoline retained heap scales with depth: rt_live %d (n=50) vs %d (n=5000) -- rt_tramp must shell-free each bounce and release its intermediates", liveSmall, liveBig)
	}
}

// ============================================================================
// Task 3: the LL twin of c_arc_test.go's Task-4 ARC gate family -- the same
// rune source programs, the same two-size rt_live-invariance assertions with
// the 64-object tolerance, run through buildAndRunLLWithReport instead of
// buildAndRunCWithReport. See each C twin's doc comment (c_arc_test.go) for
// the full shape rationale; comments here note only LL-specific deltas.
// ============================================================================

// TestLLARCRecursiveRelease is the LL twin of TestCARCRecursiveRelease: a
// constructor CHAIN (a List Nat built to length n, each cons cell holding a
// bignum) must have its ENTIRE chain recursively released when consumed, not
// just its own cell -- if K_CON release only freed the immediate cell (not
// recursing into `rest`), every cons cell and its held Nat would leak and
// rt_live would scale with n.
func TestLLARCRecursiveRelease(t *testing.T) {
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
	outSmall, liveSmall := buildAndRunLLWithReport(t, fmt.Sprintf(src, 50), "main")
	outBig, liveBig := buildAndRunLLWithReport(t, fmt.Sprintf(src, 5000), "main")
	if outSmall != "50" || outBig != "5000" {
		t.Fatalf("length(replicate n) wrong: %q (n=50), %q (n=5000)", outSmall, outBig)
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("recursive release through a cons chain leaks: rt_live %d (n=50) vs %d (n=5000) -- each cons cell (and its held bignum) must be freed as the chain is consumed", liveSmall, liveBig)
	}
}

// TestLLARCPairBalance is the LL twin of TestCARCPairBalance: a Sig pair (the
// outer core Σ, C1/R-SUM) whose two halves hold live values must have BOTH
// halves released when the pair is released. Each fold iteration builds a
// fresh K_PAIR holding two references to the loop counter k and passes it to
// `dropP`, whose body ignores it -- Perceus CDrops the dead owned arg, so the
// K_PAIR release branch must recursively free both slots or the per-iteration
// pairs accumulate and rt_live scales with n.
//
// KNOWN RESIDUAL (documented, not gated here -- inherited from C per
// constraints.md's shared-residuals note): a runtime PROJECTION of a pair
// (`Fst p`/`Snd p`) leaks the projected-from pair; this test pins the walker
// (release of a whole dead pair), which is what TestCARCPairBalance pins.
func TestLLARCPairBalance(t *testing.T) {
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
	outSmall, liveSmall := buildAndRunLLWithReport(t, fmt.Sprintf(src, 50), "main")
	outBig, liveBig := buildAndRunLLWithReport(t, fmt.Sprintf(src, 5000), "main")
	if outSmall != "50" || outBig != "5000" {
		t.Fatalf("pair-fold wrong: %q (n=50), %q (n=5000)", outSmall, outBig)
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("pair release leaks: rt_live %d (n=50) vs %d (n=5000) -- releasing a dead K_PAIR must free both slots", liveSmall, liveBig)
	}
}

// TestLLARCClosureEnvBalance is the LL twin of TestCARCClosureEnvBalance: a
// closure capturing a bignum in its environment must have that env slot
// released when the closure is released. `mkAdder k` builds a fresh K_CLO
// capturing the loop counter k (a bignum) each iteration; the closure is
// applied once and then dead -- if K_CLO's env-release loop skipped a slot,
// each iteration's captured k would leak and rt_live would scale with n.
func TestLLARCClosureEnvBalance(t *testing.T) {
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
	outSmall, liveSmall := buildAndRunLLWithReport(t, fmt.Sprintf(src, 50), "main")
	outBig, liveBig := buildAndRunLLWithReport(t, fmt.Sprintf(src, 5000), "main")
	if outSmall != sumBelow(50) || outBig != sumBelow(5000) {
		t.Fatalf("closure-env fold wrong: %q (n=50, want %s), %q (n=5000, want %s)", outSmall, sumBelow(50), outBig, sumBelow(5000))
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("closure env release leaks a captured bignum: rt_live %d (n=50) vs %d (n=5000) -- releasing a K_CLO must free every env slot", liveSmall, liveBig)
	}
}

// TestLLARCBignumBalance is the LL twin of TestCARCBignumBalance: GENUINE
// multi-limb bignum arithmetic (K_BIG is base-1e9 per limb, so a value at or
// above 1e18 needs 3+ limbs) at scale -- a seed of 1e18 added n times via the
// kernel-accelerated natAdd. rt_live must stay flat between n=50 and n=5000
// even though the intermediate/final bignums genuinely grow into extra limbs;
// the decimal result (checked against math/big) proves the arithmetic itself
// is correct at both scales, not just its ARC bookkeeping.
func TestLLARCBignumBalance(t *testing.T) {
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
	outSmall, liveSmall := buildAndRunLLWithReport(t, fmt.Sprintf(src, 50), "main")
	outBig, liveBig := buildAndRunLLWithReport(t, fmt.Sprintf(src, 5000), "main")
	if outSmall != expect(50) || outBig != expect(5000) {
		t.Fatalf("bignum fold wrong: %q (n=50, want %s), %q (n=5000, want %s)", outSmall, expect(50), outBig, expect(5000))
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("multi-limb bignum arithmetic leaks: rt_live %d (n=50) vs %d (n=5000)", liveSmall, liveBig)
	}
}

// TestLLARCBytesBalance is the LL twin of TestCARCBytesBalance: K_BYTES (the
// Phase-0 `Bin` real-byte string) is a LEAF for the free walker, so its
// balance risk is pure alloc/free churn rather than recursive release.
// `binCons` prepends a byte by allocating a FRESH, one-byte-longer Bin and
// releasing the old one (emitBinPrimsLL) -- building a Bin of length n via n
// chained binCons calls must leave only the FINAL Bin live, not one per
// intermediate length.
func TestLLARCBytesBalance(t *testing.T) {
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
	outSmall, liveSmall := buildAndRunLLWithReport(t, fmt.Sprintf(src, 50), "main")
	outBig, liveBig := buildAndRunLLWithReport(t, fmt.Sprintf(src, 5000), "main")
	if outSmall != "50" || outBig != "5000" {
		t.Fatalf("binLen(buildBin n) wrong: %q (n=50), %q (n=5000)", outSmall, outBig)
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("Bin churn leaks intermediate buffers: rt_live %d (n=50) vs %d (n=5000) -- binCons must release the buffer it grows past", liveSmall, liveBig)
	}
}

// llPressureChurnSrc is the LL twin of c_arc_test.go's pressureChurnSrc: an
// outer NatElim b3 fold (an IH-USING step, so it compiles to the constant-
// stack fold LOOP) runs n iterations, and EACH iteration builds a fresh
// 8-element list, folds it back down with the eliminator-lowered `case …
// with ih`, and abandons the whole structure. This is the loop-churn shape,
// NOT a deep list: a deep list overflows the native stack (eliminator
// recursion is not tail-call-optimized) and, combined with a `partial`
// self-call in a cons arm, would double the work per level (2^n, the
// rune-strict-case hazard) -- see c_arc_test.go's pressureChurnSrc doc
// comment for the full accounting. The loop-churn shape reaches the same
// >1e6-object pressure with O(1) stack and linear time.
const llPressureChurnSrc = `data Nat : U is zero : Nat | succ : Nat -> Nat end
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

// TestLLARCPressure is the LL twin of TestCARCPressure, the pressure gate
// that REPLACES the retired tiny-heap TestLLGCConformanceUnderTinyHeap
// (deleted from ll_test.go in this same change): under mark-sweep, forcing
// collection needed a tiny -DRUNE_GC_THRESHOLD; under ARC the release walker
// IS the collector, so the gate is simply build-and-discard enough cells that
// leak-vs-release would differ by well over a million objects, and check the
// process completes with a flat, size-invariant rt_live.
func TestLLARCPressure(t *testing.T) {
	outSmall, liveSmall := buildAndRunLLWithReport(t, fmt.Sprintf(llPressureChurnSrc, 2000), "main")
	outBig, liveBig := buildAndRunLLWithReport(t, fmt.Sprintf(llPressureChurnSrc, 100000), "main")
	if outSmall != "16000" || outBig != "800000" {
		t.Fatalf("churn fold wrong: %q (n=2000, want 16000), %q (n=100000, want 800000)", outSmall, outBig)
	}
	if liveBig-liveSmall > 64 {
		t.Fatalf("build-and-discard leaks under pressure: rt_live %d (n=2000) vs %d (n=100000) -- releases not firing at scale (a leaking run retains millions of objects)", liveSmall, liveBig)
	}
}

// asanExecCommand builds the command used to run an ASAN-instrumented binary.
// Under WSL2 (this sandbox's host), ASan's fixed low-memory shadow region can
// collide with an ASLR-randomized mapping early enough in process start-up
// that ASan itself never gets to report anything -- the process just SIGSEGVs
// (reproduced independently: a trivial `int main(void){return 0;}` compiled
// with -fsanitize=address segfaults on ~1/4 of runs on this host, entirely
// unrelated to any program logic). Disabling ASLR for the child via `setarch
// -R` (util-linux, present on essentially every Linux host) eliminates the
// collision deterministically (0/100 in local reproduction, vs ~1/4 with
// ASLR on) without weakening the sanitizer itself -- ASan's instrumentation
// and error detection are unaffected by address-space layout randomization;
// only the flaky pre-report crash is. Falls back to running the binary
// directly when `setarch` is unavailable (e.g. non-Linux).
func asanExecCommand(bin string, args ...string) *exec.Cmd {
	if _, err := exec.LookPath("setarch"); err == nil {
		return exec.Command("setarch", append([]string{"-R", bin}, args...)...)
	}
	return exec.Command(bin, args...)
}

// llASANSupported probes the toolchain for AddressSanitizer support (the LL
// twin of cASANSupported): clang is already required for the LL backend, so
// this only checks the sanitizer runtime actually works.
func llASANSupported(t *testing.T) bool {
	t.Helper()
	dir := t.TempDir()
	f := dir + "/probe.c"
	bin := dir + "/probe"
	if err := os.WriteFile(f, []byte("int main(void) {\n  return 0;\n}\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command("clang", "-fsanitize=address", "-o", bin, f).CombinedOutput(); err != nil {
		t.Logf("ASAN probe compile failed (toolchain lacks ASAN): %v\n%s", err, out)
		return false
	}
	if out, err := asanExecCommand(bin).CombinedOutput(); err != nil {
		t.Logf("ASAN probe run failed (toolchain lacks a working ASAN runtime): %v\n%s", err, out)
		return false
	}
	return true
}

// buildAndRunLLWithASAN is the LL twin of buildAndRunCWithASAN: emits the LLVM
// backend for `main`, compiles+links the .ll and the C runtime (EmitRuntimeFor,
// so any foreign prim the shape needs is included) with -fsanitize=address (no
// -DRUNE_ARC_REPORT -- ASAN's own instrumentation is the gate), runs it, and
// fails loudly on any sanitizer report. Returns trimmed stdout for a
// correctness check alongside the memory-safety one. LeakSanitizer is
// disabled (ASAN_OPTIONS=detect_leaks=0) for the same reason as the C twin:
// leak BALANCE is pinned by the rt_live invariance gates above, while LSan
// flags the deliberate constant end-of-process residual (the shown result)
// as a "leak"; UAF/double-free detection aborts mid-run regardless.
func buildAndRunLLWithASAN(t *testing.T, src, mainName string) string {
	t.Helper()
	p := llProgramFor(t, src, mainName)
	llOut, err := codegen.LL{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	llsrc := string(llOut)
	dir := t.TempDir()
	llf := dir + "/program.ll"
	rtf := dir + "/runtime.c"
	bin := dir + "/main"
	if err := os.WriteFile(llf, []byte(llsrc), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(rtf, []byte(codegen.LL{}.EmitRuntimeFor(p)), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command("clang", "-fsanitize=address", "-g", llf, rtf, "-o", bin).CombinedOutput(); err != nil {
		t.Fatalf("clang -fsanitize=address: %v\n%s\n--- emitted .ll ---\n%s", err, out, llsrc)
	}
	cmd := asanExecCommand(bin)
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

// TestLLARCUnderASAN is the LL twin of TestCARCUnderASAN: recompiles the
// pressure shape and the fold-prim shape (the highest-risk apply-in-loop
// foreign body) under AddressSanitizer. rt_live invariance can only prove
// balanced COUNTS; ASAN independently proves every retain/release pairing in
// the walker and the prim bodies is memory-SAFE (no double-free, no
// use-after-free). Scaled down from the steady-state sizes (ASAN's
// instrumentation overhead is large) since this gate is about catching a
// memory error, not about measuring scale.
func TestLLARCUnderASAN(t *testing.T) {
	if _, err := exec.LookPath("clang"); err != nil {
		t.Skip("clang not in PATH")
	}
	if !llASANSupported(t) {
		t.Skip("toolchain lacks AddressSanitizer support")
	}

	t.Run("pressure", func(t *testing.T) {
		out := buildAndRunLLWithASAN(t, fmt.Sprintf(llPressureChurnSrc, 5000), "main")
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
		out := buildAndRunLLWithASAN(t, fmt.Sprintf(src, p), "main")
		if out != "50\n50" {
			t.Fatalf("foldLines shape under ASAN: got %q, want \"50\\n50\"", out)
		}
	})
}

// TestLLARCPackedDecodeFast is the LL twin of TestCARCPackedDecodeFast: the same
// ~6KB packed-String decode must clear the seconds ceiling once the small-divisor
// big_divmod_small fast path is present in llRuntimeC. Correctness oracle is the
// go backend's stdout for the same source (its big.Int divmod is unaffected).
func TestLLARCPackedDecodeFast(t *testing.T) {
	const payloadBytes = 6000
	payload := strings.Repeat("abcdefgh", payloadBytes/8)
	src := packedDecodeSrc(payload)

	start := time.Now()
	got, _ := buildAndRunLLWithReport(t, src, "main")
	elapsed := time.Since(start)
	t.Logf("packed-decode LL build+run of %d-byte payload: %s, stdout=%q", len(payload), elapsed, got)

	if want := runGoStdout(t, src, "main"); got != want {
		t.Fatalf("ll stdout %q != go stdout %q (byteLen mismatch)", got, want)
	}
	if elapsed > 10*time.Second {
		t.Fatalf("packed decode too slow: %s (> 10s ceiling) for a %d-byte payload -- the small-divisor divmod fast path is missing", elapsed, len(payload))
	}
}

// TestLLARCDivmodSmallProperty is the LL twin of TestCARCDivmodSmallProperty:
// the same fixed-seed multi-limb/small-divisor case set (one compiled program),
// printed q/r checked against Go's math/big QuoRem oracle.
func TestLLARCDivmodSmallProperty(t *testing.T) {
	src, want := divmodSmallPropertySrc(divmodSmallCases())
	got, _ := buildAndRunLLWithReport(t, src, "main")
	if got != want {
		t.Fatalf("ll divmod-small property mismatch vs math/big oracle:\ngot:\n%s\nwant:\n%s", got, want)
	}
}
