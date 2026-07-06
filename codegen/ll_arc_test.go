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
	"os"
	"os/exec"
	"strconv"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// buildAndRunLLWithReport emits the LLVM backend for `main`, links the emitted .ll
// with the C runtime under -DRUNE_ARC_REPORT, runs the binary, and returns (trimmed
// stdout, rt_live). It skips gracefully when clang is absent. The .ll itself carries
// no preprocessor macros (it is IR); -DRUNE_ARC_REPORT applies to the runtime.c TU,
// which is where arc_report + the atexit hook live (llRuntimeC / llRuntimeMain).
func buildAndRunLLWithReport(t *testing.T, src, mainName string) (string, int64) {
	t.Helper()
	if _, err := exec.LookPath("clang"); err != nil {
		t.Skip("clang not in PATH")
	}
	llsrc := emitWith(t, codegen.LL{}, src, mainName)
	dir := t.TempDir()
	llf := dir + "/program.ll"
	rtf := dir + "/runtime.c"
	bin := dir + "/main"
	if err := os.WriteFile(llf, []byte(llsrc), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(rtf, []byte(codegen.LL{}.EmitRuntime()), 0o644); err != nil {
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
