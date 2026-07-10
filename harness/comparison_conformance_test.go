package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// comparisonCorpusSrc is the v4 Ord Plan C, Task 5 acceptance corpus. It is
// loaded ON TOP OF the REAL shared prelude (prelude.Source(), the same source
// the REPL and the CLI load - see internal/repl/prelude.go), so it exercises
// the SHIPPED `lower leb to leW by lebEquiv` directive (Task 4) exactly as
// deployed code sees it: `ordWhole`'s `le` slot is `leb`, and any top-level
// reference to `leb` reachable from `main` is redirected to `leW` at the
// shared Erase choke point (codegen/ir.go) before emission, on every backend.
//
// It exercises all three Ord/DecEq views (le, compare, eqb) across the tower:
//   - Whole: boundary (0<=0), interior (3<=5), reverse (5<=3, expect false),
//     a 10^18-scale comparison (the case an unredirected recursive leb would
//     make O(n) - a ~10^18-deep succ recursion - instead of O(1) via leW's
//     natMonus-accelerated subW), one `compare` (Ordering) call, one `eqb`
//     (DecEq) call.
//   - Int: a negative/nonneg cross comparison (negsucc vs nonneg).
//   - Frac: two NON-CANONICAL representatives of the same rational (1/2 and
//     2/4, built directly from QPair so no reduction happens before the
//     comparison), checked with both `le` and `eqb` - the quotient descent
//     must agree regardless of representative.
//
// Every sub-check is folded into one Bool (`and`-chain) so the byte-identical
// assertion across backends is a single "true" - any backend that diverges on
// ANY sub-check fails the whole gate. `ordCompareRaw` additionally prints the
// raw Ordering constructor directly (want "lt"), so a backend-specific bug in
// showing a 3-constructor enum (as opposed to Bool's 2) would also be caught.
//
// The Whole/Int checks (`main`) are what the redirect actually targets
// (ordWhole's `le` slot IS `leb`) and are asserted across the FULL 9-way
// bibleBackends() list, WASM included. The Frac non-canonical checks
// (`mainFrac`) are asserted separately, EXCLUDING wasm: the WASM backend has
// no Quot/qlift support at all today (codegen/wasm.go has no `qlift` runtime
// primitive, unlike every other backend - see js.go/py.go/golang.go/rust.go/
// beam.go/jvm.go/c.go/ll_runtime.go, which each emit one). This is a
// pre-existing gap (confirmed: codegen/wasm.go is untouched by this branch's
// Tasks 1-4, `git log 90b5797..HEAD -- codegen/wasm.go` is empty) predating
// Plan C entirely and unrelated to the leb->leW redirect; ANY Frac
// equality/order program would fail the same way on WASM today, with or
// without this feature. Excluding wasm from mainFrac's assertion is not a
// weakening of THIS gate - Plan C touches only `leb`, never `Quot`/`qlift`.
const comparisonCorpusSrc = `
wholeBoundary : Bool is leOf Whole ordWhole 0 0 end
wholeInterior : Bool is leOf Whole ordWhole 3 5 end
wholeReverse  : Bool is notB (leOf Whole ordWhole 5 3) end
wholeLarge    : Bool is leOf Whole ordWhole 1000000000000000000 1000000000000000001 end
wholeCompare  : Bool is
  case compareOf Whole ordWhole 3 5 of
  | lt -> true
  | eq -> false
  | gt -> false
  end
end
wholeEqb : Bool is eqbOf Whole decEqWhole 4 4 end

intNegative : Bool is leOf Int ordInt (negsucc 1) (nonneg 1) end

main : Bool is
  and wholeBoundary
  (and wholeInterior
  (and wholeReverse
  (and wholeLarge
  (and wholeCompare
  (and wholeEqb intNegative)))))
end

ordCompareRaw : Ordering is compareOf Whole ordWhole 3 5 end

fracA : Frac is qin QPair QRel (qpair (nonneg 1) 1) end
fracB : Frac is qin QPair QRel (qpair (nonneg 2) 3) end
fracNonCanonicalLe  : Bool is leOf Frac ordFrac fracA fracB end
fracNonCanonicalEqb : Bool is eqbOf Frac decEqFrac fracA fracB end

mainFrac : Bool is and fracNonCanonicalLe fracNonCanonicalEqb end
`

// runComparisonBackend emits+runs mainName on one backend in dir (cwd=dir),
// loading the corpus on top of the shared prelude. Mirrors runBibleBackend's
// shape (bible_conformance_test.go) but the session source is
// prelude.Source()+comparisonCorpusSrc, not a listing file loaded raw - the
// point of this gate is the LIVE prelude redirect, not a self-contained
// mirror of it. Skips (ok=false) if the backend's toolchain is absent.
func runComparisonBackend(t *testing.T, bk bibleBackend, mainName, dir string) (string, bool) {
	t.Helper()
	if _, err := exec.LookPath(bk.bin); err != nil {
		return "", false
	}
	s := session.New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	if _, err := s.LoadSource(comparisonCorpusSrc); err != nil {
		t.Fatalf("loading comparison corpus: %v", err)
	}
	p, err := s.EmitProgram(mainName)
	if err != nil {
		t.Fatalf("[%s] EmitProgram(%s): %v", bk.name, mainName, err)
	}
	src, err := bk.emit(p)
	if err != nil {
		t.Fatalf("[%s] emit: %v", bk.name, err)
	}
	f := filepath.Join(dir, "main."+bk.ext)
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
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
		if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
			t.Fatalf("[%s] compile failed: %v\n%s\n--- emitted ---\n%s", bk.name, err, out, src)
		}
		runFile = bin
	}
	cmd := bk.run(runFile)
	cmd.Dir = dir
	out, err := cmd.Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("[%s] run failed: %v\n%s", bk.name, err, stderr)
	}
	return strings.TrimSpace(string(out)), true
}

// assertComparisonAgree runs mainName on every backend (bibleBackends(), the
// existing 9-backend list: js/go/py/rs/erl + jvm/c/ll/wasm each gated on its
// own toolchain - see bible_conformance_test.go) in its own temp dir and
// asserts they all produce `want`. A missing toolchain is reported via
// t.Skipf (not a false pass), naming which backend(s) were unavailable - the
// SAME convention TestBibleConformance* already uses; this does not add a
// new skip convention.
func assertComparisonAgree(t *testing.T, mainName, want string) {
	t.Helper()
	assertComparisonAgreeExcept(t, mainName, want, nil)
}

// assertComparisonAgreeExcept is assertComparisonAgree but skips any backend
// named in exclude outright (logged, not counted as a missing-toolchain
// skip) - used only for the documented pre-existing WASM Quot/qlift gap
// (see the comparisonCorpusSrc doc comment above `mainFrac`), never to paper
// over an actual divergence introduced by this feature.
func assertComparisonAgreeExcept(t *testing.T, mainName, want string, exclude map[string]bool) {
	t.Helper()
	var skipped []string
	for _, bk := range bibleBackends() {
		if exclude[bk.name] {
			t.Logf("[%s] excluded from %s (documented pre-existing gap, not this feature)", bk.name, mainName)
			continue
		}
		dir := t.TempDir()
		got, ok := runComparisonBackend(t, bk, mainName, dir)
		if !ok {
			t.Logf("[%s] skipped (%s not in PATH)", bk.name, bk.bin)
			skipped = append(skipped, bk.name)
			continue
		}
		if got != want {
			t.Errorf("[%s] %s = %q, want %q (backends must not diverge)", bk.name, mainName, got, want)
		}
	}
	if len(skipped) > 0 {
		t.Skipf("comparison conformance gate inconclusive -- missing backend toolchain(s): %v", skipped)
	}
}

// TestComparisonConformance is the v4 Ord Plan C Task 5 acceptance gate: the
// tower's le/compare/eqb views, evaluated through the LIVE prelude redirect,
// must agree byte-for-byte across every available backend of the 9
// (js/py/go/rs/erl/jvm/c/ll/wasm) for the Whole/Int checks that the redirect
// actually targets, and across the 8 non-WASM backends for the Frac
// non-canonical checks (WASM has no Quot/qlift support - see the doc comment
// on comparisonCorpusSrc).
func TestComparisonConformance(t *testing.T) {
	assertComparisonAgree(t, "main", "true")
	assertComparisonAgree(t, "ordCompareRaw", "lt")
	assertComparisonAgreeExcept(t, "mainFrac", "true", map[string]bool{"wasm": true})
}
