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

// numericParityCorpusSrc is the v4 Float Track A, Task 2 acceptance corpus. It
// is loaded ON TOP OF the REAL shared prelude (prelude.Source(), the same
// source the REPL and the CLI load), exercising ALL FOUR numeric tower types
// (Whole/Int/Frac/Float) with arithmetic AND comparison via their shipped
// Semiring/SubR/Ord/DecEq instances.
//
// Float is the point of this gate: it now carries guarded-tier Semiring/SubR/
// Ord/DecEq instances (Std.Float.semiringFloat/subRFloat/ordFloat/decEqFloat,
// internal/prelude/prelude.rune ~8720) with NO laws records (NaN breaks Ord
// reflexivity/totality; float + is not associative). Float ARITHMETIC is
// verified BY COMPARISON (feq), never by printing a float - printFloat
// display parity is a separate, later Track B; this gate stays entirely in
// the discrete (Bool) tag, matching every other check here.
//
//   - Whole: addW via decEqWhole's eqb view, and leOf via ordWhole.
//   - Int:   leOf via ordInt across a negative/nonneg boundary (negsucc vs
//     nonneg), the case the tower's guarded Int instances exist for.
//   - Frac:  leOf via ordFrac on two NON-CANONICAL QPair representatives of
//     the same rational (qpair i d denotes i/(d+1), so (nonneg 1, d=1) and
//     (nonneg 2, d=3) both denote 1/2, mirroring comparison_conformance_test.
//     go's fracA/fracB shape) - the quotient descent must agree regardless
//     of representative.
//   - Float: fadd/fmul checked by Std.Float.feq (never printed), fle, and
//     isNaN of the canonical NaN witness fdiv 0 0. All values are built from
//     fromNat + arithmetic (no FloatLit, no decimal literals).
//
// Every sub-check folds into one Bool (`and`-chain) so the byte-identical
// assertion across backends is a single "true" - any backend that diverges
// on ANY sub-check fails the whole gate.
//
// The Whole/Int/Float checks (`main`) run the FULL 9-way bibleBackends() list,
// WASM included - Float parity across all 9 backends is the point of this
// task, so none of those three types may be weakened or excluded. The Frac
// non-canonical check (`mainFrac`) is asserted separately, EXCLUDING wasm,
// for the SAME documented pre-existing gap as comparison_conformance_test.go:
// the WASM backend has no Quot/qlift runtime primitive at all today
// (codegen/wasm.go has no `qlift`, unlike every other backend - see js.go/
// py.go/golang.go/rust.go/beam.go/jvm.go/c.go/ll_runtime.go, which each emit
// one). This predates both Plan C and this Float task and is orthogonal to
// it; ANY Frac equality/order program fails the same way on WASM today, with
// or without this feature. Excluding wasm from mainFrac is not a weakening of
// THIS gate - this task touches only Std.Float and the parity corpus, never
// Quot/qlift.
const numericParityCorpusSrc = `
-- Whole: + and le/eqb via the shipped Semiring/Ord/DecEq instances.
wAdd : Bool is eqbOf Whole decEqWhole (addW (succ (succ zero)) (succ (succ (succ zero)))) (succ (succ (succ (succ (succ zero))))) end
wLe  : Bool is leOf Whole ordWhole (succ (succ zero)) (succ (succ (succ zero))) end

-- Int: le across a negative/nonneg boundary (the shipped ordInt instance).
iNeg : Bool is leOf Int ordInt (negsucc (succ zero)) (nonneg (succ zero)) end

-- Float: arithmetic checked BY COMPARISON (feq), never printed.
-- 1.0 + 2.0 == 3.0 ; 2.0 * 3.0 == 6.0 ; 1.0 <= 2.0 ; NaN (0.0/0.0) is not <= itself.
flAdd : Bool is Std.Float.feq (Std.Float.fadd (Std.Float.fromNat (succ zero)) (Std.Float.fromNat (succ (succ zero)))) (Std.Float.fromNat (succ (succ (succ zero)))) end
flMul : Bool is Std.Float.feq (Std.Float.fmul (Std.Float.fromNat (succ (succ zero))) (Std.Float.fromNat (succ (succ (succ zero))))) (Std.Float.fromNat (succ (succ (succ (succ (succ (succ zero))))))) end
flLe  : Bool is Std.Float.fle (Std.Float.fromNat (succ zero)) (Std.Float.fromNat (succ (succ zero))) end
flNaN : Bool is Std.Float.isNaN (Std.Float.fdiv (Std.Float.fromNat zero) (Std.Float.fromNat zero)) end

main : Bool is
  and wAdd
  (and wLe
  (and iNeg
  (and flAdd
  (and flMul
  (and flLe flNaN)))))
end

-- Frac: le over non-canonical representatives of the same rational (qpair i d
-- denotes i/(d+1): (1, d=1) and (2, d=3) both denote 1/2, as raw QPair, no
-- reduction before the comparison).
fracA : Frac is qin QPair QRel (qpair (nonneg (succ zero)) (succ zero)) end
fracB : Frac is qin QPair QRel (qpair (nonneg (succ (succ zero))) (succ (succ (succ zero)))) end
fracNonCanonicalLe : Bool is leOf Frac ordFrac fracA fracB end

mainFrac : Bool is fracNonCanonicalLe end
`

// runNumericParityBackend emits+runs mainName on one backend in dir
// (cwd=dir), loading the corpus on top of the shared prelude. Mirrors
// runComparisonBackend's shape (comparison_conformance_test.go). Skips
// (ok=false) if the backend's toolchain is absent.
func runNumericParityBackend(t *testing.T, bk bibleBackend, mainName, dir string) (string, bool) {
	t.Helper()
	if _, err := exec.LookPath(bk.bin); err != nil {
		return "", false
	}
	s := session.New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	if _, err := s.LoadSource(numericParityCorpusSrc); err != nil {
		t.Fatalf("loading numeric parity corpus: %v", err)
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

// assertNumericParityAgree runs mainName on every backend (bibleBackends())
// in its own temp dir and asserts they all produce `want`. A missing
// toolchain is reported via t.Skipf (not a false pass), naming which
// backend(s) were unavailable - the same convention TestComparisonConformance
// and TestBibleConformance* already use.
func assertNumericParityAgree(t *testing.T, mainName, want string) {
	t.Helper()
	assertNumericParityAgreeExcept(t, mainName, want, nil)
}

// assertNumericParityAgreeExcept is assertNumericParityAgree but skips any
// backend named in exclude outright (logged, not counted as a
// missing-toolchain skip) - used only for the documented pre-existing WASM
// Quot/qlift gap (see the numericParityCorpusSrc doc comment above
// mainFrac), never to paper over an actual divergence introduced by this
// feature.
func assertNumericParityAgreeExcept(t *testing.T, mainName, want string, exclude map[string]bool) {
	t.Helper()
	var skipped []string
	for _, bk := range bibleBackends() {
		if exclude[bk.name] {
			t.Logf("[%s] excluded from %s (documented pre-existing gap, not this feature)", bk.name, mainName)
			continue
		}
		dir := t.TempDir()
		got, ok := runNumericParityBackend(t, bk, mainName, dir)
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
		t.Skipf("numeric parity gate inconclusive -- missing backend toolchain(s): %v", skipped)
	}
}

// TestNumericTowerParity is the v4 Float Track A, Task 2 acceptance gate: all
// four numeric tower types (Whole/Int/Frac/Float) must agree byte-for-byte on
// arithmetic and comparison across every available backend of the 9
// (js/py/go/rs/erl/jvm/c/ll/wasm) for the Whole/Int/Float checks, and across
// the 8 non-WASM backends for the Frac non-canonical check (WASM has no
// Quot/qlift support - see the doc comment on numericParityCorpusSrc). Float
// arithmetic is verified by feq comparison, never by printing a float.
func TestNumericTowerParity(t *testing.T) {
	assertNumericParityAgree(t, "main", "true")
	assertNumericParityAgreeExcept(t, "mainFrac", "true", map[string]bool{"wasm": true})
}
