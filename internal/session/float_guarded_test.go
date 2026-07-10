package session

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/prelude"
)

// TestFloatGuardedTierElaborates pins the elaboration-level acceptance of the
// FLOAT GUARDED TIER section (v4 Float Track A, Task 1): the prelude still
// loads, and fle/feq/isNaN/totalCompare plus the four ops instances
// (semiringFloat/subRFloat/ordFloat/decEqFloat) all register under their
// qualified Std.Float names without a hash collision. `Std.Float.fleqN` is a
// `foreign` axiom (bodiless in the kernel; its computation only exists in the
// per-backend codegen/ioprims.go bodies), so NormalizeExpr can never reduce a
// Float value to a concrete numeral or comparison result inside the type
// checker -- there is no kernel-level float arithmetic by design (that is
// exactly what "guarded tier" / "no laws, host-opaque ops" means). This test
// therefore checks PRESENCE and WELL-TYPEDNESS, the level the elaborator can
// actually witness; TestFloatGuardedTierComputes below drives the real JS
// backend to get genuine computed values for the "values" half of the
// acceptance criteria.
func TestFloatGuardedTierElaborates(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{
		"Std.Float.fleqN",
		"Std.Float.fle", "Std.Float.feq", "Std.Float.isNaN", "Std.Float.totalCompare",
		"Std.Float.semiringFloat", "Std.Float.subRFloat", "Std.Float.ordFloat", "Std.Float.decEqFloat",
	} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude (Float guarded-tier section missing or misnamed)", n)
		}
	}

	// Well-typedness of the derived surface + operator dispatch: each of these
	// must ELABORATE (type-check) against the real Std.Float instances, even
	// though their VALUE stays a stuck neutral term in the kernel normalizer.
	for _, expr := range []string{
		"Std.Float.fle (Std.Float.fromNat zero) (Std.Float.fromNat zero)",
		"Std.Float.feq (Std.Float.fromNat zero) (Std.Float.fromNat zero)",
		"Std.Float.isNaN (Std.Float.fromNat zero)",
		"Std.Float.totalCompare (Std.Float.fromNat zero) (Std.Float.fromNat zero)",
		"leOf Std.Float.Float Std.Float.ordFloat (Std.Float.fromNat zero) (Std.Float.fromNat zero)",
		"compareOf Std.Float.Float Std.Float.ordFloat (Std.Float.fromNat zero) (Std.Float.fromNat zero)",
		"eqbOf Std.Float.Float Std.Float.decEqFloat (Std.Float.fromNat zero) (Std.Float.fromNat zero)",
	} {
		e, err := s.ParseSrcExpr(expr)
		if err != nil {
			t.Fatalf("parse %q: %v", expr, err)
		}
		if _, _, err := s.ElabExpr(e); err != nil {
			t.Fatalf("elaborate %q: %v", expr, err)
		}
	}
}

// floatGuardedCorpus exercises fle/feq/isNaN/totalCompare and the Ord/DecEq
// operator dispatch onto the Float instances, folding every sub-check into
// one Bool per case (each printed as a single discrete token: "true" or an
// Ordering constructor) so a real backend run gives genuine computed values.
// Values are built from fromNat + arithmetic only (no FloatLit); NaN is
// `fdiv (fromNat zero) (fromNat zero)`.
const floatGuardedCorpus = `
one   : Std.Float.Float is Std.Float.fromNat (succ zero) end
two   : Std.Float.Float is Std.Float.fromNat (succ (succ zero)) end
nanV  : Std.Float.Float is Std.Float.fdiv (Std.Float.fromNat zero) (Std.Float.fromNat zero) end

fleLt        : Bool is Std.Float.fle one two end
fleGtIsFalse : Bool is notB (Std.Float.fle two one) end
feqSame      : Bool is Std.Float.feq one one end
feqNanFalse  : Bool is notB (Std.Float.feq nanV nanV) end
isNaNTrue    : Bool is Std.Float.isNaN nanV end
isNaNFalseOnNumber : Bool is notB (Std.Float.isNaN one) end

tcNumVsNanIsLt : Bool is
  case Std.Float.totalCompare one nanV of
  | lt -> true | eq -> false | gt -> false
  end
end
tcNanVsNumIsGt : Bool is
  case Std.Float.totalCompare nanV one of
  | lt -> false | eq -> false | gt -> true
  end
end
tcNanVsNanIsEq : Bool is
  case Std.Float.totalCompare nanV nanV of
  | lt -> false | eq -> true | gt -> false
  end
end

dispatchLe   : Bool is leOf Std.Float.Float Std.Float.ordFloat one two end
dispatchEqb  : Bool is eqbOf Std.Float.Float Std.Float.decEqFloat one one end
dispatchCompareIsLt : Bool is
  case compareOf Std.Float.Float Std.Float.ordFloat one two of
  | lt -> true | eq -> false | gt -> false
  end
end

main : Bool is
  and fleLt
  (and fleGtIsFalse
  (and feqSame
  (and feqNanFalse
  (and isNaNTrue
  (and isNaNFalseOnNumber
  (and tcNumVsNanIsLt
  (and tcNanVsNumIsGt
  (and tcNanVsNanIsEq
  (and dispatchLe
  (and dispatchEqb dispatchCompareIsLt))))))))))
end
`

// runFloatGuardedMain emits mainName (loaded on top of the shared prelude plus
// floatGuardedCorpus) to Go and runs it with `go run`, mirroring
// harness/comparison_conformance_test.go's runComparisonBackend shape (same
// EmitProgram + codegen.Go{}.Emit + write-file + run pattern) without
// depending on the harness package. Skips (not fails) if the go toolchain is
// unavailable, the same missing-toolchain convention the harness gates use.
//
// The Go backend is used here rather than JS: codegen/js.go's jsName does not
// replace "." in an identifier (unlike golang.go/py.go/rust.go, which all do
// `strings.ReplaceAll(n, ".", "_")`), so emitting a JS program that reaches a
// module-qualified NON-foreign definition like Std.Float.fle produces invalid
// JS (`const Std.Float.fle = ...`). This is a pre-existing gap in the JS
// backend's identifier sanitizer, not something this task's prelude change
// causes or may fix (codegen is frozen for this task) -- it was simply never
// exercised before, because every prior Std.Float member was `foreign`
// (erasing straight to a host accessor call, never emitted as a JS const).
func runFloatGuardedMain(t *testing.T, mainName string) string {
	t.Helper()
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not found on PATH; skipping real Float computation check")
	}
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	if _, err := s.LoadSource(floatGuardedCorpus); err != nil {
		t.Fatalf("loading floatGuardedCorpus: %v", err)
	}
	p, err := s.EmitProgram(mainName)
	if err != nil {
		t.Fatalf("EmitProgram(%s): %v", mainName, err)
	}
	src, err := codegen.Go{}.Emit(p)
	if err != nil {
		t.Fatalf("Go emit: %v", err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "main.go")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("go", "run", f)
	cmd.Dir = dir
	out, err := cmd.Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("go run failed: %v\n%s", err, stderr)
	}
	return strings.TrimSpace(string(out))
}

// TestFloatGuardedTierComputes drives the real Go backend (`go run`) to get
// genuine computed values for fle/feq/isNaN/totalCompare and the Ord/DecEq
// operator dispatch onto the Float instances -- the "values" half of the
// acceptance criteria that TestFloatGuardedTierElaborates cannot reach,
// because Std.Float.fleqN is a foreign (host-opaque) axiom the kernel
// normalizer never computes. Every sub-check is folded into one `main : Bool`
// (an `and`-chain), so a single "true" proves all of them at once.
func TestFloatGuardedTierComputes(t *testing.T) {
	if got := runFloatGuardedMain(t, "main"); got != "true" {
		t.Fatalf("floatGuardedCorpus main = %q, want %q", got, "true")
	}
}
