package codegen_test

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// jsFloatGuardedCorpus mirrors internal/session/float_guarded_test.go's
// floatGuardedCorpus: it exercises fle/feq/isNaN/totalCompare plus the
// Ord/DecEq operator dispatch onto the Std.Float guarded-tier instances,
// folding every sub-check into a single Bool so a real backend run gives one
// genuine computed value. It is duplicated here (rather than imported) so
// this end-to-end check lives entirely in codegen's own test package and
// exercises exactly the module-qualified, non-foreign names (Std.Float.fle
// et al.) that used to break JS emission.
const jsFloatGuardedCorpus = `
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

// TestFloatGuardedTierComputesJS is the JS-backend counterpart of
// internal/session.TestFloatGuardedTierComputes. That test used the Go
// backend because codegen/js.go's jsName previously left '.' unsanitized,
// so emitting a JS program reaching a module-qualified non-foreign name like
// Std.Float.fle produced invalid JS ("const Std.Float.fle = ..."). Now that
// jsName maps '.' to '$' (the same style already used for the prime-to-'$'
// mapping), the identical guarded-tier corpus must also compute "true" when
// emitted to JS and run under node. Skips cleanly if node is not on PATH.
func TestFloatGuardedTierComputesJS(t *testing.T) {
	s := session.New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	if _, err := s.LoadSource(jsFloatGuardedCorpus); err != nil {
		t.Fatalf("loading jsFloatGuardedCorpus: %v", err)
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatalf("EmitProgram(main): %v", err)
	}
	src, err := codegen.JS{}.Emit(p)
	if err != nil {
		t.Fatalf("JS emit: %v", err)
	}
	if strings.Contains(string(src), "Std.Float") {
		t.Fatalf("emitted JS still contains an unsanitized module-qualified name:\n%s", src)
	}
	if got := runNode(t, string(src)); got != "true" {
		t.Fatalf("jsFloatGuardedCorpus main = %q, want %q", got, "true")
	}
}
