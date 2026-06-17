package session

import (
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// The 0-fragment call-site guard. A proof assembled by ordinary ω helpers
// (here `cong`, an unannotated proof parameter) is still a proof: its type is a
// Prop, so it must erase to the unit token however it was built. Before the
// type-directed eraser, the syntactic Erase walked such a proof and emitted the
// deep numeral chains its equation endpoints mention — node's parser died
// ~1600-deep (ref_docs/rune-verified-implementations.md). The numerals live in
// the proof's IMPLICIT {x}{y} endpoint positions and inside `refl`, none of
// which a syntactic pass can see is vacuous.
//
// Nat is left unary (no `builtin nat`) on purpose: a unary numeral legitimately
// emits a succ-chain, so a leak shows up as a real chain in the shadow. The
// numeral is written INLINE in the proof — exactly as a `35` literal expands to
// a succ-chain inside a ch14 certificate — so its IMPLICIT {x}{y} endpoint
// positions and `refl` argument all carry it. A reference to a named numeral
// would erase to a bare global and hide the leak; the inline chain is what the
// historic node-parser death (~1600 deep) actually came from.
const eraseProofSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end

cong : {A : U} -> {B : U} -> (f : A -> B) -> {x : A} -> {y : A} -> x = y -> f x = f y is
  fn {A : U} {B : U} (f : A -> B) {x : A} {y : A} (p : Eq A x y) is
    subst A x y p (fn (z : A) is f x = f z end) (refl (f x))
  end
end

eqSix : Eq Nat
          (succ (succ (succ (succ (succ (succ (succ zero)))))))
          (succ (succ (succ (succ (succ (succ (succ zero)))))))
  is cong succ (refl (succ (succ (succ (succ (succ (succ zero))))))) end

realSix : Nat is succ (succ (succ (succ (succ (succ zero))))) end
`

func TestProofErasesToUnit(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(eraseProofSrc); err != nil {
		t.Fatalf("loading proof source: %v", err)
	}
	p, err := s.EmitProgram("")
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	def := defByName(t, p, "eqSix")
	if _, ok := def.Body.(codegen.IUnit); !ok {
		t.Fatalf("proof eqSix erased to %T, want codegen.IUnit (the 0-fragment leak is back)", def.Body)
	}
	// The proof inlined a 6-deep chain in its endpoints; none of it may ride
	// into the shadow.
	if d := maxSuccDepth(def.Body); d != 0 {
		t.Fatalf("proof body has a %d-deep succ chain; the numeral leaked", d)
	}
	// A genuine computational numeral survives at full depth.
	if d := maxSuccDepth(defByName(t, p, "realSix").Body); d != 6 {
		t.Fatalf("realSix emitted a %d-deep chain, want 6 (real data must survive)", d)
	}
}

func defByName(t *testing.T, p codegen.Program, name string) codegen.DefSpec {
	t.Helper()
	for _, d := range p.Defs {
		if d.Name == name {
			return d
		}
	}
	t.Fatalf("no emitted definition named %q", name)
	return codegen.DefSpec{}
}

// maxSuccDepth returns the longest chain of `succ` applications in t.
func maxSuccDepth(t codegen.Ir) int {
	switch x := t.(type) {
	case codegen.IApp:
		d := 0
		if g, ok := x.Fn.(codegen.IGlobal); ok && g.Name == "succ" {
			d = 1 + maxSuccDepth(x.Arg)
		}
		if a := maxSuccDepth(x.Fn); a > d {
			d = a
		}
		if a := maxSuccDepth(x.Arg); a > d {
			d = a
		}
		return d
	case codegen.ILam:
		return maxSuccDepth(x.Body)
	case codegen.ILet:
		d := maxSuccDepth(x.Val)
		if a := maxSuccDepth(x.Body); a > d {
			d = a
		}
		return d
	default:
		return 0
	}
}
