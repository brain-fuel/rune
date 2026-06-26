package session

import (
	"testing"

	"goforge.dev/rune/v3/surface"
)

// TestPureSeqStillWorks asserts a pure (non-IO) seq binding still elaborates and
// evaluates correctly: it must lower to a lazy let, NOT a bindIO, since its bound
// value is a plain Nat (no IO former). The result evaluates to the bound value.
func TestPureSeqStillWorks(t *testing.T) {
	s := New()
	src := `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ

result : Nat is
  seq
    let x : Nat = 5
    x
  end
end
`
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("pure seq did not elaborate: %v", err)
	}
	e, err := s.ParseSrcExpr("result")
	if err != nil {
		t.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatal(err)
	}
	got := surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames())
	if got != "5" {
		t.Fatalf("pure seq result = %q, want 5", got)
	}
}

// TestPureSeqUnannotatedStillWorks is the same but with an inferred (unannotated)
// pure binding, exercising the Infer path of elabSeqBind's pure fallthrough.
func TestPureSeqUnannotatedStillWorks(t *testing.T) {
	s := New()
	src := `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ

result : Nat is
  seq
    let x = 7
    x
  end
end
`
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("unannotated pure seq did not elaborate: %v", err)
	}
	e, err := s.ParseSrcExpr("result")
	if err != nil {
		t.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatal(err)
	}
	got := surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames())
	if got != "7" {
		t.Fatalf("unannotated pure seq result = %q, want 7", got)
	}
}
