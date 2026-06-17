package session

import (
	"strings"
	"testing"
)

// C1b / R-SUM: surface Σ — Sig/Pair/Fst/Snd elaborate, project (β), and satisfy
// definitional η (a neutral pair equals the pairing of its projections).
const sigmaSurfaceFacts = `
fstB : (A : U) -> (B : A -> U) -> (a : A) -> (b : B a) ->
    Eq A (Fst (Pair A (fn (x : A) is B x end) a b)) a is
  fn (A : U) (B : A -> U) (a : A) (b : B a) is refl a end
end
etaP : (A : U) -> (B : A -> U) -> (p : Sig A (fn (x : A) is B x end)) ->
    Eq (Sig A (fn (x : A) is B x end)) p (Pair A (fn (x : A) is B x end) (Fst p) (Snd p)) is
  fn (A : U) (B : A -> U) (p : Sig A (fn (x : A) is B x end)) is refl p end
end
`

func TestSurfaceSigmaBetaEta(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(sigmaSurfaceFacts); err != nil {
		t.Fatalf("surface Σ must elaborate with β + definitional η: %v", err)
	}
}

// Pretty round-trips the Σ keyword forms.
func TestSurfaceSigmaPretty(t *testing.T) {
	s := New()
	got := normalize(t, s, "fn (A : U) (B : A -> U) (a : A) (b : B a) is Pair A (fn (x : A) is B x end) a b end")
	if !strings.Contains(got, "Pair ") {
		t.Fatalf("Σ pair did not pretty-print as Pair: %q", got)
	}
}
