package session

import (
	"strings"
	"testing"
)

// The face lattice (§F phase 3a): atomic constraints decide on interval
// endpoints; the connectives unit/absorb on ⊤/⊥. The group is ambient.
const faceFacts = `
eq0at0 : Eq F (ieq0 i0) ftop is refl ftop end
eq0at1 : Eq F (ieq0 i1) fbot is refl fbot end
eq1at0 : Eq F (ieq1 i0) fbot is refl fbot end
eq1at1 : Eq F (ieq1 i1) ftop is refl ftop end
andTop : (p : F) -> Eq F (fand ftop p) p is fn (p : F) is refl p end end
andBot : (p : F) -> Eq F (fand p fbot) fbot is fn (p : F) is refl fbot end end
orBot  : (p : F) -> Eq F (for fbot p) p is fn (p : F) is refl p end end
orTop  : (p : F) -> Eq F (for p ftop) ftop is fn (p : F) is refl ftop end end
`

func TestFaceLattice(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(faceFacts); err != nil {
		t.Fatalf("face facts should elaborate and check (the ι-rules must fire): %v", err)
	}
}

func TestFaceDoesNotDeploy(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(`phi : F is fand (ieq0 i0) ftop end`); err != nil {
		t.Fatalf("face definition should check: %v", err)
	}
	if _, err := s.EmitProgram("phi"); err == nil || !strings.Contains(err.Error(), "inner layer") {
		t.Fatalf("emitting a face main must be refused, got err=%v", err)
	}
}
