package session

import "testing"

// Cofibration validity (§F phase 3b): holds and its intros, composing with the
// face lattice (a satisfied atomic face holds via htop).
const sysFacts = `
topHolds : holds ftop is htop end
atom0 : holds (ieq0 i0) is htop end
atom1 : holds (ieq1 i1) is htop end
both : holds (fand (ieq0 i0) (ieq1 i1)) is hand (ieq0 i0) (ieq1 i1) htop htop end
orLeft : (p : F) -> holds (for (ieq0 i0) p) is fn (p : F) is horl (ieq0 i0) p htop end end
partialEl : {A : UF} -> (a : El A) -> (phi : F) -> (holds phi -> El A) is
  fn {A : UF} (a : El A) (phi : F) is fn (h : holds phi) is a end end
end
`

func TestSystems(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(sysFacts); err != nil {
		t.Fatalf("systems facts should elaborate and check: %v", err)
	}
}
