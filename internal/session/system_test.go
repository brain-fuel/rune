package session

import (
	"strings"
	"testing"
)

// A8 (overlap mechanism + multi-branch systems, C-OVERLAP). A system over a
// disjunction of N faces is a nested fsplit: `[φ1 ↦ u1; φ2 ↦ u2; φ3 ↦ u3]` over
// `for φ1 (for φ2 φ3)` is
//   λh. fsplit A φ1 (for φ2 φ3) u1 (λh'. fsplit A φ2 φ3 u2 u3 h') h
// and it dispatches to the branch whose face is ⊤ — no new primitive beyond the
// fsplit eliminator + the face lattice. Overlap-agreement is the caller's
// obligation (C-OVERLAP option b), vacuous for the disjoint atomic faces the Kan
// rules use. These pin genuine three-way dispatch (the node was "inert" until its
// proper-face-hcomp consumer landed; it now computes end-to-end).

// threeBranch builds the canonical 3-branch system applied at chosen interval
// points i, j, dispatching across the independently-settable faces φ1 = ieq0 i,
// φ2 = ieq0 j, φ3 = ieq1 i with the witness h.
func threeBranchSrc(i, j string) string {
	return `fn (A : UF) (u1 : holds (ieq0 ` + i + `) -> El A)
	          (u2 : holds (ieq0 ` + j + `) -> El A) (u3 : holds (ieq1 ` + i + `) -> El A)
	          (h : holds (for (ieq0 ` + i + `) (for (ieq0 ` + j + `) (ieq1 ` + i + `)))) is
	          fsplit A (ieq0 ` + i + `) (for (ieq0 ` + j + `) (ieq1 ` + i + `)) u1
	            (fn (hh : holds (for (ieq0 ` + j + `) (ieq1 ` + i + `))) is
	               fsplit A (ieq0 ` + j + `) (ieq1 ` + i + `) u2 u3 hh end) h
	        end`
}

// First branch: i:=i0 makes φ1 = ieq0 i0 ~> ⊤, so the system selects u1.
func TestSystemFirstBranch(t *testing.T) {
	s := New()
	got := normalize(t, s, threeBranchSrc("i0", "i1"))
	if !strings.Contains(got, "u1 htop") {
		t.Fatalf("φ1=⊤ must select u1, got %q", got)
	}
}

// Middle branch: i:=i1 (φ1 = ieq0 i1 ~> ⊥, φ3 = ieq1 i1 ~> ⊤... guarded) and
// j:=i0 makes φ2 = ieq0 i0 ~> ⊤; the outer fsplit falls through to the ψ-disjunct
// (for φ2 φ3 ~> ⊤), the inner fsplit selects u2 (φ2 checked before φ3).
func TestSystemMiddleBranch(t *testing.T) {
	s := New()
	got := normalize(t, s, threeBranchSrc("i1", "i0"))
	if !strings.Contains(got, "u2 htop") {
		t.Fatalf("φ1=⊥, φ2=⊤ must select u2, got %q", got)
	}
}

// Third branch: i:=i1, j:=i1 makes φ1 = ieq0 i1 ~> ⊥, φ2 = ieq0 i1 ~> ⊥, and
// φ3 = ieq1 i1 ~> ⊤; both fsplits fall through to the last disjunct, selecting u3.
func TestSystemThirdBranch(t *testing.T) {
	s := New()
	got := normalize(t, s, threeBranchSrc("i1", "i1"))
	if !strings.Contains(got, "u3 htop") {
		t.Fatalf("φ1,φ2=⊥, φ3=⊤ must select u3, got %q", got)
	}
}

// A genuinely proper (neutral) disjunction leaves the system stuck — no branch's
// face is ⊤, so dispatch is symbolic (the honest neutral, as for any eliminator).
func TestSystemStaysStuckOnNeutral(t *testing.T) {
	s := New()
	src := `fn (A : UF) (phi : F) (psi : F) (chi : F)
	          (u1 : holds phi -> El A) (u2 : holds psi -> El A) (u3 : holds chi -> El A)
	          (h : holds (for phi (for psi chi))) is
	          fsplit A phi (for psi chi) u1
	            (fn (hh : holds (for psi chi)) is fsplit A psi chi u2 u3 hh end) h
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "fsplit") {
		t.Fatalf("a proper neutral disjunction must stay a stuck fsplit, got %q", got)
	}
}
