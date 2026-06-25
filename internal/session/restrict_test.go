package session

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/equality"
	"goforge.dev/rune/v3/surface"
)

// wiredMachine builds a fully-wired evaluation machine for the session's store,
// mirroring NormalizeExpr — for tests that drive core operations directly.
func (s *Session) wiredMachine() *core.Machine {
	m := core.NewMachine(s.st)
	m.Equality = equality.Default()
	m.Data, m.Quot, m.Fibrant, m.Interval, m.Path = s.st, s.st, s.st, s.st, s.st
	m.Face, m.System, m.Kan, m.Sigma, m.Coind = s.st, s.st, s.st, s.st, s.st
	m.Glue, m.Fsplit, m.Forall, m.Partial = s.st, s.st, s.st, s.st
	m.SystemU = s.st
	m.PappU = s.st
	m.FsplitD = s.st
	m.PabsU, m.Hit, m.Susp, m.QuotHit = s.st, s.st, s.st, s.st
	m.PathP, m.CircInd, m.SuspInd, m.QuotInd = s.st, s.st, s.st, s.st
	m.Trunc = s.st
	m.Guard = s.st
	return m
}

// TestRestrictIvBoundary is the face-restricted-eval foundation check (design
// step 2 / obligation 2): restricting `El (Glue A (ieq0 i) T e)` along the
// constraint that satisfies its face (i := i0) exposes the type boundary, so it
// reduces to `El (T htop)`; restricting along the constraint that refutes it
// (i := i1, so ieq0 i1 ~> fbot) leaves a ⊥-faced Glue (no boundary). This is the
// operation the proper-face Glue Kan fixup consumes; nothing in the kernel calls
// RestrictIv yet, so it is verified here in isolation.
func TestRestrictIvBoundary(t *testing.T) {
	s := New()
	m := s.wiredMachine()

	// A closed binder over A, i, T, e whose body is El (Glue A (ieq0 i) T e).
	src := `fn (A : UF) (i : I) (T : holds (ieq0 i) -> UF)
	          (e : (h : holds (ieq0 i)) -> El (Equiv (T h) A)) is
	          El (Glue A (ieq0 i) T e)
	        end`
	e, err := surface.ParseExpr(src)
	if err != nil {
		t.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatalf("elaborate: %v", err)
	}
	// Apply the closure to four fresh free variables: A=level0, i=1, T=2, e=3.
	v := m.Eval(nil, tm)
	for l := 0; l < 4; l++ {
		v = m.Apply(v, core.VVar(l))
	}
	i0h, _ := s.st.IntervalHash(core.IRoleI0)
	i1h, _ := s.st.IntervalHash(core.IRoleI1)
	i0v := m.Eval(nil, core.Ref{Hash: i0h})
	i1v := m.Eval(nil, core.Ref{Hash: i1h})

	// Restrict i (level 1) to i0: ieq0 i0 ~> ftop, El(Glue A ⊤ T e) ~> El(T htop).
	atI0 := s.show(m.Quote(4, m.RestrictIv(4, 1, i0v, v)))
	if strings.Contains(atI0, "Glue") || !strings.Contains(atI0, "htop") {
		t.Fatalf("restrict at i:=i0 should expose El (T htop), got %q", atI0)
	}
	// Restrict i (level 1) to i1: ieq0 i1 ~> fbot, the Glue stays (no boundary).
	atI1 := s.show(m.Quote(4, m.RestrictIv(4, 1, i1v, v)))
	if !strings.Contains(atI1, "Glue") {
		t.Fatalf("restrict at i:=i1 should keep a ⊥-faced Glue, got %q", atI1)
	}
}

// show pretty-prints a core term against the session's names.
func (s *Session) show(t core.Tm) string {
	return surface.PrettyWith(t, s.RefNames())
}
