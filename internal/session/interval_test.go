package session

import (
	"strings"
	"testing"
)

// TestIntervalDoesNotDeploy: interval value members have no runtime meaning yet
// (paths get a shadow at §F phase 5), so a definition built from them is
// inner-tainted and a tainted MAIN refuses to emit — the same honesty line the
// fibrant value members hold.
func TestIntervalDoesNotDeploy(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(`pt : I is imin i0 i1 end`); err != nil {
		t.Fatalf("interval definition should elaborate and check: %v", err)
	}
	_, err := s.EmitProgram("pt")
	if err == nil || !strings.Contains(err.Error(), "inner layer") {
		t.Fatalf("emitting an interval main must be refused, got err=%v", err)
	}
}

// TestIntervalDeMorgan pins the §F-phase-1 interval algebra: the connectives
// compute on the endpoints i0/i1 (the cubical De Morgan algebra), and stay
// stuck on a neutral interval variable. The group is ambient — a bare session
// already has I, i0, i1, ineg, imin, imax — so no source is loaded.
func TestIntervalDeMorgan(t *testing.T) {
	s := New()
	cases := []struct{ expr, want string }{
		// Reversal on endpoints.
		{"ineg i0", "i1"},
		{"ineg i1", "i0"},
		// ∧ (min) absorption/identity on endpoints.
		{"imin i0 i1", "i0"},
		{"imin i1 i0", "i0"},
		{"imin i1 i1", "i1"},
		// ∨ (max) absorption/identity on endpoints.
		{"imax i0 i1", "i1"},
		{"imax i1 i0", "i1"},
		{"imax i0 i0", "i0"},
		// Double reversal on endpoints (computes via two ι-steps).
		{"ineg (ineg i0)", "i0"},
		// Connectives bottom out on a free interval variable.
		{"ineg (imax i0 i1)", "i0"},
	}
	for _, c := range cases {
		if got := normalize(t, s, c.expr); got != c.want {
			t.Errorf("%s normalized to %q, want %q", c.expr, got, c.want)
		}
	}
}

// TestIntervalNeutralVariable: the connectives reduce against an endpoint even
// when the OTHER operand is a neutral variable — the unit/absorption laws hold
// definitionally, which is what makes the algebra usable under a binder (the
// path abstractions of phase 2).
func TestIntervalNeutralVariable(t *testing.T) {
	s := New()
	// Under `(i : I) -> …`, the bound i is neutral; the rule still fires.
	cases := []struct{ expr, want string }{
		{"fn (i : I) is imin i1 i end", "fn (i : U) is i end"},
		{"fn (i : I) is imin i0 i end", "fn (i : U) is i0 end"},
		{"fn (i : I) is imin i i0 end", "fn (i : U) is i0 end"},
		{"fn (i : I) is imax i i1 end", "fn (i : U) is i1 end"},
		{"fn (i : I) is imax i i0 end", "fn (i : U) is i end"},
		// A genuinely stuck term stays itself.
		{"fn (i : I) is ineg i end", "fn (i : U) is ineg i end"},
	}
	for _, c := range cases {
		if got := normalize(t, s, c.expr); got != c.want {
			t.Errorf("%s normalized to %q, want %q", c.expr, got, c.want)
		}
	}
}
