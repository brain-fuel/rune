package store

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

func TestCertifyAndLookup(t *testing.T) {
	s := New()
	a := s.Add("A", core.Univ{}, core.Univ{})
	d := s.Add("d", core.Ref{Hash: a}, core.Univ{})

	if s.Certified(d) {
		t.Fatal("uncertified definition reported certified")
	}
	s.Certify(d, []core.Hash{a})
	if !s.Certified(d) {
		t.Fatal("certificate not found after Certify")
	}
}

// A certificate whose dependency does not resolve in this store must not apply:
// the side-condition "does S realize U?" is a set of lookups.
func TestCertRequiresDepsPresent(t *testing.T) {
	s := New()
	d := s.Add("d", core.Univ{}, core.Univ{})
	var ghost core.Hash
	ghost[0] = 0xee
	s.Certify(d, []core.Hash{ghost})
	if s.Certified(d) {
		t.Fatal("certificate applied though its dependency is absent")
	}
	// Once the dependency exists (same content → same hash), the certificate
	// applies — append-only, no invalidation step.
	s2 := New()
	d2 := s2.Add("d", core.Univ{}, core.Univ{})
	a := s2.Add("A", core.Univ{}, core.Pi{Dom: core.Univ{}, Cod: core.Scope{Body: core.Univ{}}})
	s2.Certify(d2, []core.Hash{a})
	if !s2.Certified(d2) {
		t.Fatal("certificate with resolvable deps did not apply")
	}
}

func TestCertKeyCanonicalOrder(t *testing.T) {
	s := New()
	d := s.Add("d", core.Univ{}, core.Univ{})
	a := s.Add("A", core.Univ{}, core.Univ{})
	b := s.Add("B", core.Univ{}, core.Pi{Dom: core.Univ{}, Cod: core.Scope{Body: core.Univ{}}})
	// Same set in both insertion orders must be ONE certificate (sort-then-hash).
	s.Certify(d, []core.Hash{a, b})
	s.Certify(d, []core.Hash{b, a})
	if n := len(s.certsByDef[d]); n != 1 {
		t.Fatalf("expected one canonical certificate, got %d", n)
	}
}
