// Package ledger - tier classification tests.
package ledger

import (
	"testing"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/internal/session"
)

func mustSession(t *testing.T, src string) *session.Session {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	return s
}

func buildFrom(t *testing.T, src string) []Entry {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	return Build(s)
}

func find(es []Entry, name string) (Entry, bool) {
	for _, e := range es {
		if e.Name == name {
			return e, true
		}
	}
	return Entry{}, false
}

func TestTierClassification(t *testing.T) {
	// hostThing has type U -> U; idU has type (A:U)->A->A -- distinct, so their
	// PropHashes must differ even though both defs are bodiless-vs-bodied.
	src := "foreign hostThing : U -> U end\n" +
		"idU : (A : U) -> A -> A is fn (A : U) (x : A) is x end end"
	es := buildFrom(t, src)

	host, ok := find(es, "hostThing")
	if !ok || host.Tier != Assume {
		t.Fatalf("hostThing want Assume, got %v ok=%v", host.Tier, ok)
	}
	id, ok := find(es, "idU")
	if !ok || id.Tier != Proven {
		t.Fatalf("idU want Proven, got %v ok=%v", id.Tier, ok)
	}
	if id.PropHash == (host.PropHash) {
		// different types, different proposition hashes
		t.Fatalf("distinct types must have distinct proposition hashes")
	}
	if id.ProofHash == (core.Hash{}) {
		t.Fatalf("a proven def must carry a non-zero proof hash")
	}
	if host.ProofHash != (core.Hash{}) {
		t.Fatalf("a bodiless assume must have a zero proof hash")
	}
}

func TestPostulateTier(t *testing.T) {
	es := buildFrom(t, `postulate inRegion : U because "cloud API not yet modeled" end`)
	e, ok := find(es, "inRegion")
	if !ok || e.Tier != Postulate {
		t.Fatalf("inRegion want Postulate, got %v ok=%v", e.Tier, ok)
	}
	if e.Why != "cloud API not yet modeled" {
		t.Fatalf("why not carried: %q", e.Why)
	}
}
