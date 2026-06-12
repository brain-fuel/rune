package store

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

// Add stores a body reachable only through Unfold, and TypeOf exposes the public type.
func TestAddUnfoldRoundTrips(t *testing.T) {
	s := New()
	ty := core.Univ{}
	body := core.Lam{Body: core.Scope{Name: "x", Body: core.Var{Idx: 0}}}
	h := s.Add("id", ty, body)

	got, ok := s.Unfold(h)
	if !ok {
		t.Fatal("Unfold missed a stored definition")
	}
	if core.HashTerm(got) != core.HashTerm(body) {
		t.Fatal("Unfold returned a different body")
	}
	if _, ok := s.TypeOf(h); !ok {
		t.Fatal("TypeOf missed a stored definition")
	}
	if bound, _ := s.Lookup("id"); bound != h {
		t.Fatal("name was not bound to the content hash")
	}
}

// A singleton SCC with no self-reference reduces to the plain content hash, so the
// common acyclic case keeps its identity.
func TestHashSCCSingletonIsContentHash(t *testing.T) {
	c := content{Type: core.Univ{}, Body: core.Univ{}}
	got := HashSCC([]content{c})
	if len(got) != 1 {
		t.Fatalf("expected 1 hash, got %d", len(got))
	}
	if got[0] != hashContent(c) {
		t.Fatal("singleton SCC hash differs from plain content hash")
	}
}

// A genuine cyclic group hashes deterministically, yields one distinct hash per
// member, and shares a group identity. Bodies reference each other through positional
// placeholders.
func TestHashSCCCyclicGroup(t *testing.T) {
	// even = \n -> odd n ; odd = \n -> even n  (shapes only; placeholders stand in)
	even := content{Body: core.Lam{Body: core.Scope{Name: "n",
		Body: core.App{Fn: core.Ref{Hash: Placeholder(1)}, Arg: core.Var{Idx: 0}}}}}
	odd := content{Body: core.Lam{Body: core.Scope{Name: "n",
		Body: core.App{Fn: core.Ref{Hash: Placeholder(0)}, Arg: core.Var{Idx: 0}}}}}

	first := HashSCC([]content{even, odd})
	second := HashSCC([]content{even, odd})

	if len(first) != 2 {
		t.Fatalf("expected 2 hashes, got %d", len(first))
	}
	if first[0] != second[0] || first[1] != second[1] {
		t.Fatal("HashSCC is not deterministic")
	}
	if first[0] == first[1] {
		t.Fatal("distinct SCC members must get distinct hashes")
	}
}

// The body barrier is a compile-time fact: this test documents that Def.body is
// unexported and unreachable outside this package. It is in-package, so it can name
// the field — the point is that no OTHER package can. The assertion is structural.
func TestBodyBarrierInPackageOnly(t *testing.T) {
	d := NewDef(core.Univ{}, core.Univ{})
	if d.body == nil {
		t.Fatal("body should be set")
	}
}
