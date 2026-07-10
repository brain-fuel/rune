package codegen

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

// TestEraseRedirectsLoweredRef exercises the Erase choke point (v4 Ord Plan C)
// at the primitive level: a redirect table sends a Ref at the slow hash to the
// fast hash's emitted name, and a nil table disables redirection entirely.
func TestEraseRedirectsLoweredRef(t *testing.T) {
	// Two distinct hashes standing in for slow/fast: core.HashTerm over two
	// distinct terms is guaranteed to differ (content-addressed).
	slow := core.HashTerm(core.Var{Idx: 0})
	fast := core.HashTerm(core.Var{Idx: 1})
	names := map[core.Hash]string{slow: "sleb", fast: "sleW"}
	typeRefs := map[core.Hash]bool{}
	lower := map[core.Hash]core.Hash{slow: fast}

	got := Erase(core.Ref{Hash: slow}, names, typeRefs, lower)
	g, ok := got.(IGlobal)
	if !ok || g.Name != "sleW" {
		t.Fatalf("want IGlobal sleW, got %#v", got)
	}
	// Nil table = no redirect.
	got2 := Erase(core.Ref{Hash: slow}, names, typeRefs, nil)
	if g2, ok := got2.(IGlobal); !ok || g2.Name != "sleb" {
		t.Fatalf("nil table should not redirect, got %#v", got2)
	}
}
