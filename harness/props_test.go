package harness

import (
	"testing"

	"pgregory.net/rapid"

	"goforge.dev/rune/core"
	"goforge.dev/rune/surface"
)

// resolveClosed resolves a closed surface expression to core, failing the test on any
// resolution error (the generator only produces well-scoped, closed terms).
func resolveClosed(t *rapid.T, e surface.Exp) core.Tm {
	r := &surface.Resolver{}
	c, err := r.ResolveExp(e)
	if err != nil {
		t.Fatalf("resolve failed on generated term: %v", err)
	}
	return c
}

// TestRoundTrip is the parse∘pretty = id invariant, stated on core: resolving a
// generated term, pretty-printing it, parsing it back, and resolving again must
// return the same core (compared by content hash, since names are reconstructed).
func TestRoundTrip(t *testing.T) {
	rapid.Check(t, func(t *rapid.T) {
		e := GenClosedExp(t)
		c1 := resolveClosed(t, e)

		printed := surface.Pretty(c1)
		e2, err := surface.ParseExpr(printed)
		if err != nil {
			t.Fatalf("re-parse failed for %q: %v", printed, err)
		}
		c2 := resolveClosed(t, e2)

		if core.HashTerm(c1) != core.HashTerm(c2) {
			t.Fatalf("round-trip changed the core\n  printed: %s\n  before:  %s\n  after:   %s",
				printed, core.HashTerm(c1), core.HashTerm(c2))
		}
	})
}

// TestHashAlphaInvariance is the hash-invariance-under-alpha-renaming invariant: a
// term and a consistently-renamed copy resolve to identical core and therefore hash
// equal. This is the direct consequence of the locally-nameless core and of
// Scope.Name being excluded from the digest.
func TestHashAlphaInvariance(t *testing.T) {
	rapid.Check(t, func(t *rapid.T) {
		e := GenClosedExp(t)
		renamed := AlphaRename(e)

		c1 := resolveClosed(t, e)
		c2 := resolveClosed(t, renamed)

		if core.HashTerm(c1) != core.HashTerm(c2) {
			t.Fatalf("alpha-renaming changed the hash\n  original: %s\n  renamed:  %s",
				core.HashTerm(c1), core.HashTerm(c2))
		}
	})
}
