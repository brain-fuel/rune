package session

import (
	"strings"
	"testing"

	"goforge.dev/rune/core"
	"goforge.dev/rune/surface"
)

const idSrc = `
id : (A : U) -> A -> A is
  fn (A : U) (x : A) is x end
end
`

func TestLoadTypeChecksAndCertifies(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(idSrc); err != nil {
		t.Fatal(err)
	}
	if !s.Certified("id") {
		t.Fatal("well-typed definition has no certificate")
	}
}

func TestIllTypedDefinitionRejected(t *testing.T) {
	s := New()
	_, err := s.LoadSource(`bad : U -> U is U end`)
	if err == nil || !strings.Contains(err.Error(), "type mismatch") {
		t.Fatalf("ill-typed definition accepted (err=%v)", err)
	}
	if _, ok := s.byName["bad"]; ok {
		t.Fatal("rejected definition was stored anyway")
	}
}

// Reloading identical source is a proof-cache HIT: the certificate minted by the
// first load applies to the second by content hash, with no re-check. (Observable
// here as: the definition is certified before AddDef runs the checker again —
// AddDef consults Certified first.)
func TestReloadIsCacheHit(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(idSrc); err != nil {
		t.Fatal(err)
	}
	h1 := s.refs["id"]
	if _, err := s.LoadSource(idSrc); err != nil {
		t.Fatal(err)
	}
	if s.refs["id"] != h1 {
		t.Fatal("identical source produced a different hash")
	}
	if !s.Certified("id") {
		t.Fatal("certificate lost across reload")
	}
}

// A definition whose body changed gets a NEW hash — the old certificate is a
// miss, never a wrong hit, and the new definition is checked afresh.
func TestEditMintsNewHash(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(idSrc); err != nil {
		t.Fatal(err)
	}
	h1 := s.refs["id"]
	edited := `
id : (A : U) -> A -> A is
  fn (B : U) (y : B) is y end
end
`
	// Alpha-renaming only: SAME core, SAME hash, still certified.
	if _, err := s.LoadSource(edited); err != nil {
		t.Fatal(err)
	}
	if s.refs["id"] != h1 {
		t.Fatal("alpha-renamed definition changed its content hash")
	}

	reallyEdited := `
id : (A : U) -> A -> A is
  fn (A : U) (x : A) is (x : A) end
end
`
	if _, err := s.LoadSource(reallyEdited); err != nil {
		t.Fatal(err)
	}
	if s.refs["id"] == h1 {
		t.Fatal("structurally different body kept the old hash")
	}
	if !s.Certified("id") {
		t.Fatal("re-checked definition not certified")
	}
}

func TestElabExprAndNormalize(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(idSrc); err != nil {
		t.Fatal(err)
	}
	e, err := surface.ParseExpr(`(fn (A : U1) (x : A) is x end) (U -> U) (fn (z : U) is z end)`)
	if err != nil {
		t.Fatal(err)
	}
	tm, ty, err := s.ElabExpr(e)
	if err != nil {
		t.Fatal(err)
	}
	var _ core.Tm = ty
	nf := s.NormalizeExpr(tm)
	got := surface.PrettyWith(nf, s.RefNames())
	if got != "fn (z : U) is z end" {
		t.Fatalf("normal form = %q", got)
	}
}
