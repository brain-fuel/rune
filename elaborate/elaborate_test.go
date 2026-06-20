package elaborate_test

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/elaborate"
	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/store"
	"goforge.dev/rune/v3/surface"
)

// loadOK loads source into a fresh session, failing the test on any error.
func loadOK(t *testing.T, src string) *session.Session {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	return s
}

// loadErr loads source expecting an error containing want.
func loadErr(t *testing.T, src, want string) {
	t.Helper()
	s := session.New()
	_, err := s.LoadSource(src)
	if err == nil {
		t.Fatalf("expected error containing %q, got none", want)
	}
	if !strings.Contains(err.Error(), want) {
		t.Fatalf("error %q does not contain %q", err, want)
	}
}

const prelude = `
id : (A : U) -> A -> A is
  fn (A : U) (x : A) is x end
end
`

func TestWellTypedPrograms(t *testing.T) {
	cases := []string{
		prelude,
		// Polymorphic application at higher level: id1 lives one universe up,
		// so it can take U (and id's own type) as the type argument.
		prelude + `
id1 : (A : U1) -> A -> A is fn (A : U1) (x : A) is x end end
idU : U -> U is id1 U end
idid : (A : U) -> A -> A is id1 ((A : U) -> A -> A) id end
`,
		// Dependent composition.
		prelude + `
comp : (A : U) -> (B : U) -> (C : U) -> (B -> C) -> (A -> B) -> A -> C is
  fn (A : U) (B : U) (C : U) (g : B -> C) (f : A -> B) (x : A) is g (f x) end
end
`,
		// Church numerals; the encoding quantifies over U, so it lives in U1.
		`
Nat : U1 is (A : U) -> (A -> A) -> A -> A end
zero : (A : U) -> (A -> A) -> A -> A is
  fn (A : U) (s : A -> A) (z : A) is z end
end
succ : ((A : U) -> (A -> A) -> A -> A) -> (A : U) -> (A -> A) -> A -> A is
  fn (n : (A : U) -> (A -> A) -> A -> A) (A : U) (s : A -> A) (z : A) is
    s (n A s z)
  end
end
two : Nat is succ (succ zero) end
`,
		// A definition whose TYPE mentions another definition (δ in types).
		`
T : U1 is U -> U end
f : T is fn (x : U) is x end end
`,
		// let with and without annotation, and seq.
		prelude + `
g : U -> U is
  fn (x : U) is
    seq
      let y : U = x
      y
    end
  end
end
`,
	}
	for i, src := range cases {
		if s := loadOK(t, src); s == nil {
			t.Fatalf("case %d failed", i)
		}
	}
}

func TestIllTypedRejected(t *testing.T) {
	cases := []struct{ src, want string }{
		// Applying a non-function.
		{`bad : U is U U end`, "non-function"},
		// Wrong argument type: id expects a type first... U is fine; (id U) wants
		// a U, give it a function type's inhabitant mismatch instead.
		{prelude + `bad : U is id U id end`, "type mismatch"},
		// Body doesn't match the declared type.
		{`bad : U -> U is U end`, "type mismatch"},
		// Lambda against a non-function type.
		{`bad : U is fn (x : U) is x end end`, "non-function type"},
		// Binder annotation conflicts with the expected domain.
		{prelude + `bad : U -> U is fn (x : U -> U) is U end end`, "expected domain"},
		// Unbound name.
		{`bad : U is nope end`, "I can't find `nope` in scope"},
	}
	for _, c := range cases {
		loadErr(t, c.src, c.want)
	}
}

// TestElaborationMatchesResolution: on well-typed programs, the typed elaborator
// must emit exactly the core that untyped name resolution emits — the guarantee
// that Phase 1 left every content hash fixed. (session.AddDef enforces this at
// runtime; this test pins it independently.)
func TestElaborationMatchesResolution(t *testing.T) {
	defs, err := surface.ParseFile(prelude + `
apply : (A : U) -> (B : U) -> (A -> B) -> A -> B is
  fn (A : U) (B : U) (f : A -> B) (x : A) is f x end
end
`)
	if err != nil {
		t.Fatal(err)
	}
	st := store.New()
	refs := map[string]core.Hash{}
	refNames := map[core.Hash]string{}
	for _, d := range defs {
		r := &surface.Resolver{Refs: refs}
		rty, err := r.ResolveExp(d.Ty)
		if err != nil {
			t.Fatal(err)
		}
		rbody, err := r.ResolveExp(d.Body)
		if err != nil {
			t.Fatal(err)
		}
		el := elaborate.New(st, refs, refNames)
		ety, ebody, err := el.ElabDef(d)
		if err != nil {
			t.Fatalf("%s: %v", d.Name, err)
		}
		if store.HashDef(ety, ebody) != store.HashDef(rty, rbody) {
			t.Fatalf("%s: elaborated core differs from resolved core", d.Name)
		}
		h := st.Add(d.Name, ety, ebody)
		refs[d.Name] = h
		refNames[h] = d.Name
	}
}

// TestInferBareLambda: the surface elaborator uses the grammar's mandatory binder
// annotation, so a bare lambda INFERS (the core checker alone could not).
func TestInferBareLambda(t *testing.T) {
	s := session.New()
	e, err := surface.ParseExpr(`fn (A : U) (x : A) is x end`)
	if err != nil {
		t.Fatal(err)
	}
	_, ty, err := s.ElabExpr(e)
	if err != nil {
		t.Fatal(err)
	}
	got := surface.PrettyWith(ty, s.RefNames())
	if got != "(A : U) -> A -> A" {
		t.Fatalf("inferred %q", got)
	}
}
