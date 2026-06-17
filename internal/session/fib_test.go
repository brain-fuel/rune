package session

import (
	"strings"
	"testing"
)

// The two-level pipeline end to end: decoding computes, the inner J computes
// on preflF and ONLY on preflF, castU computes on both of pathU's
// introduction forms, the strict layer cannot collapse distinct inner paths,
// and the inner layer refuses to deploy.

const fibSrc = `
data Bool : U is true : Bool | false : Bool end
not : Bool -> Bool is fn (b : Bool) is BoolElim (fn (x : Bool) is Bool end) false true b end end
notNot : (b : Bool) -> Eq Bool (not (not b)) b is
  fn (b : Bool) is BoolElim (fn (x : Bool) is Eq Bool (not (not x)) x end) refl refl b end
end
boolF : UF is fib Bool end
notPath : pathU boolF boolF is ua boolF boolF not not notNot notNot end
`

func loadFib(t *testing.T) *Session {
	t.Helper()
	s := New()
	if _, err := s.LoadSource(fibSrc); err != nil {
		t.Fatal(err)
	}
	return s
}

func TestFibBuiltinsAmbientAndStable(t *testing.T) {
	s := New()
	for _, n := range []string{"UF", "El", "fib", "piF", "pathF", "preflF", "pathJ",
		"pathU", "ureflU", "ua", "castU"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("builtin %s not bound in a fresh session", n)
		}
	}
	h1, _ := s.Lookup("castU")
	s.Reset()
	h2, ok := s.Lookup("castU")
	if !ok || h1 != h2 {
		t.Fatal("castU hash changed across Reset")
	}
}

func TestElDecodes(t *testing.T) {
	s := loadFib(t)
	if got := normalize(t, s, `El boolF`); got != "Bool" {
		t.Fatalf("El (fib Bool) should decode to Bool, got %s", got)
	}
	// El through piF is a genuine Pi: an inner function checks as a lambda.
	defs := `
idF : El (piF boolF (fn (x : El boolF) is boolF end)) is fn (b : Bool) is b end end
`
	if _, err := s.LoadSource(defs); err != nil {
		t.Fatal(err)
	}
}

func TestCastUComputesOnBothIntros(t *testing.T) {
	s := loadFib(t)
	if got := normalize(t, s, `castU boolF boolF notPath true`); got != "false" {
		t.Fatalf("castU through ua should apply the iso, got %s", got)
	}
	if got := normalize(t, s, `castU boolF boolF (ureflU boolF) true`); got != "true" {
		t.Fatalf("castU along ureflU should be the identity, got %s", got)
	}
}

func TestCastUStuckOnNeutralPath(t *testing.T) {
	s := loadFib(t)
	// castU IS transport over the decoded pathU line (R-UA Decision 2): on a NEUTRAL
	// path it reduces to a STUCK `transp` over `λi. pappU … p i` (not a stuck castU,
	// and crucially not to a value — no path content to transport along).
	got := normalize(t, s, `fn (p : pathU boolF boolF) is castU boolF boolF p true end`)
	if !strings.Contains(got, "transp") || !strings.Contains(got, "pappU") {
		t.Fatalf("castU on a neutral path should reduce to a stuck transp over pappU, got %s", got)
	}
	if strings.Contains(got, "true") && !strings.Contains(got, "pappU") {
		t.Fatalf("castU on a neutral path must not fabricate a value, got %s", got)
	}
}

func TestPathJComputesOnlyOnPrefl(t *testing.T) {
	s := loadFib(t)
	defs := `
psym : (A : UF) -> (x : El A) -> (y : El A) -> El (pathF A x y) -> El (pathF A y x) is
  fn (A : UF) (x : El A) (y : El A) (p : El (pathF A x y)) is
    pathJ A x (fn (z : El A) (q : El (pathF A x z)) is pathF A z x end) (preflF A x) y p
  end
end
`
	if _, err := s.LoadSource(defs); err != nil {
		t.Fatal(err)
	}
	if got := normalize(t, s, `psym boolF true true (preflF boolF true)`); !strings.HasPrefix(got, "preflF") {
		t.Fatalf("pathJ on preflF should compute to the base case, got %s", got)
	}
	// A4: pathJ on a GENERAL path now COMPUTES (transport along the J-line), so
	// psym of a neutral path reduces to the reversed path — a `pabs`, no residual
	// `pathJ` head. (Previously this stayed stuck; A4 unblocked it.)
	got := normalize(t, s, `fn (p : El (pathF boolF true false)) is psym boolF true false p end`)
	if strings.Contains(got, "pathJ ") {
		t.Fatalf("pathJ on a general path must reduce (A4), got %s", got)
	}
}

func TestInnerPathsAreNotCollapsedByOuterEq(t *testing.T) {
	s := loadFib(t)
	bad := `
oops : Eq (pathU boolF boolF) notPath (ureflU boolF) is refl end
`
	if _, err := s.LoadSource(bad); err == nil {
		t.Fatal("distinct inner paths must not be identified by the strict layer")
	}
}

func TestInnerLayerRefusesToDeploy(t *testing.T) {
	s := loadFib(t)
	defs := `
flipped : Bool is castU boolF boolF notPath true end
`
	if _, err := s.LoadSource(defs); err != nil {
		t.Fatal(err)
	}
	// B5 / R-ERASE2: `flipped` uses the inner layer in its SOURCE but its normal
	// form computes (castU-through-ua) to the outer Bool `false`, so it now DEPLOYS
	// soundly (its erased normal form is a genuine outer value). A bare inner
	// type-path (notPath, a pathU value) still has no erased meaning and is refused.
	if _, err := s.EmitProgram("flipped"); err != nil {
		t.Fatalf("flipped computes to an outer Bool and must deploy (B5): %v", err)
	}
	if _, err := s.EmitProgram("notPath"); err == nil {
		t.Fatal("a bare inner type-path (no erased meaning) must be refused (§F)")
	}
	// A definition that only MENTIONS fibrant types in its TYPE still
	// deploys: boolF's body is a type and erases to a unit.
	if _, err := s.EmitProgram("not"); err != nil {
		t.Fatalf("outer definitions must still deploy: %v", err)
	}
}
