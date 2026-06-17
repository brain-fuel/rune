package session

import (
	"strings"
	"testing"
)

// §F phase-3 hardening: the transp constancy boundary. (Note: an attempt to
// pin system overlap-agreement by PROOF IRRELEVANCE — that `part h1 ≡ part h2`
// for two proofs of `holds φ` — does NOT check: this engine's proof
// irrelevance covers Eq/refl/cast proofs at the canonical level, not arbitrary
// Prop inhabitants in conversion. So overlap-agreement is not definitional
// here; see PARKING-LOT.md.)

// C3 (positive) — the constancy probe SEES THROUGH a β-redex inside the
// type-line: `(λj. A) i` does not really depend on i, so transport is the
// identity. A purely syntactic check would miss this; the sentinel-application
// probe reduces first, so it fires.
const betaConstFacts = `
betaSeesThrough : {A : UF} -> (a : El A) ->
    Eq (El A) (transp (fn (i : I) is (fn (j : I) is A end) i end) a) a is
  fn {A : UF} (a : El A) is refl a end
end
`

func TestTranspSeesThroughBeta(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(betaConstFacts); err != nil {
		t.Fatalf("transp must see a constant line through a β-redex: %v", err)
	}
}

// C3 (negative) — a genuinely varying line whose head former is `pathF` is NOT
// one of the structural piF cases, so transport stays STUCK rather than
// over-firing to the identity. (Historically this case stayed STUCK; A3 now
// FIRES it — see TestTranspVaryingPathFFires below. The conservative boundary
// is now pinned by the proper-φ / fib-line cases, which still stay stuck.)

// A3 (R-BOX/fsplit) — transport over a varying pathF line now FIRES to the CCHM
// comp form: `pabs A (λj. comp … (fsplit …) …)`. Previously this was the labelled
// frontier (stuck); the face-dispatching system (fsplit) is what unblocked it. A
// stuck top-level transport would NOT be a pabs, so this pins the push.
func TestTranspVaryingPathFFires(t *testing.T) {
	s := New()
	src := `fn (A : UF) (x : El A) (y : El A) (p : El (pathF A x y)) (b : El (pathF A x x)) is
	          transp (fn (i : I) is pathF A x (papp A x y p i) end) b
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "pabs") || !strings.Contains(got, "fsplit") {
		t.Fatalf("transport over a varying pathF line must fire to a pabs/comp/fsplit, got %q", got)
	}
}

// A1 (R-FILL): transport over a function-type line with a VARYING DOMAIN fires —
// it pushes under the binder and BACKWARD-FILLS the argument. Here the domain is
// a varying pathF line and the codomain is constant, so the result must be a
// lambda applying f to the backward-transported argument. The backward fill is
// over a pathF domain line, so (since A3) it further reduces to a pabs/comp form
// rather than staying a `transpG` — the result is `λx. f (pabs …)`. A stuck
// top-level transport would NOT be a lambda, so this pins the push.
func TestTranspVaryingDomainPiFFires(t *testing.T) {
	s := New()
	src := `fn (B : UF) (b0 : El B) (b1 : El B) (r : El (pathF B b0 b1)) (C : UF)
	          (f : (x : El (pathF B b0 (papp B b0 b1 r i0))) -> El C) is
	          transp (fn (i : I) is piF (pathF B b0 (papp B b0 b1 r i))
	                                    (fn (z : El (pathF B b0 (papp B b0 b1 r i))) is C end) end) f
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "is f (pabs") {
		t.Fatalf("varying-domain piF transport must push to a lambda applying f to a backward-fill, got %q", got)
	}
}

// A6 (negative) — Glue on a PROPER/neutral face does NOT collapse: El (Glue A φ
// T e) with φ a variable stays a neutral fibrant type (the type boundary fires
// only on ⊤), and unglue on a neutral g stays stuck (no β, and the ⊤ boundary
// does not fire off a total face). This pins the ready/stuck boundary: the A6
// rules never over-fire on a proper face — the deep Kan fixup is the labelled
// remainder (R-BOX), not these.
func TestGlueProperFaceStaysNeutral(t *testing.T) {
	s := New()
	src := `fn (A : UF) (phi : F) (T : holds phi -> UF)
	          (e : (h : holds phi) -> El (Equiv (T h) A))
	          (g : El (Glue A phi T e)) is
	          unglue A phi T e g
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "unglue") {
		t.Fatalf("unglue on a proper face must stay stuck, normalized to %q", got)
	}
}

// R-BOX (negative) — hcomp on a pathF over a PROPER face produces a path whose
// ENDPOINTS compute (a/b) but whose INTERIOR stays symbolic: at a free interior
// point k the normal form keeps an inner `hcomp` head. This pins the rule's
// honest reach — it builds a path without over-claiming the interior fill (which
// would need the carrier A's own proper-face hcomp, the labelled remainder).
func TestPathHcompInteriorStaysSymbolic(t *testing.T) {
	s := New()
	src := `fn (A : UF) (a : El A) (b : El A) (phi : F)
	          (u : I -> holds phi -> El (pathF A a b)) (u0 : El (pathF A a b)) (k : I) is
	          papp A a b (hcomp (pathF A a b) phi u u0) k
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "hcomp") {
		t.Fatalf("path hcomp interior must stay symbolic at a free point, got %q", got)
	}
}

// A3 (negative) — transp over a varying pathF line produces a path whose
// ENDPOINTS compute but whose interior is a symbolic comp with an fsplit-built
// endpoint system. At a free interior point k the normal form keeps a `comp`
// head and an `fsplit` (the face-dispatching endpoint system). This pins the
// honest reach: the rule builds the CCHM comp without forcing the carrier's own
// proper-face fill.
func TestPathTranspInteriorStaysSymbolic(t *testing.T) {
	s := New()
	src := `fn (A : UF) (a : El A) (b0 : El A) (b1 : El A) (r : El (pathF A b0 b1))
	          (p : El (pathF A a b0)) (k : I) is
	          papp A a b1 (transp (fn (i : I) is pathF A a (papp A b0 b1 r i) end) p) k
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "comp ") || !strings.Contains(got, "fsplit") {
		t.Fatalf("path transp interior must be a symbolic comp with fsplit, got %q", got)
	}
}

// A3 (fsplit) — the face-split eliminator dispatches by which disjunct is ⊤:
// fsplit A ⊤ ψ u v h ~> u htop ; fsplit A φ ⊤ u v h ~> v htop. With ftop on the
// left it picks u; with the left a satisfied atom (ieq0 i0 ~> ftop) likewise.
func TestFsplitDispatch(t *testing.T) {
	s := New()
	// left ⊤ → picks u (returns a)
	srcL := `fn (A : UF) (psi : F) (a : El A) (b : El A) (h : holds (for ftop psi)) is
	           fsplit A ftop psi (fn (hp : holds ftop) is a end) (fn (hq : holds psi) is b end) h
	         end`
	if g := normalize(t, s, srcL); !strings.Contains(g, "is a end") {
		t.Fatalf("fsplit with ⊤ left should pick u (a), got %q", g)
	}
	// right ⊤ (left fbot) → picks v (returns b)
	srcR := `fn (A : UF) (phi : F) (a : El A) (b : El A) (h : holds (for fbot ftop)) is
	           fsplit A fbot ftop (fn (hp : holds fbot) is a end) (fn (hq : holds ftop) is b end) h
	         end`
	if g := normalize(t, s, srcR); !strings.Contains(g, "is b end") {
		t.Fatalf("fsplit with ⊤ right (fbot left) should pick v (b), got %q", g)
	}
}

// A8 — sysU is the UF-valued (type-level) counterpart of fsplit: it dispatches a
// partial TYPE by which face holds, returning UF. Same ⊤-dispatch as fsplit.
func TestSysUDispatch(t *testing.T) {
	s := New()
	// left ⊤ → picks u (returns A)
	srcL := `fn (A : UF) (B : UF) (psi : F) (h : holds (for ftop psi)) is
	           sysU ftop psi (fn (hp : holds ftop) is A end) (fn (hq : holds psi) is B end) h
	         end`
	if g := normalize(t, s, srcL); !strings.Contains(g, "is A end") {
		t.Fatalf("sysU with ⊤ left should pick u (A), got %q", g)
	}
	// right ⊤ (left fbot) → picks v (returns B)
	srcR := `fn (A : UF) (B : UF) (phi : F) (h : holds (for fbot ftop)) is
	           sysU fbot ftop (fn (hp : holds fbot) is A end) (fn (hq : holds ftop) is B end) h
	         end`
	if g := normalize(t, s, srcR); !strings.Contains(g, "is B end") {
		t.Fatalf("sysU with ⊤ right (fbot left) should pick v (B), got %q", g)
	}
}

// A8 (ua T-system) — the type-level T-component of a univalence Glue line,
// `sysU (ieq0 i) (ieq1 i) (λ_. A) (λ_. B)`, reduces at each endpoint: at i:=i0 the
// face ieq0 i0 ~> ⊤ so T ~> A; at i:=i1 the face ieq1 i1 ~> ⊤ so T ~> B. This is the
// CHECKED, well-typed `holds φ -> UF` that the postulate-free ua-from-Glue line
// needs (and that the element-valued fsplit cannot express).
func TestUaTSystemReduces(t *testing.T) {
	s := New()
	atI0 := `fn (A : UF) (B : UF) (h : holds (for (ieq0 i0) (ieq1 i0))) is
	           sysU (ieq0 i0) (ieq1 i0) (fn (h0 : holds (ieq0 i0)) is A end)
	                (fn (h1 : holds (ieq1 i0)) is B end) h
	         end`
	if g := normalize(t, s, atI0); !strings.Contains(g, "is A end") {
		t.Fatalf("ua T-system at i0 must reduce to A, got %q", g)
	}
	atI1 := `fn (A : UF) (B : UF) (h : holds (for (ieq0 i1) (ieq1 i1))) is
	           sysU (ieq0 i1) (ieq1 i1) (fn (h0 : holds (ieq0 i1)) is A end)
	                (fn (h1 : holds (ieq1 i1)) is B end) h
	         end`
	if g := normalize(t, s, atI1); !strings.Contains(g, "is B end") {
		t.Fatalf("ua T-system at i1 must reduce to B, got %q", g)
	}
}

// A8 — fsplitD is the DEPENDENT (motive-carrying) face split: it dispatches a
// partial element whose type may depend on the face. Same ⊤-dispatch as fsplit;
// here pinned with a constant motive (so the branches type without restriction).
func TestSplitDDispatch(t *testing.T) {
	s := New()
	// left ⊤ → picks u (returns a)
	srcL := `fn (A : UF) (psi : F) (a : El A) (b : El A) (h : holds (for ftop psi)) is
	           fsplitD ftop psi (fn (hc : holds (for ftop psi)) is A end)
	                   (fn (hp : holds ftop) is a end) (fn (hq : holds psi) is b end) h
	         end`
	if g := normalize(t, s, srcL); !strings.Contains(g, "is a end") {
		t.Fatalf("fsplitD with ⊤ left should pick u (a), got %q", g)
	}
	// right ⊤ (left fbot) → picks v (returns b)
	srcR := `fn (A : UF) (phi : F) (a : El A) (b : El A) (h : holds (for fbot ftop)) is
	           fsplitD fbot ftop (fn (hc : holds (for fbot ftop)) is A end)
	                   (fn (hp : holds fbot) is a end) (fn (hq : holds ftop) is b end) h
	         end`
	if g := normalize(t, s, srcR); !strings.Contains(g, "is b end") {
		t.Fatalf("fsplitD with ⊤ right (fbot left) should pick v (b), got %q", g)
	}
}

// A8 (proof-directed dispatch) — a face split fires on a `horl`/`horr` VALIDITY
// PROOF even when the face is still a proper (non-⊤) cofibration: the proof itself
// names the disjunct. `sysU φ ψ u v (horl φ ψ h) ~> u h`. This is what makes a
// dependent system branch's type line up (e.g. `Equiv (sysU … (horl … h)) B ~>
// Equiv A B`), so the postulate-free ua line type-checks.
func TestSplitProofDispatch(t *testing.T) {
	s := New()
	// sysU on a proper face φ but a horl proof → left branch (A).
	srcU := `fn (A : UF) (B : UF) (phi : F) (psi : F) (h : holds phi) is
	           sysU phi psi (fn (h0 : holds phi) is A end) (fn (h1 : holds psi) is B end)
	                (horl phi psi h)
	         end`
	if g := normalize(t, s, srcU); !strings.Contains(g, "is A end") {
		t.Fatalf("sysU on a horl proof should pick the left branch A, got %q", g)
	}
	// fsplitD on a horr proof → right branch (b).
	srcD := `fn (A : UF) (phi : F) (psi : F) (a : El A) (b : El A) (h : holds psi) is
	           fsplitD phi psi (fn (hc : holds (for phi psi)) is A end)
	                   (fn (h0 : holds phi) is a end) (fn (h1 : holds psi) is b end)
	                   (horr phi psi h)
	         end`
	if g := normalize(t, s, srcD); !strings.Contains(g, "is b end") {
		t.Fatalf("fsplitD on a horr proof should pick the right branch b, got %q", g)
	}
}

// FRE §7 (comp inherits varying base) — comp over a Glue line whose BASE VARIES in i
// (`Glue (Bfun i) …`) reaches a canonical form: the CCHM seam reuses the now
// varying-base transp-over-Glue floor, so heterogeneous composition over a
// varying-base Glue line composes rather than sticking. Reaches a glue/hcomp form.
func TestCompGlueVaryingBase(t *testing.T) {
	s := New()
	src := `fn (Bfun : I -> UF) (phi : F) (T : holds phi -> UF)
	          (e : (i : I) -> (h : holds phi) -> El (Equiv (T h) (Bfun i))) (psi : F)
	          (u : (i : I) -> holds psi -> El (Glue (Bfun i) phi T (e i)))
	          (u0 : El (Glue (Bfun i0) phi T (e i0))) is
	          comp (fn (i : I) is Glue (Bfun i) phi T (e i) end) psi u u0
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "hcomp") {
		t.Fatalf("comp over a varying-base Glue line must reach the hcomp seam, got %q", got)
	}
	if !strings.Contains(got, "transp") {
		t.Fatalf("the seam floor must transport the varying base, got %q", got)
	}
}

// A7 (pappU) — the type-level path application computes, parallel to papp: β on a
// pabsU line, refl on ureflU, and the endpoint boundary for any path.
func TestPappUComputes(t *testing.T) {
	s := New()
	// β: pappU (line i0) (line i1) (pabsU line) i ~> line i.
	if g := normalize(t, s, `fn (line : I -> UF) (i : I) is pappU (line i0) (line i1) (pabsU line) i end`); !strings.Contains(g, "line i") {
		t.Fatalf("pappU β (pabsU) must reduce to line i, got %q", g)
	}
	// refl: pappU A A (ureflU A) i ~> A.
	if g := normalize(t, s, `fn (A : UF) (i : I) is pappU A A (ureflU A) i end`); !strings.Contains(g, "is A end") {
		t.Fatalf("pappU on ureflU must be the constant A, got %q", g)
	}
	// boundary: pappU A B p i0 ~> A, i1 ~> B.
	if g := normalize(t, s, `fn (A : UF) (B : UF) (p : pathU A B) is pappU A B p i0 end`); !strings.Contains(g, "is A end") {
		t.Fatalf("pappU at i0 must be A, got %q", g)
	}
	if g := normalize(t, s, `fn (A : UF) (B : UF) (p : pathU A B) is pappU A B p i1 end`); !strings.Contains(g, "is B end") {
		t.Fatalf("pappU at i1 must be B, got %q", g)
	}
}

// A5b — hcomp over a DEPENDENT sigmaF fires to a pairF: the first component is an
// hcomp over A, the second a comp over the dependent B-fibre line (via hfill).
// Previously this stayed stuck (A5a handled only non-dependent families).
func TestDepSigmaHcompFires(t *testing.T) {
	s := New()
	src := `fn (A : UF) (phi : F)
	          (u : I -> holds phi -> El (sigmaF A (fn (x : El A) is pathF A x x end)))
	          (u0 : El (sigmaF A (fn (x : El A) is pathF A x x end))) is
	          hcomp (sigmaF A (fn (x : El A) is pathF A x x end)) phi u u0
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "pairF") || !strings.Contains(got, "comp ") {
		t.Fatalf("dependent sigmaF hcomp must fire to a pairF with a comp 2nd component, got %q", got)
	}
}

// A5b (transp) — transport over a dependent sigma line fires to a pairF: the
// first component transports in A, the second along the B-fibre line over the
// first component's forward filler (transpFillF). Previously stuck (A5a handled
// only non-dependent families).
func TestTranspDepSigmaFires(t *testing.T) {
	s := New()
	src := `fn (A : UF) (b0 : El A) (b1 : El A) (r : El (pathF A b0 b1))
	          (p : El (sigmaF A (fn (x : El A) is pathF A x (papp A b0 b1 r i0) end))) is
	          transp (fn (i : I) is sigmaF A (fn (x : El A) is pathF A x (papp A b0 b1 r i) end) end) p
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "pairF") {
		t.Fatalf("transp over a dependent sigma line must fire to a pairF, got %q", got)
	}
}

// A5b (comp) — comp over a dependent sigma line fires to a pairF: the second
// component composes along the B-fibre line over the first component's comp-filler
// (compFill). Completes A5b (transp + hcomp + comp over dependent Σ).
func TestCompDepSigmaFires(t *testing.T) {
	s := New()
	src := `fn (A : UF) (phi : F)
	          (u : I -> holds phi -> El (sigmaF A (fn (x : El A) is pathF A x x end)))
	          (u0 : El (sigmaF A (fn (x : El A) is pathF A x x end))) is
	          comp (fn (i : I) is sigmaF A (fn (x : El A) is pathF A x x end) end) phi u u0
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "pairF") {
		t.Fatalf("comp over a dependent sigma line must fire to a pairF, got %q", got)
	}
}

// A4 — pathJ on a GENERAL (variable) path no longer stays stuck: it reduces to a
// transport over the J-line (the contracting-singletons line). The result
// typechecks at P y p via path-η. (J on refl still gives d by the fast path /
// regularity — see listing ch49.)
func TestPathJGeneralFires(t *testing.T) {
	s := New()
	src := `fn (A : UF) (x : El A) (P : (yy : El A) -> El (pathF A x yy) -> UF)
	          (d : El (P x (preflF A x))) (y : El A) (p : El (pathF A x y)) is
	          pathJ A x P d y p
	        end`
	got := normalize(t, s, src)
	if strings.Contains(got, "pathJ ") {
		t.Fatalf("pathJ on a general path must reduce (not stay a pathJ head), got %q", got)
	}
}

// A6 (forallF) — the ∀-cofibration: forallF (λ_. ⊤) ~> ⊤ (a constantly-true line
// holds everywhere); a varying/neutral line stays a real cofibration. This is the
// `∀i.φ` face the transp-over-Glue reconciliation quantifies over.
func TestForallCofibration(t *testing.T) {
	s := New()
	if g := normalize(t, s, `forallF (fn (i : I) is ftop end)`); g != "ftop" {
		t.Fatalf("forallF of a constant-⊤ line must be ftop, got %q", g)
	}
	if g := normalize(t, s, `fn (phi : I -> F) is forallF phi end`); !strings.Contains(g, "forallF") {
		t.Fatalf("forallF of a neutral line must stay a cofibration, got %q", g)
	}
}

// A6 (real Equiv) — Equiv/equivFun/equivProof now have genuine Σf.isEquiv bodies
// (not opaque): equivFun projects the function component and equivProof the
// is-equiv proof from a pairF-built equivalence, via the landed fstF/sndF.
func TestEquivProjectionsCompute(t *testing.T) {
	s := New()
	mk := func(proj string) string {
		return `fn (A : UF) (B : UF) (fn0 : El (piF A (fn (z : El A) is B end)))
		          (prf : El (isEquiv A B fn0)) is
		          ` + proj + ` A B (pairF (piF A (fn (z : El A) is B end))
		                              (fn (f : El (piF A (fn (z : El A) is B end))) is isEquiv A B f end)
		                              fn0 prf)
		        end`
	}
	if g := normalize(t, s, mk("equivFun")); !strings.HasSuffix(strings.TrimSpace(g), "is fn0 end") {
		t.Fatalf("equivFun of a pairF equiv must project the function, got %q", g)
	}
	if g := normalize(t, s, mk("equivProof")); !strings.HasSuffix(strings.TrimSpace(g), "is prf end") {
		t.Fatalf("equivProof of a pairF equiv must project the proof, got %q", g)
	}
}

// A6 (transp-over-Glue, glue-intro case) — CANONICITY: transp over a genuinely
// varying Glue line (here e varies in i) applied to a glue INTRO normalizes to a
// `glue` constructor (not a stuck transp), with the reconciled base a hcomp. This
// is the X2 canonicity gate for the transpGlueIntro rule: a closed transp-over-
// Glue term reaches constructor form. The term is well-typed (it elaborates), so
// the produced glue inhabits El (Glue A φ T (e i1)).
func TestTranspGlueIntroCanonicity(t *testing.T) {
	s := New()
	src := `fn (A : UF) (phi : F) (T : holds phi -> UF)
	          (e : I -> (h : holds phi) -> El (Equiv (T h) A))
	          (tt : (h : holds phi) -> El (T h)) (a : El A) is
	          transp (fn (i : I) is Glue A phi T (e i) end) (glue A phi T (e i0) tt a)
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "glue ") {
		t.Fatalf("transp over a Glue line on a glue intro must reach a glue constructor, got %q", got)
	}
	if !strings.Contains(got, "hcomp") {
		t.Fatalf("the reconciled base should be an hcomp, got %q", got)
	}
}

// A6 (transp-over-Glue β) — unglue of the produced glue reduces to its
// reconciled base (the glue β-rule applies to the constructor transpGlueIntro
// builds), confirming the result is a well-formed glue the eliminator computes on.
func TestTranspGlueIntroUnglueBeta(t *testing.T) {
	s := New()
	src := `fn (A : UF) (phi : F) (T : holds phi -> UF)
	          (e : I -> (h : holds phi) -> El (Equiv (T h) A))
	          (tt : (h : holds phi) -> El (T h)) (a : El A) is
	          unglue A phi T (e i1)
	            (transp (fn (i : I) is Glue A phi T (e i) end) (glue A phi T (e i0) tt a))
	        end`
	got := normalize(t, s, src)
	if strings.Contains(got, "unglue") {
		t.Fatalf("unglue of the produced glue must β-reduce (no residual unglue), got %q", got)
	}
	if !strings.Contains(got, "hcomp") {
		t.Fatalf("unglue should expose the reconciled hcomp base, got %q", got)
	}
}

// A6 (hcomp-over-Glue) — homogeneous composition in a Glue type composes the base
// through unglue and each fibre through unglueT, then re-glues: the result is a
// `glue` constructor (canonicity). Proper face ψ (the ⊤/⊥ rules fire first).
func TestHcompGlueCanonicity(t *testing.T) {
	s := New()
	src := `fn (A : UF) (phi : F) (T : holds phi -> UF)
	          (e : (h : holds phi) -> El (Equiv (T h) A)) (psi : F)
	          (u : I -> holds psi -> El (Glue A phi T e)) (u0 : El (Glue A phi T e)) is
	          hcomp (Glue A phi T e) psi u u0
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "glue ") {
		t.Fatalf("hcomp over a Glue type must reach a glue constructor, got %q", got)
	}
}

// A6 (unglueT β) — the T-component projection computes on a glue intro:
// unglueT A φ T e (glue A φ T e t a) h ~> t h.
func TestUnglueTBeta(t *testing.T) {
	s := New()
	src := `fn (A : UF) (phi : F) (T : holds phi -> UF)
	          (e : (h : holds phi) -> El (Equiv (T h) A))
	          (tt : (h : holds phi) -> El (T h)) (a : El A) (h0 : holds phi) is
	          unglueT A phi T e (glue A phi T e tt a) h0
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "tt h0") {
		t.Fatalf("unglueT of a glue intro must project the T-component (tt h0), got %q", got)
	}
}

// A6 (comp-over-Glue) — heterogeneous composition over a Glue LINE via the CCHM
// seam (transp floor + hcomp filled walls), reusing the transp/hcomp-over-Glue
// arms. Reaches a `glue` constructor (canonicity). Completes the transp/hcomp/comp
// trio over Glue for canonical inputs.
func TestCompGlueCanonicity(t *testing.T) {
	s := New()
	src := `fn (A : UF) (phi : F) (T : holds phi -> UF)
	          (e : I -> (h : holds phi) -> El (Equiv (T h) A)) (psi : F)
	          (u : I -> holds psi -> El (Glue A phi T (e i1)))
	          (tt : (h : holds phi) -> El (T h)) (a : El A) is
	          comp (fn (i : I) is Glue A phi T (e i) end) psi
	               (fn (i : I) (hh : holds psi) is glue A phi T (e i) tt a end)
	               (glue A phi T (e i0) tt a)
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "glue ") {
		t.Fatalf("comp over a Glue line must reach a glue constructor, got %q", got)
	}
}

// A7 — castU along a pabsU-path is transport along the line:
// castU (line i0)(line i1) (pabsU line) x ~> transp line x.
func TestCastUPabsU(t *testing.T) {
	s := New()
	src := `fn (line : I -> UF) (x : El (line i0)) is
	          castU (line i0) (line i1) (pabsU line) x
	        end`
	if g := normalize(t, s, src); !strings.Contains(g, "transp") {
		t.Fatalf("castU along a pabsU-path must reduce to transp, got %q", g)
	}
}

// A7 (univalence computes) — castU over a ua-from-Glue path (pabsU of a Glue
// line) applied to a glue intro reduces, through transp-over-Glue, to a `glue`
// constructor: univalence-as-transport, derived from Glue, computing. This is the
// M2 thesis result, achieved additively (the postulated ua is untouched).
func TestUnivalenceComputesViaGlue(t *testing.T) {
	s := New()
	src := `fn (A : UF) (phi : F) (T : holds phi -> UF)
	          (e : I -> (h : holds phi) -> El (Equiv (T h) A))
	          (tt : (h : holds phi) -> El (T h)) (a : El A) is
	          castU (Glue A phi T (e i0)) (Glue A phi T (e i1))
	                (pabsU (fn (i : I) is Glue A phi T (e i) end))
	                (glue A phi T (e i0) tt a)
	        end`
	if g := normalize(t, s, src); !strings.Contains(g, "glue ") {
		t.Fatalf("castU over a ua-from-Glue path must compute to a glue constructor, got %q", g)
	}
}
