package session

import (
	"strings"
	"testing"
)

// clockNorm parses, elaborates, normalizes, and pretty-prints a closed expression —
// the force/clock ι behavior is observed on the normal form.
func clockNorm(t *testing.T, s *Session, src string) string {
	t.Helper()
	e, err := s.ParseSrcExpr(src)
	if err != nil {
		t.Fatalf("parse %q: %v", src, err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatalf("elaborate %q: %v", src, err)
	}
	return s.Pretty(s.NormalizeExpr(tm))
}

// TestForceNextComputes pins the clock-β / force-next coherence (CLOCKS-DESIGN): a
// purely-delayed value cashed by `force` reduces to the value. `force A (λj. next j A x) k`
// must normalize to `x` — the `force` head is gone (the ι fired). This is the iso
// `∀κ.▹κ A ≃ ∀κ.A` on a `next` intro, the operation guarded recursion alone cannot do.
func TestForceNextComputes(t *testing.T) {
	s := New()
	got := clockNorm(t, s,
		"fn (A : UF) (x : El A) (k : Clock) is force A (fn (j : Clock) is next j A x end) k end")
	if strings.Contains(got, "force") {
		t.Fatalf("force should have fired (no residual force head), got: %s", got)
	}
	if !strings.Contains(got, "x") {
		t.Fatalf("force A (λj. next j A x) k should reduce to x, got: %s", got)
	}
}

// TestForceStaysStuckOnNeutral is the SOUNDNESS pin: `force` is the SOLE elimination of
// ▹ and it must NOT fabricate a value out of a delayed value it cannot inspect. On an
// abstract clock-indexed delayed value `la` (a neutral, not a `next` intro), `force A la k`
// must stay STUCK — the normal form still carries the `force` head. If this reduced, the
// unsound `force : ▹A -> A` would be reachable and the normalizer could diverge.
func TestForceStaysStuckOnNeutral(t *testing.T) {
	s := New()
	got := clockNorm(t, s,
		"fn (A : UF) (la : (j : Clock) -> El (Later j A)) (k : Clock) is force A la k end")
	if !strings.Contains(got, "force") {
		t.Fatalf("force on an abstract (neutral) delayed value must stay stuck, got: %s", got)
	}
}

// TestForceWrongClockRejected is the strongest soundness pin: clock-indexing makes the
// unsound `force` argument UNTYPABLE. `force` needs `(j : Clock) -> El (Later j A)` — the
// delay on the BOUND clock j. A family `λj. next c A x` whose delay sits on a DIFFERENT
// (free) clock `c` has type `(j : Clock) -> El (Later c A)`, which does NOT unify — the
// elaborator REJECTS it. This is why clock-indexing the Later blocks the inconsistent
// `Later A -> A`: you cannot even FORM a forceable value delayed off the quantified clock.
func TestForceWrongClockRejected(t *testing.T) {
	s := New()
	src := "fn (A : UF) (x : El A) (c : Clock) (k : Clock) is force A (fn (j : Clock) is next c A x end) k end"
	e, err := s.ParseSrcExpr(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	_, _, err = s.ElabExpr(e)
	if err == nil {
		t.Fatalf("force on a next bound to the wrong clock must be REJECTED, but it elaborated")
	}
	if !strings.Contains(err.Error(), "Later") {
		t.Fatalf("expected a clock/Later type mismatch, got: %v", err)
	}
}

// TestGfixUnguardedTerminates is the convGfix soundness/termination pin: the guarded
// recursive-type fixpoint `gfix` unfolds in CONVERSION (`gfix k F ≡ F (gfix k F)`), and an
// UNGUARDED operator `F = λX. X` would unfold to itself forever without the progress guard.
// Elaborating + normalizing `gfix k (λX. X)` must TERMINATE (the test completing proves it)
// and leave a stuck `gfix` head (eval keeps it neutral — no divergence). If convGfix lacked
// the progress guard this would hang.
func TestGfixUnguardedTerminates(t *testing.T) {
	s := New()
	got := clockNorm(t, s, "fn (k : Clock) is gfix k (fn (X : UF) is X end) end")
	if !strings.Contains(got, "gfix") {
		t.Fatalf("unguarded gfix must stay a stuck neutral, got: %s", got)
	}
}

// TestGfixGuardedFoldComputes pins that gfix gives a working guarded-recursive type: with
// `gStr k A = gfix k (λX. A × ▹κ X)`, an element built by `consG` (a plain inner pair, typed
// at El(gStr k A) only because convGfix unfolds the fixpoint) has its head recovered by
// `fstF` — `headG k A (consG k A h t) ~> h`. The fold both type-checks (convGfix) AND computes.
func TestGfixGuardedFoldComputes(t *testing.T) {
	s := New()
	// gStr k A = gfix k (λX. sigmaF A (λ_. Later k X)); head of a cons is the head.
	src := "fn (k : Clock) (A : UF) (h : El A) (t : El (Later k (gfix k (fn (X : UF) is sigmaF A (fn (w : El A) is Later k X end) end)))) is " +
		"fstF A (fn (w : El A) is Later k (gfix k (fn (X : UF) is sigmaF A (fn (w : El A) is Later k X end) end)) end) " +
		"(pairF A (fn (w : El A) is Later k (gfix k (fn (X : UF) is sigmaF A (fn (w : El A) is Later k X end) end)) end) h t) end"
	got := clockNorm(t, s, src)
	if !strings.Contains(got, "h") {
		t.Fatalf("headG (consG h t) should compute to h, got: %s", got)
	}
}

// TestForceDComputes pins the DEPENDENT force-next coherence: `forceD` cashes a delayed
// value whose payload TYPE varies with the clock — `forceD A (λj. next j (A j) (x j)) k`
// must reduce to `x k` (the `forceD` head gone), even though `x j : El (A j)` mentions j.
// This is the observation plain `force` cannot do (its payload must be clock-closed).
func TestForceDComputes(t *testing.T) {
	s := New()
	got := clockNorm(t, s,
		"fn (A : Clock -> UF) (x : (k : Clock) -> El (A k)) (k : Clock) is forceD A (fn (j : Clock) is next j (A j) (x j) end) k end")
	if strings.Contains(got, "forceD") {
		t.Fatalf("forceD on a next must fire (no residual forceD head), got: %s", got)
	}
	if !strings.Contains(got, "x") {
		t.Fatalf("forceD A (λj. next j (A j) (x j)) k should reduce to x k, got: %s", got)
	}
}

// TestLaterAppComputes pins the universe-level ▹κ application: `laterApp k A f (next k A x)`
// must reduce to `Later k (f x)` (the `laterApp` head gone) — lifting a UF-valued family
// through a delayed value, the piece a bisimilarity's recursive occurrence needs under ▹κ.
func TestLaterAppComputes(t *testing.T) {
	s := New()
	got := clockNorm(t, s,
		"fn (k : Clock) (A : UF) (f : El A -> UF) (x : El A) is laterApp k A f (next k A x) end")
	if strings.Contains(got, "laterApp") {
		t.Fatalf("laterApp on a next must fire (no residual head), got: %s", got)
	}
	if !strings.Contains(got, "Later") {
		t.Fatalf("laterApp k A f (next k A x) should reduce to Later k (f x), got: %s", got)
	}
}

// TestLaterAppStaysStuckOnNeutral: laterApp on an abstract (neutral) delayed value stays stuck.
func TestLaterAppStaysStuckOnNeutral(t *testing.T) {
	s := New()
	got := clockNorm(t, s,
		"fn (k : Clock) (A : UF) (f : El A -> UF) (la : El (Later k A)) is laterApp k A f la end")
	if !strings.Contains(got, "laterApp") {
		t.Fatalf("laterApp on a neutral delayed value must stay stuck, got: %s", got)
	}
}

// TestLapDComputes pins the DEPENDENT guarded application: `lapD k A B (next k (piF A B) f)
// (next k A x)` must reduce to `next k (B x) (f x)` (the `lapD` head gone) — applying a
// delayed dependent function to a delayed argument, the path-assembly's recursion combinator.
func TestLapDComputes(t *testing.T) {
	s := New()
	got := clockNorm(t, s,
		"fn (k : Clock) (A : UF) (B : El A -> UF) (f : (x : El A) -> El (B x)) (x : El A) is lapD k A B (next k (piF A B) f) (next k A x) end")
	if strings.Contains(got, "lapD") {
		t.Fatalf("lapD on two nexts must fire (no residual head), got: %s", got)
	}
	if !strings.Contains(got, "next") {
		t.Fatalf("lapD ... should reduce to next k (B x) (f x), got: %s", got)
	}
}

// TestLapDStaysStuckOnNeutral: lapD on an abstract (neutral) delayed function stays stuck.
func TestLapDStaysStuckOnNeutral(t *testing.T) {
	s := New()
	got := clockNorm(t, s,
		"fn (k : Clock) (A : UF) (B : El A -> UF) (lf : El (Later k (piF A B))) (x : El A) is lapD k A B lf (next k A x) end")
	if !strings.Contains(got, "lapD") {
		t.Fatalf("lapD on a neutral delayed function must stay stuck, got: %s", got)
	}
}

// TestHcompOverGfixUnfolds pins the Kan endpoint-repair enabler: `hcomp` over a guarded
// stream `gStr = gfix k F` (a gfix-neutral-headed type) must no longer be STUCK — it unfolds
// the gfix type one step (`gfix k F ≡ F (gfix k F)`, a sigmaF) and the landed structural-Σ
// Kan rule (A5a) fires, producing a `pairF` (a guarded-stream cons). Without the gfix-unfold
// case the formers would not match and hcomp would stay stuck. This is what lets the converse
// repair a path's endpoints over a guarded stream.
func TestHcompOverGfixUnfolds(t *testing.T) {
	s := New()
	gstr := "gfix k (fn (X : UF) is sigmaF A (fn (w : El A) is Later k X end) end)"
	src := "fn (k : Clock) (A : UF) (phi : F) (u : I -> holds phi -> El (" + gstr + ")) (u0 : El (" + gstr + ")) is " +
		"hcomp (" + gstr + ") phi u u0 end"
	got := clockNorm(t, s, src)
	if !strings.Contains(got, "pairF") {
		t.Fatalf("hcomp over gStr should unfold the gfix type and fire the Σ Kan rule (pairF), got: %s", got)
	}
}

// TestOutCommutesWithHcompOverNu is the Kan-over-Nu pin (the global endpoint-repair
// enabler): `out` commutes with an hcomp over a coinductive `Nu F`, so
// `out F (hcomp (Nu F) φ u u0) ~> hcomp (F (Nu F)) φ (out∘u) (out u0)`. For the stream
// functor `F = λX. Σ A (λ_. X)` the result type is a `sigmaF`, so the landed Σ Kan rule
// fires next and the head reads off — the normal form must reach a `pairF` (and shed the
// outer `out`). `Nu` is negative (not definitionally its unfolding), so this rides the
// observation, unlike the gfix type-unfold above.
func TestOutCommutesWithHcompOverNu(t *testing.T) {
	s := New()
	streamF := "fn (X : UF) is sigmaF A (fn (w : El A) is X end) end"
	str := "Nu (" + streamF + ")"
	src := "fn (A : UF) (phi : F) (u : I -> holds phi -> El (" + str + ")) (u0 : El (" + str + ")) is " +
		"out (" + streamF + ") (hcomp (" + str + ") phi u u0) end"
	got := clockNorm(t, s, src)
	if !strings.Contains(got, "pairF") {
		t.Fatalf("out over an hcomp-over-Nu should commute and fire the Σ Kan rule (pairF), got: %s", got)
	}
}

// TestUnfoldEtaFinalCoalgebra is the final-coalgebra η pin (coinductive uniqueness, the
// dual of the out-on-unfold ι): the anamorphism into the final coalgebra at carrier `Nu F`
// with coalgebra `out F` is the identity — `unfold F (Nu F) (out F) s ~> s`. Normalizing a
// closed instance must shed the `unfold`/`out` and leave the bare variable `s`.
func TestUnfoldEtaFinalCoalgebra(t *testing.T) {
	s := New()
	streamF := "fn (X : UF) is sigmaF A (fn (w : El A) is X end) end"
	str := "Nu (" + streamF + ")"
	got := clockNorm(t, s,
		"fn (A : UF) (s : El ("+str+")) is unfold ("+streamF+") ("+str+") (out ("+streamF+")) s end")
	if strings.Contains(got, "unfold") {
		t.Fatalf("unfold (out) s must η-reduce to s, got: %s", got)
	}
}

// TestNuConsBetaEta pins the one-level coinductive constructor `nuCons`: its β
// `out F (nuCons F x) ~> x` (observe a constructed value) and its η `nuCons F (out F
// s) ≡ s` (reconstruct from one observation), the latter via the coinductive
// extensionality convNuConsEta. The β normal form must shed `nuCons`/`out`; the η is
// checked as a refl coherence through a normalize+compare.
func TestNuConsBetaEta(t *testing.T) {
	s := New()
	streamF := "fn (X : UF) is sigmaF A (fn (w : El A) is X end) end"
	str := "Nu (" + streamF + ")"
	// β: out F (nuCons F x) reduces to x.
	beta := clockNorm(t, s,
		"fn (A : UF) (x : El ("+streamF+" ("+str+"))) is out ("+streamF+") (nuCons ("+streamF+") x) end")
	if strings.Contains(beta, "nuCons") || strings.Contains(beta, "out ") {
		t.Fatalf("out (nuCons x) must β-reduce to x, got: %s", beta)
	}
	// η: nuCons F (out F s) ≡ s — normal form sheds nuCons/out (the eval η fires here
	// because the argument is literally out-headed).
	eta := clockNorm(t, s,
		"fn (A : UF) (s : El ("+str+")) is nuCons ("+streamF+") (out ("+streamF+") s) end")
	if strings.Contains(eta, "nuCons") {
		t.Fatalf("nuCons (out s) must η-reduce to s, got: %s", eta)
	}
}

// TestGfixFUnguardedTerminates is the convGfixF soundness/termination pin: the INDEXED
// guarded fixpoint `gfixF k D Φ d ≡ Φ (gfixF k D Φ) d` unfolds in conversion, and an
// unguarded `Φ = λR. R` would unfold to itself forever without the progress guard.
// Elaborating + normalizing `gfixF k D (λR. R) d` must TERMINATE and leave a stuck head.
func TestGfixFUnguardedTerminates(t *testing.T) {
	s := New()
	got := clockNorm(t, s,
		"fn (k : Clock) (D : UF) (d : El D) is gfixF k D (fn (R : El D -> UF) is R end) d end")
	if !strings.Contains(got, "gfixF") {
		t.Fatalf("unguarded gfixF must stay a stuck neutral, got: %s", got)
	}
}

// TestForceDStaysStuckOnNeutral is the forceD soundness pin: on an abstract clock-indexed
// delayed value (a neutral, not a `next` intro), `forceD` must stay STUCK — it cannot
// fabricate a value it cannot inspect. Same C4-firewall discipline as `force`.
func TestForceDStaysStuckOnNeutral(t *testing.T) {
	s := New()
	got := clockNorm(t, s,
		"fn (A : Clock -> UF) (la : (j : Clock) -> El (Later j (A j))) (k : Clock) is forceD A la k end")
	if !strings.Contains(got, "forceD") {
		t.Fatalf("forceD on an abstract (neutral) delayed value must stay stuck, got: %s", got)
	}
}

// TestHcompOverGfixFUnfolds is the CUBICAL E2-converse enabler (telos-4/M7): `hcomp`
// over an INDEXED guarded-recursive type `gfixF k D Φ d` (the `Bisim` former — the head
// of `gBisim k A d`) must no longer be STUCK. It unfolds the gfixF one step (`gfixF k D Φ
// d ≡ Φ (gfixF k D Φ) d`, the same convGfixF equation) to expose its `sigmaF` body, so
// the landed structural-Σ Kan rule fires componentwise (a `pairF`). This is the building
// block the cubical converse composes from: Kan-filling over the bisimilarity relation.
// FIRES on a guarded (productive) code — the recursive occurrence under `Later`.
func TestHcompOverGfixFUnfolds(t *testing.T) {
	s := New()
	// Guarded indexed code: Φ = λR e. sigmaF A (λ_. Later k (R e)) — the gBisim shape
	// (a head component plus the delayed recursive occurrence under ▹κ).
	gfixf := "gfixF k D (fn (R : El D -> UF) is fn (e : El D) is " +
		"sigmaF A (fn (w : El A) is Later k (R e) end) end end) d"
	src := "fn (k : Clock) (D : UF) (A : UF) (d : El D) (phi : F) " +
		"(u : I -> holds phi -> El (" + gfixf + ")) (u0 : El (" + gfixf + ")) is " +
		"hcomp (" + gfixf + ") phi u u0 end"
	got := clockNorm(t, s, src)
	if !strings.Contains(got, "pairF") {
		t.Fatalf("hcomp over a guarded gfixF should unfold and fire the Σ Kan rule (pairF), got: %s", got)
	}
}

// TestHcompOverGfixFUnguardedStaysStuck is the SOUNDNESS + TERMINATION pin for the
// gfixF Kan enabler: on an UNGUARDED code (the recursive occurrence NOT under `Later`,
// e.g. Φ = λR e. sigmaF A (λ_. R e)) the unfold is REFUSED — firing would let the Σ Kan
// descent re-unfold forever (NbE would diverge). So `hcomp` over an unguarded gfixF must
// (a) TERMINATE and (b) stay STUCK (the `hcomp`/`gfixF` heads survive). This is strictly
// safer than naive unfolding: only productive codes (the converse's `gStr`/`gBisim`)
// unfold; non-productive ones stay stuck. (Same termination guard now hardens the gfix
// enabler too.)
func TestHcompOverGfixFUnguardedStaysStuck(t *testing.T) {
	s := New()
	gfixf := "gfixF k D (fn (R : El D -> UF) is fn (e : El D) is " +
		"sigmaF A (fn (w : El A) is R e end) end end) d"
	src := "fn (k : Clock) (D : UF) (A : UF) (d : El D) (phi : F) " +
		"(u : I -> holds phi -> El (" + gfixf + ")) (u0 : El (" + gfixf + ")) is " +
		"hcomp (" + gfixf + ") phi u u0 end"
	// clockNorm normalizes; if the unfold were unguarded this would not return (the test
	// would hang / OOM). It returning at all is the termination half of the pin.
	got := clockNorm(t, s, src)
	if !strings.Contains(got, "hcomp") || !strings.Contains(got, "gfixF") {
		t.Fatalf("hcomp over an UNGUARDED gfixF must stay stuck (no unfold), got: %s", got)
	}
}

// TestHcompOverGfixUnguardedTerminates pins that the (pre-existing) single-code `gfix`
// Kan enabler now also REFUSES an unguarded code (`F = λX. sigmaF A (λ_. X)`) — it must
// terminate and stay stuck, not diverge. Before the termination guard this looped; the
// guard makes both gfix and gfixF enablers fire only on productive (Later-guarded) codes.
func TestHcompOverGfixUnguardedTerminates(t *testing.T) {
	s := New()
	gfix := "gfix k (fn (X : UF) is sigmaF A (fn (w : El A) is X end) end)"
	src := "fn (k : Clock) (A : UF) (phi : F) " +
		"(u : I -> holds phi -> El (" + gfix + ")) (u0 : El (" + gfix + ")) is " +
		"hcomp (" + gfix + ") phi u u0 end"
	got := clockNorm(t, s, src)
	if !strings.Contains(got, "hcomp") || !strings.Contains(got, "gfix") {
		t.Fatalf("hcomp over an UNGUARDED gfix must stay stuck (no unfold), got: %s", got)
	}
}

// TestLmapConstFiresOnNeutral pins the ▹κ functor law `lmap k A B (const x) la ~> next k B x`
// — the guard-group ι (tryGuardIota constant-collapse case) that closes the E2-converse's two
// delayed-tail correction paths. On an ABSTRACT (neutral) delayed value `la` and a CONSTANT
// mapped function `λ_. x`, the lmap reduces to `next k B x` (the `lmap` head is gone) — the
// previously-stuck `lmap (const) NL ≡ next` now computes. Sound in the topos-of-trees model
// (a constant map sends every delayed value to the constant `next`).
func TestLmapConstFiresOnNeutral(t *testing.T) {
	s := New()
	got := clockNorm(t, s,
		"fn (k : Clock) (A : UF) (B : UF) (x : El B) (la : El (Later k A)) is "+
			"lmap k A B (fn (_q : El A) is x end) la end")
	if strings.Contains(got, "lmap") {
		t.Fatalf("lmap of a CONSTANT function over a neutral delayed value must fire (no residual lmap), got: %s", got)
	}
	if !strings.Contains(got, "next") {
		t.Fatalf("lmap (const x) la must reduce to next k B x, got: %s", got)
	}
}

// TestLmapNonConstStaysStuck is the SOUNDNESS pin: the constant-collapse ι is constancy-gated
// (it probes the mapped function at a fresh sentinel). On a NON-constant function (identity)
// over a neutral `la`, the lmap must STAY STUCK — no spurious `next`. If the rule over-fired
// it would be unsound (it would assert `lmap id la ≡ next (anything)`). The `lmap` head must
// survive in the normal form.
func TestLmapNonConstStaysStuck(t *testing.T) {
	s := New()
	got := clockNorm(t, s,
		"fn (k : Clock) (A : UF) (la : El (Later k A)) is "+
			"lmap k A A (fn (q : El A) is q end) la end")
	if !strings.Contains(got, "lmap") {
		t.Fatalf("lmap of the IDENTITY (non-constant) over a neutral delayed value must stay stuck, got: %s", got)
	}
}

// TestLmapConstConfluentWithNext pins confluence with the pre-existing `next`-intro case:
// `lmap (const x) (next k A y)` must reduce to `next k B x` whether the next-intro case fires
// (`next (const x applied to y)` = `next x`) or the constant-collapse case fires (`next x`) —
// both land the same value. (The result is fully canonical: no lmap head, a `next` of `x`.)
func TestLmapConstConfluentWithNext(t *testing.T) {
	s := New()
	got := clockNorm(t, s,
		"fn (k : Clock) (A : UF) (B : UF) (x : El B) (y : El A) is "+
			"lmap k A B (fn (_q : El A) is x end) (next k A y) end")
	if strings.Contains(got, "lmap") {
		t.Fatalf("lmap (const x) (next y) must fire, got: %s", got)
	}
	if !strings.Contains(got, "next") || !strings.Contains(got, "x") {
		t.Fatalf("lmap (const x) (next y) must reduce to next k B x, got: %s", got)
	}
}
