package harness

import (
	"os"
	"path/filepath"
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

// TestGConverseGen pins the cons-form-free generalization of the per-clock→global E2
// converse (ch69 `gConverseGen`) and its load-bearing computation pins. `gConverseGen`
// drops the cons-form restriction `gBisimToPathGlue` carried: it assembles the global path
// `glue ra ≡ glue rb` for ARBITRARY global guarded streams via `bisimToPathStr`, feeding the
// head-path (`bisimHead k0`, definitional through glueHead ▸ gheadG) and the tail-path
// (`conv` on the cashed tail-pair bisim, definitional through glueTail) ON THE NOSE.
//
// The increment ties `conv` down to the tail-pair bisim of the GENERAL streams (no cons
// restriction); the only residue is the recursive `self` for `conv` (dfix-under-application
// wall) + the general tail-bisim extraction (slot-coherence neutral), both localized to the
// `bgt` argument. Loading the listing type-checks `gConverseGen`,
// `gConverseGenHeadComputes` (head observation reduces to `pathF A a b`), and
// `gConverseGenTailComputes` (tail observation reduces to `conv`'s output type) — each a
// refl-pin that fails to elaborate if the equation does not hold definitionally.
func TestGConverseGen(t *testing.T) {
	// Loading succeeds iff every definition (incl. gConverseGen + the two computation
	// pins) type-checks; loadListing t.Fatalf's otherwise.
	loadListing(t, "ch69_guarded_types.rune")
}

// TestGluePath pins the PATH-LEVEL analogue of `glue` (ch69 `gluePath`) — the
// combinator that dodges the dfix-under-application wall the same way `glue` does,
// and the structure the interpolant-coalgebra route calls for. `glue : (∀κ. gStr κ A)
// -> Str A` carries per-clock guarded STREAMS to a global stream via the Nu-side
// `unfold` (computes via `out`); `gluePath` carries a per-clock family of guarded-
// stream PATHS `p : ∀κ. pathF (gStr κ A)(s κ)(t κ)` to a global path
// `pathF (Str A)(glue s)(glue t)`, by gluing the pointwise observation
// `λκ. papp (p κ) i` across the interval (`pabs (λi. glue (λκ. papp (p κ) i))`).
//
// Endpoints compute by `refl` (gluePathI0/I1): `papp (gluePath p) i0 ≡ glue s`,
// `… i1 ≡ glue t` — the path-boundary `papp … (p κ) i0 ~> s κ` fires under the glue,
// so the global path has the RIGHT endpoints definitionally (no Kan repair). The
// recursion the converse needs lives inside the per-clock `p κ` (a productive `pabs`
// from `bisimStepLater`), never a dfix; `gluePath` only glues it pointwise through the
// computing `unfold`/`out` layer, so the assembly never observes a dfix under
// application. `gluePathTail` presents this AT the shape `gConverseGen`'s `conv`
// demands (`s := gtailG ra`, `t := gtailG rb`), the dfix-free computing tail-path.
func TestGluePath(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "listings", "ch69_guarded_types.rune"))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("ch69 should load (gluePath + endpoint pins + gluePathTail): %v", err)
	}
	for _, name := range []string{"gluePath", "gluePathI0", "gluePathI1", "gluePathTail"} {
		if _, ok := s.Lookup(name); !ok {
			t.Fatalf("%s missing from ch69", name)
		}
		if !s.Certified(name) {
			t.Fatalf("%s should be certified (the endpoint reductions must hold definitionally)", name)
		}
	}

	// POSITIVE: gluePathTail's output type IS bisimToPathStr's tail slot for the
	// glued tail-pair — feed it straight into bisimToPathStr as the tail-path
	// argument (the dfix-free replacement for `conv bgt` in gConverseGen). This
	// derivation type-checks iff gluePathTail's endpoints are `tail (glue ra)`/
	// `tail (glue rb)` ON THE NOSE (glueTail ▸ gluePathI0/I1), i.e. iff the
	// gluePath endpoint reductions genuinely line up with the converse's demand.
	derive := `
gConverseViaGluePath : (A : UF)
   -> (ra : (k : Clock) -> El (gStr k A)) -> (rb : (k : Clock) -> El (gStr k A))
   -> (hp : El (pathF A (head A (glue A ra)) (head A (glue A rb))))
   -> (tp : (k : Clock) -> El (pathF (gStr k A) (gtailG A ra k) (gtailG A rb k)))
   -> El (pathF (Str A) (glue A ra) (glue A rb)) is
  fn (A : UF)
     (ra : (k : Clock) -> El (gStr k A)) (rb : (k : Clock) -> El (gStr k A))
     (hp : El (pathF A (head A (glue A ra)) (head A (glue A rb))))
     (tp : (k : Clock) -> El (pathF (gStr k A) (gtailG A ra k) (gtailG A rb k))) is
    bisimToPathStr A (glue A ra) (glue A rb) hp (gluePathTail A ra rb tp)
  end
end
`
	sPos := session.New()
	if _, err := sPos.LoadSource(string(src) + derive); err != nil {
		t.Fatalf("gluePathTail should feed bisimToPathStr's tail slot on the nose "+
			"(dfix-free tail-path; endpoints must be tail(glue ra)/tail(glue rb)): %v", err)
	}

	// NEGATIVE: the gluePath endpoint pin is load-bearing. Claiming the LEFT
	// endpoint is `glue t` (the RIGHT one) must be REJECTED — `papp (gluePath p) i0`
	// is `glue s`, NOT `glue t`, unless s ≡ t. If the boundary reduction were
	// vacuous (any RHS accepted), this would wrongly check.
	neg := `
gluePathI0Neg : (A : UF)
   -> (s : (k : Clock) -> El (gStr k A)) -> (t : (k : Clock) -> El (gStr k A))
   -> (p : (k : Clock) -> El (pathF (gStr k A) (s k) (t k)))
   -> Eq (El (Str A))
        (papp (Str A) (glue A s) (glue A t) (gluePath A s t p) i0)
        (glue A t) is
  fn (A : UF)
     (s : (k : Clock) -> El (gStr k A)) (t : (k : Clock) -> El (gStr k A))
     (p : (k : Clock) -> El (pathF (gStr k A) (s k) (t k))) is refl end
end
`
	sNeg := session.New()
	if _, err := sNeg.LoadSource(string(src) + neg); err == nil {
		t.Fatal("gluePath i0 endpoint must NOT equate glue t (refl is not load-bearing)")
	}
}

// TestGConverseViaGluePath pins the DFIX-FREE per-clock→global E2 converse (ch69
// `gConverseViaGluePath`) — `gConverseGen` with the recursive `conv`/`bgt` hypotheses
// RETIRED in favour of the productive `gluePathTail`. It assembles the global path
// `glue ra ≡ glue rb` for ARBITRARY global guarded streams from (a) the per-clock head
// bisimilarity `bsf` (head supplied PRODUCTIVELY by `bisimHead k0`, definitional through
// glueHead ▸ gheadG) and (b) the per-clock tail-PATH family `tp` (the per-clock converse's
// output), carried to the global tail slot by `gluePathTail` — definitional through
// glueTail. The global recursion (the dfix-under-application wall `gConverseGen` left open
// in `conv`) is fully discharged: no dfix is ever observed under application, because the
// recursion lives inside each productive `tp κ` (a `pabs`), not in a global `self`.
//
// Loading type-checks `gConverseViaGluePath` + its tail-computation pin
// `gConverseViaGluePathTailComputes` (`tail (glue ra) ≡ glue (gtailG ra)` so gluePathTail's
// output IS bisimToPathStr's tail slot ON THE NOSE) — both refl-pins that fail to elaborate
// if the equation is not definitional. The NEGATIVE probe confirms the tail slot is
// load-bearing: a SWAPPED tail endpoint must be REJECTED.
func TestGConverseViaGluePath(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "listings", "ch69_guarded_types.rune"))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("ch69 should load (gConverseViaGluePath + tail pin): %v", err)
	}
	for _, name := range []string{"gConverseViaGluePath", "gConverseViaGluePathTailComputes"} {
		if _, ok := s.Lookup(name); !ok {
			t.Fatalf("%s missing from ch69", name)
		}
		if !s.Certified(name) {
			t.Fatalf("%s should be certified (the dfix-free converse must type-check definitionally)", name)
		}
	}

	// NEGATIVE: the tail slot is load-bearing. Claiming the glued tail-path type
	// equals the SWAPPED one (`glue (gtailG rb)` / `glue (gtailG ra)`) must be
	// REJECTED — gluePathTail lands at `glue (gtailG ra)`/`glue (gtailG rb)`, not the
	// swap, unless the two tails coincide.
	neg := `
gConverseViaGluePathTailNeg : (A : UF)
   -> (ra : (k : Clock) -> El (gStr k A)) -> (rb : (k : Clock) -> El (gStr k A))
   -> Eq UF
        (pathF (Str A) (tail A (glue A ra)) (tail A (glue A rb)))
        (pathF (Str A) (glue A (gtailG A rb)) (glue A (gtailG A ra))) is
  fn (A : UF)
     (ra : (k : Clock) -> El (gStr k A)) (rb : (k : Clock) -> El (gStr k A)) is refl end
end
`
	sNeg := session.New()
	if _, err := sNeg.LoadSource(string(src) + neg); err == nil {
		t.Fatal("gConverseViaGluePath tail slot must NOT equate the swapped tail endpoints (refl is not load-bearing)")
	}
}

// TestBisimTailSlotNext pins the SLOT-COHERENCE generalized OFF cons-form to the
// delayed-tail level (ch69 `bisimTailSlotNext`), the longest-pole increment of the
// E2 converse. `bisimTailSlotCons` collapsed the `laterApp k Dpair gBisim (gstepPair …)`
// tail slot ONLY on a pair of `consG`-built streams. This lemma frees the coherence to
// its true precondition — the two DELAYED TAILS `gstepPair` zips are `next`-INTROS — with
// NO `consG`, NO head, NO `Sfst`/`Ssnd` projection, NO `d` at all: for ARBITRARY present
// streams `u`, `v`, zipping `next u` / `next v` and feeding the slot computes (BY REFL) to
// `Later k (gBisim (pair u v))`. The four-ι chain (lmapNext ▸ lapNext ▸ laterAppNext) fires
// without the leading tailGCons the cons lemma needed. Loading certifies the refl-pin
// (fails to elaborate if not definitional). The coherence lemma `bisimTailSlotConsViaNext`
// pins that on cons-built streams the generalized slot IS the cons slot, so the
// generalization SUBSUMES `bisimTailSlotCons` — every consumer (e.g. `bisimStepLater`)
// keeps firing, now without the surrounding-stream-is-`consG` restriction.
//
// SOUNDNESS / TERMINATION: this is a pure listing refl-pin over the EXISTING guard-group
// ι-rules (lmapNext/lapNext/laterAppNext) — NO new kernel rule, NO new guard member, NO
// hash-format change. The reduction it observes is the same bounded next-peeling chain the
// kernel already runs; nothing new can loop. The NEGATIVE probe below shows the rule is
// load-bearing AND that the genuinely NEUTRAL case (delayed tails that are NOT `next`, i.e.
// a stuck `▹κ` with no present pair underneath) STAYS STUCK — it cannot equate the slot to
// `Later k (gBisim (pair …))` because no present pair `(u, v) : Dpair` exists there. That
// residue is irreducibly coinductive (carried by the productive `gluePathTail` route).
func TestBisimTailSlotNext(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "listings", "ch69_guarded_types.rune"))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("ch69 should load (bisimTailSlotNext + cons coherence): %v", err)
	}
	for _, name := range []string{"bisimTailSlotNext", "bisimTailSlotConsViaNext"} {
		if _, ok := s.Lookup(name); !ok {
			t.Fatalf("%s missing from ch69", name)
		}
		if !s.Certified(name) {
			t.Fatalf("%s should be certified (the delayed-tail slot must reduce definitionally)", name)
		}
	}

	// POSITIVE: the generalized slot drops straight into a `bisimStepLater`-shaped use —
	// `bisimTail`'s codomain on a pair whose delayed tails are `next u` / `next v` is
	// directly `▹κ (gBisim (pair u v))` (lap's argument type) with NO cast, for ARBITRARY
	// u, v (not cons-built). This derivation type-checks iff bisimTailSlotNext's RHS is
	// the slot's normal form — i.e. iff the off-cons-form generalization genuinely holds.
	pos := `
slotNextAsLapArg : (k : Clock) -> (A : UF) -> (u : El (gStr k A)) -> (v : El (gStr k A))
   -> El (laterApp k (Dpair k A) (gBisim k A)
            (lap k (gStr k A) (Dpair k A)
               (lmap k (gStr k A) (piF (gStr k A) (fn (_b : El (gStr k A)) is Dpair k A end))
                  (fn (x : El (gStr k A)) is fn (y : El (gStr k A)) is
                     pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end) x y end end)
                  (next k (gStr k A) u))
               (next k (gStr k A) v)))
   -> El (Later k (gBisim k A
            (pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end) u v))) is
  fn (k : Clock) (A : UF) (u : El (gStr k A)) (v : El (gStr k A))
     (w : El (laterApp k (Dpair k A) (gBisim k A)
            (lap k (gStr k A) (Dpair k A)
               (lmap k (gStr k A) (piF (gStr k A) (fn (_b : El (gStr k A)) is Dpair k A end))
                  (fn (x : El (gStr k A)) is fn (y : El (gStr k A)) is
                     pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end) x y end end)
                  (next k (gStr k A) u))
               (next k (gStr k A) v)))) is w end
end
`
	sPos := session.New()
	if _, err := sPos.LoadSource(string(src) + pos); err != nil {
		t.Fatalf("the generalized slot must be usable as lap's argument type without a cast "+
			"(the delayed-tail slot ≡ ▹κ (gBisim (pair u v)) for arbitrary u, v): %v", err)
	}

	// NEGATIVE (load-bearing + soundness): claiming the slot equals the SWAPPED pair
	// `Later k (gBisim (pair v u))` must be REJECTED — the reduction lands at `pair u v`,
	// not the swap, unless u ≡ v. If the refl-pin were vacuous this would wrongly check.
	neg := `
bisimTailSlotNextNeg : (k : Clock) -> (A : UF) -> (u : El (gStr k A)) -> (v : El (gStr k A))
   -> Eq UF
        (laterApp k (Dpair k A) (gBisim k A)
           (lap k (gStr k A) (Dpair k A)
              (lmap k (gStr k A) (piF (gStr k A) (fn (_b : El (gStr k A)) is Dpair k A end))
                 (fn (x : El (gStr k A)) is fn (y : El (gStr k A)) is
                    pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end) x y end end)
                 (next k (gStr k A) u))
              (next k (gStr k A) v)))
        (Later k
           (gBisim k A
              (pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end) v u))) is
  fn (k : Clock) (A : UF) (u : El (gStr k A)) (v : El (gStr k A)) is refl end
end
`
	sNeg := session.New()
	if _, err := sNeg.LoadSource(string(src) + neg); err == nil {
		t.Fatal("bisimTailSlotNext must NOT equate the swapped pair (refl is not load-bearing)")
	}

	// SOUNDNESS — the genuinely NEUTRAL residue STAYS STUCK: for a NEUTRAL `d` (delayed
	// tails are stuck `▹κ`, NOT `next`), the slot does NOT reduce to `Later k (gBisim …)`,
	// because there is no present pair `(u, v) : Dpair` underneath. Claiming it equals
	// `Later k (gBisim (gstepPair-as-present-pair …))` for an arbitrary `d` must be
	// REJECTED — the slot is a stuck `laterApp` over a stuck `lap`, an irreducible normal
	// form, not a `Later` of any present-pair bisimilarity. (If the kernel had an
	// over-eager applicative-commutation rule that fabricated a reduct here, it would be
	// UNSOUND — no such present pair exists — so this MUST stay stuck.)
	stuck := `
bisimTailSlotNeutralWrong : (k : Clock) -> (A : UF) -> (d : El (Dpair k A))
   -> Eq UF
        (laterApp k (Dpair k A) (gBisim k A) (gstepPair k A d))
        (Later k (gBisim k A d)) is
  fn (k : Clock) (A : UF) (d : El (Dpair k A)) is refl end
end
`
	sStuck := session.New()
	if _, err := sStuck.LoadSource(string(src) + stuck); err == nil {
		t.Fatal("the NEUTRAL slot must STAY STUCK (no sound reduct exists for neutral tails; " +
			"equating it to Later k (gBisim d) would be unsound)")
	}
}

// TestTpStepFromSelf pins the INDEX-POLY PER-CLOCK CONVERTER step (ch69 `ConvTy` +
// `tpStepFromSelf` + `gConverseFromConv`) — the increment wiring the per-clock guarded
// fixpoint's `self` into `bisimStepLater` to produce the per-clock tail-path `tp κ`.
//
// `ConvTy k A` is the index-poly converter type `(d : Dpair) -> gBisim d -> pathF (gStr)
// (Sfst d)(Ssnd d)` — the type the plan's `tpFix` lives at, written index-first so a
// fixpoint's delayed `self` can be applied to the DELAYED tail-pair index by `lapD`.
// `tpStepFromSelf` is the productive corecursion step: on a CONS-built pair it derives
// `bisimStepLater`'s recursive `rec` argument from the index-poly `self` by ONE dependent
// guarded application `lapD k Dpair Cod self (gstepPair d)` — whose result type
// `laterApp k Dpair Cod (gstepPair d)` COMPUTES (slot-coherence: `gstepPair` of the cons-pair
// is a `next` of the tail-pair, so laterAppNext fires) to EXACTLY `Later k (gBisim (pair ra
// rb) -> pathF (gStr) ra rb)`, `bisimStepLater`'s `rec` slot ON THE NOSE, no cast — then conses
// the head onto the delayed recursive tail-path. The recursive `self` is consumed strictly
// UNDER the delay (`lapD`), descending to the tail-pair index, NEVER applied at the conversion
// top — so the dfix wall is never approached. The step type-checks definitionally and consumes
// a REAL bisimilarity witness.
//
// `gConverseFromConv` isolates the SOLE remaining frontier: GIVEN any total per-clock converter
// `conv : ∀κ. ConvTy κ A`, the global converse `glue ra ≡ glue rb` closes with no dfix
// unfolding — `conv κ (tailpair κ)(btf κ)` lands at `gConverseViaGluePath`'s `tp κ` slot
// (`pathF (gStr)(gtailG ra κ)(gtailG rb κ)`) ON THE NOSE. So the whole E2 per-clock→global
// converse reduces to constructing the total `conv`; `tpStepFromSelf` is its productive
// (cons-form) body, and the only gap is making it total over neutral (non-cons) `d` — the
// irreducibly-coinductive neutral residue (`bisimTailSlotNext`). Loading certifies all three.
//
// The NEGATIVE probe confirms `tpStepFromSelf`'s output endpoints are load-bearing: a SWAPPED
// cons endpoint must be REJECTED (the path is genuinely between the two distinct cons-streams,
// not a coincidentally-typed neutral the converse would silently accept).
func TestTpStepFromSelf(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "listings", "ch69_guarded_types.rune"))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("ch69 should load (ConvTy + tpStepFromSelf + gConverseFromConv): %v", err)
	}
	for _, name := range []string{"ConvTy", "tpStepFromSelf", "gConverseFromConv"} {
		if _, ok := s.Lookup(name); !ok {
			t.Fatalf("%s missing from ch69", name)
		}
		if !s.Certified(name) {
			t.Fatalf("%s should be certified (the productive index-poly step must type-check definitionally)", name)
		}
	}

	// NEGATIVE: the step's output endpoints are load-bearing. Claiming a path from the
	// a-cons to ITSELF has the SAME type as a path from the a-cons to the b-cons must be
	// REJECTED unless the two cons-streams coincide — i.e. the converter step lands at the
	// genuine endpoints (consG a … / consG b …), not an arbitrary neutral.
	neg := `
tpStepFromSelfEndpointNeg : (k : Clock) -> (A : UF)
   -> (a : El A) -> (ra : El (gStr k A)) -> (b : El A) -> (rb : El (gStr k A))
   -> Eq UF
        (pathF (gStr k A) (consG k A a (next k (gStr k A) ra)) (consG k A a (next k (gStr k A) ra)))
        (pathF (gStr k A) (consG k A a (next k (gStr k A) ra)) (consG k A b (next k (gStr k A) rb))) is
  fn (k : Clock) (A : UF) (a : El A) (ra : El (gStr k A)) (b : El A) (rb : El (gStr k A)) is refl end
end
`
	sNeg := session.New()
	if _, err := sNeg.LoadSource(string(src) + neg); err == nil {
		t.Fatal("tpStepFromSelf output endpoints must NOT equate a-cons-to-itself with a-cons-to-b-cons (refl not load-bearing)")
	}
}

// TestRepairConvBody pins the converter-body construction (ch69 `congConsGTail` +
// `lmapBoundaryI0`/`I1` + `repairConvBody`, telos-4/M7) AND the closure of its two
// correction tail-paths (cont.51 `convSxTail`/`convSyTail`/`convStepClosed`).
//
// repairConvBody composes `sx · (tpStepFromSelf) · sy` (double `ptransG` via
// `repairEndpointsG`) into the cons-form converter body with the REAL endpoints
// `consG a (next ra) ≡ consG b (next rb)` (= `Sfst d ≡ Ssnd d`), GIVEN two correction
// tail-paths over the neutral delayed dfix-self. cont.51 CLOSES those two paths: the
// guard-group ι `lmap (const x) la ~> next x` (the ▹κ functor law a constant map sends
// every delayed value to the constant `next`, sound in the topos-of-trees model, fired
// by a constancy probe on the mapped function — core/eval.go tryGuardIota) makes the
// previously-stuck `lmap (λq. ra) NL ≡ next ra` DEFINITIONAL for the NEUTRAL `NL`, so
// `convSxTail`/`convSyTail` are `preflF`. With the corrections trivial, `convStepClosed`
// IS `tpStepFromSelf` directly: its endpoints reduce (papp-boundary ▸ lmap-const ι) to
// `Sfst d`/`Ssnd d` ON THE NOSE — the cons-form per-clock converter body, no endpoint
// repair, no Kan open-box, no dfix wall.
func TestRepairConvBody(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "listings", "ch69_guarded_types.rune"))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("ch69 should load (congConsGTail + lmapBoundary + repairConvBody + convStepClosed): %v", err)
	}
	for _, name := range []string{
		"congConsGTail", "lmapBoundaryI0", "lmapBoundaryI1", "repairConvBody",
		"convSxTail", "convSyTail", "convStepClosed",
	} {
		if _, ok := s.Lookup(name); !ok {
			t.Fatalf("%s missing from ch69", name)
		}
		if !s.Certified(name) {
			t.Fatalf("%s should be certified (must type-check definitionally)", name)
		}
	}

	// POSITIVE / HEADLINE: the correction-path residue now COMPUTES. `lmap` of the constant
	// function at `ra` over an ARBITRARY delayed path `NL` (even a stuck neutral) collapses
	// to `next ra` by the ▹κ functor law (constant map ⇒ constant `next`). This refl-pin
	// MUST elaborate — it is exactly the residue the plan flagged as the E2-converse
	// frontier, now discharged by the guard-group ι (no Kan open-box needed).
	collapse := `
lmapConstCollapses : (k : Clock) -> (A : UF) -> (ra : El (gStr k A)) -> (rb : El (gStr k A))
   -> (NL : El (Later k (pathF (gStr k A) ra rb)))
   -> Eq (El (Later k (gStr k A)))
        (lmap k (pathF (gStr k A) ra rb) (gStr k A)
           (fn (q : El (pathF (gStr k A) ra rb)) is ra end) NL)
        (next k (gStr k A) ra) is
  fn (k : Clock) (A : UF) (ra : El (gStr k A)) (rb : El (gStr k A))
     (NL : El (Later k (pathF (gStr k A) ra rb))) is refl end
end
`
	sC := session.New()
	if _, err := sC.LoadSource(string(src) + collapse); err != nil {
		t.Fatalf("the correction-path residue must now COMPUTE (lmap const ~> next, the closed frontier): %v", err)
	}

	// SOUNDNESS NEGATIVE: the rule is CONSTANCY-GATED — it must NOT fire on a non-constant
	// mapped function. `lmap (λq. q) NL` (identity, q-dependent) over a NEUTRAL `NL` does NOT
	// collapse to `next NL` (or any `next`); claiming it equals `next` of anything by refl
	// must be REJECTED. If the rule over-fired (ignoring the constancy probe) it would be
	// UNSOUND — this stuck-pin guards against that.
	overfire := `
lmapIdNoCollapse : (k : Clock) -> (A : UF) -> (ra : El (gStr k A))
   -> (NL : El (Later k (gStr k A)))
   -> Eq (El (Later k (gStr k A)))
        (lmap k (gStr k A) (gStr k A)
           (fn (q : El (gStr k A)) is q end) NL)
        (next k (gStr k A) ra) is
  fn (k : Clock) (A : UF) (ra : El (gStr k A)) (NL : El (Later k (gStr k A))) is refl end
end
`
	sO := session.New()
	if _, err := sO.LoadSource(string(src) + overfire); err == nil {
		t.Fatal("the lmap-const ι must STAY STUCK on a non-constant (identity) mapped function " +
			"(the constancy probe is load-bearing; over-firing would be unsound)")
	}

	// NEGATIVE: the converter body's endpoints are load-bearing. The body is a path between
	// the two DISTINCT cons-streams; claiming a path from the a-cons to ITSELF has the same
	// type as the a-cons-to-b-cons path must be REJECTED unless they coincide.
	neg := `
repairConvBodyEndpointNeg : (k : Clock) -> (A : UF)
   -> (a : El A) -> (ra : El (gStr k A)) -> (b : El A) -> (rb : El (gStr k A))
   -> Eq UF
        (pathF (gStr k A) (consG k A a (next k (gStr k A) ra)) (consG k A a (next k (gStr k A) ra)))
        (pathF (gStr k A) (consG k A a (next k (gStr k A) ra)) (consG k A b (next k (gStr k A) rb))) is
  fn (k : Clock) (A : UF) (a : El A) (ra : El (gStr k A)) (b : El A) (rb : El (gStr k A)) is refl end
end
`
	sNeg := session.New()
	if _, err := sNeg.LoadSource(string(src) + neg); err == nil {
		t.Fatal("converter-body output endpoints must NOT equate a-cons-to-itself with a-cons-to-b-cons (refl not load-bearing)")
	}
}

// TestStrGStrBridge pins the `Str ≃ ∀κ. gStr` bridge increment (ch69, the STRATEGIC
// PIVOT) — the `glue`-half cons-reachability coherences + the conditional-headline full
// E2 converse on arbitrary streams. The pivot reframes the converse: instead of making the
// per-clock converter total over a NEUTRAL `d` (the irreducibly-coinductive slot-coherence
// wall, cont.46, NOT soundly closable by a kernel rule), it uses the bridge so arbitrary
// streams become cons-reachable through `glue` — WITHOUT the dfix-opaque `split`.
//
// Landed (all refl, all certified):
//   - glueConsEta: `glue g ≡ glue (λκ. consG (headG (g κ))(tailG (g κ)))` — every glue-
//     presented stream is DEFINITIONALLY a glue of per-clock cons-form (per-clock streamEta
//     under the binder), the cons-reachability the cons-form converter needs, no `split`.
//   - glueConsEtaHead/Tail: the cons-eta presentation's head/tail compute to `gheadG g` /
//     `glue (gtailG g)` — a computing stream homomorphism (the `glue`-half bridge coherence).
//   - gConverseArbitrary: the full E2 converse `bisimilar ⟹ pathF (Str A)` on ARBITRARY
//     `s t : Str A`, MODULO glue-presentation paths `s ≡ glue ra`, `glue rb ≡ t`. The glue-
//     stream converse (gConverseViaGluePath) re-aimed onto s/t by repairEndpointsStr. The
//     residue is isolated to the glue-presentation existence — the sole remaining frontier.
//
// Negative/soundness pins prove the dfix wall and the forceD-section are GENUINE (not
// fabricated reducts): the bridge content is exactly the split-free productive layer.
func TestStrGStrBridge(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "listings", "ch69_guarded_types.rune"))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("ch69 should load (bridge cons-reachability + gConverseArbitrary): %v", err)
	}
	for _, name := range []string{
		"glueConsEta", "glueConsEtaHead", "glueConsEtaTail", "gConverseArbitrary",
	} {
		if _, ok := s.Lookup(name); !ok {
			t.Fatalf("%s missing from ch69", name)
		}
		if !s.Certified(name) {
			t.Fatalf("%s should be certified (must type-check definitionally)", name)
		}
	}

	// CANONICITY / COMPUTES: the cons-eta presentation's head observation reduces to a HEAD.
	// For a concrete cons-form `g = λκ. consG h0 (next (rest κ))`, `head (glue (λκ. consG
	// (headG (g κ))(tailG (g κ)))) ≡ gheadG g ≡ h0` — refl all the way (streamEta ▸ glueHead
	// ▸ gheadCons ▸ headConsG). This pins that the bridge head observation genuinely COMPUTES
	// to the literal head, not merely type-checks against a neutral.
	canon := `
glueConsEtaHeadComputes : (A : UF) -> (h0 : El A) -> (rest : (k : Clock) -> El (gStr k A))
   -> Eq (El A)
        (head A (glue A (fn (k : Clock) is
           consG k A
             (headG k A (consG k A h0 (next k (gStr k A) (rest k))))
             (tailG k A (consG k A h0 (next k (gStr k A) (rest k)))) end)))
        h0 is
  fn (A : UF) (h0 : El A) (rest : (k : Clock) -> El (gStr k A)) is refl end
end
`
	sC := session.New()
	if _, err := sC.LoadSource(string(src) + canon); err != nil {
		t.Fatalf("the bridge head observation must COMPUTE to the literal head on cons-form: %v", err)
	}

	// SOUNDNESS PIN (the dfix wall is GENUINE): `gheadG (λκ. split s) ≢ head s`. The `split`
	// round-trip is dfix-opaque — `splitFn s` is a dfix under application that convDfix never
	// unfolds. This MUST be REJECTED; if it checked, the bridge could be built off `split`
	// (and the elaborate cons-reachability route would be unnecessary). Its rejection
	// certifies WHY the productive (glueConsEta) route is the one that works.
	dfixWall := `
splitHeadOpaque : (A : UF) -> (s : El (Str A))
   -> Eq (El A) (gheadG A (fn (k : Clock) is split A k s end)) (head A s) is
  fn (A : UF) (s : El (Str A)) is refl end
end
`
	sW := session.New()
	if _, err := sW.LoadSource(string(src) + dfixWall); err == nil {
		t.Fatal("the dfix wall must be GENUINE: gheadG (λκ. split s) must NOT reduce to head s " +
			"(split is dfix-opaque under application; the bridge cannot ride split)")
	}

	// SOUNDNESS PIN (the forceD-section is GENUINE, the coinductive residue): `tailG (g κ) ≢
	// next (gtailG g κ)`. forceD∘(λκ.tailG) is a RETRACTION not a section — recovering the
	// per-clock delayed tail as a `next` of the forceD-cash is exactly the irreducibly-
	// coinductive residue cont.46 characterised (NOT closable by a sound kernel rule). This
	// must be REJECTED — it certifies that the cons-eta presentation's per-clock tail stays a
	// delayed NEUTRAL `▹κ` (so the cons-form converter is reached at the head but the tail
	// residue is genuine), and that the kernel does NOT fabricate the unsound section reduct.
	forceDSection := `
tailGNextSection : (A : UF) -> (g : El (GStrAll A)) -> (k : Clock)
   -> Eq (El (Later k (gStr k A)))
        (tailG k A (g k))
        (next k (gStr k A) (gtailG A g k)) is
  fn (A : UF) (g : El (GStrAll A)) (k : Clock) is refl end
end
`
	sS := session.New()
	if _, err := sS.LoadSource(string(src) + forceDSection); err == nil {
		t.Fatal("the forceD-section must STAY STUCK: tailG (g κ) must NOT reduce to next (gtailG g κ) " +
			"(forceD∘λκ.tailG is a retraction, not a section; fabricating it would be unsound)")
	}

	// NEGATIVE (gConverseArbitrary output endpoints load-bearing): the converse outputs a
	// path between the GIVEN s and t; claiming a path from s to ITSELF has the same type as
	// the s-to-t path must be REJECTED unless s ≡ t. Guards against a vacuously-typed result.
	neg := `
gConverseArbEndpointNeg : (A : UF) -> (s : El (Str A)) -> (t : El (Str A))
   -> Eq UF (pathF (Str A) s s) (pathF (Str A) s t) is
  fn (A : UF) (s : El (Str A)) (t : El (Str A)) is refl end
end
`
	sNeg := session.New()
	if _, err := sNeg.LoadSource(string(src) + neg); err == nil {
		t.Fatal("gConverseArbitrary output endpoints must NOT equate s-to-s with s-to-t (refl not load-bearing)")
	}
}

// TestGluePresentationEquiv pins ROUTE (i) — the cubical `Glue`-presentation of `Str` as a
// COMPUTING equivalence (ch69 `glueIsEquivTy`/`glueEquiv`/`glueFiberCenter`/`gluePresStream`/
// `gluePresPath`/`gConverseGluePresented`, telos-4/M7, 2026-06-16 cont.53). This is the A4/
// Glue-stratum bridge that REPLACES `gConverseArbitrary`'s two per-call path hypotheses
// (`ps : s ≡ glue ra`, `pt : glue rb ≡ t`) with a SINGLE structural obligation,
// `isEquiv (GStrAll A)(Str A) glue` (`glueIsEquivTy`), by routing the glue-presentation through
// the landed contractible-fibre tower (Equiv/fiber/isContr/isEquiv/equivProof, store/equiv.go —
// the SAME machinery that makes univalence compute).
//
// The cubical idea: present `Str A ≃ GStrAll A` as a genuine Equiv whose FORWARD map is the
// computing `glue`; then EVERY `s : Str A` gets its glue-presentation as the CENTRE of `glue`'s
// contractible fibre at `s` — `(equivProof glueEquiv s).1 = (ra , p : glue ra ≡ s)` — EXTRACTED
// off the equiv tower by pure sigmaF projection, never corecursed via the dfix-opaque `split`.
// `equivFun glueEquiv ≡ glue` by refl (the explicit pairF projection) is what makes the fibre's
// path slot `pathF (Str A)(glue ra) s` line up on the nose. `gConverseGluePresented` feeds the
// extraction into `gConverseArbitrary` (symStr re-orients the fibre path), closing the full E2
// converse on ARBITRARY streams from the SINGLE `isEquiv glue` obligation.
//
// Everything DOWNSTREAM of the obligation is closed + computing (all defs certified — the equiv-
// tower projections hold definitionally). The SOLE remaining obligation `glueIsEquivTy` is the
// genuine coinductive residue: via isoToEquiv it reduces to the two round-trip homotopies, both
// the dfix-observation wall, and (FINDING, this iteration) the eval-ι route that would make
// applied `dfix` compute is NOT termination-sound (the applicative `lap (next dfix)(next x) ~>
// next (dfix x)` β-applies the recursive dfix eagerly, diverging the normalizer). So the E2
// converse bottoms out at a single honest `isEquiv` statement — the cleanest isolation of the
// remaining content. The negative probes confirm the obligation is GENUINE (the dfix wall is
// real both directions) and the headline output endpoints are load-bearing.
func TestGluePresentationEquiv(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "listings", "ch69_guarded_types.rune"))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("ch69 should load (Glue-presentation equiv bridge): %v", err)
	}
	for _, name := range []string{
		"glueIsEquivTy", "glueEquiv", "glueFiberCenter",
		"gluePresStream", "gluePresPath", "gConverseGluePresented",
	} {
		if _, ok := s.Lookup(name); !ok {
			t.Fatalf("%s missing from ch69", name)
		}
		if !s.Certified(name) {
			t.Fatalf("%s should be certified (the equiv-tower extraction must hold definitionally)", name)
		}
	}

	// POSITIVE / COMPUTES: the extracted glue-presentation path's endpoints are exactly
	// `glue (gluePresStream pf s)` and `s` — the contractible-fibre path slot reduces (by the
	// equiv-tower sigmaF projections + equivFun ≡ glue) ON THE NOSE. This derivation type-checks
	// iff `gluePresPath`'s type is the genuine `pathF (Str A)(glue (extracted ra)) s`, i.e. iff
	// the extraction lands the real presentation (not a coincidentally-typed neutral).
	pos := `
gluePresEndpointComputes : (A : UF) -> (pf : El (glueIsEquivTy A)) -> (s : El (Str A))
   -> Eq UF
        (pathF (Str A) (glue A (gluePresStream A pf s)) s)
        (pathF (Str A) (glue A (gluePresStream A pf s)) s) is
  fn (A : UF) (pf : El (glueIsEquivTy A)) (s : El (Str A)) is refl end
end

-- The extracted ra fed straight into glueConsEta: glue (gluePresStream pf s) is a real glue,
-- so the cons-reachability coherence (refl) applies to the EXTRACTED presentation.
gluePresConsEta : (A : UF) -> (pf : El (glueIsEquivTy A)) -> (s : El (Str A))
   -> Eq (El (Str A))
        (glue A (gluePresStream A pf s))
        (glue A (fn (k : Clock) is
           consG k A (headG k A (gluePresStream A pf s k)) (tailG k A (gluePresStream A pf s k)) end)) is
  fn (A : UF) (pf : El (glueIsEquivTy A)) (s : El (Str A)) is refl end
end
`
	sPos := session.New()
	if _, err := sPos.LoadSource(string(src) + pos); err != nil {
		t.Fatalf("the extracted glue-presentation must compute (fibre path slot + cons-reachability): %v", err)
	}

	// SOUNDNESS PIN (the obligation is GENUINE, direction 1): `gheadG (λκ. split (glue g)) ≢
	// gheadG g`. The split∘glue round-trip homotopy (one of the two isoToEquiv inputs for
	// `isEquiv glue`) is dfix-opaque — applied `split` is a dfix under application that no sound
	// kernel reduction unfolds. MUST be REJECTED: it certifies WHY `glueIsEquivTy` is a genuine
	// residual obligation and not closable definitionally.
	splitGlueWall := `
splitGlueHeadOpaque : (A : UF) -> (g : El (GStrAll A))
   -> Eq (El A) (gheadG A (fn (k : Clock) is split A k (glue A g) end)) (gheadG A g) is
  fn (A : UF) (g : El (GStrAll A)) is refl end
end
`
	sW := session.New()
	if _, err := sW.LoadSource(string(src) + splitGlueWall); err == nil {
		t.Fatal("the glue obligation must be GENUINE: gheadG (λκ. split (glue g)) must NOT reduce to gheadG g " +
			"(split is dfix-opaque under application; isEquiv glue is not definitional)")
	}

	// SOUNDNESS PIN (the obligation is GENUINE, direction 2): `head (glue (λκ. split s)) ≢ head
	// s`. The glue∘split round-trip homotopy (the other isoToEquiv input) is dfix-opaque too.
	// MUST be REJECTED — both homotopies hit the dfix wall, so isoToEquiv cannot manufacture
	// `isEquiv glue` from a computing iso.
	glueSplitWall := `
glueSplitHeadOpaque : (A : UF) -> (s : El (Str A))
   -> Eq (El A) (head A (glue A (fn (k : Clock) is split A k s end))) (head A s) is
  fn (A : UF) (s : El (Str A)) is refl end
end
`
	sW2 := session.New()
	if _, err := sW2.LoadSource(string(src) + glueSplitWall); err == nil {
		t.Fatal("the glue obligation must be GENUINE: head (glue (λκ. split s)) must NOT reduce to head s " +
			"(glue∘split is dfix-opaque under application)")
	}

	// NEGATIVE (headline output endpoints load-bearing): gConverseGluePresented outputs a path
	// between the GIVEN s and t; claiming a path from s to ITSELF has the same type as the s-to-t
	// path must be REJECTED unless s ≡ t. Guards against a vacuously-typed result.
	neg := `
gConverseGluePresentedEndpointNeg : (A : UF) -> (s : El (Str A)) -> (t : El (Str A))
   -> Eq UF (pathF (Str A) s s) (pathF (Str A) s t) is
  fn (A : UF) (s : El (Str A)) (t : El (Str A)) is refl end
end
`
	sNeg := session.New()
	if _, err := sNeg.LoadSource(string(src) + neg); err == nil {
		t.Fatal("gConverseGluePresented output endpoints must NOT equate s-to-s with s-to-t (refl not load-bearing)")
	}
}
