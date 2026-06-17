# CLOCK-IRRELEVANCE-DESIGN — the dependent force + the `Str ≃ ∀κ. gStr` bridge

*Research design for the FINAL step of the E2 converse `bisimilar ⟹ path`. Grounded
in the landed clock layer (`store/guard.go`: `Clock`/`k0`/`Later`/`next`/`dfix`/`lmap`/
`lap`/`force`/`gfix`; `core/conv.go` `convDfix`/`convGfix`; ch62/ch68/ch69). Decision-
locked in the style of CLOCKS-DESIGN / GLUE-DESIGN. This is the one primitive still
missing between the landed substrate and the constructed converse.*

## 1. The wall (why the substrate is not yet the converse)

After CLOCKS-DESIGN, the converse has all three pillars: `gfix` STATES the guarded
stream `gStr κ A = gfix κ (λX. A × ▹κ X)` (ch69), `dfix` PRODUCES it corecursively
(ch69 `repeatG`), and `force` CASHES a `∀κ`-quantified delayed value. But two gaps
remain between these and an honest `pathF (Str A) s t`:

1. **Observing a `dfix` result is stuck.** `headG (repeatG a)` does NOT reduce to `a`
   — the `dfix` sits under `fstF`, and `convDfix` only unfolds a `dfix` at the TOP of a
   comparison, never buried under an eliminator (ch69 documents this; the dropped
   `repeatHeadG`). To observe a produced guarded stream you must `force` its delayed
   tail out — guarded recursion alone cannot.

2. **`force` does not apply to `gStr`.** The landed `force` requires its payload type
   `A : UF` to be **clock-closed** (`tryForceIota` rejects a payload mentioning the
   bound clock). But the guarded stream's tail is `▹κ (gStr κ A)`, and `gStr κ A`
   **mentions κ** (the guard inside is on κ). So `force` cannot cash a guarded stream's
   tail: the type under the `Later` is clock-VARYING. This is exactly the dependent
   case the clock-closed check excludes.

The coinductive stream `Str A = Nu (λX. A × X)` (ch66) is the GLOBAL object; the
guarded `gStr κ A` is its κ-local shadow. The missing bridge is

```
Str A  ≃  ∀κ. gStr κ A        (the global stream IS the clock-quantified guarded stream)
```

and constructing it — plus observing it — is what needs the dependent force.

## 2. The mechanism — `forceD`, the clock-irrelevant (dependent) force

Generalise `force` to a clock-VARYING payload:

```
forceD : (A : Clock -> UF) -> ((k : Clock) -> El (Later k (A k))) -> ((k : Clock) -> El (A k))
```

with the same firing discipline as `force`, but the payload may now mention the clock:

```
forceD A (λκ. next κ (A κ) x)  ~>  λκ. x          (x : El (A κ), MAY mention κ)
```

The landed `force` is the special case `A = λ_. A0` (constant). `forceD` cashes a
guarded stream's tail: with `A = λκ. gStr κ A0`, a `(κ : Clock) -> El (Later κ (gStr κ
A0))` becomes a `(κ : Clock) -> El (gStr κ A0)` = the global tail.

## 3. Soundness — why dropping the clock-closed check is safe HERE

The landed `force` keeps a clock-closed check on `x` and clock-indexes the `Later` so
that the inconsistent `Later A -> A` is untypable (CLOCKS-DESIGN §3). For `forceD` the
payload `x : El (A κ)` legitimately mentions κ THROUGH ITS TYPE, so the syntactic
clock-closed check must be dropped. The replacement safety argument is **clock
inertness**:

- `Clock` is a pretype with NO eliminator (like the interval `I`) — you cannot case on
  a clock, branch on it, or extract information from it. So a value `x : El (A κ)` can
  only *mention* κ by passing it to the clock-consuming formers (`Later`/`next`/`dfix`/
  `lmap`/`lap`/`force`/`gfix`); it can never *inspect* κ. Therefore `x` is automatically
  parametric in κ — the parametricity `force` relies on holds by construction, not by a
  side-condition.
- The unsound `λκ. la` (free `la`) is STILL rejected by typing: `la`'s type cannot
  mention the bound κ, so `la : El (Later κ (A κ))` is underivable for a free `la` — the
  same protection as `force`.
- Operationally `forceD` fires only on a `next` on the BOUND clock (the sentinel probe),
  staying neutral otherwise — the C4 firewall holds, the normaliser cannot loop.

**The one genuine gap (the axiom).** For a clock-CLOSED type `A0`, clock-irrelevance
says `∀κ. El A0 ≃ El A0` definitionally (a κ-indexed family of `A0`-values is constant).
The landed `force`'s clock-closed check *enforces* this syntactically. `forceD` over a
clock-VARYING `A` cannot, and the genuine content of clock-irrelevance — that the forced
family is *uniform* — is a PROPOSITIONAL fact, not a definitional one. The standard CCTT
resolution is a **clock-irrelevance axiom / rule** `irr : (∀κ. A) -> A` for clock-closed
`A` (Bizjak–Møgelberg; Bahr–Grathwohl–Møgelberg). The decision: ship `forceD`'s
reduction (sound by inertness for the OPERATIONAL rule) and add clock-irrelevance as a
postulated coherence ONLY where the converse's endpoint proofs need it, retiring it later
if it proves derivable from the cubical stratum (parametricity-as-a-path). This mirrors
how `ua` was first postulated, then derived.

## 4. The payoff — the converse, assembled

With `forceD` and the bridge:

1. **`Bisim`** — the bisimilarity relation as a guarded-recursive type. For a fixed
   stream pair it is `gfix κ (λR. (head s = head t) × ▹κ R')`; the general relation
   (indexed by the pair) needs guarded-recursive type FAMILIES — a `gfix` whose operator
   is `Str A -> Str A -> UF`-valued. This is the one extra former beyond ch69's
   single-type `gfix` (R-COIND tail item: guarded recursive families).
2. **bridge** `glue : (∀κ. gStr κ A) -> Str A` and `split : Str A -> ∀κ. gStr κ A` via
   `out`/`unfold` (ch66) on one side and `consG`/`forceD`-of-`tailG` on the other.
3. **converse** `Bisim A s t -> pathF (Str A) s t`: `dfix` corecursively assembles, from
   a `Bisim`, a guarded path between the streams' observations; `forceD` cashes the
   `∀κ`-quantified guarded path into the honest `pathF`. Endpoints land via the `papp`
   boundary (ch66 forward half), giving `Bisim ≃ pathF (Str A)` — the E2 currency.

## 5. Build order (each a green, contained checkpoint)

1. **`forceD`** as a guard-group member (own hash space, like `force`): the dependent
   force, ι on a clock-varying `next` intro, payload clock-mention ALLOWED, sentinel-on-
   bound-clock required. Pin: cashing a `gStr` tail computes; stuck on a neutral.
2. **the bridge** `glue`/`split` + the round-trip coherences (forward already ch66).
3. **guarded-recursive type families** (`gfixF : (k) -> ((I -> UF) -> (I -> UF)) -> …`
   or the relation-valued operator) — the `Bisim` former. The genuine new primitive.
4. **`Bisim` + the converse** in `ch70_stream_bisim_converse.rune`, composing forward
   (ch66) + converse into `Bisim ≃ pathF (Str A)`. The E2 gate.
5. **clock-irrelevance axiom** only if §3's propositional gap is load-bearing in step 4;
   prefer deriving it from the cubical stratum first.

## 6. Decisions locked

- **`forceD` reduction ships sound by clock-inertness; the irrelevance AXIOM is added
  only where needed and is a retirement candidate** (derive from parametricity-as-a-path
  on the cubical stratum, like `ua`).
- **Contained, additive.** `forceD` and the guarded-family former are builtin groups on
  their own hash spaces; no outer-core constructor, no hash-format bump — exactly as
  `force`/`gfix` landed.
- **Streams stay `Nu`-based globally; `gStr` is the κ-local shadow.** The bridge is the
  only place the two meet; we do NOT redefine `Str A` as `∀κ. gStr κ A` in the kernel
  (it is a propositional equivalence, proven, not a definitional identification).

## 7. Status

Substrate fully landed: `force` (ch68), `gfix` + guarded streams + consG/headG/tailG +
fmapF-over-Later (ch69), `convDfix`/`convGfix` firewalls proven.

**STEP 1 LANDED (`forceD`; 10th guard member, ch69).** The dependent force ships:
`forceD : (A:Clock->UF) -> ((k:Clock) -> El (Later k (A k))) -> ((k:Clock) -> El (A k))`,
ι `forceD A (λκ. next κ (A κ) x) ~> λκ. x` (core/eval.go tryForceDIota), payload clock-
mention ALLOWED, sound by clock-inertness (§3): the probe at the sentinel decides firing,
the result re-applies `g` at the bound clock and strips the `next` (substituting the real
clock for κ), uniform because `Clock` has no eliminator. ch69 pins `forceDNext` + the
global stream observations `gheadG`/`gtailG` + `gtailCons` (forceD cashing a guarded
stream's clock-VARYING delayed tail into the global rest — the converse observation,
refl); internal/session/clock_test.go pins forceD fires + stays stuck on a neutral.

**STEP 3 LANDED (`gfixF`, the INDEXED guarded fixpoint = the `Bisim` former; 11th guard
member, ch69).** `gfixF : (k:Clock) -> (D:UF) -> ((El D -> UF) -> (El D -> UF)) -> (El D ->
UF)` generalises `gfix` from a single code to a FAMILY indexed by D, so the recursive
occurrence may sit at a DIFFERENT index — exactly what bisimilarity needs (it recurses on
the stream TAILS). Equation `gfixF k D Φ d ≡ Φ (gfixF k D Φ) d`, a bounded conversion on
the APPLIED (4-arg) fixpoint (core/conv.go convGfixF) with the same progress guard as
convGfix (an unguarded `Φ = λR. R` cannot loop). ch69 pins `gfixFUnfold` (the equation,
any Φ, refl) + `gfixFStep` (Φ recursing at a stepped index under ▹κ — the indexed guarded
corecursion, refl); clock_test.go pins the unguarded-Φ termination.

**STEP 3.5 LANDED (`laterApp`, the universe-level ▹κ application; 12th guard member, ch69).**
A refinement surfaced while building `Bisim`: its recursive occurrence `▹κ (Bisim tails)` is
UF-VALUED (a relation), and `lmap` only lifts `El A -> El B` functions, not `El A -> UF`
families. So a companion was needed: `laterApp : (k:Clock) -> (A:UF) -> (El A -> UF) ->
El (Later k A) -> UF`, ι `laterApp k A f (next k A x) ~> Later k (f x)` (core/eval.go
tryLaterAppIota), sound (it only re-wraps under `Later`, never escapes). ch69 pins
`laterAppNext` + `gRelStep` — the abstract bisimilarity SHAPE: a `gfixF` relation `Φ R e =
(P e) × laterApp k D R (step e)` with the UF-valued recursive occurrence at a stepped delayed
index, unfolding by refl. So the converse's primitive layer is COMPLETE.

**STEP 4 (part 1) LANDED — the concrete stream `gBisim` (ch69).** Bisimilarity of guarded
streams is built: `gfixF` over `Dpair = gStr × gStr`, with `gBisim d = (headG (fst d) =
headG (snd d)) × ▹κ (gBisim (tail-pair d))` — head-equality a `pathF`, the recursive
occurrence under ▹κ via `laterApp`, the `step` (`gstepPair`) zipping the two ▹κ-tails into
a `▹κ`-pair via `lap`∘`lmap`. `gBisimUnfold` proves the coinductive characterisation by
refl. So `gRelStep` is instantiated to the real relation.

**STEP 4 combinator LANDED — `lapD`, dependent guarded application (13th guard member, ch69).**
The path-assembly recursion applies the delayed recursive path-builder (a DEPENDENT function
`▹κ C`) to the delayed tail index — which `lap` (non-dependent) cannot. `lapD : (k:Clock) ->
(A:UF) -> (B:El A -> UF) -> El (Later k (piF A B)) -> (la : El (Later k A)) -> El (laterApp k
A B la)`, ι `lapD k A B (next k (piF A B) f) (next k A x) ~> next k (B x) (f x)`; its result
type is given by `laterApp` (hence laterApp first). `lapDNext` pins it (refl).

**STEP 4 destructors LANDED + a KEY FINDING on the path assembly (ch69).** The coinductive
destructors `bisimHead` (the head-equality `pathF`, = fstF after the gBisim unfolding) and
`bisimTail` (the delayed tail-bisimilarity, = sndF) are landed — the converse's observation
layer.

FINDING — the path assembly is a CUBICAL construction, not bare corecursion. The naive
`pabs (gStr) (λi. consG (papp hp i) (lmap (λq. papp q i) tailpath))` does NOT typecheck:
its i0-boundary is `consG (headG s) (lmap (λq. papp q i0) tailpath)`, and the `lmap` over a
delayed (neutral) `tailpath` stays a NEUTRAL `lmap` — it does NOT reduce to `tailG s`, so
`consG (headG s)(…) ≢ s` by inner-Σ η. The endpoint coherence is PROPOSITIONAL, not
definitional. So the converse must build a path with approximate endpoints and REPAIR them
by Kan composition (`comp`/`hcomp` over `gStr`, the landed structural-Σ Kan rules A5a/A5b) to
land the boundary on `s`/`t` — the standard cubical-coinduction technique (Møgelberg–Veltri
use exactly this comp-repair).

**STEP 4 enabler LANDED — `hcomp` over a `gfix`-typed value (core/eval.go).** The endpoint-
repair needs Kan ops over `gStr`, but `gStr = gfix k F` has a gfix-neutral head the structural
formers do not match, so Kan ops over it were stuck. Added `unfoldGfixType` + a tryHcomp case:
unfold the gfix TYPE one step (`gfix k F ≡ F (gfix k F)`, a sigmaF) and recurse, so the landed
structural-Σ rule (A5a/A5b) fires — `hcomp (gStr) φ u u0 ~> pairF …` (TestHcompOverGfixUnfolds).
Sound (El equal) + bounded (body's former is sigmaF). The endpoint-repair primitive over
guarded streams.

The repair is VERIFIED componentwise: ch69 `hcompHeadG`/`hcompTailG` prove (refl) that
`headG (hcomp gStr φ u u0) ≡ hcomp A φ (heads)` and `tailG (…) ≡ hcomp (Later k gStr) φ
(tails)` — hcomp acts independently on head and tail, so endpoint-repair keeps observations
coherent.

NEXT-OBSTACLE FINDING (the path-assembly proof). Any corecursive proof over `gBisim`
(b2p, bisim-refl) must fill the tail SLOT of type `laterApp k Dpair gBisim (gstepPair d)`,
but the natural recursive call — `lapD` over the delayed tail `tailG s : ▹κ gStr` — produces
the differently-shaped `laterApp k gStr (λs'. gBisim …) (tailG s)`. These two `laterApp`
NEUTRALS (different base `Dpair` vs `gStr`, different family, different delayed arg `gstepPair
d` vs `tailG s`) do NOT definitionally align — they would only agree on a `next`. So the proof
needs a SLOT-COHERENCE: a path (or definitional bridge) between `gstepPair d` (the delayed
PAIR of tails) and the pairing of the recursive calls.

**SLOT-COHERENCE DISCHARGED ON THE PRODUCTIVE CASE (`bisimTailSlotCons`, ch69).** The slot
mismatch is only a NEUTRAL-vs-neutral standoff; on the shape productive corecursion actually
emits — a pair of CONS-BUILT streams with `next`-guarded tails — the slot COMPUTES away. With
`d = pairF (consG a (next ra)) (consG b (next rb))`, four ι-rules chain with no neutral left:
`tailG (consG ..) ~> next ..` (tailGCons) feeds `lmap`/`lap` on `next` intros (lmapNext/lapNext),
giving `gstepPair k A d ~> next k (Dpair k A) (pairF .. ra rb)`, whereupon
`laterApp k _ (gBisim k A) (next k _ p) ~> Later k (gBisim k A p)` (laterAppNext). So
`laterApp k Dpair gBisim (gstepPair k A d) ≡ Later k (gBisim (pairF ra rb))` BY REFL — the
slot is definitionally the delayed bisimilarity of the literal tail pair, exactly what a
`next`-guarded recursive call inhabits. ch69 `bisimTailSlotCons` pins this (refl). So the
slot-coherence is resolved wherever the corecursion is productive (lands a `next`); the general
NEUTRAL case is the propositional residue carried by the repair path, not a definitional gap.

**PRODUCTIVE-CASE PATH-ASSEMBLY STEP LANDED (`congConsG` + `lmapPappNextI0`, ch69).** The
converse's corecursion BODY is now in hand for the productive case. The path congruence
`consG`-cong builds `pathF (gStr) (consG a (next ra)) (consG b (next rb))` from a head-path `hp`
and a delayed tail-path supplied as a `next`-intro `next k _ p` (what a guarded recursive call
yields): a single `pabs` over `gStr` that, at interval point `i`, conses `papp hp i` onto the
delayed tail observed along the delayed path, `lmap (λq. papp q i) (next k _ p)`. Its endpoints
are DEFINITIONAL — no Kan repair — because the tail arrives as a `next`: `lmapPappNextI0` pins
(refl) that `lmap k _ _ (λq. papp q i0) (next k _ p) ~> next k _ ra` (lmapNext then the papp
i0-boundary), so `pabs`'s forced i0/i1 endpoints match the declared `pathF` type on the nose.
This is exactly the case the slot-coherence (`bisimTailSlotCons`) already collapsed: on the
PRODUCTIVE (next-guarded) shape, both the recursion's TYPE slot and its term-level endpoints are
definitional. The neutral-tail residue is what the hcomp endpoint-repair still carries.

**STREAM-η COHERENCE LANDED (`streamEta`, ch69).** Every guarded stream IS its head consed onto
its tail: `s ≡ consG (headG s) (tailG s)` BY REFL, via inner-Σ η (`convSigmaEta`, core/conv.go) —
`gStr k A` unfolds (convGfix) to `sigmaF A (λ_. ▹κ gStr)`, so a stream of that Σ-type equals
`pairF (fstF s) (sndF s)`. This is the η-expansion law the converse rewrites a GENERAL endpoint
`Sfst d`/`Ssnd d` along to reach cons-form, the bridge to `congConsG` (which assembles a path
between cons-built streams). It does NOT by itself make the endpoints definitional — `tailG s` is
a NEUTRAL `▹κ`, not a `next`, so the productive-case assembly still needs the recursion to supply
a `next`-tail (and the hcomp-repair for the neutral boundary residue) — but it certifies the
rewrite is exact.

**BRIDGE HALF `split` CONSTRUCTED + the DFIX-OBSERVATION WALL (ch69).** `splitFn : Str A -> ∀κ.
gStr κ A` (the `Str ≃ ∀κ. gStr` gate's projection direction) is built by guarded corecursion: a
`dfix` at the FUNCTION type `piF (Str A) (λ_. gStr k A)` (a fibrant code; `El (piF A B)` is the
real Pi) whose body conses `head s` onto the delayed recursive call `lap self (next (tail s))`,
productive by the ▹κ guard. `split A k s = splitFn A k s` is the per-clock shadow.

FINDING (the wall): NO computing coherence about a `dfix`-corecursor is provable by `refl`. The
hoped-for unfolding `splitFn ≡ λs. consG (head s) (lap (next splitFn) (next (tail s)))` fails
because `Eq` at a Pi type FUNEXT-reduces to the pointwise `(s) -> Eq … (splitFn s) (consG …)`,
and `splitFn s` is a `dfix` buried under APPLICATION — convDfix unfolds a `dfix` only at the TOP
of a comparison, never under an eliminator/application (the same wall `repeatG` hits; it ships
without a head lemma). So every equation about `split` funext-collapses to a stuck applied form.
CONSEQUENCE: the bridge's round-trip coherences cannot be read off `split` by dfix-observation;
they must come the PRODUCTIVE way — observe via the global `gheadG`/`gtailG` (the `forceD`-cash,
which strips a `next` across all clocks and DOES compute, cf. `gtailCons`), against streams in
cons-form. `split` stands as the type-correct, productive bridge term; its observational theory
rides the forceD layer — the same discipline the converse's `congConsG` uses.

**BRIDGE HALF `glue` LANDED WITH COMPUTING OBSERVATIONS (ch69) — the dfix-wall resolved.** The
OTHER direction `glue : (∀κ. gStr κ A) -> Str A` is built on the Nu side via the anamorphism
`unfold`, so its observations COMPUTE (the coinductive ι `out (unfold F S c s) ~> fmapF F (unfold
F S c) (c s)`), exactly dodging the dfix wall that made `split` opaque. The global guarded stream
`∀κ. gStr κ A = (c : Clock) -> El (gStr c A)` gets a fibrant `unfold`-carrier code via `fib Clock`
(`fib : U -> UF`, `Clock : U`, `El (fib Clock) ≡ Clock`): `GStrAll A = piF (fib Clock) (λc. gStr c
A)`. The coalgebra observes head + tail by the PRODUCTIVE global observations `gheadG` / `gtailG`
(the forceD-cash) — the very route the dfix wall forces. Both homomorphism laws hold BY REFL:
`glueHead : head (glue g) ≡ gheadG g` and `glueTail : tail (glue g) ≡ glue (gtailG g)` (the ι
fires, `fmapF` keeps the head slot and maps the tail slot by `unfold … coalg`). So `glue` is a
COMPUTING stream homomorphism — the usable bridge half, in contrast to the opaque dfix-side
`split`. CONFIRMS the finding's resolution: build the bridge Nu-side (observation computes), not
gStr-side (dfix-observation stuck).

**ROUND-TRIP ON CONS-FORM LANDED BY REFL (ch69) — the `glue`-side coherences COMPUTE.** Step 2's
round-trip is now proved on the CONS-FORM global stream (the productive presentation, NOT through
`split`'s dfix): `glueConsHead : head (glue (λκ. consG h (t κ))) ≡ h` composes `glueHead` (head
of glue ≡ gheadG) with `gheadCons` (gheadG of a cons ≡ h), both definitional, so one `refl`; and
`glueConsTail : tail (glue (λκ. consG h (next (rest κ)))) ≡ glue rest` composes `glueTail` (tail
of glue ≡ glue ∘ gtailG) with `gtailCons` (gtailG cashes the delayed rest by forceD) — and refl
GOES THROUGH because forceD over the cons-form reduces under the binder, so `gtailG g ≡ rest` as
functions definitionally (eta + forceNext). So `glue` is a COMPUTING stream homomorphism on the
cons-form: it preserves head and steps the tail. This is exactly the step-2 round-trip the design
called for — obtained the productive way the dfix wall forces, never observing through `split`.

**CONVERSE RECURSION BODY REALIZED on the productive case (ch69 `bisimStepPath`) + the sharp
hcomp-repair reason.** The converse's step is now assembled END-TO-END for a cons-built pair `d =
(consG a (next ra), consG b (next rb))`: `bisimStepPath` consumes a REAL `gBisim d` witness, pulls
the head-path with `bisimHead` (its type computes to `pathF A a b` by `headGCons` on the cons-
projections), and conses it onto a tail-path `p` via `congConsG`; the output `pathF (gStr)(consG a
(next ra))(consG b (next rb))` IS `pathF (gStr)(Sfst d)(Ssnd d)` on the nose (Sfst/Ssnd β). So the
body type-checks definitionally — provided `p` is a PRESENT value. SHARP FINDING (why hcomp-repair
is unavoidable for the CLOSED recursion): when `p` instead comes from the guarded recursive call,
it arrives through `dfix`'s self — a NEUTRAL ▹κ bound variable, NOT a `next` — so `lmap (λq. papp q
i)` over it stays stuck (lmapPappNextI0 fires only on a `next`-intro), and the assembled path's
endpoints are non-definitional even on the cons-form. The productive-case definitionality
(`bisimStepPath`, `congConsG`) holds for a present tail-path; the recursion supplies it delayed-
and-neutral. So the closed converse = `bisimStepPath`'s body driven by a guarded fixpoint whose
delayed self forces the endpoints through `hcomp`-repair (the enabler + componentwise repair are
landed). This is the precise crank that remains.

**ENDPOINT-REPAIR TOOL LANDED (ch69 `repairEndpointsG`, via `ptransG`/`pathJ`).** The slide-the-
endpoints combinator the closed converse needs: given a path `q : x' ≡ y'` with the neutral
(approximate) endpoints the recursive-self assembly produces, and two correction paths `sx : x ≡
x'`, `sy : y' ≡ y`, it returns `x ≡ y` (the double-composition `sx · q · sy`). `ptransG` is path
transitivity at `gStr` by `pathJ`; `repairEndpointsG` composes two. (The landed `hcompHeadG`/
`hcompTailG` certify the raw-`hcomp` repair is componentwise; `ptransG` gives the repair directly
and computes — the cleaner route.) REDUCTION (the sharpened residue): with `repairEndpointsG` the
converse closes IFF the two correction paths exist; each decomposes by consG-congruence (Sfst d ≡
consG (headG (Sfst d)) (tailG (Sfst d)) definitionally, streamEta) into a HEAD-correction — which
is exactly `bisimHead`, ALREADY in hand — and a DELAYED TAIL-CORRECTION `pathF (Later k (gStr))
(tailG (Sfst d)) (lmap (λr. papp r i0) self)`. That delayed tail-correction (a path between two
▹κ-values, one neutral) is the entire remaining residue of the E2 converse.

**▹κ PRESERVES PATHS (ch69 `laterPathNext`, refl)** — `next` is a path morphism: a present path
`p : x ≡ y` lifts to `next x ≡ next y` in `Later k X` (`pabs (Later k X)(λi. next (papp p i))`,
endpoints definitional by the papp boundary under `next`). The PATH-level companion to `lmap`/
`lap`; the tool the per-clock converse uses to lift a recursive tail-path under ▹κ.

**TWO ROUTES, TWO WALLS (this iteration's probe pins the global one).** The E2 converse can be
attempted (a) per-clock at a fixed κ — but the guarded recursion's delayed self is a NEUTRAL ▹κ,
so the assembled path's endpoints need repair and the residue is the delayed tail-correction
above; OR (b) globally at `pathF (Str A)` — but PROBED THIS ITERATION: `head (hcomp (Str A) φ u
u0)` does NOT reduce (`hcompHeadStrProbe` fails — "sides not definitionally equal"). `Nu` has NO
structural Kan rule: unlike `gStr` (whose `hcomp` computes via `unfoldGfixType` exposing the
`sigmaF`, the landed enabler), the coinductive `Nu F` is NOT definitionally its unfolding — `out`
is an observation, not a reduction — so the structural-Σ Kan rules never fire and Kan-over-`Str`
stays stuck. CONSEQUENCE: the global endpoint-repair route needs a NEW KERNEL ENABLER —
**Kan-over-Nu**: `hcomp`/`transp` over a coinductive type computing componentwise through `out`/
`unfold` (the global-side dual of hcomp-over-gfix), a core/eval.go addition. This is now the most
tractable unblock for the closed converse — it makes the global repair compute, sidestepping the
per-clock neutral-self wall entirely.

**KAN-OVER-Nu LANDED (core/eval.go `tryCoindIota`; ch69 `hcompHeadStr`/`hcompTailStr`, refl;
session `TestOutCommutesWithHcompOverNu`).** `out` now COMMUTES with an hcomp over a coinductive
type: `out F (hcomp (Nu F) φ u u0) ~> hcomp (F (Nu F)) φ (λi h. out F (u i h)) (out F u0)`. Because
`Nu` is NEGATIVE (not definitionally its unfolding — `out` is an observation, not a reduction —
so the gfix type-unfold trick does NOT apply), the rule rides the OBSERVATION: the composed stream
stays a neutral, but observing it fires the rule, lands in the structural former `F (Nu F)` (a
`sigmaF` for streams), and the landed Σ Kan rule splits it. So `head (hcomp (Str A) φ u u0) ≡
hcomp A φ (heads)(head u0)` and `tail (…) ≡ hcomp (Str A) φ (tails)(tail u0)` BY REFL — hcomp over
`Str A` is fully componentwise on observations, the global-side dual of `hcompHeadG`/`hcompTailG`.
Sound (the defining equation of coinductive hcomp), bounded (fires once per `out`; `out` is not
re-introduced on the same hcomp). The global endpoint-repair route is now OPEN. (transp/comp over
Nu are the duals, deferred until a consumer needs them.) NEXT: assemble the global converse —
build the approximate Str-path (head-path consed via the Nu coalgebra) and repair its endpoints
with `hcomp (Str A)`, the head/tail observations now computing through this rule.

**GLOBAL REPAIR TOOL + PRODUCTIVE GLOBAL CONVERSE LANDED (ch69).** `ptransStr`/`repairEndpointsStr`
are the Str-level duals of `ptransG`/`repairEndpointsG` (path composition + endpoint-slide at the
coinductive level, via the ambient `pathJ`) — the global correction tool. And `unfoldSeedPath` is
the PRODUCTIVE global converse, the Str-level dual of `congConsG`/`bisimStepPath`: path-connected
SEEDS give path-connected `unfold`-streams — `pabs (Str A)(λi. unfold StreamF S c (papp ps i))`,
endpoints DEFINITIONAL by the papp boundary, NO repair. So the global converse closes on the
unfold-presented case exactly as `congConsG` closes the per-clock cons case. PROBED: `transp` over
a CONSTANT `Str` line = id (regularity holds — so a constant interpolant transports trivially).
REMAINING (the general case, both routes converge here): build the interpolant for arbitrary `s`/
`t : Str A` (not in unfold-form) — a `StreamF` coalgebra DRIVEN BY the bisimilarity (head along the
head-path, tail seed the recursive tail-bisim) whose `unfold` interpolates — then `repairEndpointsStr`
re-aims the endpoints onto `s`/`t`, the head/tail corrections computing through Kan-over-Nu. The
interpolant-from-bisimilarity is the single remaining construction; the repair + observation
machinery beneath it is now all landed.

**FINAL-COALGEBRA η LANDED (core/eval.go `tryUnfoldEta`; ch69 `nuEta`, refl; session
`TestUnfoldEtaFinalCoalgebra`).** The coinductive uniqueness law, dual to Kan-over-Nu's
observation rule: `unfold F (Nu F) (out F) s ~> s` — the anamorphism into the FINAL coalgebra at
carrier `Nu F` with coalgebra `out` is the identity (both `id` and `unfold (out)` are coalgebra
morphisms into the final coalgebra, hence equal). PROBED ABSENT before, now a kernel rule. Sound +
confluent with the `out`-ι (`out (unfold (out) s)` reduces either way to `out s`, via this η on
the recursive position + inner-Σ η `pairF (fst p)(snd p) ≡ p`); fires only on the exact shape
(carrier Nu-headed, coalgebra `out`-headed), well-typedness forcing the codes to agree. This is
the negative-type η for coinductives — it lets a stream reconstructed from its observations bottom
out definitionally, the law the global converse's endpoints rely on. With Kan-over-Nu + Nu-η + the
repair tools all landed, the coinductive cubical substrate for the converse is COMPLETE; the
remaining work is purely the interpolant-coalgebra construction (build it from the bisimilarity,
land endpoints on `s`/`t` via `repairEndpointsStr` whose corrections the bisimilarity supplies).

**STREAM PREPEND `consStr` LANDED + the converse's TRUE remaining obstacle pinned (ch69).**
`consStr h ts` (the global-stream constructor the codebase lacked) prepends `h` onto `ts`, built
by `unfold` on carrier `Σ A (Str A)` whose coalgebra re-deconstructs the rest via `out`
(`coalg (a,s) = (a, out s)`). Its head observations COMPUTE by refl: `head (consStr h ts) ≡ h`
(`headConsStr`), `head (tail (consStr h ts)) ≡ head ts` (`headTailConsStr`). CRUCIAL FINDING: the
cons-η `tail (consStr h ts) ≡ ts` — equivalently `consStr (head s)(tail s) ≡ s`, the rebuild-from-
observations law — is NOT definitional: the tail is `ts` REBUILT FROM ITS HEADS (carrier `Σ A (Str
A)`, coalgebra ≠ `out`, so Nu-η does not fire), only BISIMILAR to `ts`. This is EXACTLY the
bisimilarity-is-path coherence the converse needs — so the converse cannot be closed by any FINITE
definitional construction (every route — interpolant, prepend, rebuild — bottoms out at this same
non-definitional cons-η). It requires a COINDUCTIVE FILLING principle: either (a) the per-clock
guarded route (∀κ + `forceD`-cash, the ORIGINAL design — the guard makes the corecursion
productive where the global `unfold` is not), or (b) a genuine `comp`/`transp`-over-Nu with
coinductive filling (more than the `hcomp`/observation rule Kan-over-Nu provides). The per-clock
route (a) is why clocks were introduced; it is the indicated path. The global substrate (Kan-over-
Nu, Nu-η, repair tools, `consStr`) is complete and independently valuable, but bisimilarity-is-path
is a coinductive principle, not a reduction.

**CONVERSE CLOSED MODULO cons-η (ch69 `converseFromConsEta`) — the substrate proven complete.**
A verified CONDITIONAL theorem: given the two observations a bisimilarity supplies (a head-path
`head s ≡ head t` and a tail-path `tail s ≡ tail t` — the converse of ch66's `headPath`/`tailPath`)
AND the cons-η as a path for each stream (`consStr (head s)(tail s) ≡ s`), the path `s ≡ t` is
ASSEMBLED: cons the observations into a path between the cons-forms (`pabs (λi. consStr (papp hp
i)(papp tp i))`, endpoints definitional) then `repairEndpointsStr` re-aims the endpoints via the
cons-η paths (with `symStr` = interval reversal). So bisimilarity-is-path holds THE MOMENT cons-η
is supplied — every other piece (`consStr`, the repair tools, `symStr`, Kan-over-Nu, Nu-η) is
landed. cons-η (`consStr (head s)(tail s) ≡ s`) is precisely the single coinductive principle no
definitional reduction provides.

**THE convDfix-under-application INVESTIGATION (decisive on the per-clock route).** Tried relaxing
`convDfix` (core/conv.go) to fire on an APPLIED `dfix` (`dfix k A f x…`, spine ≥ 3, previously
rejected by `len != 3`) so `split`'s one-step unfolding `split s ≡ consG (head s)(next (split (tail
s)))` would compute. Sound and termination-safe (genuine infinite streams are dfix-guarded, so the
both-dfix→spine short-circuit stops them; an applied dfix only ever unfolds against a finite term).
But it does NOT break the wall: the comparison is at type `El (gStr k A)`, and `sigmaF`-η
(`convSigmaEta`) projects both sides through `fstF`/`sndF` BEFORE `convDfix` runs, so the `dfix`
ends up UNDER an eliminator — where `convDfix` (matching a dfix only at the comparison top) never
sees it (instrumented: convDfix is hit repeatedly, never with a dfix head). Breaking it would need
`dfix`-reduction UNDER eliminators (reaching into neutral spines) — genuinely termination-risky,
Thompson-sensitive, no other consumer. REVERTED (scope discipline: no kernel complexity without a
demonstrated consumer). CONCLUSION: both routes (global cons-η, per-clock dfix-under-eliminator) are
blocked by the SAME coinductive coherence; closing E2 is a DESIGN DECISION — either adopt the
stream coinduction principle (cons-η / bisimilarity-is-path) as a contained derived-axiom (the
substrate makes everything else compute, per `converseFromConsEta`), or build a genuine coinductive
Kan filling (`comp`/`transp`-over-Nu beyond the observation rule). Not closeable by incremental
definitional lemmas — the cubical rabbit-hole hazard the v3 design flagged.

## E2 CONVERSE CLOSED — the one-level coinductive constructor `nuCons` (computing, no postulate)

The cons-η is now DEFINITIONAL, via the genuine missing primitive: `Nu` lacked a ONE-LEVEL
constructor. The deep `unfold` rebuilds observations corecursively (so its tail is `ts` rebuilt,
only bisimilar — why cons-η failed); the constructor reconstructs from ONE layer. Added `nuCons`
as the 4th coind builtin member (store/coind.go; coind group 3→4, a contained rehash, NO hash-
FORMAT bump — no new core constructor):

    nuCons : (F : UF -> UF) -> El (F (Nu F)) -> El (Nu F)

with two rules: β `out F (nuCons F x) ~> x` (eval, tryCoindIota) and the COINDUCTIVE η
`nuCons F y ≡ t ⟺ y ≡ out F t` (conversion, `convNuConsEta` — the dual of `convSigmaEta` for the
final coalgebra; subsumes the eval form `nuCons (out s) ~> s`; bounded — one `out` then structural
compare). Confluent (β/η agree on `out (nuCons (out s))` etc.). Sound: it is the final coalgebra's
constructor + its η, true in the intended model; bounded, so conversion stays decidable. Full suite
green (the coind rehash re-elaborates ch25/ch66/ch69; no golden tests).

RESULT (ch69): with `consStr := nuCons (pairF h ts)`, head/FULL-tail compute (`headConsStr`,
`tailConsStr`, refl — the deep version could only manage `head (tail …)`), and the COINDUCTIVE η
`consStr (head s)(tail s) ≡ s` (`consEtaStr`) is REFL (convNuConsEta bridges the inner-Σ η on
`out s`). So `converseFromConsEta`'s cons-η corrections are discharged by `preflF`, and the E2
CONVERSE is CLOSED:

    bisimToPathStr : (head-path : head s ≡ head t) -> (tail-path : tail s ≡ tail t)
                     -> pathF (Str A) s t

bisimilarity ⟹ path for global streams, COMPUTING, no postulate, no dfix-observation. With ch66's
forward `headPath`/`tailPath`, the correspondence `pathF (Str A) s t ↔ (head-path × tail-path)` is
COMPLETE — the E2 currency on the cubical stratum. Pinned: ch69 `bisimToPathStr`/`consEtaStr`/
`tailConsStr`; session `TestNuConsBetaEta`. (The per-clock guarded route + `Str ≃ ∀κ. gStr` bridge
remain as an alternate construction, but E2's correctness currency — bisimilarity-is-path — is now
in hand globally.)

REMAINING: step 4 (the `Bisim ≃ pathF` proof = wire `congConsG` into the guarded corecursion
driven by `bisimHead`/`bisimTail`, rewriting endpoints via `streamEta`, then `forceD`-cash the
∀κ-guarded path to the global `pathF (Str A)` presented productively — NOT dfix-observed — with
hcomp-repair for the neutral residue); step 2 TAIL (the round-trip THROUGH `split` itself — `glue
(λκ. split κ s) ≃ s` and the dual — remains dfix-opaque: `gheadG`/`headG` of `split κ s` hit the
dfix wall, so the full extensionality `Str ≃ ∀κ. gStr` must be assembled from the cons-form round-
trip via `streamEta` + productive corecursion, not by observing `split`); step 5 (clock-irrelevance
axiom, gated on §3's gap). Combinators + relation + destructors + hcomp-over-gfix enabler +
componentwise-repair + productive-case slot-coherence + productive-case path-assembly step +
stream-η + BOTH bridge halves (`split` constructed, `glue` computing) + the cons-form round-trip
(`glueConsHead`/`glueConsTail`) are landed.
