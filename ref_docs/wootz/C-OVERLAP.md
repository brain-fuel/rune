# C-OVERLAP — System overlap-agreement mechanism

## Problem (what's stuck/absent today, with file:line)

A *system* is a partial element whose face is a disjunction: on
`for φ ψ`, the value is one thing where `φ` holds and another where `ψ` holds,
and the two must **agree on the overlap** `fand φ ψ`. In CCHM/Agda this agreement
is the *compatibility* condition on partial elements, and it must hold
**definitionally** — otherwise `hcomp`/`comp` on a proper, multi-branch face is
not well-typed and the open-box semantics (R-BOX) and genuine multi-branch
systems (A8) cannot be built.

On this engine the agreement does **not** hold. The 3b design
(`ref_docs/rune-cubical-phase1.md:131-142`) deliberately modelled validity as a
proof-irrelevant `holds : F -> Prop` (`store/sys.go:11`, `store/sys.go:29`)
precisely so that "two values on an overlapping face are definitionally equal
because the PROOFS of holding are." That bet failed. The finding is recorded at
`ref_docs/rune-cubical-phase1.md:239-244`, in `PARKING-LOT.md` ("System
overlap-agreement is not definitional"), and in `CLAUDE.md` (§F phase 3 interior
"FINDING").

Why it fails — grounded in the conversion algorithm:

- Conversion is **value-directed, not type-directed**:
  `func (m *Machine) Conv(lvl int, a, b Val) bool` (`core/conv.go:12`) is handed
  two `Val`s and **no type**. It therefore *cannot* ask "are `a` and `b` at a
  `Prop` type, so equate them blindly."
- Proof irrelevance is **constructor-keyed**, not type-keyed. The only
  irrelevance rules are: any two `VRefl` are equal (`core/conv.go:38-40`); `NCast`
  compares endpoints+subject and skips the proof (`core/conv.go:89-91`); `NSubst`
  skips the proof (`core/conv.go:93-96`). There is **no** rule "two arbitrary
  neutrals at a Prop type are equal." This is exactly the "Full definitional proof
  irrelevance" item parked in `PARKING-LOT.md` ("equating an arbitrary NEUTRAL
  proof with refl needs type-directed conversion").
- A `holds φ` proof is a `VNeu` headed by `htop`/`hand`/`horl`/`horr` (rigid
  intros, `core/eval.go:55`, `store/sys.go:23`). Two *different* proofs of the
  same `holds (fand φ ψ)` — e.g. `hand φ ψ hp hq` from the `φ` branch's witness
  vs. one reconstructed in the `ψ` branch — are distinct `VNeu` spines.
  `convSpine` (`core/conv.go:74`) compares them structurally, they differ, and
  `Conv` returns false.

Consequence for the consumers. The total-system rule
`hcomp A ⊤ u u0 ~> u i1 htop` (`core/eval.go:1208-1215`) and the degenerate
`comp` rules (`core/eval.go:1255-1271`) sidestep overlap entirely: a `⊤` or `⊥`
face has no two-branch structure, and the single proof handed in is always
`htop` from `SysHash(SRoleTop)`. The moment R-BOX fills a **proper** disjunctive
face — `ref_docs/wootz/R-BOX.md:42-54` calls this out as the bound on that node —
or A8 evaluates a user-written `for`-system, the per-branch lid expressions must
be proven convertible on the overlap, and they are not. So today multi-branch
systems are honest-stuck, and proper-face filling is blocked.

## Prior art (what the literature/other systems do; cite)

- **CCHM / cubicaltt** (Cohen–Coquand–Huber–Mörtberg,
  *Cubical Type Theory: a constructive interpretation of the univalence axiom*,
  TYPES 2015). A system `[ φ₁ ↦ u₁, …, φₙ ↦ uₙ ]` is well-formed only with the
  side condition `uᵢ = uⱼ : A` (definitionally) **on `φᵢ ∧ φⱼ`**. Compatibility
  is checked *at system formation* by the type-checker under the assumption of
  the meet face, restricting each branch to that face and converting the results.
  It is a **typing-rule-level** obligation, discharged by the checker, not a term
  that survives to runtime.
- **Cubical Agda** (Vezzosi–Mörtberg–Abel). `Partial φ A` carries a *more
  extensional* judgmental equality: two partial elements are equal iff they
  denote the same subcube — i.e. agree face-by-face (web: Agda cubical docs;
  HoTT blog "Cubical Agda"). `IsOne φ` is a **strict proposition** (`SProp`):
  *all* its inhabitants are definitionally equal because Agda has a genuine
  **type-directed** definitional proof irrelevance for `SProp`. This is exactly
  option (a) below, and the reason it works there is the type-directed checker.
- **cooltt** (RedPRL group). Implements the **full η/split law for disjunction
  of cofibrations**: `u : [φ ∨ ψ] A` is definitionally equal to its case-split,
  and conversion of two such elements splits on the disjuncts and checks each
  branch under its face assumption (web: "Every proof assistant: redtt"; cooltt
  README). Again the cofibration classifier is a strict-prop-like judgmental
  layer, *not* an ordinary `Prop` in the fibrant theory.
- **Sterling–Angiuli, Normalization for Cubical Type Theory** (arXiv 2101.11479)
  and **ABCFHL** Cartesian cubical (rwh "uniform" paper): the cofibration
  layer (`F`, `IsOne`) lives in a separate **judgmental stratum** with its own
  (strict, irrelevant) equality, kept disjoint from the fibrant universe. The
  metatheory (canonicity, normalization) is proved *with* that stratum baked in
  from the start.

The unifying lesson: **everyone makes the cofibration-validity layer a strict,
type-directed-irrelevant judgmental layer, separate from the fibrant `Prop`, and
discharges overlap as a checker-side typing obligation at system formation — not
as a value-level conversion of reconstructed proofs.**

## Chosen approach for THIS substrate (concrete; respects containment)

**Recommendation: option (b) — thread explicit agreement evidence through
systems and discharge overlap as a *checker-side obligation on the face
algebra*, NOT a value-level Prop-irrelevance rule. Do NOT add a global
irrelevant-Prop conversion rule to the outer core.** This matches the roadmap's
bracketed recommendation (`humble-humming-elephant.md:149-151`) and the spirit's
"the kernel never grows" (Thompson). Concretely, in three contained layers:

### Why NOT option (a) (the global irrelevant-Prop rule) — the metatheory veto

Option (a) is "in `Conv`, if `a` and `b` both have type some `Prop`, return
true." To implement it you must know the *type* of `a` and `b`. But `Conv`
(`core/conv.go:12`) has no type argument and no type-synthesis pass. Adding it
means one of:

1. **Type-synthesizing conversion** — rebuild `Conv` to carry a typing context
   and synthesize the type of each neutral head (the engine has `TypeOf`,
   `core/eval.go:17` / `store/store.go:103`, so head types are reachable). This is
   a *deep* rewrite of the outer-core metatheory: every `Conv` call site
   (`core/conv.go`, `elaborate/check_core.go:51`, `elaborate/check_surface.go`)
   must supply types; the Frame-Lemma dependency log now logs `TypeOf` reads it
   never logged before; and proof-irrelevant comparison of *open* neutrals at a
   `Prop` makes conversion sensitive to the universe of a subterm in a way the
   current spine-first fast path (`core/conv.go:50-53`) was built to avoid.
2. **A blanket SProp** — promote `holds`'s codomain from `Prop` to a new strict
   universe `SProp` with definitional irrelevance for *all* its inhabitants. That
   is a *new core constructor / universe* (hash-format bump, forbidden by the
   node's "no hash-format bump unless new core constructor" and by the standing
   "kernel never grows").

Both are exactly the "high-caution, touches the OUTER core metatheory" path the
roadmap flags (`humble-humming-elephant.md:309-311`). The hazard is concrete and
**not merely caution-theatre**: a value-level "all Prop inhabitants are equal"
rule, applied to *open* terms, is known to **break canonicity / decidability of
conversion** unless the proposition is genuinely a *strict* prop (no large
elimination out of it, definitional irrelevance proven admissible). Rune's `Prop`
is the **observational / impredicative** Prop of the Pujet–Tabareau stratum
(`CLAUDE.md` Phase 3), which already admits `Prop <: U` cumulativity
(`core/conv.go:115-119`) and Prop-valued eliminator motives. Blanket-equating its
inhabitants would let two observably-different proofs (e.g. two `Eq` proofs whose
*subjects* a downstream `subst`/`cast` actually transports along —
`core/conv.go:93-96` skips the proof but **uses** A/X/Y/P/Px) be conflated, and
interacts with quotient identification (`qsound`, v2) and the inner `pathU`/`ua`
data (`CLAUDE.md` v3.0.0) in ways that have **not** been shown sound on a
content-addressed, glued-NbE substrate. **This is research, not a checkbox**, and
it is the one place that risks the whole core. Park it.

### Option (b), made concrete: overlap as a typing obligation on faces

The insight from all prior art: overlap is a **typing-time** check, not a
runtime conversion. So keep `holds : F -> Prop` exactly as is, keep all five
intros rigid, change **nothing** in `core/conv.go`, and instead:

**(b1) A `system` smart-constructor with a discharged compatibility check
(contained, library/elaborator level).** Today a system is "just a `Pi`,
`holds φ -> El A`" (`store/sys.go:8`). That is too liberal — it lets you write a
disjunctive partial element with branches that disagree. Introduce a surface form

```
sys A φ [ φ₁ ↦ u₁ ; … ; φₙ ↦ uₙ ]
```

that elaborates to the same `holds φ -> El A` **but only after the elaborator has
discharged, for every pair i<j, the obligation**

```
under (h : holds (fand φᵢ φⱼ)) :  uᵢ (restrict h to holds φᵢ)  ≡  uⱼ (restrict h to holds φⱼ)   : El A
```

This is a `Conv` call (`elaborate/check_surface.go` style) at level `lvl+1` with a
fresh `holds (fand φᵢ φⱼ)` variable in scope, mirroring CCHM's "convert each
branch restricted to the meet face." It reuses the **existing** machinery — no new
conversion rule — because both sides are restricted to the *same* face and so
mention the *same* abstract proof variable, not two reconstructed `hand` spines.
The restriction `holds (fand φᵢ φⱼ) -> holds φᵢ` is `horl`/`horr`/`hand`
projection at the *type* level; but since the obligation is checked under an
**abstract** `h : holds (fand φᵢ φⱼ)`, and `holds` proofs are never inspected by
the branch bodies (they only gate definedness), the two branches end up applied
to the *same* variable `h` (after the canonical face-lattice reductions on
`fand φᵢ φⱼ ~> …` line up the `El` types), and conversion succeeds on the nose.

The teachable framing: *you may only build a disjunctive system if the branches
provably agree where the faces meet; the checker forces this at construction, the
same way CCHM does.*

**(b2) `holds`-proof opacity in the branch bodies (the property that makes b1
work, made explicit and tested).** The branch bodies `uᵢ : holds φᵢ -> El A` must
not **observe** their proof argument — there is no eliminator for `holds`
(`store/sys.go:23`), and the intros are rigid with no ι-rules, so a body *cannot*
case-split on a `holds` proof. This is already true structurally; b1 relies on it
and we promote it to a **stated invariant + property test**: substituting any two
proofs of `holds φ` into a well-typed branch body yields convertible results.
(This is "irrelevance for the branch bodies" obtained *for free* from the absence
of a `holds` eliminator — a contained, local fact, **not** a global conversion
rule.)

**(b3) Kan rules feed the *abstract* overlap proof, never reconstruct one.** When
R-BOX/A8 reduce `hcomp A (for φ ψ) u u0` on a face that forces to a single
disjunct (say `φ` holds, via `faceConst`/the lattice ι-rules `for ftop _ ~> ftop`
etc., `store/face.go:20-24`), the lid is `u i1 (horl … htop …)` — a *specific*
proof, exactly as the ⊤ rule already does (`core/eval.go:1213`). Overlap only
*matters* when **both** disjuncts could hold, i.e. on `fand φ ψ`; and there, b1
has already guaranteed the two branches agree, so whichever proof the rule
threads, the result is convertible. The evaluator therefore needs **no overlap
check at all** — it inherits b1's guarantee. The new evaluator work for A8 is just
the dispatch: read the disjunction structure of the face, and when one disjunct is
satisfied, thread the corresponding `horl`/`horr` proof.

### Containment summary

- `core/conv.go`: **untouched.** No irrelevant-Prop rule. (Thompson satisfied.)
- `core/eval.go`: a new dispatch in `tryHcomp`/`tryComp` for `for`-faces that
  threads `horl`/`horr` (additive, behind the existing `m.Sy`/`m.Fc` guards) —
  this is A8/R-BOX work that *consumes* this decision, listed under Unblocks.
- `store/sys.go`: optionally a reverse `SysHash(SRoleOrL/OrR)` is already present
  (`store/sys.go:91-103`) — used by b3, nothing to add.
- elaborator (`elaborate/`, `surface/`): the `sys` smart constructor + the
  pairwise compatibility obligation. This is the new code, and it lives **outside
  the kernel**, in the surface/elaboration layer.
- No new core constructor ⇒ **no hash-format bump**.

## Interfaces & signatures to add (Go + Rune surface as relevant)

Rune surface (new system form; sugar over `holds φ -> El A`):

```
-- a disjunctive system; elaborator discharges pairwise overlap
sys (A : UF) (φ : F) [ φ₁ ↦ u₁ ; φ₂ ↦ u₂ ; … ]  :  holds φ -> El A
   requires  φ ≡ for φ₁ (for φ₂ …)        -- the face is the join of the branches
   requires  ∀ i<j.  u_i ≡ u_j  under  holds (fand φ_i φ_j)
```

Go, in the elaborator (no core/store interface changes):

```go
// elaborate/system.go  (NEW; surface/elaboration layer, not core)

// SystemBranch is one (face, body) clause of a disjunctive system.
type SystemBranch struct {
    Face core.Tm // φ_i : F
    Body core.Tm // u_i : holds φ_i -> El A   (proof-opaque by construction)
}

// CheckSystem elaborates a disjunctive system, discharging the pairwise
// overlap-agreement obligation by conversion under the meet face. It returns
// the assembled partial element `holds φ -> El A` (a plain Pi/Lam — no new
// core form) or an error naming the first pair that fails to agree.
//
// Discharge per pair (i<j): under a fresh h : holds (fand φ_i φ_j),
//   left  = App(branches[i].Body, project_i(h))   // h restricted to holds φ_i
//   right = App(branches[j].Body, project_j(h))
//   require e.M.Conv(c.Lvl()+1, eval(left), eval(right))
// project_i / project_j use horl/horr/hand at the *type* of the proof; since the
// bodies are proof-opaque, both reduce to the same abstract h after the face ι.
func (e *Elab) CheckSystem(c Ctx, A, phi core.Tm, branches []SystemBranch) (core.Tm, error)
```

Two small, contained helpers (already mostly present):

```go
// store/sys.go already exposes SysHash(SRoleOrL), SysHash(SRoleOrR), SysHash(SRoleAnd)
// — b3 uses these in core/eval.go to thread horl/horr; nothing new to add.
```

Optional invariant test hook (b2), in `core` test surface only:

```go
// proofOpaque: substituting two distinct holds-proofs into a branch body yields
// convertible results. A property, not a code path — guards b1's soundness.
```

## Worked micro-example (the teachable artifact)

The smallest genuine two-branch system: a partial element on the *whole*
interval boundary `∂i = (i=0) ∨ (i=1)`, agreeing trivially because the two faces
**never overlap** (`fand (ieq0 i) (ieq1 i)` is `fbot`, and `holds fbot` is
uninhabited — no obligation). This is the on-ramp: overlap-agreement is *vacuous*
here, so it type-checks even on today's engine once `sys` exists, and it teaches
the *shape* before the hard case.

```
-- a path's own boundary as a system, used to fill a square (ch24, planned)
boundarySys (A : UF) (a b : El A) (p : I -> El A) (i : I) : holds (for (ieq0 i) (ieq1 i)) -> El A is
  sys A (for (ieq0 i) (ieq1 i))
    [ ieq0 i  |->  fn _ is a end      -- where i = 0, the value is a
    ; ieq1 i  |->  fn _ is b end ]    -- where i = 1, the value is b
  -- overlap obligation:  under holds (fand (ieq0 i) (ieq1 i)) = holds fbot,
  --                      DISCHARGED VACUOUSLY (no inhabitant) — checker emits nothing.
end
```

Then the *first non-vacuous* overlap (the artifact that proves the mechanism):
two faces that **do** meet, with agreeing values.

```
-- φ = (i=0), ψ = (j=0); they overlap on (i=0)∧(j=0), where both give `a`.
cornerSys (A : UF) (a : El A) (i j : I) : holds (for (ieq0 i) (ieq0 j)) -> El A is
  sys A (for (ieq0 i) (ieq0 j))
    [ ieq0 i  |->  fn _ is a end
    ; ieq0 j  |->  fn _ is a end ]
  -- overlap:  under h : holds (fand (ieq0 i) (ieq0 j)),
  --   left  = (fn _ is a end) (horl-restrict h) ~> a
  --   right = (fn _ is a end) (horr-restrict h) ~> a
  --   a ≡ a  ✓  — DISCHARGED, because the bodies ignore their proof (b2).
end
```

The teaching point a newcomer can hold: *the checker only asked whether the two
branches give the same answer where the faces meet; because neither branch looks
at its `holds` proof, "the same answer" is decided by ordinary conversion of the
bodies — no magic Prop rule.* Change the second branch to `fn _ is b end` and the
checker rejects it with "branches disagree on overlap `(i=0)∧(j=0)`: `a` vs `b`"
— an error that instructs (Savage).

## Risks / open sub-questions

- **Restriction projection details (b1).** The obligation is checked under a
  fresh `h : holds (fand φᵢ φⱼ)`, and each branch wants `holds φᵢ` / `holds φⱼ`.
  The projection is `hand`'s converse — but `hand` *builds* a meet-proof, it does
  not *split* one. We need either (i) the branch bodies to be proof-opaque so the
  *specific* proof never matters (b2 — the clean route; relies on no `holds`
  eliminator), or (ii) `holds`-projection intros `hpl : holds (fand φ ψ) -> holds
  φ`, `hpr : … -> holds ψ` (two more rigid members of the systems group — a
  contained group extension, *no* core change, no hash bump on existing groups).
  **Recommendation: rely on (i); add (ii) only if a body is found that is not
  proof-opaque (none can exist without a `holds` eliminator).** Mark (ii)
  research-adjacent.
- **`for`-associativity / n-ary faces.** A 3+-branch system's face is a nested
  `for`; pairwise overlap is `n(n-1)/2` obligations. Sound but quadratic; fine for
  the hand-sized systems A8/R-BOX need. cooltt's full disjunction-η would
  subsume this but is a larger build (research).
- **Does b1 actually line up the `El` types?** The two restricted bodies are at
  `El A` for the *same* `A`; the face only gates *definedness*, not the carrier,
  so the types are literally equal. Low risk; covered by the test plan.
- **Interaction with the inner-taint / deploy ban.** Systems are inner-tainted
  (check, don't deploy); `sys` inherits the taint. No new erasure question — the
  `holds` proofs already erase to unit (proof irrelevance, `CLAUDE.md` Phase 7).
- **The one genuine residual research risk:** if a *future* consumer needs
  overlap to hold *between two independently-reconstructed proofs at runtime*
  (not at system-formation), b1 does not cover it and only option (a) would —
  revisit then, exactly as the roadmap says ("revisit a global rule only if it's
  pervasive", `humble-humming-elephant.md:151`). No such consumer exists today.

## Test/gate plan

1. **Conversion is untouched — regression.** A snapshot test asserting
   `core/conv.go` has *no* type-directed Prop rule (grep-guard in a test comment)
   and that the existing Phase-3 conversion suite is byte-for-byte unchanged.
   Frame-Lemma property (`harness/`) still holds — no new `TypeOf` reads logged.
2. **Vacuous overlap (`boundarySys`).** `sys` on `for (ieq0 i) (ieq1 i)`
   elaborates and checks; the overlap face reduces to `fbot`, the obligation is
   skipped. (New listing ch24 candidate.)
3. **Non-vacuous agreeing overlap (`cornerSys`).** Elaborates and checks;
   property: substituting `horl`- vs `horr`-derived proofs into the assembled
   element yields convertible results (b2 invariant, via `rapid`).
4. **Disagreeing overlap is REJECTED.** `cornerSys` with `a` vs `b` fails
   elaboration with a located, instructive error. Mutation test: flipping the
   `Conv` in `CheckSystem` to always-true must make this test fail (the check is
   load-bearing).
5. **Kan threads the right proof (b3, gated with A8).** `hcomp A (for (ieq0 i)
   (ieq1 i)) u u0` with `i := i0` reduces via the lattice ι (`for ftop _ ~>
   ftop`) and the total-system rule to `u i1 (horl … htop …)`; assert the result
   equals the `φ`-branch lid. (Belongs to A8's gate; listed here as the
   downstream check this node unblocks.)
6. **Canonicity guard (Track X / X2).** Closed cubical terms built with `sys`
   still normalize to constructor form; no new stuck head leaks. Property test in
   `harness/cubical_props_test.go`.
7. **No hash drift.** Existing content hashes (interval/face/sys/path/fib/kan
   groups) byte-identical after the change — `sys` adds no core constructor.

## Unblocks (which implement nodes, and what they still need)

- **A8 (overlap mechanism + multi-branch systems)** — this node *is* A8's
  clarify gate. After C-OVERLAP, A8 is **ready-to-build** for the contained route
  (b1+b2+b3): the elaborator `sys` form + the `tryHcomp`/`tryComp` `for`-dispatch
  that threads `horl`/`horr`. A8 still needs: the surface grammar for `sys`
  (`surface/` + `ref_docs/GRAMMAR.md`), and the n-ary `for` decomposition helper.
- **R-BOX (proper-face filling / open boxes)** — `ref_docs/wootz/R-BOX.md:52-54`
  explicitly defers to this decision. With b1 in place, R-BOX can fill a proper
  *disjunctive* face: the per-former homogeneous rule (recursion on A's former,
  C-REG/R-FILL territory) now has a *well-formed* multi-branch system to fill,
  because formation already discharged overlap. R-BOX still needs: R-FILL
  (`transpFill`), C-REG (regularity decision), and the per-former box-fill
  recursion — C-OVERLAP only removes the *overlap* blocker, not the *filling*
  research.
- **Does NOT unblock** the deep inner univalence path (A6/A7, Glue/ua) directly —
  those need R-GLUE/R-SIGMA. But Glue's own boundary system (`unglue` on the
  `for`-face of the partial equivalence) will consume b1 when it lands.

**Status: ready-to-build (option b).** The metatheory-risky option (a) is
deliberately **left as research** and parked; b1+b2+b3 are concrete, contained,
and touch no outer-core conversion code. The single load-bearing assumption
(branch bodies are proof-opaque, b2) is a *consequence* of the existing
"no `holds` eliminator" design, not a new postulate, and is property-tested.
