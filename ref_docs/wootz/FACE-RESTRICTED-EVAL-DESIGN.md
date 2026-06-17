# FACE-RESTRICTED-EVAL-DESIGN — cofibration-indexed reduction (the M2 gate)

*Design doc for the authorized face-restricted-evaluation project. NO kernel edits
until this is reviewed. The goal: supply the one operation the proper-face Glue
Kan fixup needs — viewing a value "on the face where a cofibration holds" — so
`compGlue` (verbatim from cubicaltt, see below) can be ported, retiring the `ua`
postulate (M2). §F discipline: every step refl-pinned / property-tested; the
canonicity gate (X2) extended before the fixup lands.*

## 1. The problem, precisely (from the verbatim source)

cubicaltt's `compGlue` (fetched from `mortberg/cubicaltt` `Eval.hs`) uses **value
face-restriction `v `face` γ`** pervasively — e.g.
`fill i (equivDom equivG) (wi0 `face` gamma) (ws `face` gamma)`. `face` views a
value at the sub-cube where the cofibration γ holds: there a `Glue`-typed value
*is* a `T`-typed value, an `equivs`-system picks out a single equivalence, etc.

Our substrate has no analog. The boundary `El (Glue A φ T e) ~> El (T htop)`
fires only when φ **literally** reduces to `⊤` via `faceConst` (`core/eval.go`),
never under an abstract `h : holds φ`. So under a proper φ we cannot treat the
input as a `T`-element — which every non-degenerate `compGlue` branch needs.
`fsplit` dispatches a value *by* face; it does not restrict a value's *type*
under a face assumption. Different operation; the gap is real.

## 2. Design decision — restriction by quote→substitute→eval (NOT an ambient context)

cubicaltt implements `face`/`act` via a `Nominal` typeclass: every `Val`
constructor knows how to be acted on by an interval substitution. That is the
"pervasive, non-additive" route I flagged — it touches every value constructor.

**This substrate has a cheaper, contained route that reuses trusted machinery.**
A face is a conjunction of atomic constraints `(iₖ = bₖ)`. Restricting a value to
such a face is an **interval substitution**, and we already have a sound
term-level interval substitution path through quote+eval:

```
restrict(v, [i₀↦b₀, …, iₙ↦bₙ]) :=
    m.Eval(env, substIntervals(m.Quote(lvl, v), [iₖ ↦ bₖ]))
```

- `Quote` (already total, already used everywhere) turns the value back to a `Tm`.
- `substIntervals` replaces the interval `Var`s for the named levels with the
  endpoint terms `i0`/`i1` — a pure syntactic term rewrite (no new value action).
- `Eval` re-normalizes. After substitution the atomic faces reduce
  (`ieq0 i0 ~> ftop`, `core/eval.go`), so `for`/`fand` collapse, and the **existing**
  `El (Glue A ⊤ T e) ~> El (T htop)` boundary fires — *no new boundary rule
  needed*. The restriction's correctness is inherited from quote+eval, which are
  already the trusted core of NbE.

Trade-off: a quote+eval per restriction (slower than an in-place `act`). For the
Kan rules this is acceptable (restrictions are at endpoints/faces, not hot
loops); if it ever matters, the in-place `Nominal` action is a later optimization
of the *same* semantics. **Worse-is-better (Thompson): the slow, obviously-sound
version first; optimize only if a benchmark forces it.**

This converts the "foundational re-architecture" into a **contained value
operation** built from existing pieces — a much smaller, verifiable surface than
threading a cofibration context through every ι-rule.

## 3. Soundness obligations (must hold before the fixup lands)

1. **Restriction commutes with reduction** — `restrict(eval t, σ) ≡ eval(σ t)`.
   Holds by construction (restrict *is* eval∘subst∘quote, and quote∘eval = id up
   to conversion). Property test: random closed cubical `t`, random endpoint σ.
2. **Restriction at a satisfied face exposes the boundary** — `restrict(El (Glue A
   φ T e), σ)` with `σ ⊨ φ` converts to `El (T htop)`. Refl-pin per face shape.
3. **Idempotence / order-independence** of independent atomic substitutions.
4. **No effect off-support** — restricting at `iₖ` not occurring in `v` returns `v`
   (up to conversion).
5. **Canonicity (X2)** — closed terms built through the restored `compGlue`
   normalize to a `glue`/constructor form; extend `harness/cubical_props_test.go`.

## 4. Build plan (incremental, each a green checkpoint; reviewed before kernel edits)

1. **`substIntervals` term rewrite** (`core/`): replace given interval `Var`s with
   `i0`/`i1` refs. Pure, unit-tested. *(No eval change.)*
2. **`restrict` value op** (`core/eval.go`): `Quote ▸ substIntervals ▸ Eval`.
   Property-test obligations 1–4 above.
3. **Proper-face boundary via restrict** — verify obligation 2 (restricting a
   neutral `Glue` at a satisfied face yields the `T`-decode). Refl-pinned listing.
4. **Port `compGlue` for the single-equiv face** using `restrict` + the landed
   pieces (`unglue`, `transp`/`comp`, `hfill`/`compFill`, `equivProof`,
   `fiber`/`isContr`, `fsplit`, `forallF`). Refl-pin: `unglue (result) ≡`
   transported base; `⊤`-face ≡ transp-over-T.
5. **General `compGlue`** (disjunctive φ via per-disjunct `fsplit`) + the
   hcomp/comp seams.
6. **X2 canonicity** sweep over closed transp-over-Glue terms.
7. **A7** — once transp-over-Glue computes, derive `ua` as the Glue line (needs
   `pabsU`, type-level path abstraction — its own sub-task) and retire the
   postulate; the deliberate `fib`-hash event + ch10 update.

Steps 1–3 are pure additive/contained and carry no soundness risk beyond their
property tests. Step 4 is the first place the deep formula enters; it is gated on
1–3 being green and is itself refl-pinned against known boundaries. Nothing is
committed to the kernel until its checkpoint is green and the approach reviewed.

## 5. Risks / open questions

- **Quote of cubical values** must be total on every Kan/Glue/face value (it is
  today — all are quoted in tests). Confirm no value escapes `Quote`.
- **Performance** of quote+eval restriction in nested Kan rules — measure; the
  `Nominal` in-place action is the fallback optimization (same semantics).
- **`∀i.φ` interaction** — `forallF` (landed) supplies the cofibration; confirm
  restriction interacts correctly with a `forallF`-faced system.
- **Regularity (C-REG)** — restored `compGlue` is the canonical regularity
  breaker; X2 canonicity is the gate (non-regular CCHM is canonical, per
  R-FILL's C-REG resolution).

## 6. Status — UPDATED (most of steps 1–7 landed)

The foundation is built. As landed in `core/`:
- **Step 1–2 (`substIntervals`/`restrict`):** shipped as **`RestrictIv`**
  (`core/quote.go:19`) — value face-restriction along an atomic interval
  constraint via quote→subst→eval, exactly this design. Verified standalone.
- **Step 4 (port `compGlue`, canonical case):** shipped as **`transpGlueIntro`**
  (`core/eval.go:2768`) for a `glue`-INTRO input over a φ-constant line (no value
  restriction needed); plus **`extend`** (`core/eval.go:2717`), the contractibility
  filler. hcomp/comp-over-Glue re-glue also landed (ch50).

**Remaining (the M2 residue):**
- **G1 (steps 4–5, general case):** wire `RestrictIv` + `extend` through the full
  `compGlue` for a **neutral input** or **φ varying in i** — the arm at
  `core/eval.go:2913-2917` that still returns `nil,false`. The genuine consumer of
  the FRE foundation.
- **G3 (step 6):** X2 canonicity sweep over closed general transp-over-Glue terms.
- **G2 (step 7 / A7):** re-derive `ua` as the Glue line via `pabsU` (landed,
  `store/pathu.go`) and retire the postulate — the deliberate `fib`-hash + ch10 event.

Note: `pabsU` (type-level path abstraction, step 7's "own sub-task") is already
landed, and `fsplit` (ch52) discharges C-OVERLAP, so the disjunctive-φ machinery
needs no new elaborator construct.

## 7. Execution-ready spec — the general `transpGlue` arm (G1)

*The arm that replaces `nil,false` at `core/eval.go:2913-2917` (after `transpGlueIntro`
returns false). Generalizes the landed `transpGlueIntro` (`eval.go:2768`) from a
`glue`-INTRO / φ-constant input to a **neutral input** and/or **φ varying in i**.
Every helper named here is landed. Map onto the structure cubicaltt's `compGlue`
gives; refl-pin each labelled sub-equation before the arm goes live.*

Inputs: a Glue line `ALine = λi. Glue (A i) (φ i) (T i) (e i)` (via `glueFormer ∘ Apply`,
already used by `transpGlueIntro`'s `part(iv,k)`) and an input `g0 : El (Glue (A i0) …)`.
Let `Ai`, `φi`, `Ti`, `ei` abbreviate `part(i,k)`; `ψ := forallF (λi. φ i)` (landed
`ARoleForall`, the "φ holds on the whole line" cofibration).

```
# 1. base: pull g0 down to A i0, transport along the A-line (both LANDED)
a0   := unglue (A i0)(φ i0)(T i0)(e i0) g0            # unglue ι (β on intro, neutral o.w.)
ã    := transpFillF aLineOnly fbot a0                  # transport fill i→·  (LANDED)
a1'  := vTranspG transpGH aLineOnly fbot a0            # transp over the A-line (LANDED)

# 2. T-component on φ i1.  KEY DIFFERENCE from transpGlueIntro:
#    a glue-intro hands us t directly; a NEUTRAL g0 does not.  Recover it where φ holds:
#    on ψ (φ total on the line) g0 IS a T-element by the El(Glue ⊤)~>El(T) boundary —
#    expose it with RestrictIv at the satisfied face, then transport the T-line.
t1   := λh. vTranspG transpGH (λi. Ti h) fbot (Tcomp_of g0 at h)
#         where Tcomp_of is: on a glue intro, gargs[4] h (as today);
#         on a neutral g0, RestrictIv g0 to the atomic face of (φ i1) that holds,
#         exposing the El(T) decode (obligation 2, TestRestrictIvBoundary).

# 3. reconcile the base on φ i1 via the contractible fibre (extend, LANDED)
#    extend B contr ϕ u : an El B agreeing with the partial u on ϕ, composed from
#    the centre of contraction.  Here B = A i1, contr = equivProof (e i1) a1' on the
#    fibre of equivFun(e i1) at a1', ϕ = φ i1, u = λh. <naturality wall of t1,a1'>.
a1   := hcomp (A i1) (φ i1) (extend-wall a1' t1 (e i1)) a1'    # proper-face hcomp (LANDED)

# 4. re-glue
result := glue (A i1)(φ i1)(T i1)(e i1) (λh. t1 h) a1
```

**Soundness gates (each blocks the arm going live):**
1. **Single-atomic restriction sufficiency.** `RestrictIv` restricts one atomic
   constraint (`i:=i0`/`i:=i1`); cubicaltt's `v `face` γ` restricts a general face.
   The arm is sound **only** where every restriction it performs is along an atomic
   endpoint face — true for the **ua line** (`φ = (ieq0 i) ∨ (ieq1 i)`, atomic
   disjuncts resolving to ⊤/⊥ at i0/i1) and any φ built from endpoint atoms. For a
   genuinely multi-atom face the general `Nominal`/`act` restriction is required —
   **gate the arm on "φ’s atoms are endpoint-atomic", else stay `nil,false`.** This
   keeps the ua case (G2’s consumer) sound without claiming fully-general `compGlue`.
2. **Confluence with `transpGlueIntro`** on the overlap (glue intro, φ constant):
   the general arm ≡ `transpGlueIntro` (refl-pin).
3. **⊤-degeneracy agreement:** when `ψ ~> ⊤`, the arm ≡ the landed ⊤ rule
   (`transp` over the T-line, `eval.go:2893`).
4. **unglue-β round-trip:** `unglue (result) ≡ a1` (refl-pin, mirrors
   `TestTranspGlueIntroUnglueBeta`).
5. **X2 canonicity:** closed terms through the arm normalize to a `glue`/constructor
   form (extend `TestTranspGlueIntroCanonicity` to neutral/varying inputs).
6. **X1 frame exactness:** the `extend`/`equivProof` branch is on the fibre *point*,
   not proof content — confirm cache-exact (`conv.go` skips proof content) or gate.

**Why this is review-gated, not committed-on-sight.** The verbatim cubicaltt
`compGlue` (the only fully-trusted reference) is not in-repo; the arm above is the
substrate-mapped reconstruction. CLAUDE.md/MILESTONE discipline: a deep Kan-over-Glue
rule enters the kernel only behind refl-pins **and** the canonicity sweep, reviewed —
not under delivery pressure. Land gates 1–6 green, then flip the arm on; only then
take G2’s deliberate `fib`-hash event (re-typing `ua`), which *consumes* this arm.

Sources: cubicaltt `Eval.hs` `compGlue`/`unGlue`/`extend`/`mkFiberType`/
`pathComp` (verbatim in session transcript — NOT in-repo; re-fetch before landing),
CCHM [arXiv:1611.02108 §6](https://arxiv.org/pdf/1611.02108);
landed `core/eval.go` (`transpGlueIntro`, `extend`, `transpFillF`, `vTranspG`,
`glueFormer`, faceConst, the Glue/Equiv/fsplit/forall groups), `core/quote.go`
(`RestrictIv`).
