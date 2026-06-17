# R-SIGMA — sigmaF: Sigma codes in UF + Kan

## Problem (what's stuck/absent today, with file:line)

The fibrant universe `UF` is closed under `Pi` (`piF`) and the path former
(`pathF`), but **not** under `Sigma`. There are no inner pair codes, no pair
introduction, and no projections. This blocks the M2 critical path directly:

- `ua` is currently a four-component telescope baked into one postulated head —
  `store/fib.go:201-221` (`uaTy`: `f`, `g`, `s`, `t` as four separate `Pi`
  arguments). It is *not* `(A B : UF) -> Equiv (El A) (El B) -> pathU A B`,
  because `Equiv X Y = Σ (f : X -> Y), isEquiv f` cannot be written: `Σ` does
  not exist at the inner level, and `isEquiv f = Π y, isContr (fiber f y)` needs
  `fiber f y = Σ (x : X), f x = y`, again `Σ`. `Glue` (R-GLUE / A6) consumes
  `Equiv`; with no `Equiv`, `Glue` cannot be stated, let alone given Kan
  equations. PARKING-LOT.md:169-172 records this exactly: *"`Equiv` as a
  first-class fibrant type … waits on it."*

- The **outer core has no Sigma at all**. `core/term.go:176-189` enumerates the
  entire sealed `Tm` set: `Var, Meta, Prop, Eq, Refl, Cast, Subst, Ref, Univ,
  Pi, Lam, App, Let, Ann`. There is no `Sig`/`Pair`/`Fst`/`Snd`. `core/val.go`
  mirrors this: `VPi, VLam, VNeu, VU, VProp, VEq, VRefl` — no pair value, no
  projection neutral. The v1 design's `Sig`/`Pair` were parked
  (PARKING-LOT.md:71-74, "they still have no consumer"). R-SUM (DAG line 194,
  `C1`) would add them to the *outer* core, but R-SUM is on a *different track*
  (Track C, the matured core) and is an explicit last-resort outer-core change
  (roadmap line 84-86: "only C-OVERLAP / R-SUM qualify"). **R-SIGMA must not
  depend on R-SUM** — the cubical critical path (A5) cannot wait on the core
  maturation track, and Thompson forbids growing the outer core to serve an
  inner-stratum need.

- Kan over a Sigma code is therefore also absent. `store/kan.go` /
  `core/eval.go` ship structural rules only for `piF` (`tryTransp`
  eval.go:1141-1169, `tryHcomp` eval.go:1222-1237, `tryComp` eval.go:1279-1303);
  `fibFormer` (eval.go:1177-1196) can already *recognise* any fibrant former, so
  the dispatch site is ready, but there is no `FRoleSigmaF` to dispatch on.

The task: design `sigmaF` as a fibrant builtin group closing `UF` under `Sigma`,
with `El` decoding it, projections that compute, and transp/hcomp/comp rules —
**without** an outer Sigma, **without** a hash-format bump (no new core
constructor), the outer OTT core untouched.

## Prior art (what the literature/other systems do; cite)

- **CCHM / Cubical Agda.** `transp` and `hcomp` on a Σ-type compute
  *componentwise*. The Agda manual notes "transp … is computed elementwise" for
  Σ. The standard CCHM equations (Cohen–Coquand–Huber–Mörtberg, *Cubical Type
  Theory*, and Mörtberg's cubicaltt) are:

  - `transp (λi. Σ (A i) (B i)) φ (a,b) = (a', b')` where
    `a' = transp (λi. A i) φ a` and
    `b' = transp (λi. B i (transpFill (λi. A i) φ a i)) φ b`. The second
    component must transport along the *fill* of the first component's
    transport, because `B` depends on the first projection, and that projection
    is sliding as `i` moves. **This needs `transpFill` (R-FILL).**
  - `hcomp (Σ A B) φ u u0 = (a₁, b₁)` where
    `a₁ = hcomp A φ (λi h. (u i h).1) u0.1` and
    `b₁ = comp (λi. B (hfill A φ (λi h. (u i h).1) u0.1 i)) φ
              (λi h. (u i h).2) u0.2`. The first projection is a homogeneous
    composite; the second is a *heterogeneous* composite over the line traced by
    the first projection's fill. **Needs `hfill` (= R-FILL).**

  Sources:
  [Cubical — Agda docs](https://agda.readthedocs.io/en/latest/language/cubical.html),
  [cubicaltt univalence.ctt](https://github.com/mortberg/cubicaltt/blob/master/examples/univalence.ctt),
  [Wikipedia: Cubical type theory](https://en.wikipedia.org/wiki/Cubical_type_theory).

- **`fiber` / `isEquiv` / `Equiv` as Σ-types** (cubicaltt, Agda
  `Cubical.Foundations.Equiv`): `fiber f y = Σ x, f x = y`,
  `isContr A = Σ a, Π x, a = x`, `isEquiv f = Π y, isContr (fiber f y)`,
  `Equiv A B = Σ f, isEquiv f`. Every layer is Σ — this is *why* `Glue` needs
  Σ codes. [Agda.Builtin.Cubical.Glue](https://cj-xu.github.io/agda/ordinals/Agda.Builtin.Cubical.Glue.html).

- **Σ without primitive Σ (Church / Π-encoding).** Impredicative encodings give
  `Σ A B ≅ Π (C : U), (Π a, B a -> C) -> C`. Rune already uses an impredicative
  encoding for propositional truncation in the outer layer (CLAUDE.md, v2.0.0
  line: "Prop's impredicativity already contains propositional truncation").
  The encoding's defect is well known (Geuvers; the "weak" Σ): projections are
  *not* definitional and η fails, which breaks the Kan equations that quote
  pairs back. cubicaltt and Cubical Agda therefore keep Σ **primitive**, not
  encoded.

- **The builtin-group pattern in this kernel** (CLAUDE.md "v2.0.0", "v3.0.0",
  "§F phase 1/2/3"): quotients, the fibrant kit, the interval, paths, faces,
  systems, and Kan are each a *group of bodiless content-addressed definitions
  with permanently-neutral heads and ι-rules in the evaluator* — and *no
  hash-format bump* because no new core constructor is introduced
  (store/fib.go:1-38, store/kan.go:1-28). This is the template R-SIGMA follows.

## Chosen approach for THIS substrate (concrete; respects containment)

**Decision 1 — sigmaF is a primitive builtin group with computing projections,
NOT a Π-encoded pair.** The deciding constraint is Kan: `transp`/`hcomp` over
`sigmaF` must *quote a pair back* and the projections of that pair must reduce
definitionally (the CCHM formulas above project `(u i h).1`/`.2` and re-pair).
A Geuvers/Church encoding makes `fst (pair a b)` only *propositionally* equal to
`a`, so the structural Kan rules would not be confluent with the boundary rules
and `refl`-pinning (the ch23 teaching device) would fail. We therefore ship
genuine ι-rules `fstF (pairF … a b) ~> a`, `sndF (pairF … a b) ~> b`, exactly as
`papp (pabs …) ~> f i` and `qlift … (qin … a) ~> f a` are genuine ι-rules.

**Decision 2 — `El (sigmaF A B)` stays NEUTRAL; the content lives in the
projection ι-rules. It does NOT decode to any outer type, and does NOT need
outer Sigma (R-SUM).** This is the direct answer to the task's central question.

`El (piF A B)` decodes to a real outer `Pi` because outer `Pi` *exists*
(eval.go:790-798). Outer Σ does **not** exist, so there is nothing for
`El (sigmaF A B)` to decode *to* — and crucially, nothing *needs* it to. The
inner identity type already demonstrates the pattern: `El (pathF A x y)` is left
**neutral** (`tryFibIota` returns `nil,false` for non-`fib`/`piF` formers,
eval.go:799-800), yet its inhabitants `pabs`/`papp` compute perfectly
(eval.go:946-973). `sigmaF` follows `pathF`, not `piF`:

- `El (sigmaF A B)` is an abstract fibrant type (neutral `El`-application). `El`
  gains **no new case** — `sigmaF` falls through the existing `nil,false` branch
  by design.
- Its inhabitants are `pairF`-headed (a permanently-neutral canonical intro,
  like `pabs`/`qin`).
- Its destructors `fstF`/`sndF` compute by ι: `fstF (pairF … a b) ~> a`,
  `sndF (pairF … a b) ~> b`. This is the *entire* "decoding" — there is no
  second route.

This is **strictly more contained than `piF`**: `piF` reaches into the outer
core to build a real `VPi` (eval.go:795-797); `sigmaF` touches nothing outer —
it is a closed inner construction. It is also simpler and safer than the
alternative considered below, which had two reduction paths to keep confluent.

*Alternative considered and rejected — El decodes to an impredicative weak-Σ:*
one could decode `El (sigmaF A B) ~> (C : U) -> ((x : El A) -> El (B x) -> C) ->
C` (an outer `Pi` chain that does exist) and route `pairF`/`fstF`/`sndF` through
it. **Rejected** because (a) it creates *two* β-paths for `fstF (pairF a b)` —
the native ι and the encoded eliminator — which must then be proven confluent, a
gratuitous proof obligation; (b) the weak-Σ encoding has no definitional `sndF`
strong rule anyway (Prior art: Geuvers), so the native ι is needed regardless,
making the encoding pure overhead; (c) it is *less* contained (it manufactures an
outer carrier `El (sigmaF …)` is convertible to, enlarging what the outer layer
can observe). The neutral-`El` design needs none of this. *If a future chapter
needs to destructure a fibrant pair in the OUTER language via `El`* (none does
today), the encoded decoding can be added then as an opt-in — or R-SUM can supply
a native outer Σ. Neither is on the A5→A6 path.

**Decision 3 — second-projection type uses `fstF` itself.** `sndF`'s type is
`(p : El (sigmaF A B)) -> El (B (fstF A B p))`.
`fstF` appearing in a *type* is exactly what makes the family `B` land on the
right point; because `fstF (pairF a b) ~> a` definitionally, `sndF (pairF a b) :
El (B a)` checks on the nose. (This is the dependent-projection discipline of
cubicaltt's `p.2 : B p.1`.)

**Decision 4 — sigmaF is NOT added to the existing 11-member fib group.** It is
its own builtin group on its own hash space (like path/face/sys/kan), so fib's
hashes — and ch09–ch23 — stay byte-identical. The group has **four** members:
`sigmaF`, `pairF`, `fstF`, `sndF`. It is registered *after* `AddFib` (it
references `UF`/`El`), parallel to `AddPath(fhs, ihs)`.

**Decision 5 — Kan over sigmaF ships in TWO increments, honest about R-FILL.**

- *Increment A5a (ready-to-build, transpFill-free):* the **componentwise rules
  whose B does not vary**, and **hcomp on a CONSTANT (non-dependent) family**:
  - `transp (λi. sigmaF (A i) (B i)) (pairF a b)` fires *only when `B` is
    `i`-constant AND non-dependent* (i.e. `sigmaF` degenerates to a non-dependent
    product `A i × B`): then
    `~> pairF (transp (λi. A i) a) (transp (λi. B) b)` — both components are
    independent transports, no fill needed. Detected with the existing
    `mentionsRefVal` / `kanFreshSentinel` constancy probe (eval.go:1126-1129,
    1149) applied to the second component family.
  - `hcomp (sigmaF A B) φ u u0 ~> pairF (hcomp A φ (λi h. fstF … (u i h)) (fstF
    … u0)) (hcomp (B (fstF … u0)) φ (λi h. sndF … (u i h)) (sndF … u0))` fires
    *only when `B` is non-dependent* (so the second `hcomp`'s type does not slide
    with the first projection). Non-dependence of `B` is the freshness probe
    again. This is the genuine teachable artifact of A5a.
  - `comp` over a constant `sigmaF` line with non-dependent `B`: componentwise,
    same shape.
- *Increment A5b (needs R-FILL):* the **fully dependent** transp/hcomp/comp,
  where the second component transports along `transpFill`/`hfill` of the first.
  These are *labelled honest-stuck* until R-FILL lands, exactly as varying-domain
  `piF` is today (CLAUDE.md §F-structural "STILL the labelled remainder").

This keeps A5 a green contained checkpoint and lets `Equiv` be *stated* (the
non-dependent product cases cover `Equiv = Σ f, isEquiv f` where `isEquiv f` does
not depend on a *cube of* `f` at transp time — the common Glue use is at a fixed
`f`), deferring only the deepest fills to R-FILL.

## Interfaces & signatures to add (Go + Rune surface as relevant)

### Rune surface (the four member types, store/sigma.go)

```
sigmaF : (A : UF) -> (B : El A -> UF) -> UF
pairF  : (A : UF) -> (B : El A -> UF) -> (a : El A) -> (b : El (B a)) -> El (sigmaF A B)
fstF   : (A : UF) -> (B : El A -> UF) -> (p : El (sigmaF A B)) -> El A
sndF   : (A : UF) -> (B : El A -> UF) -> (p : El (sigmaF A B)) -> El (B (fstF A B p))
```

(`A`, `B` carried explicitly as in `pabs`/`papp` so the heads are saturated and
the evaluator can read them off the spine; surface sugar can make them implicit
later — parked.)

### core/eval.go (FibInfo extension — no new core constructor)

Add to `FibRole` (core/eval.go:129-154):

```go
FRoleSigmaF // closes UF under Sigma
FRolePairF  // pair introduction (permanently neutral, like pabs/qin)
FRoleFstF   // first projection (computes by ι)
FRoleSndF   // second projection (computes by ι)
```

These extend the *existing* `FibInfo` interface (no new interface): `FibRoleOf`
already returns `FibRole`, and `FibHash(FibRole)` already exists for the reverse
lookup the structural Kan rules need (eval.go:158-163). **Caveat:** because the
sigmaF members live in a *separate group* (Decision 4) but share the `FibRole`
enum and the `FibInfo` methods, `Store.FibRoleOf` / `Store.FibHash` must consult
*both* the `fib` entry and the new `sigma` entry. Cleanest: give the store a
`*sigmaEntry` and have those two methods fall through to it. (Alternative:
mint a parallel `SigmaInfo` interface + `Si` Machine field, mirroring
`PathInfo`/`Pa`. The `FibRole`-reuse option is lighter and keeps `fibFormer`
working unchanged, since it already dispatches on any `FibRole`; recommend it.)

### core/eval.go (tryFibIota extension)

Extend the `tryRules` dispatch (eval.go:673-678) so `FRoleEl`, `FRoleFstF`,
`FRoleSndF` are recognised as computing heads, then in `tryFibIota`:

```go
// FRoleEl gains NO new case — El (sigmaF A B) stays neutral (Decision 2),
// like El (pathF …) today (eval.go:799-800).
case FRoleFstF: // fstF A B (pairF A' B' a b) ~> a
    if len(args) == 3 && fibHeadIs(m, args[2], FRolePairF, 4) {
        _, pa := spineParts(force(args[2]).Spine); return pa[2], true }
case FRoleSndF: // sndF A B (pairF A' B' a b) ~> b
    if len(args) == 3 && fibHeadIs(m, args[2], FRolePairF, 4) {
        _, pa := spineParts(force(args[2]).Spine); return pa[3], true }
```

Only `FRoleFstF`/`FRoleSndF` are added to the computing-head set in `tryRules`
(eval.go:673-678); `FRoleEl` already handles `sigmaF` correctly by falling
through to neutral.

`pairF` is a permanently-neutral head (added to `rigidHead`, eval.go:432-462 —
the `m.Fib.FibRoleOf(h) != FRoleNone` check already covers it once it is in the
fib-role space; only the *separate group* membership needs wiring).

### core/eval.go (Kan dispatch — increment A5a)

In `tryTransp`/`tryHcomp`/`tryComp`, after the `FRolePiF` case in the
`fibFormer` switch, add `case FRoleSigmaF:` with the componentwise formulas of
Decision 5, each guarded by a `mentionsRefVal(…, kanFreshSentinel)` non-variance
probe on the second-component family (the same guard that makes the `piF`
constant-domain case fire, eval.go:1149, 1282).

### store/sigma.go (new file, the group)

`AddSigma(fib [11]core.Hash) [4]core.Hash` (parallel to `AddPath`), `sigmaNames
= [4]string{"sigmaF","pairF","fstF","sndF"}`, `SigmaRoleOf`/`SigmaHashes`,
group tag bytes `{defFormatVersion,'G'}` / `{defFormatVersion,'g'}` (unused
letters; 'S'/'s' are taken by systems). Registered in
`internal/session/session.go:73-77` right after `AddFib`, before `AddPath`.

## Worked micro-example (the teachable artifact)

The A5a deliverable, in the ch23 `refl`-pinning style — a fibrant pair, its
projections computing, and `hcomp` pushed through a non-dependent product:

```
-- ch24_sigma.rune  (§F phase 4 — sigmaF, the bridge to Equiv/Glue)

-- Projections compute definitionally: refl certifies fstF/sndF fire.
fstBeta : (A : UF) -> (B : El A -> UF) -> (a : El A) -> (b : El (B a)) ->
    Eq (El A) (fstF A B (pairF A B a b)) a is
  fn (A : UF) (B : El A -> UF) (a : El A) (b : El (B a)) is
    refl a
  end
end

-- hcomp over a NON-DEPENDENT fibrant product is componentwise. With B constant
-- (no dependence on the first projection), the pushed form is two parallel
-- hcomps re-paired; refl certifies the push fired (a stuck hcomp at a sigma type
-- eta-differs from a pairF).
hcompProduct : (A : UF) -> (B : UF) -> (phi : F) ->
    (u : I -> holds phi -> El (sigmaF A (fn (_ : El A) is B end))) ->
    (u0 : El (sigmaF A (fn (_ : El A) is B end))) ->
    Eq (El (sigmaF A (fn (_ : El A) is B end)))
       (hcomp (sigmaF A (fn (_ : El A) is B end)) phi u u0)
       (pairF A (fn (_ : El A) is B end)
          (hcomp A phi
             (fn (i : I) is fn (h : holds phi) is
                fstF A (fn (_ : El A) is B end) (u i h) end end)
             (fstF A (fn (_ : El A) is B end) u0))
          (hcomp B phi
             (fn (i : I) is fn (h : holds phi) is
                sndF A (fn (_ : El A) is B end) (u i h) end end)
             (sndF A (fn (_ : El A) is B end) u0))) is
  fn (A : UF) (B : UF) (phi : F)
     (u : I -> holds phi -> El (sigmaF A (fn (_ : El A) is B end)))
     (u0 : El (sigmaF A (fn (_ : El A) is B end))) is
    refl (pairF A (fn (_ : El A) is B end)
            (hcomp A phi
               (fn (i : I) is fn (h : holds phi) is
                  fstF A (fn (_ : El A) is B end) (u i h) end end)
               (fstF A (fn (_ : El A) is B end) u0))
            (hcomp B phi
               (fn (i : I) is fn (h : holds phi) is
                  sndF A (fn (_ : El A) is B end) (u i h) end end)
               (sndF A (fn (_ : El A) is B end) u0)))
  end
end
```

The lesson (Savage): *"a Sigma fills component by component — its first
projection fills by itself, its second fills over the path the first traces."*
The non-dependent case is the honest, teachable half; the dependent case is the
sentence "over the path the first traces", which is `transpFill` — named, not yet
computed.

## Risks / open sub-questions

1. **`El (sigmaF …)` is neutral, not an outer type (no Σ-η through `El`).**
   Unlike `El (piF A B)` (a real outer `Pi`), `El (sigmaF A B)` stays neutral
   (Decision 2). Consequence: there is no outer carrier to η-expand to, so
   Σ-η (`p ≡ pairF (fstF p) (sndF p)` for neutral `p`) is not definitional (see
   Risk 2). This is the honest cost of routing around R-SUM, and it is the *same*
   shape as `pathF` already living neutral. It is sufficient for `Equiv`/`Glue`,
   which manipulate fibrant pairs with `fstF`/`sndF`, never by destructuring
   through `El`. **If R-SUM later lands** *and* a chapter needs outer
   destructuring, an opt-in `El (sigmaF …)` decoding to native outer `Sig` can be
   added — *no hash change to the sigmaF group* (only an ι-rule body changes, not
   the member types). Label: ready-to-build as neutral; outer-destructuring is
   research, demand-gated, and off the A5→A6 path.

2. **η for sigmaF.** CCHM Σ has definitional η (`p ≡ pairF (fstF p) (sndF p)`).
   Because `El (sigmaF …)` is neutral (no outer carrier, Decision 2), this engine
   does *not* get Σ-η for free, and a definitional η would be a conversion rule
   keyed on the `sigmaF` type in `core/conv.go` (a conversion change —
   Thompson-sensitive). **Open sub-question:** do the Glue Kan equations
   (A6) *need* sigmaF-η definitionally, or only up to the boundary `refl`s? In
   cubicaltt, Glue's `unglue ∘ glue` uses Σ-η. **Recommendation:** start without
   η (state it as a provable inner path via `pairF (fstF p) (sndF p)`-pinning);
   add definitional η only if A6 property tests force it, and then scope it to
   the *inner* projection reduction, not outer conversion. Research until A6.

3. **Constancy/non-dependence probe precision.** A5a fires only when the
   freshness probe (`mentionsRefVal`/`kanFreshSentinel`) confirms `B` ignores
   both `i` and the first projection. The probe is occurrence-based and
   conservative-exact (eval.go:1074-1112); a `B` that mentions the projection
   only inside a dead branch would be wrongly judged dependent and stay stuck.
   That is *sound* (stuck, not wrong) and acceptable — same discipline as the
   piF constant-domain rule.

4. **Overlap-agreement (C-OVERLAP) is independent but adjacent.** The hcomp/comp
   sigmaF rules feed a system `u i h`; on a *multi-branch* φ the second
   component's overlap obligation hits the known non-definitional
   proof-irrelevance gap (PARKING-LOT.md:156-166). A5a sidesteps it (single
   ⊤/⊥/neutral face, `refl`-pinned), but A6/Glue will need C-OVERLAP resolved.
   Not a blocker for R-SIGMA itself.

5. **`fstF` in `sndF`'s type and in B's argument.** Decision 3 puts `fstF` in a
   *type*. Type-level `fstF (pairF a b)` must reduce during conversion — it does,
   because the ι-rule fires in `Eval` regardless of position (eval.go:497-499
   fires ι eagerly in `apply`). Confirm no quote-side regression (the pinned
   `fstBeta` test covers it).

## Test/gate plan

- **Listing gate:** `listings/ch24_sigma.rune` elaborates and checks (the v3
  bar is "elaborate and check", not run — sigmaF members are inner-tainted, so
  `EmitProgram` skips them; add the four names to the inner-taint set in
  `internal/session/session.go` near the existing fib/path taint, lines
  ~520-535). Gate it from `harness/listings_test.go` like ch17–ch23.
- **ι-rule unit tests** (`store/sigma_test.go`, mirroring `path_test.go`):
  `fstF (pairF … a b) ~> a`, `sndF (pairF … a b) ~> b`,
  `El (sigmaF A B)` stays **neutral** (no spurious decode — guards Decision 2),
  `pairF`/`sigmaF` stay neutral on non-matching spines, `fstF`/`sndF` on a
  *neutral* (non-`pairF`) pair stay stuck.
- **Structural Kan pins** (`internal/session/structural_test.go`, alongside the
  piF pins): `hcomp` over a non-dependent product reduces to the re-paired form
  (the ch24 `refl`); `transp` over a constant non-dependent `sigmaF` line is the
  componentwise transport; a *dependent* `sigmaF` `hcomp` stays **stuck**
  (negative pin — guards the A5a/A5b boundary, the honest-stuck label).
- **Cubical property tests** (`harness/cubical_props_test.go`): add sigmaF to the
  "every closed fibrant former normalises predictably" sweep; confluence of the
  projection ι-rules with the boundary rules; type-preservation on the new rules.
- **Hash-stability regression:** assert `AddFib` hashes and ch09–ch23 outputs are
  byte-identical before/after adding the group (Decision 4); assert *no*
  hash-format bump (`defFormatVersion` unchanged) — there is no new core
  constructor.
- **Frame Lemma (X1/R-FRAME):** the new ι-rules force scrutinees through the same
  `m.Force` seam (eval.go:497, 1322), so the dependency log captures sigmaF
  unfolds exactly as it does piF; add a cache-key regression that a definition
  using `fstF`/`sndF` records the sigmaF group hashes in its certificate.

## Unblocks (which implement nodes, and what they still need)

- **A5 (sigmaF group + Kan over sigmaF)** — *this is A5*. A5a
  (group + projections + non-dependent/constant Kan) is **ready-to-build**. A5b
  (fully dependent transp/hcomp/comp over sigmaF) is **blocked on R-FILL**
  (`transpFill`/`hfill`), exactly like varying-domain piF — label honest-stuck.
- **A6 (Glue group + Kan-over-Glue)** — unblocked to *state* `Equiv`/`fiber`/
  `isContr`/`isEquiv` as sigmaF-built fibrant types, and to *state* `Glue`
  consuming `Equiv`. Still needs: R-GLUE (the Glue former + glue/unglue + their
  Kan equations), and for the *dependent* Glue fills, R-FILL and C-OVERLAP.
- **A7 (ua-as-Glue)** — indirectly unblocked: once `Equiv` is a real type, `ua`
  can be re-typed `(A B : UF) -> Equiv (El A) (El B) -> pathU A B` (retiring the
  four-component telescope of fib.go:201-221), which A7/R-UA needs. This is a
  *type change to `ua`* — note it would alter the fib group's hash and ch10, so
  schedule it deliberately as part of A7, not A5 (A5 leaves `ua` untouched).
- **C1/R-SUM (outer Sigma)** — *not* a prerequisite (Decision 2). R-SIGMA
  deliberately routes around it; if R-SUM lands, R-SIGMA's `El` decoding can be
  upgraded to native outer Σ with no member-type/hash change (Risk 1).

**Status of this node:** ready-to-build for A5a (the group, computing
projections, neutral `El (sigmaF …)`, and the transpFill-free Kan rules); the dependent Kan
fills remain research, blocked on R-FILL, and are labelled honest-stuck — the
same discipline the piF slice already ships under.

Sources:
[Cubical — Agda docs](https://agda.readthedocs.io/en/latest/language/cubical.html),
[cubicaltt](https://github.com/mortberg/cubicaltt),
[cubicaltt univalence.ctt](https://github.com/mortberg/cubicaltt/blob/master/examples/univalence.ctt),
[Agda.Builtin.Cubical.Glue](https://cj-xu.github.io/agda/ordinals/Agda.Builtin.Cubical.Glue.html),
[Wikipedia: Cubical type theory](https://en.wikipedia.org/wiki/Cubical_type_theory).
