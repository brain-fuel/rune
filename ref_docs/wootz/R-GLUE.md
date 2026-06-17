# R-GLUE — Glue types on a content-addressed substrate

## Problem (what's stuck/absent today, with file:line)

Inner univalence in this kernel is **postulated, not computed**. `ua` is a
permanently-neutral head (`store/fib.go:201-221`, member index 9) and `castU`
computes *through* it as a programming idiom only — `tryFibIota`,
`core/eval.go:818-824`: `castU A B (ua _ _ f _ _ _) x ~> f x`. But:

- **`pathJ` over a ua-path does not compute** (`core/eval.go:801-809`: `FRoleJ`
  fires only when the path is `preflF`). So path induction over a univalence path
  is stuck — the §F frontier the whole roadmap is climbing toward (M2,
  `humble-humming-elephant.md:240-243`).
- **`ua` is a four-component telescope** (`f`, `g`, `s`, `t` as separate `Pi`
  args, `store/fib.go:205-221`), *not* `(A B : UF) -> Equiv (El A) (El B) ->
  pathU A B`. It is a black box: it carries an iso, not a genuine equivalence,
  and nothing connects it to a transport that *computes the right function on
  every interval point*. There is no `pathU`-as-line; `pathU A B : U1` is opaque
  data (`store/fib.go:195-196`).
- **There is no `Glue`.** `Glue` is the single CCHM/ABCFHL construct that makes
  `transp` over a univalence path reduce — it is *the* mechanism by which "an
  equivalence becomes a path that transports correctly." Without it, the inner
  stratum has an interval (phase 1), paths (phase 2), Kan operations (phase 3),
  and `sigmaF` (R-SIGMA / A5) — every brick *except the keystone of
  computational univalence*. The roadmap names it precisely: A6 = "Glue group +
  Kan-over-Glue", A7 = "ua-as-Glue" (`humble-humming-elephant.md:176-177`).

The blocking dependency chain is real and already mapped: `Glue (A:UF) (φ:F)
(T : partial UF on φ) (e : partial Equiv on φ)` cannot be *stated* until `Equiv`
exists, and `Equiv (El A)(El B) = Σ (f : El A -> El B), isEquiv f` needs `sigmaF`
(R-SIGMA, `ref_docs/wootz/R-SIGMA.md:14-17`, `PARKING-LOT.md` "`Equiv` as a
first-class fibrant type … waits on it"). R-SIGMA is **ready-to-build** and
delivers `sigmaF`/`pairF`/`fstF`/`sndF` with computing projections — so `Equiv`,
`fiber`, `isContr`, `isEquiv` can be *stated* the moment A5 lands
(`00-INDEX.md:134-138`). The dispatch site is also ready: `fibFormer`
(`core/eval.go:1177-1196`) already decomposes *any* fibrant former, so a new
`FRoleGlue` plugs into the existing `tryTransp`/`tryHcomp`/`tryComp` switches
(`core/eval.go:1141`, `1223`, `1281`) with no new dispatch machinery.

The **transp-over-Glue equation is the hardest single rule in the whole §F
programme.** It is the formula that, applied to a `Glue` built from an
equivalence, recovers `equivFun e x` at the boundary — i.e. it is what forces
`ua`/`castU` to *agree with applying the equivalence*. This node designs:
(1) the `Glue` builtin group; (2) `Equiv` over `sigmaF`; (3) the glue/unglue
ι-rules with the unglue boundary; (4) the transp/hcomp-over-Glue Kan equations,
honestly split into a ready-to-build slice and an R-FILL/C-OVERLAP-gated
remainder; (5) how A7 then re-derives `ua` from `Glue`.

This stays research-grade where it must (the dependent fills), but the **former,
glue/unglue, the unglue boundary, and the φ=⊤ degeneracies are ready-to-build**.

## Prior art (what the literature/other systems do; cite)

- **CCHM (Cohen–Coquand–Huber–Mörtberg, *Cubical Type Theory: a constructive
  interpretation of the univalence axiom*).** §6 introduces `Glue [φ ↦ (T,f)] A`:
  a type that *extends* `A` by a partial family of types `T` equivalent (via `f`)
  to `A` on `φ`. Boundary: `Glue [1 ↦ (T,f)] A = T`. Intro `glue [φ ↦ t] a :
  Glue …` with `glue [1 ↦ t] a = t`. Elim `unglue : Glue … -> A` with `unglue
  (glue [φ ↦ t] a) = a` and, *on φ*, `unglue b = f b`. §7 derives univalence:
  `ua` is `\i -> Glue [ (i=0) ↦ (A,id-equiv), (i=1) ↦ (B,e) ] B`-shaped, and
  Theorem 9 ("unglue is an equivalence") drives the computation. The hard part is
  **`comp` for `Glue`** (CCHM §6.3): it composes in `A` via `unglue`, then
  *re-glues* using the equivalence's contractible-fiber data (`equivProof`) to
  fix up the `T`-components. Source:
  [CCHM arXiv:1611.02108](https://arxiv.org/pdf/1611.02108),
  [Chalmers PDF](https://www.cse.chalmers.se/~coquand/cubicaltt.pdf).

- **Cubical Agda** (the readable presentation). The exact primitives my surface
  mirrors:
  ```
  primGlue   : (A : Set ℓ) {φ : I} (T : Partial φ (Set ℓ'))
               (e : PartialP φ (λ o → T o ≃ A)) → Set ℓ'
  prim^glue  : {…} → PartialP φ T → A → primGlue A T e
  prim^unglue: {…} → primGlue A T e → A
  ```
  Computation rules (verbatim from the Agda docs):
  ```
  unglue φ (glue t a)              = a
  glue (λ{(φ=i1) → g}) (unglue φ g) = g       -- η for Glue
  unglue i1 {Te} g                 = Te 1=1 .snd .fst g   -- on φ: apply equiv
  glue {φ = i1} t a                = t 1=1
  Glue A {i1} Te                   = Te 1=1 .fst           -- boundary
  ```
  with `_≃_ A B = Σ (A → B) isEquiv`, `isEquiv f = (y:B) → isContr (fiber f y)`,
  `fiber f y = Σ[x∈A] (f x ≡ y)`, `isContr A = Σ[x∈A] (∀ y → x ≡ y)`. Source:
  [Agda Cubical docs](https://agda.readthedocs.io/en/latest/language/cubical.html),
  [agda/cubical Core/Glue.agda](https://github.com/agda/cubical/blob/master/Cubical/Core/Glue.agda).

- **cubicaltt** (`examples/univalence.ctt`) gives three univalence proofs; the
  one used here is §7.2's "`unglue` is an equivalence", because it is the one that
  shows *why* `comp`-over-Glue needs the equivalence's contractibility data, not
  just the forward map. Source:
  [cubicaltt univalence.ctt](https://github.com/mortberg/cubicaltt/blob/master/examples/univalence.ctt).

- **ABCFHL / cooltt (Angiuli–Brunerie–Coquand–Favonia–Harper–Licata, *Syntax and
  Models of Cartesian Cubical Type Theory*).** The *Cartesian* variant decomposes
  `comp` into `coe` (transport `r→r'`) + `hcom`, and gives `Glue`'s `coe` and
  `hcom` separately. cooltt and `redtt` implement exactly this split. This matters
  here because **Rune's substrate is De Morgan (CCHM), not Cartesian** — the
  interval is `imin`/`imax`/`ineg` (`store/interval.go:33`), and faces are
  `ieq0`/`ieq1` (`store/face.go:28`). So I follow the **CCHM** Glue equations, not
  the Cartesian ones, while borrowing ABCFHL's *pedagogical* `coe`/`hcom` split to
  layer the increments. Source: [ABCFHL arXiv:1712.01800].

- **The regularity problem for Glue.** The HoTT mailing-list thread and the CCHM
  follow-ups note `comp`-for-Glue is *the* place regularity breaks (you cannot
  make `comp`-Glue reduce to `comp`-A on φ=⊤ *and* stay regular). Swan's
  non-regular fix is the accepted answer; canonicity survives (Huber). This is
  C-REG, already resolved in `ref_docs/wootz/R-FILL.md` ("ship non-regular CCHM;
  adopt Swan only if X2 canonicity tests fail"). R-GLUE *inherits* that decision.
  Source:
  [HoTT list: regularity in CCHM](https://groups.google.com/g/HomotopyTypeTheory/c/R2hbva-TiGk/m/BnukKUEbBAAJ).

- **The builtin-group pattern in this kernel.** Quotients (v2), the fibrant kit
  (v3), interval/path/face/sys/kan (§F 1–3), and `sigmaF` (R-SIGMA) are each a
  *group of bodiless content-addressed defs with permanently-neutral heads and
  ι-rules in the evaluator*, no hash-format bump (`store/fib.go:1-44`,
  `store/kan.go:30-64`, `R-SIGMA.md:83-88`). `Glue` follows this template exactly.

## Chosen approach for THIS substrate (concrete; respects containment)

### Decision 0 — `Glue` is a builtin group on its own hash space; the outer core never grows.

No new `core.Tm`/`core.Val` constructor, no `defFormatVersion` bump
(`store/face.go:25`, the standing discipline). `Glue`/`glue`/`unglue` are
bodiless content-addressed defs with permanently-neutral heads, registered after
`sigmaF` (it consumes `Equiv`, which is `sigmaF`-built). `unglue` computes by
ι-rule in the evaluator; `glue` is a canonical neutral intro (like `pabs`/`qin`/
`pairF`). This is the seventh §F builtin group (interval, path, face, sys, kan,
sigma, glue) and keeps ch09–ch24 byte-identical.

### Decision 1 — `Equiv` is a derived inner type over `sigmaF`, NOT a builtin.

`Glue`'s `e` argument is a *partial `Equiv`*. `Equiv` must be a genuine fibrant
type (`UF`) so it can be the codomain of a partial element `holds φ -> UF`. Built
entirely from R-SIGMA's `sigmaF`/`pairF`/`fstF`/`sndF` + the existing `pathF`/
`piF`/`fib` — **no new builtin**:

```
fiber   (A B : UF) (f : El A -> El B) (y : El B) : UF
        := sigmaF A (λ x. pathF B (f x) y)
isContr (A : UF) : UF
        := sigmaF A (λ c. piF A (λ x. pathF A c x))
isEquiv (A B : UF) (f : El A -> El B) : UF
        := piF B (λ y. isContr (fiber A B f y))
Equiv   (A B : UF) : UF
        := sigmaF (piF A (λ _. B)) (λ f. isEquiv A B f)
```

These ship as **ordinary Rune library definitions** in the listing (ch25), not as
kernel builtins — Savage/Thompson: `Equiv` is *teachable code a learner can
read*, and the kernel stays minimal. The evaluator never special-cases `Equiv`;
it only ever sees `sigmaF` heads and `fstF`/`sndF` projections, which already
compute (R-SIGMA Decision 1).

Two accessors (also library defs) name the projections the Kan rules conceptually
use (the *rules* destructure `sigmaF` directly, but the surface reads better):

```
equivFun  (A B : UF) (e : El (Equiv A B)) : El A -> El B := fstF … e
equivProof(A B : UF) (e : El (Equiv A B))
          : El (isEquiv A B (equivFun A B e))            := sndF … e
```

### Decision 2 — the `Glue` former and its boundary.

```
Glue  : (A : UF) -> (φ : F)
        -> (T : holds φ -> UF)
        -> (e : (h : holds φ) -> El (Equiv (T h) A))
        -> UF
```

`T` is a partial fibrant code (a `holds φ -> UF`, exactly the partial-element
shape of `store/sys.go:8`: "a partial element of A on φ is `holds φ -> A`"). `e`
is the partial equivalence. The **boundary rule** is the heart of the former:

```
El (Glue A φ T e)   ~>   El (T h)     when φ ≡ ⊤   (h := htop)
```

i.e. `Glue A ⊤ T e = T htop` at the type level. This is an `El`-ι-rule in
`tryFibIota` (`core/eval.go:771-800`), in the `FRoleEl` case alongside `fib`/
`piF`/`sigmaF`: when `El`'s argument is a saturated `Glue` head and its φ forces
to `⊤` (via the existing `faceConst`, `core/eval.go:977`), `El (Glue A ⊤ T e)`
decodes to `El (T htop)`. On a **proper or neutral φ**, `El (Glue …)` stays
neutral — `Glue` is then a genuinely new fibrant type, abstract until Kan acts on
it. (CCHM `Glue [1 ↦ (T,f)] A = T`; the φ=⊤ rule.)

There is no φ=⊥ boundary rule for `Glue` itself (when φ=⊥ the partial data is
empty and `Glue A ⊥ T e` is just a fresh code equivalent to `A` — it does *not*
reduce to `A` definitionally in CCHM; that equivalence is a path, provided by the
Kan structure, not a reduction). Honest-stuck on φ=⊥.

### Decision 3 — `glue`/`unglue` and the unglue boundary.

```
glue   : (A : UF) -> (φ : F) -> (T : holds φ -> UF)
         -> (e : (h : holds φ) -> El (Equiv (T h) A))
         -> (t : (h : holds φ) -> El (T h))    -- partial T-component, agreeing on φ
         -> (a : El A)                          -- the A-component
         -> El (Glue A φ T e)
unglue : (A : UF) -> (φ : F) -> (T : holds φ -> UF)
         -> (e : (h : holds φ) -> El (Equiv (T h) A))
         -> El (Glue A φ T e) -> El A
```

`glue` is a **canonical neutral intro** (permanently-neutral head, in
`rigidHead`). `unglue` **computes** by ι-rule (`tryGlueIota`, new, dispatched
from `tryRules` like `papp`/`fstF`). Two rules, exactly CCHM/Agda:

```
unglue A φ T e (glue A' φ' T' e' t a)  ~>  a              -- β
unglue A φ T e g                       ~>  equivFun (T htop) A (e htop) g
                                                          -- the BOUNDARY, when φ ≡ ⊤
```

The **unglue boundary** is the load-bearing equation. Read it (Savage): *on the
face where `Glue` collapses to `T`, `unglue` is not a projection — it is the
forward map of the equivalence applied to the `T`-element.* When φ=⊤, `g : El
(Glue A ⊤ T e) ≡ El (T htop)` (by Decision 2's boundary), so `g` *is* a
`T`-element, and `unglue g = equivFun (e htop) g : El A`. This boundary is what
glues the type-level `Glue A ⊤ T = T` to the term-level `unglue = f`: it makes
the two ends of a univalence path land on `id` and `f` respectively. The φ=⊤
case fires via `faceConst`; the β case fires on a `glue`-headed scrutinee.

There is also **η for Glue** (`glue [φ ↦ unglue g] (unglue g) ≡ g`), which CCHM
needs for `comp`-Glue to round-trip. As with `sigmaF`-η (R-SIGMA Risk 2), this
engine does **not** ship definitional Glue-η in the outer conversion checker
(Thompson: no `core/conv.go` edit). It is provided as a *provable inner path* and
revisited only if A6 property tests force a scoped inner reduction. Labelled
research-until-A6.

### Decision 4 — the transp-over-Glue equation (the hard one).

This is the formula that makes computational univalence real. Following CCHM §6.3
(De Morgan), for a **Glue line** `λi. Glue (A i) (φ i) (T i) (e i)`:

```
transp (λi. Glue (A i) (φ i) (T i) (e i)) g0
  ~>  glue (A i1) (φ i1) (T i1) (e i1)
           (λh. t1 h)        -- the T-component at i1 (only defined where φ i1 holds)
           a1                -- the A-component at i1
```
where, abbreviating `ψ := ∀i. φ i` (the cofibration "φ holds on the whole line"):

```
a0  := unglue (A i0) (φ i0) (T i0) (e i0) g0          -- pull g0 down to A i0
ã   := transpFill (λi. A i) a0                        -- the fill of a0 in the A-line
a1' := transp (λi. A i) a0                            -- a0 transported to A i1 in A
δ   := the cofibration (φ i1) ∨ ψ
t1, a1 := the equiv-fixup: on (φ i1) use the contractible fiber of
          (equivFun (e i1)) over a1' to produce a T i1 component t1 whose
          image equals a1' on the nose, and set
          a1 := hcomp (A i1) δ (walls from equivProof + ã at i1) a1'
```

The precise CCHM construction (the part that *needs the equivalence's
contractibility*, not just its forward map): on `φ i1`, take
`(equivProof (e i1) a1')` — the centre of contraction of `fiber (equivFun (e i1))
a1'`, a pair `(t1 , p1)` where `t1 : El (T i1)` and `p1 : equivFun (e i1) t1 ≡
a1'`. Then `a1` is the homogeneous composite in `A i1` that uses `p1` (and `ã` on
ψ) to reconcile `equivFun (e i1) t1` with the transported `a1'`. The result
`glue (…) (λh. t1) a1` lands in `Glue (A i1)(φ i1)(T i1)(e i1)`, and **on φ=⊤ it
reduces to `transp (λi. T i) g0`** (the regularity-modulo boundary that
canonicity tests will pin).

**This formula consumes, in order: `transp`/`transpFill` over the A-line
(R-FILL), `hcomp` on a proper face δ in `A i1` (R-BOX), `fstF`/`sndF` and `pathF`
on the contractible-fiber data (R-SIGMA, ready), and the De Morgan ∀-quantifier
`∀i.φ i` as a cofibration.** It is therefore **NOT ready-to-build in full**: it
is honest-stuck on R-FILL + R-BOX, exactly the same gate as varying-domain `piF`
and dependent `sigmaF`. What R-GLUE delivers *now* is the *structure* of the rule
(the `tryTranspGlue` skeleton in eval.go), wired so that as R-FILL/R-BOX land
their members, the formula completes — and the **two boundary degeneracies that
need neither**:

- φ ≡ ⊤ on the whole line (`ψ = ⊤`): `transp Glue-line g0 ~> transp (λi. T i)
  g0` — pure `transp` over the `T`-line, no fixup, **ready-to-build**.
- the A-line and φ both **constant** in i and φ neutral-but-`e i` an *identity*
  equivalence: collapses to `transp (λi. T i)` — research-adjacent, low value,
  park.

### Decision 5 — hcomp-over-Glue.

CCHM hcomp on `Glue A φ T e` (a *fixed* code, homogeneous) composes the `T`-part
homogeneously and the `A`-part through `unglue`, then re-glues:

```
hcomp (Glue A φ T e) ψ u u0
  ~>  glue A φ T e
          (λh. hcomp (T h) ψ (λj h'. unglue⁻-on-φ (u j h')) ((…) u0))   -- T-part on φ
          (hcomp A ψ (λj h'. unglue A φ T e (u j h')) (unglue A φ T e u0)) -- A-part
```

On φ the `T`-part is a genuine homogeneous `hcomp (T h)`; the `A`-part is
`hcomp A` of the `unglue`d walls/floor; `glue` re-assembles them. This is
**closer to ready** than transp-Glue because it needs no `transpFill` (the type
is fixed) — but the `T`-part on a *proper* φ is a proper-face `hcomp`, which is
**R-BOX** (`hcomp (T h)` on the open box). So:

- ψ ≡ ⊤: `hcomp Glue ⊤ u u0 ~> u i1 htop` (the generic total-system rule,
  already in `tryHcomp`, `core/eval.go:1208-1214`; no Glue special-case needed).
- the re-glue formula with ψ proper: needs R-BOX's `hcomp (T h)` per-former rule
  + C-OVERLAP for multi-branch φ. **Honest-stuck**, structure shipped.

### Decision 6 — comp-over-Glue = transpFill ▸ hcomp (the seam).

`comp` over a Glue *line* is, as everywhere in CCHM, `transp`(Glue) then
`hcomp`(Glue) via the R-FILL seam (`R-FILL.md` "comp = transpFill ▸ hcomp", and
`tryComp` already short-circuits ⊤/⊥, `core/eval.go:1255-1271`). It inherits both
gates. Structure shipped; full rule honest-stuck on R-FILL + R-BOX + C-OVERLAP.

## Interfaces & signatures to add (Go + Rune surface as relevant)

### Rune surface — the Glue builtin group (store/glue.go, new file)

```
Glue   : (A : UF) -> (φ : F) -> (T : holds φ -> UF)
         -> ((h : holds φ) -> El (Equiv (T h) A)) -> UF
glue   : (A : UF) -> (φ : F) -> (T : holds φ -> UF)
         -> (e : (h : holds φ) -> El (Equiv (T h) A))
         -> (t : (h : holds φ) -> El (T h)) -> (a : El A)
         -> El (Glue A φ T e)
unglue : (A : UF) -> (φ : F) -> (T : holds φ -> UF)
         -> (e : (h : holds φ) -> El (Equiv (T h) A))
         -> El (Glue A φ T e) -> El A
```

Three members. **Caveat — `Equiv` in the member types.** `Equiv` is a *library*
def (Decision 1), not a builtin, so it has a content hash like any def. But the
`Glue`/`glue`/`unglue` member *types* reference `Equiv` (`El (Equiv (T h) A)`).
Two options:

1. **Inline `Equiv`'s definition** into the member types as raw `sigmaF`/`piF`/
   `pathF` applications (the member types reference only builtin hashes:
   `sigmaF`/`piF`/`pathF`/`fstF`/`El` from R-SIGMA+fib). Then `Glue`'s hash
   depends only on builtin-group hashes — stable, session-independent, the
   invariant every group keeps (`store/fib.go:50-53`). **Recommended.**
2. Take `Equiv`'s hash as an `AddGlue(equivHash …)` parameter — but `Equiv` is a
   user-space def whose hash is *not* fixed across sessions (it depends on its
   elaborated body), which would make `Glue`'s hash unstable. **Rejected.**

So `AddGlue` takes the builtin group hashes it builds `Equiv` from inline:
`AddGlue(fib [11]core.Hash, sigma [4]core.Hash, iv [6]core.Hash, face [7]core.Hash, sys [5]core.Hash) [3]core.Hash`.
Group tag bytes `{defFormatVersion,'U'}`/`{defFormatVersion,'u'}` (U for
univalence; unused — 'G'/'g' taken by sigma per R-SIGMA Decision-via-`'G'`,
verify and pick fresh letters at build).

### core/eval.go — FibRole extension (no new core constructor, no new interface)

Add to `FibRole` (`core/eval.go:129-154`):

```go
FRoleGlue   // Glue former (closes UF under gluing)
FRoleGlueIn // glue introduction (permanently neutral, like pairF/pabs/qin)
FRoleUnglue // unglue elimination (computes by ι)
```

Reuse the existing `FibInfo` interface (`FibRoleOf`/`FibHash`,
`core/eval.go:158-163`) and have `Store.FibRoleOf`/`FibHash` fall through to the
new `*glueEntry` (the same multi-group fall-through R-SIGMA Decision 4 / its
"caveat" already establishes for `sigmaF`). `fibFormer` (`core/eval.go:1177`)
then recognises `FRoleGlue` with no change — its dispatch is role-generic. Add
`Glue`/`glue`/`unglue` to `rigidHead` coverage (already covered transitively by
`m.Fib.FibRoleOf(h) != FRoleNone`, `core/eval.go:444-446`, once they live in the
fib-role space).

### core/eval.go — the ι-rules

- **`El (Glue A ⊤ T e) ~> El (T htop)`**: extend the `FRoleEl` decode switch in
  `tryFibIota` (`core/eval.go:785-799`) with `case FRoleGlue:` — force φ
  (`cargs[1]`) via `faceConst`; if ⊤, return `El (T htop)` by applying the `El`
  ref to `(T htop)` (`m.Apply(m.refVal(elHash), m.Apply(cargs[2], htop))`).
- **`unglue` (new `tryGlueIota`)**: dispatch `FRoleUnglue` from `tryRules`
  (`core/eval.go:673-678`, the `FRoleEl||FRoleJ||FRoleCastU` list). β: scrutinee
  forces to a `glue`-headed spine → return its `a` (last arg). Boundary: φ forces
  to ⊤ → return `equivFun (e htop) g` = `m.Apply(fstF-applied(e htop), g)` (build
  via `FibHash(FRoleFstF)`).
- **`tryTranspGlue` / `tryHcompGlue` / `tryCompGlue`**: new `case FRoleGlue:` arms
  in the `fibFormer`-role switches of `tryTransp`/`tryHcomp`/`tryComp`
  (`core/eval.go:1141`, after the `FRolePiF` case). Each ships the ⊤-degeneracy
  *now* and a guarded skeleton for the full formula that returns `nil,false`
  (honest-stuck) until `KanHash(KRoleTransp)`-built `transpFill` (R-FILL) and the
  R-BOX `hcomp` members are available. Gate the full arms behind a
  `transpFillAvailable()` predicate so they light up automatically when R-FILL
  lands — no re-plumbing.

### store/glue.go — the group (parallel to store/sigma.go / store/path.go)

`glueNames = [3]string{"Glue","glue","unglue"}`, `GlueRoleOf`/`GlueHashes`,
inline-`Equiv` member types (Option 1). Registered in
`internal/session/session.go` after `AddSigma` (which is after `AddFib`), before
or after `AddKan` — order only matters for *which group references which hashes*;
Glue references fib+sigma+face+sys+iv, Kan references fib+iv+face+sys, so either
order works; register Glue last to keep the Kan hashes fixed.

### internal/session/session.go — taint + type-former erasure

`Glue` is a type former → add `glueHashes[0]` to `typeRefs` (erases to unit,
`session.go:518-522` pattern). `glue`/`unglue` are inner value members → covered
by `innerTaint` (`session.go:604-616`); a def using them checks but does not
deploy, the v3 line. `Equiv`/`fiber`/`isContr`/`isEquiv` library defs are
`sigmaF`/`piF`-built and therefore transitively inner-tainted automatically.

## Worked micro-example (the teachable artifact)

The ready-to-build deliverable, in the ch23/ch24 `refl`-pinning style: the
**unglue boundary** and the **φ=⊤ type boundary** — the two equations that make
`Glue` collapse to its equivalence on a total face. (The transp-over-Glue *fixup*
is the honest-stuck remainder; this listing certifies the parts that compute.)

```
-- ch25_glue.rune  (§F phase 4 — Glue: the heart of computational univalence)

-- Equiv as a library type over sigmaF (R-SIGMA), readable, not a builtin:
fiber : (A : UF) -> (B : UF) -> (f : El A -> El B) -> (y : El B) -> UF is
  fn (A : UF) (B : UF) (f : El A -> El B) (y : El B) is
    sigmaF A (fn (x : El A) is pathF B (f x) y end)
  end
end
-- isContr, isEquiv, Equiv, equivFun, equivProof: as in Decision 1.

-- (1) TYPE boundary: Glue on a TOTAL face IS its T-component (definitional).
glueTopType : (A : UF) -> (T : holds ftop -> UF)
   -> (e : (h : holds ftop) -> El (Equiv (T h) A))
   -> Eq U1 (El (Glue A ftop T e)) (El (T htop)) is
  fn (A : UF) (T : holds ftop -> UF)
     (e : (h : holds ftop) -> El (Equiv (T h) A)) is
    refl (El (T htop))      -- El (Glue A ⊤ T e) ~> El (T htop) fires; refl certifies it
  end
end

-- (2) unglue BETA: unglue of a glued pair is its A-component (definitional).
unglueBeta : (A : UF) -> (phi : F) -> (T : holds phi -> UF)
   -> (e : (h : holds phi) -> El (Equiv (T h) A))
   -> (t : (h : holds phi) -> El (T h)) -> (a : El A)
   -> Eq (El A) (unglue A phi T e (glue A phi T e t a)) a is
  fn (A : UF) (phi : F) (T : holds phi -> UF)
     (e : (h : holds phi) -> El (Equiv (T h) A))
     (t : (h : holds phi) -> El (T h)) (a : El A) is
    refl a                  -- unglue (glue … t a) ~> a fires; refl certifies it
  end
end

-- (3) unglue BOUNDARY: on a total face, unglue is the equivalence's forward map.
unglueTop : (A : UF) -> (T : holds ftop -> UF)
   -> (e : (h : holds ftop) -> El (Equiv (T h) A))
   -> (g : El (Glue A ftop T e))
   -> Eq (El A) (unglue A ftop T e g) (equivFun (T htop) A (e htop) g) is
  fn (A : UF) (T : holds ftop -> UF)
     (e : (h : holds ftop) -> El (Equiv (T h) A))
     (g : El (Glue A ftop T e)) is
    refl (equivFun (T htop) A (e htop) g)
    -- unglue A ⊤ T e g ~> equivFun (e htop) g fires; the φ=⊤ boundary, certified.
    -- NB g : El (Glue A ⊤ T e) ≡ El (T htop) by (1), so it typechecks as a T-element.
  end
end
```

The lesson (Savage): *"`Glue` welds a partial type `T` onto a base `A` along an
equivalence `e`. On the face where the weld is total, the glued type IS `T`, and
`unglue` is not a projection — it is `e`'s forward map. Off the face, `Glue` is a
new type, and transporting along it runs `e` exactly where it must."* The
transp-over-Glue fixup — "run the equivalence's *inverse* (its contractible
fiber) to recover the T-component" — is the sentence that needs R-FILL; named,
not yet computed, the same honesty as varying-domain `piF`.

## Risks / open sub-questions

1. **The transp-over-Glue fixup is the deepest rule in §F and is gated on
   R-FILL + R-BOX (both still open).** This is the honest core caveat: R-GLUE
   delivers the former, glue/unglue, both boundaries, and the rule *skeletons*,
   but the formula that *recovers the T-component via contractible fibers* cannot
   complete until `transpFill` (R-FILL, the keystone, not yet delivered) and
   proper-face `hcomp` (R-BOX, needs-more-research) land. Label: **former + glue
   + unglue + boundaries = ready-to-build; the fixup = research, blocked on
   R-FILL/R-BOX.** This is unavoidable — it is the same gate the whole A-track
   inherits from the missing keystone (`00-INDEX.md:152-157`).

2. **C-OVERLAP (overlap-agreement) bites harder here than anywhere.** `glue`'s
   `t` and `a` must *agree on φ* (`unglue (glue t a) = a` requires `equivFun (e h)
   (t h) = a` on φ), and the transp/hcomp fixups feed *multi-branch* systems
   where overlapping faces must agree. This kernel's proof-irrelevance covers
   Eq/refl/cast proofs, **not** arbitrary `holds φ` inhabitants in conversion
   (PARKING-LOT.md, confirmed `R-SIGMA.md:337-342`, CLAUDE.md §F-structural
   FINDING). So a `glue` whose agreement obligation is discharged by *distinct*
   `holds` proofs will not convert. **A6 cannot ship multi-branch Glue Kan
   without C-OVERLAP resolved** (explicit-Eq-evidence threading, the recommended
   route). The boundaries in the worked example use single ⊤ faces and sidestep
   it; the full Kan rules do not.

3. **Glue-η and sigmaF-η.** `comp`-Glue's round-trip (`glue (unglue g) = g`) uses
   Glue-η, and the `Equiv`/`fiber` manipulation uses sigmaF-η. Neither is
   definitional in this engine (Thompson: no `core/conv.go` edit). Open: do the
   *shipped* (boundary/⊤) rules need either η? **They do not** — β and the ⊤
   boundary are first-order. The *fixup* may; revisit as scoped inner reductions
   only if A6 property tests force it (mirrors R-SIGMA Risk 2). Research-until-A6.

4. **`Equiv` as a library def vs. the member-type hash.** Decision-Interface
   Option 1 (inline `Equiv` into member types) keeps `Glue`'s hash stable but
   means the *kernel* encodes `Equiv`'s shape in three places (the three member
   types). If R-SUM later gives a native outer `Σ` and R-SIGMA's `El`-decoding
   changes (R-SIGMA Risk 1), the *member types* of Glue are unaffected (they use
   `sigmaF`/`piF` codes, not the `El`-decoding), so no hash churn. Confirmed safe.

5. **`∀i. φ i` (the "φ holds on the whole line" cofibration) has no member.** The
   transp-Glue formula quantifies a face over the interval. CCHM/cubicaltt have a
   `forall` cofibration former. Rune's face lattice (`store/face.go`) has
   `ieq0`/`ieq1`/`fand`/`for`/`ftop`/`fbot` but **no `∀i`**. Open sub-question:
   does the shipped slice need it? The ⊤-degeneracy does not (φ already total).
   The full fixup does. **If** the full rule is built, `∀i.φ i` is added as a
   face-group member (an eighth face member, or a derived helper) — flag it for
   R-BOX/R-FILL coordination since they hit the same need. Research.

6. **Regularity (C-REG) for Glue.** `comp`-Glue is *the* canonical regularity
   breaker. R-FILL already resolved C-REG ("ship non-regular CCHM, Swan only if
   X2 fails", `R-FILL.md` C-REG section). R-GLUE inherits it; the X2 canonicity
   suite must include closed terms built through transp-over-Glue once the fixup
   lands.

## Test/gate plan

- **Listing gate:** `listings/ch25_glue.rune` (the worked example) elaborates and
  checks; gated from `harness/listings_test.go` like ch17–ch24. The v3 bar is
  "elaborate and check" — Glue value members are inner-tainted, `EmitProgram`
  skips them (`session.go:610-613`).
- **ι-rule unit tests** (`store/glue_test.go`, mirroring `path_test.go`/the
  proposed `sigma_test.go`): `unglue … (glue … t a) ~> a` (β);
  `El (Glue A ⊤ T e) ~> El (T htop)` (type boundary); `unglue A ⊤ T e g ~>
  equivFun (e htop) g` (unglue boundary); `Glue`/`glue` stay neutral on a proper
  φ (negative pins).
- **Structural Kan pins** (`internal/session/structural_test.go`, alongside the
  piF/sigmaF pins): `transp` over a *fully-⊤* Glue line reduces to `transp` over
  the T-line (the ready degeneracy); `hcomp Glue ⊤ u u0 ~> u i1 htop` (generic
  total rule); a **proper-face transp-over-Glue stays stuck** (negative pin —
  guards the ready/honest-stuck boundary, the R-FILL gate).
- **Equiv-library check:** `fiber`/`isContr`/`isEquiv`/`Equiv` elaborate and
  check as `sigmaF`-built fibrant codes (depends on A5/R-SIGMA landing first).
- **Cubical property tests** (`harness/cubical_props_test.go`): add `Glue` to the
  closed-former normalisation sweep; confluence of the unglue β/boundary rules
  with the `El` decode; type-preservation on the new ι-rules.
- **Hash-stability regression:** ch09–ch24 outputs and all prior group hashes
  byte-identical before/after adding the Glue group; assert no `defFormatVersion`
  bump (no new core constructor).
- **Frame Lemma (X1/R-FRAME):** the new ι-rules force scrutinees through the same
  `m.Force` seam; the dependency log captures Glue/Equiv unfolds. Per
  `r-frame.md` and the `00-INDEX.md:178-181` open question, **the transp-over-Glue
  fixup must be checked for branching on irrelevant proof content** (it consults
  `equivProof`, a contractibility *proof*) — until that is cleared, the fixup rule
  ships behind `markImprecise`, the self-retiring cache-imprecision fallback. The
  shipped boundary/β rules do not branch on proof content and are cache-exact.
- **Canonicity (M1/M2 gate, post-fixup):** closed inner terms built through
  transp-over-Glue normalise to a `glue`/constructor form (the C-REG/Swan check).

## Unblocks (which implement nodes, and what they still need)

- **A6 (Glue group + Kan-over-Glue)** — *this is A6.* The **group + former +
  glue/unglue + both boundaries + the ⊤-degeneracies** are **ready-to-build**
  *once A5/R-SIGMA lands* (it needs `sigmaF` to state `Equiv`). The **full
  transp/hcomp/comp-over-Glue fixups** are **blocked on R-FILL (`transpFill`,
  keystone, not delivered) + R-BOX (proper-face `hcomp`, needs-more-research) +
  C-OVERLAP (multi-branch agreement)** — labelled honest-stuck, with the rule
  skeletons shipped so they complete automatically as those land.
- **A7 (ua-as-Glue; J/castU over ua compute)** — unblocked to *re-derive* `ua`:
  with `Glue` in hand, `ua A B e := λi. Glue B ((ieq0 i) ∨ (ieq1 i)) [T,e-data]`
  (the standard CCHM ua-from-Glue), retiring the postulated four-component head
  (`store/fib.go:201-221`). **Caveat (from R-SIGMA Decision 7 / `00-INDEX.md:159`):
  re-typing `ua` over `Equiv` and re-deriving it from `Glue` mutates the fib
  group's hash and ch10** — schedule that hash event deliberately under A7, not
  A6 (A6 leaves `ua`/`castU` untouched and only *adds* the Glue group). A7 still
  needs: the transp-over-Glue fixup *computing* (so `castU`/`pathJ` over a
  ua-path reduce) — i.e. A7's compute-half inherits A6's R-FILL/R-BOX gate.
- **A9 (inner HIT kit)** — indirectly: HITs reuse `hcomp` on their formers; the
  Glue `hcomp` pattern (compose-then-reglue) is the template R-HIT's
  constructor-hcomp rules follow. Not a hard dep.
- **X1/R-FRAME** — R-GLUE surfaces the *concrete* instance of the open
  cache-exactness question (does a Kan rule branch on irrelevant proof?): the
  transp-Glue fixup consults `equivProof`. R-FRAME must classify it (proof
  content is *erased*/irrelevant, so the branch is on the *fiber point*, not the
  proof — likely cache-exact, but must be proven before the fixup lands exact;
  `markImprecise` until then).

**Status of this node:** **ready-to-build** for the Glue former, glue/unglue,
the type boundary (`Glue A ⊤ T = T`), the unglue boundary (`unglue = equivFun e`
on φ), and the ⊤-system Kan degeneracies — *contingent on A5/R-SIGMA landing
first* to provide `Equiv`. The transp/hcomp/comp-over-Glue **fixups remain
research, blocked on R-FILL + R-BOX + C-OVERLAP**, labelled honest-stuck with
shipped skeletons — the same discipline the varying-domain `piF` and dependent
`sigmaF` slices already ship under. This node is the keystone of computational
univalence: it is the construct that turns an equivalence into a transporting
path, and the design here makes its *contained, teachable* half buildable now
while naming the deep half precisely.

Sources:
[CCHM arXiv:1611.02108](https://arxiv.org/pdf/1611.02108),
[CCHM Chalmers PDF](https://www.cse.chalmers.se/~coquand/cubicaltt.pdf),
[Agda Cubical docs](https://agda.readthedocs.io/en/latest/language/cubical.html),
[agda/cubical Core/Glue.agda](https://github.com/agda/cubical/blob/master/Cubical/Core/Glue.agda),
[cubicaltt univalence.ctt](https://github.com/mortberg/cubicaltt/blob/master/examples/univalence.ctt),
[HoTT list: regularity in CCHM](https://groups.google.com/g/HomotopyTypeTheory/c/R2hbva-TiGk/m/BnukKUEbBAAJ).
