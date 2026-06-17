# R-FILL — transpFill (transport i→j) on glued NbE

Status: **ready-to-build for the generalized `transp` + `transpFill` helper + the
`piF` varying-domain rule (A1) + the `comp = transpFill ▸ hcomp` seam; research-gated
on C-REG (regularity) and on the proper-face inner `hcomp` it bottoms out in
(R-BOX/C-OVERLAP).** This is the **keystone**: A1→A3→A4 and A2 all hang off it.
It is the heterogeneous twin of **R-BOX** (the homogeneous `hcomp` keystone); read
them together — `comp` is `transpFill` (this node) *then* `hcomp` (R-BOX), and R-BOX
already specified the per-former `hcomp` table this node's `comp` reduces *into*.

---

## Problem (what's stuck/absent today, with file:line)

Today the engine has **only the φ = ⊥, 0→1 special case of transport**, plus a
single structural push. Concretely, in `core/eval.go`:

- `tryTransp` (`core/eval.go:1122`) implements `transp : (A : I -> UF) -> El (A i0)
  -> El (A i1)` — transport **from i0 to i1 only**, with **no `r`/`φ` argument** at
  all. There is no way to express "transport from `i` to `j`". The two rules that
  fire are:
  1. **regularity** (`core/eval.go:1126`): probe the type-line `A` at
     `kanFreshSentinel` (`core/eval.go:1069`); if `mentionsRefVal`
     (`core/eval.go:1079`) says the sentinel is absent, the line is constant and
     `transp A a0 ~> a0`.
  2. the **constant-domain `piF` push** (`core/eval.go:1141`):
     `transp (λi. piF D (Fam i)) f ~> λx. transp (λi. Fam i x) (f x)`, fired only
     when the probed domain `cargs[0]` does **not** mention the sentinel
     (`core/eval.go:1149`) — i.e. **D is constant in i**.
- The comment at `core/eval.go:1147` states the gap exactly: *"A varying domain
  needs the argument conjugation (transpFill) and stays stuck — the labelled deep
  remainder."*
- `tryComp` (`core/eval.go:1251`) ships only the ⊤ endpoint
  (`core/eval.go:1257`), the ⊥-on-constant-line degenerate case
  (`core/eval.go:1264`), and the same constant-domain `piF` push
  (`core/eval.go:1281`). There is **no `comp = transp-fill ▸ hcomp`** general
  derivation; a proper-face `comp`, or a `comp` over any non-`piF` line, is stuck.
- `TestTranspVaryingStaysStuck` (`internal/session/structural_test.go:37`) pins
  the current conservative boundary: transport over a varying `pathF` line stays
  stuck with a `transp` head.

Why this is the keystone. **Every CCHM structural fill needs `transpFill`:**
- **A1** (varying-domain `piF` transp) needs to conjugate the argument by a
  *backward* transport `transpFill` from `j` back to `i0` — the exact thing the
  `core/eval.go:1147` comment defers.
- **A3** (`transp`/`comp` over `pathF`) is base composition: `comp` over a path
  line bottoms out in `hcomp (A i1)` whose floor is `transp A u0` and whose walls
  are `transpFill`-conjugated (R-BOX §"Where comp reduces into R-BOX",
  `ref_docs/wootz/R-BOX.md:228`).
- **A2** / **A5** (`sigmaF` elementwise fill) needs `transpFill` for the second
  component's `comp (λj. B (a* j)) …` (R-BOX `R-BOX.md:193`).

So `transpFill` is the one missing primitive that unblocks the entire heterogeneous
half of the cubical interior.

---

## Prior art (what the literature/other systems do; cite)

The substrate already committed to **CCHM** (De Morgan interval with
`ineg`/`imin`/`imax`, `ref_docs/rune-cubical-phase1.md:120`), so the formulas are
CCHM's, not Cartesian (cooltt/ABCFHL `coe`).

- **CCHM** (Cohen–Coquand–Huber–Mörtberg, *Cubical Type Theory: a constructive
  interpretation of the univalence axiom*). The primitive is **generalized
  transport** `transp A φ a0` taking a cofibration `φ : F` on which `A` is
  *definitionally constant* (the "where transport is the identity" annotation).
  Plain transport is `φ = ⊥`. `comp` is **derived**:
  `comp A φ u u0 = hcomp (A i1) φ (λj h. transpFill (λi. A i) j (u j h)) (transp A ⊥ u0)`
  — i.e. `comp` = fill the walls forward with `transpFill`, transport the floor,
  then `hcomp`. (https://arxiv.org/abs/1611.02108)
- **Cubical Agda** — `transp : (A : I → Set ℓ) (φ : I) (a : A i0) → A i1`, "Agda
  makes sure A is constant on φ; with φ = i1 this is the identity". `transp`
  *computes by cases on the type former* (Σ elementwise, Π under the binder, Path
  pointwise). This is the per-former recursion R-FILL implements.
  (https://agda.readthedocs.io/en/latest/language/cubical.html)
- **The transpFill formula** (Cubical Agda stdlib `Cubical.Foundations.Transport`,
  and the Agda cubical docs): the *filler* relating `u0` to its transport is
  ```
  transpFill (φ) (A) (u0) (i)  =  transp (λ j → A (i ∧ j)) (~ i ∨ φ) u0
  ```
  Read: `transpFill … i` transports `u0` from `i0` only **partway**, up to `i`,
  along the **restricted line** `λj. A(i∧j)`; the cofibration `~i ∨ φ` records that
  at `i = i0` (and on φ) this restricted line is constant, so the fill is the
  identity there. At `i = i1` it is the full `transp A φ u0`. This is the *single
  formula* that turns 0→1 transport into "transport up to `i`", and from it
  "transport from `i` to `j`" follows by composing a backward fill with a forward
  fill. (https://agda.github.io/cubical/Cubical.Foundations.Transport.html)
- **C-REG (regularity).** CCHM `comp`/`transp` is **non-regular**: transport along
  a *reflexive* (constant) line is only *propositionally* the identity, not
  definitionally, once you go through the per-former recursion (Cubical Agda docs
  note this shortcoming explicitly). **Swan**'s fix (algebraic weak factorization /
  the regularity-restoring `comp`) makes it definitional but complicates the model.
  The canonicity literature (Huber, *Canonicity for Cubical Type Theory*,
  arXiv:1607.04156; Coquand–Huber–Sattler, *Homotopy Canonicity*, arXiv:1902.06572)
  proves canonicity for the **non-regular** CCHM theory: *every closed natural
  number is judgmentally a numeral* even without regularity. **This is the decisive
  fact for C-REG** (see "C-REG resolution" below).

---

## Chosen approach for THIS substrate (concrete; respects containment)

**Thompson:** no new core constructor, no hash-format bump, no new outer-core
member. R-FILL is (a) **one new builtin-group member** `transp` *re-typed* to carry
the cofibration argument — but to keep ch09–ch23 hashes fixed we **do not re-type
the existing `transp`**; instead R-FILL adds the generalized form as a **Go-internal
filling helper** driven from `tryTransp`, plus new ι-rules. The keystone insight is
that **`transpFill` need not be a surface builtin at all** — it is a *derived
neutral-builder* the structural rules call. Below, the two viable shapes, and the
recommendation.

### The representation problem: "transport from i to j" with no ambient de Bruijn level

The hard substrate question. CCHM `transpFill … i` is parameterized by an interval
*variable* `i`; classically the engine threads a de Bruijn **level** for the
filling dimension. **This engine deliberately has no such level** — `tryTransp`
detects i-dependence by *probing at `kanFreshSentinel`* and *occurrence-scanning*
(`mentionsRefVal`, `core/eval.go:1079`), never by a level counter
(`ref_docs/rune-cubical-phase1.md:152`). R-FILL must stay inside that discipline.

The resolution: **the filling dimension is a value-level interval argument, not a
metalevel level.** `transpFill` is a *function of an interval point* — its `i` is
an ordinary `Val` of type `I`, supplied by whatever rule calls it. So we represent
"transport from `r` to `s`" entirely with the **existing interval algebra** and the
**generalized `transp`** (`transp` with a φ cofibration), never with a fresh level:

```
fill from r to s  along A  of  u0      ≡    transpᵍ (λi. A (imax r (imin i s')))  …
```
but more usefully, the two directed primitives we actually need are:

- **forward fill** `transpFillF A φ u0 s` = "transport `u0` from `i0` up to `s`":
  ```
  transpFillF A φ u0 s  :=  transpᵍ (λj. A (imin s j)) (for (ieq0 s) φ) u0
  ```
  (the CCHM `transpFill` with `s` the explicit interval point; `imin s j` is the
  restricted line `A(s∧j)`, and `for (ieq0 s) φ` is the De Morgan `~s ∨ φ` —
  built from `ieq0`/`for`, the existing face members).
- **backward fill** `transpFillB A φ u1 r` = "transport `u1` from `i1` down to `r`":
  ```
  transpFillB A φ u1 r  :=  transpᵍ (λj. A (imax r (ineg j))) (for (ieq1 r) φ) u1
  ```
  (the `ineg`-reversed line, used by A1 to pull the argument back to `i0`).

Here `transpᵍ` is the **generalized transport** `transpᵍ A φ a0` — transport from
i0 to i1 along `A`, *with a constancy cofibration φ* recording where `A` is already
constant (so the fill is the identity there). This is the single new Go primitive.

### `transpᵍ` (generalized transport) — the one new Go primitive

`transpᵍ A φ a0` reduces by the **existing constancy probe, generalized to respect
φ**:

1. **φ = ⊤** (`faceConst` says ⊤): `transpᵍ A ⊤ a0 ~> a0` (constant everywhere on
   ⊤; the identity). *New rule, the φ-driven identity.*
2. **A constant in i** (the existing sentinel probe, `core/eval.go:1126`):
   `transpᵍ A φ a0 ~> a0` regardless of φ. *Existing rule.*
3. **structural**, by `fibFormer` on the probed former (the existing
   `core/eval.go:1137` switch, **now with the varying-domain `piF` case**, below).
4. otherwise **stuck** (a neutral carrying `A`, `φ`, `a0`).

The plain `transp A a0` already shipped is **definitionally `transpᵍ A ⊥ a0`** —
so the existing surface member's ι-rule is reimplemented as `transpᵍ` with φ := the
`fbot` value, leaving its **type, hash, and listings untouched** (Thompson:
ch21/ch23 hashes fixed). `transpᵍ` itself is **not a surface member** in the first
increment — it is a Go helper `m.transpG(A, phi, a0 Val) Val` that `tryTransp`
calls with `phi = fbot`, and that the structural rules call recursively with the
φ they compute. (If a later node needs `transpᵍ` at the surface — e.g. to *state*
regularity as a path — it ships then as a Kan-group member; no consumer yet → park.)

### A1: the varying-domain `piF` transp (the headline consumer)

This is the rule the `core/eval.go:1147` comment defers. With `transpFillB` in
hand it is the standard CCHM Π-transport:

```
transpᵍ (λi. piF (Dom i) (Fam i)) φ f
  ~>  λ(x : El (Dom i1)).
        let  x̄ : (j:I) -> El (Dom j)
             x̄ j := transpFillB (λj. Dom j) φ x j          -- pull x from i1 back to j
        in   transpᵍ (λi. Fam i (x̄ i)) φ (f (x̄ i0))
```

Reading (Savage): to transport a function `f : (x:El(Dom i0)) -> …` to the i1 end,
we receive an argument `x` *at the i1 domain* and must feed `f` an argument *at the
i0 domain* — so we **backward-fill** `x` from i1 to i0 (`x̄ i0`), apply `f`, then
**forward-transport** the result along the codomain line `Fam i (x̄ i)` (the
codomain depends on the *fill* of x, not just its endpoints — that conjugation is
exactly what the constant-domain rule could skip). When `Dom` is constant in i,
`x̄ j ~> x` (regularity: the restricted line is constant), and this **collapses to
the existing `core/eval.go:1141` constant-domain rule** — so A1 *subsumes* the
shipped rule rather than replacing it. That collapse is the soundness pin (below).

### A3 / R-BOX seam: `comp = transpFill ▸ hcomp`

`tryComp` (`core/eval.go:1251`) gains the **general derivation** for a varying line,
after the ⊤/⊥ endpoints short-circuit:

```
comp A φ u u0
  ~>  hcomp (A i1) φ
            (λj h. transpFillF (λi. A i) φ' (u j h) j)   -- walls forward-filled to i1
            (transpᵍ (λi. A i) ⊥ u0)                      -- floor transported i0->i1
```
where the inner `hcomp` is **R-BOX's per-former table** (`ref_docs/wootz/R-BOX.md`).
This is the seam R-BOX already named (`R-BOX.md:228`): R-FILL provides the
`transpFill`-built floor and walls; R-BOX provides the `hcomp (A i1)`. The **walls
need forward-fill** because each `u j h : El (A j)` lives at level `j` and must be
lifted to `A i1`. *Increment honesty:* this rule is **ready to build only as far as
the inner `hcomp (A i1)` is** — i.e. the ⊥-face / single-atom slice R-BOX ships now;
proper multi-branch faces stay gated on **C-OVERLAP** (R-BOX risk 1).

### How the connection algebra drives it

The whole construction is `imin`/`imax`/`ineg`/`ieq0`/`ieq1`/`for` over existing
members — **no new interval or face member**:

- `imin s j` restricts a line to `[i0, s]` (forward fill stops at `s`).
- `imax r (ineg j)` restricts to `[r, i1]` reversed (backward fill stops at `r`).
- `for (ieq0 s) φ` = `~s ∨ φ` is the constancy cofibration: at `s = i0`,
  `ieq0 i0 ~> ftop` (`core/eval.go:1010`), so `for ftop φ ~> ftop`
  (`core/eval.go:1051`), so `transpᵍ … ⊤ … ~> identity` — the fill is the identity
  at its start, definitionally, **purely through the existing ι-rules**. This is
  the payoff of building on the De Morgan substrate: the directedness and the
  endpoint-collapse of `transpFill` are *computed by the face/interval algebra the
  engine already has*, not by a bespoke level mechanism.

### C-REG resolution (the node's required decision)

**Recommendation: ship the non-regular CCHM `comp`/`transp`. Do NOT adopt Swan's
fix unless the X2 canonicity property tests fail.** Rationale, grounded in this
substrate:

1. **Canonicity does not need regularity.** Huber (arXiv:1607.04156) and
   Coquand–Huber–Sattler (arXiv:1902.06572) prove canonicity for *non-regular*
   CCHM: every closed inner natural number reduces to a numeral. The roadmap's
   M1 gate (closed cubical terms satisfy canonicity, `humble-humming-elephant.md`
   §Verification) is therefore *attainable without regularity*. C-REG's bracketed
   recommendation in the roadmap (`humble-humming-elephant.md:147`) — "follow CCHM,
   adopt Swan only if canonicity property tests fail" — is **confirmed**.
2. **Regularity is partly *free* here anyway.** This engine's constancy probe
   (`mentionsRefVal`) gives `transpᵍ (const A) φ a0 ~> a0` **definitionally** at the
   *outermost* former — a stronger-than-CCHM regularity for fully-constant lines
   (it is rule 2 above). What it does *not* give is regularity *under* the
   per-former recursion (e.g. `transp (λi. piF (const D) (const Fam)) f` reduces to
   a λ that *re-transports* the constant codomain rather than returning `f`). That
   is exactly CCHM's non-regularity, and it is **harmless for canonicity** (the
   result is still a canonical λ).
3. **Swan's fix would complicate the contained model.** It changes the `comp`
   derivation (and the model's filling structure) globally — a poor trade against
   Thompson's containment when canonicity is already provable without it. Keeping
   `comp` independent of the regularity choice (R-BOX risk 3, `R-BOX.md:351`) means
   *if* X2 ever fails, the Swan fix touches only the `transpᵍ`/`comp` derivation in
   this node, not R-BOX's per-former `hcomp` table.

**Gate that decision:** X2 must include a canonicity property — random *closed*
inner-`nat` terms built through `transp`/`comp` normalize to numerals — and a
regularity-regression property (transport along a syntactically-constant line of
each former converts to the identity *up to the propositional path*, and
definitionally for the fully-constant case). If the canonicity property ever fails,
re-open C-REG and adopt Swan; until then, non-regular.

---

## Interfaces & signatures to add (Go + Rune surface as relevant)

**No new builtin-group member, no new core `Tm`/`Val`, no hash bump.** All
additions are Go-internal helpers on `*Machine` in `core/eval.go`, built from
existing reverse-lookups (`m.Iv.IntervalHash`, `m.Fc.FaceHash`,
`m.Fib.FibHash`, `m.Kn.KanHash`).

```go
// core/eval.go — the generalized transport, the one new primitive. φ is the
// constancy cofibration (a Val of type F); plain transport is φ = fbot.
// Reduces by: φ=⊤ -> a0; A constant -> a0; structural by former; else stuck.
func (m *Machine) transpG(A, phi, a0 Val) Val

// Forward fill: transport u0 from i0 up to the interval point s (a Val:I).
//   transpFillF A φ u0 s := transpG (λj. A (imin s j)) (for (ieq0 s) φ) u0
func (m *Machine) transpFillF(A, phi, u0, s Val) Val

// Backward fill: transport u1 from i1 down to the interval point r (a Val:I).
//   transpFillB A φ u1 r := transpG (λj. A (imax r (ineg j))) (for (ieq1 r) φ) u1
func (m *Machine) transpFillB(A, phi, u1, r Val) Val

// Small interval/face value constructors over the existing members (helpers so
// the rules read like the formulas; each is m.Apply over m.refVal(hash)):
func (m *Machine) vImin(a, b Val) Val   // imin a b
func (m *Machine) vImax(a, b Val) Val   // imax a b
func (m *Machine) vIneg(a Val) Val      // ineg a
func (m *Machine) vIeq0(a Val) Val      // ieq0 a
func (m *Machine) vIeq1(a Val) Val      // ieq1 a
func (m *Machine) vFor(a, b Val) Val    // for a b
func (m *Machine) vFbot() Val           // fbot   (m.Fc.FaceHash(CRoleBot))
```

`tryTransp` (`core/eval.go:1122`) is rewritten to call `m.transpG(args[0], m.vFbot(),
args[1])`; the existing regularity and constant-domain `piF` rules move *into*
`transpG`'s `case FRolePiF` as the **D-constant fast path**, and the **varying-domain
A1 rule** is added as the general `case FRolePiF`. `tryComp` (`core/eval.go:1251`)
gains the `comp = transpFill ▸ hcomp` general branch after its endpoint
short-circuits, calling `m.tryHcomp` (R-BOX) on the constructed box.

**A subtlety the implementer must handle: the stuck `transpᵍ` must quote.** When
`transpG` is stuck it returns a neutral. Since `transpᵍ` is *not* a surface member,
its stuck form must be encoded as the **existing `transp` member applied to a
φ-massaged line** — concretely, fold φ into the line so the stuck residual is a
genuine `transp L a0` neutral (over `store/kan.go`'s member), preserving quote and
the η-pin discipline. The encoding: a stuck `transpᵍ A φ a0` with φ neutral becomes
`transp (λi. A (imax i⟨φ⟩ i)) a0`-shaped — **OR**, cleaner and recommended, **ship
`transpᵍ` as a real Kan-group member after all** (a fourth member of `store/kan.go`,
`transpG : (A : I -> UF) -> (φ : F) -> El (A i0) -> El (A i1)`), so the stuck form is
just its own neutral. *That is a group-member addition, not a core constructor, so
still no hash-format bump* — it changes the Kan group's digest (the kn group is its
own hash space, ch21+ Kan listings re-hash) but **does not touch ch09/ch10/fib/
interval/path/face/sys hashes**. **Recommendation: add `transpG` as the 4th Kan
member.** It is the honest CCHM primitive, makes the stuck form trivial, and lets a
listing *state* regularity. The plain `transp` stays as a derived/legacy member
(`transp A a0 := transpG A fbot a0`) for ch21/ch23 compatibility, or is retired and
those listings updated — implementer's call, but adding `transpG` is the clean path.

### Rune surface

If `transpG` ships as a member, surface gets `transpG : (A : I -> UF) -> (φ : F) ->
El (A i0) -> El (A i1)` (one new name in `store/kan.go`'s `kanNames`, the array
grows 3→4). No new syntax. `transpFill` is **not** surfaced (it is a derived helper);
if a listing needs it, it is *defined in Rune* from `transpG` + `imin`/`for`/`ieq0`,
which is itself a teachable artifact.

---

## Worked micro-example (the teachable artifact)

The smallest honest win that *exercises `transpFill`* (not just the φ-identity) is
**transport over a function type with a genuinely varying domain**, certified to
*collapse to the constant-domain rule when the domain is in fact constant* — the
soundness pin that A1 subsumes the shipped rule.

```
-- chXX (sketch): A1 — varying-domain piF transp fires, and DEGENERATES correctly.

-- (a) When the domain is constant, the new general rule must agree with the old
--     constant-domain rule: transporting the identity function is the identity.
transpConstDomId :
  (D : UF) -> (C : I -> UF) -> (f : El (piF D (fn (_ : El D) is C i0 end))) ->
  (x : El D) ->
  Eq (El (C i1))
     (papp ... )   -- the transported f applied to x, codomain-transported
     (transp (fn (i : I) is C i end) (f x))
is
  fn (D : UF) (C : I -> UF) (f : ...) (x : El D) is refl _ end
end

-- (b) The headline: a varying domain. The argument is BACKWARD-FILLED. We pin
--     that the transported function is DEFINITIONALLY a lambda (the push fired),
--     by an Eq to its explicit pushed form (η-style, mirroring ch23).
transpVaryDomFires :
  (Dom : I -> UF) -> (Fam : (i : I) -> El (Dom i) -> UF) ->
  (f : El (piF (Dom i0) (Fam i0))) ->
  Eq (El (piF (Dom i1) (Fam i1)))
     (transp (fn (i : I) is piF (Dom i) (Fam i) end) f)
     (pabsLam ... the explicit  λx. transp (λi. Fam i (x̄ i)) (f (x̄ i0)) ... )
is
  fn (...) is refl _ end
end
```

Reading: (a) checks the **regularity collapse** — when `Dom` is constant, `x̄ j`
reduces to `x` (the backward fill of a constant line is the identity, via
`for (ieq1 r) φ`→⊤ when the line is constant), so the general rule's output
converts to the old rule's. (b) checks the **push fires on a varying domain** — a
stuck top-level `transp` η-differs from a λ (the ch23 pin technique,
`rune-cubical-phase1.md:223`), so `refl` only checks if the rule reduced `transp …`
to the explicit lambda. Together they demonstrate `transpFill` *both* doing real
work *and* degenerating soundly — the teachable "transport pulls the argument back
before applying" story.

A second listing (Savage, teach the edge): `transp` over a varying `pathF` line
**still stays stuck** at the `transpᵍ` level (A3 handles paths, not A1) — exactly
`TestTranspVaryingStaysStuck` (`structural_test.go:37`), now re-pinned to show A1's
`piF` rule does *not* over-fire on a `pathF` line.

---

## Risks / open sub-questions

1. **`transpᵍ` stuck-form / quote (dominant engineering risk).** A stuck
   generalized transport must round-trip through quote and respect the η-pin used
   to certify pushes. *Mitigation:* add `transpG` as a real 4th Kan-group member
   (recommended above) so the stuck form is its own neutral — no encoding tricks.
   This re-hashes the Kan group (own hash space) but touches no other group.
   *Open:* whether to retire the legacy `transp` (cleaner) or keep it as a derived
   member for ch21/ch23 hash stability (safer). Recommend retire + bump those
   listings, since the Kan group re-hashes regardless once a member is added.

2. **Regularity under recursion (C-REG, gated not blocking).** Non-regular CCHM:
   `transpᵍ` over a per-former-constant line returns a re-transporting λ, not `f`
   on the nose. Canonicity survives (prior art), but *some* expected definitional
   equalities become merely propositional. *Mitigation:* X2 canonicity + regularity
   property tests; adopt Swan only on failure. **This is the resolution C-REG
   asked for** (non-regular, gated on tests).

3. **The constancy probe under the φ cofibration.** `transpᵍ`'s φ=⊤ rule and the
   sentinel constancy rule can both apply; order matters (φ=⊤ first, then the
   sentinel probe, then structural — mirroring `tryComp`'s ⊤/⊥ ordering at
   `core/eval.go:1255`). *Open:* confirm the probe still terminates when the line
   contains a *nested* `transpFill` (it builds `λj. A (imin s j)`, which the probe
   enters under `VVar(0)` per `mentionsRefVal` `core/eval.go:1088` — finite, no
   forcing, so it terminates; pin with a property test).

4. **`comp` bottoms out in proper-face `hcomp`.** The `comp = transpFill ▸ hcomp`
   seam is only as complete as R-BOX's inner `hcomp`. Proper multi-branch faces
   stay gated on **C-OVERLAP** (R-BOX risk 1, `R-BOX.md:337`). *Honest label:* the
   `transpFill`/A1/floor-and-walls construction is ready; the *closed* `comp` on a
   proper face waits on R-BOX+C-OVERLAP.

5. **No `hfill` yet.** A5 (`sigmaF`) needs `hfill` (the filler form of `hcomp`);
   R-BOX owns it (R-BOX risk 4). R-FILL's `transpFillF`/`transpFillB` are the
   *transport* fillers; the *homogeneous* filler is R-BOX's. Keep the two named
   distinctly to avoid confusion.

---

## Test/gate plan

- **Listing chXX (A1 ready slice):** the constant-domain collapse (refl) and the
  varying-domain push (η-pinned), plus the `pathF`-line-stays-stuck negative pin.
- **Store round-trip (if `transpG` is added):** `KanRoleOf`/`KanHash` agree for the
  4th member; `KanNames()` length 4; group determinism (every session agrees on the
  hash). Mirror the existing 3-member Kan tests.
- **Negative pins (session test):** `transpᵍ` with a *neutral* φ and a varying line
  stays stuck (normal form has a `transp`/`transpG` head); A1 does not over-fire on
  `pathF`/`fib` lines (mirror `structural_test.go:37`).
- **Rapid properties (harness/cubical_props_test.go):**
  - *regularity-collapse:* `transpFillF A φ u0 i0` converts to `u0`, and
    `transpFillB A φ u1 i1` converts to `u1` (start-of-fill is the identity, via
    `ieq0 i0 ~> ftop` / `ieq1 i1 ~> ftop`).
  - *full-fill:* `transpFillF A ⊥ u0 i1` converts to `transpᵍ A ⊥ u0` (end-of-fill
    is full transport).
  - **canonicity (the C-REG gate):** random *closed* inner-`nat` terms built
    through `transp`/`comp`/A1 normalize to numerals.
  - *boundary coherence of `comp`:* for closed φ that reduce to ⊤, the general
    `comp` branch's output converts to `u i1 htop` (matches the shipped ⊤ rule).
- **Frame Lemma (X1):** the new helpers build sub-terms via `m.refVal(hash)` and
  force scrutinees through `m.Force` — the same logged seam; assert no new unlogged
  unfolds (the proof-cache dependency set is unchanged in shape).
- **Containment (Thompson):** `git grep` confirms no new core `Tm`/`Val`
  constructor; no hash-format bump (`defFormatVersion` unchanged); hashes of
  ch09/ch10/ch17–ch20 and the fib/interval/path/face/sys groups unchanged. Only the
  Kan group re-hashes (own space) iff `transpG` is added.

---

## Unblocks (which implement nodes, and what they still need)

- **A1** (`varying-domain piF transp/comp` ⇐ R-FILL): **fully unblocked** — A1 *is*
  the varying-domain `piF` rule above, built on `transpFillB`. Still needs the
  `transpG` stuck-form decision (risk 1) settled before coding. Ready-to-build.
- **A2** (`hcomp/comp proper-face fills (fib,piF)` ⇐ R-FILL, R-BOX, C-REG): R-FILL
  delivers the `comp = transpFill ▸ hcomp` seam and the `transpFill` floor/walls;
  A2 still needs **R-BOX**'s per-former `hcomp` (the inner box) and **C-REG**
  (resolved here: non-regular, gated on X2). The *proper multi-branch* face still
  needs **C-OVERLAP** (lands as A8).
- **A3** (`transp/comp over pathF` (base composition) ⇐ A1, A2): R-FILL's `comp`
  derivation reduces a `pathF`-line `comp` into R-BOX's `hcomp (pathF …)`. A3 needs
  A1 (the line's domain transport, done) and A2 (the inner path `hcomp`, R-BOX +
  overlap gate).
- **A4** (`pathJ-via-comp on a general path` ⇐ A3): needs A3 (path composition)
  landed; R-FILL is its transitive prerequisite. Not directly built here.
- **A5** (`sigmaF group + Kan over sigmaF` ⇐ R-SIGMA, A2): R-FILL provides the
  `comp` over the B-fibre line the elementwise `sigmaF` rule needs (R-BOX `R-BOX.md:193`);
  A5 still needs **R-SIGMA** (the former + `fst`/`snd`/`spair`) and R-BOX's `hfill`.
- **X1/X2** (soundness guard): R-FILL adds the canonicity, regularity-collapse,
  full-fill, and boundary-coherence properties listed above; X2 is the C-REG gate.

**Honest label:** *ready-to-build now* — `transpG` helper (+ optional 4th Kan
member), `transpFillF`/`transpFillB`, the **A1 varying-domain `piF` rule** (the
keystone deliverable), the `comp = transpFill ▸ hcomp` seam *as far as R-BOX's
inner `hcomp` reaches*, chXX, the negative pins, the regularity/full-fill/canonicity
properties. *Research/gated* — closed `comp` on a proper multi-branch face
(C-OVERLAP/A8), Swan regularity (only if X2 canonicity fails), `pathF`/`sigmaF`/
`Glue` inner fills (R-BOX/R-SIGMA/R-GLUE).
