# GLUE-DESIGN — computational univalence on the glued NbE substrate (A6/A7)

*Consolidated, decision-locked design. Synthesizes the three fan-out docs
([R-GLUE](R-GLUE.md), [C-OVERLAP](C-OVERLAP.md), [R-UA](R-UA.md)) against the
**actually-landed** code (the 11-member `fib` group `store/fib.go`, the 4-member
`sigmaF` group `store/sigma.go`, the 4-member `kan` group `store/kan.go` incl.
`transpG`). Where the fan-out research differs from what shipped, this doc
follows the shipped code and says so. The §F discipline: ship the computing
slice, honestly stuck-label the deep half.*

> **AS BUILT (A6 ready slice — LANDED, green).** The ready slice of this design
> is now implemented and verified:
> - **`equiv` group** (`store/equiv.go`): `Equiv`/`equivFun` as **opaque
>   bodiless builtins** (not the inlined `sigmaF` tower §2 first proposed). The
>   kernel rules never inspect an equivalence's internals — only its *type* — so
>   an opaque `Equiv` is a sound over-approximation (fewer equations, none wrong).
>   This sidesteps all de-Bruijn-inlining fragility AND makes the unglue ⊤
>   boundary cleanly containable (next point). Real `Equiv := Σf.isEquiv f` body
>   is the A7 refinement.
> - **`glue` group** (`store/glue.go`): `Glue`/`glue`/`unglue` (`core.GlueRole`/
>   `GlueInfo`, a **separate** group mirroring `sigmaF` — not an extended
>   `FibRole`). Member types reference `Equiv` by builtin hash; no library hash in
>   the kernel.
> - **Three ι-rules** (`core/eval.go`): type boundary `El (Glue A ⊤ T e) ~>
>   El (T htop)`; **unglue β** `unglue A φ T e (glue A φ T e t a) ~> a`; **unglue
>   ⊤ boundary** `unglue A ⊤ T e g ~> equivFun (T htop) A (e htop) g`. The ⊤
>   boundary IS shipped (revising §4.3's earlier "deferred" call): with `equivFun`
>   as its own builtin hash the reduct is a clean spine, no `fstF`/`isEquiv`
>   reconstruction, no containment break.
> - **Gotcha corrected:** `unglue` takes **5** args (A φ T e g); `glue` takes 6
>   (A φ T e t a).
> - **Certified:** `listings/ch45_glue.rune` (all three theorems refl-pinned),
>   `store/glue_test.go` (determinism/roles), `structural_test.go`
>   `TestGlueProperFaceStaysNeutral` (negative pin: proper face stays stuck). No
>   hash-format bump; full suite green.
> - **UPDATE — since landed:** `∀i.φ` (`forallF`), `RestrictIv` (value
>   face-restriction, `core/quote.go:19`), `extend` (contractibility filler,
>   `core/eval.go:2717`), `pabsU` (type-level path abstraction), and **hcomp/comp
>   over Glue** (re-glue via `unglue`/`unglueT`, `tryHcomp`/`tryComp`, ch50) all
>   shipped. **transp over Glue** computes for a `glue`-INTRO input over a φ-constant
>   line (`transpGlueIntro`, `core/eval.go:2768`). `fsplit` (ch52) discharges
>   C-OVERLAP — the §1 `sys` smart-constructor is **superseded, not built**.
> - **Still stuck (the M2 residue):** the **general** transp-over-Glue arm for a
>   **neutral input** or **φ varying in i** (`core/eval.go:2913-2917`, returns
>   `nil,false`) — wire `RestrictIv`+`extend` through full `compGlue` (G1); and
>   **A7** (re-body the postulated `ua` head as the Glue line + delete the
>   `castU`-over-`ua` shortcut, the deliberate `fib`-hash event — G2).

---

## 0. Where this sits

`Glue` is the construct that turns an *equivalence* into a *transporting path* —
it is the engine of `ua`. With it, `transp`/`castU` over a univalence path
*compute*, retiring the postulated `ua` head (`store/fib.go:96`,
`core.FRoleUa`). It is the keystone of M2 (telos 1, "univalence COMPUTES").

**Dependency reality (what is landed vs. what blocks):**

| Prereq | Status | Member / file |
|---|---|---|
| `Equiv` substrate (`sigmaF`/`pairF`/`fstF`/`sndF` + computing projections) | **LANDED** | `store/sigma.go` (A5) |
| `pathF`/`pabs`/`papp` + boundary | **LANDED** | `store/path.go` (§F ph2) |
| `transpG` generalized transport (i→j) + `transpFill` | **LANDED** | `store/kan.go:30` (A1/R-FILL) |
| varying-domain `piF` transp | **LANDED** | `core/eval.go:1855` (A1) |
| face lattice `F`/`ieq0`/`ieq1`/`fand`/`for`/`ftop`/`fbot` | **LANDED** | `store/face.go` (3a) |
| partial elements `holds`/`htop`/`hand`/`horl`/`horr` | **LANDED** | `store/sys.go` (3b) |
| proper-face `hcomp` (open-box, non-`piF` formers) — **R-BOX** | **OPEN** | research |
| multi-branch system overlap-agreement — **C-OVERLAP** | **RESOLVED (design)** | option (b), this doc §1 |
| `∀i. φ i` cofibration | **ABSENT** | research, §6.5 |

**The cut this doc locks:**

- **READY-TO-BUILD now** (everything its prereqs are landed for): the `Glue`
  group (former + `glue` + `unglue`), the **type boundary** `El (Glue A ⊤ T e) ~>
  El (T htop)`, the **unglue β** `unglue … (glue … t a) ~> a`, the **unglue
  boundary** `unglue A ⊤ T e g ~> equivFun (e htop) g`, the **⊤/⊥ Kan
  degeneracies**, and the `Equiv` library layer. These need only what is
  *landed*. **This is the deliverable of A6.**
- **HONEST-STUCK** (shipped as guarded skeletons that light up automatically as
  the prereq lands): the **transp-over-Glue fixup** (blocked on R-BOX proper-face
  `hcomp` + `∀i.φ` + C-OVERLAP-for-multi-branch), the **hcomp-over-Glue re-glue
  on a proper face** (blocked on R-BOX), and **comp-over-Glue** (= the seam of
  both). A7's *compute-half* inherits this gate.

The honest-stuck half is the **same gate the whole A-track inherits** from R-BOX,
not a new failure. The contained, teachable half is buildable today.

---

## 1. C-OVERLAP — resolved (gates the multi-branch half)

**Decision: option (b) — overlap-agreement is a *checker-side typing obligation
at system formation*, discharged by conversion under the meet face. NO
value-level irrelevant-`Prop` conversion rule. `core/conv.go` is untouched.**
(Full rationale: [C-OVERLAP.md](C-OVERLAP.md); roadmap recommendation
`humble-humming-elephant.md:149-151`; Thompson's "kernel never grows".)

Why this is the load-bearing decision for Glue specifically: `glue`'s `t`-
component and `a`-component must **agree on φ** (`unglue (glue t a) = a` requires
`equivFun (e h) (t h) = a` on φ), and the transp/hcomp fixups feed *multi-branch*
systems whose overlapping faces must agree definitionally. This engine's proof
irrelevance is **constructor-keyed** (`VRefl=VRefl`, `NCast`/`NSubst` skip the
proof — `core/conv.go:38-96`), **not** type-keyed; `Conv` (`core/conv.go:12`)
takes no type, so it *cannot* equate two arbitrary `holds φ` neutrals. Two
distinct `hand … `/`horl …` spines do not convert.

**The three contained layers (no kernel change):**

- **(b1) `sys` smart-constructor.** A disjunctive system
  `sys A φ [φ₁ ↦ u₁ ; … ; φₙ ↦ uₙ]` elaborates to the plain `holds φ -> El A`
  (no new core form) **only after** the elaborator discharges, for every `i<j`,
  the obligation `uᵢ ≡ uⱼ` under a fresh `h : holds (fand φᵢ φⱼ)`. One `Conv`
  call at `lvl+1`, mirroring CCHM "convert each branch restricted to the meet
  face." Lives in `elaborate/system.go` (new), **outside the kernel**.
- **(b2) `holds`-proof opacity.** Branch bodies cannot observe their proof —
  there is no `holds` eliminator (`store/sys.go`), the intros are rigid with no
  ι-rules. So both restricted branches end up applied to the *same* abstract `h`
  and conversion succeeds on the nose. This is a *consequence* of the existing
  design, promoted to a stated invariant + property test, **not** a new
  postulate.
- **(b3) Kan threads the abstract proof, never reconstructs one.** When a Kan
  rule reduces on a face that forces to a single disjunct (via the lattice ι
  `for ftop _ ~> ftop`, `store/face.go`), it threads the corresponding
  `horl`/`horr` proof, exactly as the ⊤ rule already feeds `htop`
  (`core/eval.go:1213`). Overlap only *matters* on `fand φ ψ`, where b1 already
  guaranteed agreement — so the evaluator needs **no overlap check at all**.

**Consequence for this doc:** the *boundary/β/⊤-degeneracy* slice uses only single
`⊤` faces and **sidesteps C-OVERLAP entirely** — buildable today. The *full Kan
fixups* feed multi-branch systems and **consume b1** — they ship behind the same
R-BOX gate, and inherit b1 when it and R-BOX land together.

---

## 2. `Equiv` — a library type over the landed `sigmaF` (not a builtin)

`Glue`'s `e` argument is a *partial `Equiv`*, which must be a genuine fibrant code
(`UF`) so it can be the codomain of `holds φ -> UF`. Built **entirely** from the
landed `sigmaF`/`pairF`/`fstF`/`sndF` (`store/sigma.go`, roles
`GRoleSigma`/`GRolePair`/`GRoleFst`/`GRoleSnd`) + `pathF`/`piF` — **no new
builtin**. Ships as ordinary Rune library defs (ch25 listing), Savage-readable:

```
fiber   (A B : UF) (f : El A -> El B) (y : El B) : UF
        := sigmaF A (fn x. pathF B (f x) y)
isContr (A : UF) : UF
        := sigmaF A (fn c. piF A (fn x. pathF A c x))
isEquiv (A B : UF) (f : El A -> El B) : UF
        := piF B (fn y. isContr (fiber A B f y))
Equiv   (A B : UF) : UF
        := sigmaF (piF A (fn _. B)) (fn f. isEquiv A B f)

equivFun   (A B : UF) (e : El (Equiv A B)) : El A -> El B          := fstF … e
equivProof (A B : UF) (e : El (Equiv A B)) : El (isEquiv A B (equivFun A B e)) := sndF … e
```

The evaluator never special-cases `Equiv`; it sees only `sigmaF` heads and
`fstF`/`sndF` projections, **which already compute** (A5). `equivProof`
contractibility is what the transp-over-Glue fixup consumes — and the reason that
fixup is deep.

---

## 3. The `Glue` group — a SEPARATE builtin group (mirroring `sigmaF`)

**Correction to R-GLUE.md.** R-GLUE proposed extending `core.FibRole` with
`FRoleGlue`/`FRoleGlueIn`/`FRoleUnglue`. The landed `sigmaF` group did **not**
extend `FibRole` — it defined its **own** `core.SigmaRole`/`core.SigmaInfo`
interface (`GRoleSigma…`, `store/sigma.go:72`, `SigmaRoleOf`/`SigmaHash`). Glue
**follows the landed precedent**: a new `core.GlueRole`/`core.GlueInfo` interface
+ `store/glue.go`, parallel to sigma. This keeps each group's hash space and
role-dispatch self-contained (Thompson) and matches the code reviewers will read.

**Three members, own hash space, no `defFormatVersion` bump** (no new core
constructor — same discipline as every group since v2):

```
Glue   : (A : UF) -> (φ : F) -> (T : holds φ -> UF)
         -> (e : (h : holds φ) -> El (Equiv (T h) A)) -> UF
glue   : (A : UF) -> (φ : F) -> (T : holds φ -> UF)
         -> (e : (h : holds φ) -> El (Equiv (T h) A))
         -> (t : (h : holds φ) -> El (T h))   -- T-component, agrees with a on φ
         -> (a : El A)                          -- A-component
         -> El (Glue A φ T e)
unglue : (A : UF) -> (φ : F) -> (T : holds φ -> UF)
         -> (e : (h : holds φ) -> El (Equiv (T h) A))
         -> El (Glue A φ T e) -> El A
```

`Glue` is a former (closes `UF` under gluing); `glue` is a **canonical neutral
intro** (permanently-neutral head, in `rigidHead` via
`GlueRoleOf(h) != GRoleNone`, mirroring `core/eval.go:579`); `unglue` **computes**
by ι-rule.

**Member-type hash stability (R-GLUE Interface Option 1, confirmed):** the member
types reference `Equiv`, a user-space def with a session-dependent hash. To keep
`Glue`'s hash stable, **inline `Equiv`'s `sigmaF`/`piF`/`pathF` expansion** into
the member types so they reference only *builtin-group* hashes. So
`AddGlue(fib [11]core.Hash, sigma [4]core.Hash, iv [6]core.Hash, face [7]core.Hash, sys [5]core.Hash) [3]core.Hash`
— takes the groups it builds `Equiv` from, exactly as `AddSigma(fib)` /
`AddKan(fib,iv,face,sys)` take theirs. Register **last** in `session.go` (after
`AddSigma`, after `AddKan`) so all referenced hashes are fixed. Group tag bytes:
pick fresh letters (`'U'`/`'u'` for univalence — verify unused at build; `'G'`/
`'g'` are sigma's).

---

## 4. The ι-rules (the READY slice)

All four fire via the same `Force`/`tryRules` seam the landed groups use.

### 4.1 Type boundary — `El (Glue A ⊤ T e) ~> El (T htop)`

In `tryFibIota`'s `FRoleEl` decode (`core/eval.go:1114`), add a `Glue`-head case:
force φ (`cargs[1]`) via `faceConst` (`core/eval.go`); if `⊤`, return
`El (T htop)` = `m.Apply(m.refVal(elHash), m.Apply(cargs[2], htop))`. On a proper
or neutral φ, `El (Glue …)` stays neutral — `Glue` is then a genuinely new
fibrant type. (CCHM `Glue [1 ↦ (T,f)] A = T`.) **No φ=⊥ boundary** — `Glue A ⊥ T e`
is path-equivalent to `A`, not definitionally equal; honest-stuck on ⊥.

### 4.2 unglue β — `unglue A φ T e (glue A' φ' T' e' t a) ~> a`

New `tryGlueIota`, dispatched from `tryRules` alongside `papp`/`fstF`. Scrutinee
forces to a `glue`-headed spine → return its `a` (last arg).

### 4.3 unglue boundary — `unglue A ⊤ T e g ~> equivFun (T htop) A (e htop) g`

In `tryGlueIota`: φ forces to `⊤` → return `equivFun` applied to `e htop` and `g`,
built via `SigmaHash(GRoleFst)` (`store/sigma.go:92`). **The load-bearing
equation:** on the face where `Glue` collapses to `T`, `unglue` is *not a
projection* — it is the equivalence's forward map. When φ=⊤, `g : El (Glue A ⊤ T
e) ≡ El (T htop)` by §4.1, so `g` *is* a `T`-element and `equivFun (e htop) g :
El A` typechecks. This is what makes the two ends of a univalence path land on
`id` and `f`.

### 4.4 Glue-η — NOT definitional (parked, mirrors sigmaF-η)

`glue [φ ↦ unglue g] (unglue g) ≡ g` (needed by comp-Glue round-trip) is **not**
shipped in `core/conv.go` (Thompson: no conv edit, same call as sigmaF-η, A5
Risk 2). Provided as a *provable inner path*; revisited only if A6 property tests
force a scoped inner reduction. The shipped β/boundary rules are first-order and
need no η.

---

## 5. Kan over Glue — ⊤/⊥ degeneracies READY, fixups STUCK

New role-arms in the `fibFormer`-role switches of `tryTransp`/`tryHcomp`/`tryComp`
(after the `FRolePiF`/sigma arms, e.g. `core/eval.go:1855`). Each ships the
degeneracy **now** and a guarded skeleton that returns `nil,false` (honest-stuck)
until R-BOX lands, gated behind a predicate (e.g. `properFaceHcompAvailable()`)
so it lights up automatically — no re-plumbing.

### 5.1 transp-over-Glue (R-GLUE Decision 4 — the deepest rule)

For a Glue line `λi. Glue (A i)(φ i)(T i)(e i)`, abbreviating `ψ := ∀i. φ i`:

```
a0  := unglue (A i0)(φ i0)(T i0)(e i0) g0       -- pull g0 down to A i0
ã   := transpFill (λi. A i) a0                   -- LANDED (A1/R-FILL)
a1' := transp (λi. A i) a0                        -- LANDED
on (φ i1): (t1, p1) := equivProof (e i1) a1'     -- contractible fiber centre — needs equivProof
a1  := hcomp (A i1) ((φ i1) ∨ ψ) [walls from p1, ã] a1'   -- proper-face hcomp — R-BOX
result := glue (A i1)(φ i1)(T i1)(e i1) (λh. t1) a1
```

- **READY now — the ⊤-degeneracy** (`ψ = ⊤`, φ total on the whole line):
  `transp (Glue-line) g0 ~> transp (λi. T i) g0` — pure `transp` over the
  `T`-line (LANDED), no fixup. Ship it.
- **STUCK — the full fixup:** consumes proper-face `hcomp` on `A i1` (**R-BOX**),
  the `∀i.φ` cofibration (**absent**, §6.5), and `equivProof` branching
  (**X1/R-FRAME** cache-exactness, §6.6). Ship the skeleton; `transpFill`/`transp`
  over the A-line are *already landed*, so only R-BOX + `∀i.φ` block completion.

### 5.2 hcomp-over-Glue (R-GLUE Decision 5 — closer to ready)

`Glue A φ T e` is a *fixed* code, so no `transpFill` — composes the `T`-part
homogeneously and the `A`-part through `unglue`, then re-glues:

```
hcomp (Glue A φ T e) ψ u u0
  ~> glue A φ T e
       (λh. hcomp (T h) ψ (λj h'. u j h') u0|_φ)              -- T-part on φ
       (hcomp A ψ (λj h'. unglue … (u j h')) (unglue … u0))   -- A-part via unglue
```

- **READY now — ⊤:** `hcomp Glue ⊤ u u0 ~> u i1 htop` (the generic total-system
  rule, already in `tryHcomp` `core/eval.go:1208`; no Glue special-case needed).
  **⊥:** `hcomp Glue ⊥ u u0 ~> u0` (landed empty-system rule, A2).
- **STUCK — re-glue on a proper φ:** the `T`-part is a proper-face `hcomp (T h)`
  (**R-BOX**) and feeds a multi-branch system (**C-OVERLAP b1**, resolved §1).
  Closer to ready than transp-Glue (no `transpFill`), but still R-BOX-gated.

### 5.3 comp-over-Glue = transpFill ▸ hcomp (the seam)

`comp` over a Glue line is `transp`(Glue) then `hcomp`(Glue), via the landed
R-FILL seam (`tryComp` already short-circuits ⊤/⊥, `core/eval.go:1255`). Inherits
both gates. Structure shipped; full rule stuck on R-BOX + `∀i.φ`.

---

## 6. A7 — `ua` from `Glue` (retiring the postulate)

With `Glue` in hand, the postulated 4-component `ua` head (`core.FRoleUa`,
`store/fib.go:96`) is **re-derived**, the standard CCHM construction:

```
ua A B (e : Equiv A B) : pathU A B
   := pabsU (λi. Glue B ((ieq0 i) ∨ (ieq1 i))
                  [ (i=0) ↦ A , (i=1) ↦ B ]
                  [ (i=0) ↦ e , (i=1) ↦ idEquiv B ])
```

At `i=0` the face is `⊤`, so `Glue B ⊤ [A] [e] ~> A` (§4.1 type boundary); at
`i=1`, `~> B`. The endpoints land definitionally — that is *why* the boundary
rule is load-bearing. `castU`/`pathJ` over a ua-path then reduce **through**
transp-over-Glue.

**Hash-event caveat (R-SIGMA Decision 7, `00-INDEX.md:159`).** Re-typing `ua` over
`Equiv` and re-deriving it from `Glue` **mutates the `fib` group's hash and
ch10**. Schedule that under **A7**, deliberately — **A6 leaves `ua`/`castU`
untouched and only *adds* the Glue group** (no hash churn). A7's *compute-half*
(`castU` over ua actually reducing) inherits A6's R-BOX/`∀i.φ` gate, because it
runs transp-over-Glue.

**A7 ready vs. stuck:** the *re-derivation* (defining `ua` as the `Glue` line, so
it is no longer postulated) is **ready once A6's former + boundary land**. The
*computation* of `castU`/`J` over the ua-path is **stuck on the same fixup** as
transp-over-Glue.

---

## 6.5 The `∀i. φ` cofibration (absent — research)

The transp-Glue fixup quantifies a face over the interval (`ψ := ∀i. φ i`,
"φ holds on the whole line"). Rune's face lattice (`store/face.go`) has
`ieq0`/`ieq1`/`fand`/`for`/`ftop`/`fbot` but **no `∀i`**. CCHM/cubicaltt have a
`forall` cofibration former. **The shipped (⊤-degeneracy) slice does not need it**
(φ already total). **The full fixup does.** When the fixup is built, add `∀i.φ` as
an eighth `face`-group member — flag for R-BOX/R-FILL coordination (they hit the
same need). Research, not blocking the ready slice.

## 6.6 Frame Lemma (X1/R-FRAME) — the fixup branches on `equivProof`

The shipped β/boundary/⊤ rules do **not** branch on proof content → cache-exact.
The **fixup consults `equivProof`** (a contractibility *proof*). R-FRAME must
classify this: the proof content is erased/irrelevant, so the branch is on the
*fiber point*, not the proof — *likely* cache-exact, but **must be proven before
the fixup lands exact**. Until then the fixup ships behind `markImprecise` (the
self-retiring cache-imprecision fallback). Ship the exact rules now; gate the
fixup.

---

## 7. Containment (session wiring)

- `core/conv.go`: **untouched** (C-OVERLAP option b).
- `core/eval.go`: additive role-arms only (Glue ι-rules + Kan degeneracies +
  guarded fixup skeletons).
- `store/glue.go` (new): the group, mirroring `store/sigma.go`
  (`glueNames=[3]string{"Glue","glue","unglue"}`, `GlueRoleOf`/`GlueHashes`,
  inline-`Equiv` member types).
- `internal/session/session.go`: register `AddGlue` last; add `glueHashes[0]`
  (`Glue` former) to `typeRefs` (erases to unit); `glue`/`unglue` covered by
  `innerTaint` (check, don't deploy — the v3 line). `Equiv`/`fiber`/`isContr`/
  `isEquiv` are `sigmaF`-built → transitively inner-tainted automatically.
- `elaborate/system.go` (new): the C-OVERLAP `sys` smart-constructor (b1), needed
  by the multi-branch Kan half.
- **No `defFormatVersion` bump** (no new core constructor). ch09–ch24 byte-identical.

---

## 8. The teachable artifact — `listings/ch25_glue.rune`

The READY deliverable in the ch23/ch24 `refl`-pinning style: the three equations
that compute, certified by `refl`. (The transp-over-Glue fixup is the honest-stuck
remainder, named not shipped.)

```
-- (1) TYPE boundary: Glue on a TOTAL face IS its T-component.
glueTopType : (A : UF) -> (T : holds ftop -> UF)
   -> (e : (h : holds ftop) -> El (Equiv (T h) A))
   -> Eq U1 (El (Glue A ftop T e)) (El (T htop)) is
  fn A T e is refl (El (T htop)) end          -- §4.1 fires; refl certifies

-- (2) unglue BETA: unglue of a glued pair is its A-component.
unglueBeta : (A : UF) -> (phi : F) -> (T : holds phi -> UF)
   -> (e : …) -> (t : (h : holds phi) -> El (T h)) -> (a : El A)
   -> Eq (El A) (unglue A phi T e (glue A phi T e t a)) a is
  fn A phi T e t a is refl a end              -- §4.2 fires; refl certifies

-- (3) unglue BOUNDARY: on a total face, unglue is the equivalence's forward map.
unglueTop : (A : UF) -> (T : holds ftop -> UF)
   -> (e : (h : holds ftop) -> El (Equiv (T h) A)) -> (g : El (Glue A ftop T e))
   -> Eq (El A) (unglue A ftop T e g) (equivFun (T htop) A (e htop) g) is
  fn A T e g is refl (equivFun (T htop) A (e htop) g) end   -- §4.3 fires; refl certifies
  -- NB g : El (Glue A ⊤ T e) ≡ El (T htop) by (1), so it typechecks as a T-element.
```

**The lesson (Savage):** *`Glue` welds a partial type `T` onto a base `A` along an
equivalence `e`. On the face where the weld is total, the glued type IS `T`, and
`unglue` is not a projection — it is `e`'s forward map. Off the face, `Glue` is a
new type, and transporting along it runs `e` exactly where it must.* The
transp-over-Glue fixup — "run the equivalence's *inverse* (its contractible
fiber) to recover the T-component" — is the sentence that needs R-BOX; named, not
yet computed, the same honesty as varying-domain `piF`.

---

## 9. Test / gate plan

- **Listing gate** (`harness/listings_test.go`): ch25 elaborates and checks
  (inner-tainted → `EmitProgram` skips it). The v3 bar is "elaborate and check".
- **ι-rule unit tests** (`store/glue_test.go`, mirroring `sigma_test.go`): unglue
  β; type boundary; unglue boundary; `Glue`/`glue` stay neutral on a proper φ
  (negative pins).
- **Structural Kan pins** (`internal/session/structural_test.go`): transp over a
  fully-⊤ Glue line ~> transp over the T-line (ready degeneracy); `hcomp Glue ⊤`
  / `⊥` rules; **a proper-face transp-over-Glue stays stuck** (negative pin —
  guards the ready/stuck boundary, the R-BOX gate).
- **Equiv-library check:** `fiber`/`isContr`/`isEquiv`/`Equiv` elaborate+check as
  `sigmaF`-built codes.
- **Cubical property tests** (`harness/cubical_props_test.go`): `Glue` in the
  closed-former normalisation sweep; confluence of unglue β/boundary with the `El`
  decode; type-preservation on the new ι-rules.
- **Hash-stability regression:** ch09–ch24 outputs + all prior group hashes
  byte-identical after adding the Glue group; assert no `defFormatVersion` bump.
- **C-OVERLAP gate** (`elaborate/system_test.go`): vacuous overlap
  (`for (ieq0 i)(ieq1 i)` → `fbot`) checks; non-vacuous agreeing overlap checks;
  **disagreeing overlap is REJECTED** with a located error; mutation test —
  flipping the `Conv` in `CheckSystem` to always-true must break that test.
- **Frame Lemma (X1):** shipped rules cache-exact (no proof branch); the fixup
  behind `markImprecise` until R-FRAME clears the `equivProof` branch.
- **Canonicity (post-fixup, M2 gate):** closed terms through transp-over-Glue
  normalise to a `glue`/constructor form (C-REG/Swan check; C-REG resolved by
  R-FILL — ship non-regular CCHM, Swan only if this fails).

---

## 10. Build order (incremental, each a green checkpoint)

1. **`store/glue.go` + `core.GlueRole`/`GlueInfo`** — the group, registered last;
   former neutral, inline-`Equiv` member types. Hash-stability test green.
2. **`Equiv` library layer** (ch25 prelude) — `fiber`/`isContr`/`isEquiv`/`Equiv`
   /`equivFun`/`equivProof` over landed `sigmaF`. Elaborate+check green.
3. **Type boundary §4.1** + **unglue β §4.2** + **unglue boundary §4.3** — the
   three ι-rules. ch25 (1)(2)(3) refl-pinned green. **← A6 ready slice complete.**
4. **Kan ⊤/⊥ degeneracies §5** — transp/hcomp/comp over Glue on total/empty
   faces. Structural pins green.
5. **C-OVERLAP `sys` constructor** (`elaborate/system.go`, §1) — the multi-branch
   gate. `system_test.go` green. *(Independent of 1–4; can parallelize.)*
6. **— honest-stuck boundary —** Ship guarded skeletons for the fixups (§5.1–5.3)
   returning `nil,false` behind `properFaceHcompAvailable()`. Negative pins green
   (they stay stuck).
7. **A7 re-derivation** — define `ua` as the Glue line (§6), retiring the
   postulate; the deliberate `fib`-hash event + ch10 update.
8. **[BLOCKED on R-BOX + `∀i.φ`]** the transp/hcomp-over-Glue fixups; A7
   compute-half (`castU`/`J` over ua reduce); canonicity suite. Lights up
   automatically as R-BOX lands — the skeletons are already wired.

Steps 1–7 are buildable on the **currently landed** substrate. Step 8 is the
research tail, gated on R-BOX (proper-face open-box `hcomp`) and the `∀i.φ`
cofibration — the same keystone gap the rest of the A-track waits on.

---

## 11. Status

**A6 (Glue group + Kan-over-Glue):** LANDED for the group, former, glue/unglue,
both boundaries, ⊤/⊥ Kan degeneracies, the `Equiv` library, **hcomp/comp over
Glue**, and **transp over Glue for a `glue`-intro / φ-constant line**
(`transpGlueIntro`). The prereqs the fixups waited on — `∀i.φ` (`forallF`),
value face-restriction (`RestrictIv`), the contractibility filler (`extend`) —
are all **landed**; C-OVERLAP is discharged by `fsplit`. The **one remaining
fixup** is the general transp-over-Glue arm (neutral input / φ varying in i,
`core/eval.go:2913-2917`) — wiring the landed pieces through full `compGlue` (G1).

**A7 (ua-as-Glue):** `pabsU` is landed and the derived route already computes
(`TestUnivalenceComputesViaGlue`); the remaining work is re-bodying the postulated
`ua` head over `Equiv` as the Glue line and deleting the `castU`-over-`ua`
shortcut — the deliberate `fib`-hash event (G2). Depends on G1 for the general case.

This is the keystone of computational univalence: the construct that turns an
equivalence into a transporting path. The design makes its *contained, teachable*
half buildable **now** on landed code, while naming the deep half precisely —
the §F discipline held end to end.

Sources: [R-GLUE.md](R-GLUE.md), [C-OVERLAP.md](C-OVERLAP.md), [R-UA.md](R-UA.md),
[R-FILL.md](R-FILL.md), [R-BOX.md](R-BOX.md), [R-SIGMA.md](R-SIGMA.md);
CCHM ([arXiv:1611.02108](https://arxiv.org/pdf/1611.02108)) §6.3; landed code
`store/{glue→sigma,kan,fib,face,sys}.go`, `core/eval.go`.
