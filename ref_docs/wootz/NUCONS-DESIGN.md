# NUCONS-DESIGN — the cubical coinduction principle that closed the E2 converse

*Consolidated design record (decision-locked, grounded in landed code), in the style
of RBOX-DESIGN / GLUE-DESIGN. This is the answer to the question the roadmap parked
as "a real cubical COINDUCTION PRINCIPLE = a dedicated MULTI-ITERATION research
effort" (`humble-humming-elephant.md` E2/cont.55). The principle was found and
landed; this doc names it, says why it works, and reconciles the now-stale "E2
PARKED at `isEquiv glue`" status.*

## 1. The question that was parked

E2's correctness currency is **bisimilarity-is-path** for the coinductive stream
`Str A = Nu (λX. A × X)`: a `pathF (Str A) s t` should correspond to its pointwise
observations. The **forward** half landed early (ch66 `headPath`/`tailPath`: a path
entails equal heads + a path between tails). The **converse** —

```
bisimToPathStr : (head s ≡ head t) -> (tail s ≡ tail t) -> pathF (Str A) s t
```

— resisted every route for many iterations. The findings (CLOCK-IRRELEVANCE-DESIGN
§§1–4, and the plan's E2 log) converged on a single obstruction, reachable three
different ways:

- **per-clock guarded route** (`split`/`∀κ. gStr`): the converse's corecursion
  assembles a guarded path whose endpoints are non-definitional, and `split` is a
  `dfix` corecursor whose observations are **stuck under application** (the
  dfix-observation wall: `convDfix` fires only at a comparison top, never under an
  eliminator; relaxing it to fire on an applied `dfix` is termination-unsound —
  `lap (next dfix)(next x)` applies the recursive `dfix` eagerly and diverges).
- **per-clock bridge as an equivalence** (`isEquiv glue`): localising the converse
  to one obligation `isEquiv (GStrAll A)(Str A) glue` still needs the two round-trip
  homotopies, both back at the dfix wall.
- **global `unfold` route**: building the stream from its observations by `unfold`
  rebuilds the tail *corecursively*, so the rebuilt tail is only **bisimilar** to
  the original, not equal — the very coherence we are trying to prove.

Every route bottomed out at the **cons-η** / rebuild-from-observations law

```
consStr (head s) (tail s)  ≡  s          -- "a stream IS its head consed onto its tail"
```

being **non-definitional**. CLOCK-IRRELEVANCE-DESIGN's `converseFromConsEta` proved
the decisive reduction: *the moment cons-η is supplied as a path, the converse
assembles* (cons the observations into a path between cons-forms, then
`repairEndpointsStr` re-aims the endpoints along the cons-η paths). So the **entire**
remaining content of the E2 converse was exactly this one coinductive coherence — and
no *definitional reduction* over `Nu`/`unfold`/`dfix` provided it. That is what the
roadmap meant by "needs genuinely new content."

## 2. The principle — the one-level final-coalgebra constructor `nuCons` + its coinductive η

The missing primitive is **a one-level constructor for the final coalgebra**, the
exact categorical dual of an inductive datatype's constructor (and of the inner-Σ
`pairF`). `Nu`/`out`/`unfold` give the *observer* (`out`) and the *deep anamorphism*
(`unfold`, which rebuilds all observations corecursively — why its tail is only
bisimilar). What was absent was the constructor that reconstructs from **one** layer:

```
nuCons : (F : UF -> UF) -> El (F (Nu F)) -> El (Nu F)
```

shipped as the **4th member of the coind builtin group** (`store/coind.go`,
`NRoleNuCons`; group 3→4, a contained rehash, **no hash-format bump** — no new core
constructor). Two rules:

- **β (eval, `core/eval.go` `tryCoindIota`):** `out F (nuCons F x) ~> x`. Observing a
  one-level-constructed value returns the layer it was built from.
- **the COINDUCTIVE η (conversion, `core/conv.go` `convNuConsEta`):**
  ```
  nuCons F y  ≡  t      ⟺      y  ≡  out F t
  ```
  When exactly one side forces to a saturated `nuCons F y`, compare `y` against
  `out F (other side)`. This is the **negative-type η for coinductives** — the dual
  of `convSigmaEta` (`pairF (fst p)(snd p) ≡ p`) for the final coalgebra. It subsumes
  the eval form `nuCons F (out F s) ~> s`, and because `Str`'s functor is a `sigmaF`,
  it bridges the inner-Σ η so that

  ```
  consStr (head s) (tail s)  =  nuCons (pairF (head s)(tail s))  ≡  s     -- cons-η, BY REFL
  ```

  becomes **definitional** (ch69 `consEtaStr`). That is the coherence every route
  needed.

### Why this is sound and decidable (the metatheory)

- **Sound.** `nuCons` is the final coalgebra's constructor and `convNuConsEta` is its
  universal-property η, both true in the intended model: a coinductive value is
  determined by its single observation. It is the exact dual of the Σ pair + η the
  kernel already trusts.
- **Bounded / decidable.** `convNuConsEta` unfolds **one** `out` then compares
  structurally — it cannot recurse into itself unboundedly (one side strictly loses a
  `nuCons` head per step). Conversion stays decidable; the C4 firewall (normaliser
  cannot diverge) holds.
- **Confluent with β.** `out (nuCons (out s))` reduces either way to `out s` (via this
  η on the recursive position + inner-Σ η), and `out (nuCons x) ~> x` agrees. The two
  rules do not race.

## 3. The supporting cubical rules (also landed — the repair half)

`nuCons` makes cons-η definitional; the converse also needs to *move endpoints* over
`Str`, which is Kan composition over a **negative** (coinductive) type. Two more
kernel rules complete the substrate (all `core/eval.go`, ch69, refl-pinned + session
tests):

- **Kan-over-Nu** (`tryCoindIota`, `TestOutCommutesWithHcompOverNu`): `out` commutes
  with `hcomp` over a coinductive type —
  ```
  out F (hcomp (Nu F) φ u u0)  ~>  hcomp (F (Nu F)) φ (λi h. out F (u i h)) (out F u0)
  ```
  Because `Nu` is **negative** (not definitionally its unfolding — `out` is an
  observation, not a reduction — so the `gfix` type-unfold trick does NOT apply), the
  rule rides the **observation**: the composed stream stays neutral, but observing it
  fires the rule, lands in the structural former `F (Nu F)` (a `sigmaF` for streams),
  and the landed Σ-Kan rule splits it. So `head`/`tail` of an `hcomp (Str A)` are
  componentwise (`hcompHeadStr`/`hcompTailStr`, refl) — the global dual of the guarded
  `hcompHeadG`/`hcompTailG`. This is what makes `repairEndpointsStr` (path
  composition + endpoint-slide at the coinductive level, via the ambient `pathJ`)
  compute.
- **Final-coalgebra η** (`tryUnfoldEta`, `TestUnfoldEtaFinalCoalgebra`):
  `unfold F (Nu F) (out F) s ~> s` — the anamorphism into the final coalgebra *at its
  own carrier with coalgebra `out`* is the identity (both `id` and `unfold (out)` are
  coalgebra morphisms into the final coalgebra, hence equal). Fires only on the exact
  shape (carrier `Nu`-headed, coalgebra `out`-headed); confluent with the `out`-β.

`nuCons` (cons-η definitional) + Kan-over-Nu (repair computes) + Nu-η (rebuilt values
bottom out) are jointly the cubical coinduction principle. `nuCons` is the keystone:
with it, `converseFromConsEta`'s cons-η corrections are `preflF` (reflexivity), so the
repair is trivial and the converse is `refl`-clean.

## 4. The payoff — E2 converse CLOSED, computing, no postulate

```
bisimToPathStr A s t hp tp  :=  converseFromConsEta A s t hp tp (preflF (Str A) s) (preflF (Str A) t)
```

(ch69, body verbatim). `bisimilarity ⟹ path` for global streams, **computing, no
postulate, no dfix-observation, no clock-irrelevance axiom.** With ch66's forward
`headPath`/`tailPath` the correspondence

```
pathF (Str A) s t   ↔   (head s ≡ head t) × (tail s ≡ tail t)
```

is **complete** — the E2 correctness currency on the cubical stratum. It is already
**consumed by E3**: ch69/ch70 derive *trace bisimulation ⟹ trace equality*
(`bisimToPathStr (traceOf … s)(traceOf … t)`), the bisimulation machinery the
distributed track (R-CALC/R-PROTO) needs. The full listings gate is green (ch69, ch70
elaborate, check, and run).

## 5. Status reconciliation — what "PARKED at `isEquiv glue`" actually means now

The plan's E2 log and the index's R-COIND tail were written at cont.55, **before**
this closure, and are **stale**. The accurate status:

- **E2's correctness currency (bisimilarity-is-path) is CLOSED** via `nuCons`, global,
  computing, consumed by E3. This is the E2 *gate* — what every downstream consumer
  (R-CALC bisimulation, R-PROTO liveness, D5 OTP liveness, D7 transactional upgrade)
  needs.
- **`isEquiv glue` / the `Str ≃ ∀κ. gStr` bridge is an OPTIONAL ALTERNATE
  construction, NOT E2-gating.** It is the per-clock guarded route's attempt to prove
  a *stronger* statement — a full type equivalence between global `Nu`-streams and
  clock-quantified guarded streams. The global `nuCons` route **superseded** it for
  E2's purposes. Per Standing Rule 1 (no feature without a consumer) and Rule 5 (don't
  hoard the superseded), it is parked as an alternate route with no current consumer,
  to be revisited only if a future need wants the guarded↔global equivalence itself
  (not merely bisimilarity-is-path). The Glue-presentation scaffolding (`glueEquiv`,
  `gConverseGluePresented`) stands ready to consume `isEquiv glue` if that day comes.

So the roadmap's "dedicated multi-iteration research effort" **was completed** — the
answer was a *negative-type constructor + η on the final coalgebra* (a small, sound,
bounded kernel addition), not a large Glue/clock-irrelevance edifice. The lesson
(Savage): *coinduction's missing piece was the same shape as induction's — a
constructor and its η — only dualised; the cubical part (Kan-over-Nu) just lets you
slide endpoints once the constructor makes "a stream is its observations"
definitional.*

## 6. Decisions locked

- **`nuCons` + `convNuConsEta` are the principle; both ship** (landed: `store/coind.go`
  `NRoleNuCons`, `core/eval.go` `tryCoindIota`/`tryNuConsEta`/`tryUnfoldEta`,
  `core/conv.go` `convNuConsEta`). Sound (final-coalgebra constructor + universal-η),
  bounded (one `out` then structural compare), confluent with β. No hash-format bump.
- **Kan-over-Nu + Nu-η ship** as the repair/uniqueness companions (negative-type duals
  of the Σ-Kan + Σ-η rules). transp/comp-over-Nu are the duals, **deferred until a
  consumer needs them** (Standing Rule 1).
- **E2 currency is CLOSED; `isEquiv glue` is demoted to an optional alternate-bridge
  residue**, not a gating obligation. The DAG is updated to say so.
- **`Str` stays `Nu`-based globally.** `bisimToPathStr` does not redefine `Str` as
  `∀κ. gStr`; the guarded layer remains the κ-local shadow and an alternate route.

## 7. Status / pins

LANDED and green: `nuCons` (4th coind member), its β + coinductive η, Kan-over-Nu,
final-coalgebra η, `consStr`/`consEtaStr`/`tailConsStr`, `repairEndpointsStr`/
`ptransStr`/`symStr`, `converseFromConsEta`, and **`bisimToPathStr`** (ch69), consumed
by E3 trace equality (ch69/ch70). Tests: `internal/session/clock_test.go`
`TestNuConsBetaEta`, `TestOutCommutesWithHcompOverNu`, `TestUnfoldEtaFinalCoalgebra`;
`store/coind_test.go` (4-member group determinism); the `harness` listings gate
(ch69+ch70 elaborate/check/run). No postulate, no `dfix`-observation, no
clock-irrelevance axiom.

Sources / prior art (the cubical-coinduction technique this matches):
[Møgelberg–Veltri, *Bisimulation as path type for guarded recursive types* (POPL 2019)](https://doi.org/10.1145/3290317),
[Birkedal et al., *Guarded Cubical Type Theory*](https://arxiv.org/abs/1611.09263),
[Cohen–Coquand–Huber–Mörtberg, *Cubical Type Theory* (the Kan/`hcomp` substrate)](https://arxiv.org/abs/1611.02108),
[Vezzosi–Mörtberg–Abel, *Cubical Agda* (negative-type η for records/coinductives)](https://doi.org/10.1145/3341691).
