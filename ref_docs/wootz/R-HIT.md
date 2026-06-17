# R-HIT — Inner higher inductive types

## Problem (what's stuck/absent today, with file:line)

The inner cubical stratum can *form* fibrant types and *fill* them, but it cannot
yet *introduce* a fibrant type with higher-dimensional generators. Today:

- The fibrant universe `UF` is closed under exactly three formers: `fib` (small
  outer set), `piF` (Pi), and the inner identity `pathF`
  (`store/fib.go:41-44`, members 2/3/4). There is **no way to add a fibrant type
  whose constructors include paths.** The circle, suspensions, set-truncation,
  and "quotient-as-HIT" are all unstatable in the inner layer.
- The OUTER core *does* have a points-only inductive mechanism — datatypes by
  eliminators (`store/data.go:17-26`, `core.ElimSig`/`core.CtorSig`
  `core/eval.go:67-86`) — and a *propositional* quotient (`store/quot.go`, the
  v2 group). But neither is fibrant/cubical: a `data` type lives in `U`, its
  eliminator's ι-rule (`tryIota`, `core/eval.go:1317`) fires only on
  *point* constructors forced to a saturated head, and `Quot` identifies points
  only up to a `Prop`-valued relation with `qsound` UIP-collapsed
  (`store/quot.go:21-24`). There is no path constructor, no interaction with
  `hcomp`, no transport that respects a generator path.
- The Kan machinery that a HIT must hook into already exists and already
  dispatches by *head former*. `fibFormer` (`core/eval.go:1177-1196`) decomposes
  any fibrant code into `(role, args)`; `tryTransp`/`tryHcomp`/`tryComp`
  (`core/eval.go:1122/1204/1251`) switch on that role. **There is no `FRole` for a
  user-declared HIT, so transport/composition at a HIT is permanently stuck** —
  even the trivial `transp (λi. Circle) base ~> base` (a constant line, which
  *would* fire via the regularity rule `core/eval.go:1127`) is the only thing that
  works, and only by accident of constancy.
- The roadmap names this node exactly: **A9 "inner HIT kit + cubical
  computation" ⇐ R-HIT, A6** (`humble-humming-elephant.md:179`), gated into
  milestone **M3 "Full cubical inner"** (`:242`). The index records it as *not
  delivered* (`ref_docs/wootz/00-INDEX.md:212`).

The hard, genuinely-new problem this node must solve — beyond "add a constructor
list" — is the one the CHM paper is *about*: **`hcomp` does not reduce on a
generic HIT.** For `piF`/`pathF` the structural fills push the composite under the
former (`core/eval.go:1218`, `1276`). A HIT has no such structure to push under:
`hcomp Circle φ u u0` where `u0` is `base` and `u` is a partial `loop` has no
constructor to land on. CHM's answer — the spine of this whole design — is that
**`hcomp` at a HIT is itself retained as a new (formal) point constructor of the
HIT**, and the eliminator and `transp` are then *defined to compute through it*.
That is the schema. Everything else (the surface, the hashing, the dispatch) is
the established builtin-group pattern.

## Prior art (what the literature/other systems do; cite)

- **CHM — Coquand, Huber, Mörtberg, *On Higher Inductive Types in Cubical Type
  Theory* (LICS 2018).** The canonical schema this node follows. Two load-bearing
  ideas: (1) **`hcomp` is a constructor.** Because Agda/CCHM decompose Kan
  composition into `transp` + `hcomp`, and `hcomp` cannot reduce on a generic HIT,
  the HIT's set of canonical forms is *closed up* with formal `hcomp` cells — a
  point of `Circle` is `base`, `loop i`, *or* `hcomp [φ ↦ u] u0`. (2) **`transp`
  computes by pushing through every constructor, including the formal `hcomp`.**
  Transport over a *constant* HIT line is the identity on each generator; transport
  over a HIT *parameterised* by an interval (e.g. `Susp (A i)`) recurses into the
  parameter. The eliminator's computation rules are given for `base`, `loop`, AND
  the formal `hcomp` (the "Kan" branch of the elim).
  [paper](https://staff.math.su.se/anders.mortberg/papers/cubicalhits.pdf),
  [slides](https://staff.math.su.se/anders.mortberg/slides/MortbergLICS2018.pdf),
  [hcomp.pdf draft](https://simhu.github.io/misc/hcomp.pdf).

- **Cubical Agda** (Vezzosi–Mörtberg–Abel) ships exactly this as user-definable
  `data` with path constructors. The circle:
  ```agda
  data S¹ : Type where
    base : S¹
    loop : base ≡ base
  ```
  and the eliminator computes `elim base ~> b`, `elim (loop i) ~> ℓ i`
  (definitionally), with the `hcomp` branch handled by the implementation. The
  decomposition of `comp` into `transp ∘ hcomp` is *what makes the schema work*
  (per the docs).
  [Agda cubical docs](https://agda.readthedocs.io/en/v2.6.3/language/cubical.html),
  [agda/cubical HITs](https://github.com/agda/cubical/tree/master/Cubical/HITs).

- **cubicaltt `hcomptrans` branch** (Mörtberg): the experimental implementation
  where `hcomp`/`transp` are primitive and HITs carry formal `hcomp` cells —
  the literal source for the "hcomp-as-constructor" data layout.
  [github.com/mortberg/cubicaltt/tree/hcomptrans](https://github.com/mortberg/cubicaltt/tree/hcomptrans).

- **CHTT IV — Cavallo–Harper, *Computational Higher Type Theory IV: Inductive
  Types* / Angiuli–Favonia–Harper, "Higher Inductive Types in Cubical
  Computational Type Theory" (POPL 2019).** The *meaning-explanation* counterpart:
  a HIT is specified by its canonical values, the Kan operations *on* those
  values, and the eliminator, with `hcomp` cells as canonical. Confirms the schema
  is robust across the two cubical traditions (CCHM-syntactic and
  computational/cubical-Nuprl).
  [AFH paper](https://www.cs.cmu.edu/~rwh/papers/higher/paper.pdf),
  [CHTT IV arXiv:1801.01568](https://arxiv.org/pdf/1801.01568).

- **The general-schema work** (Cavallo–Harper "Cubical syntax for
  reflection-free…", and the schema in the cubicalhits paper §6) handles
  *parameterised* HITs (`Susp A`, pushouts), *recursive* path constructors, and
  *function applications in path endpoints*. For THIS substrate we deliberately
  take a **restricted, monomorphic-first schema** (see the containment note).

- **Local relevance.** The substrate already mirrors CCHM at every brick below
  this one: interval (`store/interval.go`), paths-as-interval-functions
  (`store/path.go`), face lattice + systems (`store/face.go`, `store/sys.go`),
  and `transp`/`hcomp`/`comp` decomposed exactly as CHM prescribe
  (`store/kan.go:5-16` — "comp, classically transp ∘ hcomp"). R-HIT is the
  payoff of that decomposition: it is the one construction that *requires*
  `hcomp` to be a separable primitive.

## Chosen approach for THIS substrate (concrete; respects containment)

### Containment decision (Thompson)

A HIT is a **per-declaration builtin group**, exactly like a `data` declaration
(`store/data.go`) but emitting *fibrant* members and registering a *new
`FRole`-like role* so the Kan switches can recurse on it. **No outer-core
constructor is added** (no hash-format bump): a HIT declaration is content, its
former/constructors are permanently-neutral heads, and its eliminator + the Kan
operations compute by ι-rules in the evaluator — the same three-part shape as
`data`, `quot`, `fib`, and `kan`. The outer MLTT/OTT core never sees a HIT; it
sees `UF` codes and `El` decodings, as it does for `piF`.

This is the v2-quotient move generalised: **a HIT *is* the cubical, higher-
dimensional replacement for `Quot`** (quotient-as-HIT is one of the four target
HITs, and it subsumes the propositional quotient when the relation is a
proposition). We keep `Quot` for the strict/UIP world and add HITs for the
fibrant world; they coexist the way the two equalities coexist
(`store/fib.go:25-31`).

### The schema (what a HIT declaration may contain)

A HIT declaration `D` over a (possibly empty) telescope of *fibrant* parameters
`Δ = (p₁ : UF, …)` introduces:

1. a **fibrant former** `D : Δ → UF` (a code, so `El (D p̄)` is the carrier);
2. **point constructors** `cᵢ : Γᵢ → El (D p̄)` (telescopes `Γᵢ` of fibrant
   arguments, possibly recursive in `D`, strictly positive);
3. **path constructors** `pⱼ : Γⱼ → (i : I) → El (D p̄)` with declared
   **boundary** `pⱼ Γⱼ i0 = lⱼ`, `pⱼ Γⱼ i1 = rⱼ` where `lⱼ, rⱼ` are point
   expressions over `Γⱼ` (the surface presents these as
   `pⱼ : pathF (D p̄) lⱼ rⱼ`, but the *cell* form `(i : I) → El (D p̄)` is the
   one the evaluator manipulates, mirroring `pabs`/`papp` `store/path.go:11-12`);
4. one **formal `hcomp` cell** per HIT (not user-written): `hcompD φ u u0` is a
   canonical inhabitant of `El (D p̄)` whenever `u0 : El (D p̄)` and `u` is a
   partial path on `φ` into `El (D p̄)`. This is the closure CHM require.

**Restriction (v1 of the kit, ready-to-build slice):** dimension-0 path
constructors only (one interval argument; `loop`, `merid a`, `surf`-free),
monomorphic or single-fibrant-parameter `Δ`, no `function-application-in-endpoint`
beyond a recursive constructor head. This covers all four named targets:

| HIT | points | path ctors | params |
|---|---|---|---|
| `Circle` | `base` | `loop : pathF Circle base base` | — |
| `Susp A` | `north`, `south` | `merid : (a:El A) → pathF (Susp A) north south` | `A : UF` |
| `Trunc₀ A` (set-trunc) | `inc : El A → El (Trunc₀ A)` | `squash : (x y : T)(p q : pathF T x y) → pathF (pathF T x y) p q` *(dim-2; see Risks)* | `A : UF` |
| `Quotient A R` | `qinc : El A → …` | `qrel : (a b)(r : El (R a b)) → pathF Q (qinc a) (qinc b)` | `A : UF`, `R : El A→El A→UF` |

The set-truncation's `squash` is a **dim-2** path constructor and is the one
member that pushes past the ready-to-build slice (Risks §). `Circle`, `Susp`, and
`Quotient` are dim-≤1 and are the ready slice.

### How the four ι-rules fire (the heart)

For a HIT `D` with point ctors `c̄`, path ctors `p̄`, and the formal `hcompD`:

**(a) Eliminator** `D-elim` (CHM §"elimination"). Layout mirrors `tryIota`
(`core/eval.go:1317`): `D-elim p̄ P methods… scrut`. The motive `P : El(D p̄) → UF`
(or `UF`-non-dep for recursion). One method per point ctor and per path ctor; the
path-ctor method is a *dependent path* over the point methods at the boundary
(`PathP`-shaped — see Interfaces). The ι-rule, by forcing `scrut`:
```
D-elim … (cᵢ ā)        ~>  methodᵢ ā (IHs)        -- as tryIota today
D-elim … (papp _ _ _ (pⱼ ā) i)  ~>  methodⱼ ā i    -- path ctor: the dependent-path method at i
D-elim … (hcompD φ u u0)        ~>  hcomp (P-line) φ
                                      (λ i h. D-elim … (u i h))
                                      (D-elim … u0)         -- the Kan/hcomp branch
```
The first rule is exactly the existing constructor ι. The second uses the path
boundary that `papp` already computes (`tryPathIota`, `core/eval.go:946-972`): at
`i0`/`i1` the method's endpoints definitionally equal the point methods, so the
elim is coherent on the nose. The third — eliminating a formal `hcomp` — is the
CHM rule: the eliminator *commutes with `hcomp`*, pushing the motive's
`comp`/`hcomp` over the recursively-eliminated box. (`P-line` is the fibrant
type-line `λi. P (hfill … i)`; for a non-dependent motive it is the constant `P`
and reduces to `hcomp (P) φ …`.)

**(b) `transp` over a HIT line** (`tryTransp`, add a `case FRoleHIT`). Two
sub-cases:
```
transp (λi. D)        u0  ~>  u0                  -- constant line: already fires via regularity (eval.go:1127)
transp (λi. D (q i))  u0  ~>  push through ctors  -- parameterised line:
   transp(λi.D(q i)) (cⱼ ā)            ~> cⱼ (transpᵃ ā)        -- transport the ctor args along q
   transp(λi.D(q i)) (papp…(pⱼ ā) r)  ~> pⱼ (transpᵃ ā) r       -- and respect the path ctor
   transp(λi.D(q i)) (hcompD φ u u0)  ~> comp (λi.D(q i)) φ …    -- transport-of-hcomp = comp (CHM)
```
For the **monomorphic** HITs (`Circle`) the line is always constant, so *only the
regularity rule is needed* — that is already in the engine. The parameterised
cases (`Susp (A i)`) need the argument transports; those are the
R-FILL-flavoured remainder (Risks).

**(c) `hcomp` at a HIT** (`tryHcomp`, add a `case FRoleHIT`). This is where the
formal constructor is *introduced*: when the face is proper and no structural rule
applies, `hcomp` does **not** stay stuck — it reduces to the formal cell.
```
hcomp (D p̄) ⊤ u u0       ~>  u i1 htop          -- total system (already: eval.go:1208)
hcomp (D p̄) φ u u0       ~>  hcompD φ u u0       -- PROPER face: build the formal constructor
```
The second rule is the new canonical form. `hcompD` is then a head the eliminator
and `transp` know how to consume (rules (a) and (b)). This is the single rule that
distinguishes a HIT from `piF`/`pathF`: instead of pushing the composite *under* a
former, it *records* the composite as a generator. (Soundness: the formal cell
satisfies the hcomp boundary `hcompD ⊤ u u0 = u i1 htop` and the cap
`hcompD φ u u0` restricted to `φ` equals `u i1` — checked by the elim's hcomp
branch; this is the HIT analogue of the path-ctor boundary.)

**(d) `comp` at a HIT** reuses the decomposition `comp = transp ∘ hcomp`
(`store/kan.go:15-16`): `comp (λi.D(q i)) φ u u0 ~> transp(λi.D(q i)) (hfillD …)`.
For constant lines it collapses to `hcomp` (rule c).

### Why this respects the substrate

- **`fibFormer` already returns `(role, args)` for any fibrant former**
  (`core/eval.go:1191`). A HIT former just needs `FibRoleOf` to report a new role
  family (`FRoleHIT` carrying the declaration's hashes), so the existing switches
  in `tryTransp`/`tryHcomp`/`tryComp` get a new arm — *no new dispatch
  machinery*, exactly as R-GLUE notes for `FRoleGlue` (`R-GLUE.md:36-38`).
- **The formal `hcompD` cell is a permanently-neutral head**, registered like a
  constructor (`store/data.go` `'c'` role). `tryHcomp` *produces* it; the elim and
  `transp` *consume* it via `spineParts`/`FibRoleOf` head inspection — the same
  pattern as `qin` (`tryQuotIota`, `core/eval.go:747-753`) and `ua`
  (`tryFibIota`, `core/eval.go:819-824`).
- **Inner-taint** (`internal/session/session.go:621`): every HIT member (former,
  ctors, elim, formal hcomp) is inner — they check but do not deploy until R-ERASE2
  gives inner Kan a runtime meaning (B5, `humble-humming-elephant.md:209-210`).
  Add the declared names to the `inner` set the way `pabs`/`papp` were added.

## Interfaces & signatures to add (Go + Rune surface as relevant)

### Go: a new builtin-group module `store/hit.go` (mirrors `data.go`)

```go
// HitDecl is the elaborated content of one inner-HIT declaration.
type HitDecl struct {
    Name      string
    Params    []core.Tm   // fibrant parameter telescope Δ (each : UF)
    PointTys  []core.Tm   // point-ctor types, former as Placeholder(0)
    PointSigs []core.CtorSig // arity + recursive positions (reuse Phase-4 sig)
    PathNames []string
    PathTys   []core.Tm   // path-ctor types: Γⱼ -> (i:I) -> El (D p̄), with
                          // declared boundary (lⱼ, rⱼ) as point exprs
    PathBnds  []core.HitBoundary // l/r endpoint terms per path ctor
    ElimTy    core.Tm     // generated eliminator type (PathP-aware motive)
}

// AddHit stores a HIT group and binds names: former=Name, points=PointNames,
// paths=PathNames, elim=Name+"Elim", and the FORMAL hcomp cell = Name+"#hcomp"
// (an internal name; not user-writable). Returns the role-tagged hashes.
func (s *Store) AddHit(d HitDecl) HitHashes

type HitHashes struct {
    Former core.Hash
    Points []core.Hash
    Paths  []core.Hash
    Hcomp  core.Hash   // the formal hcomp constructor
    Elim   core.Hash
}
```

### Go: extend `core.FibInfo` with a HIT role family (no new interface, one method)

```go
// In core/eval.go, alongside FibRole. A HIT is identified by its FORMER hash,
// which carries the whole declaration's role table.
type HitRole byte
const (
    HRoleNone HitRole = iota
    HRoleFormer    // the fibrant former D : Δ -> UF  (fibFormer returns this)
    HRolePoint     // a point constructor
    HRolePath      // a path constructor (a cell: Γ -> I -> El (D p̄))
    HRoleHcomp     // the formal hcomp cell
    HRoleElim      // the eliminator (computes by the HIT ι-rules)
)

type HitInfo interface {
    HitRoleOf(Hash) (role HitRole, former Hash, ok bool)
    HitSigOf(former Hash) (HitSig, bool)   // points/paths/elim layout for ι
    HitHash(former Hash, role HitRole, idx int) (Hash, bool) // construct sub-terms
}

// HitSig drives the eliminator + Kan ι-rules (parallel to ElimSig).
type HitSig struct {
    Former    Hash
    NumParams int
    Points    []CtorSig          // reuse Phase-4 ctor sigs
    Paths     []HitPathSig        // dim, boundary point-ctor indices
    Hcomp     Hash
    Elim      Hash
}
type HitPathSig struct{ Arity, Dim int; L, R HitEndpoint }
```

Add `Hit HitInfo` to the `Machine` struct (`core/eval.go:27-60`), `store.Store`
implements it, and `tryRules` (`core/eval.go:645`) gains a HIT arm that dispatches
`HRoleElim → tryHitIota` and (inside `tryTransp`/`tryHcomp`/`tryComp`) a
`FRoleHIT`-equivalent former branch.

### Go: the three new evaluator functions (mirror existing ι-firers)

```go
func (m *Machine) tryHitIota(sig HitSig, args []Val, spine Neutral) (Val, bool) // rule (a)
// plus a case in tryHcomp that emits the formal hcomp cell (rule c),
// and cases in tryTransp/tryComp that push through ctors / build comp (rules b,d).
```

### Rune surface (extends `data`, GRAMMAR.md territory)

```
hit Circle : UF is
  base : Circle
  loop : pathF Circle base base
end

hit Susp (A : UF) : UF is
  north : Susp A
  south : Susp A
  merid : (a : El A) -> pathF (Susp A) north south
end

hit Quotient (A : UF) (R : El A -> El A -> UF) : UF is
  qinc : El A -> Quotient A R
  qrel : (a b : El A) -> (r : El (R a b)) -> pathF (Quotient A R) (qinc a) (qinc b)
end
```

The eliminator is generated as `CircleElim` / `SuspElim` / `QuotientElim`, with a
`PathP`-typed method for each path constructor (a *dependent* path of methods over
the boundary point-methods).

## Worked micro-example (the teachable artifact)

**The circle, and "the loop is not refl".** The Savage artifact: a learner who
has met `pathF`/`pabs`/`papp` (ch18) meets the first type with a *non-trivial*
path, and *computes* with it.

```
hit Circle : UF is
  base : Circle
  loop : pathF Circle base base
end

-- the non-dependent eliminator (recursor): map out of the circle
-- CircleElim : (P : UF) -> (b : El P) -> (l : pathF P b b) -> El Circle -> El P
-- double : Circle -> Circle, sending loop to loop ∘ loop
double : El Circle -> El Circle is
  CircleElim Circle base (concatLoop loop loop)
end
```

What computes, on the nose, by the ι-rules:

1. `papp Circle base base loop i0  ~>  base`  (path-ctor boundary, *already*
   `tryPathIota` `eval.go:966`).
2. `CircleElim P b l base            ~>  b`     (rule (a), point branch — identical
   to a `data` eliminator firing, `tryIota`).
3. `papp P b b (CircleElim … (papp … loop ·)) i  ~>  l i`  — eliminating along the
   loop runs the loop-method `l`. Concretely `CircleElim P b l (loop i)` reduces
   to `l i` (rule (a), path branch).
4. `transp (λ_. Circle) base        ~>  base`   — transport over the *constant*
   circle line is the identity, and this *already fires* via the regularity rule
   (`eval.go:1127`): the line does not mention the sentinel, so `transp` returns
   its argument. **The circle's `transp` needs no new code** — the teachable
   first HIT lands on machinery that already exists.
5. `hcomp Circle φ u base` on a *proper* `φ` ~> `Circle#hcomp φ u base` — the
   formal cell (rule (c)). The learner sees that "you cannot always compute a
   filler down to `base` or `loop` — sometimes the answer *is* the filling," which
   is precisely the conceptual content of a HIT.

The payoff sentence for the curriculum: *`loop` is a path `base ≡ base` that the
checker will not equate with `refl base`* — because `loop` is a distinct neutral
head from `preflF` (the same coexistence argument as `ua ≠ ureflU`,
`store/fib.go:25-31`), and the eliminator that sends `loop` to `concatLoop loop
loop` *computes*, witnessing that the circle "really has" a loop. This is the
first place a Wootz learner touches genuine homotopy and sees it run.

## Risks / open sub-questions

- **[research] Higher-dimensional path constructors (`squash`, the torus
  `surf`).** The ready slice is dim-≤1 (`Circle`, `Susp`, `Quotient`). Set/prop
  truncation needs a dim-2 constructor, whose elim method is a *square* of methods
  and whose `transp`/`hcomp` interactions need the full general schema (CHM §6,
  the parameterised/recursive cases). Recommend: ship dim-≤1 first; truncations
  arrive with the dim-n generalisation. Until then, **propositional truncation is
  still available via Prop's impredicativity** (the v2 note,
  CLAUDE.md "Prop's impredicativity already contains propositional truncation"),
  and *set*-truncation is the only genuinely-blocked target.

- **[research] Parameterised-line transport** (`transp (λi. Susp (A i))`). Pushing
  `transp` through `merid a` needs to transport `a` along the parameter line and
  *fill* the meridian — this is the R-FILL/`transpFill` flavour the rest of §F is
  blocked on (`R-FILL.md`). Monomorphic HITs (`Circle`, and `Quotient`/`Susp` at a
  *constant* parameter) are ready; varying-parameter transport is the labelled
  remainder, exactly as varying-domain `piF` is for `tryTransp` today
  (`eval.go:1147-1148`).

- **[research] The `hcomp`-cell coherence checks.** The formal cell must satisfy
  its boundary (`hcompD ⊤ u u0 = u i1 htop`) and cap (`hcompD φ u u0|φ = u i1`)
  *definitionally*, and the elim's hcomp branch must agree on `φ`. This is the
  CHM "homogeneous composition structure on the HIT" obligation; getting it wrong
  breaks canonicity. Property-tested (below), not assumed.

- **[open] System overlap-agreement** bites here as it does everywhere
  (`PARKING-LOT.md`, C-OVERLAP): the path-ctor boundary and the hcomp cap both
  require partial elements to *agree on overlaps*, and this engine's proof
  irrelevance does not cover arbitrary `holds φ` proofs in conversion
  (CLAUDE.md §F-structural FINDING). The dim-≤1 slice mostly avoids multi-branch
  systems; the dim-2 and parameterised cases will need A8's overlap mechanism
  (`humble-humming-elephant.md:178`).

- **[open] Strict positivity for path constructors.** Phase-4 positivity
  (`store/data.go` declaration-time check) must extend to path-ctor endpoints
  (a recursive `D` may appear in `lⱼ`/`rⱼ` only strictly positively). Reuse the
  existing checker, extended to the cell form.

- **[decision] Eliminator vs. pattern-matching surface.** Stay
  eliminator-only (Thompson + the "totality is by construction" rule,
  CLAUDE.md Phase 4): the generated `DElim` is the sole recursion principle, no
  termination checker, mirroring `data`. No `split`/copattern surface.

## Test/gate plan

- **harness/cubical_props_test.go extension** (the existing cubical property
  file): for each shipped HIT, *closed-term canonicity* — every closed `El (D p̄)`
  term normalises to a generator form (`base`, `loop i`, `cᵢ ā`, or `D#hcomp φ u
  u0`), never a stuck non-canonical neutral. This is the M3 canonicity gate
  (`humble-humming-elephant.md:288`).
- **Eliminator ι property:** `D-elim … (cᵢ ā) ≡ methodᵢ ā` and
  `CircleElim P b l (loop i) ≡ l i` hold by conversion, on random method choices
  (rapid generators), parallel to the Phase-4 datatype ι tests.
- **Boundary preservation:** `papp … (loop) i0 ≡ base`, `… i1 ≡ base`;
  `merid a i0 ≡ north`, `… i1 ≡ south` — by conversion, for any (even neutral)
  args, reusing the `tryPathIota` boundary path.
- **Regularity pin:** `transp (λ_. Circle) base ≡ base` (no new code — a
  regression that the constant-line rule still fires through a HIT former).
- **hcomp-cell distinctness:** `Circle#hcomp φ u u0` is a distinct neutral the
  strict `Eq` neither identifies with nor refutes against `base` (the coexistence
  pin, parallel to `ua ≠ ureflU` in `fib_test.go`).
- **Frame Lemma (X1):** the proof-cache key captures HIT unfolding — a certificate
  mentioning a HIT member's hash travels and re-checks; the new ι-rules log their
  forced scrutinees through `Force` as every other ι does.
- **Listing:** a new `listings/` chapter (the M3 furnace artifact) — the circle,
  `double`, and "loop is not refl" — elaborates and checks (not deploys; inner-
  tainted).

## Unblocks (which implement nodes, and what they still need)

- **A9 "inner HIT kit + cubical computation"** — this node *is* A9's design. Ready
  to build for the **dim-≤1, monomorphic/constant-parameter slice** (Circle,
  Quotient, constant-`A` Susp): former + point/path ctors + eliminator ι + the
  formal-hcomp constructor + the constant-line `transp` (which already fires). A9
  still needs, before the *full* kit: R-FILL (`transpFill`) for varying-parameter
  transport, A8 (overlap) for multi-branch path-ctor systems, and the dim-n
  schema generalisation for truncations.
- **M3 "Full cubical inner"** (`humble-humming-elephant.md:242`) — R-HIT is the
  last research element M3 names (with A10/R-UFH *only if needed*). Delivering the
  ready slice gives M3 its teachable HIT artifact.
- **D-track / stdlib quotients** — `Quotient`-as-HIT gives the fibrant quotient
  the strict `Quot` cannot (effective, path-respecting), feeding any stdlib type
  that needs a higher quotient.
- **Depends on A6 (R-GLUE)** per the DAG (`:179`), but only *softly*: the
  monomorphic HIT kit needs `transp`/`hcomp` (phase 3, done) and the path/face/sys
  groups (done), **not** Glue. The A6 dependency is real for *univalent* HITs
  (transporting a HIT *along a `ua` path*); the base kit can land before A6.
  **Honest status: the dim-≤1 monomorphic kit is ready-to-build now; the full
  schema (truncations, parameterised transport) stays research, gated on R-FILL +
  A8 + the dim-n generalisation.**

## Status

**DELIVERED — A9 total at its consumer-driven scope (2026-06-16).** The kit ships
four HITs — `Circle`, `Susp A`, `Quotient A R`, `Trunc0 A` — each with its former,
point/path constructors, the formal `hcomp` cell, and BOTH a recursor and a
dependent inductor, all computing by ι. What now computes on all generator shapes:
the point/path-ctor β and boundaries (`tryHitIota`/`trySuspIota`/`tryQuotHitIota`/
`tryTruncIota`), the recursor/inductor **commuting with the formal `hcomp` cell**
(CHM hcomp-as-constructor), **varying-parameter transport** over `Susp`/`Quotient`
lines for the point ctors (re-index) and path ctors (transport the carried element
+ rebuild), and — the final filler, closed 2026-06-16 — **transport of the formal
`hcomp` cell itself** (`transpHitHcompCell`: `transp` commutes with the cell because
its system is transport-dimension-constant; ch196, `internal/session/transphcompcell_test.go`).
The set-truncation's dim-2 `squash` square also computes (`tryTruncIota`; ch98) — the
first fully-computing dim-2 HIT. So the suspension/quotient Kan structure is TOTAL:
every closed `El (D p)` term's transport reduces to a generator.

**Parked (no consumer, Standing Rule 1):** `comp` over a HIT line as a *primitive*
canonical form (not needed — transp-over-the-cell lands on an `hcomp` cell directly,
and `comp = transp ∘ hcomp` is available where a varying-line box is genuinely
written); multi-branch path-ctor systems beyond what proof-irrelevance discharges
(gated on A8/C-OVERLAP, no listing exercises them); and A10/R-UFH (a fibrant
universe hierarchy — `if needed`, condition unmet; see `PARKING-LOT.md`).
