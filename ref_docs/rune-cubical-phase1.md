# §F Phase 1 — the cubical interval (plan)

The frontier (ref_docs/rune-v3-design.md §F) makes inner univalence *compute* by
making the inner stratum **cubical**: interval, Kan operations, Glue — isolated
to the inner layer, the outer OTT core untouched. Phase 1 lays the substrate:
the **interval as a De Morgan algebra**, shipped the way quotients (v2) and the
fibrant layer (v3) were — a content-addressed BUILTIN GROUP, not new core syntax.

## Members (store/interval.go)

A six-member group, separate hash space from the fib group (so ch09/ch10 hashes
are untouched):

```
I    : U                 the interval (a neutral type former; morally a pretype)
i0   : I                 endpoint 0
i1   : I                 endpoint 1
ineg : I -> I            reversal
imin : I -> I -> I       the ∧ connection (min)
imax : I -> I -> I       the ∨ connection (max)
```

`I : U` is the pragmatic choice — no new sort. The interval is morally a
*pretype* (not fibrant: you cannot transport over `I`), but nothing in phase 1
needs that distinction enforced; the fibrancy/pretype split is a phase-3 concern
(Kan operations need to know `I` is not fibrant). `i0`/`i1` are distinct neutrals
(like `ua` vs `ureflU`): the strict `Eq` can neither prove `i0 = i1` nor refute
it, and there is no interval eliminator — you cannot case on `I`. That is the
point: the interval is driven by Kan operations later, not by pattern matching.

## ι-rules (core/eval.go, tryIntervalIota)

The De Morgan algebra computes on endpoints; a neutral interval term stays stuck:

```
ineg i0      ~> i1        ineg i1      ~> i0
imin i0 _    ~> i0        imin i1 j    ~> j
imin _ i0    ~> i0        imin i j i1  ~> i        (force 2nd arg when 1st is stuck)
imax i0 j    ~> j         imax i1 _    ~> i1
imax _ i0    ~> i         imax _ i1    ~> i1
```

Wired exactly like the quotient/fib ι-rules: a new `core.IntervalInfo` interface
(`IntervalRoleOf(Hash) IntervalRole`), a `Machine.Iv` field, dispatch added to
`rigidHead` and `tryRules`. Scrutinees are forced (logged), so the proof-cache
seam is the one ι-reduction has always used. Idempotence/De Morgan LAWS
(`ineg (ineg i) = i`, etc.) are not definitional on neutrals — they become
provable once paths exist (phase 2) or are postulated; phase 1 only ships the
endpoint reductions.

## Wiring

- `store/interval.go`: `AddInterval`, `IntervalRoleOf`, `IntervalHashes`,
  `IntervalNames`; `Store.iv` field.
- `core/eval.go`: interface + enum + `Machine.Iv` + `rigidHead`/`tryRules` hooks
  + `tryIntervalIota`.
- `internal/session`: register ambiently in `Reset` (like `AddQuot`/`AddFib`),
  set `el.M.Iv`/`m.Iv = s.st`.
- `elaborate/data.go`: thread `inner.M.Iv`.
- codegen: `I` is a type former → erases to the unit token (a typeRef); the
  interval value members are inner-tainted (no runtime meaning yet, exactly like
  the fibrant value members) — phase 5 gives paths a shadow.

No hash-format bump: no new core constructor exists (same as quotients/fib).

## Gate

A `ch17`-style listing exercises the algebra (`ineg i0` ↝ `i1`, `imin i1 i0` ↝
`i0`, `imax i0 i1` ↝ `i1`, …) and checks; a Go test pins the endpoint
reductions. This validates the thesis that the cubical machinery stays
*contained* in a builtin group before phase 3 commits to Kan composition.

---

# §F Phase 2 — paths as interval functions (plan)

Phase 1 gave the interval. Phase 2 gives paths COMPUTATIONAL content: an inner
path `p : El (pathF A x y)` is morally a function `I -> El A` pinned at the
endpoints (`p i0 = x`, `p i1 = y`). Two operations make that real, shipped as a
fifth builtin group (store/path.go) on its own hash space — the fib group (and
ch09/ch10 hashes) stay untouched:

```
pabs : (A : UF) -> (f : (i : I) -> El A) -> El (pathF A (f i0) (f i1))   path abstraction
papp : (A : UF) -> (x y : El A) -> El (pathF A x y) -> (i : I) -> El A   path application
```

`pabs` is a canonical path intro (neutral, like a constructor); `papp` COMPUTES
(core.PathInfo / tryPathIota):

```
papp A _ _ (pabs A' f)    i   ~>  f i        (β: applying an abstraction)
papp A x x (preflF A' x') i   ~>  x'         (reflexivity is the constant path)
papp A x y  p             i0  ~>  x          (boundary: a path at i0 is its left end)
papp A x y  p             i1  ~>  y          (boundary: a path at i1 is its right end)
```

The boundary rules fire for ANY path `p` (even a neutral variable) — that is the
defining property of the path type, definitional. `β` and the refl rule are
checked first (most informative when `i` is itself neutral). Composing with
phase 1's connections (`imin`/`imax`) inside a `pabs` body builds squares and
the connection paths the Kan operations (phase 3) will consume.

What still does NOT compute: `pathJ` over a non-refl path (it needs Kan
transport — phase 3), and the De Morgan/path LAWS as definitional equalities
(they are paths, provable, not reductions). Phase 2 ships intro/elim with β and
boundary, not yet transport. pabs/papp are inner-tainted — they elaborate and
check but do not deploy (the runtime shadow for paths is phase 5).

Gate: listing ch18 exercises β, refl-as-constant, the boundary equations, and a
connection path; a Go test pins the reductions.

# §F Phase 3 — Kan operations (plan)

Phases 1–2 gave the interval and paths. Phase 3 gives fibrant types their
COMPUTATIONAL FILLING — the operations that make transport and composition
reduce, the substrate `pathJ`-over-a-general-path (and, at phase 4, `ua`) will
stand on. Five builtin groups, each on its own hash space (fib/interval/path/
ch09–ch18 hashes untouched), gated in sequence. The interval already carries De
Morgan reversal + connections, so this commits to CCHM-style Kan, not Cartesian.

## 3a — the face lattice (store/face.go, seven members)

The cofibrations: `F : U`, atomic constraints `ieq0`/`ieq1 : I -> F` (i = 0,
i = 1), and the bounded lattice `fand`/`for : F -> F -> F`, `ftop`/`fbot : F`.
ι-rules (core.FaceInfo, tryFaceIota), the De Morgan algebra's mirror at the face
level: `ieq0 i0 ~> ftop`, `ieq0 i1 ~> fbot` (ieq1 dual); `fand ftop φ ~> φ`,
`fand fbot _ ~> fbot`; `for fbot φ ~> φ`, `for ftop _ ~> ftop` (+ symmetric).
Stuck on neutral faces. Pure substrate, no φ-semantics yet. Listing ch19.

## 3b — partial elements & systems (store/sys.go, five members)

WHEN a face holds, modelled by a proof-irrelevant `holds : F -> Prop` with
canonical intros `htop : holds ftop`, `hand`/`horl`/`horr` (permanently neutral
heads, no ι of their own). A partial element of A on φ is just `holds φ -> El A`
(a Pi — no new former); a system dispatches on `holds (for …)`. Choosing
`holds : F -> Prop` over an interval-substitution restriction reuses the existing
proof-irrelevant Prop/Eq stratum: two values on an overlapping face are
definitionally equal because the PROOFS of holding are (proof-irrelevance), so
overlap-agreement is discharged for free at the canonical level. Computation
happens not here but AT the Kan operations, which inspect the face and feed the
system `htop` when it is ⊤. There is no intro for `holds fbot`: ⊥ never holds.
Listing ch20.

## 3c — transp (store/kan.go)

`transp : (A : I -> UF) -> El (A i0) -> El (A i1)` — transport a point along a
LINE of fibrant types. Phase 3 ships REGULARITY (core.KanInfo, tryTransp): a
type-line that does not depend on i transports by the identity, `transp A a0 ~>
a0`. Detecting "does not depend on i" is the genuine new NbE machinery on a
content-addressed substrate: rather than thread an interval-level counter through
the Machine, A is applied to a fixed sentinel ref (`kanFreshSentinel`, never a
real BLAKE3 hash) standing for a fresh interval point, and the resulting `UF`
value is occurrence-scanned (`mentionsRefVal` — structural, no unfold forcing,
closures entered under VVar(0)). Absent ⇒ the line is constant ⇒ identity;
present ⇒ stuck. Sound: the sentinel cannot collide, and a genuinely varying line
stays conservatively stuck. Listing ch21.

## 3d — hcomp (store/kan.go)

`hcomp : (A : UF) -> (φ : F) -> (I -> holds φ -> El A) -> El A -> El A` — fill a
box whose sides are a partial path on φ and whose floor is u0, returning the lid.
Ships the TOTAL-system rule (tryHcomp): when φ forces to ⊤ (faceConst) the
partial path is defined everywhere, so the lid is the system at i1 — `hcomp A ⊤
u u0 ~> u i1 htop` (the ⊤-proof is `htop` from 3b, by SysHash(SRoleTop)). A
proper (neutral) face leaves hcomp stuck. Listing ch22.

## 3e — comp (store/kan.go)

`comp : (A : I -> UF) -> (φ : F) -> ((i:I) -> holds φ -> El (A i)) -> El (A i0)
-> El (A i1)` — the HETEROGENEOUS composite, a box-fill along a varying type-line
(classically transp ∘ hcomp). Ships two honest endpoints (tryComp): the
TOTAL-system rule, identical to hcomp's, `comp A ⊤ u u0 ~> u i1 htop` (independent
of A); and the DEGENERATE rule, `comp (const A) ⊥ u u0 ~> u0` — an empty system
on a constant line collapses comp to the identity transport (regularity reused
via the same freshness sentinel). A proper face, or a varying line under ⊥, stays
stuck. Listing ch22.

## The labelled remainder

Endpoints compute; the INTERIOR does not yet. Still stuck (honestly): the
STRUCTURAL fills — transp/comp over `piF`/`pathF` (domain backward, codomain
forward, conjugating with the argument's transport; comp in the base for paths),
and homogeneous filling on a PROPER face by recursion on A's former — which need
`transpFill` and the full CCHM formulas; `pathJ`-via-comp on a general path
(lifting the phase-2 preflF-only ι-rule); and transport through `ua` (the
Glue-free special case is the bridge into phase 4). No hash-format bump (no new
core constructor). All Kan members are inner-tainted — they elaborate and check
but do not deploy.

Gate: listings ch19–ch22 (faces, systems, transp regularity, hcomp/comp
endpoints), each refl-certified; Go tests in store/ and internal/session/ pin the
group determinism, roles, and reductions.

## Phase 3, the structural interior — the `piF` slice

The endpoint rules compute on degenerate input; the interior makes the Kan
operations recurse on a type's HEAD FORMER. This increment lands the slice that
is sound WITHOUT the deep `transpFill` machinery: composition/transport over a
FUNCTION type (`piF`), which pushes UNDER the binder.

Enabler. The structural rules must CONSTRUCT fibrant sub-terms (`El (Fam x)`)
and recurse (`transp`/`hcomp`/`comp` on a sub-line), so `core.FibInfo` and
`core.KanInfo` gain reverse lookups `FibHash(role)` / `KanHash(role)` — the same
shape as `IntervalHash`/`SysHash`/`FaceHash`, implemented on `store.Store`. A
helper `fibFormer(v)` forces a fibrant code and returns its former role +
arguments; a type-line's head former is read by probing it at the freshness
sentinel (`fibFormer(m.Apply(A, sentinel))`). Sub-lines are rebuilt as Go
closures over the original `A` that decompose the former at each interval point —
the piF head is i-stable, so this is well-defined.

Three rules (core/eval.go), each firing only AFTER the endpoint rules
short-circuit:

    transp (λi. piF D (Fam i)) f  ~>  λx. transp (λi. Fam i x) (f x)   [D constant]
    hcomp  (piF P Fam) φ u u0     ~>  λx. hcomp (Fam x) φ (λi h. u i h x) (u0 x)
    comp   (λi. piF P (Fam i)) φ u u0 ~> λx. comp (λi. Fam i x) φ (λi h. u i h x) (u0 x)  [P constant]

The CONSTANT-DOMAIN restriction (checked by scanning the probed domain for the
sentinel) is what makes transp/comp `transpFill`-free: the argument is valid at
both ends, so it needs no backward fill. hcomp is homogeneous (base fixed), so
it pushes for ANY face. Soundness pin: a stuck top-level transport η-differs
from a lambda, so an `Eq` to the explicit pushed form (proven by `refl`)
certifies the push fired — that is the ch23 gate.

Substrate hardening. Two rapid properties (harness/cubical_props_test.go): every
CLOSED interval term normalizes to an endpoint, every CLOSED face term to ⊤/⊥ —
the confluence-to-canonical the refl listings only sample, and the invariant
`faceConst` relies on. Plus transp constancy-boundary pins
(internal/session/structural_test.go): the probe sees a constant line through a
β-redex, and a varying `pathF` line stays STUCK (no over-firing to the identity).

Still the labelled remainder (honest-stuck): `transpFill` (transport i→j) and
with it varying-domain `piF` (argument conjugation), `transp`/`comp` over
`pathF` (composition in the base — even a constant base bottoms out in
`hcomp` on a proper face), `hcomp` on a proper face for non-`piF` formers,
`pathJ`-via-comp on a general path, and transport through `ua` (phase 4).

Finding. System overlap-agreement is NOT definitional on this engine: proof
irrelevance equates Eq/refl/cast proofs at the canonical level (UIP), not
arbitrary `Prop` inhabitants in conversion, so `part h1 ≡ part h2` for two
proofs of `holds φ` does not check. The 3b design assumed this would discharge a
system's overlap obligation for free; it does not. Parked (PARKING-LOT.md) until
a multi-branch system needs it.

Gate (this increment): listing ch23 (the three piF pushes, η-certified); store
round-trip tests for `FibHash`/`KanHash`; the two rapid properties; the
constancy-boundary session pins. Hashes of ch09/ch10/ch17–ch22 unchanged — the
reverse lookups add no members and there is no hash-format bump.
