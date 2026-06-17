# R-BOX — Homogeneous filling on a proper face (open box)

Status: **ready-to-build for `piF` (any face) and the ⊥/⊤-degenerate `fib`/`pathF`
cases; ready-to-build for proper single-atom `pathF` once the overlap-via-htop
analysis below is implemented; research-gated for proper multi-branch `for`-faces
(C-OVERLAP / A8), for `sigmaF` (needs R-SIGMA + R-FILL's `hfill`/`comp`), and for
`Glue` (needs R-GLUE).**

R-BOX is the **homogeneous** keystone — genuine open-box filling `hcomp` by
recursion on a type's head former. It is the twin of **R-FILL** (the heterogeneous
`transp`/`comp`/`transpFill` keystone). Read R-FILL alongside this: `comp` is
`transpFill` (R-FILL) **then** `hcomp` (this node), and several formers
(`pathF`, `sigmaF`, `Glue`) bottom out in `hcomp` even when their type-LINE is
constant, so R-BOX is needed even on the homogeneous slice R-FILL can already drive.

The substrate already committed to **CCHM** (De Morgan interval `ineg`/`imin`/
`imax`, `ref_docs/rune-cubical-phase1.md:120`), so every formula below is the CCHM
one, not Cartesian (cooltt/ABCFHL `hcom`).

---

## Problem (what's stuck/absent today, with file:line)

Today exactly two `hcomp` cases compute, both *degenerate or structurally
homogeneous*:

1. **total-system** `hcomp A ⊤ u u0 ~> u i1 htop`
   (`core/eval.go:1208`, `tryHcomp`, the `faceConst(args[1]) == CRoleTop` branch).
   This is the boundary equation, fired only when the *whole* boundary is
   constrained — it never actually *fills* an open box.
2. **piF push** `hcomp (piF P Fam) φ u u0 ~> λx. hcomp (Fam x) φ (λi h. u i h x) (u0 x)`
   (`core/eval.go:1222`–`1236`). Sound *for any face* because the function former
   is structurally homogeneous: the base `piF P Fam` does not move in the filling
   direction, so the push just commutes `hcomp` under the binder. It recurses on
   the former (good) but never confronts the open-box obstacle (the result is
   another `hcomp`, pushed inward, not a filled value).

Everything else is honestly stuck. On a **proper (neutral) face** φ — the *actual*
open-box scenario, where `u` is defined on part of the boundary and `hcomp` must
genuinely fill the missing lid — there is **no rule** for:

- `hcomp (fib X) φ u u0` — composition inside an embedded outer (strict) type.
  `fibFormer` (`core/eval.go:1177`) returns `FRoleFib`, but `tryHcomp` only
  matches `FRolePiF` (`core/eval.go:1223`), so it falls through to stuck.
- `hcomp (pathF A a b) φ u u0` — composition of paths. **The workhorse**: every
  `pathJ`-via-comp (A4), every law-as-path, and the base step of A3 bottoms out
  here. No path-former branch exists.
- `hcomp (sigmaF A B) φ u u0` — composition in a dependent pair. `sigmaF` does not
  exist yet (**R-SIGMA**); specified here so R-SIGMA lands it in the same
  increment.
- `hcomp (Glue T A φ' e) φ u u0` — `Glue` does not exist yet (**R-GLUE**); the
  hardest CCHM formula, specified here as the research target it is.

**The contained finding that bounds the whole node** (PARKING-LOT.md, "System
overlap-agreement is not definitional"; `internal/session/structural_test.go:8`):
proof irrelevance on this engine equates `Eq`/`refl`/`cast` proofs at the
canonical level (UIP), **not** arbitrary `Prop` inhabitants in conversion. So for
two `holds φ` proofs `h1`, `h2`, the system values `u i h1` and `u i h2` are **not
definitionally equal** — only their *types* are. The total-system rule sidesteps
this by feeding the *single* canonical proof `htop` (there is no other inhabitant
of `holds ftop`). Every proper-face fill that must reconcile a system value on an
*overlap* of two faces re-encounters this. §"The overlap obstacle, precisely"
below shows which per-former overlaps **collapse to `htop`-fed branches** (and are
therefore buildable now) and which need a genuine multi-proof reconciliation
(gated on **C-OVERLAP**/A8).

---

## Prior art (what the literature/other systems do; cite)

The open-box / homogeneous-composition semantics is **CCHM** (Cohen–Coquand–
Huber–Mörtberg, *Cubical Type Theory: a constructive interpretation of the
univalence axiom*, arXiv:1611.02108). In CCHM:

- `hcomp` (CCHM writes `comp` for the homogeneous case over a constant line, and a
  separate `hfill` for the filler) is **defined per type former** — there is a
  case for Π, Σ, Path, the universe, `Glue`, and the data/`fib` carriers.
- The heterogeneous `comp` is **derived**:
  `comp A φ u u0 = hcomp (A i1) φ (λj h. transpFill (λi. A i) j (u j h)) (transp A ⊥ u0)`
  — i.e. **R-FILL's `comp` calls R-BOX's `hcomp`**. This is why R-BOX is specified
  independently and why `pathF`/`sigmaF`/`Glue` fills are needed even when the
  type-line is constant.

Per-system references and what each contributes:

- **cubicaltt** (mortberg, https://github.com/mortberg/cubicaltt) — the reference
  reductions. `comp` for Σ is **elementwise**; `comp` for Π **pushes under the
  binder** (already done here); `comp` for Path **adds the two endpoint
  constraints to the system** via `for`/`ieq0`/`ieq1`; `comp` for Glue is the long
  unglue/glue dance (`examples/univalence.ctt`). The CCHM hands-on intro
  (https://homotopytypetheory.org/2017/09/16/a-hands-on-introduction-to-cubicaltt/)
  walks the Path/Σ cases concretely.
- **Cubical Agda** docs (https://agda.readthedocs.io/en/latest/language/cubical.html)
  — `hcomp`/`hfill`: `u0` is the floor, `u` the open-box sides, `hcomp` the missing
  lid; `hfill` gives the interior (`hfill … i0 = u0`, `hfill … i1 = hcomp …`). "For
  Σ-types it is computed elementwise" confirms the per-former recursion shape.
- **cooltt / ABCFHL** (Cartesian; *not* the door here — no De Morgan connections)
  — its `hcom`/`coe` split is the same architecture (`hcom` = our `hcomp`,
  homogeneous; `coe` = our `transp`, the line). Useful as a cross-check on the
  boundary equations, and its `hcom` for Σ/Path is identical up to the
  endpoint-formula difference.
- **"Automating Boundary Filling in Cubical Type Theories"** (Mörtberg et al.,
  arXiv:2402.12169) — confirms the boundary-coherence equations are the hard part
  and motivates restricting to faces where overlap is trivial first.
- **Simon Huber, *Canonicity for Cubical Type Theory*** (arXiv:1607.04156) and
  Coquand–Huber–Sattler, *Homotopy Canonicity* (arXiv:1902.06572) — canonicity
  holds for **non-regular** CCHM (the C-REG resolution, owned by R-FILL): the X2
  canonicity gate is attainable without Swan's fix, so R-BOX's `hcomp` table need
  not be regularized.
- **C-REG** (roadmap clarify node, `humble-humming-elephant.md:147`): R-BOX's
  `hcomp` on `fib`/`pathF`/`sigmaF` does **not** by itself break regularity;
  regularity is a `transp`/`comp` (R-FILL) concern. R-BOX must be written so that
  *if* C-REG later adopts Swan's fix, only the `comp` derivation (R-FILL) changes,
  not the per-former `hcomp` table here.

---

## Chosen approach for THIS substrate (concrete; respects containment)

**Thompson:** no new core constructor, no hash-format bump. R-BOX is entirely new
ι-rules inside `tryHcomp` (`core/eval.go:1204`), dispatched by `fibFormer` on the
head former — exactly the pattern already established by the piF push and by
`tryTransp`'s structural branch (`core/eval.go:1141`). Every sub-term is built
from existing builtin-group members via the reverse-lookup hashes (`FibHash`,
`KanHash`, `IntervalHash`, `FaceHash`, `SysHash`, and a new `PathHash`). **No
member is added to any group** (R-FILL may add a 4th Kan member `transpG`; R-BOX
needs no new member); **no new `Machine` field** beyond what phase 3 wired.

### The general shape of a proper-face `hcomp`, and how the system feeds the recursion

`hcomp A φ u u0` fills the open box whose **floor** is `u0 : El A`, whose
**tube/walls** are the partial path `u : (i:I) -> holds φ -> El A` (defined where φ
holds), and returns the **lid** at i1. Two definitional obligations any rule must
meet:

- **floor:** at the start of the filling direction the value is `u0`. (We implement
  `hcomp`, the lid, directly — the floor obligation is the implicit `i0` boundary
  of the derived `hfill`.)
- **boundary:** on φ, `hcomp A φ u u0 ≡ u i1 htop`-shaped — the lid agrees with the
  wall at i1. On the total face this is exactly rule (1). For a proper face the
  per-former rule must *reduce to* `u i1 (proof)` whenever φ becomes ⊤, **by
  construction** — the freshness/face machinery must preserve this.

**How a system feeds a recursive sub-fill (the mechanism that makes recursion on
the former work).** When a former-rule recurses, it builds a *new* system
`λi h. <expr over u i h>` and a *new* floor `<expr over u0>`, then calls
`hcomp`/`comp` on a **smaller** former. The new system is again a value of type
`(i:I) -> holds ψ -> El B` for the sub-former `B` and a (possibly extended) face ψ.
Crucially:

- The wall projection `u i h` is applied to the **same** validity proof `h : holds ψ`
  the sub-`hcomp` hands back — so no new `holds` proof is invented; the proof is
  *threaded*, never reconstructed. This is why the **piF push is overlap-free**: it
  reuses `h` verbatim.
- The only place a *new* face is built (and so a new overlap can arise) is when a
  former rule must **add boundary constraints** to φ — `pathF` adds the two
  endpoint atoms `ieq0 j`/`ieq1 j`; `Glue` adds the Glue's own `φ'`. Those added
  atoms are the *entire* source of the overlap obstacle, and §"The overlap
  obstacle, precisely" characterizes exactly when they collapse to `htop`.

We extend `tryHcomp` with a `switch role` on `fibFormer(args[0])`, each case firing
**after** the total-⊤ short-circuit and **for any proper or ⊥ face** (the piF
branch already does this).

### Per-former `hcomp` formulas (CCHM)

Notation: `A` is the (fixed, homogeneous) carrier code; φ the face; `u` the system
`λi h. …`; `u0` the floor. `H[B](ψ, v, v0)` abbreviates a recursive `hcomp B ψ v v0`
(via `KanHash(KRoleHcomp)`); `C[L](ψ, v, v0)` a recursive heterogeneous
`comp L ψ v v0` (via `KanHash(KRoleComp)`, owned by R-FILL); `pabs`/`papp` via the
new `PathHash`. The filling/binding direction variable is the bound `j` of an
inner `pabs`/`comp` line — a value-level interval `Val`, never a metalevel level
(staying inside the substrate's no-de-Bruijn-level discipline,
`ref_docs/rune-cubical-phase1.md:152`).

---

**0. `hcomp (piF P Fam) φ u u0` — function type (SHIPPED; the model case).**

```
hcomp (piF P Fam) φ u u0  ~>  λ(x : El P). hcomp (Fam x) φ (λi h. u i h x) (u0 x)
```

Already in `tryHcomp` (`core/eval.go:1222`). Overlap-free (the sub-system reuses
`h`). Reproduced here because it is the template every other rule follows:
*recurse on the former, push the system under the introduction form.*

---

**1. `hcomp (pathF A a b) φ u u0` — composition of paths. *(the workhorse, A3/A4)***

A path `p : El (pathF A a b)` is `papp`-applied at a point `j` to a point of `El A`
with `p @ i0 = a`, `p @ i1 = b`. To `hcomp` a *system of paths* into a lid path,
fill **pointwise in j** in the smaller former `A`, and **add the two endpoint
constraints** to the face so the lid still runs `a` to `b`:

```
hcomp (pathF A a b) φ u u0
  ~>  pabs A (λj.
        hcomp A (for φ (for (ieq0 j) (ieq1 j)))     -- extended face: φ ∨ (j=0) ∨ (j=1)
              (λi h'. SYS j h')                       -- extended system (below)
              (papp A a b u0 j))                      -- floor: the floor path at j
```

where the extended system `SYS j h'` dispatches by which disjunct of
`for φ (for (ieq0 j) (ieq1 j))` the proof `h'` witnesses:

- on **φ** (proof `horl`-injected, i.e. `h' = horl … h`): `papp A a b (u i h) j`
  — the i-th wall path, evaluated at j;
- on **j = i0** (`ieq0 j`): the constant `a` (the left endpoint, fixed in i);
- on **j = i1** (`ieq1 j`): the constant `b` (the right endpoint, fixed in i).

The two endpoint constraints force the resulting `pabs` to have boundary `a`/`b`,
so it inhabits `pathF A a b`. The inner `hcomp` is over `A` — a **smaller** former
— the recursion R-BOX is named for.

*Implementation of `SYS`:* the system is a function on `holds (for φ (for (ieq0 j)
(ieq1 j)))`. The engine cannot case-split a neutral `holds` proof, so `SYS` is
built to **compute by the same face-ι machinery the proof's face reduces through**:
when `j ~> i0`, `ieq0 j ~> ftop` (`core/eval.go:1019`) and the whole `for`
collapses to ⊤ (`core/eval.go:1052`), so the *outer* `hcomp`'s total-system rule
fires and projects the `a`-branch via `htop` — **the endpoint constraint is read
off by the existing ι-rules, not by inspecting the proof.** §"The overlap obstacle"
shows this is exactly why single-atom faces are safe and only genuine multi-disjunct
*neutral* φ needs C-OVERLAP.

---

**2. `hcomp (fib X) φ u u0` — embedded outer (strict) type.**

`fib X` views a strict set `X : U` as fibrant. CCHM's set/data carriers fill by a
**genuine homogeneous composition on the carrier**, not by collapsing to the floor.
On *this* substrate, however, `X` is an arbitrary outer type with no internal
`hcomp` of its own — the inner cubical layer cannot recurse *into* an outer type.
Two honest sub-cases:

```
hcomp (fib X) φ u u0  ~>  u0          -- φ = ⊥ (empty system: floor is the lid)   [READY]
hcomp (fib X) φ u u0  ~>  u i1 htop   -- φ = ⊤ (already rule 1)                     [SHIPPED]
hcomp (fib X) φ u u0  --  proper φ:  STUCK unless X carries a canonical-UIP witness  [GATED]
```

Honest position: on a *proper* face we may return neither `u0` (it would violate
`≡ u i1 htop` on φ unless `u i1 htop ≡ u0`, unknown) nor `u i1 htop` (undefined off
φ). The sound proper-face value for a *general* `fib X` is a **neutral `hcomp`**.
The exception: when `X` is a **strict set with canonical UIP** — which is exactly
what the outer OTT core provides for `Eq`/data-built carriers — all walls of `u` are
*outer-equal* to `u0`, and the lid is `u0`. But there is no *internal* `isSet X`
predicate to dispatch on here. **Decision (first increment):** ship the ⊥ case
(new, trivially sound) and keep proper-face `fib` **stuck**; revisit a UIP-witness
premise only when a listing needs it (park; C-OVERLAP-adjacent).

---

**3. `hcomp (sigmaF A B) φ u u0` — dependent pair (needs R-SIGMA + R-FILL).**

`comp`/`hcomp` for Σ is **elementwise** (cubicaltt; Cubical Agda docs). Writing
`fst`/`snd` for the (R-SIGMA) projections and `spair` for the pair intro:

```
hcomp (sigmaF A B) φ u u0
  ~>  spair afill1 bfill
  where
    -- first component: a HOMOGENEOUS fill in A, kept as a FILLER a*(j) (i0..j)
    a* : (j:I) -> El A
    a* = λj. hfill A φ (λi h. fst (u i h)) (fst u0) j        -- R-BOX's hfill (R-FILL §hfill)
    afill1 = a* i1 = H[A](φ, λi h. fst (u i h), fst u0)       -- the first-component lid
    -- second component: a HETEROGENEOUS comp along the line of B-fibres over a*
    bfill = C[λj. B (a* j)](φ, λi h. snd (u i h), snd u0)     -- comp over the B-line
```

The first component is a plain `hcomp A`; the second is a **`comp` along the line
`λj. B (a* j)`** of B-fibres over the first component's *filler* `a*`. This is
precisely where R-BOX **calls R-FILL** (`comp` over a varying `B(a* j)` line) and
needs **`hfill`** (`a*` is a filler, not just a lid). So `sigmaF` `hcomp` is
**ready-to-build only after** R-SIGMA (the former + `fst`/`snd`/`spair`) **and**
R-FILL (`hfill` + `comp` over a B-line) land. Overlap behaviour: elementwise, so it
**inherits** whatever overlap status the sub-`hcomp A`/`comp` have — overlap-free
on the faces those are overlap-free on (single-atom/⊥); no *new* overlap is
introduced by Σ itself (unlike `pathF`/`Glue`, Σ adds no boundary atom).

---

**4. `hcomp (Glue T A φ' e) φ u u0` — Glue (needs R-GLUE; research).**

The CCHM Glue `hcomp` is the long one. Sketch (the full equation is R-GLUE's
hardest deliverable, owned there, specified here only to record the consumer
relationship):

```
hcomp (Glue T A φ' e) φ u u0
  ~>  glue T A φ' e
        (λ(_ : holds φ'). t1)          -- glued-fibre part: fill in T
        a1                              -- base part: fill in A with unglue'd walls
  where
    t1 = H[T](φ, λi h. unglue (u i h), unglue u0)   -- fill the T-fibre (on φ')
    a1 = H[A](for φ φ', <walls reconciled via the equivalences e>, unglue u0)
```

i.e. fill the *base* `A` with `unglue`'d walls, fill each glued fibre `T` with
`hcomp T`, then `glue` them back, using the equivalences `e` to reconcile the two
on the overlap `for φ φ'`. **Not ready-to-build.** It is a *consumer* of the
`pathF`/`sigmaF` fills above (an equivalence's components are Σ of functions and
paths and its coherence is a path), so **R-BOX (pathF) + R-SIGMA must land first**.
R-GLUE owns the equation; R-BOX records the dependency and the `for φ φ'` overlap
(which is genuine multi-branch → C-OVERLAP).

### The overlap obstacle, precisely (the analysis that decides what is buildable now)

The parked finding says `u i h1 ≢ u i h2` for distinct `holds φ` proofs. The
question for each former rule is: *does any sub-fill it builds require reconciling
two system values that share an overlapping face?* The analysis:

- **piF, sigmaF, fib (⊥/⊤):** **no new face atom is added.** The sub-system reuses
  the *same* proof `h` (piF, σ elementwise) or the system is empty/total (fib).
  **No overlap arises → buildable now.**
- **pathF, single-atom φ:** the extended face is `for φ (for (ieq0 j) (ieq1 j))`.
  The two endpoint atoms `ieq0 j`, `ieq1 j` are **never simultaneously ⊤** (j cannot
  be both 0 and 1), so they never overlap *each other*. They overlap φ only at
  `j = i0`/`j = i1` *where φ also holds*; at those points the relevant atom reduces
  to ⊤ by the **existing face-ι rules**, the outer `hcomp` total-system rule fires,
  and the value is read via the **canonical `htop`** — never by comparing two
  arbitrary proofs. **Boundary coherence holds because the overlap point feeds
  `htop`, the one inhabitant.** So a single-atom (or ⊥) φ `pathF` `hcomp` is
  **buildable now**; the endpoint constraints' agreement with φ is discharged by
  ι, not by an irrelevant-Prop conversion rule.
- **pathF / Glue, proper *multi-disjunct neutral* φ:** here two distinct disjuncts
  of φ can hold simultaneously on a neutral face, and the rule would have to assert
  `u i (horl … h1) ≡ u i (horr … h2)` on the overlap — which is **not definitional
  here**. **Gated on C-OVERLAP** (lands as A8): either an irrelevant-`Prop`
  conversion rule (touches outer conversion — high caution) or threaded `Eq`
  evidence. This is the *only* genuine blocker, and it is the same one the roadmap
  already isolated.

**Net:** the open-box fill is buildable *now* for piF (done), the ⊥/⊤ `fib`/`pathF`
cases, and **proper single-atom-face `pathF`** (the workhorse for A3/A4 on the
common case), with the multi-branch face and `sigmaF`/`Glue` gated on their
respective prerequisites. This is materially more than "only ⊥" — the single-atom
`pathF` fill is what unblocks the practical A3/A4 path.

### Where `comp` (R-FILL) reduces into R-BOX (the tie to R-FILL)

CCHM derives, for a varying line:

```
comp A φ u u0  =  hcomp (A i1) φ (λj h. transpFill (λi. A i) j (u j h)) (transp A ⊥ u0)
```

So `tryComp` (`core/eval.go:1251`), once R-FILL ships `transpFill`, reduces a
**non-piF** line `comp` to a `hcomp (A i1) …` — i.e. **into the R-BOX table above**.
Today `tryComp` handles only the piF push and the ⊤/⊥ endpoints; **R-FILL + R-BOX
together complete it.** This is the seam: R-BOX is the homogeneous half, R-FILL the
heterogeneous half, and `comp = transpFill then hcomp` is where they meet. R-BOX
must keep its per-former `hcomp` table **independent of the `transpFill`/regularity
choice** so a later Swan fix (C-REG) touches only R-FILL (R-BOX risk 3).

---

## Interfaces & signatures to add (Go + Rune surface as relevant)

**No new builtin-group member, no new core `Tm`/`Val`, no hash-format bump** for the
`fib`/`piF`/`pathF` slice. `tryHcomp` already has `m.Fib`, `m.Kn`, `m.Fc`, `m.Iv`,
`m.Sy`; the one wiring gap is the path constructors: `tryHcomp` does not yet read
`m.Pa`, and `PathInfo` lacks a reverse lookup.

```go
// core/eval.go — extend PathInfo with the reverse lookup the path hcomp needs to
// CONSTRUCT pabs/papp sub-terms (mirrors FibHash/KanHash/FaceHash/SysHash).
type PathInfo interface {
    PathRoleOf(Hash) PathRole
    PathHash(PathRole) (Hash, bool)   // NEW
}

// store/path.go — implement it (trivial; mirrors store/face.go FaceHash).
func (s *Store) PathHash(role core.PathRole) (core.Hash, bool)
```

```go
// core/eval.go — tryHcomp gains a former switch, after the ⊤ short-circuit,
// subsuming the lone piF branch:
func (m *Machine) tryHcomp(args []Val) (Val, bool) {
    // ... existing arity guard + ⊤ short-circuit (core/eval.go:1208) ...
    role, cargs, ok := m.fibFormer(args[0])
    if !ok { return nil, false }
    A, phi, u, u0 := args[0], args[1], args[2], args[3]
    switch role {
    case FRolePiF:   // SHIPPED push (any face); cargs = [P, Fam]
    case FRolePathF: // NEW: path hcomp; cargs = [A', a, b]. Build the extended
                     // pabs/face/system above. Fire for ⊥ and proper SINGLE-ATOM
                     // φ; on a proper MULTI-DISJUNCT neutral face return stuck
                     // (overlap gate). _ = phi; _ = u; _ = u0
    case FRoleFib:   // NEW: ⊥ -> u0 ; ⊤ already handled ; proper -> stuck.
    // case FRoleSigmaF: // R-SIGMA increment: elementwise; calls H[A]/C[B-line]/hfill
    // case FRoleGlue:   // R-GLUE increment: research
    }
    return nil, false
}

// Helpers so the rules read like the formulas (each is m.Apply over m.refVal):
func (m *Machine) vPabs(A core.Val, f core.Val) core.Val // pabs A f
func (m *Machine) vPapp(A, a, b, p, j core.Val) core.Val // papp A a b p j
func (m *Machine) vIeq0(j core.Val) core.Val             // ieq0 j
func (m *Machine) vIeq1(j core.Val) core.Val             // ieq1 j
func (m *Machine) vFor(a, b core.Val) core.Val           // for a b
func (m *Machine) vHcomp(B, psi, v, v0 core.Val) core.Val // hcomp B psi v v0
func (m *Machine) faceIsSingleAtomOrBot(phi core.Val) bool // overlap gate (below)
```

```go
// R-FILL companions (specified for the seam, NOT built by R-BOX):
//   hfill : the filler form of hcomp (returns the i0..j filler, not just the lid).
//           Needed by the sigmaF rule's a*. A derived Go helper; lives with R-BOX
//           conceptually (homogeneous) but is only consumed by comp/sigmaF.
func (m *Machine) hfill(B, psi, v, v0, j core.Val) core.Val
//   tryComp gains the non-piF `comp = transpFill ▸ hcomp` derivation (R-FILL).
func (m *Machine) tryComp(args []Val) (Val, bool)
```

**The overlap gate `faceIsSingleAtomOrBot`.** The path rule fires only when the
*input* φ is ⊥ or a single atom (`ieq0`/`ieq1` of any interval term, or a neutral
variable face that is not a `for`). On a `for`-headed neutral φ it returns stuck.
This is a **syntactic, conservative** check on the forced head of φ
(`faceConst`/`fibFormer`-style): force φ; if it is `fbot` → fire (empty); if it is
a single atomic constraint or a bare neutral non-`for` → fire; if its head is `for`
with a neutral that cannot reduce → stuck. (When φ reduces to ⊤ the total-system
rule already fired upstream.)

**Rune surface:** none. `hcomp`/`comp`/`transp` are already the Kan builtin group
(`store/kan.go`); R-BOX adds reductions, not surface. Teachable artifacts are new
listings (`chXX`), not new syntax.

---

## Worked micro-example (the teachable artifact)

Two listings: the **ready win** (overlap-free path fill) and the **edge pin**
(proper multi-branch stays stuck).

**(a) The ready win — `hcomp` of a path with an empty (⊥) system reduces to a path
with the correct boundary**, certified by `refl` on the endpoints. This exercises
the full stack — face ι, total-system `hcomp`, path β — with **zero overlap**:

```
-- chXX: hcomp on a path, empty system, has the floor path's endpoints.
fillEmptyPathLeft :
  (A : UF) -> (a b : El A) -> (p : El (pathF A a b)) ->
  Eq (El A)
     (papp A a b (hcomp (pathF A a b) fbot
                        (fn (i : I) (h : holds fbot) is p end)
                        p)
                 i0)
     a
is
  fn (A : UF) (a b : El A) (p : El (pathF A a b)) is refl a end
end
```

Reading: the `hcomp` reduces to `pabs A (λj. hcomp A (for fbot (for (ieq0 j) (ieq1
j))) … (papp A a b p j))`; applying at `i0` makes `ieq0 i0 ~> ftop`
(`core/eval.go:1019`), the inner `hcomp A` face reduces to ⊤, its total-system rule
fires and yields the `a`-endpoint wall via `htop`, so the whole thing is `a` on the
nose and `refl a` checks. This is the teachable *"a box whose only walls are its
two path endpoints fills to a path with exactly those endpoints"* story. The dual
`fillEmptyPathRight` (at `i1`, yielding `b`) is the second certifying line.

**(b) A single-atom proper face that fires** (the practical A3 case): a path `hcomp`
on `φ = ieq0 k` for a free interval variable `k` reduces (single-atom → no overlap)
to a `pabs` whose value the implementer pins by `refl` against its explicit pushed
form, mirroring the ch23 η-pin technique.

**(c) The edge pin (Savage — teach the boundary):** `hcomp (pathF …) φ …` with φ a
*`for`-headed neutral* face must stay **stuck** (normal form still has an `hcomp`
head), mirroring `TestTranspVaryingStaysStuck`
(`internal/session/structural_test.go:37`). This pins the overlap gate: the engine
refuses to silently assume overlap-agreement it cannot justify.

---

## Risks / open sub-questions

1. **Overlap-agreement (the dominant risk, but now bounded).** Proper *multi-
   disjunct neutral* `for`-faces need `u i h1 ≡ u i h2` on overlaps, not
   definitional here. *Mitigation:* the gate `faceIsSingleAtomOrBot` fires only
   where the overlap is *empty or resolved by ι-to-`htop`* (§"The overlap obstacle,
   precisely"); multi-branch faces stay stuck and are owned by **C-OVERLAP**/A8.
   This keeps R-BOX a green, contained checkpoint and — unlike the prior framing —
   delivers the *practical single-atom `pathF` fill* now, not only ⊥.

2. **`fib X` proper-face fill needs a set/UIP witness.** A strict carrier's `hcomp`
   is `u0` only if `X` is set-truncated; we have canonical UIP but no *internal*
   `isSet` predicate to dispatch on. *Open:* (a) leave proper `fib` stuck
   (recommended), or (b) add an internal `isSet` premise consumed by the rule (no
   consumer yet → park).

3. **Regularity (C-REG, owned by R-FILL).** R-BOX's `hcomp` table is
   regularity-neutral; the risk lives in R-FILL's `comp = transpFill ▸ hcomp`
   derivation. *Mitigation:* keep the per-former `hcomp` rules independent of the
   `transpFill` choice so a later Swan fix touches only R-FILL. X2 canonicity tests
   detect a regularity break early.

4. **`hfill` ownership.** The `sigmaF` rule needs `hfill` (the filler, not the
   lid). *Recommendation:* `hfill` is the *filler form of `hcomp`* (homogeneous, so
   conceptually R-BOX's), implemented as a derived Go helper threading the
   direction variable (`hcomp B (for φ (ieq0 j)) (λi h. …) v0` shape), consumed by
   `comp`/`sigmaF` (R-FILL/R-SIGMA). Build it in R-BOX, not as a new member.

5. **Boundary-equation testing.** "The lid agrees with the wall on φ" is a
   *metatheorem* about the rules, not type-checked. *Mitigation:* a rapid property
   (X2): for random closed φ that reduce to ⊤, the proper-face rule's output
   converts to the total-system rule's output (`u i1 htop`); for closed φ reducing
   to ⊥, the `fib`/`pathF` output converts to the floor.

6. **The extended-system constructor must compute by ι, not proof inspection.** The
   `pathF` rule's `SYS j h'` must read off the endpoint constraints through the
   existing face-ι rules (j→i0 ⇒ `ieq0 j`→⊤), never by casing a neutral `holds`
   proof. *Open:* confirm the constructed inner `hcomp`'s face genuinely reduces to
   ⊤ at the endpoints under the engine's force order (pin with a property test that
   the `i0`/`i1` boundary of the produced `pabs` converts to `a`/`b`).

---

## Test/gate plan

- **Store round-trip:** `PathHash`/`PathRoleOf` agree (mirror the `FibHash`/
  `KanHash`/`FaceHash` tests).
- **Listing chXX (ready slice):** `fillEmptyPathLeft`/`Right` (⊥-face path `hcomp`
  endpoint reductions, `refl`-certified) and the single-atom-face fire (η-pinned).
- **Negative pin (session test):** `for`-headed neutral-face `hcomp (pathF …)` and
  `hcomp (fib …)` stay stuck — normalized form still contains `hcomp` (mirror
  `structural_test.go:37`).
- **Rapid properties (harness/cubical_props_test.go):**
  - *boundary coherence:* for closed φ normalizing to ⊤, the `pathF`/`fib` rule's
    output converts to `u i1 htop`; for closed φ normalizing to ⊥, the `fib` output
    converts to `u0` and the `pathF` output's endpoints convert to `a`/`b`.
  - *overlap-gate conservativity:* a `for`-headed neutral φ never fires (output has
    an `hcomp` head).
  - confluence/canonicity of closed cubical terms still holds (X2).
- **Frame Lemma (X1):** the new ι-rules force scrutinees through the same `m.Force`
  seam and build sub-terms via `m.refVal(hash)` (the logged path), so the
  dependency log is unchanged; assert no new unlogged unfolds.
- **Containment (Thompson):** hashes of ch09/ch10/ch17–ch23 and the fib/interval/
  path/face/sys groups unchanged; no hash-format bump (`defFormatVersion`
  unchanged); `git grep` confirms no new core `Tm`/`Val` constructor and no new
  Kan/Fib/Path member (R-BOX adds only `PathHash`, a method, not a member).

---

## Unblocks (which implement nodes, and what they still need)

- **A2** (`hcomp/comp proper-face fills (fib,piF)` ⇐ R-FILL, R-BOX, C-REG): R-BOX
  delivers the `piF` push (done), the `fib` ⊥-case, and — beyond the prior framing
  — the **proper single-atom-face `pathF` fill**. A2's *proper multi-branch* fills
  still need **C-OVERLAP** (A8); its `comp` half needs **R-FILL** (`transpFill`/
  `hfill`/the `comp = transpFill ▸ hcomp` seam) and **C-REG** (resolved in R-FILL:
  non-regular, gated on X2). Buildable now to the single-atom/⊥ boundary; completes
  when R-FILL + C-REG + C-OVERLAP land.
- **A3** (`transp/comp over pathF` (base composition) ⇐ A1, A2): A3's base
  composition *is* a `hcomp (pathF …)` — R-BOX's path rule is exactly what A3
  reduces into. A3 still needs A1 (varying-domain `transp` for the type-line, from
  R-FILL) and the overlap gate for proper-face inner `hcomp`. **The single-atom
  `pathF` fill makes the common A3 path buildable now.**
- **A4** (`pathJ-via-comp on a general path` ⇐ A3): bottoms out in R-BOX's `pathF`
  `hcomp`; needs A3 landed. Transitive on R-BOX.
- **A5** (`sigmaF group + Kan over sigmaF` ⇐ R-SIGMA, A2): R-BOX specifies the
  elementwise `sigmaF` `hcomp`; A5 still needs **R-SIGMA** (former + `fst`/`snd`/
  `spair`) and **R-FILL** (`hfill` + `comp` over the B-line). The rule here drops in
  once those land.
- **A6** (`Glue group + Kan-over-Glue` ⇐ R-GLUE, A5, A2): R-BOX records that Glue's
  `hcomp` consumes the `pathF`/`sigmaF` fills and the `for φ φ'` overlap; the
  equation is **research** owned by R-GLUE, not buildable here.
- **X2** (cubical property tests): R-BOX adds the boundary-coherence, ⊥-collapse,
  and overlap-gate-conservativity properties above.

**Honest label.** *Ready-to-build now* — `PathHash` plumbing; `fib`-⊥ and the
`pathF` ⊥/total fills; the **proper single-atom-face `pathF` fill** (the practical
A3/A4 unblocker); the helper constructors; chXX; the negative pins; the
boundary/⊥ properties. *Research/gated* — proper *multi-branch* `for`-faces
(C-OVERLAP/A8), `fib` proper face (needs internal `isSet`), `sigmaF` (R-SIGMA +
R-FILL `hfill`/`comp`), `Glue` (R-GLUE, the hardest CCHM equation).
