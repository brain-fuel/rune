# RBOX-DESIGN — proper-face open-box `hcomp` (homogeneous filling)

*Consolidated, decision-locked design. Synthesizes [R-BOX.md](R-BOX.md) against
the **now-landed** code (since the fan-out: `transpG`/`transpFill` landed (A1),
`sigmaF` + A5a landed (A5), `Glue`/`Equiv` landed (A6), the generic `⊥` hcomp
rule landed). Where the fan-out differs from what shipped or from a grounded
re-analysis, this doc follows the code and says so. §F discipline: ship the
computing slice, honestly stuck-label the deep half.*

R-BOX is the **homogeneous** keystone — genuine open-box filling `hcomp` by
recursion on a type's head former, the twin of R-FILL (`transp`/`comp`, landed).
`comp = transpFill ▸ hcomp`, so several formers bottom out in `hcomp` even when
their type-line is constant.

> **AS BUILT (pathF slice — LANDED, green).** Steps 1–2 of §6 are implemented and
> verified:
> - `PathHash` reverse lookup (`core.PathInfo` + `store/path.go`), round-trip test.
> - The **pathF proper-face hcomp rule** (§2) in `core/eval.go` tryHcomp — fires
>   for any face, overlap-free, endpoints definitional.
> - Certified: `listings/ch46_pathfill.rune` (papp of the fill at i0/i1 ~> a/b on
>   a PROPER face, refl-pinned), `structural_test.go`
>   `TestPathHcompInteriorStaysSymbolic` (interior at a free point keeps an
>   `hcomp` head — honest reach). No hash-format bump; full suite green.
> - Confirms grounded findings 1 (⊥ fill already worked) and 2 (walls-are-paths →
>   no dispatch needed → any face). Unblocks the common A3/A4 path.
> - Still: `fib` proper face (isSet, parked), dependent `sigmaF` (A5b increment),
>   `Glue` hcomp (R-GLUE fixup).
>
> **AS BUILT 2 (the dispatch primitive + A3 — LANDED, green).** §1 claimed the
> engine "cannot express a face-dispatching system" and that this blocked A3. That
> was **wrong** — the dispatcher is an *additive builtin group*, no kernel growth:
> - **`fsplit` group** (`store/fsplit.go`, `core.FsplitInfo`): the eliminator of
>   `holds (for φ ψ)`, `fsplit A φ ψ u v h`, computing by face-ι (`~> u htop` when
>   φ≡⊤, `~> v htop` when ψ≡⊤). A system `[φ↦u; ψ↦v]` is now `λh. fsplit … h` — a
>   genuine *dispatching* `holds (for φ ψ) -> El A` function. No conversion change
>   (option-(b)-aligned); overlap-agreement is the caller's obligation, vacuous
>   for disjoint faces.
> - **transp-over-pathF (A3)** in `transpG` (`core/eval.go`): `transp (λi. pathF
>   (A i)(a i)(b i)) p ~> pabs (A i1) (λj. comp (λi. A i) (for (ieq0 j)(ieq1 j))
>   [fsplit endpoint system] (papp … p j))`. Endpoints compute (a i1 / b i1); the
>   interior is the CCHM symbolic comp. Plain transport (φ≡⊥) handled; proper-φ
>   transpG stays stuck (a later increment).
> - Certified: `listings/ch47_pathtransp.rune` (endpoints refl-pinned on a varying
>   line), `structural_test.go` `TestFsplitDispatch` / `TestTranspVaryingPathFFires`
>   / `TestPathTranspInteriorStaysSymbolic`. Full suite green, no hash bump.
> - **Unblocks the rest of Track A:** the dispatch primitive is the keystone the
>   convergence analysis identified — A4 (pathJ-via-comp), A5b (dependent sigmaF,
>   needs hfill = a dispatching system), and the Glue Kan fixup all now have their
>   missing piece.

---

## 0. What is already landed (re-grounded)

| `hcomp A φ u u0` case | status | where |
|---|---|---|
| `φ ≡ ⊤` → `u i1 htop` (total system) | LANDED | `core/eval.go` tryHcomp |
| `φ ≡ ⊥` → `u0` (empty system) **— any former** | LANDED (A2) | tryHcomp `CRoleBot` |
| `hcomp (piF P Fam) φ …` push under binder, **any face** | LANDED | tryHcomp `FRolePiF` |
| non-dependent `sigmaF` product transp/hcomp/comp | LANDED (A5a) | `hcompSigma` |

> **Grounded finding 1 (verified live).** R-BOX.md's headline "teachable win" —
> `hcomp (pathF A a b) ⊥ u u0` reducing to a path with endpoints `a`/`b` —
> **already works with no new rule**: the generic `⊥` rule gives `u0`, then the
> papp-boundary gives `papp u0 i0 ~> a`, `papp u0 i1 ~> b`. Confirmed by probe
> (`papp (hcomp (pathF A a b) fbot … p) i0 ~> a`). So the ⊥ slice needs nothing.

The genuinely-absent case is **proper-face** `hcomp` for the non-`piF` formers:
`pathF`, `fib`, dependent `sigmaF`, `Glue`.

---

## 1. The system-dispatch problem (and why `pathF` escapes it)

CCHM fills `hcomp (Path A a b) φ u u0` with a **three-branch system**
`[ φ ↦ u@j, (j=0) ↦ a, (j=1) ↦ b ]` over the extended face `φ ∨ (j=0) ∨ (j=1)`.
On this engine a "system" is a *function* `holds ψ -> El A` — and `holds` has **no
eliminator** (`store/sys.go`; the parked finding). So a system **cannot dispatch
on which face holds**: given a proof, the function body can't ask "is this the
`φ` disjunct or the `(j=0)` disjunct?". This is the same root obstacle as
C-OVERLAP, and at first glance it blocks every per-former rule that adds boundary
branches.

> **Grounded finding 2 (the key insight — `pathF` needs no dispatch).** In
> `hcomp (pathF A a b) φ u u0`, the walls `u i h : El (pathF A a b)` are
> **themselves paths from `a` to `b`**. So `papp (u i h) i0 ~> a` and
> `papp (u i h) i1 ~> b` *automatically*, by the papp-boundary rule, for **every**
> wall. Therefore the endpoint branches `(j=0) ↦ a`, `(j=1) ↦ b` need not be
> written into the system at all — their *values* fall out of the walls' own path
> boundary. The system is just the **φ-branch reindexed by `j`**:
> `λi h. papp A a b (u i h) j`. No per-face dispatch, and the endpoint/φ overlap
> agrees **definitionally** (on `φ ∧ (j=0)` both give `a` via papp-boundary).

This is strictly stronger than R-BOX.md's "single-atom faces only" claim:
`pathF` hcomp is buildable for **any** face φ (⊥/⊤/single-atom/multi-branch),
because the dispatch obstacle never arises — it is deferred to the *inner* `A`
hcomp, which stays a safe neutral on proper faces.

---

## 2. The `pathF` hcomp rule (READY — the deliverable)

```
hcomp (pathF A a b) φ u u0
  ~>  pabs A (λj.
        hcomp A (for φ (for (ieq0 j) (ieq1 j)))      -- extended face φ ∨ (j=0) ∨ (j=1)
              (λi h. papp A a b (u i h) j)            -- φ-branch reindexed by j
              (papp A a b u0 j))                       -- floor path at j
```

Why it is sound, and why the endpoints compute definitionally:

- **Endpoint j=i0:** the extended face is `for φ (for ftop fbot)` = `for φ ftop` =
  `ftop` (face-ι). The total-system rule fires: `hcomp A ⊤ SYS floor ~> SYS i1
  htop` = `papp A a b (u i1 htop) i0` ~> `a` (papp-boundary, for *any* wall).
  Symmetrically j=i1 ~> `b`. So `papp result i0 ~> a`, `papp result i1 ~> b` —
  the result inhabits `pathF A a b` on the nose, for **any** φ.
- **Interior (generic j, proper φ):** the extended face is proper, so the inner
  `hcomp A …` stays a neutral — the result is a `pabs` with a symbolic interior
  but correct endpoints. A genuine improvement: hcomp on a path now *produces a
  path* you can `papp`, compose, and read endpoints from (the A3/A4 unblocker).
- **Overlap-free:** the rule applies `u` to the *same* proof `h` it receives and
  never reconstructs one; any actual reconciliation is deferred to the inner `A`
  hcomp (stuck/safe on proper faces). So this needs **no C-OVERLAP** — unlike the
  fan-out's multi-branch worry.

This recurses on the former (pathF → A), the recursion R-BOX is named for.

---

## 3. The other formers (status, grounded)

- **`fib X` proper face** — STUCK, no clean rule. A strict carrier's homogeneous
  fill is the floor only if `X` is set-truncated; the outer OTT core gives
  canonical UIP but there is **no internal `isSet X` predicate** to dispatch on.
  Decision: leave proper-face `fib` stuck (⊥/⊤ already covered generically);
  revisit a UIP-witness premise only when a listing needs it (park).
- **dependent `sigmaF`** — elementwise: first component `hcomp A`, second a `comp`
  along the B-line `λj. B (a* j)` over the first component's *filler* `a*`. Since
  `transpFill`/`transpG` **are now landed**, the `comp`-over-a-B-line and the
  `hfill` (filler, not lid) this needs are buildable — this is the A5b increment,
  no longer fully blocked. Overlap-free (elementwise; Σ adds no boundary atom).
  Specified, not built here.
- **`Glue`** — the long CCHM unglue/glue dance; consumes the `pathF`/`sigmaF`
  fills and the `for φ φ'` overlap (genuine multi-branch → C-OVERLAP). Owned by
  the A6 fixup tier; research, not built here.

---

## 4. Interfaces (the one plumbing gap)

`tryHcomp` already reads `m.Fib`, `m.Kn`, `m.Fc`, `m.Iv`, `m.Sy`. The gap is the
**path reverse-lookup** (to construct `pabs`/`papp`), mirroring
`FibHash`/`KanHash`/`FaceHash`/`IntervalHash`:

```go
// core/eval.go — extend PathInfo:
type PathInfo interface {
    PathRoleOf(Hash) PathRole
    PathHash(PathRole) (Hash, bool)   // NEW
}
// store/path.go — implement (trivial; mirrors FaceHash).
func (s *Store) PathHash(role core.PathRole) (core.Hash, bool)
```

No new builtin-group member, no new core `Tm`/`Val`, no hash-format bump. The
rule is a new `case FRolePathF` arm in `tryHcomp`'s former switch, after the ⊤/⊥
short-circuits, built from `PathHash`/`FaceHash`/`IntervalHash` reverse lookups.

---

## 5. Test / gate plan

- **`PathHash` round-trip** (`store/path_test.go`): `PathHash`/`PathRoleOf` agree.
- **Endpoint reductions** (listing `chXX_pathfill.rune`, refl-pinned): for a
  proper face (a free `holds φ` wall system), `papp (hcomp (pathF A a b) φ u u0)
  i0 ~> a` and `… i1 ~> b` — the boundary computes for any φ. (The ⊥ case already
  works; this pins the *proper-face* endpoint computation, the new content.)
- **Interior stays symbolic** (negative pin, `structural_test.go`): for a proper
  φ and `A = fib X`, the *interior* `papp (hcomp …) k` at a free interior point
  `k` keeps an `hcomp` head — pins that the rule produces a path without
  over-claiming the interior.
- **Boundary coherence** (rapid, `cubical_props_test.go`): for closed φ → ⊤ the
  rule's output converts to `u i1 htop`'s path; for closed φ → ⊥ to `u0`.
- **Containment:** ch09–ch24 + all group hashes byte-identical; no
  `defFormatVersion` bump; `PathHash` is a method, not a new member.

---

## 6. Build order

1. **`PathHash`** (PathInfo + store) — the plumbing. Round-trip test green.
2. **`pathF` hcomp rule** (§2) in `tryHcomp`. Endpoint refl-pins green (proper
   face computes to a/b); interior negative pin green.
3. **dependent `sigmaF` hcomp** (§3) — the A5b increment, now unblocked by landed
   `transpFill`; elementwise via `hfill` + comp-over-B-line. *(separate increment)*
4. **`fib`/`Glue` proper face** — parked (isSet premise) / owned by R-GLUE fixup.

Steps 1–2 are the R-BOX ready slice on the **currently landed** substrate, and
they unblock the common A3/A4 path (base composition is exactly a `pathF` hcomp).

---

## 7. Unblocks

- **A3** (transp/comp over pathF, base composition) — bottoms out in this rule;
  the proper-face pathF fill is what A3 reduces into. Still needs A1 (landed) for
  the type-line.
- **A4** (pathJ-via-comp on a general path) — transitive on A3 / this rule.
- **A5b** (Kan over dependent `sigmaF`) — §3, unblocked by landed `transpFill`.
- **A6 fixup** (Kan-over-Glue) — consumes the pathF/sigmaF fills (research).

**Status:** READY-TO-BUILD for `PathHash` + the `pathF` hcomp rule (any face,
overlap-free, endpoints definitional) — materially more than the fan-out's
"single-atom only", because the walls-are-paths insight removes the dispatch
obstacle. `fib` proper face and `Glue` stay stuck (isSet / R-GLUE); dependent
`sigmaF` is a now-unblocked separate increment.

Sources: [R-BOX.md](R-BOX.md), [R-FILL.md](R-FILL.md), CCHM
([arXiv:1611.02108](https://arxiv.org/pdf/1611.02108)) §4.3; landed
`core/eval.go` tryHcomp, `store/{path,face,interval,kan}.go`.
