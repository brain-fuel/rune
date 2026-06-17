# R-ERASE2 — Erased runtime meaning of inner paths/Kan

> Roadmap node **B5** (`B5 [I] erased meaning of inner paths/Kan; lift the
> inner-taint deploy ban ⇐ R-ERASE2, A7, B2`). Track B, the codegen track.
> Consumes **R-IR** (B1, delivered: the `IInner{Op,Args}` structural seam) and is
> gated on **A7** (R-UA — ua computes through Glue) for the *univalence* fragment.
> Lifts the deploy ban enforced by `internal/session/session.go:621` `innerTaint`
> + the `EmitProgram`/`EmitExpr` refusals (`session.go:439-446,469-473`). Its
> output is the runtime ABI for the cubical builtin groups, expressed as IR
> lowering rules over R-IR's `IInner`.
>
> **Status: partly ready-to-build, partly gated.** A *large, exact* subset of the
> inner layer is runtime-irrelevant or already reduces to plain λ-calculus in the
> kernel; that subset's erasure is **ready-to-build today** and can lift the
> deploy ban for everything except live transport-over-`Glue`/`ua`. The
> remaining fragment (transport along a *genuinely varying* line / a `ua` path
> whose forward map must run) is gated on **A7** and on the kernel actually
> reducing those redexes (it does, for `ua`, *today* — see §Problem fact 4).

## Problem (what's stuck/absent today, with file:line)

The v3 release criterion is "the inner chapters *check*", not "run"
(CLAUDE.md, §v3.0.0). The single mechanism enforcing that is a **name-walk taint
ban** that refuses to deploy any definition whose erased body mentions an inner
value member:

1. **The taint set is a hard-coded string set in `session`, not `codegen`.**
   `internal/session/session.go:621-654` `innerTaint` walks the erased IR for
   `IGlobal` names in `{preflF, pathJ, ureflU, ua, castU, i0, i1, ineg, imin,
   imax, pabs, papp, ieq0, ieq1, fand, for, ftop, fbot, htop, hand, horl, horr,
   transp, hcomp, comp}` (`session.go:622-627`). Any def reaching one is marked
   tainted (`session.go:610-613`); a tainted **main** is a hard error
   (`session.go:439-446`), and `EmitExpr` refuses likewise (`session.go:469-473`).
   This is the *entire* "checks but does not deploy" boundary.

2. **The ban is all-or-nothing and over-broad.** It refuses *every* inner member
   uniformly, including ones that are **provably runtime-irrelevant** (the face
   lattice `F`, `holds` proofs) or that **already reduce to plain λ-calculus in
   the kernel** (`papp` β, `papp` boundary, `ineg`/`imin`/`imax` on endpoints,
   `transp` over a constant line — the regularity rule). The ban exists because
   *one* construct — live transport over a varying line — had no erased meaning
   in v3. It penalises the whole stratum for that one gap.

3. **Erasure already discards the OUTER equality's computational story, by the
   right discipline — and the inner one must mirror it.** `core.Subst` (Leibniz
   transport) erases to *the identity on its payload*:
   `elaborate/erase.go:151-155` returns `check(Px)` — "Transport is the identity
   on its computational payload Px : P x." `core.Cast` erases to its subject
   (`erase.go:147-150`), `core.Eq`/`core.Refl`/`core.Prop` to `IUnit`
   (`erase.go:79-81`). The syntactic `codegen.Erase` agrees (`codegen/ir.go:128-151`:
   `Subst → Px`, `Cast → X`). **The outer transport is already runtime-irrelevant
   — it erases away.** R-ERASE2's question is: *which inner transports are the
   same, and which carry a real function the runtime must apply?*

4. **The kernel ALREADY reduces inner transport to its computational payload —
   the erasure can read it off.** This is the load-bearing fact the deploy ban
   does not exploit:
   - `castU A B (ua A B f g s t) x ~> f x` — `core/eval.go:818-824` forces the
     path, matches a 6-arg `FRoleUa` spine, and returns `m.Apply(pargs[2],
     args[3])` = **the forward map applied to the point**. The erased meaning of
     "transport along a univalence path" is *literally the application `f x`*.
   - `castU A B (ureflU A) x ~> x` — `core/eval.go:815-816` — **the identity**,
     same as outer `Subst`.
   - `papp … (pabs A f) i ~> f i`; `papp … (preflF A x) i ~> x`; boundary
     `papp A x y p i0 ~> x` / `… i1 ~> y` (`core/eval.go:939-983`,
     `core.PathInfo/tryPathIota`) — **plain application / projection**.
   - `transp (λi. A) a0 ~> a0` when the line is constant (`tryTransp`,
     `core/eval.go:1122-1129`) — **the identity**.
   - `transp (λi. piF D (Fam i)) f ~> λx. transp (λi. Fam i x) (f x)` (constant
     domain, `core/eval.go:1142-1168`) — **a λ wrapping a sub-transport**.
   - `hcomp A ⊤ u u0 ~> u i1 htop`; `comp A ⊤ u u0 ~> u i1 htop`; degenerate
     `comp (const A) ⊥ u u0 ~> u0` (`tryHcomp`/`tryComp`,
     `core/eval.go:1204-,1251-`) — **picking the total-system value, or the
     identity.**

   So for every inner op the deploy ban refuses, the *normaliser* already knows
   the answer — it is a λ-calculus term. The ban is conservative precisely where
   the kernel is *not* stuck. The gap is the **genuinely stuck** residue (a
   transport over a *neutral/varying* line that is not piF-constant), which is
   the §F frontier and is gated on R-FILL/R-GLUE/A7 — not on erasure.

The net: **R-ERASE2 must (a) classify each inner construct as runtime-irrelevant,
λ-reducible, or genuinely-payload-carrying; (b) give the payload-carrying ones an
erased lowering through R-IR's `IInner`; (c) replace the all-or-nothing string
ban with a structural, op-precise ban that fires only on the still-stuck residue.**

## Prior art (what the literature/other systems do; cite)

- **Cubical Agda's `--cubical=erased`** is the closest precedent and the exact
  shape to steal. It is a *mode* in which `Glue` and the other cubical builtins
  "must only be used in erased settings"
  ([Agda Cubical docs](https://agda.readthedocs.io/en/latest/language/cubical.html)),
  i.e. the language *already recognises* that the cubical machinery is, for the
  purpose of compiled code, **runtime-irrelevant data that must not be run**. Agda
  enforces this with its *erasure annotation* (`@0`) and runtime-irrelevance
  checker ([Agda runtime-irrelevance](https://agda.readthedocs.io/en/latest/language/runtime-irrelevance.html)).
  The lesson for Rune: the right framing is not "ban the inner layer" but
  "the inner layer is **0-quantity by construction at the boundary** — it erases,
  and the only thing that survives is the function a `ua`/`transp` *computes to*."
  Rune already has the QTT 0-fragment as its erasure boundary (CLAUDE.md Phase 5);
  R-ERASE2 makes the inner stratum live in it.

- **uaβ is the erasure rule, stated as a theorem.** Across CCHM/Cubical Agda/1Lab,
  `transport (ua e) x ≡ equivFun e x` — *"transporting along an equality derived
  from an equivalence applies the equivalence"*
  ([1Lab Univalence](https://1lab.dev/1Lab.Univalence.html);
  [Cubical Agda paper](https://staff.math.su.se/anders.mortberg/papers/cubicalagda2.pdf)).
  This is **the** erased-runtime-meaning statement: the computational content of a
  univalence transport *is exactly the forward map*. Rune's kernel already bakes
  `uaβ` in (`core/eval.go:823`), so its erasure is *read off the reduct*: `ua`'s
  forward map `f`, applied. (R-UA's project is to make this a *derived* reduction
  through Glue rather than a bolted rule; R-ERASE2's erasure is *indifferent* to
  which — it erases the reduct either way. See §Risk "co-evolution with A7".)

- **Type Theory with Erasure / presheaf extraction.** Recent work formalises code
  extraction from a type theory to *untyped lambda calculus*, proving the
  extraction correct by a gluing argument
  ([Type Theory With Erasure, arXiv:2605.00655](https://arxiv.org/html/2605.00655)).
  The principle Rune inherits: **the erased target is untyped λ-calculus + data**
  (exactly R-IR's IR), and correctness means *the erased program computes the same
  observable as the kernel normal form*. R-ERASE2's gate (§Test plan) is precisely
  this gluing property restricted to the inner ops: `run(erase(t)) = observe(nf(t))`.

- **Glue carries no runtime data beyond its base + equivalences; `unglue` is a
  projection.** In CCHM, a `Glue B [φ ↦ (T,e)]` element on the *interior* (where
  `φ` is false) is just an element of `B`; the equivalence data is needed only to
  *transport across* faces ([CCHM §6](https://www.cse.chalmers.se/~coquand/cubicaltt.pdf)).
  For a *compiled, closed* program every interval variable is eventually
  instantiated to an endpoint, the faces decide, and what remains is `B`-data and
  the equivalences' *forward maps*. This is why the erased meaning is always a
  plain function: **all the homotopy is in the proof of where the faces sit, which
  is `holds φ` — proof-irrelevant, erased.**

- **The builtin-group / 0-fragment containment pattern** (CLAUDE.md v2/v3/§F;
  R-IR §Containment; R-EFFECT's erased `World`): every stratum extension stays a
  contained group behind an interface, and *erases through the same 0-fragment
  boundary*. R-ERASE2 adds **no core constructor, no hash bump** — it is entirely
  in `codegen/` + `elaborate/erase.go` + the deletion of `session.go`'s string
  ban. Same move as R-IR, applied to the *meaning* of `IInner` rather than its
  *marking*.

## Chosen approach for THIS substrate (concrete; respects containment)

**Thesis: the inner stratum is 0-quantity at the deploy boundary; its only
runtime residue is the function a transport/`papp` *reduces to*, read off the
kernel normal form.** Three pieces: (A) a **classification** of every inner
construct into one of three erasure classes; (B) the **erased lowering** of the
two classes that survive, expressed over R-IR's `IInner`; (C) the **op-precise
deploy ban** that replaces `session.go`'s string set, firing only on the genuinely
stuck residue. All shadow; no `core/`, `store/`, `equality/`, or hash-preimage
change.

### (A) The three erasure classes

Partition the inner builtin members (fib / interval / face / sys / path / kan,
and the R-GLUE/R-SIGMA groups when they land) by *what survives erasure*:

**Class 0 — runtime-irrelevant (erase to `IUnit`).** No computational content at
all; same status as `Eq`/`Refl`/`Prop` (`erase.go:79`).
- The face lattice: `F`, `ieq0`, `ieq1`, `fand`, `for`, `ftop`, `fbot`
  (`store/face.go`). Cofibrations are *constraints on interval variables*; a
  closed compiled program has no interval variables, so faces carry nothing. They
  are already type-/Prop-adjacent data.
- The system proof layer: `holds`, `htop`, `hand`, `horl`, `horr`
  (`store/sys.go`). `holds φ : Prop` — **already erased by the TypedEraser's
  proof-irrelevance rule** (`erase.go:64`, `isProp`). These never needed a taint
  ban; they unit by the existing mechanism.
- The interval *type* `I` and its De Morgan algebra used *only inside a `pabs`
  that itself erases* (see Class 1). The endpoints `i0`/`i1` and `ineg`/`imin`/
  `imax` appearing in a *deployed* position are Class-1-reducible (below), not
  Class 0 — but as arguments to an erased path they vanish with it.
- `UF`, `El`, `fib`, `piF`, `pathF`, `pathU`, `sigmaF` (when it lands) — **type
  formers**, already `typeRefs → IUnit` (`session.go:518-522`, `erase.go:71`).

**Class 1 — λ-reducible (erase to plain IR by NORMALISING FIRST, no `IInner`).**
The kernel already reduces these to λ-calculus; the eraser should *let it*, then
erase the reduct. These need **no** runtime ABI:
- `papp` (all four rules: β, refl-const, boundary i0/i1 —
  `core/eval.go:939-983`). A saturated `papp (pabs A f) i` is `f i` — application.
- `pabs A f` in a deployed position: a path-as-data is `λi. f i` over the
  (erased) interval; since the only *use* is `papp`, and `papp ∘ pabs` β-reduces,
  a *closed* path applied to a *closed* endpoint always reduces. A path that
  escapes as a *value* (stored, returned) erases to `λ_. body` — **the interval
  argument erases to a unit-consuming binder** (it is 0-quantity: an interval
  point carries no runtime data). This is the paths-as-data answer: **a path
  erases to a thunk `λ(erased i). point`; `papp` erases to applying it.**
- `ineg`/`imin`/`imax` on closed interval terms (`core/eval.go`, `tryIntervalIota`)
  — reduce to an endpoint; the endpoint erases to a unit (Class 0).
- `transp` over a *constant* line (`tryTransp` regularity, `eval.go:1127-1128`) —
  reduces to the identity `a0`. Erase the reduct.
- `transp` over a constant-domain `piF` (`eval.go:1142-1168`) — reduces to
  `λx. transp …`; erase the reduct (which recurses into a smaller transp).
- `hcomp`/`comp` on a **total** (`⊤`) system or degenerate (`⊥`-const) system
  (`tryHcomp`/`tryComp`, `eval.go:1204-,1251-`) — reduce to `u i1 htop` / `u0`.
- `castU` over `ureflU` (`eval.go:815`) — the identity (like outer `Subst`).
- `pathJ` over `preflF` (`eval.go:806`) — the `d` branch.

  **The mechanism for Class 1 is one line of policy: erase against the NORMAL
  FORM.** The TypedEraser already evaluates as it goes (`erase.go:47,127,153`); the
  refinement is, *when the eraser meets an `App`/`Ref` whose head is an inner op
  and whose spine is saturated, force/normalise that subterm and erase the
  reduct* instead of emitting an `IGlobal`. Because the kernel's ι-rules fire on
  saturated closed spines, this turns Class-1 ops into plain `IApp`/`IVar`/`ILam`
  with **no `IInner` node and no taint** — they simply compute away.

**Class 2 — payload-carrying (erase to an `IInner`, lowered to the reduct's
function).** The genuinely interesting case: a transport whose erased meaning is
*a real function the runtime must apply*, and which the kernel reduces to that
function (so the payload is recoverable):
- `castU A B (ua A B f g s t) x ~> f x` (`eval.go:818-824`). Erased meaning:
  **apply the forward map.** `castU` over a `ua` path erases to `IApp{f', x'}`
  where `f'` = `erase(f)` (the forward map; `g/s/t` are the inverse + the two
  homotopy proofs — `s,t : Eq …` are Class 0, erased). This is `uaβ` as an
  erasure rule.
- `transp (λi. Glue …) ψ g0` (R-GLUE's rule, A6/A7): erases to the `glue [...]`
  reduct, whose base is a `comp` in `B` (Class-1-reducible when `B` is constant,
  as in the `ua` line) and whose φ-part is the equivalence forward map (Class 2,
  recursively). For the **`ua` specialisation** (constant base, disjoint faces)
  this collapses to the forward map — *exactly the `castU`/`ua` case above*.

  **The mechanism for Class 2 is the same "erase the reduct" policy as Class 1**,
  with one difference: the reduct *still mentions the forward map `f`*, which is an
  ordinary fibrant-layer function (`not`, an equivalence's `.fst`), itself
  erasable to real IR. So Class 2 is **Class 1 where the residue is non-trivial.**
  The `IInner` node is *only* needed when the spine is **stuck** (open / varying
  line) — i.e. Class 3 below.

**Class 3 — genuinely stuck (the residue; still banned).** A transport over a
*neutral or genuinely-varying* line that the kernel does **not** reduce: varying-
domain `piF`, `transp`/`comp` over a non-constant `pathF`, `hcomp` on a proper
face for a non-`piF` former, `pathJ` over a non-refl/non-ua path before A4, a
`ua`/Glue transport whose forward map is itself stuck. These have **no erased
meaning yet** because *the kernel has no normal form for them* (they are the
honest-stuck §F remainder, CLAUDE.md "the labelled remainder"). **These keep an
`IInner{Op,Args}` node and `Backend.Emit` errors on them** — the precise,
structural successor to today's string ban.

> **Key reframing.** The deploy ban is not "the inner layer cannot deploy." It is
> **"a *stuck* inner redex cannot deploy, because there is no value to run."** A
> *reducing* inner redex deploys fine — it erases to its reduct. The vast majority
> of the inner chapters (ch17–ch23, the `ua`/`castU`/`papp` listings) are
> Class 0/1/2 on *closed* terms and **become deployable the moment we erase
> against the normal form.** Only the open §F-frontier residue stays banned, and
> it stays banned *because A1/R-FILL/R-GLUE have not made it reduce* — a kernel
> gap, not an erasure gap.

### (B) Erased lowering — "normalise the inner redex, then erase"

Concretely, in `elaborate/erase.go`'s `check`, add an inner-redex pre-pass:

```
case core.App / core.Ref (saturated inner-op spine):
    if isInnerOpHead(h) && saturatedAt(spine):
        nf := z.el.M.Force( z.el.Eval(c, t) )      // fire the ι-rule
        if reducedAwayFromInnerHead(nf):            // Class 1 / Class 2
            return z.check(c, quote(nf), want)      // erase the reduct
        else:                                       // Class 3: still stuck
            return codegen.IInner{Op: opOf(h), Args: erasedArgs}, nil
```

The `IInner` Args carry the *erasable* arguments only (the forward map for `ua`,
the type-line for `transp`); the interval/face/proof arguments are Class 0 and
erase to `IUnit`. This makes the residue node *carry exactly the data a future
backend would need if R-FILL later teaches it to run* — but until then `Emit`
errors.

**Why force/normalise in the eraser is sound and contained.** The eraser already
drives the Machine (logs deps, `erase.go:30`); forcing a saturated inner spine is
the *same* evaluation the checker did. It reads bodies through `store.Unfold`
(the body barrier), mutates only its own IR output (the Shadow Rule), and adds no
core construct. The cost is that erasure now *normalises* inner subterms (it was
purely syntactic before for non-Prop terms); cap it to inner-op heads so ordinary
data is untouched (Thompson — no speculative normalisation).

**The interval/path-as-data ABI (for paths that escape as values).** A path
`p : El (pathF A x y)` that is *returned or stored* (not immediately `papp`'d)
erases to `λ(0 i:I). body` — a thunk whose binder is the erased interval point.
R-IR's data ABI (the `Repr` table) gets one row:

| inner notion | canonical erased meaning | JS | BEAM |
|---|---|---|---|
| interval point `I` | the erased token (0-qty) | `null`/`$unit` | atom `unit` |
| path value `pabs A f` | `λ_. point` (interval erased) | arity-1 arrow ignoring arg | fun/1 |
| `papp p i` | apply the path to a unit | `p($unit)` | `apply p [unit]` |
| `castU/ua` transport | the forward map `f`, applied | `f(x)` | `apply f [X]` |
| `transp`/`hcomp` (reduced) | the reduct (identity / λ / total value) | (its IR) | (its IR) |
| **stuck transp/comp (Class 3)** | **error — `IInner`, deploy ban** | error | error |

This is the "paths-as-data vs erased-to-identity" answer the node asks for:
**a path is data only insofar as it is a `λ(erased interval). point` thunk; its
*homotopy* content (where faces sit, the proofs) is erased; transport along it is
either the identity (refl/regular) or a stored forward map applied.**

### (C) The op-precise deploy ban (replace the string set)

Delete `internal/session/session.go:621-654` `innerTaint` and the taint-tracking
in `emitDefs` (`session.go:567-613`). The ban moves to `codegen` and becomes
structural, exactly as R-IR specifies (`R-IR.md` §D, "the inner-taint ban moves
into `codegen`"):

```go
// codegen: any reachable IInner is a non-deployable program.
func (b Backend) Emit(p Program, opts Options) (Artifacts, error) {
    if op, ok := firstReachableInner(p); ok {
        return Artifacts{}, &InnerStuckError{Op: op}   // names the stuck op
    }
    ...
}
```

`firstReachableInner` walks the IR for an `IInner` node (a sealed-sum case, not a
string compare). Because Class 0/1/2 *never emit* an `IInner` (they erased to real
IR or units), the only programs that error are those with a **genuinely stuck**
inner redex — Class 3. `EmitProgram`/`EmitExpr` (`session.go:428,454`) drop their
taint checks and surface the backend's `InnerStuckError`. The error message names
the op and points at the gating research node ("transport over a varying line:
needs R-FILL / A1; see §F").

**Containment audit.** No `core/`, `store/`, `equality/`, hash-preimage, or
`defFormatVersion` change. The cubical groups are unchanged. `IInner` is produced
by R-IR's erasure; R-ERASE2 only decides *when* it is produced (Class 3 only) and
what its presence means (`Emit` error). The IR stays the disposable shadow.

## Interfaces & signatures to add (Go + Rune surface as relevant)

`codegen/` (extends R-IR's `IInner`):

```go
// ir.go — IInner already defined by R-IR: struct { Op InnerOp; Args []Ir }.
// R-ERASE2 fixes the InnerOp domain to the genuinely-stuck residue only:
type InnerOp int
const (
    IOTranspStuck InnerOp = iota // transp over a varying/neutral line
    IOCompStuck                  // comp over a varying line, proper face
    IOHcompStuck                 // hcomp on a proper face, non-piF former
    IOPathJStuck                 // pathJ over a non-refl/non-ua path (pre-A4)
    IOGlueTranspStuck            // transp over Glue, stuck forward map (pre-A7 deep)
)

// codegen.go — the structural deploy ban (replaces session.innerTaint):
type InnerStuckError struct{ Op InnerOp; Detail string } // names the op + gating node
func (e *InnerStuckError) Error() string
func firstReachableInner(p Program) (InnerOp, bool)
```

`elaborate/erase.go` (the "normalise the inner redex" policy):

```go
// inner returns IR for a saturated inner-op application: it normalises the
// subterm and erases the reduct (Class 1/2); if the reduct is still headed by
// the inner op (Class 3), it emits IInner with the erasable args.
func (z *TypedEraser) inner(c *Ctx, t core.Tm, want core.Val) (codegen.Ir, error)

// isInnerOpHead reports whether a Ref's FibRole/PathInfo/KanRole is a value
// member (castU/papp/transp/hcomp/comp/ua/…), driving the inner() pre-pass.
func (z *TypedEraser) isInnerOpHead(h core.Hash) (core.InnerKind, bool)
```

`core/` — **no change.** (R-ERASE2 reads existing `FibRoleOf`/`PathInfo`/`KanInfo`
reverse lookups; it adds an `InnerKind` *classification helper* in `core` only if
the eraser cannot already distinguish heads — and even that is a pure read, no new
`Tm`/`Val`.)

`internal/session/session.go` — **deletions:** `innerTaint` (`:621-654`), the
`tainted` map + taint loop in `emitDefs` (`:567,610-613`), the taint checks in
`EmitProgram`/`EmitExpr` (`:439-446,469-473`). They become `Emit`-error
propagation.

**No Rune surface change.** R-ERASE2 is entirely below the language.

## Worked micro-example (the teachable artifact)

**The headline: ch10 (`listings/ch10_univalence.rune`) — which today *checks but
does not deploy* — RUNS, printing the same value the kernel computes.** The
deploy ban that `internal/session/session.go:622` lists `ua`/`castU` for is lifted
*for this program* because every inner redex in it reduces.

Source (the ch10 `not`-transport, unchanged):

```
data Bool : U is true : Bool | false : Bool end
not : Bool -> Bool is fn (b:Bool) is BoolElim (fn _ is Bool end) false true b end end
notNot : (b:Bool) -> Eq Bool (not (not b)) b is
  fn (b:Bool) is BoolElim (fn x is Eq Bool (not (not x)) x end) refl refl b end end
boolF : UF is fib Bool end
notPath : pathU boolF boolF is ua boolF boolF not not notNot notNot end
transported : Bool is castU boolF boolF notPath true end   -- = not true = false
main : Bool is transported end
```

Erasure trace (the teachable steps):

```
transported = castU boolF boolF (ua boolF boolF not not notNot notNot) true
  -- inner() pre-pass: castU spine is saturated, head is an inner op.
  -- Normalise:  castU … (ua … not not s t) true  ~>  not true   (eval.go:823, uaβ)
  -- The reduct `not true` is NOT headed by an inner op  ⇒ Class 2.
  -- Erase the reduct:   IApp{ IGlobal "not", ICon true [] }
  -- s,t : Eq …  are Class 0 (proof-irrelevant, erased) — never walked.
```

JS emission (R-IR's `Repr`): `not(TRUE)` → evaluates to `false`. **Prints
`false`.** No `ua`, no `castU`, no `transp` appears in the shadow — *the
univalence transport erased to a plain function call*. The deploy ban does not
fire because **no `IInner` was produced**: the redex reduced.

Contrast — the still-banned case (the honest frontier):

```
-- a transport over a GENUINELY VARYING line (no constant-domain piF shape):
stuck : El A1 is transp (λi. someVaryingLine i) a0 end   -- kernel: STUCK
main : El A1 is stuck end
```

```
  -- inner() pre-pass: transp spine saturated; normalise:  stays headed by transp.
  -- Class 3  ⇒  emit IInner{ IOTranspStuck, [erase(line), erase(a0)] }.
  -- Emit:  *InnerStuckError{IOTranspStuck, "transport over a varying line:
  --         needs R-FILL/A1 (§F frontier)"}.
```

The lesson (Savage): **"Univalence doesn't deploy as magic — it deploys as the
function it computes to. `transport (ua e)` *is* `e`'s forward map; that is what
runs. The only thing that can't deploy is a transport the kernel can't yet reduce
— and that's a kernel gap (R-FILL), not a runtime mystery."** The pedagogy is the
*disappearance*: a learner watches `ua`/`castU` vanish from the compiled output,
leaving `not`. Transport is erasure-irrelevant *except* for the equivalence's
forward map — and that was an ordinary function all along.

## Risks / open sub-questions

1. **Normalising in the eraser may not terminate / may be expensive (Lambert).**
   The "erase against the normal form" policy forces inner subterms. On a *closed*
   well-typed term this terminates (the kernel is normalising on the fragment that
   reduces; the stuck fragment is detected and stops at the stuck head). Risk:
   accidentally normalising *non-inner* subterms (an exponential `Nat`). Mitigation:
   the pre-pass fires **only** on inner-op heads (`isInnerOpHead`), and only forces
   *that* spine, then erases the reduct *structurally* (the reduct's sub-data is
   erased by the ordinary type-directed walk, not re-normalised). Cap is exact.

2. **Open terms under a binder.** Inside a `λ`, an inner redex may have a *variable*
   interval/point and not reduce, yet the *whole function* is deployable once
   applied. Today's def-level taint over-bans these. Open sub-question: do we
   (a) keep them as `IInner` and ban (conservative, matches today), or (b) erase
   the path binder to `λ(0 i). body` and let `papp` apply it (the paths-as-data
   ABI), deploying the function? **Recommend (b) for `papp`/`pabs` (paths-as-data,
   honest and teachable), (a) for `transp`/`comp` (a transport under an open line
   genuinely has no value until R-FILL).** This is the precise line between "path
   data deploys" and "stuck Kan does not."

3. **Co-evolution with A7 / R-UA.** R-UA re-derives `ua` from Glue, so the *reduct*
   of `castU … (ua …)` will route through `transp`-over-`Glue` rather than the
   bolted `eval.go:823` rule. **R-ERASE2's "erase the reduct" policy is robust to
   this**: it erases whatever normal form the kernel produces. The only coupling:
   the Glue reduct's *base comp over the constant `ua` base* must itself be
   Class-1-reducible (it is, by regularity — R-UA Decision 3 / Risk 3), else the
   `ua` transport becomes Class 3 (banned) until R-FILL. **So R-ERASE2 deploys
   `ua` exactly when the kernel reduces it — which is *today* (bolted rule) and
   *after A7* (derived), provided regularity stays on (C-REG).** If C-REG drops
   regularity, the `ua` transport reverts to a path-not-a-reduction and **becomes
   Class 3** — deploy of `ua` is then gated on a richer erased Glue runtime. Flag
   this coupling in the C-REG decision record.

4. **Glue/`unglue` runtime when faces are *not* closed (post-A6).** A `glue`
   value with a live face needs the equivalence forward maps stored at runtime
   (Class 2 with real residue). This is genuinely a *runtime data structure*
   (a tagged value carrying the base + the φ-maps), not just a function call —
   the first place the inner layer has non-trivial *runtime representation*.
   **Research, deferred to A6/A7 landing**: the `glue`/`unglue` `Repr` row. R-ERASE2
   ships the *closed-program* erasure (where faces decide and Glue collapses to its
   base or its forward map) ready-to-build; the *open-face Glue value* ABI is
   labelled research, downstream of R-GLUE.

5. **Frame Lemma interaction (R-FRAME / X1).** Erasing against the normal form
   means the eraser *forces* inner redexes, logging their dependency hashes (the
   Glue/Kan groups) — exactly the cache-key growth R-UA Risk 6 and R-FRAME flag.
   This is *correct* (the deployed code genuinely depends on those reductions) but
   the eraser's dep log must not pollute the *checker's* certificate. Mitigation:
   the eraser logs are emit-time only (`erase.go:30` already notes this is
   harmless at emit time) — confirm under R-FRAME that emit-time forcing does not
   write into the proof cache.

6. **Conformance oracle across the Class-1/2 boundary (M4).** A program that
   deploys via "erase the reduct" must produce the *same observable* on every
   backend as the kernel normal form. The risk is a backend whose `not`/`Bool`
   `Repr` disagrees. Mitigation: this is R-IR's `$show`-from-`Repr` conformance
   gate; R-ERASE2 adds the `ua`/`castU` programs (ch10) to the cross-backend
   corpus as the univalence-deploys regression.

## Test/gate plan

- **Listing deploy gate (the headline):** `listings/ch10_univalence.rune` (and the
  ch17–ch23 closed cubical listings) must now `rune run` and print the kernel
  normal form — promoted from "checks" to "checks + runs". Gate from
  `harness/listings_test.go` with a `runNode` assertion (the `codegen_test.go:33`
  pattern). This is the literal lifting of the deploy ban.
- **Erase-the-reduct correctness (the gluing property, arXiv:2605.00655):** for a
  corpus of closed inner terms, `run(erase(t)) == observe(NormalizeUnfold(t))`
  (`session.go:365` is the kernel oracle). Property-tested over generated
  closed `castU`/`papp`/`transp`-constant terms.
- **Class-3 ban precision:** a *stuck* transport (varying line) must still error
  at `Emit` with the op named (regression for `session.go:439-446`'s behaviour,
  now structural); a *reducing* inner program must **not** error (the over-ban
  regression — today ch10 wrongly bans, must now pass). Reuse/replace the
  `internal/session` taint tests.
- **No `IInner` for Class 0/1/2:** assert the erased IR of ch10 contains **zero**
  `IInner` nodes and zero `IGlobal{ua|castU|papp}` (they reduced/erased away).
- **Proof-irrelevance regression:** `holds`/`htop`/face members never reach the IR
  (already unit by `erase.go:64`) — assert no `IGlobal{htop|ftop|…}` in any shadow.
- **Containment / Shadow-Rule audit (CI):** no `core/`/`store/`/`equality/`/
  hash-preimage diff; `defFormatVersion` unchanged; `innerTaint` deleted from
  `session`. Mechanical.
- **Cross-backend conformance (M4 seam):** ch10 produces equal `$show` on JS and
  (when B3 lands) BEAM — univalence transport deploys identically.
- **C-REG coupling pin (Risk 3):** a test asserting `ua` deploys *iff* the kernel
  reduces `castU … (ua …)` (regularity on); flagged to break knowingly if C-REG
  drops regularity.

## Unblocks (which implement nodes, and what they still need)

- **B5 (lift the inner-taint deploy ban)** — *this is B5.* R-ERASE2 delivers: the
  three-class classification (**ready-to-build now**), the "erase the reduct"
  policy for Class 1/2 (**ready-to-build now** — the kernel already reduces these,
  `eval.go` cited throughout), the structural op-precise ban replacing
  `session.innerTaint` (**ready-to-build now**, consumes R-IR's delivered `IInner`),
  and the paths-as-data `λ(0 i). point` ABI row (**ready-to-build** for closed
  programs). *Still needs:* R-IR/B1 landed (delivered, ready) and B2 (JS on the
  new IR) for the deploy target; **A7/R-UA** only for the *derived* (vs bolted)
  `ua` reduct — and R-ERASE2 is robust to either, so it does **not hard-block on
  A7** for the bolted-rule path that exists today.
- **M2 (Univalence COMPUTES) → deployable.** A7 makes `ua` compute *in the kernel*;
  R-ERASE2 makes that computation *deploy* (erase to the forward map). Together
  they turn the ch10/ch25 `refl`-certified univalence into a *running* artifact —
  the Lambert half of the M2 spirit gate (a deployed artifact, not just a passing
  check).
- **M0 (vertical slice on BEAM)** — R-ERASE2 ensures the slice's inner content (if
  any) either deploys (Class 0/1/2) or fails *loudly and precisely* (Class 3),
  rather than being silently miscompiled. The slice itself stays mostly outer-core;
  R-ERASE2 guarantees the boundary is honest.
- **A6/A7 deep Glue deploy (open-face `glue` values)** — R-ERASE2 leaves the
  *runtime representation of a live-face `glue` value* as labelled research
  (Risk 4), downstream of R-GLUE. The closed-program collapse deploys now; the
  open-face data structure is the next increment.

---

**Status: partly ready-to-build, partly gated.** Ready-to-build *now*: the
classification, the "erase the reduct → plain IR for Class 1/2" policy (the kernel
already reduces these — `core/eval.go` cited), the structural op-precise ban
(consuming R-IR's delivered `IInner`, deleting `session.innerTaint`), and the
closed-program deploy of ch10's univalence transport (it erases to `not`).
**Gated/research:** the open-face `glue`-value runtime representation (downstream
of R-GLUE/A6), and the deploy of *genuinely stuck* transports (Class 3 — a kernel
gap on R-FILL/A1, not an erasure gap). The containment line holds: all shadow, no
core growth, no hash bump — the same move R-IR made, applied to the *meaning* of
the inner ops rather than their *marking*.

Sources:
[Cubical — Agda docs (`--cubical=erased`, Glue erased)](https://agda.readthedocs.io/en/latest/language/cubical.html) ·
[Run-time Irrelevance — Agda docs](https://agda.readthedocs.io/en/latest/language/runtime-irrelevance.html) ·
[Cubical Agda: A Dependently Typed Programming Language (Vezzosi–Mörtberg–Abel)](https://staff.math.su.se/anders.mortberg/papers/cubicalagda2.pdf) ·
[1Lab — Univalence (uaβ = transport applies the equivalence)](https://1lab.dev/1Lab.Univalence.html) ·
[Cohen–Coquand–Huber–Mörtberg, Cubical Type Theory (PDF) §6](https://www.cse.chalmers.se/~coquand/cubicaltt.pdf) ·
[Type Theory With Erasure (presheaf extraction to untyped λ-calculus)](https://arxiv.org/html/2605.00655)
