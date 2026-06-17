# R-PART — Partiality / general recursion

> Roadmap node C4 (`C4 [I] partiality/general-recursion mechanism ⇐ R-PART,
> C-PARTLAW`). Resolves the clarify node **C-PARTLAW** (where the partiality
> boundary sits). Telos-3 enabler: D2 (verified collections) lists C4 as a direct
> dependency (`humble-humming-elephant.md:216`), and R-EFFECT flagged unbounded
> effectful loops (`forever : IO a`, an echo *server*) as blocked on this node
> (`ref_docs/wootz/R-EFFECT.md:381-383`).

## Problem (what's stuck/absent today, with file:line)

The Rune outer core is **total by construction**, and totality is enforced by a
structural device, not a checker:

- **The eliminator is the only recursion principle.** Phase 4 declares a datatype
  as a former + constructors + a generated eliminator, all *bodiless* permanently
  neutral heads; the eliminator computes by the ι-rule in the evaluator, firing
  when the scrutinee forces to a saturated constructor, "with induction hypotheses
  for recursive arguments" (CLAUDE.md, Phase 4). There is **no `fix`, no `letrec`,
  no `Tm` node for self-reference**. `core/term.go` enumerates the whole `Tm` set
  (`Var, Meta, Prop, Eq, Refl, Cast, Subst, Ref, Univ, Pi, Lam, App, Let, Ann` +
  the Phase-6 universe levels) — nothing names itself.

- **A reference to a not-yet-defined name is rejected.** `session.AddDef`
  (`internal/session/session.go:139-140`): *"A reference to a not-yet-defined name
  is rejected — recursion arrives with Phase-4 totality."* The acyclic resolver is
  the only one wired: `store.HashSCC` + positional `Placeholder` lay down
  SCC-as-unit hashing, "but the resolver wires only the acyclic case; the CLI
  rejects recursive groups" (`PARKING-LOT.md:51-54`). The stated reason is exactly
  the soundness one: *"Cyclic resolution lands WITH Phase-4 totality — unchecked
  recursion would let conversion diverge"* (`PARKING-LOT.md:51-54`).

- **Why this is load-bearing, not incidental.** Conversion is decided by the NbE
  machine (`core/eval.go`, `core/conv.go`); `m.tryRules` fires ι eagerly inside
  `apply` (`core/eval.go:497`). If a definition could refer to itself with an
  unfolding body, `refVal`'s glued `Unfold` thunk (`core/eval.go:405-425`) would
  loop: forcing the head forces the body, which forces the head. **Termination of
  the type-checker IS the totality of the term language here** — there is no
  separate "runtime may diverge but checking is fine" stratum, because checking
  *runs* terms (NbE). That is the precise thing C-PARTLAW must protect.

- **Consequences today.** Every stdlib algorithm that is not structurally
  recursive has no home: `gcd` by Euclid (recursion on the *remainder*, not a
  constructor sub-term), `quicksort`/`mergesort` (recursion on a *partition*, not
  a tail), `Map`/`Set` rebalancing, fixpoint iteration, a parser's backtracking
  loop, an interpreter's `eval`, a `while`/server loop. The parking lot already
  names the gap twice: *"General recursive definitions + termination checking …
  A termination checker arrives only if a listing cannot be written
  eliminator-style"* (`PARKING-LOT.md:31-33`) and the recursive-resolution park
  (`PARKING-LOT.md:51-54`). D2 (List/Vec/Map/Set/Str) cannot be written
  eliminator-only without contortion; that is the listing that finally demands it.

So the question C-PARTLAW poses is sharp: **how do we let the typed leaves of the
stdlib run non-structural algorithms without (a) growing the outer core, (b)
making the proven core unsound, or (c) making the type-checker diverge?**

## Prior art (what the literature/other systems do; cite)

Four families. Each trades a different cost.

1. **Termination certificates that stay inside the total core (Bove–Capretta;
   well-founded / accessibility recursion).** Rewrite a generally-recursive `f`
   into an *accessibility predicate* + a structurally-recursive function over a
   proof that the argument is accessible. Idris records a function `total` only if
   "by the time a sequence of recursive calls reaches `f` again … one of its
   arguments has decreased"; for non-structural cases the **Bove–Capretta method
   (2005)** "rewrites programs that make use of general recursion into a pair of
   an accessibility predicate and a function defined by recursion over the
   predicate," and the accessibility proof is *collapsible* — erased at runtime.
   ([Idris 2 totality](https://idris2.readthedocs.io/en/latest/tutorial/theorems.html),
   [Idris totality notes](https://idris-community.github.io/idris2-tutorial/Tutorial/Folds/Totality.html))
   - **Cost:** *zero* metatheory cost — the result is an ordinary total term. The
     cost is *to the programmer*: you must supply (or have inferred) the
     well-founded order and the descent proof. This is the **proven-core** path:
     it stays total, it just needs `Acc`/`WellFounded` as a library datatype.

2. **Sized types (Abel; MiniAgda; Agda 2.4+).** Annotate datatypes with a size
   index; "for recursive calls it is checked that the sizes decrease, which by
   well-foundedness entails termination," handling "complex patterns of recursion
   … with mixed inductive and coinductive types," with the advantage of
   *compositionality* over the syntactic `foetus` checker.
   ([MiniAgda, Abel](https://www2.tcs.ifi.lmu.de/~abel/par10.pdf),
   [Well-founded recursion with copatterns and sized types, JFP 2016](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/wellfounded-recursion-with-copatterns-and-sized-types/39794AEA4D0F5003C8E9F88E564DA8DD))
   - **Cost:** sizes are a *pervasive type-system extension* — a size sort, size
     quantification, subtyping on sizes, and a delicate metatheory (naive `∞`
     handling was a known Agda inconsistency source). This is **Thompson-hostile**:
     it grows the outer core's sort structure and conversion. It buys more
     accepted-without-proof programs than Bove–Capretta, but it is exactly the
     "powerful but pushes complexity into the type system" cost R-EFFECT warned
     against for effect rows (`R-EFFECT.md:92-96`).

3. **Fuel / step-indexing (the engineer's escape hatch).** Make the function total
   by adding a `Nat` budget that structurally decreases; `f : Fuel -> A ->
   Option B`, return `none` when fuel runs out. Always total, always structural,
   no new theory. Used everywhere in practice (CompCert's `fuel`, Coq's
   `Function … {measure}` fallback, hand-rolled interpreters).
   - **Cost:** the function's *type lies a little* — `Option B` even when the
     algorithm always succeeds; the caller must thread/refute fuel; reasoning
     about it is awkward (every theorem is "for sufficient fuel"). It is the most
     teachable and the least honest.

4. **The partiality (delay) monad — Capretta; coinductive `D A`.** A *value*
   `D A`, coinductively generated by `now : A -> D A` and `later : D A -> D A`,
   with the infinitely-delayed `⊥ = later ⊥`. General recursion becomes a *total*
   corecursive definition producing a `D A`; "the delay monad provides a way to
   introduce general recursion in type theory." The honest equality is **weak
   bisimilarity** (ignore finite delays); the *quotient* `D A / ≈` is the true
   partiality monad. Two sharp results: (i) quotienting and keeping a *monad*
   needs **countable choice** in plain MLTT (Chapman–Uustalu–Veltri); (ii) you can
   sidestep choice by building the partiality monad **directly as a
   quotient-inductive-inductive / higher-inductive type** (Altenkirch–Danielsson–
   Kraus), which is the modern HoTT answer.
   ([Capretta, General Recursion via Coinductive Types](https://arxiv.org/pdf/cs/0505037),
   [Quotienting the Delay Monad by Weak Bisimilarity, Uustalu–Veltri](https://cs.ioc.ee/~niccolo/ictac15.pdf),
   [The Partiality Monad as a QIIT, Altenkirch–Danielsson–Kraus](https://arxiv.org/pdf/1610.09254),
   [Veltri thesis, A Type-Theoretical Study of Nontermination](https://digikogu.taltech.ee/en/Download/b4fad99e-0fc0-4ecc-8df1-36af5172135a/ATypeTheoreticalStudyofNontermination.pdf))
   - **Cost:** `D A` is a **coinductive** type, and the Rune core is "inductive-
     only" (`humble-humming-elephant.md:120`, R-COIND). A *clean* `D` needs
     R-COIND; a *quotiented* `D/≈` needs either choice (unavailable/unwanted) or a
     HIT (R-HIT, inner stratum). So the fully-principled partiality monad is the
     most expensive of the four on *this* substrate.

Cross-cutting lessons for the three smiths:

- **Idris's design is the C-PARTLAW template.** Idris does not pick one mechanism;
  it has a **totality lattice** — `total` / `covering` / `partial` — and
  `assert_total` as the explicit, auditable escape hatch (`assert_total x`
  "asserts that the expression x is terminating and covering, even if the totality
  checker cannot tell"). Consistency comes from `total`; `partial` "does not
  ensure" termination. ([Idris misc](https://docs.idris-lang.org/en/latest/reference/misc.html))
  The lesson: **the boundary is a per-definition label, machine-tracked and
  propagating, not a global mode.** That is precisely C-PARTLAW.
- **Bove–Capretta is the bridge between tiers.** A program written generally can
  *later* be promoted to total by supplying the accessibility proof, *without
  changing its computational content* (the proof erases). This is the Savage
  on-ramp ("I tested it" → "I proved it") at the recursion level.
- **A partiality boundary that contaminates is the danger.** If a `partial`
  definition could be `Unfold`-ed during conversion of a `total` one, the checker
  could diverge. So the boundary must be a *firewall the kernel respects*, exactly
  like the inner-taint firewall already in `session.go` (`innerTaint`,
  `EmitProgram` refusal, `session.go:439`, `:621`).

## Chosen approach for THIS substrate (concrete; respects containment)

**C-PARTLAW, resolved: a three-tier totality lattice with a kernel-respected
firewall. The proven core is total (Tier 0); typed leaves opt into a contained
partiality construct (Tier 1) that the kernel REFUSES TO UNFOLD during
conversion; the promotion path between tiers is Bove–Capretta (no metatheory
cost). The partiality construct is a `fuel`-backed library `Delay`, NOT a core
fixpoint and NOT (yet) a coinductive `D`.**

This is the Idris totality-lattice design transplanted onto Rune's *containment*
discipline, with the mechanism choice forced by Thompson.

### Tier definitions (this IS C-PARTLAW)

| Tier | Label | What it may use | Kernel treatment | Deploys? |
|------|-------|-----------------|------------------|----------|
| **0** | `total` (default) | eliminators only; may call Tier-0 | unfolds freely; ι fires; certified | yes |
| **1** | `partial` (opt-in) | `Delay`/`fuel`, may call Tier-0 **and** Tier-1 | **never unfolded in conversion**; rigid head | yes |
| **promotion** | `total via wf` | Bove–Capretta `Acc` recursion | unfolds freely (it *is* Tier 0) | yes |

The default is `total`; a definition becomes `partial` only by an explicit
surface annotation (`partial name : T is … end`). `partial` is **infectious
upward**: any definition that *calls* a `partial` def is itself at most `covering`
unless it goes through `Delay`-eliminating glue (below). This mirrors the existing
`innerTaint` propagation (`session.go:621-654`, "a previously-tainted definition")
— the machinery is already in the codebase; R-PART adds a *second taint colour*.

### Why fuel-backed `Delay` and not a core `fix`, not sized types, not raw coinduction

- **Not a core `fix` / `letrec`.** A `Tm` self-reference node is the one thing
  that would let conversion diverge (Problem). It also forces a hash-format bump
  and an SCC body-barrier rework. Thompson vetoes it. The whole point of C-PARTLAW
  is to get general recursion *without* a divergent kernel.
- **Not sized types.** They grow the outer core's sort structure and conversion
  (Prior art 2) — the metatheory-review-event cost the roadmap reserves only for
  C-OVERLAP/R-SUM (`humble-humming-elephant.md:84-86`). R-PART does not qualify.
- **Not (yet) a genuine coinductive `D A`.** The honest Capretta delay monad is
  coinductive, and the core is inductive-only; that is R-COIND, a *separate*
  research node (`humble-humming-elephant.md:121`). R-PART must ship *before*
  R-COIND lands (D2 needs it, E-track is the tail). So Tier 1's runnable
  construct is the **fuel approximation** of `Delay`, which is *structural*
  (hence total *as a term*) and needs **zero new core and zero new theory**:

```
-- a contained library datatype (Phase-4 data machinery — NO core change):
data Delay (A : U) : U is
  now   : A -> Delay A
  later : Delay A -> Delay A          -- finite, INDUCTIVE: this is the fuel
end
-- "run" a Delay with a step budget; STRUCTURAL on the Nat (total, Tier 0!):
runFor : {A : U} -> Nat -> Delay A -> Option A
  -- runFor zero      _          = none
  -- runFor (succ n) (now a)     = some a
  -- runFor (succ n) (later d)   = runFor n d        (NatElim/DelayElim — total)
```

The trick that resolves the tension: **`Delay A` as written above is an ordinary
inductive datatype** (finite `later` chains), so `runFor` is *Tier-0 total* by the
eliminator. What is *not* total is **building an infinite `Delay`** — a
generally-recursive `f` that emits `later (f …)` forever. That construction is the
*only* thing that needs Tier 1, and Tier 1 is exactly "a definition the kernel
will not unfold." So the design splits cleanly:

- **Producing** a possibly-infinite `Delay` (the recursive algorithm) = **Tier 1
  `partial`**, rigid in conversion.
- **Consuming** it with a finite budget (`runFor`) = **Tier 0 total**, computes
  normally.

This is the fuel mechanism (Prior art 3) *dressed as* the delay monad (Prior art
4) so the surface and the eventual R-COIND upgrade line up: when R-COIND lands,
`Delay` becomes the *coinductive* `D`, `runFor` becomes the bona-fide weak-
bisimilarity eliminator, and **Tier-1 `partial` defs that produced `Delay`
become genuinely total corecursive defs** — *the surface does not change.* R-PART
ships the teachable, runnable approximation; R-COIND promotes it to principled.

### How the firewall is implemented (the one genuinely new mechanism)

The kernel must *never read a `partial` body during conversion*, or the checker
can loop. This is a one-field extension of the existing `rigidHead` machinery
(`core/eval.go:432-463`), not a new theory:

- A `partial` definition is registered with a **rigidity flag**. `refVal`
  (`core/eval.go:405-425`) and `rigidHead` already gate whether a head is unfolded.
  Add: `if m.Partial != nil && m.Partial.IsPartial(h) { return VNeu{Spine:
  NRef{Hash: h}} }` to `rigidHead` — a `partial` head is **permanently neutral in
  the kernel**, exactly like a constructor or a fibrant value member. It never
  unfolds, so conversion *cannot* diverge on it: two `partial` calls are equal iff
  their spines are syntactically equal (the fast path in `conv.go`, which "compares
  spines first and forces only on mismatch" — and here there is no forcing).
- This is **sound** because a `partial` def is treated as an *opaque postulate at
  the type level* — its type is public and checked, its body is sealed from
  conversion. It is the body-barrier (`CLAUDE.md` rule 3) applied *to conversion*,
  not just to layering. The proof cache is unaffected: a `partial` def is certified
  for *well-typedness of its body* (the body still type-checks; it just isn't run
  during *other* defs' conversion), and Tier-0 defs that mention it log it as a
  rigid dependency, never unfolding it.
- **Recursion finally becomes expressible** because the SCC resolver
  (`store.HashSCC`, `PARKING-LOT.md:51-54`) can now be wired *for `partial` groups
  only*: a cyclic group is admissible iff every member is `partial`. Tier 0 stays
  acyclic (eliminator-only). This is the safe activation of the parked
  recursive-resolution path, gated by the firewall.

### Deploys, unlike the inner layer

A `partial` def **erases and runs** (Tier 1 is deployable): its erased body is an
ordinary recursive JS function (`codegen/ir.go` `IGlobal`/`ILam`/`IApp`; the JS
backend already emits recursive eliminators, `codegen/js.go:84` `emitElim`). The
runtime *may* diverge — that is the honest meaning of `partial` — but the **type
checker never does**, because the firewall kept the body out of conversion. This is
the crucial difference from the §F inner taint (`innerTaint`, `session.go:621`):
inner-tainted defs *don't deploy* (no erased meaning); `partial` defs *do* deploy
(their erased meaning is "a recursive function that might not return"). So R-PART
adds a *taint colour with the opposite deploy verdict*.

### The promotion path (Bove–Capretta) — proven-core, zero metatheory cost

A `partial` algorithm is promoted to `total` by supplying a well-founded descent
proof, with **no change to its computational content** (the proof erases via QTT's
0-fragment, the existing erasure boundary). The library datatype:

```
-- accessibility: x is accessible if every R-smaller y is accessible
data Acc {A : U} (R : A -> A -> Prop) : A -> U is
  acc : {x : A} -> ((y : A) -> R y x -> Acc R y) -> Acc R x
end
-- well-founded recursion = AccElim (STRUCTURAL on the Acc proof → Tier 0 total):
wfRec : {A : U} {R : A -> A -> Prop} {P : A -> U}
     -> ((x : A) -> ((y : A) -> R y x -> P y) -> P x)
     -> (x : A) -> Acc R x -> P x
```

`wfRec` is `AccElim` — totally structural, Tier 0. A leaf author writes `gcd`
*generally* (Tier 1) to ship fast, then later re-expresses it via `wfRec` over
`<` on the remainder (Tier 0), and the `Acc` argument erases. This is the Savage
on-ramp at the recursion level and the literal "any leaf can be promoted to proven
over time" (`humble-humming-elephant.md:27`).

### Why this respects all three smiths

- **Thompson:** zero outer-core change, zero hash-format bump (no new `core.Tm`;
  `Delay`/`Acc` are Phase-4 datatypes; `wfRec`/`runFor` are eliminators). The
  *only* new kernel surface is one boolean gate in `rigidHead` + a `Partial`
  interface field on the Machine — a contained extension of the *existing*
  rigidity machinery, the same shape as `Fib`/`Kan`/`Quot` (`core/eval.go:30-45`).
- **Savage:** three labels (`total`/`covering`/`partial`) the learner meets one at
  a time; `runFor` is "give it a budget"; the promotion story (`Acc`/`wfRec`) is
  the on-ramp to proofs. Fuel is the most teachable mechanism, and we keep its
  pedagogy while dressing it as `Delay` so the later R-COIND upgrade is invisible.
- **Lambert:** `partial` defs *run* on real backends (recursive functions, real
  divergence possible) — they face reality. A server loop is a `partial Delay`;
  `runFor` is the bounded scheduler quantum. Failure/nontermination is honest, not
  hidden behind a checker lie.

## Interfaces & signatures to add (Go + Rune surface as relevant)

Go (all contained; mirrors the existing `DataInfo`/`FibInfo` pattern, **no
`core.Tm` change, no hash bump**):

```go
// core/eval.go — a new Machine field + interface, mirroring DataInfo (eval.go:37).
type PartialInfo interface {
    IsPartial(Hash) bool   // is this head a Tier-1 partial definition?
}
// add to Machine:  Partial PartialInfo
// rigidHead (eval.go:432) gains, before the other checks:
//   if m.Partial != nil && m.Partial.IsPartial(h) { return true }
// → a partial head is permanently neutral; refVal never reads its body in conv.

// store/store.go — partiality is a per-definition flag on the stored Def.
//   Def gains:  Partial bool
//   Add(name, ty, body) keeps an AddPartial(name, ty, body) sibling that sets it.
func (s *Store) IsPartial(h core.Hash) bool    // implements core.PartialInfo
func (s *Store) AddPartial(name string, ty, body core.Tm) core.Hash

// store/scc.go — admit a cyclic group ONLY if every member is partial.
//   ResolvePartialGroup(defs []Def) error   // rejects a cycle with any total member

// internal/session/session.go — track the label per name, propagate upward.
//   Def gains Tier (Total|Covering|Partial); AddDef reads the surface annotation;
//   a totality-taint walk (twin of innerTaint, session.go:621) downgrades any def
//   that calls a partial def and isn't itself partial → a "covering"/error per
//   the chosen strictness, with an instructive message (Savage).
```

Rune surface (a library + one annotation keyword — **NOT core**):

```
-- the annotation (surface/ only; resolves to the Partial flag, no core node):
partial loop : {A : U} -> A -> Delay A is fn (x) is later (loop x) end

-- library datatypes (Phase-4 data; lib/delay.rune, lib/wf.rune):
data Delay (A : U) : U is now : A -> Delay A | later : Delay A -> Delay A end
runFor : {A : U} -> Nat -> Delay A -> Option A is ...   -- total (NatElim)
data Acc {A:U} (R : A->A->Prop) : A -> U is acc : ... end
wfRec  : ... -> (x:A) -> Acc R x -> P x is ...           -- total (AccElim)
```

Codegen: **no new IR node** (unlike R-EFFECT). A `partial` def erases like any
recursive def to `IGlobal`/`ILam`/`IApp`; `Delay`/`Acc` are ordinary datatypes
(`codegen/js.go` `emitElim`). `partial` is **not** added to the `innerTaint` set
(`session.go:622-627`) — it deploys.

## Worked micro-example (the teachable artifact)

`gcd.rune` — the furnace's first *non-structural* algorithm, shown in all three
tiers so the on-ramp is visible.

**Tier 1 (`partial`) — ship it fast, it runs:**
```
partial gcd : Nat -> Nat -> Delay Nat is
  fn (a b) is
    natCase b
      (now a)                          -- b = 0: done
      (fn (b') is later (gcd b (mod a b))) end   -- recurse on the remainder
  end
-- consume with a budget — this part is TOTAL:
gcdRun : Nat -> Nat -> Option Nat is fn (a b) is runFor 1000 (gcd a b) end
main   : Option Nat is gcdRun 48 36     -- runs, prints (some 12)
```
What the learner sees and can verify:
1. `gcd` checks at type `Nat -> Nat -> Delay Nat`. It is **recursive on the
   remainder** (`mod a b`), which is *not* a constructor sub-term of `b` — illegal
   for an eliminator, legal for `partial`.
2. `gcd`'s body is **never unfolded when checking `gcdRun`** — the firewall. So
   adding `gcd` cannot make any *other* definition's type-check diverge. The
   teaching beat: *"`partial` means the kernel trusts the type and seals the body —
   it will run it, but it won't think about it."*
3. `runFor`/`gcdRun`/`main` are **Tier 0 total** and `main` **deploys and runs**
   (unlike a v3 inner-tainted def). `rune run gcd.rune main` prints `some 12`.

**Promotion to Tier 0 (`total via wf`) — earn the proof later:**
```
gcdT : (a b : Nat) -> Acc lt b -> Nat is
  wfRec (fn (b ih) is natCase b a (fn (b') is ih (mod a b) (modDecreases a b) end) end)
-- modDecreases : (a b : Nat) -> lt (mod a b) (succ b')   -- the descent proof
gcdTotal : Nat -> Nat -> Nat is fn (a b) is gcdT a b (ltWf b) end   -- Tier 0, no Delay
```
The lesson (Savage): *"general recursion is recursion whose descent you haven't
proved yet. Fuel runs it now; `Acc` proves it later; the code in between barely
changes and the proof erases."* The contrast artifact (containment): a Tier-0 def
that tries to call `gcd` directly (not through `runFor`) is flagged
`covering`/error — *you cannot launder partiality into the proven core by calling
it.* The firewall is visible.

## Risks / open sub-questions

- **Strictness of the `partial` taint (the C-PARTLAW knob).** Two policies: (a)
  *strict* — any def that mentions a `partial` head is rejected unless itself
  `partial` or it goes through a `total` eliminator like `runFor` (cleanest
  firewall; recommended); (b) *Idris-style* — a middle `covering` tier that may
  call partial code but isn't itself proven total. **Recommend (a) for M5/D2**
  (binary boundary: total vs partial), add `covering` only if a listing needs the
  middle ground. *Status: ready-to-build for (a); `covering` is a labelled
  increment.*
- **Conversion of two partial defs.** With bodies sealed, `partial f x ≡ partial
  f x` holds only *syntactically* (spine equality) — two extensionally-equal
  partial functions are **not** convertible. This is correct and safe (it is the
  postulate/rigid-head semantics already used for `ua`), but it means you cannot
  *prove* equations about a `partial` def by `refl`. To reason about a partial
  algorithm you must promote it (Bove–Capretta) or reason about `runFor`'s output.
  *Status: by design; documented limitation.*
- **`Delay` is fuel, not true coinduction (the honest gap).** `Delay A` here is
  inductive (finite `later` chains); a `partial` producer can build an *unbounded*
  one only because it is rigid (never forced in conv). The *value* `gcd a b` is, as
  a term, a non-normalizing neutral if you tried to run it without `runFor`. The
  principled fix is R-COIND (a real coinductive `D` + guarded corecursion +
  weak-bisimilarity quotient via R-HIT, per Altenkirch–Danielsson–Kraus). **R-PART
  deliberately ships the approximation; the surface is chosen so R-COIND is a
  drop-in upgrade.** *Status: principled version is research (R-COIND); the
  fuel/`Delay` version is ready-to-build.*
- **Erasure of the `Acc` argument.** Bove–Capretta's selling point is that the
  accessibility proof erases. Rune's QTT 0-fragment + `TypedEraser` already erase
  Prop-typed/0-quantity arguments (`codegen/ir.go` `IUnit`, R-SUM notes the same
  boundary). `Acc R x : U` is *not* Prop, so to erase it cleanly it should be
  declared in `Prop` (it is proof-only) or its argument marked `(0 _ : Acc R x)`.
  *Status: ready-to-build; pick the Prop-valued `Acc` so erasure is automatic.*
- **Interaction with the proof cache / Frame Lemma (R-FRAME).** A `partial` head is
  a rigid dependency: a Tier-0 def's certificate logs it via `Unfold` *without
  reading the body* (the rigid path logs no dep, `eval.go:415-418`). So editing a
  `partial` body does **not** invalidate certificates of total defs that call it —
  which is *correct*, because those defs never depended on the body, only the type.
  But editing the *type* must invalidate. *Status: needs a one-line check that the
  partial def's TYPE is in the dependency key; mostly ready, one cache-keying
  sub-question.*
- **Mutual recursion across the tier boundary.** A `partial` SCC may call Tier-0
  defs freely; a Tier-0 SCC may not call `partial` (firewall). Mutual
  partial-and-total in one `seq`/group is rejected — split the group. *Status:
  ready-to-build (SCC resolver gates on all-partial cycles).*

## Test/gate plan

- **Containment gates (Thompson, must pass):** no change to `core/term.go`,
  `core/hash.go`, `hashFormatVersion`/`defFormatVersion`; a regression asserts an
  existing pure def's hash is byte-identical before/after the `partial` flag and
  `Delay`/`Acc` libraries are registered (the fib/interval groups already prove
  this is achievable).
- **Firewall property (the soundness gate):** a generated *divergent* `partial`
  def (`partial loop : Unit -> Delay Unit is fn (x) is later (loop x) end`) must
  **not** cause `Conv`/type-checking of any *total* def that mentions it to
  diverge — a timed property test in `harness/` (the partial head stays neutral;
  `tryRules` never fires on it). Mutation target: a surviving mutant that makes
  `rigidHead` unfold a partial head must hang the test (caught as a timeout =
  soundness hole).
- **Tier-taint propagation:** a Tier-0 def that calls a `partial` head *not*
  through a total eliminator is rejected with an instructive error; calling it
  through `runFor` is accepted. Twin of the `innerTaint` tests.
- **Run gate (Lambert):** `gcd.rune` runs under node, `gcdRun 48 36 = some 12`;
  the divergent `loop` with `runFor 5` returns `none` (bounded, terminates). A
  listing chapter (next furnace listing, e.g. ch25) is the acceptance gate.
- **Promotion gate (Savage):** `gcdTotal` (the `wfRec` version) checks at Tier 0,
  the `Acc` argument erases to `IUnit` in the shadow (golden-IR test), and
  `gcdTotal 48 36 = 12` runs *without* fuel.
- **Cache regression:** editing a `partial` body does not invalidate a total
  caller's certificate; editing its type does (extend `store/cert_test.go`).
- **SCC resolver:** an all-`partial` cyclic group resolves; a cycle with one total
  member is rejected; this is the safe activation of `PARKING-LOT.md:51-54`.

## Unblocks (which implement nodes, and what they still need)

- **C4 (this node, the implement edge):** **ready-to-build** for the
  fuel/`Delay` + `Acc`/`wfRec` libraries, the `partial` annotation, the
  `rigidHead` firewall gate + `Partial` Machine field, the all-partial SCC
  resolver activation, and the totality-taint walk. Still needs: the chosen taint
  strictness (recommend strict binary; `covering` is a later increment) and the
  cache-keying confirmation that a partial def's *type* (not body) is in its
  dependents' keys.
- **D2 (verified collections: List/Vec/Map/Set/Str):** directly unblocked — the
  non-structural algorithms (mergesort/quicksort, Map/Set rebalancing) ship as
  `partial` leaves first, then promote the ones with clean measures to Tier 0 via
  `wfRec`. Needs D1 (Order, `Nat`, `Option`) and C1/R-SUM (Map/Set are records).
- **R-EFFECT unbounded loops (C3):** unblocked — `forever`/an echo *server* is a
  `partial` `Delay`-producing IO loop; `runFor` (or the backend's scheduler
  quantum) bounds each step. Resolves the dependency R-EFFECT flagged
  (`R-EFFECT.md:381-383`). Needs the R-EFFECT IO base.
- **R-COIND (C5):** R-PART's surface is the *staging ground* for it. When R-COIND
  lands a genuine coinductive `D` + guarded corecursion + (via R-HIT) the
  weak-bisimilarity quotient, the `partial`+`Delay` defs upgrade to true total
  corecursion **without surface change**, and Tier-1 producers that were rigid
  become first-class total values. R-COIND still needs the coinductive core
  machinery and (for the quotiented monad) either countable choice (rejected) or
  the HIT (R-HIT, inner stratum) — both research.
- **E-track (process calculus → actors):** the projection to long-running actors
  bottoms out at `partial` IO loops + bisimulation (R-COIND); R-PART supplies the
  recursion-without-divergent-checker half. Needs E1/E2 and R-COIND.

---

**Status: ready-to-build** for the tiered totality lattice (`total`/`partial`
labels), the `rigidHead` firewall (a partial head is permanently neutral in
conversion — the one new mechanism, a contained extension of existing rigidity),
the fuel-backed `Delay` + `runFor` library, the Bove–Capretta `Acc`/`wfRec`
promotion library, and the safe activation of the parked all-partial SCC resolver
— all with **zero outer-core change and zero hash bump**, repeating the
quotient/fibrant containment pattern with a *second taint colour* (opposite deploy
verdict from §F). **Research/parked:** the principled coinductive partiality monad
(`D`/guarded corecursion/weak-bisimilarity quotient) is R-COIND + R-HIT; sized
types are explicitly rejected (Thompson-hostile); the `covering` middle tier is a
labelled increment. **C-PARTLAW is resolved:** proven core total (Tier 0), typed
leaves may go `partial` (Tier 1) behind a kernel-respected firewall, with
Bove–Capretta as the zero-cost promotion bridge.

Sources:
[Capretta, General Recursion via Coinductive Types](https://arxiv.org/pdf/cs/0505037),
[Quotienting the Delay Monad by Weak Bisimilarity (Uustalu–Veltri)](https://cs.ioc.ee/~niccolo/ictac15.pdf),
[The Partiality Monad as a QIIT (Altenkirch–Danielsson–Kraus)](https://arxiv.org/pdf/1610.09254),
[Veltri, A Type-Theoretical Study of Nontermination](https://digikogu.taltech.ee/en/Download/b4fad99e-0fc0-4ecc-8df1-36af5172135a/ATypeTheoreticalStudyofNontermination.pdf),
[MiniAgda: Integrating Sized and Dependent Types (Abel)](https://www2.tcs.ifi.lmu.de/~abel/par10.pdf),
[Well-founded recursion with copatterns and sized types (JFP 2016)](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/wellfounded-recursion-with-copatterns-and-sized-types/39794AEA4D0F5003C8E9F88E564DA8DD),
[Idris 2 totality](https://idris2.readthedocs.io/en/latest/tutorial/theorems.html),
[Idris totality notes (Bove–Capretta)](https://idris-community.github.io/idris2-tutorial/Tutorial/Folds/Totality.html),
[Idris miscellaneous (assert_total, totality lattice)](https://docs.idris-lang.org/en/latest/reference/misc.html).
</content>
</invoke>
