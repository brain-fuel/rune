# R-UA — ua from Glue; J/castU over ua compute

> **Depends on R-GLUE (A6), and through it R-SIGMA (A5), R-FILL (the keystone),
> and C-OVERLAP.** R-GLUE has no design doc in this fan-out yet; this node states
> the *exact* R-GLUE interface it consumes (§Interfaces, "What R-GLUE must
> deliver") so the two can be co-designed. **Status: needs-more-research** — the
> *retirement plan* (how to swap `ua`/`castU`'s rules onto Glue without a
> hash-format bump for unaffected groups) is ready-to-build and is the deliverable
> of this node; the *compute* itself is gated on R-GLUE + R-FILL.

## Problem (what's stuck/absent today, with file:line)

Today `ua` is a **postulated neutral** and `castU` fires only on two pinned
intro shapes. Concretely:

1. **`ua` is a permanently-neutral head with a four-component telescope.**
   `store/fib.go:201-221` types it
   `ua : (A B : UF) -> (f : El A -> El B) -> (g : El B -> El A) -> (s : …) -> (t : …) -> pathU A B`,
   i.e. a raw bi-invertible-map iso, *not* `(A B : UF) -> Equiv (El A) (El B) -> pathU A B`
   (R-SIGMA §Problem: `Equiv = Σ f, isEquiv f` cannot even be *written* without
   `sigmaF`). `FRoleUa` is recognised (`core/eval.go:150-151`) only so the *one*
   special rule below fires. `ua` is in `rigidHead` (`core/eval.go:444`) — it
   never unfolds, has no body, computes nothing on its own.

2. **`castU` computes on exactly two intro shapes, by head-matching.**
   `tryFibIota`'s `FRoleCastU` case (`core/eval.go:810-826`):
   - `castU A B (ureflU _) x ~> x` (via `fibHeadIs(args[2], FRoleUrefl, 1)`, line 815);
   - `castU A B (ua _ _ f _ _ _) x ~> f x` (forces `p`, matches an `FRoleUa`
     spine of arity 6, returns `m.Apply(pargs[2], args[3])` — `pargs[2]` is the
     forward map `f`, line 818-824).

   This is **`uaβ` baked in by fiat**, not derived. It is sound (transport across
   an equivalence *is* the forward map) but it is a *postulate with a reduction
   bolted on*: there is no Glue, no interval line, no actual transport happening.

3. **`pathJ` over a ua-path is stuck — the labelled §F frontier.**
   `tryFibIota`'s `FRoleJ` case (`core/eval.go:801-809`) fires *only* when the
   path forces to `preflF` (`fibHeadIs(args[5], FRolePrefl, 2)`). A `ua …`-headed
   path never matches, so `pathJ A x P d y (ua …)` is permanently neutral. This is
   the exact gap CLAUDE.md (§F phase 3 "the labelled remainder") and the roadmap
   (telos 1: "path induction over a univalence path computes") call out.
   `store/fib.go:36-38` states it plainly: *"What does NOT compute: pathJ on
   ua-paths and every higher coherence."*

4. **Consequence for deploy.** Because `ua`/`castU` have no honest computational
   content (just the bolted rule), every definition mentioning them is
   inner-tainted: `internal/session/session.go:622` lists `"ua"`, `"castU"`,
   `"pathJ"` in the `innerTaint` set, and `EmitProgram` refuses a tainted `main`
   (`session.go:439-446`). ch10 (`listings/ch10_univalence.rune`) elaborates and
   checks but cannot run.

The task: re-derive `ua` as `⟨i⟩ Glue B [(i=0)↦(A,e), (i=1)↦(B, idEquiv)]`, so
that **`castU` over a ua-path computes *because transport over the Glue line
computes*** (not by fiat), and **`pathJ` over a ua-path computes** through the
general comp-based J (A4). Then retire the postulate — without bumping the hash
format for the interval / face / sys / path / sigma / kan groups, or ch09–ch23,
which do not change.

## Prior art (what the literature/other systems do; cite)

The whole point of CCHM is that `ua` is **not an axiom** — it is a *definition*
whose computation rule (`uaβ`) is a *theorem about transport over Glue*.

- **CCHM / cubicaltt.** `ua` is literally a one-line Glue:
  ```cubicaltt
  ua (A B : U) (e : equiv A B) : Path U A B =
    <i> Glue B [ (i = 0) -> (A, e), (i = 1) -> (B, idEquiv B) ]
  ```
  and `uaβ : Path (A -> B) (transport (ua A B e)) e.1` is *provable*, falling out
  of `comp`/`fill` over the Glue line (cubicaltt `examples/univalence.ctt`). The
  Glue at `i1` is `B` with `idEquiv`, so the line's right endpoint is `B`
  definitionally; at `i0` it is `A` glued to `B` along `e`.

- **Cubical Agda / 1Lab.** Identical:
  ```agda
  ua : A ≃ B → A ≡ B
  ua e i = Glue B (λ { (i = i0) → (A , e) ; (i = i1) → (B , idEquiv B) })
  uaβ : (e : A ≃ B) (x : A) → transport (ua e) x ≡ equivFun e x
  uaβ e x = transportRefl (equivFun e x)
  ```
  `uaβ` is `transportRefl ∘ equivFun` — i.e. transport over `ua e` *computes* to
  `equivFun e x` up to the (regular) `transport` over the constant base `B`, which
  is itself the identity by regularity. **This is exactly the rule
  `core/eval.go:818-824` hardcodes — the difference is whether it is a postulate
  with a bolted reduction or a derived consequence of the Glue transport rule.**
  ([1Lab Univalence](https://1lab.dev/1Lab.Univalence.html),
  [Cubical.Foundations.Univalence](https://agda.github.io/cubical/Cubical.Foundations.Univalence.html),
  [agda/cubical Glue.agda](https://github.com/agda/cubical/blob/master/Cubical/Core/Glue.agda))

- **The transport-over-Glue equation (the load-bearing CCHM rule, R-GLUE's
  deliverable, consumed here).** From CCHM §6 / cubicaltt:
  `transp (λi. Glue (B i) [φ ↦ (T i, e i)]) ψ g0` computes to a `glue [...]`
  whose base part is a `comp` in `B` and whose `φ`-part transports through the
  equivalences. For the *ua line specifically* — where `φ = (i=0) ∨ (i=1)`, `B`
  is **constant**, `T` is constant (`A` then `B`), and the equivalences are `e`
  then `idEquiv` — the equation collapses: the base `comp` over constant `B` is
  the identity (regularity), and the `φ`-transport at the live face is `equivFun
  e`. That collapse *is* `uaβ`. ([CCHM paper](https://www.cse.chalmers.se/~coquand/cubicaltt.pdf) §6,
  §7.2.)

- **J over a univalence path.** In CCHM `pathJ` (`J`) is *not* primitive on Glue;
  it is the general `comp`-based eliminator (`J A x P d y p = comp (λi. P (p i)
  (λj. p (i∧j))) ⊥ … d`, the "transport the motive along the path" formulation).
  So **once A4 (pathJ-via-comp on a general path) lands, J over a ua-path computes
  for free** — there is *no ua-specific J rule*; J reduces by transporting the
  motive along the Glue line, and the Glue line transports by the rule above. This
  is the central architectural fact for R-UA: **we do not write a `pathJ`-on-`ua`
  ι-rule; we delete the special-casing and let A4 + R-GLUE do it.**

- **The builtin-group / containment pattern** (CLAUDE.md v2/v3/§F; R-SIGMA
  Decision 4): every stratum extension is *a group of bodiless content-addressed
  defs with neutral heads and ι-rules in the evaluator, on its own hash space, no
  hash-format bump*. R-UA is unusual: it does **not add** a group; it *changes the
  reduction behaviour of an existing member (`ua`)* and *retires its postulate
  status*. The discipline question this raises (how to do that without a
  hash-format bump for unaffected groups) is the heart of §Chosen approach.

## Chosen approach for THIS substrate (concrete; respects containment)

The North Star: **`castU` and `pathJ` over a ua-path must compute *because the
Glue line computes*, not because a rule is pinned to the `ua` head.** The bolted
`uaβ` rule (`core/eval.go:818-824`) and the bolted `J`-on-`preflF`-only rule are
*replaced* by genuine reduction through Glue + comp. Five decisions.

### Decision 1 — `ua` becomes a *defined* term: a `pabs`-style line of Glue, not a postulate.

R-GLUE delivers `Glue : (B : UF) -> (φ : F) -> (T : holds φ -> UF) -> (e : holds
φ -> Equiv (El (T ·)) (El B)) -> UF` (the precise shape is R-GLUE's; §Interfaces
fixes the minimum). With it, define the *body* of a new builtin `uaGlue` (or
re-body `ua` — Decision 4) as the path

```
ua A B e  :=  pabs UF-as-path-carrier (λi. Glue B ((i=0)∨(i=1))
                  (λh. caseFace h A B)
                  (λh. caseFace h e (idEquiv B)))   :  pathU A B
```

Subtlety: `pathU A B : U1` and its inhabitants are produced by `ureflU`/`ua`, not
by `pabs` (which produces `El (pathF …)`, a point-level path). **The universe-
level path `pathU` is a `pathF`-of-`UF` morally, but `UF : U1` is not itself a
`UF` code**, so `pabs`/`papp` (which quantify over `A : UF`) do not directly
apply. Two routes:

- **(1a) `pathU` becomes an interval function `I -> UF` at the value level.**
  Re-body `ureflU A := λi. A` and `ua A B e := λi. Glue B …`, and re-body `castU`
  as `transp` over that line. This needs `pathU A B` to *decode* (like `El`) to
  `I -> UF`, i.e. a `papp`-analogue at the universe level (`pappU : pathU A B -> I
  -> UF`, with boundary `pappU p i0 ~> A`, `pappU p i1 ~> B`). This is the
  **honest CCHM structure**: paths in the universe *are* interval functions into
  `UF`, and `castU` *is* `transp`.
- **(1b) keep `pathU`/`castU`/`ureflU`/`ua` as opaque heads, but give `ua` and
  `castU` bodies that route through Glue+transp internally.** Lighter on surface,
  but `pathJ` over `ua` still cannot reduce (J needs to *see* the line), so it
  fails the second half of the task.

**Choose 1a.** It is the only route that makes *both* `castU` *and* `pathJ`
compute, because both become "do something with the interval line `I -> UF`", and
that line is a Glue. The cost is a `pappU`/`transpU` pair of universe-level
analogues of `papp`/`transp` — but those are exactly what the CCHM universe-as-
fibrant-type needs, and R-UFH (A10) wants them too.

### Decision 2 — `castU` is *defined as* `transpU` over the `pathU` line; the bolted rules are deleted.

Re-body (Decision 4 mechanics):

```
castU A B p x  :=  transpU (λi. pappU p i) x
```

where `transpU : (L : I -> UF) -> El (L i0) -> El (L i1)` is the universe-level
transport — *which is just the existing `transp`* (`store/kan.go:130-133`:
`transp : (A : I -> UF) -> El (A i0) -> El (A i1)`). So **`castU` is literally
`transp` applied to the decoded `pathU` line.** Then:

- `castU A B (ureflU A) x` → `transp (λi. A) x` → **regularity rule
  (`tryTransp`, `core/eval.go:1126-1129`) fires** → `x`. *The `ureflU` special
  case in `core/eval.go:815` is deleted; it is now an instance of transp
  regularity.*
- `castU A B (ua A B e) x` → `transp (λi. Glue B [(i=0)↦(A,e),(i=1)↦(B,id)]) x` →
  **the transp-over-Glue rule (R-GLUE) fires** → after the regularity collapse of
  the constant base `B`, `equivFun e x` = `e.fst x`. *The `ua` special case in
  `core/eval.go:818-824` is deleted; it is now an instance of transp-over-Glue.*

This is the crux: **the two hand-pinned `castU` rules become two derived
consequences of rules that already exist (regularity) or that R-GLUE delivers
(transp-over-Glue).** `uaβ` stops being a postulate.

### Decision 3 — `pathJ` over a ua-path computes via A4 (pathJ-via-comp), with *no* ua-specific rule.

A4 (DAG line 174: "pathJ-via-comp on a general path") makes `pathJ A x P d y p`
reduce by composing the motive along `p`. Once `pathJ` is the comp-based
eliminator, **a ua-path is just a path** — `pathJ` transports `d` along the line
`λi. P (pappU-point p i) …`, which contains the Glue line, which transports by
R-GLUE's rule. There is *nothing ua-specific to add*. The `FRoleJ`-on-`preflF`-
only rule (`core/eval.go:806-808`) is *subsumed*: refl is the constant line,
comp over a constant line with the degenerate face is the identity (`tryComp`
⊥-degenerate rule already ships, `core/eval.go:1264-1270`), recovering `pathJ … d
y preflF ~> d`. **R-UA's J story is therefore "depend on A4; delete the special
case"** — A4 is the consumer-side prerequisite, R-GLUE the line-side prerequisite.

### Decision 4 — Retire the postulate WITHOUT a hash-format bump for unaffected groups.

This is the delicate engineering deliverable. Three facts make it clean:

1. **No new core constructor.** `ua`/`castU`/`ureflU`/`pathU` already exist as
   `Ref`s; giving them *bodies* (instead of leaving them bodiless) changes the
   store's `Def.Body`, **not** the `Tm`/`Val` set. `defFormatVersion`
   (the hash *preimage* format) does not move. The CLAUDE.md rule holds: *"No
   hash-format bump unless new core constructor."* There is none.

2. **The fib group's *hashes* are a pure function of the *member types*
   (`store/fib.go:53-72`)** — `AddFib` hashes `core.HashTerm(ty)` for each
   member, never the body. **If the member *types* of `ua`/`castU`/`ureflU` do not
   change, the fib group's hashes — and ch09–ch10 — are byte-identical even though
   the *bodies* change.** Bodies are not in the hash preimage (CLAUDE.md Standing
   Rule 2: "Never hash modulo conversion / hashing must never call eval").

3. **Therefore the retirement is: stop registering `ua`/`castU`/`ureflU` as
   `rigidHead` bodiless members, and instead give them stored bodies** (the
   Glue/transp definitions of Decisions 1-2), keeping their *types* fixed. The
   interval/face/sys/path/sigma/kan groups are untouched (own hash spaces); their
   hashes do not move. **The only thing that moves is the fib group's *member
   types* IF we retype `ua` over `Equiv`** — see the fork below.

**The `Equiv` retype fork (the one real hash decision).** R-SIGMA §Unblocks warns
that retyping `ua : (A B : UF) -> Equiv (El A) (El B) -> pathU A B` *does* change
`ua`'s member type, hence the **fib group hash and ch10**. Two sub-options:

- **(4a) Retype `ua` over `Equiv` (clean, costs the fib hash + ch10 once).** Glue
  consumes `Equiv` natively (R-SIGMA, R-GLUE), so this is the honest type. It is a
  *deliberate, scheduled, one-time* fib-group rehash (like the Phase-6 cache nuke
  CLAUDE.md prices in). Because `ua`'s *type* is part of the *whole fib group's*
  digest (`AddFib` hashes all 11 member types into one `group` digest,
  `store/fib.go:57-64`, then derives each member hash from it), changing `ua`'s
  type **rehashes all 11 fib members** — `UF`, `El`, `fib`, `piF`, `pathF`, … all
  get new hashes. That cascades to path/sigma/kan (which embed fib hashes in their
  *types*, `store/path.go:96`, `store/kan.go:111`) — so it is *not* contained to
  ch10; it rehashes the entire cubical stack. **This is the expensive option.**
- **(4b) Keep `ua`'s four-component telescope type; build `Equiv` *internally*.**
  `ua`'s public type stays `(A B : UF) -> (f) -> (g) -> (s) -> (t) -> pathU A B`
  (fib.go:201-221, *unchanged*), so **the fib group hash and ch10 do not move at
  all**. The *body* of `ua` assembles an `Equiv` from `(f,g,s,t)` internally
  (R-SIGMA's `pairF`/`isEquiv`-from-bi-invertible) and feeds it to `Glue`. The
  iso→equiv construction (`isoToEquiv`, standard: a bi-invertible map is an
  equivalence) lives in the *body*, invisible to the hash.

**Recommend 4b for retirement, 4a as a later cleanup.** 4b achieves the node's
explicit requirement — *"retire the postulate without a hash-format bump for
unaffected groups"* — with **zero** hash movement anywhere (no fib rehash, no
cascade, ch09–ch23 byte-identical), because only bodies change. The four-
component telescope is uglier than `Equiv`, but it is already what ch10 writes
(`ua boolF boolF not not notNot notNot`, `listings/ch10_univalence.rune:39`), so
4b *also keeps ch10's source unchanged*. 4a (the `Equiv` retype) becomes a
separate, scheduled "surface cleanup" commit once the compute is proven, paid
once, deliberately — not entangled with making univalence compute.

### Decision 5 — `idEquiv` and `isoToEquiv` are *inner library* terms, not new builtins.

The Glue at `i1` needs `idEquiv B : Equiv (El B) (El B)`, and 4b needs
`isoToEquiv : (f,g,s,t) -> Equiv`. Both are **ordinary fibrant-layer definitions**
built from `sigmaF`/`pairF` (R-SIGMA) and `pathF`/`preflF` — *not* builtin group
members. They ship in a small inner prelude (`listings/` or a `prelude.rune`),
keeping the kernel's builtin surface fixed (Thompson). `idEquiv`'s `isEquiv`
component is `λy. (y, refl)`-with-contractibility, standard. This is where the
"teachable" library content lives.

## Interfaces & signatures to add (Go + Rune surface as relevant)

### What R-GLUE MUST deliver (the contract R-UA consumes — co-design these)

R-GLUE ships a builtin group (own hash space, like sigma/kan) with at least:

```
Glue   : (B : UF) -> (phi : F) -> (T : holds phi -> UF)
            -> (e : holds phi -> Equiv (El (T ·)) (El B)) -> UF
glue   : ... -> El B (partial) -> El (Glue B phi T e)
unglue : (B phi T e) -> El (Glue B phi T e) -> El B
```

and the **two Kan rules R-UA's compute depends on** (R-GLUE's research, R-FILL +
C-OVERLAP-gated):

- **Boundary collapse:** `Glue B ⊤ T e ~> T htop` (a totally-defined Glue is its
  glued type) — so the ua line's endpoints are right: at `i1`, `(i=0)∨(i=1)` is
  `⊤`, the system picks `(B, idEquiv)`, and `Glue B ⊤ (λ_.B) (λ_.id) ~> B`. ✔
- **transp-over-Glue:** `transp (λi. Glue (B i) φ (T i) (e i)) ψ g0 ~> glue [...]`
  with base `comp` in `B` and `φ`-part through the equivalences (CCHM §6). For the
  **ua specialisation** (constant `B`, `φ=(i=0)∨(i=1)`, `e`/`id` at the faces),
  R-UA needs *only the collapsed form*: `transp (λi. ua-line) x ~> equivFun e x`.

**R-UA's job is to verify these two R-GLUE rules, instantiated at the ua line,
reproduce the current bolted `castU` behaviour — and to make J ride them.**

### core/eval.go — DELETIONS (the retirement), no additions to the constructor set

- Delete the `FRoleCastU` bolted rules (`core/eval.go:810-826`): the `ureflU`
  fast-path and the `ua`-spine match. `castU` is no longer a computing *head*; it
  unfolds to its `transpU`-over-`pappU` body and computes through `transp`/Glue.
  Remove `FRoleCastU` from the `tryRules` dispatch trigger (`core/eval.go:674`).
- Delete the `FRoleJ`-on-`preflF` rule (`core/eval.go:801-809`) *only after A4
  lands* — until then keep it as the refl fast-path. Once `pathJ` has a body
  (comp-based, A4), remove `FRoleJ` from `rigidHead`/dispatch and let it unfold.
- Remove `ua`, `castU`, `ureflU`, (eventually `pathJ`) from `rigidHead`
  (`core/eval.go:444` covers them via `FibRoleOf != FRoleNone`) — they must now
  take the **lazy glued path** (`refVal`, `core/eval.go:405-425`) so forcing
  unfolds their bodies. **This is the mechanical core of "retire the postulate":
  flip them from rigid-bodiless to glued-with-body.**

  *Caveat (containment):* `rigidHead` currently returns `true` for *any*
  `FRoleNone`-distinct fib member. Split it: the *type formers and canonical
  intros* (`UF`,`El`,`fib`,`piF`,`pathF`,`preflF`,`pathU`,`ua`-as-*intro*) stay
  rigid; only the *defined* members (`castU`, `pathJ`, and `ureflU` if re-bodied)
  leave. Cleanest: a `fibDefined(role) bool` helper listing exactly the
  re-bodied roles.

### store/fib.go — bodies for the re-bodied members (the only store change)

`AddFib` currently stores `Def{Type: subst(ty)}` with **no body**
(`store/fib.go:81`). For the re-bodied members, store `Def{Type: subst(ty), Body:
subst(bodyTerm)}`. The body terms reference `Glue`/`glue`/`transp`/`pappU` by
their group hashes (passed in, like `AddPath(fhs, ihs)` takes deps). So:

```go
// AddFib gains the downstream group hashes it bodies against (a registration
// re-order: Glue/Kan must register BEFORE fib's bodies are filled, OR fib ships
// bodiless and a later `FinishFib(glue, kan)` patches the bodies in — see below).
```

**Registration-order wrinkle (real, must be resolved).** Fib registers *first*
(`session.go:73`), Glue/Kan *after* — but now fib's *bodies* reference Glue/Kan
hashes. Two fixes:

- **(A) Two-phase fib registration.** `AddFib()` registers types + hashes as
  today (so path/face/… can reference them); a later `s.st.FinishFib(glueHs,
  kanHs, sigmaHs)` installs the bodies for `castU`/`ua`/`ureflU`/`pathJ` once the
  downstream groups exist. The hashes never move (bodies are not hashed), so this
  is sound. **Recommend (A).**
- (B) Topologically reorder so Glue/Kan register before fib — impossible, since
  Glue's *type* references `UF`/`Equiv` (fib/sigma). It's a cycle at the type
  level only if fib bodies reference Glue; (A) breaks the cycle by deferring
  bodies.

### Rune surface — universe-level path eliminators (Decision 1a)

```
pappU  : (A B : UF) -> pathU A B -> I -> UF      -- decode a UF-path to a line
ureflU : (A : UF) -> pathU A A                    -- re-bodied: λi. A
ua     : (A B : UF) -> (f) -> (g) -> (s) -> (t) -> pathU A B   -- re-bodied (4b): λi. Glue B …
castU  : (A B : UF) -> pathU A B -> El A -> El B  -- re-bodied: transp (pappU …)
```

`pappU` is a new computing head (boundary: `pappU A B p i0 ~> A`, `… i1 ~> B`;
β on `ua`/`ureflU` lines). It is the universe-level twin of `papp`
(`store/path.go:119-125`) and the *one genuinely new builtin* R-UA adds — on its
own hash space, no hash-format bump. (Alternatively fold it into `pathU`-as-
`pathF`-of-a-`UF`-code, if R-UFH lands a code for `UF`; parked to R-UFH.)

### internal/session/session.go — lift the taint for what now computes

Once `castU`/`ua`/`ureflU` have honest erased meaning (transport over Glue lowers
to the equivalence's forward map — R-ERASE2 / B5), **remove them from the
`innerTaint` set** (`session.go:622`). This is the seam B5 (DAG line 209: "erased
meaning of inner paths/Kan; lift the inner-taint deploy ban") reads. *Until B5
lands, they stay tainted* — computing in the kernel, not yet deploying. R-UA's
deliverable is *kernel computation*; deploy is B5's.

## Worked micro-example (the teachable artifact)

The ch10 successor — same `not`-isomorphism, but now `castU`/`pathJ` compute
*through Glue*, certified by `refl`. The headline: **the same `refl`s that ch10
pins (`transportIsNot`, `roundTrip`) still pass — but now they pass because
transport over a Glue line computes, not because a rule is pinned to `ua`.**

```
-- ch25_ua_glue.rune  (§F phase 4 — ua from Glue; castU/pathJ COMPUTE)

data Bool : U is true : Bool | false : Bool end
not : Bool -> Bool is
  fn (b : Bool) is BoolElim (fn (_ : Bool) is Bool end) false true b end end
notNot : (b : Bool) -> Eq Bool (not (not b)) b is
  fn (b : Bool) is
    BoolElim (fn (x : Bool) is Eq Bool (not (not x)) x end) refl refl b end end
boolF : UF is fib Bool end

-- ua is now DEFINED (a Glue line), not postulated. The source is unchanged
-- from ch10 (Decision 4b keeps ua's four-component type), but the term now
-- unfolds to  λi. Glue boolF ((i=0) ∨ (i=1)) [boolF, boolF] [notEquiv, id].
notPath : pathU boolF boolF is ua boolF boolF not not notNot notNot end

-- castU over notPath computes BY TRANSPORT OVER THE GLUE LINE:
--   castU … notPath x  =  transp (λi. pappU notPath i) x
--                      =  transp (λi. Glue boolF …) x        (pappU β)
--                      ~> equivFun notEquiv x  =  not x       (transp-over-Glue)
-- refl certifies the reduction landed on `not`, same as ch10 — but derived now.
transportIsNot : (b : Bool) -> Eq Bool (castU boolF boolF notPath b) (not b) is
  fn (b : Bool) is refl end end

-- The NEW capability ch10 could not state: pathJ OVER a ua-path computes.
-- Motive: "transporting along notPath lands in Bool"; the J reduces by comp
-- along the Glue line (A4 + R-GLUE), not by matching preflF.
-- Here, a trivial-but-honest use: J's d-branch is reached because comp over the
-- ua line transports the motive — and refl certifies J fired (did not stick).
uaJComputes :
    (P : (B : UF) -> pathU boolF B -> U) ->
    (d : P boolF (ureflU boolF)) ->
    Eq U (pathJU boolF P d boolF notPath) (castU-motive …) is   -- schematic
  fn (P) (d) is refl end end
-- (pathJU = the UF-level path eliminator; the concrete motive is spelled in the
--  listing. The teachable point is the `refl`: J over a ua-path is no longer
--  stuck.)

-- ureflU's transport is the identity — now an INSTANCE of transp regularity,
-- not a pinned castU rule:
same : (b : Bool) -> Eq Bool (castU boolF boolF (ureflU boolF) b) b is
  fn (b : Bool) is refl end end
```

The lesson (Savage): **"`ua` is not magic — it is a Glue. Transport along it is
transport over that Glue, and that is why it computes to the equivalence. And
because it is an honest path, path induction over it works like path induction
over any path: by transporting the motive along the line."** The pedagogy is the
*deletion* — ch10 said "computes by a special rule, J does not"; ch25 says "there
is no special rule; both are transport over Glue." The two pinned `refl`s
(`transportIsNot`, `same`) are *the same listing lines* as ch10 — only their
*justification* changed from postulate to derivation, which is the whole thesis.

## Risks / open sub-questions

1. **R-GLUE is undelivered and is the hard dependency.** R-UA cannot land before
   R-GLUE ships the Glue former *and* the transp-over-Glue rule (which itself
   needs R-FILL for the base `comp` fill and C-OVERLAP for the multi-branch
   `(i=0)∨(i=1)` system overlap). **R-UA is "needs-more-research" precisely
   because of this chain.** What R-UA *can* deliver now, ready-to-build, is the
   *retirement mechanism* (Decision 4: re-body without hash movement) and the
   *deletion plan* (Decision 2/3: the bolted rules become instances) — both
   testable against a *stubbed* Glue that only ships the collapsed ua-specialised
   rule.

2. **The `(i=0)∨(i=1)` system needs C-OVERLAP.** The ua Glue's system has *two*
   faces, and at no interior point do they overlap (`(i=0)∧(i=1) = ⊥`), so the
   overlap-agreement obligation is *vacuous* — **the ua line specifically may dodge
   the C-OVERLAP gap** (PARKING-LOT.md:156-166) because the faces are disjoint.
   This is a genuinely favourable special case worth verifying: if the disjoint-
   face Glue transport rule can be stated *without* needing `part h1 ≡ part h2`,
   then `castU`-over-`ua` computes *even before* C-OVERLAP is resolved in general.
   **Open sub-question, flagged as a possible early win.** (general Glue Kan still
   needs C-OVERLAP; only the ua specialisation may not.)

3. **Regularity collapse of the base.** `castU … (ua …) x ~> equivFun e x`
   requires the base `comp`/`transp` over the *constant* type `B` to be the
   identity. Rune *ships regularity* (`tryTransp`, `core/eval.go:1126-1129`); CCHM
   classically does *not* (it needs `transportRefl`, a path not a reduction —
   that's why `uaβ` in Agda is `transportRefl ∘ equivFun`, not `refl`). **Rune's
   regularity makes `uaβ` a definitional `refl` where Agda's is a path** — *better*
   than CCHM here, and it is what makes ch10's `transportIsNot` a bare `refl`
   today. Risk: if C-REG (the clarify node) forces *dropping* regularity (Swan's
   fix for canonicity), `uaβ` reverts to a path and ch10/ch25's `refl`s break.
   **R-UA's `refl`-pinning is coupled to the C-REG decision** — must be co-checked.

4. **`pappU`/universe-level path eliminator is new surface.** Decision 1a adds
   `pappU` (and morally `transpU` = `transp`). This is a new builtin head (own
   hash space). It is the minimal honest cost of making `pathU` lines *visible* to
   J; R-UFH (A10) would want it anyway. Open: whether to instead unify `pathU`
   with `pathF` once a code for `UF` exists (R-UFH) — parked to R-UFH so R-UA does
   not block on the universe hierarchy.

5. **J-via-comp (A4) is a hard co-dependency for the J half.** R-UA's *castU* half
   needs only R-GLUE; its *pathJ* half needs A4 *and* R-GLUE. If A4 slips, ship
   R-UA in two increments: **R-UA-cast** (castU computes through Glue; the J rule
   stays the preflF-only fast-path) and **R-UA-J** (delete the fast-path once A4
   lands). The roadmap already orders A4 before A7 (DAG line 174 vs 177), so this
   is consistent.

6. **Frame Lemma (R-FRAME) across the re-bodying.** `castU`/`ua` flipping from
   rigid-bodiless to glued-with-body means **forcing them now logs dependencies**
   (the Glue/Kan group hashes) into the proof cache — where before they logged
   nothing (rigid heads skip the unfold thunk, `core/eval.go:406-424`). The
   certificate key for any definition using `castU` *grows* to include the Glue
   stack. This is *correct* (the definition now genuinely depends on those
   reductions) but it is a **cache-invalidating change**: ch10's existing
   certificates go stale and re-check. Price it like the re-body it is; assert the
   new dependency sets in a Frame regression (R-SIGMA §Test plan does the
   analogue). R-FRAME's open question (INDEX:178-181 — "whether any Kan-over-Glue
   or transport-through-ua rule branches on irrelevant proof content") **must be
   resolved here**: the transp-over-Glue rule must not branch on the *proof*
   `holds φ`, only on the *face* `φ` — confirm the R-GLUE rule reads `φ`, not its
   inhabitant, or ship behind `markImprecise`.

## Test/gate plan

- **Listing gate:** `listings/ch25_ua_glue.rune` elaborates, checks, and its
  `refl`-pins normalise (`transportIsNot`, `same`, the J-computes pin). Gate from
  `harness/listings_test.go` like ch17–ch23. ch10 stays in the corpus *unchanged*
  (4b keeps its source) and must still pass — the **regression that the retirement
  is source-transparent**.
- **Hash-stability regression (the headline gate for "no hash-format bump"):**
  assert `AddFib` member hashes are byte-identical before/after re-bodying (bodies
  are not hashed; Decision 4b changes no member type); assert interval/face/sys/
  path/sigma/kan group hashes and ch09–ch23 emit byte-identical; assert
  `defFormatVersion` unchanged. A failing hash here means a member *type* leaked
  into the change (the 4a/4b fork was crossed accidentally).
- **castU-through-Glue unit tests** (`store/fib_test.go` / a new `ua_test.go`,
  against a stubbed Glue): `castU A B (ua A B f g s t) x` normalises to `f x` *via
  the transp-over-Glue path* (assert the dependency log contains the Glue + Kan
  hashes — proving it went through Glue, not a bolted rule); `castU A B (ureflU A)
  x ~> x` via *transp regularity* (assert it does **not** depend on a deleted
  castU rule).
- **pathJ-over-ua pin** (`internal/session/structural_test.go`): once A4 lands,
  `pathJU … notPath` reduces (negative pin beforehand: it is stuck while the
  preflF-only rule stands — guards the R-UA-cast / R-UA-J increment boundary).
- **Disjoint-face dodge (Risk 2):** a unit test that the ua Glue line transports
  *without* invoking any `part h1 ≡ part h2` overlap step — if it passes, document
  that `castU`-over-`ua` is C-OVERLAP-independent.
- **Regularity coupling (Risk 3):** a pin that `transportIsNot` is `refl` *iff*
  regularity is on; flag it in the C-REG decision record so dropping regularity
  knowingly breaks it.
- **Cubical property tests** (`harness/cubical_props_test.go`): add `ua` lines to
  the "closed fibrant former normalises predictably" sweep; confluence of the
  (now derived) castU reduction with the Glue boundary collapse; type-preservation
  on `pappU`.
- **Frame Lemma (X1/R-FRAME):** a cache-key regression that a definition using
  `castU` records the Glue/Kan group hashes (Risk 6); and that the
  transp-over-Glue rule's dependency log does **not** include a `holds φ` proof
  hash (it must read the face, not the proof).

## Unblocks (which implement nodes, and what they still need)

- **A7 (ua-as-Glue; J/castU over ua compute)** — *this is A7.* R-UA delivers: the
  retirement mechanism (re-body without hash movement, **ready-to-build now**),
  the deletion plan (bolted castU/J rules → derived instances, **ready-to-build
  against a stubbed Glue**), and the `pappU`/`transpU`-as-`transp` decoding. The
  *compute* itself is **blocked on R-GLUE** (Glue former + transp-over-Glue rule)
  and, for the J half, **A4** (pathJ-via-comp). R-GLUE in turn needs R-FILL +
  (general) C-OVERLAP — though the **ua specialisation may dodge C-OVERLAP**
  (Risk 2, disjoint faces) and may even dodge the deep R-FILL fill (constant base
  ⇒ regularity collapse, Risk 3).
- **B5 (erased meaning of inner paths/Kan; lift inner-taint)** — R-UA fixes the
  *kernel* meaning of transport-along-ua (it is the equivalence's forward map);
  B5/R-ERASE2 turns that into an *erased* runtime value and removes `ua`/`castU`
  from `innerTaint` (`session.go:622`), lifting the ch10/ch25 deploy ban. R-UA's
  compute is B5's prerequisite; B5 still needs the IR (R-IR/B1, delivered) plus
  the erased transport semantics (R-ERASE2, undelivered).
- **M2 (Univalence COMPUTES)** — A7 is the *thesis heart* of M2 (roadmap line
  240). R-UA is the last A-node before M2 closes; with A5/A6/A7 it makes the
  univalence regression suite (the `refl`-certified `ua`/`Glue` computations) the
  M2 gate.

**Status of this node: needs-more-research.** The *retirement engineering* (no
hash-format bump for unaffected groups; bolted rules become derived instances; the
two-phase fib body installation) is concrete and **ready-to-build**. The
*compute* is **blocked on R-GLUE** (undelivered) and **A4** (for J), with R-GLUE
itself gated on R-FILL + C-OVERLAP — but R-UA identifies two favourable
specialisations (disjoint faces dodge C-OVERLAP; constant base + regularity dodge
the deep fill) that may let `castU`-over-`ua` compute *ahead* of fully-general
Glue. That early-win path is the recommended first increment.

Sources:
[1Lab — Univalence](https://1lab.dev/1Lab.Univalence.html),
[Cubical.Foundations.Univalence (agda/cubical)](https://agda.github.io/cubical/Cubical.Foundations.Univalence.html),
[agda/cubical Core/Glue.agda](https://github.com/agda/cubical/blob/master/Cubical/Core/Glue.agda),
[Cohen–Coquand–Huber–Mörtberg, Cubical Type Theory (PDF)](https://www.cse.chalmers.se/~coquand/cubicaltt.pdf),
[cubicaltt univalence.ctt](https://github.com/mortberg/cubicaltt/blob/master/examples/univalence.ctt).
