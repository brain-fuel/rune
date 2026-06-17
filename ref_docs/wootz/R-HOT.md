# R-HOT — Verified hot code reload / live upgrade

> Proposed roadmap node **D7** (`D7 [I] verified hot code reload / live upgrade ⇐
> R-HOT, R-OTP(D5), R-ERASE2(B5), R-SUM(C1), content-addressing, A7(R-UA)`). Track
> D, the stdlib/runtime track — the node that turns runtime *evolution* into a
> verified operation. It sits on **R-OTP** (D5 — the `gen_server` record-of-callbacks
> over a state type `S`, and the `Step` LTS its guarantees are stated against,
> `ref_docs/wootz/R-OTP.md:286-293`), **content-addressing** (`store/store.go` — the
> append-only immutable `defs` map plus the single mutable `names` pointer),
> **R-ERASE2/A7** (the landed fact that `castU (uaGlue …) x ~> f x` reduces *and
> deploys* as the forward map — `core/eval.go:2657-2682`, `ref_docs/wootz/R-ERASE2.md:206-225`),
> and **R-SUM** (C1 — state is a Σ/record, so migrations are field-wise). It is the
> node that turns the substrate's three latent facts — *an immutable N-version
> store*, *univalence-transport-as-deployable-state-migration*, *the gen_server as a
> record* — into a single verified live-upgrade story. The user's two intuited axes
> ("link something new" / "recursively do something else for backward-incompatible
> changes") are, respectively, the store's existing relink mechanism and a
> structural migration synthesis over the state's telescope; the third tier — a
> *provably-correct automatic* migration via univalence transport — is the payoff no
> other language has.

## Problem (what is absent today, with file:line)

The substrate already holds every ingredient of a hot upgrade *except the upgrade
itself*. Three facts make the gap precise:

1. **The store is an N-version immutable history with no runtime-versioning verb.**
   `store/store.go:32-36`: the definition map is "append-only and immutable in
   spirit: a content hash names a fixed (Type, body) forever. The name map is the
   only mutable, non-trusted part." Every version of a definition coexists in
   `s.defs` (writes at `store.go:144,170,192`); only `s.names[name]` is re-pointed
   (`store.go:145,171,193`). The eraser keeps shadowed versions distinct — "a body
   compiled against the old binding keeps calling the old code"
   (`internal/session/session.go:829`; the `$`-hash suffixing at `session.go:834`).
   The proof cache is likewise append-only with **no invalidation logic**
   (`store/cert.go:16`), valid forever if a cert's deps still resolve
   (`cert.go:57,70`). So the store *is* a code-version archive — but nothing reads
   it as one. There is no `code_change`, no state migration, no rollback, no
   relink-of-a-running-process primitive. The version history exists; the
   live-upgrade *operation over it* does not.

2. **Transport across a proven equivalence already computes AND deploys — but is
   never connected to state.** `castU A B (uaGlue A B f g s t) x ~> f x` reduces
   definitionally: `core/eval.go:2657` (the `FRoleCastU` case), with the ι-rule
   `castU A B (ua _ _ f _ _ _) x ~> f x` documented at `eval.go:2564-2565` and the
   `ureflU`-identity at `eval.go:2662`. The derived `uaGlue` is a postulate-free
   univalence path built from an iso `(f, g, s, t)` (`listings/ch58_pathalgebra.rune`),
   refl-proven to compute for the *non-identity* iso `not`
   (`transportIsNotDerived`, `ch58:136`) and to be the identity for the identity
   iso (`uaBetaId`, `ch58:108`). It **deploys** as the forward map applied — Class-2
   of `ref_docs/wootz/R-ERASE2.md:206-225`, lifted by the normalize-then-erase
   override (`NormalizeExpr`, `internal/session/session.go:585-616`; the B5 slice).
   **This is, structurally, exactly a state-migration function that is provably
   correct and runs** — yet nothing in R-OTP calls it. The gen_server's state type
   `S` and the univalence machine have never been introduced to each other.

3. **The gen_server has no `code_change`.** R-OTP's `GenServer S Call Cast` is a
   record `{init, handleCall, handleCast, terminate}` over a state type `S`
   (`R-OTP.md:286-293`). There is no fifth callback for upgrade, no version token,
   no `primUpgrade`. R-OTP's six R0 primitives are spawn/self/send/receive/monitor/
   exit (`R-OTP.md:188`) — upgrade is scoped out. A repository-wide search confirms
   `code_change`, `-vsn`, `reload`, `primUpgrade`, and "live upgrade" appear
   **nowhere** in `ref_docs/wootz/`. The gap is total — but R-OTP already
   established the *pattern* for closing it: a runtime op is a bodiless `IForeign`
   builtin + a session wire, with **no new IR node and no hash bump**
   (`R-OTP.md:455-472`); six backends already dispatch `IForeign` uniformly
   (`codegen/{beam.go:96, py.go:95, jvm.go:111, rust.go:125, js.go:155,
   golang.go:97}`).

So a live upgrade is the missing verb over an already-versioned, already-transport-
capable, already-record-structured substrate. **The user's two axes are the
substrate's two existing mechanisms:** "linking something new" *is* re-pointing
`names` to a new hash and erasing the dependents against it (fact 1); "recursively
doing something else for backward-incompatible changes" *is* synthesizing a
migration `S_old -> S_new` structurally over the Σ-structure of the state (fact 3,
R-SUM), bottoming out in transport (fact 2) where the field types are equivalent
and in a developer obligation where they are not.

## Prior art (what other systems do; cite)

**Erlang/OTP `code_change` — the spec being matched.** OTP release-handling does a
*suspend → load new module → `code_change(OldVsn, State, Extra) -> {ok, NewState}`
→ resume* on each affected `gen_server`. The module carries a `-vsn` attribute;
`OldVsn` is its previous value. Two structural facts R-HOT inherits: (a) the
upgrade is a **per-process state transformation keyed by an old-version token**, and
(b) the BEAM holds **at most two versions of a module loaded** (current + old); a
third load purges the oldest, killing any process still running it. This 2-version
limit is the central tension R-HOT reconciles against the store's *N-version*
history (§C).

**Unison — content-addressed code, the closest store precedent.** Unison stores
every definition by content hash, immutable forever; a name is a label on a hash,
and updating a name leaves old code callable. Unison's `update` propagates a change
to transitive dependents by *re-typechecking* them against the new hash, surfacing
exactly the breakages where a type changed incompatibly. **This is Rune's dev-loop
(§F.i) almost exactly** — `names[name] := newHash` + re-elaborate dependents — and
it is the proof that content-addressing *is* the natural substrate for "link
something new." Unison has no running-process migration story; R-HOT adds that half.

**Bidirectional transformations / schema evolution.** The database-migration
literature (Flyway/Liquibase forward+backward migrations) and the PL work on
type-safe schema evolution (Cambria's lenses, the BX `get`/`put` tradition) converge
on: a migration is a *function between old and new state types*, and a *reversible*
one is an *isomorphism/equivalence*. R-HOT's tiering is exactly this reading made
provable — a reversible migration is an `Equiv S_old S_new` (tier 2), an
irreversible one a total function with a developer witness (tier 3). The HoTT
slogan — *equivalent types are interchangeable, and transport carries every
structure across* — is the formal core (`ch58`, A7).

**Verified upgrades in proof assistants.** Mechanized live-upgrade verification is
thin; the transferable idea is from refinement-based distributed-systems
verification (Verdi, IronFleet): make the upgrade *a step of the transition system*
and prove it *preserves the system invariant*. R-OTP already adopted this move
(fault rules as `Step` constructors, `R-CALC.md:246-252`); R-HOT adds one more
constructor (`UPGRADE`) and one more preservation obligation (§C).

Cross-cutting lessons: (1) content-addressing gives "link something new" for free
(Unison); (2) a reversible migration is an equivalence and transport deploys it
(HoTT/BX); (3) the upgrade is a transition-system step with an invariant-
preservation proof (Verdi); (4) BEAM's 2-version limit is a runtime resource
constraint orthogonal to the store's logical N-version history, managed explicitly.

## Chosen approach for THIS substrate (concrete; respects containment)

**Recommendation: R-HOT is a tiered-verified *library + one IForeign op + one
`Step` constructor*, classifying every behaviour change by the TYPE relationship
between old and new state, and synthesizing the migration structurally. The
migration is, at the leaves, either the identity (tier 1), a univalence transport
`castU (uaGlue …)` the kernel already reduces and deploys (tier 2), or a developer-
supplied total function gating the upgrade (tier 3). The upgrade is a new `Step`
constructor preserving the gen_server invariant; preservation is automatic for tier
2 (via `subst`) and a discharge obligation for tier 3. Zero outer-core growth, no
hash bump: the op is an `IForeign` in `store/otp.go`, the migration synthesis is a
library + an elaborator pass, the proof is a theorem over R-OTP's `Step`.**

This is the Unison dev-loop and the OTP `code_change` runtime made one mechanism,
with univalence supplying the *provably-correct automatic* middle tier no other
system has.

### §A — the tiered change classification

The behaviour being upgraded is a `gen_server` value `B : GenServer S Call Cast`
(`R-OTP.md:286-293`). An upgrade replaces `B_old` (state type `S_old`, content hash
`h_old`) with `B_new` (`S_new`, `h_new`). The tier is determined by the **type
relationship between `S_old` and `S_new`** and the **callback interface**
`(Call, Cast)`, decided in this order:

- **Tier 0 — Identical (no-op).** `h_new == h_old`. Content-addressing detects this
  for free (`store.Add` returns the same hash for the same `(Type, body)`,
  `store.go:189-193`). This also catches **pure renames**: renaming `counter` to
  `tally` with an unchanged body re-binds `names["tally"]` to the *same* hash; the
  lookup name differs but the hash is identical, so no process is touched. **Emit
  nothing; the running process already runs `h_new`.**

- **Tier 1 — Interface-preserving relink (`S_old ≡ S_new`; "link something new").**
  `h_new ≠ h_old`, but `S_old ≡ S_new`, `Call_old ≡ Call_new`, `Cast_old ≡
  Cast_new` are **definitionally convertible** by the kernel's existing conversion
  (the same convertibility the certificate layer uses). Only the *behaviour bodies*
  changed (a bug fix, a faster handler). **The migration is `id : S_old -> S_new`
  (it type-checks because `S_old ≡ S_new`); re-point `names[name] := h_new` and run
  the process forward on its existing state with the new code.** This is the user's
  "linking something new" — exactly what `emitNames` already does for shadowed
  definitions (`session.go:829-836`); R-HOT adds only the *runtime re-point of a
  live process* (§D).

- **Tier 2 — Equivalent representation (`S_old ≃ S_new` via a provable `Equiv`;
  transport). The centerpiece.** `S_old` and `S_new` are not definitionally equal,
  but there is a supplied (or synthesized, §B) `e : Equiv (fib S_old) (fib S_new)` —
  equivalently an iso `(f, g, s, t)` (`equivFromIso`, ch58). **The migration IS the
  univalence transport:**
  `migrate = λ(x : S_old). castU (fib S_old) (fib S_new) (uaGlue … f g s t) x`,
  which the kernel reduces to `f x` (`eval.go:2657-2682`) and which *deploys* as the
  forward map applied (Class-2, `R-ERASE2.md:206-225`; the normalize-then-erase
  slice, `session.go:585-616`). **Automatic, provably correct, and it runs.** A
  representation change (a counter stored as `Nat` ⟷ as a `Vec` of unary tallies)
  is a tier-2 upgrade whose migration the developer never writes: they supply the
  iso (or it is derived, §B), and transport *is* the `code_change`.

- **Tier 3 — Breaking (no equivalence; information added or removed).** `S_old` and
  `S_new` are genuinely non-equivalent — a field added (new information with no
  canonical old value), removed (information discarded), or changed
  non-equivalently. No `Equiv` exists, so no automatic migration exists. **The
  upgrade is GATED on a developer-supplied total `migrate : S_old -> S_new` that
  type-checks.** Totality is by construction (an ordinary Rune function, eliminator-
  checked; the core forbids partiality outside `partial`). §B makes most of
  `migrate` auto-derivable, leaving only the genuinely-novel parts as residual
  obligations.

**The decision procedure (the exact algorithm).** Given `B_old @ h_old`,
`B_new @ h_new`, and optionally a supplied `mig` (an `Equiv`, an iso, or a
function):

```
1. if h_new == h_old:                                  → TIER 0 (no-op; catches renames)
2. else if convertible(S_old, S_new)
        ∧ convertible(Call_old, Call_new)
        ∧ convertible(Cast_old, Cast_new):             → TIER 1 (identity migration, relink)
3. else if a term e : Equiv (fib S_old) (fib S_new)
        is supplied or synthesizable (§B):
        migrate := λx. castU (fib S_old)(fib S_new)(uaGlue … (isoOf e)) x
                                                        → TIER 2 (transport; automatic, proven)
4. else if a term migrate : S_old -> S_new is supplied
        and type-checks:                               → TIER 3 (gated total migration)
5. else:                                               → REJECT: "breaking change needs a
                                                          migration witness S_old -> S_new
                                                          (or an Equiv for an automatic one)"
```

Steps 1–2 are decided by the kernel alone (hash equality + the existing conversion
check). Step 3 elaborates the supplied equivalence against the landed A7/`uaGlue`
machinery. Step 4 is ordinary type-checking. The procedure is **total and
decidable** — each test is decidable; the only search is §B's structural synthesis,
itself terminating (it recurses on the *finite* Σ-telescope of the state type).

### §B — the "recursive for breaking changes" mechanism (migration synthesis)

The user's intuition — "recursively doing something else for backward-incompatible
changes" — is **migration synthesis by structural recursion over the state type's
Σ-telescope.** Because R-SUM makes state a (possibly nested) record = iterated `Sig`
(`R-OTP.md:243`), `S_old` and `S_new` are telescopes of fields. The synthesizer
`deriveMigrate : (S_old S_new : U) -> (S_old -> S_new, [Hole])` walks both
telescopes field by field, deciding the tier locally per field and recursing into
nested records:

```
deriveMigrate(S_old, S_new):
  match (S_old, S_new):
    record Fs_old, record Fs_new →                       -- match fields by name
      for each (name : T_new) in Fs_new:
        case lookup(name, Fs_old):
          found (name : T_old), convertible(T_old,T_new):   -- TIER 1 at the field
              fieldMig[name] := λs. s.name                    -- identity projection
          found (name : T_old), Equiv(fib T_old, fib T_new): -- TIER 2 at the field
              fieldMig[name] := λs. castU … (uaGlue … iso) (s.name)   -- transport
          found (name : T_old), both records:                -- RECURSE
              fieldMig[name] := deriveMigrate(T_old,T_new) ∘ (λs. s.name)
          not found (genuinely NEW field):                   -- RESIDUAL OBLIGATION
              emit hole  ?init[name] : (S_old -> T_new)
      -- fields in Fs_old absent from Fs_new are DROPPED (info removed; flagged).
      assemble: λs. record { name = fieldMig[name] s | name ∈ Fs_new }
    _ → tierDecision(S_old, S_new)                           -- base case (§A)
```

**Auto-derivable vs hand-supplied:**

| field relationship (old → new)                  | tier | synthesized migration                | developer supplies |
|-------------------------------------------------|------|--------------------------------------|--------------------|
| unchanged (definitionally equal type)           | 1    | identity projection `λs. s.name`     | nothing            |
| representation-equivalent (`Equiv` exists/derived) | 2 | `castU (uaGlue …)` — transport       | the iso (or derived) |
| nested record, recursively migratable           | 1/2  | `deriveMigrate` recursion ∘ projection | nothing (or inner residuals) |
| **genuinely new field** (no old source)         | 3    | **hole** `?init[name] : S_old -> T_new` | **the initializer** |
| dropped field (in old, not new)                 | —    | omitted from assembled record        | nothing (flagged)  |
| type changed *non-equivalently*                 | 3    | **hole** `?conv[name] : T_old -> T_new` | **the conversion** |

So tier 3 is **not** "write the whole migration by hand" — it is "write *only* the
genuinely-novel parts." The synthesizer discharges every unchanged field (tier 1),
every equivalent-representation field (tier 2 transport), and every nested record,
leaving a *minimal* set of typed holes: one initializer per new field, one converter
per non-equivalently-changed field. The upgrade is gated until every hole is filled
with a total term. This is the recursion the user intuited, made precise: **identity
on unchanged fields, transport on equivalent fields, recurse into nested records,
residual obligation only at genuine novelty.**

**Auto-derivation of the tier-2 iso itself** is opportunistic: for a numeric carrier
change between two `data` types with a constructor-preserving bijection (e.g. the
`builtin nat`/`builtin bin` representation swap) `f`, `g` derive by structural
recursion and `s`, `t` by the existing induction-proof patterns (ch35-shaped). Where
the bijection is not canonical the developer supplies the iso; the synthesizer still
assembles the surrounding record migration around it. (General iso-derivation is
parked — see Risks.)

### §C — the upgrade as a PROVEN step

Model a hot upgrade as a **new constructor of R-OTP's `Step` LTS** (`R-CALC.md:238`,
the relation `CRASH`/`LOSS`/`DETECT` are constructors of — `R-CALC.md:246-252`). A
running gen_server process is a configuration `proc(h, s)` — a behaviour content-hash
`h` paired with state `s : S`. The new constructor:

```
-- a hot upgrade is a labelled transition: the process at (h_old, s : S_old)
-- becomes the process at (h_new, mig s : S_new).  upg h_old h_new is the label.
UPGRADE : (mig : S_old -> S_new)
       -> Step (proc h_old s) (upg h_old h_new) (proc h_new (mig s))
```

`UPGRADE` is one more `Step` constructor — **zero outer-core growth** (`Step` is a
Rune relation, `R-CALC.md:238`; the same containment R-OTP relies on).

**The correctness guarantee — invariant preservation.** A gen_server carries an
invariant `I : S -> U` (the protocol property its state always satisfies). The
upgrade is correct iff it carries the old invariant to the new one:

```
upgradePreservesInvariant :
  (mig : S_old -> S_new) -> (s : S_old) -> I_old s -> I_new (mig s)
```

- **Tier 1:** `S_old ≡ S_new`, `mig = id`, `I_new ≡ I_old`, so this is `λs i. i` —
  trivially proven.
- **Tier 2 (the headline):** `mig = castU (uaGlue … e)`. The invariant is carried
  **automatically by transport**: `I_new` is itself the transport of `I_old` along
  the same path, so `I_new (mig s)` holds by `subst` (Leibniz transport, the
  equality stratum's `subst`, Phase 4) over the `ua`→`Eq` bridge. **Transport
  carries the invariant for free** — the deep payoff of making the migration *be*
  univalence transport: the proof obligation discharges by the same path that
  defines the migration. (`ch58`'s `transportIsNotDerived`/`uaBetaId` are the
  refl-witnesses that the transport computes; the invariant-carry is `subst` over
  the same `uaGlue` path.)
- **Tier 3:** `mig` is arbitrary, so this is a **genuine discharge obligation** — the
  developer proves `I_old s -> I_new (mig s)` per genuinely-new/changed field. The
  proof-cost of a breaking change, paid exactly where the breakage is.

**In-flight messages.** `UPGRADE` is a single atomic transition on a quiescent
process — it fires *between* message-handling steps, never mid-`handleCall`. On
BEAM this is OTP's `sys:suspend → code_change → sys:resume`: the process finishes
its current callback, the mailbox buffers arrivals (FIFO, R-OTP's `primReceive`
ordering, `R-OTP.md:203`), the state migrates, buffered messages are then handled by
the new code against `migrate s`. **Messages are neither lost nor reordered across
an upgrade** — `UPGRADE` commutes with mailbox buffering, because the mailbox is
runtime state external to `S`. A tier-1/2 upgrade preserves the interface (all
buffered messages remain well-typed); a tier-3 upgrade that *changes* the message
interface must also supply a message migration (the same §B synthesis applied to
`Call`/`Cast`), or is rejected if buffered old-typed messages cannot be retyped.

**Rollback.** Because `defs` is append-only immutable (`store.go:32-36`), `h_old`
still resolves after the upgrade. Rollback is another `UPGRADE` step in reverse:
tier 1 is `id` (always available); tier 2 is `castU (uaGlue … e)` with the iso
reversed (`Equiv` is symmetric) — **always available and proven** (tier-2 upgrades
are reversible by construction); tier 3 is reversible **iff** the developer also
supplied a backward `S_new -> S_old` with its own preservation proof. Information-
discarding upgrades cannot be losslessly rolled back, and the type system makes that
visible (no backward witness ⇒ no rollback step).

**BEAM 2-version limit vs store N-version history.** The store holds *all* versions
forever (`defs`, N versions); BEAM holds at most two loaded module versions. R-HOT
reconciles them by treating **the store as the source of truth and BEAM as a 2-deep
cache**: the behaviour content-hash is the `-vsn` (§D); at most two are *loaded on
the VM* at once, but any archived version can be *re-loaded from the store* to roll
back to an arbitrary ancestor (a sequence of `UPGRADE` steps through intermediate
versions, or a direct migration if the composite iso exists). The N-version history
is logical and persistent; the 2-version limit is a runtime loading constraint the
upgrade scheduler respects by loading exactly the `(h_old, h_new)` pair per
transition and purging only after all processes have stepped. **The store's
immutability is what makes rollback-to-any-ancestor possible at all** — BEAM alone
could not, because it forgets.

### §D — projection to BEAM (and the non-BEAM shim)

**`migrate` projects to `code_change`.** The emitted Erlang module carries
`-vsn(H_short)` where `H_short` is the behaviour's content hash (the same
`h.Short()` already used for shadow-suffixing, `session.go:834`). `OldVsn =
h_old.Short()` is passed to `code_change(OldVsn, State, _Extra) -> {ok,
MIGRATE(State)}.` For tier 1, `MIGRATE = identity` (the `{ok, State}` no-op). For
tier 2, `MIGRATE` erases to **the forward map `f` applied** (`castU (uaGlue …) ~> f`,
`eval.go:2657-2682`; deploys per B5, `R-ERASE2.md:206-225`) — `code_change` literally
calls the equivalence's forward function. For tier 3, `MIGRATE` is the developer's
total function, erased ordinarily.

**`primUpgrade` triggers it.** Add **one** R0-style `IForeign` op to R-OTP's seam
(`store/otp.go`, the pattern of `R-OTP.md:455-472`; **no new IR node, no hash
bump**):

```
primUpgrade : {S_old S_new Call Cast : U}
           -> Pid (GenMsg Call Cast)            -- the running process
           -> GenServer S_new Call Cast          -- the new behaviour
           -> (S_old -> S_new)                   -- the migration (id / transport / supplied)
           -> IO Unit
```

| op            | BEAM (B3)                                                            | JS / Go shim (cooperative scheduler)                       |
|---------------|---------------------------------------------------------------------|------------------------------------------------------------|
| `primUpgrade` | `sys:suspend` → load new module (`-vsn`) → `sys:change_code(Pid,Mod,OldVsn,Extra)` → `sys:resume` | re-point the process record's behaviour ref + run `migrate` on its state, between runloop dispatches |

On BEAM `primUpgrade` *is* the OTP suspend/code_change/resume dance, near-free (the
Lambert gift, `R-OTP.md:70-79`). The **non-BEAM shim**: the cooperative scheduler
(`R-OTP.md:385-414`) holds each process as `(pid, mailbox, continuation,
behaviourRef)`; `primUpgrade` re-points `behaviourRef` to the new behaviour and
replaces `s` with `migrate s`, atomically between dispatches (the shim has no
preemption, so "between dispatches" is trivially atomic — the upgrade fires when the
process next yields at `primReceive`). The same `lib/otp.rune` + the migration term
run on BEAM (binding `code_change`) and on JS/Go (re-pointing the ref) — proven once,
run everywhere.

Six backends already dispatch `IForeign` uniformly (`codegen/beam.go:96`,
`py.go:95`, `jvm.go:111`, `rust.go:125`, `js.go:155`, `golang.go:97`), so adding the
`"upgrade"` op name costs no IR node and no per-backend structural change — only the
host accessor each backend already emits.

### §E — containment check (Thompson)

- **Zero outer-core growth.** No new `core.Tm`/`core.Val` constructor. `primUpgrade`
  is a bodiless `IForeign` builtin in `store/otp.go` (the `World`/`IO` pattern,
  `R-OTP.md:455-472`). `UPGRADE` is a constructor of the *library* relation `Step`
  (a Rune `data`, `R-CALC.md:238`). The classifier/synthesizer is an elaborator pass
  reading types only via `store.TypeOf` (`store.go:206`) — **never around the body
  barrier**. The tier decision is hash-equality + the existing conversion + existing
  type-checking.
- **No hash-format bump.** No new core constructor ⇒ `defFormatVersion`/
  `hashFormatVersion` untouched (stays 0x05). A test asserts an existing pure def's
  hash is byte-identical after `store/otp.go` registers `primUpgrade` (the
  fib/IO/quotient groups prove this is achievable).
- **Reuses existing univalence + content-addressing.** Tier 2 *is*
  `castU`/`uaGlue` (landed, A7) + `subst` (Phase 4) — R-HOT adds *no* cubical
  machinery. Tier 0/1 *are* `store.Add`/`names`/conversion (present). The B5
  normalize-then-erase deploy (`session.go:585-616`) is reused unchanged.
- **Flagged containment risks (honest):** (i) interface-changing tier-3 message
  migration may need a *runtime retyping of buffered messages* in the shim (BEAM
  handles it via `code_change`'s `Extra`) — still shim-stratum, not core; (ii) the
  inner-`pathU` → outer-`Eq` bridge `subst` needs for the tier-2 invariant carry
  must be confirmed against the A7 surface (it may require `pathJ` at the inner level
  instead).

### §F — dev-loop vs runtime (the two halves)

**(i) Hot reload in development (REPL / file re-load).** The substrate **nearly does
this already.** Re-loading a file or REPL-redefining `counter` re-points
`names["counter"]` to the new hash; the old hash persists; dependents elaborated
against the old binding keep calling old code via `emitNames` shadow-suffixing
(`session.go:829-836`). **What dev-reload needs that is missing:** (a) re-elaborate
the transitive dependents against the new hash (the Unison `update` propagation —
currently a re-load re-elaborates the *file*, not arbitrary cross-module
dependents); (b) surface the *tier* of each change (`:reload` reports "tier-1
relink" / "tier-2 transport, automatic" / "tier-3: needs migration for field `x`").
Dev-reload is **stateless** — no running process, no state to migrate; pure
re-binding + re-elaboration. **Ready-to-build now** (content-addressing + a
dependency walk; no `primUpgrade`, no `Step`, no BEAM).

**(ii) Live upgrade of a running deployed system (BEAM `code_change`).** The
**stateful** half: a process with live state `s` transitions to new code *and
migrates `s`*. Needs the whole apparatus — `primUpgrade` (§D), the `UPGRADE` `Step`
+ invariant-preservation (§C), the migration synthesis (§B), the BEAM projection
(§D), the 2-version/N-version reconciliation (§C). **Gated** on R-OTP/D5 (the
running gen_server) + B3/BEAM (the `sys` calls); A7 (transport) is landed.

The two halves share §A classification and §B synthesis; the only difference is
whether there is **live state `s` to carry.** Dev-reload is the upgrade with `s`
absent; live-upgrade is the upgrade with `s` present and `migrate s` mandatory.

## Interfaces & signatures to add (Go + Rune surface)

**Go — contained, mirroring R-EFFECT's `store/io.go` / R-OTP's `store/otp.go`; no
`core/term.go` or `core/hash.go` change:**

```go
// store/otp.go — ADD one op to the OTP builtin group (R-OTP.md:455 pattern).
//   primUpgrade : {S_old S_new Call Cast : U} -> Pid (GenMsg Call Cast)
//               -> GenServer S_new Call Cast -> (S_old -> S_new) -> IO Unit
// One more rigid IForeign head; NOT added to innerTaint (defined erased meaning).

// codegen — NO new IR node: primUpgrade is the existing IForeign; op name "upgrade".
//   Backend.Foreign maps it: BEAM → sys:change_code; shim → re-point the ref.

// elaborate/ — the tier classifier + migration synthesizer (a pass over stored types):
//   func ClassifyUpgrade(st *store.Store, hOld, hNew core.Hash, supplied *core.Tm) (Tier, error)
//   func DeriveMigrate(st *store.Store, sOld, sNew core.Tm) (core.Tm, []Hole, error)
//        // walks the Σ-telescope via store.TypeOf (store.go:206) — NEVER around the
//        // body barrier; returns the synthesized migration + residual typed holes.

// internal/session/session.go — :reload reports the tier; `rune upgrade <pid> <new>`
//   wires primUpgrade. The -vsn token is h.Short() (already used at session.go:834).
```

**Rune surface — a *library* `lib/hot.rune` (NOT core):**

```
data Tier : U is tier0 | tier1 | tier2 | tier3 end
-- the upgrade Step constructor (extends R-OTP/R-CALC's Step relation):
UPGRADE : {S_old S_new : U} -> (mig : S_old -> S_new)
       -> Step (proc h_old s) (upg h_old h_new) (proc h_new (mig s))
-- the correctness obligation (auto for tier 1/2, discharged for tier 3):
upgradePreservesInvariant :
  {S_old S_new : U} -> (I_old : S_old -> U) -> (I_new : S_new -> U)
  -> (mig : S_old -> S_new) -> (s : S_old) -> I_old s -> I_new (mig s)
-- the tier-2 migration is JUST univalence transport (no new machinery):
transportMigrate : {A B : UF} -> El (Equiv A B) -> El A -> El B
-- transportMigrate e x  is  castU A B (uaGlue A B (equivFun … e) …) x   ~> f x
primUpgrade : {S_old S_new Call Cast : U} -> Pid (GenMsg Call Cast)
           -> GenServer S_new Call Cast -> (S_old -> S_new) -> IO Unit
```

## Worked micro-example (the teachable artifact)

`counter_upgrade.rune` — the counter gen_server of `R-OTP.md:507-539`, upgraded
three ways. The furnace beat: **rename (free) → represent differently (transport,
automatic + proven) → add a field (gated, minimal obligation).**

```
-- the original (R-OTP.md:512): state is Nat, cast `inc`, call `get`.
data Call : U is get end
data Cast : U is inc end
counter : GenServer Nat Call Cast is
  record
    init       is pureIO 0 end
    handleCall is fn (msg s) is case msg of get -> pureIO (pair s s) end end
    handleCast is fn (msg s) is case msg of inc -> pureIO (succ s) end end
    terminate  is fn (r s) is pureIO unit end
  end
```

**Tier 1 — rename.** Rename `counter` to `tally`, body unchanged. `names["tally"]`
binds the *same hash*; `ClassifyUpgrade` returns **tier 0** (`h_new == h_old`) — a
no-op, the running process is untouched. (Had the body also changed — a faster
`handleCast` — it would be **tier 1**: `S = Nat` unchanged, migration `= id`,
relink, `upgradePreservesInvariant = λs i. i`.)

**Tier 2 — representation change `Nat ⟷ Vec-of-tallies`, via transport.** New state
`Vec Unit` (a list of `n` tally marks) instead of `Nat`. The iso is the canonical
`Nat ≃ Vec Unit` (`f = replicate unit`, `g = length`, with `s, t` the round-trip
proofs, ch35/ch36-shaped). `ClassifyUpgrade` finds the supplied/derived `Equiv (fib
Nat) (fib (Vec Unit))` and returns **tier 2**:

```
counterV : GenServer (Vec Unit) Call Cast is … (inc = cons unit s; get = length s) end
natVecEquiv : El (Equiv (fib Nat) (fib (Vec Unit))) is equivFromIso … replicate length s t end
migrate : Nat -> Vec Unit is fn (s) is castU (fib Nat) (fib (Vec Unit)) (uaGlue … natVecEquiv) s end
-- kernel reduces  migrate s ~> replicate unit s  (eval.go:2657-2682); DEPLOYS as that (B5).
```

The developer writes **no migration body** beyond the iso — `migrate` *is* the
transport, it runs (erases to `replicate unit` applied), and
`upgradePreservesInvariant` holds **automatically** because `I_new = subst
(uaToEq natVecEquiv) I_old`. The headline: *the provably-correct automatic state
migration no other system has.* `primUpgrade pid counterV migrate` on BEAM is
`sys:change_code` calling `replicate`; on the shim it re-points and runs `replicate`.

**Tier 3 — add a field `lastTouched : Time`.** New state `Sig Nat (λ_. Time)` — the
count *plus* a timestamp with no old source. `ClassifyUpgrade` finds no `Equiv`
(information added) → **tier 3**. `DeriveMigrate` walks the telescope: `count : Nat`
is unchanged → identity projection (tier 1); `lastTouched : Time` is **new** → a
residual hole `?init_lastTouched : Nat -> Time`. The upgrade is **gated** until
filled:

```
migrate3 : Nat -> Sig Nat (λ_. Time) is fn (s) is pair s epoch0 end   -- fills ?init_lastTouched
```

plus the developer discharges `upgradePreservesInvariant` for the new field's part
of `I_new`. **Minimal obligation:** the developer wrote *only* `epoch0` (the new
field's initializer) and its one-line proof — `count` migrated itself. Rollback
`Sig Nat (λ_. Time) -> Nat` is `Fst` (information-discarding, available); a *proven*
rollback needs the reverse invariant proof.

What the learner sees: **a rename costs nothing (content-addressing saw it was the
same), a representation change costs an iso and runs as transport with the invariant
carried for free, and a breaking change costs exactly the novelty — one
initializer, one proof — and nothing more.** The three tiers are the three honest
prices of change.

## Risks / open sub-questions

- **Tier 2 requires a *fibrant* equivalence (`UF`, not `U`).** The state type must
  be `fib`-embeddable for `uaGlue`/`castU` to apply. First-order state (Nat, Vec,
  records of these) embeds fine; state containing functions or large universes may
  not have a usable `Equiv` in `UF`. *Mitigation:* tier 2 is opportunistic — when
  the fibrant `Equiv` exists it is automatic, else the change falls to tier 3
  (supply a plain `S_old -> S_new`). **Status: tier-2 ready for first-order state
  (A7 landed); higher-type state is a fibrancy sub-question.**
- **The inner-path → outer-`Eq` bridge for tier-2 invariant carry.** `I_new = subst
  (uaToEq e) I_old` needs the inner `pathU` to bridge to the outer `Eq` that `subst`
  consumes. *Mitigation:* confirm the bridge exists, or state the invariant at the
  inner-path level via `pathJ`. **Status: needs confirmation against the A7 surface
  — flagged.**
- **Iso auto-derivation generality.** §B auto-derives the *record-shaped* migration,
  but deriving the *field iso itself* is only canonical for structurally-bijective
  datatypes (the "deriving Generic" problem). *Mitigation:* ship record synthesis +
  identity/transport-on-supplied-iso first; park general iso-derivation. **Status:
  record synthesis ready; iso-derivation research (parked).**
- **Message-interface migration (tier-3 changing `Call`/`Cast`).** Buffered in-flight
  messages of the old type must be retyped or rejected. *Mitigation:* apply §B to
  `Call`/`Cast`; drain the mailbox first or reject if buffered old-typed messages
  cannot be retyped. The exact drain-vs-retype policy is a faithfulness question
  against OTP's `Extra`. **Status: interface-preserving (tier 1/2) ready;
  interface-changing tier-3 is a medium-risk sub-question.**
- **Atomicity across a supervision tree.** Upgrading a subtree is a *set* of
  `UPGRADE` steps; a mid-sequence failure leaves a mixed-version tree. *Mitigation:*
  stage upgrades supervisor-down; on failure roll back already-upgraded children
  (tier 1/2 always reversible). Tree-wide transactional upgrade (OTP `relup`/`appup`)
  is a follow-up. **Status: per-process ready; transactional tree upgrade is
  research.**
- **Rollback-to-arbitrary-ancestor composes migrations.** Composition of tier-2
  transports is fine (equivalences compose); a chain crossing a tier-3 boundary is
  reversible only if every tier-3 step supplied a backward witness. *Mitigation:*
  the store has every version, so any composite exists; the composite proof is the
  composition of per-step proofs. **Status: pairwise rollback ready; arbitrary-
  ancestor ready iff every intervening step is reversible.**

## Test / gate plan

- **Containment gates (Thompson, must pass):** no change to `core/term.go`,
  `core/hash.go`, `defFormatVersion`/`hashFormatVersion`; a test asserts an existing
  pure def's hash is byte-identical after `store/otp.go` registers `primUpgrade`
  (the fib/IO/quotient pattern). `primUpgrade` is a rigid `IForeign` head, NOT in
  `innerTaint`.
- **Tier-classification correctness:** a table-driven test over `(h_old, h_new,
  supplied)` asserting §A returns the right tier — identical hash → 0 (incl.
  rename), convertible state → 1, supplied `Equiv` → 2, supplied total fn → 3,
  nothing → reject. Mutation target: a classifier that mis-routes a breaking change
  to tier 1 must let an ill-typed migration through and break the test.
- **Tier-2 deploy gate (Lambert, the headline):** `counter_upgrade.rune`'s tier-2
  transport migration runs on BEAM (`sys:change_code` calling the forward map) and
  on the JS/Go shim, migrating `Nat`→`Vec` state with identical observable
  post-upgrade behaviour (`get` returns the same count). Reuses the B5 normalize-
  then-erase deploy.
- **Invariant-preservation gate (the verified part):** `upgradePreservesInvariant`
  type-checks and its proof is accepted — `λs i. i` (tier 1), `subst`-over-`uaGlue`
  (tier 2, *automatic, no hand-written proof*), developer discharge (tier 3).
- **Rollback gate:** a tier-2 upgrade then its reverse-`UPGRADE` returns the process
  to a state observably equal to the original (round-trip via the iso's `s`/`t`);
  a tier-3 upgrade *without* a backward witness has *no* rollback step (the type
  system refuses it).
- **In-flight-message gate:** messages sent during an `UPGRADE` are handled (FIFO) by
  the new code against `migrate s` — none lost, none reordered (trace identical to
  upgrading on a quiesced mailbox).
- **Dev-reload gate (§F.i):** a REPL `:reload` of a changed `counter` re-points
  `names`, re-elaborates dependents, reports the tier; the old hash still resolves
  (a cert against the old hash still holds, `cert.go:70`).

## Unblocks / dependencies

- **Depends on (hard):** **R-OTP/D5** (the running `gen_server` + the `Step` LTS —
  `UPGRADE` is a new constructor of *its* `Step`); **content-addressing**
  (`store/store.go` — the immutable N-version `defs` + mutable `names`, the
  substrate of tiers 0/1 and rollback); **R-SUM/C1** (state-as-Σ/record, the
  substrate of §B); **A7/R-UA** (`uaGlue`/`castU`/transport — the tier-2 migration,
  landed); **R-ERASE2/B5** (normalize-then-erase deploy of the transport, landed);
  **B3/BEAM** (the `sys:change_code` projection).
- **Unblocks:** tier-2 invariant-carry is the first *deployed* use of univalence-as-
  engineering — the M2 spirit gate made operational on a running system; the
  dev-loop half (§F.i) is an immediate Unison-class `update` for the REPL, ready
  independent of the BEAM half.
- **Readiness:** **Dev-reload (§F.i) ready-to-build now** (content-addressing +
  dependency walk; no new runtime). **Tiers 0/1 ready** (hash equality + existing
  conversion). **Tier 2 ready** for first-order fibrant state (A7 + B5 landed).
  **Tier 3 ready** for the gated total-migration + per-field synthesis (R-SUM
  landed). **`primUpgrade` + the BEAM `code_change` projection ready on D5 + B3.**
  **Research-gated:** transactional tree-wide upgrade (R-COIND/E2-shaped, like
  R-OTP's liveness); the inner-path→outer-`Eq` invariant bridge; general iso
  auto-derivation; interface-changing tier-3 message migration.

---

**Status: substantially ready-to-build in layers, with the verified-liveness tail
research-gated** — mirroring R-OTP's posture. The dev-loop, the tier classification,
the migration synthesis, and the tier-2 transport-migration (the headline:
*automatic, provably-correct, deploying state migration via univalence*) are all
ready on landed substrate (A7, B5, B3, R-SUM, content-addressing); only transactional
tree-wide upgrade and a few faithfulness sub-questions are research. The containment
line holds exactly as R-OTP's did: **one `IForeign` op, one `Step` constructor, a
library, and an elaborator pass — zero outer-core growth, no hash bump.**
