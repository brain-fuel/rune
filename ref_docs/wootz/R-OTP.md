# R-OTP — OTP-class concurrency, verified

> Roadmap node D5 (`D5 [I] OTP-class concurrency (verified) ⇐ R-OTP, C3, B3(BEAM)`).
> Telos 3: "OTP-class concurrency" inside the tiered-verified stdlib. Sits on
> **R-EFFECT** (C3 — the `IForeign` IO base + the `primSpawn`/`primSend`/
> `primReceive` seam), **R-CALC** (E1 — the process calculus whose `project`
> bottoms out here), **R-COIND** (C5 — servers/streams are coinductive),
> **R-SUM** (C1 — gen_server state + callback records are Σ), and **B3(BEAM)**
> (the backend that makes the runtime near-free). It is the **projection runtime**
> R-CALC names ("the *verified* actor/mailbox/supervision library is R-OTP/D5
> over B3(BEAM)", `ref_docs/wootz/R-CALC.md:611-612`) and the consumer R-EFFECT's
> concurrency seam exists for ("their *typed* meaning is D5/R-OTP/E-track work",
> `ref_docs/wootz/R-EFFECT.md:391-394`).

## Problem (what's stuck/absent today, with file:line)

There is no concurrency anywhere in the kernel, and the three things OTP-class
concurrency requires — *processes that run forever*, *typed mailboxes*, and
*behavioral guarantees* (a supervisor *guarantees* a child is restarted) — each
hit a wall in the current substrate:

- **No process, no mailbox, no spawn.** The erased IR is an untyped lambda
  calculus (`codegen/ir.go`: `IVar/IGlobal/IUnit/ILam/IApp/ILet`); the JS backend
  prints one pure value (`codegen/js.go` — `console.log($show(main))`). The only
  "reach into the runtime" mechanisms are `builtin nat`/`builtin bin`
  (`internal/session/session.go:230,256`), which swap a *pure* datatype's
  representation. There is no `spawn`, no `!`/`send`, no `receive`, no process
  address. R-EFFECT proposes the `IForeign{Op,Args}` node and *names* the seam
  (`primSpawn`/`primSend`/`primReceive`, `ref_docs/wootz/R-EFFECT.md:391`) but
  explicitly defers their *typed meaning*: "their *typed* meaning is D5/R-OTP/
  E-track work; R-EFFECT only commits to 'they are IO primitives with a backend
  extern'" (`R-EFFECT.md:391-394`). That typed meaning is **this node**.

- **A server loops forever; the core is total by construction.** A `gen_server`
  is a process that receives a message, computes a new state, and *loops* — it
  never returns a normal form. But "the eliminator is the only recursion
  principle, so no termination checker exists or is needed" (CLAUDE.md, Phase 4);
  `NormalizeUnfold` reduces every closed term to a normal form
  (`internal/session/session.go:365`); `m.tryRules` fires ι eagerly inside
  `Apply` (`core/eval.go`). A self-referencing receive loop has **no home**.
  R-EFFECT flagged exactly this: "a *recursive* IO loop (`forever : IO a`) needs
  general recursion, which the core forbids… long-running servers wait on
  R-PART" (`R-EFFECT.md:380-383`). The server loop is the R-PART (C4) consumer.

- **A mailbox is typed, and the type is a stream/coalgebra.** A typed actor
  receives a `Msg` and its *protocol* (the sequence of messages it will accept) is
  a coinductive object — exactly what Gleam's `Subject(message)` and selectors
  type. But the outer core is **inductive-only**: "you literally cannot state 'R
  is a bisimulation, therefore P ~ Q'" (`R-COIND.md:31`, `R-CALC.md:37`). A
  *running server's behavior* (an infinite trace of receives) and the *guarantee*
  that a supervisor's restart policy holds forever are both coinductive
  statements. R-COIND (C5) is the substrate.

- **gen_server / supervisor callbacks are records; there are no records.** A
  `gen_server` behavior is a *record of callbacks* (`init`, `handle_call`,
  `handle_cast`, `terminate`) over a state type `S` — Gleam OTP and Hamler both
  model OTP behaviours as records/type-classes. The outer core has no Σ, no
  records: "Sigma/dependent records in the OUTER core (currently absent…)"
  (`humble-humming-elephant.md:114-115`). R-SUM (C1) delivers the record the
  behavior spec is written against.

- **No model of the guarantee.** "A supervisor *guarantees* its children are
  restarted under strategy X within intensity Y" is a *liveness/safety property* —
  but there is no transition system, no temporal statement, nothing to prove a
  guarantee *against*. R-CALC supplies the LTS (`Step : Proc -> Label -> Proc ->
  U`, `R-CALC.md:225-238`) and the Lambert failure rules (`CRASH`/`LOSS`/`DETECT`,
  `R-CALC.md:246-249`); R-OTP must connect the supervisor's *spec* to that LTS so
  the guarantee is a theorem.

So OTP-class concurrency is greenfield, but — and this is the Lambert gift the
roadmap leans on — **the BEAM backend makes the *runtime* near-free**: spawn,
mailboxes, monitors, links, and supervisors already exist as battle-tested
primitives. The work is not building a scheduler (on BEAM); it is (1) giving the
primitives *Rune types*, (2) writing the actor/gen_server/supervisor *as a
verified Rune library* over them, (3) stating the *behavioral guarantees* against
R-CALC's LTS, and (4) for non-BEAM backends, supplying a *scheduler shim* so the
same library runs. The Thompson constraint is the usual one: **zero outer-core
growth, no hash-format bump** — everything is a builtin group + a library +
codegen ops, exactly as quotients/fibrant/interval/Kan shipped.

## Prior art (what the literature/other systems do; cite)

**Erlang/OTP itself — the spec being matched.** OTP organizes execution into
*supervision trees*: internal nodes are supervisors (start/stop/monitor
children), leaves are workers ([Erlang design principles](https://marianoguerra.github.io/otp/doc/design_principles/gen_server_concepts.html)).
A `gen_server` is a process whose loop is hidden behind callbacks (`init`,
`handle_call`, `handle_cast`, `terminate`), created as part of a supervision tree
via `start_link` ([erlang/otp gen_server.erl](https://github.com/erlang/otp/blob/master/lib/stdlib/src/gen_server.erl)).
Supervisors carry a *strategy* (`one_for_one`, `one_for_all`, `rest_for_one`),
a *restart type* per child (`permanent`/`transient`/`temporary`), and a *restart
intensity* (max restarts / period) — these are the parameters whose behavioral
meaning R-OTP must specify. The "let it crash" model is fail-stop: a process dies,
its supervisor restarts it; failures propagate via *links* and are observed via
*monitors*. This is the exact fail-stop/async-reorder model R-CALC already adopted
(`R-CALC.md:104-116`).

**Gleam OTP — type-safe OTP on the BEAM, the closest engineering precedent.**
Gleam OTP is "the result of approaching 2 years of research… striking a balance
between type safety and compatibility with existing Erlang OTP patterns"
([gleam-lang/otp](https://github.com/gleam-lang/otp)). Two load-bearing lessons:
(1) **Full type safety of actors and messages** is achievable — a process address
is typed by the message it accepts (`Subject(msg)`); an actor is a function
`State -> Message -> Next(State)`. (2) **Most of OTP can live in a library over a
tiny runtime core**: "rather than build on top of Erlang's supervisor and
gen_server, Gleam OTP has a small core written in Erlang which implements the
Receiver half of channels, and the rest is implemented in Gleam." This is exactly
the Wootz split: a *minimal* set of `IForeign` runtime primitives (the Erlang
core) + a *Rune library* (the actor/gen_server/supervisor). Crucially: "some
functionality is not possible to represent in a type safe way, so it is not
included" — Gleam *narrows* OTP to its typeable fragment. Wootz goes further:
the typeable fragment is *also provable*.

**Hamler / NVLang — typed OTP behaviours.** Hamler models OTP behaviours with
**type classes** ([EMQ Hamler 0.2](https://www.emqx.com/en/blog/hamler-0-2-otp-behaviours-with-type-classes)),
and NVLang formalizes a **supervision tree as a rooted tree** — internal nodes
are supervisors with `(strategy, restart-set function, limit)`, leaves are workers
([NVLang, arXiv:2512.05224](https://arxiv.org/pdf/2512.05224)). This is the data
shape R-OTP's `Spec` datatype mirrors. The C-ABS clarify (typeclasses vs
records/canonical-structures) is resolved by R-SUM toward **records** (`R-SUM.md`,
the C-ABS resolution), so Wootz models behaviours as *records of callbacks*, the
Gleam shape, not Hamler's type classes.

**Verified actors / distributed systems in proof assistants.** Full mechanized
verification of OTP supervision in Coq/Isabelle is *thin* (the literature has
actor-model fault-tolerance papers but little end-to-end supervision-tree proof —
the search surfaced NVLang's formalization as the most complete). The transferable
technology is from **verified distributed systems**: Verdi (Coq, network
semantics + fault models as a parameter, verified system transformers for fault
tolerance), IronFleet (TLA+-style refinement to executable code), and Disel
(distributed separation logic). The lesson R-OTP takes: **make the fault model a
parameter of the semantics** (Verdi's move) — which R-CALC already did by making
`CRASH`/`LOSS`/`DETECT` *constructors of the `Step` relation* (`R-CALC.md:252`),
so a supervisor proof is "for all fault sequences drawn from these rules."

**Actor calculi & adequacy.** Agha's actor model, the asynchronous π-calculus
(an actor *is* an asynchronous-π process with a private name = its address), and
the distributed-π fault model (Hennessy–Riely, `R-CALC.md:105-108`) give the
*semantics* a supervisor guarantee is stated against. R-CALC already chose
asynchronous name-passing π with HOAS-via-Pi binders (`R-CALC.md:163-186`); an
actor's mailbox is a `Name`, a `gen_server` is a `recv`-loop `Proc`, a supervisor
is a `Proc` that `monitor`s its children and re-`spawn`s on a `fail` label. R-OTP
is therefore *a structured library over R-CALC's `Proc`*, not a new calculus.

Cross-cutting lessons:
- BEAM gives the runtime for free; the job is *types + a library + proofs*, plus a
  *scheduler shim* for non-BEAM (Gleam's "small Erlang core, rest in the language"
  split, generalized).
- An actor address is typed by its message (`Subject(msg)`); lean on QTT for
  *linear* endpoints where a protocol demands single-use (R-CALC.md:121-124).
- A supervision tree is *data* (rooted tree of `(strategy, restart, limit)`); its
  guarantee is a *theorem against an LTS with fault rules as constructors* (Verdi
  + R-CALC).
- Narrow to the typeable/provable fragment (Gleam); add escape hatches (untyped
  `dynamic` interop) as a *typed-leaf* tier, never in the proven core.

## Chosen approach for THIS substrate (concrete; respects containment)

**Recommendation: R-OTP is a *three-layer tiered-verified library* over R-CALC's
`Proc` and R-EFFECT's `IForeign` concurrency primitives — NOT a new calculus and
NOT new core. Layer R0 is a tiny *runtime-primitive seam* (six `IForeign` ops,
the only genuinely-new reality, living in codegen where the strata thesis says it
belongs). Layer R1 is the *typed actor/mailbox API* (a Rune library: typed
addresses, `spawn`/`send`/`receive`, the actor loop via R-PART's `Delay`/`runFor`
or R-COIND's corecursor). Layer R2 is the *behavioral library* (`gen_server`,
`Supervisor`) as *records of callbacks* (R-SUM) whose *guarantees are theorems
against R-CALC's LTS*. The non-BEAM backends get a *single-threaded cooperative
scheduler shim* implementing the same six ops. Zero outer-core growth, no
hash-format bump.**

This is the Gleam split made provable, and it is **Thompson-forced**: the actor is
a library, the semantics is R-CALC's relation, the guarantee is a theorem, reality
is six codegen ops.

### Layer R0 — the runtime-primitive seam (the only new reality)

Six `IForeign` ops (extending R-EFFECT's vocabulary; R-CALC named the first set,
`R-CALC.md:354`). Each is a **bodiless, permanently-neutral builtin** whose *type
is Rune* and whose *meaning is a backend extern* — exactly R-EFFECT's primitive
discipline (`R-EFFECT.md:182-196`), deployable (not inner-tainted), because each
has a *defined erased meaning*:

```
-- store/otp.go : the OTP runtime builtin group (bodiless, neutral heads).
-- Pid is typed by the message it accepts: Pid M  ~  Gleam's Subject(M).
Pid    : U -> U                                    -- a typed process address
World  : U                                         -- (from R-EFFECT)

primSpawn   : {M : U} -> (Pid M -> IO Unit) -> IO (Pid M)   -- spawn a loop, get its addr
primSelf    : {M : U} -> IO (Pid M)                          -- own address
primSend    : {M : U} -> Pid M -> M -> IO Unit               -- async; into mailbox (fire-and-forget)
primReceive : {M : U} -> IO M                                -- block on own mailbox; next message
primMonitor : {M N : U} -> Pid M -> IO (Pid (Down N))        -- watch; get DOWN on its exit
primExit    : {A : U} -> Reason -> IO A                       -- fail-stop: this process dies
```

Six ops, mapped per backend (the conformance table the M4 corpus checks):

| op            | BEAM (B3, near-free)        | JS shim (single-thread)            | Go shim (goroutines+chan) |
|---------------|-----------------------------|-------------------------------------|----------------------------|
| `primSpawn`   | `spawn`/`spawn_link`        | enqueue a coroutine on the runloop  | `go` + a `chan M` mailbox  |
| `primSelf`    | `self()`                    | current-coroutine handle            | goroutine-local pid        |
| `primSend`    | `Pid ! Msg`                 | push to target's mailbox queue      | `ch <- msg` (buffered)     |
| `primReceive` | `receive Msg -> … end`      | yield until mailbox non-empty       | `<-ch`                     |
| `primMonitor` | `erlang:monitor(process,P)` | register a DOWN callback            | watch goroutine done-chan  |
| `primExit`    | process death + signal      | throw to runloop; deliver DOWN      | panic→recover→DOWN         |

Notes:
- **`Pid M` is the typed mailbox.** It is a *new* fibrant-style opaque former in
  `store/otp.go` (no constructors, no eliminator — like R-EFFECT's `World`), so
  `primSend p m` only type-checks when `m : M` and `p : Pid M`. This is Gleam's
  `Subject(msg)` type safety, in Rune. `Pid M` *erases to the unit token* (a type
  former, `typeRefs` set, `session.go:505`); a runtime pid is a backend value.
- **Selective receive is the honest hard part.** BEAM's `receive` does pattern
  *selection* over the mailbox; the typed-API floor is `primReceive : IO M` (next
  message, FIFO). Selective receive (skip messages not matching a pattern) is a
  *library* combinator `selectiveReceive : (M -> Bool) -> IO M` implemented on
  BEAM by a guarded `receive`, on the shim by a scan-and-requeue. Park the
  *dependently-typed* selective receive (a session index narrowing the mailbox
  type per state) under R-CALC's typed-channel refinement; ship the predicate
  version first.
- **Why these are deployable, not inner-tainted** (the §F contrast, `session.go:
  621`): unlike `ua`/`transp`, each op has a *defined* erased meaning (a foreign
  op). They are **not** added to the `innerTaint` set. They *are* added to
  R-IR's structural `IInner`-style "needs runtime" classification only insofar as
  they require the actor runtime shim — but that is a backend-capability check
  (does this backend implement `spawn`?), not a taint.

### Layer R1 — the typed actor (a Rune library, `lib/actor.rune`)

An actor is **a function `S -> M -> Next S` looped over a mailbox** — the Gleam
shape. The loop is the R-PART / R-COIND consumer:

```
-- what an actor does with one message: keep going (new state) or stop.
data Next (S : U) : U is
  continue : S -> Next S
  stop     : Reason -> Next S
end

-- the actor's behavior: initial state + a step function.  (S is the actor's
-- private state, M its mailbox message type.)
Actor : U -> U -> U is fn (S M : U) is
  Sigma S (fn (s0 : S) is (S -> M -> IO (Next S)) end)   -- (init, step)   [R-SUM]
end

-- the LOOP.  Two honest implementations, both contained:
--  (a) R-PART fuel-Delay (ships first, deploys & runs):
runActor  : {S M : U} -> Actor S M -> Pid M -> Fuel -> IO Unit
--  (b) R-COIND corecursor (the principled, unbounded version; drop-in by R-PART's
--      staging — R-PART's Delay is "the swap-in point for the principled
--      coinductive partiality monad", 00-INDEX.md:69-70):
runActorForever : {S M : U} -> Actor S M -> Pid M -> IO Unit   -- corecursive

spawnActor : {S M : U} -> Actor S M -> IO (Pid M)
spawnActor a = primSpawn (fn (self) is runActorForever a self end)
```

The receive loop (`runActorForever`) is *the* place general recursion is needed:
`loop s = bind primReceive (λm. bind (step s m) (λnext. case next of continue s' →
loop s' | stop r → primExit r))`. This is **not** structurally recursive (it
recurses on a *received message*, not a constructor sub-term), so per
`R-PART.md:42-51` it has no eliminator-only form. R-OTP therefore **consumes
R-PART (C4)**: `runActor` ships first as a fuel-bounded `Delay`/`runFor` loop
(`R-PART.md`, the Tier-1 fuel construct, "inductive, deploys and runs"), and
`runActorForever` is the R-COIND corecursive upgrade (`R-COIND.md`, the
`Nu`/`unfold` group, productivity-by-construction) — *the same surface*, swapped
when C5 lands. This staging is **exactly what R-PART designed for** ("chosen so
R-COIND can later swap in the principled coinductive partiality monad with no
surface change", `00-INDEX.md:55-57`). The actor loop is the forcing consumer of
both.

Typed mailboxes, links, monitors are thin wrappers; `call` (synchronous
request/reply) is a library combinator over async `send`+`receive` with a
fresh reply `Pid` and a monitor for crash-during-call:

```
call : {Req Rep : U} -> Pid (Request Req Rep) -> Req -> IO (Result CallError Rep)
-- send (Request req replyPid); monitor server; receive reply OR DOWN; demonitor.
```

### Layer R2 — behaviors as records, guarantees as theorems

**`gen_server` is a record of callbacks** (R-SUM; the Gleam/Hamler shape, the
C-ABS-via-records resolution):

```
GenServer : U -> U -> U -> U is fn (S Call Cast : U) is        -- state, call-msg, cast-msg
  Record
    init       : IO S
    handleCall : Call -> S -> IO (Sigma Reply (fn (_) is S))   -- reply + new state
    handleCast : Cast -> S -> IO S
    terminate  : Reason -> S -> IO Unit
  end                                                          -- nested Σ sugar [R-SUM]
end

-- a gen_server IS an actor whose message is Call|Cast and whose step dispatches:
genToActor : {S Call Cast : U} -> GenServer S Call Cast -> Actor S (GenMsg Call Cast)
startLink  : {S Call Cast : U} -> GenServer S Call Cast -> IO (Pid (GenMsg Call Cast))
```

`startLink` = `spawnActor (genToActor g)` + a link to the caller (the OTP
`start_link` contract that ties the new process into the supervision tree). The
*guarantee* `gen_server` offers — *every `call` either returns a `Rep` or a
`CallError` reporting the server crashed* (never a silent hang) — is a **theorem**
stated against R-CALC's LTS: under the fault rules `{CRASH, LOSS, DETECT}`, the
`call`/`handleCall` protocol *weakly bisimulates* a total request-reply function
extended with a crash outcome. The proof (E3, per concrete server) exhibits the
bisimulation; R-OTP delivers the *statement* and the projection.

**The supervisor is a tree (data) + a restart engine (a gen_server) + a
guarantee (a temporal theorem).** The spec, mirroring OTP / NVLang:

```
data Strategy : U is oneForOne | oneForAll | restForOne end
data Restart  : U is permanent | transient | temporary end
data ChildSpec (M : U) : U is
  child : Restart -> (Unit -> IO (Pid M)) -> ChildSpec M    -- restart policy + a start fn
end
-- a supervision tree is a rooted tree: internal = supervisor(strategy, intensity),
-- leaves = workers (NVLang's shape, arXiv:2512.05224):
data SupTree : U is
  worker : {M : U} -> ChildSpec M -> SupTree
  sup    : Strategy -> Intensity -> List SupTree -> SupTree
end
Intensity : U is Sigma Nat (fn (_) is Nat)   -- (maxRestarts, periodSeconds)  [R-SUM]

startSup : SupTree -> IO (Pid SupMsg)         -- spawn the whole tree
```

**The supervisor's behavioral guarantee — the heart of "verified OTP".** State it
as three properties against R-CALC's LTS (`Step`, the fault rules), each a Rune
proposition the supervisor library proves (or, for the per-protocol part,
hands to E3):

1. **Restart safety (one_for_one).** If a `permanent` child crashes
   (`Step (childProc) (fail c) crash`) and the supervisor has not exhausted its
   intensity, then within finitely many `τ`-steps a *fresh* child with the same
   `ChildSpec` is running. Stated: `restarted : (sup running) -> (child crashes) ->
   (intensity ok) -> Eventually (fresh child running)`. `Eventually` is a
   bounded-trace predicate (a Σ of a step count) — *not* full LTL, the
   teachable/provable fragment.
2. **Intensity bound (safety).** No more than `maxRestarts` restarts occur within
   `periodSeconds`; on exceedance the supervisor itself crashes (escalation),
   propagating to its parent. A pure invariant over the restart-counter state —
   provable by ordinary induction (no coinduction).
3. **Strategy fidelity.** `oneForAll` restarts *all* siblings on any crash;
   `restForOne` restarts the crashed child and those started after it. A
   *relational* spec: the multiset of post-crash running children equals the
   strategy's prescribed set. Provable structurally over `SupTree`.

These are the guarantees a supervisor *makes* — the thing "verified OTP" means.
Properties (2) and (3) are **provable today-shaped** (induction over data/state,
needs only R-SUM). Property (1)'s `Eventually` over an unbounded fault stream is
the **coinductive/liveness part** that needs R-COIND (for the infinite-trace
model) and ties to E2's weak-bisimulation; ship the *bounded* version (within `k`
steps) first, generalize to the coinductive `Eventually` when C5 lands.

### Connection to the process-calculus track (R-CALC / E1–E3)

R-OTP is the **runtime end** of R-CALC's projection. R-CALC's `project : Proc ->
IO Unit` (`R-CALC.md:336-360`) bottoms out at *these* primitives; R-OTP supplies
the typed, verified library that table targets:

| R-CALC `Proc` ctor | R-CALC projects to        | R-OTP realizes as                     |
|--------------------|---------------------------|----------------------------------------|
| `send c m`         | `primSend c m`            | typed `send : Pid M -> M -> IO Unit`   |
| `recv c k`         | `bindIO primReceive …`    | the `runActor` receive loop            |
| `par P Q`          | `bindIO (primSpawn …) …`  | `spawnActor`                           |
| `nu k`             | `bindIO primNewChan …`    | `primSpawn` returns a fresh typed `Pid`|
| `bang P`           | `primSpawnLoop …`         | `runActorForever` (R-COIND)            |
| `crash`            | `primExit`                | `primExit` + supervisor restart        |

The **adequacy theorem** R-CALC names ("the projected actor's observable behavior
*refines* (weak-simulates) the `Proc`'s LTS", `R-CALC.md:362-368`) is, for OTP,
*the supervisor guarantee proven against the LTS*. R-OTP makes the guarantees
*statable*; E2 (bisimulation) makes them *provable in general*; E3 proves them for
*concrete protocols* (a supervised replicated counter — the M0 artifact). So the
correctness chain is: **R-OTP states the guarantee → R-CALC's `Step` + fault rules
are the model → R-COIND's coinduction makes the infinite-behavior part statable →
E2's bisimulation library proves it → E3 discharges it per protocol.** R-OTP owns
the *statement and the library*; the *general proof machinery* is E2; the
*per-protocol discharge* is E3.

### Non-BEAM: the scheduler shim

BEAM gives spawn/mailbox/monitor/exit for free. **Every other backend needs a
scheduler**, and this is the one piece of genuine new runtime to *write* (BEAM
just *binds*). Recommendation: a **single-threaded cooperative scheduler** in each
backend's runtime shim (R-IR's `Shim`, `R-IR.md:278`), implementing the six R0
ops:

- **A runloop** holding a queue of *ready* processes, each a `(pid, mailbox FIFO,
  continuation)`. `primReceive` *yields* the current continuation back to the
  runloop tagged "blocked on mailbox"; `primSend` pushes to a mailbox and marks
  the target ready; the runloop dispatches the next ready process. This is the
  Gleam "small core implements the Receiver half of channels" idea, generalized
  to a non-preemptive scheduler.
- **JS:** continuations are async functions / generators; the runloop is a
  microtask queue. (Single OS thread; concurrency is cooperative — honest about
  no true parallelism, which is fine for the actor *semantics*.)
- **Go:** the most faithful non-BEAM — `primSpawn` = a goroutine with a buffered
  `chan M` mailbox, `primSend` = channel send, `primReceive` = channel receive,
  `primMonitor` = a done-channel watcher, `primExit` = panic/recover delivering a
  DOWN. Go gives *real* parallelism + cheap goroutines, the closest BEAM analog.
- **The shim's fault model must match BEAM's fail-stop** (Lambert): a `primExit`
  or an uncaught error in a process *only* kills that process and delivers DOWN to
  its monitors — it must not crash the runloop. The M4 conformance corpus pins
  this: "kill a worker; supervisor restarts it" produces the same observable
  trace on BEAM and on each shim.

The shim is **codegen-stratum reality** (Thompson-clean): it lives in each
backend's runtime support, behind R-IR's `Backend`/`Shim` boundary, never in the
core. The *same* `lib/actor.rune` + `lib/otp.rune` library runs on BEAM (binding)
and on JS/Go (the shim) — proven once, run everywhere, the tiered-stdlib promise.

### Why this respects all three smiths

- **Thompson:** zero outer-core constructors, no hash-format bump. `Pid`/`World`
  are bodiless builtins (the §F/R-EFFECT pattern); `Actor`/`GenServer`/`Supervisor`
  are *library records and datatypes*; the six runtime ops are `IForeign` codegen
  reality; the guarantees are *theorems over R-CALC's relation*. Nothing new in
  `core/`.
- **Savage:** the API *is* OTP — a learner who knows `gen_server`/`Supervisor`
  recognizes it immediately, and one who doesn't learns the canonical concurrency
  curriculum. The furnace on-ramp is sharp: **spawn an actor and watch it run
  (test) → write a `gen_server` with typed callbacks → property-test that crashes
  are restarted → *prove* the supervisor guarantee.** "I tested my supervisor
  restarts the child" → "I proved it always does" is the continuous on-ramp the
  spirit demands.
- **Lambert:** the failure model is BEAM's actual fail-stop/let-it-crash, not an
  idealization; the same source simulates (shim scheduler) and deploys (BEAM); a
  partition is R-CALC's `LOSS` across a cut; a supervisor *actually restarts a
  dead process* on a real BEAM node. Reality first.

## Interfaces & signatures to add (Go + Rune surface as relevant)

**Go — contained, mirroring R-EFFECT's `store/io.go` exactly; no `core/term.go`
or `core/hash.go` change:**

```go
// core/eval.go — mirror of FibInfo/KanInfo/IOInfo (R-EFFECT). Machine.Otp field.
type OTPRole byte
const (
    OTPRoleNone OTPRole = iota
    OTPRolePid       // Pid : U -> U          (opaque typed address; no ctor/elim)
    OTPRolePrim      // any primSpawn/Send/Receive/Self/Monitor/Exit
)
type OTPInfo interface {
    OTPRoleOf(Hash) OTPRole
    OTPPrimOf(Hash) (op string, arity int, ok bool)  // reverse lookup for the eraser
}
// add to Machine:  Otp OTPInfo   (wired in session.elaborator(), like m.Io)
// primitives are RIGID heads (rigidHead true, like a constructor / R-EFFECT prim).

// store/otp.go — the builtin group (pattern of store/io.go AddIO / store/fib.go)
func (s *Store) AddOTP(ioHashes []core.Hash) []core.Hash  // depends on IO/World
func OTPNames() []string
func (s *Store) OTPRoleOf(h core.Hash) core.OTPRole
func (s *Store) OTPPrimOf(h core.Hash) (string, int, bool)
func (s *Store) OTPHashes() ([]core.Hash, bool)

// codegen — NO new IR node: the six ops are R-EFFECT's existing IForeign{Op,Args}
// (codegen/ir.go). R-OTP only adds op NAMES to the vocabulary:
//   "spawn","self","send","receive","monitor","exit"
// Each Backend.Foreign() (R-IR.md:278) maps them; BEAM binds, JS/Go shim.

// internal/session/session.go — register the group in Reset() after AddIO,
// add typeRefs[PidFormer] (Pid erases to unit), wire m.Otp in elaborator().
// Pids/prims are NOT added to innerTaint (R-EFFECT.md:259-267): defined erased
// meaning. A backend-capability check (does this target implement "spawn"?)
// replaces taint for actor programs on a non-actor backend.
```

**Rune surface — *library* modules (NOT core):**

```
-- lib/actor.rune  (Layer R1; over store/otp.go primitives + R-PART/R-COIND)
Pid : U -> U                              -- bodiless builtin (store/otp.go)
data Next (S : U) : U is continue : S -> Next S | stop : Reason -> Next S end
Actor : U -> U -> U                       -- Sigma S (S -> M -> IO (Next S))   [R-SUM]
spawnActor : {S M : U} -> Actor S M -> IO (Pid M)
send       : {M : U} -> Pid M -> M -> IO Unit
call       : {Req Rep : U} -> Pid (Request Req Rep) -> Req -> IO (Result CallError Rep)
runActor        : {S M : U} -> Actor S M -> Pid M -> Fuel -> IO Unit      -- R-PART tier
runActorForever : {S M : U} -> Actor S M -> Pid M -> IO Unit              -- R-COIND tier

-- lib/otp.rune  (Layer R2; records + supervision)
GenServer : U -> U -> U -> U              -- record of init/handleCall/handleCast/terminate
startLink : {S Call Cast : U} -> GenServer S Call Cast -> IO (Pid (GenMsg Call Cast))
data Strategy : U is oneForOne | oneForAll | restForOne end
data Restart  : U is permanent | transient | temporary end
data SupTree  : U is worker : ... | sup : Strategy -> Intensity -> List SupTree -> SupTree end
startSup  : SupTree -> IO (Pid SupMsg)

-- the GUARANTEES (the verified part; stated against R-CALC's Step LTS)
restartSafe   : (t : SupTree) -> (c : crashesIn t) -> intensityOk t -> EventuallyRestarted t c
intensityBound: (t : SupTree) -> RestartsWithin (maxR t) (period t)
strategyFidelity : (t : SupTree) -> (c : crash) -> RunningAfter t c = prescribed (strat t) c
```

## Worked micro-example (the teachable artifact)

`counter_sup.rune` — a supervised counter `gen_server`, the M0/D5 artifact: it
*runs* (BEAM + shim), it has *typed* messages, and its supervisor's restart is a
*proven property*. The furnace beat: **run → test the crash → prove the restart.**

```
-- a counter gen_server: cast `inc`, call `get`.
data Call : U is get  end
data Cast : U is inc | boom end          -- `boom` crashes the server (let-it-crash)

counter : GenServer Nat Call Cast is
  record
    init       is pureIO 0 end
    handleCall is fn (msg s) is case msg of get -> pureIO (pair s s) end end
    handleCast is fn (msg s) is
      case msg of
        inc  -> pureIO (succ s)
        boom -> primExit crashed              -- deliberate crash
      end
    end
    terminate  is fn (r s) is pureIO unit end
  end

-- supervise it: one_for_one, permanent, at most 3 restarts / 5s.
tree : SupTree is
  sup oneForOne (pair 3 5)
      (cons (worker (child permanent (fn (_) is startLink counter end))) nil)

main : IO Unit is
  seq
    root <- startSup tree
    c    <- whereIs root "counter"            -- typed Pid (GenMsg Call Cast)
    send c (cast inc)                          -- count -> 1
    send c (cast boom)                         -- CRASH; supervisor restarts -> fresh 0
    n    <- call c get                          -- observes the RESTARTED server: 0
    primPutStr (showNat n)                      -- prints 0 : the restart is visible
  end
```

What the learner sees and can verify:
1. **It type-checks.** `send c (cast inc)` only checks because `c : Pid (GenMsg
   Call Cast)` and `inc : Cast` — the typed mailbox catches a mis-sent message at
   compile time (Gleam's safety, in Rune). Certified like any def (`AddDef`).
2. **It runs (M0, Lambert).** `rune run counter_sup.rune main` on BEAM spawns a
   real supervisor + worker; `boom` kills the worker; the supervisor restarts it;
   `get` returns `0` (fresh state). On the JS/Go shim the *same* program produces
   the *same* output — cross-backend conformance.
3. **Watch the crash (the teachable failure moment).** Toggle the intensity to
   `(0, 5)` (no restarts allowed): the supervisor escalates, the program reports
   the tree's death. Failure is *observable*, not abstract — Lambert's beat.
4. **Prove the restart (the furnace climax).** `restartSafe tree (boomCrashes …)
   intensityOK : EventuallyRestarted tree …` — the learner *proves* that under the
   fault rules, the permanent child is restarted within the intensity window. The
   first time a Wootz user *proves a concurrency guarantee* about a running system.
   First as a bounded (`within k steps`) theorem (provable on R-SUM today-shaped),
   then — once R-COIND lands — as the full coinductive `Eventually`.

Contrast artifact (containment visible): `bad.rune` with `n : Nat is
runActorForever …` trying to extract a `Nat` from a never-returning loop — a type
error (`runActorForever : IO Unit`, and only `main` runs; you cannot pull a pure
value out of a live actor). The firewall R-EFFECT/R-PART draw is visible: the
kernel *reasons about* the actor; the runtime *runs* it.

## Risks / open sub-questions

- **The server loop needs general recursion (hard dep on R-PART, then R-COIND).**
  `runActorForever` is non-structural recursion on a received message
  (`R-PART.md:42-51`). *Mitigation:* ship `runActor` as R-PART's fuel-`Delay`
  (Tier-1, deploys & runs) first; `runActorForever` is the R-COIND drop-in (the
  swap R-PART staged, `00-INDEX.md:55-57`). **Status: R1 actor *loop* is
  ready-to-build the moment R-PART (C4) lands (ready-to-build per 00-INDEX); the
  unbounded/coinductive loop waits on R-COIND (C5).**
- **gen_server callbacks + supervisor intensity are records/Σ (hard dep on
  R-SUM/C1).** `Actor`, `GenServer`, `ChildSpec`, `Intensity` all use Σ/records.
  R-SUM is ready-to-build (`00-INDEX.md:35-43`) but not yet *landed*. **Status:
  R2 (behaviors) blocked on C1; R0 (primitives) + a *non-record* R1 actor are
  not.**
- **Behavioral guarantees: the `Eventually`/liveness fragment is coinductive.**
  Restart safety quantifies over an *unbounded fault stream* → an infinite-trace
  liveness property → R-COIND + E2's weak bisimulation. *Mitigation:* ship the
  **bounded** guarantee (`within k steps`, a Σ of a step count, provable
  structurally) first; the full coinductive `Eventually` is the C5/E2 upgrade.
  Intensity-bound and strategy-fidelity (safety/invariant) are provable today-
  shaped (induction). **Status: safety guarantees ready (on C1); liveness gated on
  R-COIND + E2.**
- **Selective receive's typed meaning.** BEAM `receive` selects by pattern; the
  typed floor is FIFO `primReceive : IO M`. A *session-typed* mailbox (the receive
  type narrows per protocol state) is the R-CALC typed-channel refinement
  (`R-CALC.md:121-124`, QTT-linear endpoints). *Mitigation:* ship FIFO + a
  predicate `selectiveReceive`; park dependent session typing. **Status:
  predicate version ready; dependent version research.**
- **Pid identity, links vs monitors, and exit-signal propagation.** OTP's link
  (bidirectional, propagates exit) vs monitor (unidirectional, delivers DOWN) is
  semantically subtle and the supervisor *depends* on links to tear down a failed
  subtree. *Mitigation:* model both as primitives; the supervisor uses monitors
  for restart decisions and links for shutdown propagation, matching OTP. The
  *exact* exit-signal algebra (trap_exit, kill vs normal) is a Lambert-grade
  faithfulness question; pin it against BEAM in the conformance corpus. **Status:
  monitor-based restart ready; full link/exit-signal algebra is a faithfulness
  sub-question (medium risk).**
- **The non-BEAM scheduler's fidelity to BEAM semantics.** A single-threaded
  cooperative shim does *not* give true parallelism or preemption; a CPU-bound
  actor can starve others (BEAM preempts by reduction count). *Mitigation:* the
  shim is honest about cooperative scheduling; the actor *semantics* (message
  ordering per pair, fail-stop) match, which is what the proofs are about;
  fairness/preemption differences are documented and conformance-tested only for
  *observable message traces*, not timing. Go's goroutine shim is closest to BEAM
  (real parallelism). **Status: cooperative shim ready-to-build; preemptive
  fairness is a documented non-goal of the shim, not the proofs.**
- **Adequacy (projection-refines-LTS) is the deep theorem (E3).** That the running
  actor *refines* the `Proc`/`Step` model needs a runtime-trace semantics for the
  six ops + a simulation proof — genuinely hard, per-protocol, the heart of E3
  (`R-CALC.md:526-531`). R-OTP delivers the *guarantee statements* and the
  *library*; the *general* adequacy proof is E2/E3. **Status: statements ready;
  general proof is research (E3).**
- **Distributed (multi-node) OTP.** This node is *single-node* OTP (one BEAM VM /
  one runloop). Cross-node pids, global registration, and net-split handling are
  the R-PROTO / E3 distributed layer (where `LOSS`/partition bite for real).
  **Status: single-node ready-shaped; multi-node is E3/R-PROTO.**

## Test/gate plan

- **Containment gates (Thompson, must pass):** no change to `core/term.go`,
  `core/hash.go`, `defFormatVersion`/`hashFormatVersion`; a test asserts an
  existing pure def's hash is byte-identical after `store/otp.go` registers (the
  fib/interval/IO groups already prove this is achievable). `store/otp_test.go`
  mirrors `store/io_test.go`/`store/fib_test.go`. `Pid` is an opaque former that
  declares and erases to unit; the six ops are rigid heads.
- **Typed-mailbox property (Savage/safety):** `send p m` checks iff `p : Pid M`
  and `m : M`; a mis-typed send fails to check (mutation target: a relaxed `Pid`
  type that drops the index must let a bad send through and break the test). The
  actor `step : S -> M -> IO (Next S)` is total per message.
- **Run gate (Lambert, M0):** `counter_sup.rune` runs on BEAM (real
  spawn/supervise/restart) and on the JS + Go shims; `boom` crashes the worker and
  the supervisor restarts it; `get` returns the fresh state on all three. Listing
  chapter (next furnace listing, e.g. ch26) is the acceptance gate, paralleling
  `listings/`.
- **Cross-backend conformance (M4):** identical observable message traces for
  `counter_sup` on BEAM / JS-shim / Go-shim; a crash-and-restart trace is
  byte-identical (observable behavior, not timing). An ordering test pins
  per-pair message order (FIFO) on every backend.
- **Behavioral-guarantee gate (the verified part):** `intensityBound` and
  `strategyFidelity` type-check and their *structural* proofs are accepted (these
  are the safety guarantees, provable on R-SUM). The *bounded* `restartSafe`
  (`within k`) type-checks and proves; the *coinductive* `restartSafe` is the C5/E2
  increment, gated. A reference furnace listing proves the counter's restart.
- **Fail-stop fidelity (Lambert):** a property that a `primExit` in one actor
  delivers DOWN to its monitors and does *not* crash the runloop (shim) / the VM
  (BEAM); that a supervisor exceeding its intensity escalates (crashes upward).
  Mutation target: a shim that lets an actor crash take down the runloop must
  break this.
- **Proof-cache regression:** an actor/gen_server/supervisor def is certified and
  cache-hits on reload exactly like a pure def (extend `store/cert_test.go`); the
  primitives' rigid-head status logs no spurious dependencies (R-FRAME seam
  untouched — these are neutral heads with no ι, like R-EFFECT's primitives).

## Unblocks (which implement nodes, and what they still need)

- **D5 (this node, the implement edge):** **ready-to-build for Layer R0** (the six
  `IForeign` runtime primitives + `Pid` opaque former + BEAM bindings + the
  JS/Go cooperative scheduler shim) **the moment R-EFFECT/C3 lands** (R0 is R-EFFECT's
  primitive discipline with five more ops; R-EFFECT is ready-to-build,
  `00-INDEX.md:44-48`). **Layer R1** (typed actor + receive loop) is ready-to-build
  the moment **R-PART/C4** lands (the fuel-`Delay` loop; ready-to-build,
  `00-INDEX.md:49-59`); the *unbounded* loop waits on **R-COIND/C5**. **Layer R2**
  (gen_server/supervisor records + safety guarantees) waits on **R-SUM/C1**
  (records; ready-to-build, `00-INDEX.md:35-43`); the *liveness* guarantee waits on
  **R-COIND/C5 + E2**. Plus **B3(BEAM)** for the near-free runtime (B3 has no
  research node; it binds the six ops to OTP).
- **M0 vertical slice (BEAM end-to-end):** R-OTP delivers the *actor/supervision
  half* of the M0 artifact (a supervised replicated counter,
  `humble-humming-elephant.md:236-239,285`). Still needs B1/B2/B3 (IR + BEAM
  backend, `00-INDEX.md:168-171`) to map the six ops to OTP, and E-track's tiny
  consensus core for the *replicated* part. R-OTP makes the *supervision* part
  runnable and (safety-) provable.
- **E3 (verified protocols + projection to actors):** R-OTP is the runtime E3
  projects onto — it supplies the typed actor library and the *guarantee
  statements*; E3 supplies the *per-protocol proofs* (adequacy: projection refines
  the LTS) over E2's bisimulation. Needs E2 (R-COIND), R-OTP (this), D5.
- **E4 / C-INFRA (infra-as-code surface):** the supervised actor tree is the
  "inflight" runtime; its proven guarantees are the "preflight" certificate
  (R-CALC's Wing mapping, `R-CALC.md:537-545`). Needs E3 + a `surface/` sugar pass
  (`supervisor … end` / `actor … end` blocks desugaring to the library).
- **R-CALC (the calculus this realizes):** R-OTP is the concrete realization of
  R-CALC's projection table (`R-CALC.md:343-360`) and adequacy statement; R-CALC
  delivers the LTS + fault rules R-OTP's guarantees are *stated against*. The two
  are co-designed: R-CALC = the algebra, R-OTP = the verified runtime library.
- **R-COIND / R-PART / R-SUM (the three substrate deps):** R-OTP is a *forcing
  consumer* of each — R-PART's fuel-`Delay` *for the actor loop* (its named
  consumer), R-COIND's corecursor *for the unbounded loop + liveness guarantee*,
  R-SUM's records *for the behavior specs*. All three are delivered ready-to-build;
  R-OTP is the library that exercises them together.

---

**Status: needs-more-research** overall (the *verified* part — the coinductive
liveness guarantee — is gated on R-COIND + E2, both with R-COIND undelivered/open
for the full version), but **substantially ready-to-build in layers**:
**Layer R0** (the six runtime primitives + `Pid` typed mailbox + BEAM binding +
the non-BEAM cooperative scheduler shim) is **ready-to-build on R-EFFECT/C3 +
B3** — it is R-EFFECT's primitive discipline plus a scheduler, requiring **zero
outer-core growth**, no hash bump (the quotient/fibrant/IO containment pattern,
five more `IForeign` ops). **Layer R1** (typed actor + loop) is **ready on
R-PART/C4** (fuel tier) with the unbounded loop on R-COIND. **Layer R2's safety
guarantees** (intensity bound, strategy fidelity) are **ready on R-SUM/C1**
(structural induction over the supervision tree). The genuinely-open research is
concentrated in two honest places: **(1) the coinductive liveness guarantee**
(restart-safety over an unbounded fault stream — R-COIND + E2), and **(2) adequacy**
(projection-refines-LTS — E3, per-protocol). The Lambert gift holds: **on BEAM the
runtime is near-free (it binds OTP); the work is types, a library, proofs, and the
non-BEAM scheduler shim** — and the failure model (fail-stop/let-it-crash) is
BEAM's own, the same model R-CALC adopted, so the algebra and the runtime meet
cleanly.
