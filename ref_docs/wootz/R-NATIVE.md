# R-NATIVE — closure conversion + the native-backend (LLVM/Cranelift) ABI

> Roadmap node **telos-2 / M4**, the *native* half. The six source-emitter
> backends (js/py/go/rust/jvm/beam) are landed over the shared erased IR; this
> doc covers the prerequisite, and the staged plan, for backends with **no host
> closures and no host GC** — LLVM and Cranelift. It depends on **R-IR** (the
> erased IR) and is independent of R-ERASE2 (the inner-cubical erasure ban stays
> as-is: an `IInner`/inner-tainted program does not deploy on any backend).

## Problem (what the source emitters give for free that native backends do not)

Every shipped backend realizes an `ILam` (`codegen/ir.go:91`) as a **host
closure**: a JS arrow `x => body`, an Erlang `fun`, a Rust `Rc<dyn Fn>`, a Go
`func`, a JVM lambda. The host runtime does two jobs invisibly:

1. **Capture** — the surrounding lexical scope is captured automatically; a free
   variable in the lambda body just *works*, because the host allocates a closure
   record behind the curtain.
2. **Allocate + collect** — the closure record lives on the host's GC heap; the
   host frees it.

LLVM and Cranelift give neither. A function there is a **bare code pointer**: it
has a fixed parameter list and *no* implicit captured environment. A native
backend that naively emitted "a function per `ILam`" would produce code that
references variables that are not in scope. So before any native emission, the IR
must be rewritten so that:

- every function is a **code pointer paired with an explicit captured-environment
  record** (a heap object the program builds), and
- every application **loads the code pointer and the environment and calls** —
  there is no implicit capture left to discover.

This rewrite is **closure conversion** (a.k.a. lambda lifting). It is the
load-bearing prerequisite for *any* native backend, and it is the same rewrite
regardless of whether the eventual target is LLVM, Cranelift, or WASM-machine —
so it belongs in `codegen/` once, ahead of the backends, exactly like `LowerElim`
(`codegen/lower.go`) is shared across the source emitters.

A second native-only concern is the **closure heap**: those `{code, env}` records
and the constructor records (`ICon`/the `{tag,args}` shape) must be allocated and
reclaimed, and Rune has no host GC to lean on. That is the *runtime* half — a
research item, staged below, deliberately separated from the (ready-to-build)
conversion pass.

## Prior art

- **Appel, *Compiling with Continuations*** and **Minamide–Morrisett–Harper,
  *Typed Closure Conversion* (POPL'96)** — the canonical recipe: compute a
  function's free variables, build an environment record, rewrite free-variable
  references to read from that record, and lift the body to a top-level code
  block taking the environment as an extra parameter. Closure conversion is a
  *source-to-source* (here IR-to-IR) transformation, verifiable independently of
  any backend.
- **GHC's STG machine** — a closure is literally `{ info-pointer, free-vars }`;
  `info-pointer` points at the entry code, the free-vars are the environment.
  Constructors are heap objects with an info table carrying the tag. This is the
  shape the native heap layout below adopts. (See also R-IR's eval/apply notes —
  the same Marlow & Peyton Jones currying decision applies, but the *machine*
  realization, the `apply` routine, is this doc's concern.)
- **MLton / OCaml's flambda / Cranelift-based backends (e.g. cranelift in
  Wasmtime)** — all do closure conversion before lowering to an SSA IR; the env
  record is a flat heap tuple, application is `(env, code) = unpack clo; code(env,
  arg)`.
- **The boot/heap-GC choice** — a semispace (Cheney) copying collector is the
  simplest correct GC for a uniform boxed heap (every value is a tagged pointer);
  refcounting (Perceus-style, as in Koka/Lean) is the alternative when in-place
  reuse and predictable latency matter. This doc *specifies the seam*; the choice
  is staged as research (below), not prejudged.

## What landed now (the ready-to-build slice)

`codegen/closure.go` — `ClosureConvert(Program) -> ClosureProgram`, the full
closure-conversion pass over the erased IR, plus a sealed closure-IR (`CIr`) the
native backends will consume. Entirely in the throwaway shadow (THE SHADOW RULE):
no `core/`/`store/` change, no hash bump, and **opt-in** — the existing backends
keep emitting the un-converted `Ir`, so no shipped output changes.

The closure-IR (`CIr`, a sealed sum like `Ir`) replaces `ILam`/`IApp`/`IVar` with
the explicit native forms and carries everything else through:

| `Ir` node            | `CIr` node                    | meaning for a native backend                          |
|----------------------|-------------------------------|-------------------------------------------------------|
| `ILam`               | `MkClosure{Code, Env []CIr}`  | allocate `{code-ptr, env₀ … envₙ}` on the heap        |
| `IApp{Fn,Arg}`       | `AppClosure{Clo, Arg}`        | unpack the closure, call `code(arg, env)`             |
| `IVar` (a binder)    | `CVar{Idx}` / `CEnv{Idx}`     | a code block's argument / a slot of the captured env  |
| (the lifted body)    | `CodeBlock{Name, Body, …}`    | a standalone top-level function of two binders        |
| `IGlobal`/`IForeign` | `CGlobal`/`CForeign`          | unchanged (top-level symbol / host-linked accessor)   |
| `IUnit`/`ILit`       | `CUnit`/`CLit`                | unchanged (the erased token / a native literal)       |
| `ILet`/`IPair`/`IFst`/`ISnd` | `CLet`/`CPair`/`CFst`/`CSnd` | carried through                              |
| `IField`/`ICase`     | `CField`/`CCase`              | carried through (the eliminator's tag dispatch)       |

Each **code block has exactly two binders, innermost-first**: `CVar{0}` is the
lambda's original parameter (the runtime argument), and the environment record is
reached only via `CEnv{slot}`. A code block is **closed** but for those two —
the invariant a native function depends on, tested directly.

The pass:

1. **Free-variable analysis** (`freeVars`, de Bruijn index arithmetic — no name
   capture, no freshening). For a lambda body, index `0` is the parameter and
   every free index `≥ 1` is a capture; captures are recorded in ascending,
   de-duplicated order so a backend's env layout is deterministic and stable.
2. **Lift** every `ILam` (including the lambdas `LowerElim` produces for each
   datatype eliminator) to a fresh top-level `CodeBlock`; rewrite the body so the
   parameter routes to `CVar{0}` and each captured variable to its `CEnv{slot}`.
3. **Build the closure** at the lambda's original site as `MkClosure{Code, Env}`,
   where each env term is the captured outer variable evaluated in the enclosing
   converted context. Nested lambdas lift recursively, each capturing through the
   enclosing code block's own argument/env.

The result `ClosureProgram` carries the `Datas`/`Nat`/`Main`/`IOMain` metadata
verbatim, so a native backend reads it exactly as the source backends read
`Program`, plus the lifted `Blocks`.

### Worked micro-example

`const = λx. λy. x` converts to two code blocks. The inner block captures `x`:
its body is `CEnv{0}` (read the captured `x` out of the environment), not a
`CVar` — closure conversion's whole point. The outer block returns
`MkClosure{Code: inner, Env: [CVar{0}]}` — a closure that captures the outer
block's argument `x` into env slot 0. A native backend emits each block as a
function `f(arg, env)` and `MkClosure` as a heap allocation `{&inner, x}`.

## Verification (semantics preservation — the M4 gate)

`codegen/closure_test.go` and `codegen/closure_internal_test.go`:

- **free-var correctness** — `freeVars` checked on leaf/lambda/let/case/nested
  de Bruijn terms (`TestFreeVarsLeaf`); the capture-recording on `λx.λy.x`
  (`TestLiftCaptureRecordsFreeVar`) and the empty capture of `λx.x`
  (`TestLiftIdentityIsClosed`).
- **closed-block invariant** — every lifted `CodeBlock` references nothing beyond
  its argument (a `CVar` shifted by inner `CLet` binders) and its env (`CEnv`);
  no `CVar` escapes the block's binders (`TestCodeBlocksAreClosed`). This is the
  property an LLVM/Cranelift function depends on.
- **semantics preservation** — `TestClosureConvertPreservesSemantics` runs the
  conformance corpus (nat arithmetic via `NatElim`; list `length` via the
  recursive `ListElim`; the erasure-drops-proofs `subst` program) through **two
  independent direct evaluators** — one over the original `Ir`, one over the
  converted `CIr` (a code block applied with `(arg, env)`) — and requires an
  IDENTICAL observable value (`$show`-style oracle). Closure conversion is proven
  meaning-preserving without leaning on any host backend. (The alternative
  cross-backend round-trip — emit the converted IR through JS and diff against the
  un-converted JS — is equally valid and can be added when the first native
  backend lands; the evaluator-equivalence check is the host-free version and is
  what gates the pass today.)

All of `go build ./...`, `go vet ./...`, `go test ./...` (incl. `./codegen/` and
`./harness/`) stay green; no existing backend output changes (the pass is opt-in).

## Interfaces added (`codegen/closure.go`)

```go
type CIr interface{ isCIr() }                 // sealed, like Ir, but no bare ILam
type CVar struct{ Idx int }                   // code block's own binder (arg + CLet)
type CEnv struct{ Idx int }                   // a slot of the captured env record
type MkClosure struct{ Code string; Env []CIr }   // {code-ptr, env...} allocation
type AppClosure struct{ Clo, Arg CIr }            // unpack + indirect call
type CodeBlock struct{ Name string; Body CIr; Captures []int }  // a lifted lambda
type ClosureProgram struct{ Datas []DataSpec; Blocks []CodeBlock
    Defs []CDefSpec; Nat *NatSpec; Main string; IOMain bool }
func ClosureConvert(p Program) ClosureProgram
```

(`CGlobal`/`CForeign`/`CUnit`/`CLit`/`CLet`/`CPair`/`CFst`/`CSnd`/`CField`/`CCase`
carry the remaining `Ir` cases through; `freeVars` is the internal analysis.)

## The native-backend ABI (specified now, built next)

A native backend (LLVM IR or Cranelift CLIF) consumes a `ClosureProgram`. The ABI
it must realize, in three pieces — this is the **contract**; the heap *policy* is
the research seam.

### (1) Value representation — a uniform tagged word

Every Rune value is one machine word: a tagged pointer (low-bit tag, NaN-boxing,
or a header word — implementation choice). The variants the IR produces:

| value     | source node           | boxed layout (semispace-friendly)                       |
|-----------|-----------------------|---------------------------------------------------------|
| closure   | `MkClosure`           | `{ header(CLO, nenv), code-ptr, env₀ … env_{n-1} }`     |
| ctor      | `ICon`/`{tag,args}`   | `{ header(CON, arity), tag, field₀ … field_{k-1} }`     |
| unit      | `CUnit`               | a fixed boxed singleton (or an immediate tag)           |
| machine int (`builtin nat`/`bin`) | `CLit{LitNat}` | an immediate i64 / a bignum box (matches the source-backend nat rep) |
| native literal (B4 FFI) | `CLit{LitInt/Str/Ptr}` | immediate i64 / heap string / opaque boxed handle |

The header carries the variant tag and the field/env count — everything the GC
needs to trace the object, and everything `$show` needs to render it (the
conformance oracle is the same `$show` the source backends derive from the rep).

### (2) Calling convention — eval/apply over `{code, env}`

Functions are curried (one binder per `CodeBlock`), matching the source IR. The
saturated case is the fast path:

- a `CodeBlock` compiles to `value code_<name>(value arg, value* env)`;
- `AppClosure(clo, arg)` compiles to `clo = force(clo); code = clo->code-ptr; env
  = &clo->env; return code(arg, env);` — unpack and indirect-call;
- `MkClosure{Code, Env}` allocates `{&code_<Code>, eval(Env₀) … eval(Envₙ)}`.

Multi-argument functions are nested closures (the inner `MkClosure` is the result
of the outer call), so partial application is *already* the natural shape — no
separate PAP machinery is required for correctness. The eval/apply optimization
(detect a saturated multi-arg spine, call directly, build a PAP only on
under-application) is a *later* speed pass over the spine, exactly as R-IR notes;
correctness needs only the curried `{code,env}` calls above.

### (3) Allocation + GC — the research seam (staged)

`MkClosure` and `ICon` are the only allocation sites; `CCase`/`CField` and the
eliminator loops are the only readers. The runtime must:

- provide `value alloc(header, n)` returning an n-slot boxed object;
- trace the live set from the roots (the current evaluation stack + `Main`) using
  the header tags to find the pointer slots;
- reclaim the rest.

**Decision deferred (research):** a **semispace (Cheney) copying collector** is
the recommended first cut — a uniform boxed heap with a forwarding pointer per
object, trivially correct, no fragmentation, and the header already carries the
trace map. Refcounting (Perceus-style) is the alternative if predictable latency
or in-place reuse dominates; it needs the IR to carry ownership/dup-drop, a
larger change. The first native backend should ship the semispace collector and
benchmark before considering refcounting (Lambert: correct first, fast under
benchmark).

## Risks / open sub-questions

- **The GC is genuinely the research wall, not the conversion.** The conversion
  pass is ready-to-build and landed; the closure heap + collector + the stack-map
  / root-finding integration with LLVM/Cranelift is the multi-iteration effort.
  Honest label: **R-NATIVE conversion = ready-to-build (landed); the GC'd closure
  runtime = research, staged below.**
- **Stack maps / precise roots on LLVM/Cranelift.** Precise GC needs the backend
  to emit stack maps (LLVM's `gc.statepoint`/`gc.relocate`, or a shadow stack).
  This is the fiddliest integration point; a conservative collector (scan the
  stack, treat anything that looks like a heap pointer as a root) is a correct,
  simpler first cut that sidesteps stack maps entirely — recommended for the first
  native bring-up, upgraded to precise later.
- **`builtin nat` representation parity.** The native int rep must match the
  source backends' nat rep so the cross-backend conformance corpus stays
  byte-identical; `CLit{LitNat}` already pins the canonical decimal magnitude, so
  the native backend uses i64 (with a bignum box past the machine bound, as the
  arbitrary-precision source backends do).
- **Inner-taint ban is unchanged.** A native backend, like every backend, refuses
  an inner-tainted program; R-ERASE2 (lifting that ban) is orthogonal and not on
  this path.
- **No PAP needed for correctness.** Currying via nested `MkClosure` is correct
  but allocates one closure per argument; the eval/apply saturation pass is the
  optimization, deferred under benchmark.

## Staged plan (the M4 native fan-out)

1. **[LANDED] Closure conversion + closure-IR + semantics-preservation tests**
   (`codegen/closure.go`, `codegen/closure_*_test.go`). This doc.
2. **[NEXT, ready-to-build] The minimal native runtime in C** — `value`, `alloc`,
   `force`, the `apply` helper, `$show` over the header tags, and the nat
   primitives, as a small C shim (the analog of the source backends' runtime
   blobs). A **conservative** semispace collector (scan-the-stack roots) for the
   first cut. This is the realization of ABI piece (3) at its simplest correct
   point.
3. **[NEXT] The first native emitter — Cranelift OR LLVM** over `ClosureProgram`:
   each `CodeBlock` → a function `code(arg, env)`; `MkClosure`/`ICon` → `alloc`;
   `AppClosure` → unpack + indirect call; `CCase`/`CField` → header-tag switch +
   field load; `CLit` → immediate/box; `CGlobal`/`CForeign` → symbol/extern.
   Cranelift is the lighter bring-up (no LLVM toolchain dependency, a Go-friendly
   JIT/AOT story); LLVM gives the optimizing AOT path. Gate: the cross-backend
   conformance corpus (`harness/backend_conformance_test.go`) produces
   byte-identical `$show` output on the native backend as on js/py/go/rust/jvm/beam.
4. **[LATER] Precise GC** (stack maps) + the **eval/apply saturation** speed pass
   (saturated multi-arg calls, PAPs only on under-application), both under
   benchmark.
5. **[LATER] The second native backend** (whichever of LLVM/Cranelift was not the
   first, + WASM-machine) — a port off the same `ClosureProgram`, not a rewrite.

---

**Status: closure conversion ready-to-build and LANDED** (the pass, the
closure-IR, free-var + closed-block + semantics-preservation tests, all green, no
hash bump, opt-in). **Research-deferred (clearly labelled):** the GC'd closure
runtime (semispace → precise) and the first native emitter (Cranelift/LLVM) over
the converted IR. The containment line holds: all shadow, no core growth.

Sources:
[Appel, *Compiling with Continuations*] ·
[Minamide–Morrisett–Harper, *Typed Closure Conversion*, POPL'96] ·
[Marlow & Peyton Jones, *Making a fast curry* (eval/apply)] ·
[GHC STG closures] · [Cranelift / Wasmtime closure lowering] ·
[Cheney semispace GC] · [Perceus refcounting (Koka/Lean)]
```
