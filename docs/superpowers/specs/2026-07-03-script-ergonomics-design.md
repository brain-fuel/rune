# Script Ergonomics: Compilation Sets, Imports, Prelude, Float IO

Date: 2026-07-03
Status: approved (design), awaiting implementation plan
Sub-project A of the UX-simplification pair (B is the English explainer, see
`2026-07-03-explainer-design.md`).

## Problem

"Read a float from the command line and double it" takes roughly 200 lines of
Rune today. Five compounding causes:

1. No cross-file mechanism: every compiled file is self-contained, so each
   program re-declares Nat, Bool, add/mul/monus, div/mod before doing anything
   (ch213 spends about 100 lines on this before parsing starts).
2. The 1266-line prelude is REPL-only; `rune run file.rune` gets nothing.
3. No float parse/print at the IO boundary: `getNat` exists, `getFloat`,
   `parseFloat`, and `printFloat` do not. Reading "3.14" means `readLineCode`
   into packed bytes and a hand-rolled decimal parser.
4. `module M is ... end` blocks (C6) exist but allow only plain definitions:
   no `data`, `builtin`, or `foreign` inside.
5. Fully qualified names are the only cross-namespace reference form; there is
   no `import` or `alias`.

Python equivalent: `print(float(input()) * 2)`.

## Goal

The double demo, on any backend, in about 7 lines, with `bindIO` kept
explicit (deliberate: the explainer sub-project makes `bindIO` legible to
non-programmers rather than sugar hiding it):

```
import Std.Float

main : IO Float is
  bindIO Float Float getFloat
    (fn (x : Float) is printFloat (fmul x (fromNat 2)) end)
end
```

## Non-goals

- No do-notation for monadic bind (`do` is taken by the `par` desugaring;
  `bindIO` stays visible by decision).
- No kernel changes. Everything here is surface, elaborator, session, host-op,
  or CLI level. The kernel stays frozen.
- No `Std.*` namespacing of the existing prelude contents in this project.
  The prelude joins compilation sets as-is; splitting it into `Std` modules is
  a follow-up refactor (pre-1.0 freedom applies, it should stay easy).
- No visibility/privacy (`defp`-style) yet. All names public.
- No package registry, versioning, or remote fetch. Local files only.

## Design

### 1. Compilation set (mix-style, name/path decoupled)

`rune run <path...> <name>` and `rune emit <path...> [name]` accept any mix of
files and directories alongside the existing single-file form, which keeps
working unchanged.

- A directory argument means its `*.rune` files, non-recursive.
- All files parse into one session. Files are topologically sorted by
  cross-file name references (a reference to `Math.tau` orders the file
  defining `Math.tau` first). A reference cycle between files is an error
  naming the cycle path.
- Module identity comes from `module` blocks, never from file paths or file
  names, exactly the Elixir/mix model.
- A `rune.toml` manifest listing the set is a later addition; argument lists
  ship first.

### 2. `import` and `alias` (contextual keywords, pure surface rewrites)

Modules are already sugar over qualified global names, so both directives are
surface-level name rewrites with no new core:

- `import Math.Geometry` makes the module's names usable unqualified. An
  unqualified name matching two imported modules (or an import and a local
  definition) is an elaboration error listing the candidates.
- `alias Math.Geometry` makes the module usable as `Geometry.x`.
  `alias Math.Geometry as G` makes it usable as `G.x`.
- Both are contextual keywords, not reserved words. They are recognized only
  at top-level item position, where `ident ident` cannot begin a definition
  (definitions are always `name : Type is`). Existing listings using `import`
  or `alias` as ordinary identifiers keep parsing. Full `go test ./...`
  before any tag, per the standing surface-keyword rule.

### 3. Datatypes inside modules

Lift the C6 defs-only restriction: `data`, `builtin`, and `foreign` become
legal inside `module ... end`. Constructor names are qualified with the module
prefix like definition names. Case patterns resolve constructors through
`import`/`alias` the same way expression references do. This is the riskiest
parser/resolver item in the project (new resolution path for qualified
constructors in patterns) and gets its own increment.

### 4. Prelude for compiled files, always on

The REPL prelude source joins every compilation set implicitly, loaded first,
through the same pipeline as everything else (it keeps its no-special-status
property). `--no-prelude` opts out.

Existing listings redeclare prelude names and would collide; the listings test
harness passes `--no-prelude` (or the equivalent session flag) for listings,
so no listing changes.

### 5. Tree-shaking (dead-code elimination at emit)

`EmitProgram` currently emits every definition in the session. Add a
reachability pass: starting from `Main`, walk free references through
`DefSpec` bodies and keep only reachable definitions. Backend-independent,
one pass at the session/codegen boundary, all 9 backends inherit it. This is
what makes the always-on prelude free: the emitted double demo contains none
of the 1266 prelude lines it does not use.

### 6. Float IO host ops

New host ops in `codegen/ioprims.go`, implemented on all 9 backends
(js, py, go, rs, erl, jvm, c, ll, wasm) and in the REPL:

- `parseFloat : Nat -> Option Float`. Packed-string bytes to float. Returns
  `none` on garbage.
- `getFloat : IO Float`. Read a line from stdin and parse it. Garbage parses
  to 0.0, mirroring the total contract style of `getNat`.
- `printFloat : Float -> IO Float`. Print the float and return it, matching
  the `printNat` shape.

`printFloat` formatting is canonical across backends: shortest round-trip
decimal. JS `Number` printing, Python `repr`, and Go
`strconv.FormatFloat(x, 'g', -1, 64)` already agree on shortest round-trip
digits; exponent spelling and the remaining backends (Erlang, C, LLVM, JVM,
Rust, WASM) need shims to the one canonical spelling. The conformance test
joins the divergence-lock family (same pattern as the bible op family):
byte-identical output across all 9 backends over a fixed input corpus
including exponent-range, negative, integral-valued, and garbage-input cases.

The prelude adds a `Num Float` instance (via existing `fadd`/`fmul`) so `*`
works on floats in scripts that want it; the demo uses explicit `fmul`.

## Acceptance

1. The double demo above runs on all 9 backends, fed "3.14" on stdin,
   printing "6.28" byte-identically.
2. `printFloat` conformance test in the divergence-lock family passes 9-way.
3. The demo works in `rune repl` and a repl test covers it (standing
   acceptance rule: not done until it works in the REPL).
4. Full `go test ./...` green; no existing listing broken by the contextual
   keywords or the always-on prelude.
5. Tree-shake verified: a test asserts a known-unused prelude symbol is
   absent from the emitted output of the demo.
6. Errors are Diagnostic-grade: unknown module on import, ambiguous
   unqualified name (listing candidates), import cycle (naming the path),
   parse errors for malformed `import`/`alias` with carets.

## Sequencing (small green increments, each tagged)

1. Float IO host ops + conformance family + REPL wiring.
2. Compilation set: multi-file/dir arguments, topo-sort, cycle errors.
3. `import` / `alias` contextual keywords and resolution.
4. Always-on prelude + tree-shaking pass.
5. Datatypes inside modules (qualified constructors in patterns).

The explainer sub-project follows after all five.

## Risks

- Canonical float formatting on C/LLVM/Erlang: shim work, known pattern from
  the bible host-op family, but shortest-round-trip in C is nontrivial
  (acceptable fallback: fixed-precision canonical form chosen once, applied
  everywhere, as long as it is byte-identical 9-way and round-trips).
- Prelude name collisions with existing listings: mitigated by harness
  `--no-prelude`.
- Qualified constructors in case patterns: new resolution path, staged last.
- Multiple agents may work in this repo concurrently: implementation should
  run in its own worktree, or commit with explicit pathspecs.
