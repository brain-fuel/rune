# English Explainer: Code-to-English Views for Non-Programmers

Date: 2026-07-03
Status: approved (design), awaiting implementation plan
Sub-project B of the UX-simplification pair (A is script ergonomics, see
`2026-07-03-script-ergonomics-design.md`). Builds after A ships.

## Problem

Rune programs are opaque to readers who cannot code. `bindIO Float Float
getFloat (fn (x : Float) is ...)` scares off exactly the people who could
otherwise follow "get a number, double it, print it". The goal is a
deterministic, compiler-derived English rendering of a program, at
configurable depth, so a non-programmer can read what a program does without
learning the syntax first.

Target output for the double demo:

```
$ rune explain double.rune main

[Entrypoint: main]
[Get Float `x` from Command Line]
[Apply Function (fmul x (fromNat 2))]
[Print Result to Command Line]
```

## Design

### 1. Core renderer: `internal/explain`

One renderer, three thin frontends. Input is the resolved surface AST of a
target definition plus the elaborator session, queried for types on demand
(hybrid: surface tree for structure so output matches what the reader sees on
screen, elaborator for type facts when a template wants them). Output is a
tree of English step nodes, not flat text, so every frontend and every depth
renders from the same structure.

### 2. Template registry

Known forms render to English:

- `bindIO A B m (fn (x : A) is k end)`: a step for `m` phrased with "as `x`",
  then the steps of `k`. Chains flatten to a straight step list; the word
  bindIO never appears at depth 0.
- Host ops carry their own phrasing, registered next to the prim itself in
  `codegen/ioprims.go` (or a sibling table keyed by prim name):
  `getFloat` renders "Get Float `x` from Command Line", `printFloat e`
  renders "Print (e) to Command Line", `readFileCode p` renders
  "Read File (p)", and so on for every prim.
- `case` renders "When (scrutinee) is (pattern): ..." per branch.
- `seq` and `do` blocks render as "Then"-lists.
- `fn` renders "Given (x), ...".
- Fallback for anything unknown is the bracket form with the pretty-printed
  expression inside: `[Apply Function (fmul x (fromNat 2))]`.

### 3. Depth dial

`--depth` controls how much the reader is asked to hold at once:

- `0` (default): the target definition's top-level steps only. Calls to other
  definitions or packages are one line each, named and typed via the
  elaborator: "Apply `Geometry.area` (takes two Floats, gives Float)".
- `1..n`: inline the bodies of called user functions n levels deep, indented
  under their call step.
- `core`: the elaborated core walk. Implicits visible, sugar expanded, the
  nothing-hidden view.

Same step tree throughout; depth only controls expansion.

### 4. Frontends

1. `rune explain <files...> <name> [--depth n|core] [--annotate]`: plain step
   list to stdout. Accepts the same compilation-set arguments as `rune run`.
2. REPL `:explain $N` and `:explain name`: explains a numbered history
   result's expression or a definition, same flags. Rides the existing
   jshell-style `$N` result mechanism.
3. Annotated view (`--annotate`): terminal at or above a width threshold gets
   two columns, code left, English right, aligned by line. Narrower terminals
   get interleaved comment lines (`--` prefixed) above each expression.

## Acceptance

1. `rune explain double.rune main` emits the four-line English view above at
   depth 0.
2. Golden files for `--depth 0`, `--depth 1`, and `--core` over 3 to 5
   representative listings; explain output is deterministic, goldens lock the
   phrasing.
3. `:explain $N` works in the REPL with a repl test (standing acceptance
   rule).
4. `--annotate` goldens for both layouts (wide two-column, narrow
   interleaved).
5. A test enumerates every prim in `ioprims.go` and fails if any lacks an
   English template, so future host ops cannot ship unexplained.

## Non-goals

- No LLM involvement: output is deterministic and golden-testable.
- No natural-language input (explaining is one-way, English out only).
- No editor/LSP integration in this project.

## Risks

- Phrasing churn: goldens make every wording change a visible diff, which is
  the point, but batch phrasing edits to avoid golden thrash.
- Depth-n inlining across module boundaries pulls in prelude bodies; depth
  levels above 0 should still summarize prims and `builtin`-accelerated
  definitions rather than showing fuel loops.
- Two-column layout with long expressions needs a wrap rule; goldens fix it
  early.
