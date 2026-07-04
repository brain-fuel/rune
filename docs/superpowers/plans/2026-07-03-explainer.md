# English Explainer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `rune explain`, REPL `:explain`, and `--annotate` render a checked definition as a deterministic tree of English steps at a configurable depth (0 | n | core), so a non-programmer can read what a program does.

**Architecture:** One renderer, three thin frontends. A new package `internal/explain` builds a `Step` tree from the retained (import-qualified) SURFACE definition, querying the elaborator session for type facts; the CLI subcommand, the REPL command, and the annotate layouts only format that tree. The session gains surface-Def retention (`surfaceDefs` + `SurfaceDef`), the REPL gains `historyExps` for `$N`, and codegen gains exported `IOPrimNames()` so a coverage test can force an English template for every host-op prim.

**Tech Stack:** Go 1.24, stdlib only. Spec: `docs/superpowers/specs/2026-07-03-explainer-design.md` (normative, including the four-line double output). Research record: the explain-research scratchpad (binding controller decisions are folded into this plan).

## Global Constraints

- **Kernel frozen:** `core/`, `equality/`, `store/` are untouched. `elaborate/` and `surface/` are also untouched by this plan (the explainer carries its own printers).
- **Deterministic, no LLM:** output is compiler-derived and golden-testable; goldens lock the phrasing. Batch phrasing edits to avoid golden thrash.
- **No em or en dashes ANYWHERE** (user ban): not in prose, comments, English templates, goldens, README, or commit messages. ASCII hyphen `-` only. The template coverage test enforces this for templates.
- **No new module dependencies.** DECISION: `golang.org/x/term` is NOT added (it is absent from go.mod, and the repo records every dependency with a rationale). Width comes from a `--width <n>` flag, default 80, no TTY probing. Runtime TTY width detection goes to PARKING-LOT.md (Task 8).
- **Conventional Commits, explicit pathspecs** on every `git add` (shared-checkout discipline).
- **Per-task tests are scoped to the touched packages.** NEVER run `go test ./...` mid-plan: the harness package alone takes ~16 minutes. Scoped commands are given per task.
- **REPL acceptance rule:** a REPL feature is not done without REPL wiring plus a repl test (Task 9).
- `listings/` and `examples/` are read-only inputs (explain targets); do not edit them.
- Test style follows the repo: plain string comparison against `testdata/*.golden` files (no `-update` flag), script -> `RunWith` -> substring asserts for the REPL.

### Resolved spec ambiguities (recorded so reviewers do not re-litigate)

1. **"Every prim in ioprims.go"** (spec acceptance 5) is read as ALL prim names declared in `codegen/ioprims.go`: the `ioPrims` map (45 names) PLUS the auxiliary families `dataPlanePrims` (8), `binPrims` (5), `netPrims` (6), `fsPrims` (5), and the singletons `procRun`, `sha256`, `tlsGet` (3). Total 72. `fileEnvPrims`/`streamPrims` are subsets of `ioPrims` and deduplicate away.
2. **Display names are last dot-segments** (codegen's `primName` rule): the spec's normative double output shows `fmul`, `getFloat`, not `Std.Float.fmul`. All explainer output shortens qualified names to the last segment.
3. **The "[Apply Function (fmul x (fromNat 2))]" line** in the normative output is the bracket-code form of a COMPOUND ARGUMENT to a prim step, not a recursive template expansion: templates apply at step positions (IO actions, calls, case, let); a pure argument expression stays in code form, preceded by one `Apply Function (...)` step, and the prim's slot reads `Result`.
4. **Root label** is `[Entrypoint: <name>]` for every named target (the spec pins it only for `main`; one phrasing is kept). A REPL `$N` expression gets root `[Expression]`.
5. **Core view** flattens `bindIO` chains too (spec: "Same step tree throughout; depth only controls expansion") but applies NO English templates; step bodies are raw core printed with implicit applications visible in braces, underscore binders shown, numerals as literals. bindIO's bound-value type is shown after the binder.
6. **REPL `:explain` supports the same flags** (`--depth n|core`, `--core`, `--annotate`, `--width n`), per spec frontend 2.
7. **A user definition whose short name collides with a prim template** renders through the template (same identity rule codegen uses; `CheckPrimCollisions` already polices real collisions at emit time).

## File structure

```
codegen/ioprims.go                 MODIFY  add IOPrimNames() (exported)
codegen/ioprims_names_test.go      CREATE  Task 1
internal/session/session.go        MODIFY  surfaceDefs field + retention + SurfaceDef/Accelerated
internal/session/surfacedef_test.go CREATE Task 2
internal/explain/explain.go        CREATE  Step, Options, Explain, ExplainExp, ctx/steps, templates hooks
internal/explain/print.go          CREATE  short, printExp (one-line surface printer)
internal/explain/coreprint.go      CREATE  printCore (open-term core printer)
internal/explain/templates.go      CREATE  primTemplate + table (seed Task 3, full Task 4)
internal/explain/render.go         CREATE  RenderText
internal/explain/coresteps.go      CREATE  Task 6 (depth=core walk)
internal/explain/annotate.go       CREATE  Task 8 (RenderAnnotate wide/narrow)
internal/explain/*_test.go         CREATE  Tasks 3,4,5,6,8
cmd/rune/explain.go                CREATE  Task 7 (parseExplainArgs + runExplainCLI)
cmd/rune/main.go                   MODIFY  switch case + usage line + doc comment
cmd/rune/explain_test.go           CREATE  Task 7 golden tests
cmd/rune/testdata/explain/*.golden CREATE  Tasks 7, 8
internal/repl/repl.go              MODIFY  historyExps + runExpr recording + :explain case + :help line
internal/repl/explain.go           CREATE  Task 9 (runExplain)
internal/repl/repl_explain_test.go CREATE  Task 9
README.md                          MODIFY  Task 10
CLAUDE.md                          MODIFY  Task 10
PARKING-LOT.md                     MODIFY  Task 8 (x/term TTY note)
```

---

### Task 1: codegen.IOPrimNames()

**Files:**
- Modify: `codegen/ioprims.go` (add `sort` import; add one function at the end of the file)
- Test: `codegen/ioprims_names_test.go` (new)

**Interfaces:**
- Produces: `func IOPrimNames() []string` in package `codegen` - sorted, deduplicated names of every prim declared in ioprims.go (72 names today). Task 4's coverage test consumes it.

- [ ] **Step 1: Write the failing test**

Create `codegen/ioprims_names_test.go`:

```go
package codegen

import (
	"sort"
	"testing"
)

// TestIOPrimNames pins the exported prim enumeration: it must cover every
// prim family declared in ioprims.go, sorted and without duplicates, so the
// explainer's template-coverage gate can rely on it.
func TestIOPrimNames(t *testing.T) {
	names := IOPrimNames()
	if !sort.StringsAreSorted(names) {
		t.Errorf("IOPrimNames is not sorted: %v", names)
	}
	seen := map[string]bool{}
	for _, n := range names {
		if seen[n] {
			t.Errorf("IOPrimNames contains duplicate %q", n)
		}
		seen[n] = true
	}
	for n := range ioPrims {
		if !seen[n] {
			t.Errorf("ioPrims key %q missing from IOPrimNames", n)
		}
	}
	fams := [][]string{
		dataPlanePrims, binPrims, netPrims, fsPrims,
		{"procRun", "sha256", "tlsGet"},
	}
	for _, fam := range fams {
		for _, n := range fam {
			if !seen[n] {
				t.Errorf("family prim %q missing from IOPrimNames", n)
			}
		}
	}
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `go test ./codegen -run TestIOPrimNames`
Expected: FAIL to compile with `undefined: IOPrimNames`

- [ ] **Step 3: Implement IOPrimNames**

In `codegen/ioprims.go`, change the import block to:

```go
import (
	"fmt"
	"sort"
	"strings"
)
```

Append at the end of the file:

```go
// IOPrimNames returns every host-op prim name declared in this file, sorted
// and deduplicated: the ioPrims map plus the auxiliary prim families
// (data-plane, Bin, net, fs) and the singleton prims procRun, sha256, tlsGet.
// The explainer's template-coverage gate (internal/explain) enumerates this
// list and fails if any prim lacks an English template, so a future host op
// cannot ship unexplained.
func IOPrimNames() []string {
	seen := map[string]bool{}
	for n := range ioPrims {
		seen[n] = true
	}
	fams := [][]string{fileEnvPrims, streamPrims, dataPlanePrims, binPrims, netPrims, fsPrims}
	for _, fam := range fams {
		for _, n := range fam {
			seen[n] = true
		}
	}
	for _, n := range []string{"procRun", "sha256", "tlsGet"} {
		seen[n] = true
	}
	names := make([]string, 0, len(seen))
	for n := range seen {
		names = append(names, n)
	}
	sort.Strings(names)
	return names
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `go test ./codegen -run TestIOPrimNames`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git -C /home/brainfuel/matt/goforge.dev/rune add codegen/ioprims.go codegen/ioprims_names_test.go
git -C /home/brainfuel/matt/goforge.dev/rune commit -m "feat(codegen): export IOPrimNames for the explainer coverage gate"
```

---

### Task 2: Session surface-Def retention

**Files:**
- Modify: `internal/session/session.go` (struct field ~line 86, `resetBuiltins` ~line 159, `AddDef` ~line 384, `addPartialDef` ~line 446, `AddDefGroup` before its final return)
- Test: `internal/session/surfacedef_test.go` (new)

**Interfaces:**
- Produces: `func (s *Session) SurfaceDef(name string) (surface.Def, bool)` - the retained, import-qualified surface definition (only defs WITH a body are retained; foreign axioms are not).
- Produces: `func (s *Session) Accelerated(name string) bool` - true when the name is registered as a kernel-accelerated builtin arithmetic op (`builtin natAdd add` etc.). The explainer keeps such defs one-line at every depth.
- Note: retention lives in `AddDef`/`addPartialDef`/`AddDefGroup`, which is exactly "LoadSource post-qualification" (LoadSource hands AddDef the def AFTER `sc.rewriteDef` import qualification) AND covers `LoadSet` and REPL `addItem` for free.

- [ ] **Step 1: Write the failing test**

Create `internal/session/surfacedef_test.go`:

```go
package session

import (
	"testing"

	"goforge.dev/rune/v3/surface"
)

// TestSurfaceDefRetention: LoadSource retains the QUALIFIED surface def, so a
// body reference rewritten by import resolution is visible to the explainer.
func TestSurfaceDefRetention(t *testing.T) {
	s := New()
	src := "module M is\n  t : U1 is U end\nend\nimport M\nu : U1 is t end\n"
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("LoadSource: %v", err)
	}
	d, ok := s.SurfaceDef("u")
	if !ok {
		t.Fatal("SurfaceDef(u): not retained")
	}
	v, ok := d.Body.(surface.EVar)
	if !ok {
		t.Fatalf("u body = %T, want surface.EVar", d.Body)
	}
	if v.Name != "M.t" {
		t.Errorf("u body name = %q, want import-qualified \"M.t\"", v.Name)
	}
	if _, ok := s.SurfaceDef("M.t"); !ok {
		t.Error("SurfaceDef(M.t): module-qualified def not retained")
	}
	if _, ok := s.SurfaceDef("nosuch"); ok {
		t.Error("SurfaceDef(nosuch): want ok=false")
	}
}

// TestSurfaceDefResetClears: Reset drops retained surface defs with the rest
// of the session.
func TestSurfaceDefResetClears(t *testing.T) {
	s := New()
	if _, err := s.LoadSource("w : U1 is U end\n"); err != nil {
		t.Fatalf("LoadSource: %v", err)
	}
	if _, ok := s.SurfaceDef("w"); !ok {
		t.Fatal("SurfaceDef(w): not retained before Reset")
	}
	s.Reset()
	if _, ok := s.SurfaceDef("w"); ok {
		t.Error("SurfaceDef(w): still retained after Reset")
	}
}

// TestAccelerated: a `builtin natAdd` registration is visible by name, so the
// explainer can keep accelerated defs one-line instead of showing fuel loops.
func TestAccelerated(t *testing.T) {
	s := New()
	src := "data Nat : U is zero : Nat | succ : Nat -> Nat end\n" +
		"builtin nat Nat zero succ\n" +
		"addN : Nat -> Nat -> Nat is\n" +
		"  fn (a : Nat) (b : Nat) is NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a end\n" +
		"end\n" +
		"builtin natAdd addN\n"
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("LoadSource: %v", err)
	}
	if !s.Accelerated("addN") {
		t.Error("Accelerated(addN) = false, want true")
	}
	if s.Accelerated("zero") {
		t.Error("Accelerated(zero) = true, want false")
	}
	if s.Accelerated("nosuch") {
		t.Error("Accelerated(nosuch) = true, want false")
	}
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `go test ./internal/session -run 'TestSurfaceDef|TestAccelerated'`
Expected: FAIL to compile with `s.SurfaceDef undefined` / `s.Accelerated undefined`

- [ ] **Step 3: Implement retention**

In `internal/session/session.go`:

(a) Add the field to the `Session` struct, after the `meta map[string]defMeta` field (~line 86):

```go
	// surfaceDefs retains the import-qualified SURFACE definition of every
	// loaded def that has a body (plain, instance, partial; foreign axioms
	// are bodiless and not retained). The explainer (internal/explain) reads
	// program STRUCTURE from here so its output matches what the reader sees
	// on screen; the core stays the authority on types. Latest-wins on
	// redefinition, exactly like refs.
	surfaceDefs map[string]surface.Def
```

(b) In `resetBuiltins`, after `s.meta = map[string]defMeta{}` (~line 159), add:

```go
	s.surfaceDefs = map[string]surface.Def{}
```

(c) In `AddDef`, in the normal path, immediately after `s.byHash[h] = rd` (~line 384), add:

```go
	s.surfaceDefs[d.Name] = d
```

(d) In `addPartialDef`, immediately after its `s.byHash[h] = rd` (~line 446), add:

```go
	s.surfaceDefs[d.Name] = d
```

(e) In `AddDefGroup`, immediately before its successful `return names, nil`, add:

```go
	for _, m := range g.Members {
		s.surfaceDefs[m.Name] = m
	}
```

(f) Add the accessors after the `Lookup` method (~line 1141):

```go
// SurfaceDef returns the retained, import-qualified surface definition of
// name. Only definitions with a body are retained; foreign axioms, builtin
// group members, and data constructors return ok=false.
func (s *Session) SurfaceDef(name string) (surface.Def, bool) {
	d, ok := s.surfaceDefs[name]
	return d, ok
}

// Accelerated reports whether name is registered as a kernel-accelerated
// builtin arithmetic op (builtin natAdd/natMul/natMonus/natDiv/natMod). The
// explainer keeps such definitions one-line at every inline depth: their
// eliminator bodies are an implementation detail, not the meaning.
func (s *Session) Accelerated(name string) bool {
	h, ok := s.refs[name]
	if !ok {
		return false
	}
	return s.natAccel[h] != core.NatOpNone
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `go test ./internal/session -run 'TestSurfaceDef|TestAccelerated'`
Expected: PASS

- [ ] **Step 5: Run the whole session package (guard against regressions)**

Run: `go test ./internal/session`
Expected: PASS (this package is fast; it is not the harness)

- [ ] **Step 6: Commit**

```bash
git -C /home/brainfuel/matt/goforge.dev/rune add internal/session/session.go internal/session/surfacedef_test.go
git -C /home/brainfuel/matt/goforge.dev/rune commit -m "feat(session): retain qualified surface defs (SurfaceDef) and expose Accelerated"
```

---

### Task 3: internal/explain core - Step tree, printers, depth-0 rendering

**Files:**
- Create: `internal/explain/explain.go`
- Create: `internal/explain/print.go`
- Create: `internal/explain/coreprint.go`
- Create: `internal/explain/templates.go` (seed table; Task 4 completes it)
- Create: `internal/explain/render.go`
- Test: `internal/explain/explain_test.go`

**Interfaces:**
- Consumes: `session.SurfaceDef`, `session.Defs()`, `session.RefNames()`, `session.LoadSource` (Task 2); `surface.Exp` node set; `core.Tm` node set.
- Produces (used by Tasks 5-9):
  - `type Step struct { Text, Code string; Kids []Step }` (Text WITHOUT brackets; renderers add `[...]`)
  - `type Options struct { Depth int }` (Task 6 adds `Core bool`)
  - `func Explain(s *session.Session, name string, opts Options) (Step, error)`
  - `func ExplainExp(s *session.Session, e surface.Exp, opts Options) (Step, error)`
  - `func RenderText(root Step) string`
  - internal: `printExp(e surface.Exp) string`, `printCore(t core.Tm, env []string, refNames map[core.Hash]string) string`, `short(name string) string`, `typeSummary(s *session.Session, ty core.Tm) string`, `primTemplates map[string]primTemplate`

**Rendering rules (NORMATIVE for every golden in this plan):**
1. Root step: `Entrypoint: <short name>`, Code `<short name> : <printCore of core type>`. Expression root: `Expression`.
2. `RenderText` prints `[Text]` per step; root and its direct Kids print flush left; each level below indents 2 more spaces (indent = `2*(depth-1)` for depth >= 2, where root is depth 0). Output ends with a newline.
3. `bindIO A B m k` (4 explicit args, head short-name `bindIO`) flattens: steps of `m` with `k`'s lambda parameter as binder, then steps of the lambda body as SIBLINGS. The word bindIO never appears. Binder names starting with `_` are treated as unnamed. If `k` is not a lambda: steps of `m` unbound, then step `Pass the Result to (<printExp k>)`. If the CHAIN itself has a binder (a let-bound chain), it nests under one step `Compute ` + backtick-name + ` by:`.
4. Prim step (head short-name in `primTemplates`, enough explicit args): the template consumes the TRAILING `args` explicit arguments (leading explicit args are type parameters). Atomic argument -> slot `(<printExp arg>)`; compound argument -> a PRECEDING step `Apply Function (<printExp arg>)` and slot `Result`. Binder: `bound` template when `args == 0` and `bound != ""`, else append `` as `x` ``.
5. User-definition call (head EVar resolvable via `Defs()`): one line `` Apply `name` `` + (if args) `` to (a1) and (a2) `` + `(<typeSummary>)` + optional `` as `x` ``. Depth 0: no Kids (Task 5 adds inlining).
6. `case`: one step per clause: `When (<printExp scrut>) is (<ctor b1 b2>):` with the clause body's steps as Kids.
7. `fn`: step `` Given `x`: `` with the body's steps as Kids.
8. `let` / seq-binding: value steps with the binding name as binder, then body steps as siblings.
9. Fallback: an application renders `Apply Function (<printExp e>)`; anything else `Result: (<printExp e>)`; plus binder suffix.
10. `typeSummary`: peel explicit core Pis: no domains -> `gives T`; else `takes A, B and C, gives T` (joinAnd: one item bare, two `A and B`, three+ `A, B and C`). Implicit Pis are skipped.
11. `pureIO` is an ordinary template row (`Give Back %s`, args 1), not a special case.

- [ ] **Step 1: Write the failing tests**

Create `internal/explain/explain_test.go`:

```go
package explain

import (
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

// doubleSrc is a prelude-free replica of examples/double.rune: the spec's
// normative four-line target output is asserted against it exactly.
const doubleSrc = `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
foreign Float : U end
foreign fromNat : Nat -> Float end
foreign fmul : Float -> Float -> Float end
foreign getFloat : IO Float end
foreign printFloat : Float -> IO Float end
main : IO Float is
  bindIO Float Float getFloat
    (fn (x : Float) is printFloat (fmul x (fromNat 2)) end)
end
`

func load(t *testing.T, src string) *session.Session {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("LoadSource: %v", err)
	}
	return s
}

func explainText(t *testing.T, s *session.Session, name string, opts Options) string {
	t.Helper()
	root, err := Explain(s, name, opts)
	if err != nil {
		t.Fatalf("Explain(%s): %v", name, err)
	}
	return RenderText(root)
}

// TestExplainDoubleDepth0 is acceptance item 1: the spec's four-line English
// view of the double demo, exactly.
func TestExplainDoubleDepth0(t *testing.T) {
	s := load(t, doubleSrc)
	want := "[Entrypoint: main]\n" +
		"[Get Float `x` from Command Line]\n" +
		"[Apply Function (fmul x (fromNat 2))]\n" +
		"[Print Result to Command Line]\n"
	if got := explainText(t, s, "main", Options{}); got != want {
		t.Errorf("double depth 0:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainCaseAndPure: case clauses render as When steps with indented
// kids; pureIO renders Give Back; an atomic prim argument stays inline.
func TestExplainCaseAndPure(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Option : U -> U is none : (A : U) -> Option A | some : (A : U) -> A -> Option A end
foreign Float : U end
foreign parseFloat : Nat -> Option Float end
foreign printNat : Nat -> IO Nat end
probe : IO Nat is
  case parseFloat 302 of
  | none -> printNat 999
  | some x -> pureIO Nat zero
  end
end
`
	s := load(t, src)
	want := "[Entrypoint: probe]\n" +
		"[When (parseFloat 302) is (none):]\n" +
		"  [Print (999) to Command Line]\n" +
		"[When (parseFloat 302) is (some x):]\n" +
		"  [Give Back (zero)]\n"
	if got := explainText(t, s, "probe", Options{}); got != want {
		t.Errorf("case/pure:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainSeq: a seq binding names its value step and continues as
// siblings (the Then-list).
func TestExplainSeq(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
foreign printNat : Nat -> IO Nat end
m : IO Nat is
  seq
    let r = printNat 1;
    printNat r
  end
end
`
	s := load(t, src)
	want := "[Entrypoint: m]\n" +
		"[Print (1) to Command Line as `r`]\n" +
		"[Print (r) to Command Line]\n"
	if got := explainText(t, s, "m", Options{}); got != want {
		t.Errorf("seq:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainLambdaFallback: fn renders Given with kids; a bound variable
// leaf renders through the atom fallback.
func TestExplainLambdaFallback(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
idn : Nat -> Nat is fn (n : Nat) is n end end
`
	s := load(t, src)
	want := "[Entrypoint: idn]\n" +
		"[Given `n`:]\n" +
		"  [Result: (n)]\n"
	if got := explainText(t, s, "idn", Options{}); got != want {
		t.Errorf("lambda/fallback:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainExp: the REPL $N path renders a bare expression; a data
// constructor call is typed via the elaborator.
func TestExplainExp(t *testing.T) {
	s := load(t, "data Nat : U is zero : Nat | succ : Nat -> Nat end\n")
	e, err := s.ParseSrcExpr("succ zero")
	if err != nil {
		t.Fatalf("ParseSrcExpr: %v", err)
	}
	root, err := ExplainExp(s, e, Options{})
	if err != nil {
		t.Fatalf("ExplainExp: %v", err)
	}
	want := "[Expression]\n" +
		"[Apply `succ` to (zero) (takes Nat, gives Nat)]\n"
	if got := RenderText(root); got != want {
		t.Errorf("expression:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainErrors: unknown names and bodiless targets report cleanly.
func TestExplainErrors(t *testing.T) {
	s := load(t, "foreign F : U end\n")
	if _, err := Explain(s, "nosuch", Options{}); err == nil {
		t.Error("Explain(nosuch): want error")
	}
	if _, err := Explain(s, "F", Options{}); err == nil {
		t.Error("Explain(F): want error (foreign axiom has no surface body)")
	}
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `go test ./internal/explain`
Expected: FAIL (package does not exist yet / undefined symbols)

- [ ] **Step 3: Create the package**

Create `internal/explain/print.go`:

```go
package explain

import (
	"fmt"
	"strings"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/surface"
)

// short returns the last dot-segment of a possibly module-qualified name,
// the same display-identity rule as codegen's primName: after an import the
// reader sees the short name on screen, so the explainer shows it too.
func short(name string) string {
	if i := strings.LastIndex(name, "."); i >= 0 {
		return name[i+1:]
	}
	return name
}

// isAtomExp reports whether e prints as a single token (no parens needed).
func isAtomExp(e surface.Exp) bool {
	switch e.(type) {
	case surface.EVar, surface.ENum, surface.EUniv, surface.EProp, surface.EHole,
		surface.ESig, surface.EPair, surface.EEq, surface.ERefl, surface.ECast,
		surface.ESubst, surface.EFst, surface.ESnd:
		return true
	}
	return false
}

func atom(e surface.Exp) string {
	if isAtomExp(e) {
		return printExp(e)
	}
	return "(" + printExp(e) + ")"
}

// printExp renders a surface expression compactly on one line, close to what
// the user wrote: applications as spines, compound arguments parenthesized,
// names as their last dot-segment. Quantity annotations are omitted (display
// only). The switch is exhaustive over surface.Exp; an unknown node panics so
// an AST addition fails loudly in tests (the WalkExp discipline).
func printExp(e surface.Exp) string {
	switch x := e.(type) {
	case surface.EVar:
		return short(x.Name)
	case surface.ENum:
		return x.Val.String()
	case surface.EUniv:
		if x.Lvl == 0 {
			return "U"
		}
		return fmt.Sprintf("U%d", x.Lvl)
	case surface.EHole:
		return "_"
	case surface.EProp:
		return "Prop"
	case surface.ESig:
		return "Sig"
	case surface.EPair:
		return "pair"
	case surface.EEq:
		return "Eq"
	case surface.ERefl:
		return "refl"
	case surface.ECast:
		return "cast"
	case surface.ESubst:
		return "subst"
	case surface.EFst:
		return atom(x.P) + ".1"
	case surface.ESnd:
		return atom(x.P) + ".2"
	case surface.EApp:
		if x.Icit == core.Impl {
			return printExp(x.Fn) + " {" + printExp(x.Arg) + "}"
		}
		return printExp(x.Fn) + " " + atom(x.Arg)
	case surface.ELam:
		lp, rp := "(", ")"
		if x.Icit == core.Impl {
			lp, rp = "{", "}"
		}
		return "fn " + lp + x.Param + " : " + printExp(x.Dom) + rp + " is " + printExp(x.Body) + " end"
	case surface.EPi:
		if x.Param == "_" && x.Icit == core.Expl {
			return atom(x.Dom) + " -> " + printExp(x.Cod)
		}
		lp, rp := "(", ")"
		if x.Icit == core.Impl {
			lp, rp = "{", "}"
		}
		return lp + x.Param + " : " + printExp(x.Dom) + rp + " -> " + printExp(x.Cod)
	case surface.ELet:
		return "let " + x.Name + " = " + printExp(x.Val) + "; " + printExp(x.Body)
	case surface.ESeqBind:
		return "let " + x.Name + " = " + printExp(x.Val) + "; " + printExp(x.Body)
	case surface.EAnn:
		return "(" + printExp(x.Term) + " : " + printExp(x.Ty) + ")"
	case surface.ECase:
		var sb strings.Builder
		sb.WriteString("case " + printExp(x.Scrut) + " of")
		for _, cl := range x.Clauses {
			sb.WriteString(" | " + cl.Ctor)
			for _, b := range cl.Binders {
				sb.WriteString(" " + b)
			}
			if len(cl.IHs) > 0 {
				sb.WriteString(" with " + strings.Join(cl.IHs, " "))
			}
			sb.WriteString(" -> " + printExp(cl.Body))
		}
		sb.WriteString(" end")
		return sb.String()
	default:
		panic(fmt.Sprintf("explain.printExp: unknown surface.Exp %T; update print.go", e))
	}
}
```

Create `internal/explain/coreprint.go`:

```go
package explain

import (
	"fmt"

	"goforge.dev/rune/v3/core"
)

// printCore renders a possibly-OPEN core term compactly on one line, given
// the binder names in scope (env[len(env)-1] names Var{0}). surface.Pretty
// requires closed terms, so the explainer carries its own small open-term
// printer. Display only: binder hints are used as-is, without capture-
// avoidance freshening. Implicit applications print in braces (the depth=core
// "implicits visible" rule rides on this printer).
func printCore(t core.Tm, env []string, refNames map[core.Hash]string) string {
	switch x := t.(type) {
	case core.Var:
		i := len(env) - 1 - x.Idx
		if i < 0 || i >= len(env) {
			return fmt.Sprintf("?v%d", x.Idx)
		}
		return env[i]
	case core.Ref:
		if n, ok := refNames[x.Hash]; ok {
			return short(n)
		}
		return fmt.Sprintf("#%s", x.Hash)
	case core.Univ:
		if x.Lvl == 0 {
			return "U"
		}
		return fmt.Sprintf("U%d", x.Lvl)
	case core.Prop:
		return "Prop"
	case core.NatLit:
		return x.N.String()
	case core.Meta:
		return fmt.Sprintf("?m%d", x.ID)
	case core.App:
		arg := printCore(x.Arg, env, refNames)
		if x.Icit == core.Impl {
			return printCore(x.Fn, env, refNames) + " {" + arg + "}"
		}
		if !coreAtom(x.Arg) {
			arg = "(" + arg + ")"
		}
		return printCore(x.Fn, env, refNames) + " " + arg
	case core.Lam:
		n := scopeName(x.Body)
		lp, rp := "(", ")"
		if x.Icit == core.Impl {
			lp, rp = "{", "}"
		}
		return "fn " + lp + n + rp + " is " + printCore(x.Body.Body, append(env, n), refNames) + " end"
	case core.Pi:
		n := scopeName(x.Cod)
		dom := printCore(x.Dom, env, refNames)
		cod := printCore(x.Cod.Body, append(env, n), refNames)
		if n == "_" && x.Icit == core.Expl {
			if !coreAtom(x.Dom) {
				dom = "(" + dom + ")"
			}
			return dom + " -> " + cod
		}
		lp, rp := "(", ")"
		if x.Icit == core.Impl {
			lp, rp = "{", "}"
		}
		return lp + n + " : " + dom + rp + " -> " + cod
	case core.Let:
		n := scopeName(x.Body)
		return "let " + n + " = " + printCore(x.Val, env, refNames) + "; " +
			printCore(x.Body.Body, append(env, n), refNames)
	case core.Ann:
		return "(" + printCore(x.Term, env, refNames) + " : " + printCore(x.Ty, env, refNames) + ")"
	case core.Sig:
		n := scopeName(x.Cod)
		return "Sig (" + n + " : " + printCore(x.Dom, env, refNames) + ") " +
			printCore(x.Cod.Body, append(env, n), refNames)
	case core.Pair:
		return "pair " + coreAtomStr(x.A, env, refNames) + " " + coreAtomStr(x.B, env, refNames)
	case core.Fst:
		return coreAtomStr(x.P, env, refNames) + ".1"
	case core.Snd:
		return coreAtomStr(x.P, env, refNames) + ".2"
	case core.Eq:
		return "Eq " + coreAtomStr(x.Ty, env, refNames) + " " + coreAtomStr(x.L, env, refNames) +
			" " + coreAtomStr(x.R, env, refNames)
	case core.Refl:
		return "refl " + coreAtomStr(x.Tm, env, refNames)
	case core.Cast:
		return "cast " + coreAtomStr(x.A, env, refNames) + " " + coreAtomStr(x.B, env, refNames) +
			" " + coreAtomStr(x.P, env, refNames) + " " + coreAtomStr(x.X, env, refNames)
	case core.Subst:
		return "subst " + coreAtomStr(x.A, env, refNames) + " " + coreAtomStr(x.X, env, refNames) +
			" " + coreAtomStr(x.Y, env, refNames) + " " + coreAtomStr(x.Prf, env, refNames) +
			" " + coreAtomStr(x.P, env, refNames) + " " + coreAtomStr(x.Px, env, refNames)
	default:
		panic(fmt.Sprintf("explain.printCore: unknown core.Tm %T; update coreprint.go", t))
	}
}

func scopeName(sc core.Scope) string {
	if sc.Name == "" {
		return "_"
	}
	return sc.Name
}

func coreAtom(t core.Tm) bool {
	switch t.(type) {
	case core.Var, core.Ref, core.Univ, core.Prop, core.NatLit, core.Meta, core.Fst, core.Snd:
		return true
	}
	return false
}

func coreAtomStr(t core.Tm, env []string, refNames map[core.Hash]string) string {
	s := printCore(t, env, refNames)
	if coreAtom(t) {
		return s
	}
	return "(" + s + ")"
}
```

Create `internal/explain/render.go`:

```go
package explain

import "strings"

// RenderText flattens a Step tree to the bracketed step list: the root and
// its direct Kids print flush left (the spec's four-line double view), and
// each level below that indents two more spaces. Output ends with a newline.
func RenderText(root Step) string {
	var sb strings.Builder
	var walk func(st Step, depth int)
	walk = func(st Step, depth int) {
		ind := 0
		if depth > 1 {
			ind = (depth - 1) * 2
		}
		sb.WriteString(strings.Repeat(" ", ind))
		sb.WriteString("[" + st.Text + "]\n")
		for _, k := range st.Kids {
			walk(k, depth+1)
		}
	}
	walk(root, 0)
	return sb.String()
}
```

Create `internal/explain/templates.go` (seed; Task 4 replaces the map literal with the complete table):

```go
package explain

// primTemplate is the English rendering of one host-op prim.
//
// verb is a fmt template with one %s per consumed argument, in application
// order. args is the number of TRAILING explicit arguments the template
// consumes (leading explicit arguments are type parameters, e.g. foldLines'
// state type, and are not shown). bound, when non-empty and args == 0, is
// the fmt template used when a bindIO chain names the result (its one %s is
// the binder name); every other bound step appends " as `x`". Template text
// must never contain an em or en dash (ASCII hyphen only); the coverage test
// in templates_test.go enforces this.
type primTemplate struct {
	verb  string
	args  int
	bound string
}

// primTemplates maps a prim's LAST-DOT-SEGMENT name (codegen's primName rule)
// to its English template. Every name in codegen.IOPrimNames() must have an
// entry (Task 4's coverage gate). pureIO is listed too: it is a builtin, not
// an ioprim, but it renders as an ordinary payload step. bindIO is structural
// (the chain flattener) and never appears in output.
var primTemplates = map[string]primTemplate{
	"pureIO":     {verb: "Give Back %s", args: 1},
	"getFloat":   {verb: "Get Float from Command Line", args: 0, bound: "Get Float `%s` from Command Line"},
	"printFloat": {verb: "Print %s to Command Line", args: 1},
	"printNat":   {verb: "Print %s to Command Line", args: 1},
	"getNat":     {verb: "Get Number from Command Line", args: 0, bound: "Get Number `%s` from Command Line"},
	"parseFloat": {verb: "Parse %s as a Float", args: 1},
	"fromNat":    {verb: "Convert %s to Float", args: 1},
	"fmul":       {verb: "Multiply %s by %s", args: 2},
}
```

Create `internal/explain/explain.go`:

```go
// Package explain renders a checked definition (or a REPL expression) as a
// deterministic tree of English steps. One renderer, three thin frontends
// (the `rune explain` CLI, the REPL `:explain` command, and the --annotate
// views): each frontend only formats the Step tree this package builds.
//
// Structure comes from the retained, import-qualified SURFACE definition
// (session.SurfaceDef) so the output matches what the reader sees on screen;
// type facts come from the elaborated core (session.Defs). Depth controls
// expansion only: the same tree shape underlies every view. Output is
// compiler-derived and golden-tested; no LLM is involved.
package explain

import (
	"fmt"
	"strings"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/surface"
)

// Step is one English step. Text is the step's sentence WITHOUT the square
// brackets (renderers add them). Code is the source fragment the step came
// from (the --annotate views pair it with the English; empty for steps with
// no code counterpart). Kids are sub-steps: inlined callee bodies, case
// branches, lambda bodies.
type Step struct {
	Text string
	Code string
	Kids []Step
}

// Options selects the view depth.
type Options struct {
	// Depth is the user-definition inline budget: 0 (default) explains only
	// the target's top-level steps, with calls one line each; n > 0 inlines
	// called user definitions n levels deep. Prims, foreign axioms, and
	// builtin-accelerated definitions stay one-line at every depth.
	Depth int
}

// Explain renders the named definition from the session as a Step tree.
func Explain(s *session.Session, name string, opts Options) (Step, error) {
	defs := defMap(s)
	d, ok := defs[name]
	if !ok {
		return Step{}, fmt.Errorf("explain: no definition named %q", name)
	}
	root := Step{
		Text: "Entrypoint: " + short(name),
		Code: short(name) + " : " + printCore(d.Ty, nil, s.RefNames()),
	}
	sd, ok := s.SurfaceDef(name)
	if !ok {
		return Step{}, fmt.Errorf("explain: %q has no surface body to explain (a foreign axiom, builtin, or constructor)", name)
	}
	c := &ctx{s: s, defs: defs, depth: opts.Depth}
	root.Kids = c.steps(sd.Body, "")
	return root, nil
}

// ExplainExp renders a bare surface expression (the REPL $N path).
func ExplainExp(s *session.Session, e surface.Exp, opts Options) (Step, error) {
	c := &ctx{s: s, defs: defMap(s), depth: opts.Depth}
	return Step{Text: "Expression", Code: printExp(e), Kids: c.steps(e, "")}, nil
}

func defMap(s *session.Session) map[string]session.Def {
	m := make(map[string]session.Def)
	for _, d := range s.Defs() {
		m[d.Name] = d
	}
	return m
}

// ctx carries one Explain run: the session (types, retained surface defs),
// the name -> core def map, and the remaining inline budget.
type ctx struct {
	s     *session.Session
	defs  map[string]session.Def
	depth int
}

// steps renders expression e as a flat list of sibling steps. binder is the
// name the surrounding bindIO chain or let gives e's result; "" means
// unnamed, and names starting with "_" are treated as unnamed.
func (c *ctx) steps(e surface.Exp, binder string) []Step {
	if strings.HasPrefix(binder, "_") {
		binder = ""
	}
	head, args := spine(e)
	if v, ok := head.(surface.EVar); ok {
		n := short(v.Name)
		expl := explicitArgs(args)
		if n == "bindIO" && len(expl) == 4 {
			return c.bindSteps(e, expl, binder)
		}
		if t, ok := primTemplates[n]; ok && len(expl) >= t.args {
			return c.primSteps(t, e, expl, binder)
		}
		if _, ok := c.defs[v.Name]; ok {
			return []Step{c.callStep(v.Name, expl, e, binder)}
		}
	}
	switch x := e.(type) {
	case surface.ELam:
		return []Step{{
			Text: "Given `" + x.Param + "`:",
			Code: "fn (" + x.Param + " : " + printExp(x.Dom) + ")",
			Kids: c.steps(x.Body, ""),
		}}
	case surface.ECase:
		var out []Step
		for _, cl := range x.Clauses {
			pat := cl.Ctor
			if len(cl.Binders) > 0 {
				pat += " " + strings.Join(cl.Binders, " ")
			}
			out = append(out, Step{
				Text: "When (" + printExp(x.Scrut) + ") is (" + pat + "):",
				Code: "| " + pat + " -> " + printExp(cl.Body),
				Kids: c.steps(cl.Body, ""),
			})
		}
		return out
	case surface.ELet:
		return append(c.steps(x.Val, x.Name), c.steps(x.Body, binder)...)
	case surface.ESeqBind:
		return append(c.steps(x.Val, x.Name), c.steps(x.Body, binder)...)
	case surface.EAnn:
		return c.steps(x.Term, binder)
	}
	return []Step{fallbackStep(e, binder)}
}

// bindSteps flattens `bindIO A B m k`: m's step carries k's parameter as its
// binder, then k's body continues as SIBLING steps. The word bindIO never
// appears at any surface depth. A chain that is itself let-bound nests under
// one "Compute `x` by:" step so the name is not lost.
func (c *ctx) bindSteps(e surface.Exp, expl []surface.Exp, binder string) []Step {
	m, k := expl[2], expl[3]
	var out []Step
	if lam, ok := k.(surface.ELam); ok {
		out = append(c.steps(m, lam.Param), c.steps(lam.Body, "")...)
	} else {
		out = append(c.steps(m, ""), Step{
			Text: "Pass the Result to (" + printExp(k) + ")",
			Code: printExp(k),
		})
	}
	if binder != "" {
		return []Step{{Text: "Compute `" + binder + "` by:", Code: printExp(e), Kids: out}}
	}
	return out
}

// primSteps renders a host-op application through its English template
// (rendering rule 4: atomic argument inline, compound argument as a
// preceding Apply Function step whose slot reads Result).
func (c *ctx) primSteps(t primTemplate, e surface.Exp, expl []surface.Exp, binder string) []Step {
	take := expl[len(expl)-t.args:]
	var pre []Step
	slots := make([]any, 0, t.args)
	for _, a := range take {
		if isAtomExp(a) {
			slots = append(slots, "("+printExp(a)+")")
			continue
		}
		pre = append(pre, Step{Text: "Apply Function (" + printExp(a) + ")", Code: printExp(a)})
		slots = append(slots, "Result")
	}
	text := fmt.Sprintf(t.verb, slots...)
	if binder != "" {
		if t.bound != "" && t.args == 0 {
			text = fmt.Sprintf(t.bound, binder)
		} else {
			text += " as `" + binder + "`"
		}
	}
	return append(pre, Step{Text: text, Code: printExp(e)})
}

// callStep renders a call to a user definition (or a bodiless axiom without
// a template) as one line, named and typed via the elaborator. Task 5 adds
// depth-n inlining here.
func (c *ctx) callStep(full string, expl []surface.Exp, e surface.Exp, binder string) Step {
	d := c.defs[full]
	text := "Apply `" + short(full) + "`"
	if len(expl) > 0 {
		parts := make([]string, len(expl))
		for i, a := range expl {
			parts[i] = "(" + printExp(a) + ")"
		}
		text += " to " + strings.Join(parts, " and ")
	}
	text += " (" + typeSummary(c.s, d.Ty) + ")"
	if binder != "" {
		text += " as `" + binder + "`"
	}
	return Step{Text: text, Code: printExp(e)}
}

// fallbackStep is the bracket-code form for anything without a template: an
// application renders as Apply Function (...), an atom as Result: (...).
func fallbackStep(e surface.Exp, binder string) Step {
	text := "Result: (" + printExp(e) + ")"
	if _, ok := e.(surface.EApp); ok {
		text = "Apply Function (" + printExp(e) + ")"
	}
	if binder != "" {
		text += " as `" + binder + "`"
	}
	return Step{Text: text, Code: printExp(e)}
}

// spineArg is one application argument with its plicity.
type spineArg struct {
	e    surface.Exp
	icit core.Icit
}

// spine flattens a left-associated application into head + arguments.
func spine(e surface.Exp) (surface.Exp, []spineArg) {
	var args []spineArg
	for {
		a, ok := e.(surface.EApp)
		if !ok {
			break
		}
		args = append([]spineArg{{a.Arg, a.Icit}}, args...)
		e = a.Fn
	}
	return e, args
}

func explicitArgs(args []spineArg) []surface.Exp {
	var out []surface.Exp
	for _, a := range args {
		if a.icit == core.Expl {
			out = append(out, a.e)
		}
	}
	return out
}

// typeSummary phrases a definition's core type: "takes A and B, gives C".
// Explicit Pi domains only; implicit parameters are skipped. printCore
// carries the binder environment so dependent codomains print their names.
func typeSummary(s *session.Session, ty core.Tm) string {
	var doms []string
	var env []string
	rn := s.RefNames()
	for {
		pi, ok := ty.(core.Pi)
		if !ok {
			break
		}
		if pi.Icit == core.Expl {
			doms = append(doms, printCore(pi.Dom, env, rn))
		}
		env = append(env, scopeName(pi.Cod))
		ty = pi.Cod.Body
	}
	ret := printCore(ty, env, rn)
	if len(doms) == 0 {
		return "gives " + ret
	}
	return "takes " + joinAnd(doms) + ", gives " + ret
}

func joinAnd(xs []string) string {
	switch len(xs) {
	case 0:
		return ""
	case 1:
		return xs[0]
	}
	return strings.Join(xs[:len(xs)-1], ", ") + " and " + xs[len(xs)-1]
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `go test ./internal/explain`
Expected: PASS (all 6 tests)

- [ ] **Step 5: Vet and commit**

Run: `go vet ./internal/explain`
Expected: clean

```bash
git -C /home/brainfuel/matt/goforge.dev/rune add internal/explain
git -C /home/brainfuel/matt/goforge.dev/rune commit -m "feat(explain): step-tree renderer with English templates, depth-0 view"
```

---

### Task 4: Full prim template table + coverage gate

This is spec acceptance item 5: a test enumerates every prim in ioprims.go and fails if any lacks an English template, so future host ops cannot ship unexplained.

**Files:**
- Modify: `internal/explain/templates.go` (replace the seed map literal with the complete table below)
- Test: `internal/explain/templates_test.go` (new)

**Interfaces:**
- Consumes: `codegen.IOPrimNames()` (Task 1), `primTemplates` (Task 3).
- Produces: the complete `primTemplates` table; every later golden depends on this exact phrasing.

- [ ] **Step 1: Write the failing coverage test**

Create `internal/explain/templates_test.go`:

```go
package explain

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// TestPrimTemplateCoverage is the spec's acceptance gate: every prim declared
// in codegen/ioprims.go must carry an English template. It also pins template
// hygiene: slot counts match, bound templates are 0-arg with one slot, and no
// template contains an em or en dash (the ASCII-hyphen-only rule).
func TestPrimTemplateCoverage(t *testing.T) {
	for _, n := range codegen.IOPrimNames() {
		tpl, ok := primTemplates[n]
		if !ok {
			t.Errorf("prim %q has no English template; add a row to primTemplates", n)
			continue
		}
		if got := strings.Count(tpl.verb, "%s"); got != tpl.args {
			t.Errorf("prim %q: verb %q has %d slots, args = %d", n, tpl.verb, got, tpl.args)
		}
		if tpl.bound != "" {
			if tpl.args != 0 {
				t.Errorf("prim %q: bound template requires args == 0", n)
			}
			if strings.Count(tpl.bound, "%s") != 1 {
				t.Errorf("prim %q: bound template %q must have exactly one slot", n, tpl.bound)
			}
		}
		// U+2014 em dash, U+2013 en dash: written as escapes so neither
		// the test source nor the plan carries the characters literally.
		for _, bad := range []string{"\u2014", "\u2013"} {
			if strings.Contains(tpl.verb, bad) || strings.Contains(tpl.bound, bad) {
				t.Errorf("prim %q: template contains an em/en dash (ASCII hyphen only)", n)
			}
		}
	}
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `go test ./internal/explain -run TestPrimTemplateCoverage`
Expected: FAIL listing every prim missing from the seed table (64 of 72)

- [ ] **Step 3: Fill the complete table**

In `internal/explain/templates.go`, replace the entire `var primTemplates = ...` map literal with this complete table (the struct and comments above it stay). 73 rows: all 72 ioprims.go prims plus pureIO. Argument order follows each prim's documented signature in ioprims.go.

```go
var primTemplates = map[string]primTemplate{
	// Builtin IO group (store/io.go) - not an ioprim, but a step shape.
	"pureIO": {verb: "Give Back %s", args: 1},

	// D6 console / OS basics.
	"printNat":     {verb: "Print %s to Command Line", args: 1},
	"getNat":       {verb: "Get Number from Command Line", args: 0, bound: "Get Number `%s` from Command Line"},
	"timeNanos":    {verb: "Read the OS Clock in Nanoseconds", args: 0},
	"readLineCode": {verb: "Read a Line from Command Line", args: 0, bound: "Read a Line `%s` from Command Line"},

	// D6 stream / file-processing vocabulary.
	"foldLines":    {verb: "Process File %s Line by Line with Step %s from Start Value %s", args: 3},
	"foldDir":      {verb: "Process Directory %s (files ending %s) with Step %s from Start Value %s", args: 4},
	"splitOn":      {verb: "Split on Separator %s: %s", args: 2},
	"byteLen":      {verb: "Measure the Byte Length of %s", args: 1},
	"jsonStrField": {verb: "Extract JSON Field %s from %s", args: 2},
	"sqlQuote":     {verb: "SQL-Quote %s", args: 1},
	"openWrite":    {verb: "Open %s for Writing", args: 1},
	"writeChunk":   {verb: "Write to Handle %s the Line %s", args: 2},
	"closeWrite":   {verb: "Close Handle %s", args: 1},
	"sortFile":     {verb: "Sort the Lines of File %s into %s", args: 2},
	"dbApply":      {verb: "Run on Database %s the SQL Script %s", args: 2},

	// D6 env / file / argv / process.
	"getEnvCode":    {verb: "Get Environment Variable %s", args: 1},
	"readFileCode":  {verb: "Read File %s", args: 1},
	"writeFileCode": {verb: "Write to File %s the Content %s", args: 2},
	"printStrCode":  {verb: "Print Text %s to Command Line", args: 1},
	"argCountCode":  {verb: "Count the Command-Line Arguments", args: 0},
	"argAtCode":     {verb: "Get Command-Line Argument %s", args: 1},
	"exitWith":      {verb: "Exit the Program with Status %s", args: 1},

	// scribe rasterizer accel.
	"rasterFill": {verb: "Rasterize a %s by %s Alpha Mask from Stream %s", args: 3},

	// D3 machine floats (f64).
	"fromNat":    {verb: "Convert %s to Float", args: 1},
	"fadd":       {verb: "Add %s and %s", args: 2},
	"fsub":       {verb: "Subtract %s minus %s", args: 2},
	"fmul":       {verb: "Multiply %s by %s", args: 2},
	"fdiv":       {verb: "Divide %s by %s", args: 2},
	"fabsP":      {verb: "Take the Absolute Value of %s", args: 1},
	"floatToNat": {verb: "Truncate %s to a Whole Number", args: 1},
	"fleqN":      {verb: "Test whether %s is at most %s", args: 2},
	"fsqrt":      {verb: "Take the Square Root of %s", args: 1},
	"fpow":       {verb: "Raise %s to the Power %s", args: 2},
	"parseFloat": {verb: "Parse %s as a Float", args: 1},
	"getFloat":   {verb: "Get Float from Command Line", args: 0, bound: "Get Float `%s` from Command Line"},
	"printFloat": {verb: "Print %s to Command Line", args: 1},

	// D3 BLAS kernels.
	"dot2":    {verb: "Compute the Dot Product of %s, %s and %s, %s", args: 4},
	"dotList": {verb: "Compute the Dot Product of %s and %s", args: 2},
	"gemmSum": {verb: "Multiply Matrices %s and %s and Sum the Entries", args: 2},

	// D4 NumPy-capability suite.
	"npDot":    {verb: "Compute the Dot Product of %s and %s (NumPy)", args: 2},
	"npMean":   {verb: "Compute the Mean of %s (NumPy)", args: 1},
	"npMatSum": {verb: "Multiply Matrices %s and %s and Sum the Entries (NumPy)", args: 2},
	"npVar":    {verb: "Compute the Variance of %s (NumPy)", args: 1},
	"npMax":    {verb: "Find the Maximum of %s (NumPy)", args: 1},
	"npNorm":   {verb: "Compute the Norm of %s (NumPy)", args: 1},

	// E4 wavelet data plane (kv / object / queue).
	"kvPutCode":   {verb: "Store under Key %s the Value %s in the Key-Value Store", args: 2},
	"kvGetCode":   {verb: "Fetch the Value under Key %s from the Key-Value Store", args: 1},
	"kvDelCode":   {verb: "Delete Key %s from the Key-Value Store", args: 1},
	"objPutCode":  {verb: "Store under Object Key %s the Content %s in the Object Store", args: 2},
	"objGetCode":  {verb: "Fetch Object %s from the Object Store", args: 1},
	"objDelCode":  {verb: "Delete Object %s from the Object Store", args: 1},
	"enqueueCode": {verb: "Add %s to the Queue", args: 1},
	"dequeueCode": {verb: "Take the Next Item from the Queue", args: 0},

	// Phase-0 real-byte strings (Bin).
	"binEmpty": {verb: "Make an Empty Byte String", args: 0},
	"binCons":  {verb: "Prepend Byte %s to %s", args: 2},
	"binLen":   {verb: "Measure the Length of %s", args: 1},
	"binAt":    {verb: "Get from %s the Byte at Position %s", args: 2},
	"printBin": {verb: "Print %s to Command Line", args: 1},

	// Phase-1 sockets.
	"sockConnect": {verb: "Connect to Host %s on Port %s", args: 2},
	"sockWrite":   {verb: "Send over Connection %s the Data %s", args: 2},
	"sockRead":    {verb: "Receive from Connection %s up to %s Bytes", args: 2},
	"sockClose":   {verb: "Close Connection %s", args: 1},
	"sockListen":  {verb: "Listen for Connections on Port %s", args: 1},
	"sockAccept":  {verb: "Accept a Connection from Listener %s", args: 1},

	// Phase-1 filesystem over Bin.
	"fsWrite":  {verb: "Write to File %s the Bytes %s", args: 2},
	"fsRead":   {verb: "Read the Bytes of File %s", args: 1},
	"fsExists": {verb: "Check whether File %s Exists", args: 1},
	"fsRemove": {verb: "Remove File %s", args: 1},
	"fsMkdir":  {verb: "Make Directory %s", args: 1},

	// Singletons: process, crypto, TLS.
	"procRun": {verb: "Run Program %s with Argument %s", args: 2},
	"sha256":  {verb: "Compute the SHA-256 Digest of %s", args: 1},
	"tlsGet":  {verb: "Fetch over HTTPS from Host %s Port %s Path %s", args: 3},
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `go test ./internal/explain`
Expected: PASS (coverage gate + all Task 3 tests still green; the seed rows were kept verbatim inside the full table)

- [ ] **Step 5: Commit**

```bash
git -C /home/brainfuel/matt/goforge.dev/rune add internal/explain/templates.go internal/explain/templates_test.go
git -C /home/brainfuel/matt/goforge.dev/rune commit -m "feat(explain): English template for every ioprims prim + coverage gate"
```

---

### Task 5: Depth 1..n inlining

**Files:**
- Modify: `internal/explain/explain.go` (`callStep` only)
- Test: `internal/explain/depth_test.go` (new)

**Interfaces:**
- Consumes: `session.SurfaceDef`, `session.Accelerated` (Task 2); `ctx.depth` (Task 3).
- Produces: `Options.Depth > 0` inlines called USER definitions under their call step, one budget unit per level. Prims (template hit happens before the def lookup), foreign axioms (no SurfaceDef), and builtin-accelerated defs stay one-line.

- [ ] **Step 1: Write the failing tests**

Create `internal/explain/depth_test.go`:

```go
package explain

import (
	"strings"
	"testing"
)

const depthSrc = `data Nat : U is zero : Nat | succ : Nat -> Nat end
foreign printNat : Nat -> IO Nat end
inner : IO Nat is printNat zero end
outer : IO Nat is bindIO Nat Nat inner (fn (r : Nat) is inner end) end
top : IO Nat is outer end
`

// TestExplainDepth0Calls: at depth 0 a user call is one line, no kids.
func TestExplainDepth0Calls(t *testing.T) {
	s := load(t, depthSrc)
	want := "[Entrypoint: outer]\n" +
		"[Apply `inner` (gives IO Nat) as `r`]\n" +
		"[Apply `inner` (gives IO Nat)]\n"
	if got := explainText(t, s, "outer", Options{}); got != want {
		t.Errorf("depth 0:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainDepth1Inlines: depth 1 inlines the callee body one level.
func TestExplainDepth1Inlines(t *testing.T) {
	s := load(t, depthSrc)
	want := "[Entrypoint: outer]\n" +
		"[Apply `inner` (gives IO Nat) as `r`]\n" +
		"  [Print (zero) to Command Line]\n" +
		"[Apply `inner` (gives IO Nat)]\n" +
		"  [Print (zero) to Command Line]\n"
	if got := explainText(t, s, "outer", Options{Depth: 1}); got != want {
		t.Errorf("depth 1:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainDepthBudget: depth 1 from `top` inlines outer but NOT inner
// (budget exhausted); depth 2 reaches both levels.
func TestExplainDepthBudget(t *testing.T) {
	s := load(t, depthSrc)
	want1 := "[Entrypoint: top]\n" +
		"[Apply `outer` (gives IO Nat)]\n" +
		"  [Apply `inner` (gives IO Nat) as `r`]\n" +
		"  [Apply `inner` (gives IO Nat)]\n"
	if got := explainText(t, s, "top", Options{Depth: 1}); got != want1 {
		t.Errorf("depth 1 from top:\ngot:\n%swant:\n%s", got, want1)
	}
	want2 := "[Entrypoint: top]\n" +
		"[Apply `outer` (gives IO Nat)]\n" +
		"  [Apply `inner` (gives IO Nat) as `r`]\n" +
		"    [Print (zero) to Command Line]\n" +
		"  [Apply `inner` (gives IO Nat)]\n" +
		"    [Print (zero) to Command Line]\n"
	if got := explainText(t, s, "top", Options{Depth: 2}); got != want2 {
		t.Errorf("depth 2 from top:\ngot:\n%swant:\n%s", got, want2)
	}
}

// TestExplainAcceleratedStaysOneLine: a builtin-accelerated def never inlines
// (its eliminator body is a fuel loop, not the meaning).
func TestExplainAcceleratedStaysOneLine(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
addN : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a end
end
builtin natAdd addN
useAdd : Nat -> Nat is fn (n : Nat) is addN n n end end
`
	s := load(t, src)
	got := explainText(t, s, "useAdd", Options{Depth: 5})
	want := "[Entrypoint: useAdd]\n" +
		"[Given `n`:]\n" +
		"  [Apply `addN` to (n) and (n) (takes Nat and Nat, gives Nat)]\n"
	if got != want {
		t.Errorf("accelerated:\ngot:\n%swant:\n%s", got, want)
	}
	if strings.Contains(got, "NatElim") {
		t.Error("accelerated def body leaked into the explanation")
	}
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `go test ./internal/explain -run TestExplainDepth`
Expected: TestExplainDepth0Calls PASSES already; TestExplainDepth1Inlines and TestExplainDepthBudget FAIL (no kids under call steps)

- [ ] **Step 3: Implement inlining in callStep**

In `internal/explain/explain.go`, replace `callStep`'s final `return Step{Text: text, Code: printExp(e)}` with:

```go
	st := Step{Text: text, Code: printExp(e)}
	if c.depth > 0 && !c.s.Accelerated(full) {
		if sd, ok := c.s.SurfaceDef(full); ok {
			sub := &ctx{s: c.s, defs: c.defs, depth: c.depth - 1}
			st.Kids = sub.steps(sd.Body, "")
		}
	}
	return st
```

And update `callStep`'s doc comment last sentence from "Task 5 adds depth-n inlining here." to:

```go
// With a positive depth budget the callee's retained surface body inlines as
// Kids, one budget unit per level; prims, foreign axioms (no surface body),
// and builtin-accelerated defs stay one-line. The decrementing budget bounds
// recursion through partial/self-referential definitions.
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `go test ./internal/explain`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git -C /home/brainfuel/matt/goforge.dev/rune add internal/explain/explain.go internal/explain/depth_test.go
git -C /home/brainfuel/matt/goforge.dev/rune commit -m "feat(explain): depth-n inlining of user definitions with budget"
```

---

### Task 6: Depth core - the elaborated walk

**Files:**
- Modify: `internal/explain/explain.go` (`Options` + `Explain` + `ExplainExp`)
- Create: `internal/explain/coresteps.go`
- Test: `internal/explain/core_test.go` (new)

**Interfaces:**
- Consumes: `printCore` (Task 3), `session.Defs` (core bodies), `session.ElabExpr`, `session.RefNames`.
- Produces: `Options.Core bool`; core-mode Step trees. Core step texts (NORMATIVE for the Task 7 core goldens):
  - lambda: `` Given `x`: `` / `` Given implicit `x`: `` with kids (underscore binders SHOWN: nothing hidden)
  - let: `` Let `x` be (<printCore val>) `` then body steps as siblings
  - bindIO spine (exactly 4 args, head Ref named bindIO): `` Do (<printCore m>) as `x` : <printCore A> `` then the continuation body's steps as siblings; non-lambda continuation: `Do (<m>)` then `Pass the Result to (<k>)`
  - everything else: `Compute (<printCore t>)`
  - NO English prim templates in core mode; implicit applications visible in braces via printCore.

- [ ] **Step 1: Write the failing tests**

Create `internal/explain/core_test.go`:

```go
package explain

import "testing"

// TestExplainCoreDouble: the core walk of the double demo flattens the
// bindIO chain but shows raw core (no English templates), with the bound
// value's type after the binder.
func TestExplainCoreDouble(t *testing.T) {
	s := load(t, doubleSrc)
	want := "[Entrypoint: main]\n" +
		"[Do (getFloat) as `x` : Float]\n" +
		"[Do (printFloat (fmul x (fromNat 2)))]\n"
	if got := explainText(t, s, "main", Options{Core: true}); got != want {
		t.Errorf("core double:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainCoreImplicits: implicit binders are visible in the core walk.
func TestExplainCoreImplicits(t *testing.T) {
	src := "idi : {A : U} -> A -> A is fn {A : U} (x : A) is x end end\n"
	s := load(t, src)
	want := "[Entrypoint: idi]\n" +
		"[Given implicit `A`:]\n" +
		"  [Given `x`:]\n" +
		"    [Compute (x)]\n"
	if got := explainText(t, s, "idi", Options{Core: true}); got != want {
		t.Errorf("core implicits:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainExpCore: the $N path elaborates the expression and walks its
// core term.
func TestExplainExpCore(t *testing.T) {
	s := load(t, "data Nat : U is zero : Nat | succ : Nat -> Nat end\n")
	e, err := s.ParseSrcExpr("succ zero")
	if err != nil {
		t.Fatalf("ParseSrcExpr: %v", err)
	}
	root, err := ExplainExp(s, e, Options{Core: true})
	if err != nil {
		t.Fatalf("ExplainExp: %v", err)
	}
	want := "[Expression]\n" +
		"[Compute (succ zero)]\n"
	if got := RenderText(root); got != want {
		t.Errorf("core expression:\ngot:\n%swant:\n%s", got, want)
	}
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `go test ./internal/explain`
Expected: FAIL to compile (`Options` has no field `Core`)

- [ ] **Step 3: Implement core mode**

(a) In `internal/explain/explain.go`, add to `Options` after the `Depth` field:

```go
	// Core renders the elaborated core walk instead of the surface tree: the
	// nothing-hidden view. Implicits visible (braces), sugar expanded,
	// underscore binders shown, numerals as literals, no English templates.
	Core bool
```

(b) In `Explain`, insert BETWEEN the `root := ...` assignment and the `sd, ok := s.SurfaceDef(name)` line:

```go
	if opts.Core {
		if d.Body == nil {
			return Step{}, fmt.Errorf("explain: %q has no body to walk (a foreign axiom or postulate)", name)
		}
		root.Kids = coreSteps(s, d.Body, nil)
		return root, nil
	}
```

(c) In `ExplainExp`, insert at the top of the function body:

```go
	if opts.Core {
		tm, _, err := s.ElabExpr(e)
		if err != nil {
			return Step{}, err
		}
		return Step{Text: "Expression", Kids: coreSteps(s, tm, nil)}, nil
	}
```

(d) Create `internal/explain/coresteps.go`:

```go
package explain

import (
	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/internal/session"
)

// coreSteps renders the elaborated core walk (the nothing-hidden view):
// binders peel to Given steps (implicits marked, underscore names shown),
// lets show their value, bindIO chains still flatten (the step LIST is the
// shared shape across depths) with the bound value's type after the binder,
// and everything else prints whole via printCore. English prim templates do
// NOT apply here: the reader asked to see the program as the kernel sees it.
func coreSteps(s *session.Session, t core.Tm, env []string) []Step {
	rn := s.RefNames()
	switch x := t.(type) {
	case core.Lam:
		n := scopeName(x.Body)
		label := "Given `" + n + "`:"
		if x.Icit == core.Impl {
			label = "Given implicit `" + n + "`:"
		}
		return []Step{{Text: label, Kids: coreSteps(s, x.Body.Body, append(env, n))}}
	case core.Let:
		n := scopeName(x.Body)
		st := Step{Text: "Let `" + n + "` be (" + printCore(x.Val, env, rn) + ")"}
		return append([]Step{st}, coreSteps(s, x.Body.Body, append(env, n))...)
	}
	head, args := coreSpine(t)
	if r, ok := head.(core.Ref); ok && len(args) == 4 {
		if n, named := rn[r.Hash]; named && short(n) == "bindIO" {
			aTy := printCore(args[0], env, rn)
			m, k := args[2], args[3]
			if lam, isLam := k.(core.Lam); isLam {
				b := scopeName(lam.Body)
				first := Step{Text: "Do (" + printCore(m, env, rn) + ") as `" + b + "` : " + aTy}
				return append([]Step{first}, coreSteps(s, lam.Body.Body, append(env, b))...)
			}
			return []Step{
				{Text: "Do (" + printCore(m, env, rn) + ")"},
				{Text: "Pass the Result to (" + printCore(k, env, rn) + ")"},
			}
		}
	}
	return []Step{{Text: "Compute (" + printCore(t, env, rn) + ")"}}
}

// coreSpine flattens a left-associated core application into head + args
// (implicit args included: nothing hidden).
func coreSpine(t core.Tm) (core.Tm, []core.Tm) {
	var args []core.Tm
	for {
		a, ok := t.(core.App)
		if !ok {
			break
		}
		args = append([]core.Tm{a.Arg}, args...)
		t = a.Fn
	}
	return t, args
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `go test ./internal/explain`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git -C /home/brainfuel/matt/goforge.dev/rune add internal/explain/explain.go internal/explain/coresteps.go internal/explain/core_test.go
git -C /home/brainfuel/matt/goforge.dev/rune commit -m "feat(explain): depth-core elaborated walk with implicits visible"
```

---

### Task 7: CLI `rune explain` subcommand + goldens

This is spec acceptance items 1 and 2 at the CLI: the double demo's four lines verbatim, and goldens for `--depth 0`, `--depth 1`, and `--core` over the double demo plus listings ch566/ch567/ch568 (4 representative programs).

**Files:**
- Create: `cmd/rune/explain.go`
- Modify: `cmd/rune/main.go` (doc comment ~line 12, switch ~line 98 before `case "build"`, usage ~line 285 after the `rune run` line)
- Create: `cmd/rune/testdata/explain/` goldens (9 files, contents below)
- Test: `cmd/rune/explain_test.go` (new)

**Interfaces:**
- Consumes: `explain.Explain`, `explain.Options`, `explain.RenderText` (Tasks 3-6); `pathExists`, `expandRunePath`, `readNamedSources`, `sourcesHaveBuiltinNat`, `prelude.Source`, `session.LoadSet` (existing).
- Produces: `func runExplainCLI(args []string, w io.Writer) error` and `func parseExplainArgs(args []string) (explainArgs, error)` with `type explainArgs struct { files []string; main string; opts explain.Options; noPrelude bool }`. Task 8 extends both with `--annotate`/`--width`.

- [ ] **Step 1: Write the golden files**

Create `cmd/rune/testdata/explain/double_depth0.golden` (the spec's normative output):

```
[Entrypoint: main]
[Get Float `x` from Command Line]
[Apply Function (fmul x (fromNat 2))]
[Print Result to Command Line]
```

Create `cmd/rune/testdata/explain/double_depth1.golden` (identical to depth 0: the double demo calls only prims, which never inline):

```
[Entrypoint: main]
[Get Float `x` from Command Line]
[Apply Function (fmul x (fromNat 2))]
[Print Result to Command Line]
```

Create `cmd/rune/testdata/explain/double_core.golden`:

```
[Entrypoint: main]
[Do (getFloat) as `x` : Float]
[Do (printFloat (fmul x (fromNat 2)))]
```

Create `cmd/rune/testdata/explain/ch566_depth0.golden`:

```
[Entrypoint: main]
[Get Float `x` from Command Line]
[Apply Function (fmul x (fromNat 2))]
[Print Result to Command Line]
[Apply `probe` (gives IO Nat)]
[Apply `probe2` (gives IO Nat)]
```

Create `cmd/rune/testdata/explain/ch566_depth1.golden`:

```
[Entrypoint: main]
[Get Float `x` from Command Line]
[Apply Function (fmul x (fromNat 2))]
[Print Result to Command Line]
[Apply `probe` (gives IO Nat)]
  [When (parseFloat 23290465) is (none):]
    [Print (999) to Command Line]
  [When (parseFloat 23290465) is (some x):]
    [Print (111) to Command Line]
[Apply `probe2` (gives IO Nat)]
  [When (parseFloat 302) is (none):]
    [Print (999) to Command Line]
  [When (parseFloat 302) is (some x):]
    [Print (111) to Command Line]
```

Create `cmd/rune/testdata/explain/ch566_core.golden` (underscore binders SHOWN: nothing hidden):

```
[Entrypoint: main]
[Do (getFloat) as `x` : Float]
[Do (printFloat (fmul x (fromNat 2))) as `_y` : Float]
[Do (probe) as `_r` : Nat]
[Do (probe2)]
```

Create `cmd/rune/testdata/explain/ch567_depth0.golden` (13 named binds + the tail; binder `u` repeats in the source):

```
[Entrypoint: main]
[Apply `tryPrint` to (304) (takes Nat, gives IO Nat) as `u`]
[Apply `tryPrint` to (306) (takes Nat, gives IO Nat) as `u`]
[Apply `tryPrint` to (5170605619) (takes Nat, gives IO Nat) as `u`]
[Apply `tryPrint` to (5187186733) (takes Nat, gives IO Nat) as `u`]
[Apply `tryPrint` to (20407601) (takes Nat, gives IO Nat) as `u`]
[Apply `tryPrint` to (5103576369) (takes Nat, gives IO Nat) as `u`]
[Apply `tryPrint` to (5120353585) (takes Nat, gives IO Nat) as `u`]
[Apply `tryPrint` to (5203911985) (takes Nat, gives IO Nat) as `u`]
[Apply `tryPrint` to (5220689201) (takes Nat, gives IO Nat) as `u`]
[Apply `tryPrint` to (344342111006257) (takes Nat, gives IO Nat) as `u`]
[Apply `tryPrint` to (20000304) (takes Nat, gives IO Nat) as `u`]
[Apply `tryProbe` to (77358) (takes Nat, gives IO Nat) as `u`]
[Apply `tryProbe` to (91441) (takes Nat, gives IO Nat) as `u`]
[Apply `tryProbe` to (20258869) (takes Nat, gives IO Nat)]
```

Create `cmd/rune/testdata/explain/ch568_depth0.golden` (the dotted foreign `Std.Demo.printNat` displays and templates by its last segment):

```
[Entrypoint: main]
[Print (42) to Command Line]
```

Create `cmd/rune/testdata/explain/ch568_core.golden`:

```
[Entrypoint: main]
[Compute (printNat 42)]
```

- [ ] **Step 2: Write the failing test**

Create `cmd/rune/explain_test.go`:

```go
package main

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"
)

// TestParseExplainArgs pins the flag surface: positional paths + [name],
// --depth n|core, --core, --no-prelude; name defaults to main.
func TestParseExplainArgs(t *testing.T) {
	ea, err := parseExplainArgs([]string{"../../examples/double.rune", "main", "--depth", "2"})
	if err != nil {
		t.Fatal(err)
	}
	if len(ea.files) != 1 || ea.main != "main" || ea.opts.Depth != 2 || ea.opts.Core {
		t.Errorf("got %+v", ea)
	}
	ea, err = parseExplainArgs([]string{"../../examples/double.rune", "--core"})
	if err != nil {
		t.Fatal(err)
	}
	if ea.main != "main" || !ea.opts.Core {
		t.Errorf("got %+v, want default main + core", ea)
	}
	if _, err := parseExplainArgs([]string{"../../examples/double.rune", "--depth", "banana"}); err == nil {
		t.Error("want error for --depth banana")
	}
	if _, err := parseExplainArgs(nil); err == nil {
		t.Error("want error for no files")
	}
}

// TestExplainGoldens locks the phrasing (spec acceptance items 1 and 2):
// depth 0 / depth 1 / core over the double demo and listings ch566-ch568.
func TestExplainGoldens(t *testing.T) {
	cases := []struct {
		name string
		args []string
	}{
		{"double_depth0", []string{"../../examples/double.rune", "main"}},
		{"double_depth1", []string{"../../examples/double.rune", "main", "--depth", "1"}},
		{"double_core", []string{"../../examples/double.rune", "main", "--core"}},
		{"ch566_depth0", []string{"../../listings/ch566_float_io.rune", "main"}},
		{"ch566_depth1", []string{"../../listings/ch566_float_io.rune", "main", "--depth", "1"}},
		{"ch566_core", []string{"../../listings/ch566_float_io.rune", "main", "--core"}},
		{"ch567_depth0", []string{"../../listings/ch567_float_format.rune", "main"}},
		{"ch568_depth0", []string{"../../listings/ch568_dotted_foreign_run.rune", "main"}},
		{"ch568_core", []string{"../../listings/ch568_dotted_foreign_run.rune", "main", "--core"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			var buf bytes.Buffer
			if err := runExplainCLI(tc.args, &buf); err != nil {
				t.Fatalf("runExplainCLI: %v", err)
			}
			want, err := os.ReadFile(filepath.Join("testdata", "explain", tc.name+".golden"))
			if err != nil {
				t.Fatal(err)
			}
			if buf.String() != string(want) {
				t.Errorf("golden mismatch:\ngot:\n%s\nwant:\n%s", buf.String(), want)
			}
		})
	}
}
```

- [ ] **Step 3: Run the test to verify it fails**

Run: `go test ./cmd/rune -run 'TestParseExplainArgs|TestExplainGoldens'`
Expected: FAIL to compile with `undefined: parseExplainArgs` / `undefined: runExplainCLI`

- [ ] **Step 4: Implement the subcommand**

Create `cmd/rune/explain.go`:

```go
package main

import (
	"fmt"
	"io"
	"strconv"
	"strings"

	"goforge.dev/rune/v3/internal/explain"
	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// explainArgs is the parsed command line of `rune explain`.
type explainArgs struct {
	files     []string
	main      string
	opts      explain.Options
	noPrelude bool
}

// parseExplainArgs mirrors parseEmitArgs: positional <path...> [name] with
// non-recursive directory expansion, plus --depth <n|core> (or --depth=),
// --core (alias for --depth core), and --no-prelude. The last positional is
// the target name iff an earlier positional names an existing path and the
// last one does not; the name defaults to "main".
func parseExplainArgs(args []string) (explainArgs, error) {
	var ea explainArgs
	var pos []string
	for i := 0; i < len(args); i++ {
		a := args[i]
		switch {
		case a == "--no-prelude":
			ea.noPrelude = true
		case a == "--core":
			ea.opts.Core = true
		case a == "--depth" || strings.HasPrefix(a, "--depth="):
			v := strings.TrimPrefix(a, "--depth=")
			if a == "--depth" {
				if i+1 >= len(args) {
					return ea, fmt.Errorf("--depth needs a value (a number or core)")
				}
				i++
				v = args[i]
			}
			if v == "core" {
				ea.opts.Core = true
				break
			}
			n, err := strconv.Atoi(v)
			if err != nil || n < 0 {
				return ea, fmt.Errorf("--depth needs a non-negative number or core, got %q", v)
			}
			ea.opts.Depth = n
		default:
			pos = append(pos, a)
		}
	}
	if len(pos) == 0 {
		return ea, fmt.Errorf("explain needs a file")
	}
	paths := pos
	if len(pos) > 1 {
		lastExists := pathExists(pos[len(pos)-1])
		anyEarlierExists := false
		for _, p := range pos[:len(pos)-1] {
			if pathExists(p) {
				anyEarlierExists = true
				break
			}
		}
		if !lastExists && anyEarlierExists {
			ea.main = pos[len(pos)-1]
			paths = pos[:len(pos)-1]
		}
	}
	for _, p := range paths {
		expanded, err := expandRunePath(p)
		if err != nil {
			return ea, err
		}
		ea.files = append(ea.files, expanded...)
	}
	if len(ea.files) == 0 {
		return ea, fmt.Errorf("no .rune source files found in the given paths")
	}
	if ea.main == "" {
		ea.main = "main"
	}
	return ea, nil
}

// runExplainCLI drives `rune explain`: load the compilation set exactly as
// emit/run do (prelude on-demand, LoadSet topo sort), render the named
// definition as English steps, print.
func runExplainCLI(args []string, w io.Writer) error {
	ea, err := parseExplainArgs(args)
	if err != nil {
		return err
	}
	sources, err := readNamedSources(ea.files)
	if err != nil {
		return err
	}
	s := session.New()
	if !ea.noPrelude && !sourcesHaveBuiltinNat(sources) {
		if _, err := s.LoadSource(prelude.Source()); err != nil {
			return fmt.Errorf("loading prelude: %w", err)
		}
	}
	if err := session.LoadSet(s, sources); err != nil {
		return err
	}
	root, err := explain.Explain(s, ea.main, ea.opts)
	if err != nil {
		return err
	}
	_, err = io.WriteString(w, explain.RenderText(root))
	return err
}
```

In `cmd/rune/main.go`:

(a) Add to the doc comment, after the `rune run` line (~line 7):

```go
//	rune explain <file> [name]      render a definition as English steps (--depth n|core)
```

(b) Add the switch case, immediately before `case "build":` (~line 98):

```go
	case "explain":
		if err := runExplainCLI(os.Args[2:], os.Stdout); err != nil {
			fatal(err)
		}
```

(c) Add to `usage()`, after the `rune run` line (~line 285):

```go
	fmt.Fprintln(os.Stderr, "  rune explain <path...> [name] [--depth n|core] [--no-prelude]   (English step view; name defaults to main)")
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `go test ./cmd/rune -run 'TestParseExplainArgs|TestExplainGoldens'`
Expected: PASS (9 golden subtests + arg parsing). If a golden subtest fails, the implementation diverged from the Task 3/6 rendering rules; fix the CODE against the rules, not the golden. The one legitimate golden-side fix is a core binder-name hint differing from the source (`x`, `_y`, `_r` in ch566): if the elaborated Scope.Name hints differ, verify against `rune repl` `:list`-style output, then correct the core goldens and note it in the commit message.

- [ ] **Step 6: Smoke-run the CLI (the spec's exact invocation)**

Run: `go run ./cmd/rune explain examples/double.rune main`
Expected output (exactly the spec's four lines):

```
[Entrypoint: main]
[Get Float `x` from Command Line]
[Apply Function (fmul x (fromNat 2))]
[Print Result to Command Line]
```

- [ ] **Step 7: Commit**

```bash
git -C /home/brainfuel/matt/goforge.dev/rune add cmd/rune/explain.go cmd/rune/explain_test.go cmd/rune/main.go cmd/rune/testdata/explain
git -C /home/brainfuel/matt/goforge.dev/rune commit -m "feat(cli): rune explain subcommand with depth 0/1/core goldens"
```

---

### Task 8: --annotate layouts (wide two-column, narrow interleaved)

Spec acceptance item 4: goldens for both layouts at a fixed `--width`. The layout rule (NORMATIVE): rows are the pre-order step list; each row is (indent, code, english). Wide layout (width >= 100): left column width `colW` = the longest `indent+code`, capped at `(width-2)/2`; a longer code hard-wraps at `colW` onto continuation lines; the first chunk is padded to `colW`, then TWO spaces, then `indent + [english]`. Narrow layout (width < 100): `indent + "-- " + [english]` on one line, `indent + code` on the next (code line omitted when the step has no code). Width comes only from `--width` (default 80): deterministic, no TTY probing, no x/term dependency.

**Files:**
- Create: `internal/explain/annotate.go`
- Modify: `cmd/rune/explain.go` (add `--annotate`/`--width` to explainArgs + parsing + rendering choice)
- Modify: `cmd/rune/main.go` (usage line gains the flags)
- Modify: `PARKING-LOT.md` (one line: runtime TTY width)
- Create: `cmd/rune/testdata/explain/double_annotate_wide.golden`, `cmd/rune/testdata/explain/double_annotate_narrow.golden`
- Test: `internal/explain/annotate_test.go` (new), extend `cmd/rune/explain_test.go`

**Interfaces:**
- Consumes: `Step` (Task 3; the `Code` field carries each step's source fragment).
- Produces: `func RenderAnnotate(root Step, width int) string`; `const annotateWideMin = 100`; explainArgs gains `annotate bool` and `width int`.

- [ ] **Step 1: Write the failing unit tests**

Create `internal/explain/annotate_test.go`:

```go
package explain

import (
	"strings"
	"testing"
)

// TestRenderAnnotateNarrow: below the width threshold the English rides a
// `--` comment line above each code fragment.
func TestRenderAnnotateNarrow(t *testing.T) {
	s := load(t, doubleSrc)
	root, err := Explain(s, "main", Options{})
	if err != nil {
		t.Fatal(err)
	}
	want := "-- [Entrypoint: main]\n" +
		"main : IO Float\n" +
		"-- [Get Float `x` from Command Line]\n" +
		"getFloat\n" +
		"-- [Apply Function (fmul x (fromNat 2))]\n" +
		"fmul x (fromNat 2)\n" +
		"-- [Print Result to Command Line]\n" +
		"printFloat (fmul x (fromNat 2))\n"
	if got := RenderAnnotate(root, 60); got != want {
		t.Errorf("narrow:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestRenderAnnotateWide: at or above the threshold, two columns aligned by
// line; the column is the longest code line (capped at half the width).
func TestRenderAnnotateWide(t *testing.T) {
	s := load(t, doubleSrc)
	root, err := Explain(s, "main", Options{})
	if err != nil {
		t.Fatal(err)
	}
	got := RenderAnnotate(root, 120)
	lines := strings.Split(strings.TrimRight(got, "\n"), "\n")
	if len(lines) != 4 {
		t.Fatalf("wide: got %d lines, want 4:\n%s", len(lines), got)
	}
	// Every English bracket opens at the same column: colW (31) + 2.
	for _, ln := range lines {
		if idx := strings.Index(ln, "["); idx != 33 {
			t.Errorf("wide: bracket at col %d, want 33 in %q", idx, ln)
		}
	}
	if !strings.HasPrefix(lines[0], "main : IO Float") {
		t.Errorf("wide first line = %q", lines[0])
	}
}

// TestRenderAnnotateWrap: a code fragment longer than the capped column
// hard-wraps onto continuation lines; the English stays on the first line.
func TestRenderAnnotateWrap(t *testing.T) {
	long := strings.Repeat("abcdefghij", 7) // 70 chars
	root := Step{Text: "Entrypoint: w", Code: "w : T", Kids: []Step{
		{Text: "Apply Function (long)", Code: long},
	}}
	got := RenderAnnotate(root, 100) // cap = (100-2)/2 = 49
	lines := strings.Split(strings.TrimRight(got, "\n"), "\n")
	if len(lines) != 3 {
		t.Fatalf("wrap: got %d lines, want 3 (2 rows + 1 continuation):\n%s", len(lines), got)
	}
	if lines[2] != long[49:] {
		t.Errorf("continuation = %q, want %q", lines[2], long[49:])
	}
	if !strings.Contains(lines[1], "[Apply Function (long)]") {
		t.Errorf("English missing from first chunk line: %q", lines[1])
	}
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `go test ./internal/explain -run TestRenderAnnotate`
Expected: FAIL to compile with `undefined: RenderAnnotate`

- [ ] **Step 3: Implement the layouts**

Create `internal/explain/annotate.go`:

```go
package explain

import "strings"

// annotateWideMin is the width at or above which --annotate uses the
// two-column layout; below it the narrow interleaved layout is used. Width
// is supplied by the frontend (the CLI --width flag, default 80): the
// explainer never probes a TTY, so output stays deterministic.
const annotateWideMin = 100

// RenderAnnotate renders the annotated view of a Step tree: code beside its
// English. Wide (width >= annotateWideMin): two columns aligned by line,
// code left, English right, long code hard-wrapped at the column. Narrow:
// a `--` comment line with the English above each code fragment.
func RenderAnnotate(root Step, width int) string {
	rows := annotateRows(root)
	if width >= annotateWideMin {
		return renderWide(rows, width)
	}
	return renderNarrow(rows)
}

// annotateRow is one pre-order step: its indentation, source fragment (may
// be empty for structural steps), and bracketed English.
type annotateRow struct {
	ind  string
	code string
	eng  string
}

func annotateRows(root Step) []annotateRow {
	var rows []annotateRow
	var walk func(st Step, depth int)
	walk = func(st Step, depth int) {
		ind := ""
		if depth > 1 {
			ind = strings.Repeat(" ", (depth-1)*2)
		}
		rows = append(rows, annotateRow{ind: ind, code: st.Code, eng: "[" + st.Text + "]"})
		for _, k := range st.Kids {
			walk(k, depth+1)
		}
	}
	walk(root, 0)
	return rows
}

func renderNarrow(rows []annotateRow) string {
	var sb strings.Builder
	for _, r := range rows {
		sb.WriteString(r.ind + "-- " + r.eng + "\n")
		if r.code != "" {
			sb.WriteString(r.ind + r.code + "\n")
		}
	}
	return sb.String()
}

func renderWide(rows []annotateRow, width int) string {
	colW := 0
	for _, r := range rows {
		if n := len(r.ind) + len(r.code); n > colW {
			colW = n
		}
	}
	if capW := (width - 2) / 2; colW > capW {
		colW = capW
	}
	var sb strings.Builder
	for _, r := range rows {
		chunks := hardWrap(r.ind+r.code, colW)
		first := chunks[0]
		sb.WriteString(first + strings.Repeat(" ", colW-len(first)) + "  " + r.ind + r.eng + "\n")
		for _, ch := range chunks[1:] {
			sb.WriteString(ch + "\n")
		}
	}
	return sb.String()
}

// hardWrap splits s into chunks of at most w bytes (the explainer's output
// is ASCII); an empty s yields one empty chunk.
func hardWrap(s string, w int) []string {
	if s == "" || w <= 0 {
		return []string{""}
	}
	var out []string
	for len(s) > w {
		out = append(out, s[:w])
		s = s[w:]
	}
	return append(out, s)
}
```

- [ ] **Step 4: Run the unit tests to verify they pass**

Run: `go test ./internal/explain -run TestRenderAnnotate`
Expected: PASS

- [ ] **Step 5: Wire the CLI flags + write the CLI goldens**

In `cmd/rune/explain.go`:

(a) Add two fields to `explainArgs`:

```go
	annotate  bool
	width     int
```

(b) In `parseExplainArgs`, set the default before the loop (first statement of the function body, after `var ea explainArgs`):

```go
	ea.width = 80
```

and add two cases to the flag switch (before `default:`):

```go
		case a == "--annotate":
			ea.annotate = true
		case a == "--width" || strings.HasPrefix(a, "--width="):
			v := strings.TrimPrefix(a, "--width=")
			if a == "--width" {
				if i+1 >= len(args) {
					return ea, fmt.Errorf("--width needs a value")
				}
				i++
				v = args[i]
			}
			n, err := strconv.Atoi(v)
			if err != nil || n < 1 {
				return ea, fmt.Errorf("--width needs a positive number, got %q", v)
			}
			ea.width = n
```

(c) In `runExplainCLI`, replace the final `_, err = io.WriteString(w, explain.RenderText(root))` with:

```go
	out := explain.RenderText(root)
	if ea.annotate {
		out = explain.RenderAnnotate(root, ea.width)
	}
	_, err = io.WriteString(w, out)
```

(d) In `cmd/rune/main.go` `usage()`, replace the Task 7 explain line with:

```go
	fmt.Fprintln(os.Stderr, "  rune explain <path...> [name] [--depth n|core] [--annotate] [--width n] [--no-prelude]   (English step view; name defaults to main)")
```

Create `cmd/rune/testdata/explain/double_annotate_wide.golden` (width 120; colW = 31 = the longest code line, padding then two separator spaces, so every `[` opens at column 34, 1-indexed). EXACT content:

```
main : IO Float                  [Entrypoint: main]
getFloat                         [Get Float `x` from Command Line]
fmul x (fromNat 2)               [Apply Function (fmul x (fromNat 2))]
printFloat (fmul x (fromNat 2))  [Print Result to Command Line]
```

Create `cmd/rune/testdata/explain/double_annotate_narrow.golden` (width 60):

```
-- [Entrypoint: main]
main : IO Float
-- [Get Float `x` from Command Line]
getFloat
-- [Apply Function (fmul x (fromNat 2))]
fmul x (fromNat 2)
-- [Print Result to Command Line]
printFloat (fmul x (fromNat 2))
```

Append to the `cases` slice in `cmd/rune/explain_test.go` `TestExplainGoldens`:

```go
		{"double_annotate_wide", []string{"../../examples/double.rune", "main", "--annotate", "--width", "120"}},
		{"double_annotate_narrow", []string{"../../examples/double.rune", "main", "--annotate", "--width", "60"}},
```

- [ ] **Step 6: Run the CLI golden tests to verify they pass**

Run: `go test ./cmd/rune -run 'TestParseExplainArgs|TestExplainGoldens'`
Expected: PASS (11 golden subtests). Padding invariant if the wide golden mismatches: left column = 31 chars exactly, then 2 spaces (fix the code, not the golden; the column rule is normative).

- [ ] **Step 7: Record the parked TTY improvement**

Append one line to `PARKING-LOT.md` under its list of parked items:

```
- explain --annotate runtime TTY width detection (x/term.GetSize): parked to keep the dependency graph closed; --width flag (default 80) is the deterministic override and goldens fix both layouts.
```

- [ ] **Step 8: Commit**

```bash
git -C /home/brainfuel/matt/goforge.dev/rune add internal/explain/annotate.go internal/explain/annotate_test.go cmd/rune/explain.go cmd/rune/explain_test.go cmd/rune/main.go cmd/rune/testdata/explain/double_annotate_wide.golden cmd/rune/testdata/explain/double_annotate_narrow.golden PARKING-LOT.md
git -C /home/brainfuel/matt/goforge.dev/rune commit -m "feat(explain): --annotate wide two-column and narrow interleaved layouts"
```

---

### Task 9: REPL `:explain` ($N and names) + repl test

Spec acceptance item 3 and the standing REPL rule: a feature is not done until it works in `rune repl` with a repl test.

**Files:**
- Modify: `internal/repl/repl.go` (replState ~line 122, runExpr ~line 243, runCommand switch ~line 345, printHelp ~line 435)
- Create: `internal/repl/explain.go`
- Test: `internal/repl/repl_explain_test.go` (new)

**Interfaces:**
- Consumes: `explain.Explain`, `explain.ExplainExp`, `explain.Options`, `explain.RenderText`, `explain.RenderAnnotate` (Tasks 3-8); `session.SurfaceDef` via Explain; replState/runCommand/runExpr (existing).
- Produces: `replState.historyExps map[int]surface.Exp` (the surface expression of each numbered result, recorded before elaboration side effects); `:explain <name|$N|$> [--depth n|core] [--core] [--annotate] [--width n]`.

- [ ] **Step 1: Write the failing repl test**

Create `internal/repl/repl_explain_test.go`:

```go
package repl

import (
	"bytes"
	"strings"
	"testing"
)

// TestREPLExplain drives :explain through a scripted bare session: a numbered
// result ($N and bare $), a named definition, and the error paths. This is
// the standing REPL acceptance test for the explainer.
func TestREPLExplain(t *testing.T) {
	script := []string{
		"data Nat : U is zero : Nat | succ : Nat -> Nat end",
		"succ zero",
		":explain $2",
		"idn : Nat -> Nat is fn (n : Nat) is n end end",
		":explain idn",
		":explain $",
		":explain nosuch",
		":explain $99",
		":explain",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith: %v", err)
	}
	got := out.String()
	wants := []string{
		// :explain $2 and :explain $ both render the recorded expression.
		"[Expression]",
		"[Apply `succ` to (zero) (takes Nat, gives Nat)]",
		// :explain idn renders the retained surface definition.
		"[Entrypoint: idn]",
		"[Given `n`:]",
		"[Result: (n)]",
		// Error paths keep the loop alive.
		"no definition named \"nosuch\"",
		"no result $99 in this session",
		"usage: :explain",
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\nfull output:\n%s", w, got)
		}
	}
}

// TestREPLExplainCoreFlag: the depth dial reaches the REPL frontend.
func TestREPLExplainCoreFlag(t *testing.T) {
	script := []string{
		"idc : (A : U1) -> A -> A is fn (A : U1) (x : A) is x end end",
		":explain idc --depth core",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith: %v", err)
	}
	got := out.String()
	for _, w := range []string{"[Entrypoint: idc]", "[Given `A`:]", "[Given `x`:]", "[Compute (x)]"} {
		if !strings.Contains(got, w) {
			t.Errorf("core output missing %q\nfull output:\n%s", w, got)
		}
	}
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `go test ./internal/repl -run TestREPLExplain`
Expected: FAIL with `unknown command ":explain"` substrings missing (the loop prints the unknown-command error)

- [ ] **Step 3: Implement :explain**

(a) In `internal/repl/repl.go`, extend `replState` (~line 122):

```go
type replState struct {
	lineNo     int
	lastResult int
	lastLoad   string // the most recent :load path, for :reload (the D7 dev-loop)
	// historyExps retains each numbered result's SURFACE expression (as
	// entered, after $N expansion), so :explain $N can render it. The core
	// binding (__resN via BindResult) stays the evaluation authority; this is
	// display-side only.
	historyExps map[int]surface.Exp
}
```

(b) In `runExpr`, after `s.BindResult(...)` and before `st.lastResult = n` (~line 243), add:

```go
	if st.historyExps == nil {
		st.historyExps = map[int]surface.Exp{}
	}
	st.historyExps[n] = e
```

(c) In `runCommand`, add a case before `default:` (~line 345):

```go
	case ":explain":
		return runExplain(s, st, arg, out)
```

(d) In `printHelp`, after the `:run` line (~line 435), add:

```go
	fmt.Fprintln(out, "  :explain <name|$N> [--depth n|core] [--annotate] [--width n]   English step view of a definition or a numbered result")
```

(e) Create `internal/repl/explain.go`:

```go
package repl

import (
	"fmt"
	"io"
	"strconv"
	"strings"

	"goforge.dev/rune/v3/internal/explain"
	"goforge.dev/rune/v3/internal/session"
)

// runExplain is `:explain <name|$N|$> [--depth n|core] [--core] [--annotate]
// [--width n]`: the REPL frontend of internal/explain. A $N target renders
// the recorded surface expression of result N (bare $ = the latest); a name
// renders the retained surface definition. Same flags as the CLI; width
// defaults to 80 (deterministic, no TTY probing).
func runExplain(s *session.Session, st *replState, arg string, out io.Writer) error {
	fields := strings.Fields(arg)
	if len(fields) == 0 {
		return fmt.Errorf("usage: :explain <name|$N> [--depth n|core] [--annotate] [--width n]")
	}
	target := fields[0]
	var opts explain.Options
	annotate := false
	width := 80
	for i := 1; i < len(fields); i++ {
		switch fields[i] {
		case "--core":
			opts.Core = true
		case "--depth":
			i++
			if i >= len(fields) {
				return fmt.Errorf("--depth needs a value (a number or core)")
			}
			if fields[i] == "core" {
				opts.Core = true
				break
			}
			n, err := strconv.Atoi(fields[i])
			if err != nil || n < 0 {
				return fmt.Errorf("--depth needs a non-negative number or core, got %q", fields[i])
			}
			opts.Depth = n
		case "--annotate":
			annotate = true
		case "--width":
			i++
			if i >= len(fields) {
				return fmt.Errorf("--width needs a value")
			}
			n, err := strconv.Atoi(fields[i])
			if err != nil || n < 1 {
				return fmt.Errorf("--width needs a positive number, got %q", fields[i])
			}
			width = n
		default:
			return fmt.Errorf("unknown :explain flag %q", fields[i])
		}
	}
	var root explain.Step
	var err error
	if strings.HasPrefix(target, "$") {
		n := st.lastResult
		if len(target) > 1 {
			n, err = strconv.Atoi(target[1:])
			if err != nil {
				return fmt.Errorf(":explain: bad result reference %q", target)
			}
		}
		e, ok := st.historyExps[n]
		if !ok {
			return fmt.Errorf(":explain: no result $%d in this session", n)
		}
		root, err = explain.ExplainExp(s, e, opts)
	} else {
		root, err = explain.Explain(s, target, opts)
	}
	if err != nil {
		return err
	}
	if annotate {
		fmt.Fprint(out, explain.RenderAnnotate(root, width))
		return nil
	}
	fmt.Fprint(out, explain.RenderText(root))
	return nil
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `go test ./internal/repl -run TestREPLExplain`
Expected: PASS (both tests)

- [ ] **Step 5: Run the whole repl package (guard the loop)**

Run: `go test ./internal/repl`
Expected: PASS

- [ ] **Step 6: Commit**

```bash
git -C /home/brainfuel/matt/goforge.dev/rune add internal/repl/repl.go internal/repl/explain.go internal/repl/repl_explain_test.go
git -C /home/brainfuel/matt/goforge.dev/rune commit -m "feat(repl): :explain command for definitions and \$N results with repl test"
```

---

### Task 10: Docs - README section + CLAUDE.md note

**Files:**
- Modify: `README.md` (insert a new `### The explainer` subsection AFTER the "The double demo" section's closing line "The program is 9-way byte-identical across js/py/go/rs/erl/jvm/c/ll/wasm." and BEFORE "### The --no-prelude flag", ~line 263)
- Modify: `CLAUDE.md` (append one bullet after the "Script ergonomics (feat/script-ergonomics)" block, whose last line is "      prelude Std namespacing follow-up.", ~line 1026)

- [ ] **Step 1: Add the README section**

Insert into `README.md` (exact text; note ASCII hyphens only):

````markdown
### The explainer

`rune explain` renders a checked program as deterministic English steps, so a
non-programmer can read what a program does without learning the syntax first:

```sh
rune explain examples/double.rune main
# [Entrypoint: main]
# [Get Float `x` from Command Line]
# [Apply Function (fmul x (fromNat 2))]
# [Print Result to Command Line]
```

`--depth n` inlines called user definitions n levels deep (prims and
builtin-accelerated definitions stay one line); `--depth core` (or `--core`)
shows the elaborated core walk with implicits visible. `--annotate` pairs each
code fragment with its English: two columns at `--width` 100 or more,
interleaved `--` comment lines below that (width defaults to 80; no TTY
probing, so output is deterministic). The REPL has the same view:
`:explain <name>` or `:explain $N`. Every host-op prim carries an English
template, enforced by a coverage test against `codegen.IOPrimNames()`, and
golden files lock the phrasing. The output is compiler-derived; no LLM is
involved.
````

- [ ] **Step 2: Add the CLAUDE.md note**

Append after the script-ergonomics block (anchor line: `      prelude Std namespacing follow-up.`):

```markdown
    - **English explainer.** `rune explain FILES [name]`, REPL `:explain
      name|$N`, and `--annotate` render a checked definition as deterministic
      English steps from ONE Step-tree renderer (internal/explain): structure
      from the retained qualified surface Def (session.SurfaceDef), types from
      the elaborated core, bindIO chains flattened, every ioprims.go prim
      carrying an English template (coverage-gated via codegen.IOPrimNames()),
      depth dial 0|n|core, goldens lock phrasing (cmd/rune/testdata/explain).
      REPL retains $N surface expressions (historyExps). No new deps: --width
      flag (default 80), no TTY probe; x/term parked.
```

- [ ] **Step 3: Sanity-check the touched packages once**

Run: `go build ./... && go test ./codegen ./internal/session ./internal/explain ./internal/repl && go test ./cmd/rune -run 'TestParseExplainArgs|TestExplainGoldens'`
Expected: PASS everywhere. (Do NOT run the harness package; ~16 minutes.)

- [ ] **Step 4: Commit**

```bash
git -C /home/brainfuel/matt/goforge.dev/rune add README.md CLAUDE.md
git -C /home/brainfuel/matt/goforge.dev/rune commit -m "docs: explainer README section and CLAUDE.md feature note"
```

---

## Acceptance mapping (spec -> tasks)

| Spec acceptance item | Task(s) |
|---|---|
| 1. `rune explain double.rune main` emits the four-line English view at depth 0 | Task 3 (unit, exact string), Task 7 (CLI golden `double_depth0.golden` + smoke run) |
| 2. Goldens for `--depth 0`, `--depth 1`, `--core` over 3-5 representative listings, deterministic, phrasing locked | Task 7 (double + ch566 + ch567 + ch568; depth 0 x4, depth 1 x2, core x3) |
| 3. `:explain $N` works in the REPL with a repl test | Task 9 |
| 4. `--annotate` goldens for both layouts (wide two-column, narrow interleaved) | Task 8 (unit + CLI goldens at --width 120 / 60) |
| 5. A test enumerates every prim in ioprims.go and fails on a missing English template | Task 1 (IOPrimNames) + Task 4 (coverage gate + full table) |

Frontends: CLI (Task 7), REPL (Task 9), annotate views (Task 8) - all formatting the one Step tree (Task 3). Depth dial: 0 (Task 3), 1..n (Task 5), core (Task 6). Non-goals respected: no LLM anywhere (templates + goldens are static), no natural-language input, no editor/LSP work. Risks addressed: phrasing churn is confined to templates.go + goldens (Task 4 note); depth-n never shows fuel loops (Accelerated guard, Task 5); the two-column wrap rule is fixed early by goldens (Task 8).

## Final verification (after Task 10)

```bash
git -C /home/brainfuel/matt/goforge.dev/rune status --porcelain   # clean
go vet ./internal/explain ./cmd/rune ./internal/repl ./internal/session ./codegen
go run ./cmd/rune explain examples/double.rune main               # the spec's four lines
go run ./cmd/rune explain examples/double.rune main --annotate --width 120
printf 'idn : (A : U1) -> A -> A is fn (A : U1) (x : A) is x end end\n:explain idn\n:quit\n' | go run ./cmd/rune repl --no-prelude
grep -rnP '\x{2014}|\x{2013}' internal/explain cmd/rune/explain.go cmd/rune/testdata/explain README.md   # no em/en dashes: expect NO output (grep exits 1)
```

Do not run `go test ./...` (the harness takes ~16 minutes); the scoped commands in Task 10 Step 3 are the gate.
