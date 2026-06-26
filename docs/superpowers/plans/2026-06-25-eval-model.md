# Evaluation Model (Sub-plan 1): seeded effect-frontier + `do` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Introduce an opt-in concurrent-effect model: a `do ... end` block runs its effects on a seeded cooperative frontier scheduler (independent effects interleave by seed, `seq` linearizes), deterministic under a fixed seed and byte-identical across backends.

**Architecture:** `do` is surface sugar (like `seq`) that desugars to an ambient `frontier` effect builtin. `frontier` takes a list of `IO` actions and runs them on a scheduler that, at each step, uses a single shared LCG to pick the next ready action. One LCG specified once means every backend produces the same interleaving for the same seed, so the cross-backend conformance gate holds; varying the seed exhibits reordering (the Hello/World swap). No OS threads in this increment (the observable semantics is the seeded interleaving, not parallelism); real M-on-N parallelism is a later increment. The kernel is untouched: `frontier` is a bodiless content-addressed builtin, `do` is parser-level desugar.

**Tech Stack:** Go (the compiler and the Go and JS code emitters), the rune surface (lexer/parser/elaborate), the rune listings corpus, the Go test harness.

## Global Constraints

- Kernel frozen. No outer-core changes; hash-format stays 0x06. `frontier` is a session-registered bodiless builtin in its own hash space (the data-declaration pattern); `do` is parser desugar only.
- Content-addressing and byte-identical erasure are non-negotiable. The gate is `harness/backend_conformance_test.go` (same source, byte-identical `$show` on every backend).
- This increment ships on two backends (Go, JS) to prove portability; the remaining backends are a follow-on plan, explicitly scoped, not silently claimed.
- The scheduler PRNG is exactly this 64-bit LCG, identical on every backend: `state := state*6364136223846793005 + 1442695040888963407` (wrapping mod 2^64); the next ready index is `state mod readyCount`. The initial state is the seed (default seed 0).
- REPL acceptance: `do` must work in `wvl repl` (today `rune repl`) plus a repl test.
- No em or en dashes in any writing. Conventional Commits. Verify before claiming done.

---

## INTERFACE REVISION (pre-flight, authoritative; overrides any `frontier`/`List` mention below)

`List` is NOT an ambient former (listings define it themselves); only `IO` is ambient
(`s.st.AddIO()`). So the effect combinator is a binary `par`, not a list-taking `frontier`:

- **Builtin:** `par : (A : U) -> (B : U) -> IO A -> IO B -> IO B` (ambient, IO-only, bodiless,
  permanently neutral, own hash space).
- **`do a1 a2 ... an end`** desugars to right-nested `par`: `par _ _ a1 (par _ _ a2 (... an))`;
  a single-item `do a end` desugars to `a`.
- **Codegen flattens the `par`-spine.** When the emitter sees a saturated application headed by
  `par`, it recursively flattens nested `par` on the second IO argument into a compile-time slice
  `[a1, a2, ..., an]` and emits one `__frontier([a1, ..., an])` call. A bare `par x y` (not inside
  a `do`) flattens to `__frontier([x, y])`. So the runtime `__frontier` takes a host slice built by
  the emitter, NOT an erased `List` decoded at runtime.

Everywhere below: read `frontier` (the builtin) as `par`; read "the type `... List (IO A) -> IO A`"
as `par`'s type above; read "`__frontier` decodes the erased List" as "the emitter passes a
compile-time slice from the flattened `par`-spine." The LCG, `seq`-linearizes, conformance, and
order-independence tasks are unchanged.

---

### Task 1: The `frontier` builtin and its type

**Files:**
- Modify: `internal/session/session.go` (register the `frontier` builtin alongside the existing effect prims)
- Test: `internal/session/frontier_test.go`

**Interfaces:**
- Produces: an ambient identifier `frontier : (A : U) -> List (IO A) -> IO A`, a bodiless permanently-neutral builtin (a `core.Ref` with its own placeholder hash), resolvable from any program without a `foreign` declaration. Codegen targets the emitted name `frontier`.

- [ ] **Step 1: Write the failing test**

```go
// internal/session/frontier_test.go
package session

import "testing"

func TestFrontierBuiltinResolves(t *testing.T) {
	s := New()
	// `frontier` is ambient: a program may reference it with no foreign decl.
	src := "main : (A : U) -> List (IO A) -> IO A is frontier end"
	if _, err := s.LoadString("frontier_probe.rune", src); err != nil {
		t.Fatalf("frontier did not resolve as an ambient builtin: %v", err)
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestFrontierBuiltinResolves ./internal/session/`
Expected: FAIL ("frontier: unbound identifier" or similar).

- [ ] **Step 3: Register the builtin**

In `internal/session/session.go`, find where the existing effect/builtin group is registered (grep for `pureIO`/`bindIO`/`AddBuiltin`/the IO group registration) and add `frontier` to that group with type `(A : U) -> List (IO A) -> IO A`, bodiless and permanently neutral, in its own hash space (mirror the existing IO-group registration exactly; do not invent a new mechanism). The type references the ambient `List` and `IO` formers already registered.

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestFrontierBuiltinResolves ./internal/session/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add internal/session/session.go internal/session/frontier_test.go
git commit -m "feat(eval): ambient frontier builtin (List (IO A) -> IO A)"
```

---

### Task 2: `do ... end` surface syntax desugaring to `frontier`

**Files:**
- Modify: `surface/lexer.go` (recognize `do` as a contextual keyword, mirroring `seq`)
- Modify: `surface/parser.go` (add `parseDo`, dispatched where `parseSeq` is dispatched at line ~926)
- Test: `surface/parser_do_test.go`

**Interfaces:**
- Consumes: the `frontier` identifier from Task 1, and the ambient `cons`/`nil` list constructors.
- Produces: `do a1 a2 ... an end` parses to the application `frontier _ (cons _ a1 (cons _ a2 (... (nil _))))` (the type arg and list element type are holes the elaborator fills). `do` with one item desugars to that item wrapped in a singleton list.

- [ ] **Step 1: Write the failing test**

```go
// surface/parser_do_test.go
package surface

import "testing"

func TestParseDoDesugarsToFrontier(t *testing.T) {
	e, err := ParseExpr("do printHello printWorld end")
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	// The head of the desugared application must be the identifier `frontier`.
	if got := headIdent(e); got != "frontier" {
		t.Fatalf("do did not desugar to a frontier application; head = %q", got)
	}
}
```

(If `headIdent` does not exist as a test helper, add it in this test file: walk `EApp.Fn` to the leftmost `EVar` and return its `Name`.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestParseDoDesugarsToFrontier ./surface/`
Expected: FAIL ("do" parsed as an identifier application, or a parse error).

- [ ] **Step 3: Implement `parseDo`**

In `surface/lexer.go`, add `do` as a contextual keyword exactly as `seq` is handled (no new reserved token if `seq` uses a contextual check; follow that pattern so `do` stays usable as an identifier elsewhere). In `surface/parser.go`, at the dispatch site near line 926 (`return p.parseSeq()`), add a sibling `case` for `do` that calls a new `parseDo`. `parseDo` collects space-separated atom expressions until `end` (model the item collection on `parseSeq`/`desugarSeq`), then folds them into `frontier _ (cons _ item1 (... (nil _)))` using `EApp`, `EVar{"frontier"}`, `EVar{"cons"}`, `EVar{"nil"}`, and `EHole` for the implicit type arguments.

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestParseDoDesugarsToFrontier ./surface/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add surface/lexer.go surface/parser.go surface/parser_do_test.go
git commit -m "feat(eval): do ... end desugars to a frontier application"
```

---

### Task 3: The Go-backend seeded cooperative scheduler runtime

**Files:**
- Modify: `codegen/golang.go` (add the `frontier` runtime constant and wire `frontier` to emit a call into it; the IO runtime is at `goIORuntime` near line 708)
- Modify: `codegen/ioprims.go` (gate the frontier runtime in when a program uses `frontier`, mirroring the `usesNet`/`usesFileEnv` gates)
- Test: `codegen/frontier_go_test.go`

**Interfaces:**
- Consumes: `frontier` emitted name (Task 1); the existing `ap`, `pureIO_d`, `bindIO_d` Go runtime helpers.
- Produces: a Go runtime function `__frontier(actions []any) any` that forces each action thunk through the seeded LCG ready-pick order, returning the last result; the LCG state is a package var seeded from `WAVELET_SEED` (default 0).

- [ ] **Step 1: Write the failing test**

```go
// codegen/frontier_go_test.go
package codegen

import (
	"os/exec"
	"strings"
	"testing"
)

// A do-block of two prints runs BOTH; under seed 0 the order is fixed and
// reproducible (the conformance contract).
func TestFrontierGoRunsBothSeeded(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	out := emitAndRunGo(t, "ch_frontier_two_prints.rune", "main") // helper below
	// Both effects must appear exactly once, regardless of order.
	if strings.Count(out, "hello") != 1 || strings.Count(out, "world") != 1 {
		t.Fatalf("frontier dropped or duplicated an effect: %q", out)
	}
}
```

(Add `emitAndRunGo` as a helper in this test file if absent: load the listing via the session, `EmitProgram("main")`, `Go{}.Emit`, write to a temp `.go`, `go run` it, return stdout. Model it on the existing emit-and-run helpers in `harness/`.)

Also create the listing `listings/ch_frontier_two_prints.rune`:

```
-- two independent prints in a do-block: both run, order is seed-determined.
data Unit : U is unit : Unit end
foreign printStr : Bin -> IO Unit end
foreign Bin : U end
main : IO Unit is do (printStr b"hello\n") (printStr b"world\n") end end
```

(If `printStr` is not an existing prim, use the existing string/byte print prim from `ch483_bytes.rune` / `ioprims.go`; match its exact name and type.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestFrontierGoRunsBothSeeded ./codegen/`
Expected: FAIL (emit error: `frontier` has no Go runtime).

- [ ] **Step 3: Implement the runtime**

Add to `codegen/golang.go`:

```go
const goFrontierRuntime = `
var __schedState uint64 = func() uint64 {
	if s := os.Getenv("WAVELET_SEED"); s != "" { var v uint64; fmt.Sscan(s, &v); return v }
	return 0
}()
func __schedNext(n int) int {
	__schedState = __schedState*6364136223846793005 + 1442695040888963407
	return int(__schedState % uint64(n))
}
// __frontier forces the action thunks in a seed-determined ready order and
// returns the last forced result. Single-threaded: the observable semantics is
// the interleaving, not parallelism.
func __frontier(_A any) any { return func(actions any) any { return func(_u any) any {
	xs := __toSlice(actions) // the erased List -> []any of IO thunks
	var last any
	for len(xs) > 0 {
		i := __schedNext(len(xs))
		last = ap(xs[i], nil) // force the IO action
		xs = append(xs[:i], xs[i+1:]...)
	}
	return last
} } }
`
```

Wire `frontier` in the Go emitter to emit `__frontier`, gate `goFrontierRuntime` plus its `os`/`fmt` imports behind a `usesFrontier(p)` check in `codegen/ioprims.go` (mirror `usesNet`), and implement `__toSlice` to walk the erased cons/nil record into `[]any` (the erased list shape is the tagged constructor record the other backends already decode; reuse that decoder).

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestFrontierGoRunsBothSeeded ./codegen/`
Expected: PASS (output contains exactly one `hello` and one `world`).

- [ ] **Step 5: Commit**

```bash
git add codegen/golang.go codegen/ioprims.go codegen/frontier_go_test.go listings/ch_frontier_two_prints.rune
git commit -m "feat(eval): Go seeded cooperative frontier scheduler runtime"
```

---

### Task 4: Seed reordering is observable (the Hello/World swap)

**Files:**
- Test: `codegen/frontier_seed_test.go`

**Interfaces:**
- Consumes: the Go runtime from Task 3 and the `WAVELET_SEED` env var.

- [ ] **Step 1: Write the failing test**

```go
// codegen/frontier_seed_test.go
package codegen

import "testing"

// There EXISTS a pair of seeds that order the two prints differently.
func TestFrontierSeedReorders(t *testing.T) {
	a := emitAndRunGoSeed(t, "ch_frontier_two_prints.rune", "main", "1")
	b := emitAndRunGoSeed(t, "ch_frontier_two_prints.rune", "main", "4")
	if a == b {
		// try a small sweep before declaring no reordering
		found := false
		for _, s := range []string{"2", "3", "5", "6", "7"} {
			if emitAndRunGoSeed(t, "ch_frontier_two_prints.rune", "main", s) != a {
				found = true
				break
			}
		}
		if !found {
			t.Fatalf("no seed reordered the two prints; scheduler is not interleaving")
		}
	}
}
```

(`emitAndRunGoSeed` = `emitAndRunGo` with `cmd.Env = append(os.Environ(), "WAVELET_SEED="+seed)`.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestFrontierSeedReorders ./codegen/`
Expected: FAIL only if the scheduler ignores the seed; otherwise it should already PASS once Task 3 is correct. If it fails because both orders are equal for all seeds, the LCG wiring is wrong (fix `__schedNext`).

- [ ] **Step 3: (No new code if Task 3 is correct)**

If the test fails, the bug is in `__schedNext` or the seed plumbing; correct it so distinct seeds yield distinct ready picks.

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestFrontierSeedReorders ./codegen/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add codegen/frontier_seed_test.go
git commit -m "test(eval): seed reorders independent frontier effects"
```

---

### Task 5: `seq` inside `do` linearizes

**Files:**
- Create: `listings/ch_frontier_seq_order.rune`
- Test: `codegen/frontier_seq_test.go`

**Interfaces:**
- Consumes: the existing `seq ... end` desugar (sequential `let`) and the Task 3 runtime.

- [ ] **Step 1: Write the failing test**

```go
// codegen/frontier_seq_test.go
package codegen

import (
	"strings"
	"testing"
)

// A seq block inside do is a single ordered frontier action: hello ALWAYS
// precedes world, for every seed.
func TestSeqInsideDoIsOrdered(t *testing.T) {
	for _, seed := range []string{"0", "1", "2", "3", "7", "42"} {
		out := emitAndRunGoSeed(t, "ch_frontier_seq_order.rune", "main", seed)
		if strings.Index(out, "hello") > strings.Index(out, "world") {
			t.Fatalf("seq did not order effects under seed %s: %q", seed, out)
		}
	}
}
```

Listing `listings/ch_frontier_seq_order.rune`:

```
data Unit : U is unit : Unit end
foreign Bin : U end
foreign printStr : Bin -> IO Unit end
-- one do-item that is a seq: its two effects are internally ordered.
ordered : IO Unit is
  seq
    let a : Unit = printStr b"hello\n"
    printStr b"world\n"
  end
end
main : IO Unit is do ordered end end
```

(Match `seq`'s exact binding syntax to `parseSeqBind` / `desugarSeq`; adjust if the no-`in` form differs.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestSeqInsideDoIsOrdered ./codegen/`
Expected: PASS if `seq` already sequences correctly (this test guards the property); if it FAILS, `seq` is being flattened into the frontier (a desugar bug) and must be fixed so a `seq` block is one opaque IO action.

- [ ] **Step 3: (Fix only if failing)**

Ensure `do` collects `ordered` as ONE action (it is a single `IO Unit` value), never flattening the inner `seq`. No new code expected.

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestSeqInsideDoIsOrdered ./codegen/`
Expected: PASS for all seeds.

- [ ] **Step 5: Commit**

```bash
git add listings/ch_frontier_seq_order.rune codegen/frontier_seq_test.go
git commit -m "test(eval): seq inside do linearizes for every seed"
```

---

### Task 6: The JS backend runtime (portability + cross-backend conformance)

**Files:**
- Modify: `codegen/js.go` (add the JS `__frontier` runtime, same LCG)
- Modify: `codegen/ioprims.go` (extend the `usesFrontier` gate to JS)
- Test: `harness/frontier_conformance_test.go`

**Interfaces:**
- Produces: a JS `__frontier` with bit-identical LCG behavior to Go (same multiplier, increment, modulus via `BigInt`), so a fixed seed yields the same interleaving on both backends.

- [ ] **Step 1: Write the failing test**

```go
// harness/frontier_conformance_test.go
package harness

import "testing"

// Under a FIXED seed, Go and JS produce byte-identical output for a do-block.
func TestFrontierConformsGoJS(t *testing.T) {
	goOut := emitRunSeed(t, "go", "ch_frontier_two_prints.rune", "main", "0")
	jsOut := emitRunSeed(t, "js", "ch_frontier_two_prints.rune", "main", "0")
	if goOut != jsOut {
		t.Fatalf("frontier not byte-identical under seed 0:\n go: %q\n js: %q", goOut, jsOut)
	}
}
```

(`emitRunSeed` mirrors the existing conformance harness emit-run helpers, setting `WAVELET_SEED`.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestFrontierConformsGoJS ./harness/`
Expected: FAIL (JS has no `frontier` runtime yet).

- [ ] **Step 3: Implement the JS runtime**

Add to `codegen/js.go` a `jsFrontierRuntime` constant using `BigInt` so the 64-bit LCG matches Go exactly:

```javascript
let __schedState = (() => { const s = (typeof process !== 'undefined' && process.env.WAVELET_SEED) || "0"; return BigInt(s) & 0xFFFFFFFFFFFFFFFFn; })();
const __MASK = 0xFFFFFFFFFFFFFFFFn;
function __schedNext(n) {
  __schedState = (__schedState * 6364136223846793005n + 1442695040888963407n) & __MASK;
  return Number(__schedState % BigInt(n));
}
function __frontier(_A){ return (actions) => (_u) => {
  let xs = __toArray(actions); // erased List -> array of IO thunks
  let last;
  while (xs.length > 0) { const i = __schedNext(xs.length); last = ap(xs[i], null); xs.splice(i, 1); }
  return last;
};}
```

Gate it behind `usesFrontier` in `codegen/ioprims.go`, wire `frontier` to emit `__frontier`, and reuse the JS list decoder for `__toArray`.

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestFrontierConformsGoJS ./harness/`
Expected: PASS (identical output under seed 0).

- [ ] **Step 5: Commit**

```bash
git add codegen/js.go codegen/ioprims.go harness/frontier_conformance_test.go
git commit -m "feat(eval): JS frontier runtime, byte-identical LCG with Go"
```

---

### Task 7: Order-independence as a proof obligation (the verification tie-in)

**Files:**
- Create: `listings/ch_frontier_order_independent.rune`
- Test: `harness/frontier_order_independence_test.go`

**Interfaces:**
- Consumes: an existing verified commutative merge (reuse a CvRDT `merge` with its `mergeComm` proof from the corpus, e.g. the G-Counter from `ch72`).

- [ ] **Step 1: Write the failing test**

```go
// harness/frontier_order_independence_test.go
package harness

import "testing"

// The converged value is identical under two different seeds: the frontier
// order does not affect the result, witnessed by the listing's mergeComm proof.
func TestFrontierOrderIndependentResult(t *testing.T) {
	a := emitRunSeed(t, "go", "ch_frontier_order_independent.rune", "main", "1")
	b := emitRunSeed(t, "go", "ch_frontier_order_independent.rune", "main", "9")
	if a != b {
		t.Fatalf("merge result depended on schedule: seed1=%q seed9=%q", a, b)
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestFrontierOrderIndependentResult ./harness/`
Expected: FAIL until the listing exists.

- [ ] **Step 3: Write the listing**

`listings/ch_frontier_order_independent.rune`: two replicas update locally, the do-block merges them as two frontier effects, and the listing carries `mergeComm : (a b) -> Eq State (merge a b) (merge b a)` (copy the proven G-Counter merge and its `mergeComm` from `ch72_replicated_counter.rune`). `main` prints `value (merge replicaA replicaB)`. The proof certifies that the printed result cannot depend on the frontier's merge order; the test confirms it operationally under two seeds.

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestFrontierOrderIndependentResult ./harness/`
Expected: PASS (identical under both seeds), and the listing elaborates (so `mergeComm` checks).

- [ ] **Step 5: Commit**

```bash
git add listings/ch_frontier_order_independent.rune harness/frontier_order_independence_test.go
git commit -m "feat(eval): order-independence proof obligation, schedule-invariant merge"
```

---

### Task 8: REPL acceptance for `do`

**Files:**
- Modify: `internal/repl/repl.go` (only if `do` needs REPL wiring; the parser change should suffice)
- Test: `internal/repl/repl_do_test.go`

**Interfaces:**
- Consumes: the parser desugar (Task 2) and the session builtin (Task 1).

- [ ] **Step 1: Write the failing test**

```go
// internal/repl/repl_do_test.go
package repl

import (
	"strings"
	"testing"
)

func TestReplAcceptsDo(t *testing.T) {
	out := replEval(t, `do (printStr b"hi\n") end`) // replEval = the existing repl test helper
	if !strings.Contains(out, "hi") {
		t.Fatalf("repl did not run the do-block: %q", out)
	}
}
```

(Use the existing repl test helper; match its name and signature from `internal/repl/repl_test.go`.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestReplAcceptsDo ./internal/repl/`
Expected: FAIL or PASS depending on whether the REPL shares the surface parser. If FAIL, the REPL has a separate parse path that needs the `do` case.

- [ ] **Step 3: Wire the REPL (only if failing)**

If the REPL uses the shared `surface` parser (it should, per the architecture), no change is needed and the test passes. If it has a divergent path, add the `do` dispatch there.

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestReplAcceptsDo ./internal/repl/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add internal/repl/repl_do_test.go internal/repl/repl.go
git commit -m "test(eval): repl acceptance for do blocks"
```

---

## Self-Review

**Spec coverage (against the eval-model decisions):**
- Lazy pure core: unchanged and relied upon (Task 1 type-checks pure terms normally). Covered by the existing kernel; no task needed.
- `seq` linearizes: Task 5.
- `do` as the concurrent block: Tasks 2, 3, 6.
- M-on-N frontier with nondeterministic ordering: modeled as the seeded cooperative scheduler (Tasks 3, 4). NOTE deliberately scoped: real OS-thread parallelism is a follow-on increment; this increment delivers the observable interleaving semantics, which is what the proofs and the demo need.
- Correctness under every interleaving as a first-class proof target: Task 7 (order-independence via `mergeComm`). The fully-general all-process theorem remains open research, as the index states.
- Byte-identical conformance: Task 6 (Go and JS under a fixed seed). Remaining backends are a scoped follow-on plan.
- REPL acceptance: Task 8.

**Placeholder scan:** Two integration points reference existing code the implementer must match by name rather than reproduce blindly: the IO-group registration in `session.go` (Task 1) and the erased-list decoder / emit-run helpers (Tasks 3, 6). These are not placeholders; they are "match the existing pattern, here is the exact file and symbol." Every code step shows real code.

**Type consistency:** `frontier : (A : U) -> List (IO A) -> IO A` is used identically in Tasks 1, 2, 3, 6. The runtime helpers `__frontier`, `__schedNext`, `__schedState`, `__toSlice`/`__toArray` are named consistently across Go and JS. The LCG constants are identical in both runtimes.

**Scope honesty:** This is increment one of the eval model. Explicit follow-on plans: (1b) the `frontier` runtime on the remaining backends (Py, Rust, BEAM, JVM, C, LLVM, WASM); (1c) the data-dependency DAG inside `do` (let-induced ordering, so a consumer waits on its producer); (1d) making top-level evaluation implicitly async; (1e) real M-on-N OS-thread parallelism. None are claimed by this plan.

## Open questions

- The default seed is 0; confirm whether production wants a time-seeded default (nondeterministic by default, with `WAVELET_SEED` to pin) or a fixed default (deterministic by default). The conformance gate needs a pinned seed regardless; this only affects the un-pinned developer run.
- `printStr`/`Bin` names in the demo listings must match the existing byte-print prim; verify against `ch483_bytes.rune` before writing the listings.
