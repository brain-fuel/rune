# NatAccel Codegen Provenance Gate Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Gate the codegen NatOp acceleration export on the DECLARING definition name (provenance), so a user def that is hash-equal to a prelude accel def no longer inherits native-arithmetic emission; the kernel/normalizer accel stays hash-keyed.

**Architecture:** `Session` gains a `natAccelDecl map[core.Hash]string` recorded at `AddBuiltinNatOp` registration time; `emitDefs` exports a `NatSpec.Ops` entry only when the hash's emitted name equals that declaring name. Hash-equal shadows conservatively lose codegen acceleration and fall back to the ordinary eliminator loop (correct output). No backend, kernel, or store change.

**Tech Stack:** Go (module `goforge.dev/rune/v3`), standard `go test`; manual CLI conformance via `go run ./cmd/rune run ... --target erl|go|js`.

## Global Constraints

- Kernel frozen: `core/`, `equality/`, `store/` are untouched. The fix lives entirely in `internal/session` (plus its tests and `PARKING-LOT.md`).
- No em dashes or en dashes anywhere in prose or comments you add; ASCII hyphen only. (Existing file text keeps whatever it has; do not introduce new ones.)
- Conventional Commits; every `git add` uses EXPLICIT pathspecs only (other agents may share this checkout; never `git add -A` or `git add .`).
- Scope test runs per task to the touched packages: `go test ./internal/session` (Task 1-3), plus `go test ./codegen` once in Task 3. Do NOT run the full harness (`go test ./...` includes `harness/`, ~16 minutes); the final task runs targeted CLI conformance only.
- End every commit message with the trailer `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`.

## Bug Mechanism (verified 2026-07-03 at HEAD 8805136)

Structurally identical datatypes and defs hash equal in the de Bruijn core (names are erased). The registration path discards the name:

- `internal/session/session.go:83` - `natAccel map[core.Hash]core.NatOp` is the accel table.
- `internal/session/session.go:745` - `AddBuiltinNatOp` registers `s.natAccel[s.refs[b.DefName]] = kind.op`; `b.DefName` is discarded.
- `internal/session/session.go:1369-1381` - `emitDefs` exports `p.Nat.Ops[name] = op` where `name` is the hash's EMITTED name via `emitNames` (latest binding wins). A user `add` hash-equal to the prelude's `addW` therefore gets `Ops["add"] = NatOpAdd`.
- `codegen/ir.go:263-287` - `accelMatch` recognizes `IApp(IApp(IGlobal "add", a), b)` and every backend emits the host's native arithmetic for it.
- Meanwhile the user's hash-equal data group emits under the LATEST binding's name (`NatElim`), which does NOT match `p.Nat.ElimName` (`WholeElim`), so the group compiles to constructor records, not native integers (`codegen/beam.go:337` and the analogous check in every backend). Native `+` on constructor records: BEAM badarith, JS `[object Object]` string concatenation, Go interface-conversion panic.

**Ctor path finding (investigated, NOT affected):** `NatSpec{Zero, Succ, ElimName}` is built at `internal/session/session.go:1261` directly from the SURFACE names of the active `builtin nat` binding (`s.nat.Zero`, `s.nat.Succ`, `s.nat.TyName+"Elim"`); there is no hash-to-name translation on that path. Backends only emit native zero/succ inside a data group matched by `d.ElimName == p.Nat.ElimName`, and in the shadow scenario that match fails (observed: the repro's ctors emitted as `{c, 0, "zero", []}` records). So the Zero/Succ ctor path cannot cross-resolve and needs no gating; only the `Ops` export does.

The kernel/normalizer accel (`natAccelTable.NatOpOf`, `internal/session/session.go:94-109`) STAYS hash-keyed: the registration-time differential gate proved the function IS that op, and hash-equal defs are the same function, so the same-hash fast path is sound in the evaluator. The bug is codegen-only.

## Verified Reproduction (run 2026-07-03, before any fix)

Repro source (`shadow_add.rune`; note it must NOT contain the substring `builtin nat`, or `cmd/rune/main.go:sourcesHaveBuiltinNat` auto-disables the prelude and hides the bug):

```
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end

add : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    case m of
    | zero -> n
    | succ k with ih -> succ ih
    end
  end
end

main : Nat is
  add (succ (succ zero)) (succ zero)
end
```

Commands and observed output (repo root):

```
$ go run ./cmd/rune emit shadow_add.rune main --target erl | tail -2
d_add() -> fun(V_m) -> fun(V_n) -> ap(ap(ap(ap(d_NatElim(), fun(V_m_) -> unit end), V_n), fun(V_k) -> fun(V_ih) -> ap(d_succ(), V_ih) end end), V_m) end end.
d_main() -> (ap(d_succ(), ap(d_succ(), d_zero())) + ap(d_succ(), d_zero())).
```

Native `+` on constructor records. Runtime divergence, each vs the correct `--no-prelude` baseline `succ (succ (succ zero))`:

```
$ go run ./cmd/rune run shadow_add.rune main --target erl
escript: exception error: an error occurred when evaluating an arithmetic expression
  in operator  +/2
     called as {c,1,"succ",[{c,1,"succ",[{c,0,"zero",[]}]}]}
               +
               {c,1,"succ",[{c,0,"zero",[]}]}

$ go run ./cmd/rune run shadow_add.rune main --target js
[object Object][object Object]

$ go run ./cmd/rune run shadow_add.rune main --target go
panic: interface conversion: interface {} is map[string]interface {}, not *big.Int
	(in main._natAdd)

$ go run ./cmd/rune run shadow_add.rune main --target erl --no-prelude
succ (succ (succ zero))
```

## File Structure

- Modify: `internal/session/session.go` - the `Session` struct (field), `resetBuiltins` (init), `AddBuiltinNatOp` (record provenance), `emitDefs` (gate the export).
- Modify: `internal/session/nataccel_test.go` - two shared source constants + four new tests appended at the end of the file.
- Modify: `PARKING-LOT.md` - rewrite the parked entry (currently lines 580-591) to record the fix.
- No other file changes. `README.md` and `CLAUDE.md` do not document this parked bug (verified by grep); leave them untouched.

---

### Task 1: RED reproduction tests in internal/session

**Files:**
- Modify: `internal/session/nataccel_test.go` (append at end of file; current file is 347 lines)

**Interfaces:**
- Consumes: existing `Session` API: `New()`, `(*Session).LoadSource(src string) ([]string, error)`, `(*Session).EmitProgram(main string) (codegen.Program, error)`; `codegen.ByTarget(name string) (Backend, bool)`; `codegen.NatSpec{Zero, Succ, ElimName string; Ops map[string]core.NatOp}`.
- Produces: test source constants `accelProviderSrc` and `accelShadowUserSrc` and tests `TestNatAccelNotExportedForHashEqualShadow`, `TestNatAccelShadowEmitsEliminatorCall` (Task 2 turns them green; Task 3 adds two more tests that reuse the constants).

- [ ] **Step 1: Append the two RED tests and their shared sources**

The test file already imports `"fmt"`, `"testing"`, `"goforge.dev/rune/v3/core"`, and `"goforge.dev/rune/v3/surface"`. Add `"strings"` and `"goforge.dev/rune/v3/codegen"` to the existing import block, then append at the end of `internal/session/nataccel_test.go`:

```go
// accelProviderSrc mimics the always-on prelude's shape (internal/prelude/
// prelude.rune): a builtin-nat datatype plus an eliminator-shaped addW carrying
// the `builtin natAdd` acceleration.
const accelProviderSrc = `
data Whole : U is
  zero : Whole
| succ : Whole -> Whole
end

builtin nat Whole zero succ

addW : Whole -> Whole -> Whole is
  fn (m : Whole) (n : Whole) is
    case m of
    | zero -> n
    | succ k with ih -> succ ih
    end
  end
end

builtin natAdd addW
`

// accelShadowUserSrc mimics a user file loaded AFTER the prelude with NO
// builtin declarations of its own: its Nat and add are structurally identical
// to Whole and addW, so they hash equal in the de Bruijn core.
const accelShadowUserSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end

add : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    case m of
    | zero -> n
    | succ k with ih -> succ ih
    end
  end
end

main : Nat is
  add (succ (succ zero)) (succ zero)
end
`

// TestNatAccelNotExportedForHashEqualShadow is the provenance RED test: a user
// def that never declared an acceleration, but is hash-equal to a registered
// accel def, must NOT appear in the codegen NatSpec.Ops export. (The kernel
// accel staying hash-keyed for it is fine and separately locked by
// TestNatAccelOnlyFiresForRegisteredHash; the codegen export is the unsound
// half, because the user's data group compiles to constructor records.)
func TestNatAccelNotExportedForHashEqualShadow(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(accelProviderSrc); err != nil {
		t.Fatalf("load provider source: %v", err)
	}
	if _, err := s.LoadSource(accelShadowUserSrc); err != nil {
		t.Fatalf("load user source: %v", err)
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatalf("EmitProgram: %v", err)
	}
	if p.Nat == nil {
		t.Fatalf("expected a NatSpec: the provider source declared a builtin nat binding")
	}
	if op, ok := p.Nat.Ops["add"]; ok {
		t.Fatalf("user def add never declared an acceleration, but codegen exported Ops[%q] = %v (cross-registered from addW by hash equality)", "add", op)
	}
}

// TestNatAccelShadowEmitsEliminatorCall pins the emitted-source shape on BEAM:
// the shadow program's main must call d_add() (the eliminator loop), never
// native arithmetic on constructor records (the badarith of the parked bug).
func TestNatAccelShadowEmitsEliminatorCall(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(accelProviderSrc); err != nil {
		t.Fatalf("load provider source: %v", err)
	}
	if _, err := s.LoadSource(accelShadowUserSrc); err != nil {
		t.Fatalf("load user source: %v", err)
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatalf("EmitProgram: %v", err)
	}
	be, ok := codegen.ByTarget("erl")
	if !ok {
		t.Fatalf("no erl backend registered")
	}
	src, err := be.Emit(p)
	if err != nil {
		t.Fatalf("Emit erl: %v", err)
	}
	var mainLine string
	for _, line := range strings.Split(string(src), "\n") {
		if strings.HasPrefix(line, "d_main()") {
			mainLine = line
			break
		}
	}
	if mainLine == "" {
		t.Fatalf("emitted BEAM source has no d_main() definition")
	}
	if strings.Contains(mainLine, "+") {
		t.Fatalf("shadow add emitted native arithmetic on constructor records (BEAM badarith): %s", mainLine)
	}
	if !strings.Contains(mainLine, "d_add()") {
		t.Fatalf("shadow add should emit an ordinary call to d_add(): %s", mainLine)
	}
}
```

- [ ] **Step 2: Run the new tests to verify they fail (RED)**

Run: `go test ./internal/session -run 'TestNatAccelNotExportedForHashEqualShadow|TestNatAccelShadowEmitsEliminatorCall' -v`

Expected: BOTH tests FAIL, with exactly these messages (verified against HEAD 8805136):

```
--- FAIL: TestNatAccelNotExportedForHashEqualShadow
    user def add never declared an acceleration, but codegen exported Ops["add"] = 1 (cross-registered from addW by hash equality)
--- FAIL: TestNatAccelShadowEmitsEliminatorCall
    shadow add emitted native arithmetic on constructor records (BEAM badarith): d_main() -> (ap(d_succ(), ap(d_succ(), d_zero())) + ap(d_succ(), d_zero())).
```

If either test PASSES here, STOP: the bug is not being reproduced and something is wrong with the environment or the test transcription.

- [ ] **Step 3: No commit yet**

Do not commit a failing test on its own (the tree must stay green per commit; other agents share this checkout). The Task 2 commit lands the tests together with the fix.

---

### Task 2: Provenance recording in Session + emitDefs export gate

**Files:**
- Modify: `internal/session/session.go:83-86` (struct field), `internal/session/session.go:166` (resetBuiltins init), `internal/session/session.go:745` (AddBuiltinNatOp), `internal/session/session.go:1362-1381` (emitDefs export)

**Interfaces:**
- Consumes: Task 1's tests (they define done).
- Produces: `Session.natAccelDecl map[core.Hash]string` (unexported; hash -> declaring def name, populated only by `AddBuiltinNatOp`); the gated `emitDefs` export rule "Ops[name] only when name == natAccelDecl[h]". Task 3 and 4 rely on this behavior; nothing else reads the map.

- [ ] **Step 1: Add the provenance field to the Session struct**

In `internal/session/session.go`, the struct currently ends its accel block like this (around line 83):

```go
	natAccel map[core.Hash]core.NatOp
	// meta is per-name surface metadata the ledger reads but the store never
	// hashes (postulate-ness + reason). Keyed by def name; absent => zero value.
	meta map[string]defMeta
```

Change it to:

```go
	natAccel map[core.Hash]core.NatOp
	// natAccelDecl records, per natAccel hash, the NAME the acceleration was
	// declared under (`builtin natAdd addW` records "addW"). The kernel accel
	// stays hash-keyed (sound: the differential gate proved the function IS
	// that op, and hash-equal defs are the same function), but the CODEGEN
	// export is provenance-gated: a hash-equal user def bound under a
	// different name must not inherit native-arithmetic emission, because its
	// data group may compile to constructor records, not native integers.
	natAccelDecl map[core.Hash]string
	// meta is per-name surface metadata the ledger reads but the store never
	// hashes (postulate-ness + reason). Keyed by def name; absent => zero value.
	meta map[string]defMeta
```

- [ ] **Step 2: Initialize the map in resetBuiltins**

In `resetBuiltins` (around line 166), change:

```go
	s.natAccel = map[core.Hash]core.NatOp{}
```

to:

```go
	s.natAccel = map[core.Hash]core.NatOp{}
	s.natAccelDecl = map[core.Hash]string{}
```

- [ ] **Step 3: Record the declaring name in AddBuiltinNatOp**

At the end of `AddBuiltinNatOp` (around line 745), change:

```go
	s.natAccel[s.refs[b.DefName]] = kind.op
	return nil
}
```

to:

```go
	h := s.refs[b.DefName]
	s.natAccel[h] = kind.op
	s.natAccelDecl[h] = b.DefName
	return nil
}
```

(The REPL's `builtin natAdd` path, `internal/repl/repl.go:207-215`, calls this same method, so it is covered with no further change.)

- [ ] **Step 4: Gate the emitDefs export on the declaring name**

In `emitDefs` (around lines 1362-1381), replace the whole export block:

```go
	// C7 / R-NUM Decision 4: thread the registered accel-op def NAMES into the
	// NatSpec so each backend can emit a call to a registered natAdd/natMul/
	// natMonus def as the host's native arithmetic (mirroring how ElimName flows
	// to the emitNat fast path). natAccel keys are def hashes; the accel def is
	// the current (unshadowed) binding, so its emit name is the plain name. Only
	// done when the builtin-nat data group itself compiles to native integers
	// (p.Nat set above) — otherwise there is no native representation to add on.
	if p.Nat != nil && len(s.natAccel) > 0 {
		ops := map[string]core.NatOp{}
		for h, op := range s.natAccel {
			name, ok := emitNames[h]
			if !ok {
				name = s.refNames[h]
			}
			if name != "" {
				ops[name] = op
			}
		}
		p.Nat.Ops = ops
	}
```

with:

```go
	// C7 / R-NUM Decision 4: thread the registered accel-op def NAMES into the
	// NatSpec so each backend can emit a call to a registered natAdd/natMul/
	// natMonus def as the host's native arithmetic (mirroring how ElimName flows
	// to the emitNat fast path). Only done when the builtin-nat data group itself
	// compiles to native integers (p.Nat set above), otherwise there is no native
	// representation to add on. PROVENANCE GATE: natAccel keys are def hashes,
	// and structurally identical defs hash equal, so a user def that shadows the
	// hash under a DIFFERENT name (e.g. a user `add` hash-equal to the prelude's
	// `addW`) would otherwise inherit the export and get native arithmetic
	// emitted onto its constructor-record data group. Export an op only when the
	// hash's emitted name IS the declaring name recorded at registration
	// (natAccelDecl); hash-equal shadows conservatively lose codegen accel and
	// fall back to the eliminator loop. The kernel/normalizer accel
	// (natAccelTable.NatOpOf) stays hash-keyed: the differential gate proved the
	// function IS that op, so the same-hash fast path is sound there.
	if p.Nat != nil && len(s.natAccel) > 0 {
		ops := map[string]core.NatOp{}
		for h, op := range s.natAccel {
			name, ok := emitNames[h]
			if !ok {
				name = s.refNames[h]
			}
			if name != "" && name == s.natAccelDecl[h] {
				ops[name] = op
			}
		}
		p.Nat.Ops = ops
	}
```

(Note the replaced comment contained an em dash; the replacement text has none. Do not reintroduce it.)

- [ ] **Step 5: Run the Task 1 tests to verify they pass (GREEN)**

Run: `go test ./internal/session -run 'TestNatAccelNotExportedForHashEqualShadow|TestNatAccelShadowEmitsEliminatorCall' -v`

Expected: both PASS.

- [ ] **Step 6: Run the whole internal/session package**

Run: `go test ./internal/session`

Expected: `ok  	goforge.dev/rune/v3/internal/session` (all pre-existing tests, including the eight `TestNatAccel*` tests in `nataccel_test.go`, stay green; takes about 4s).

- [ ] **Step 7: Commit (explicit pathspecs)**

```bash
git add internal/session/session.go internal/session/nataccel_test.go
git commit -m "fix(session): gate codegen NatOp accel export on declaring-name provenance

A user def hash-equal to a registered accel def (e.g. a shadow Nat/add
identical to the prelude Whole/addW under the always-on prelude) inherited
the builtin natAdd export, so backends emitted native arithmetic on
constructor records (BEAM badarith, JS object concat, Go panic).
AddBuiltinNatOp now records the declaring name (natAccelDecl) and emitDefs
exports an op only when the hash's emitted name equals it. The kernel
normalizer accel stays hash-keyed (sound by the differential gate).

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 3: Regression locks: declaring name keeps accel, shadowed name drops it

**Files:**
- Modify: `internal/session/nataccel_test.go` (append at end of file)

**Interfaces:**
- Consumes: `accelProviderSrc` from Task 1; `natAccelDecl` gating behavior from Task 2; `core.NatOpAdd`.
- Produces: `TestNatAccelExportKeepsDeclaringName`, `TestNatAccelShadowedDeclNameLosesExport` (nothing later consumes them; they are permanent locks).

- [ ] **Step 1: Append the two regression tests**

Append at the end of `internal/session/nataccel_test.go`:

```go
// TestNatAccelExportKeepsDeclaringName locks the positive half of the
// provenance gate: when the declaring def IS the current binding (the normal
// prelude case), its codegen acceleration export survives.
func TestNatAccelExportKeepsDeclaringName(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(accelProviderSrc); err != nil {
		t.Fatalf("load provider source: %v", err)
	}
	p, err := s.EmitProgram("")
	if err != nil {
		t.Fatalf("EmitProgram: %v", err)
	}
	if p.Nat == nil {
		t.Fatalf("expected a NatSpec: the provider source declared a builtin nat binding")
	}
	if op := p.Nat.Ops["addW"]; op != core.NatOpAdd {
		t.Fatalf("declaring def addW must keep its codegen acceleration, got %v", op)
	}
}

// TestNatAccelShadowedDeclNameLosesExport locks the second way the gate fires:
// rebinding the DECLARING NAME to a different def gives the registered hash a
// suffixed emit name (name$hash), which no longer equals the declaring name,
// so the export is dropped (conservative correctness; the old binding's calls
// fall back to the eliminator loop).
func TestNatAccelShadowedDeclNameLosesExport(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(accelProviderSrc); err != nil {
		t.Fatalf("load provider source: %v", err)
	}
	if _, err := s.LoadSource(`
addW : Whole -> Whole -> Whole is
  fn (m : Whole) (n : Whole) is
    n
  end
end
`); err != nil {
		t.Fatalf("rebind addW: %v", err)
	}
	p, err := s.EmitProgram("")
	if err != nil {
		t.Fatalf("EmitProgram: %v", err)
	}
	if p.Nat == nil {
		t.Fatalf("expected a NatSpec: the provider source declared a builtin nat binding")
	}
	if len(p.Nat.Ops) != 0 {
		t.Fatalf("rebinding addW must drop the codegen export for the old hash (suffixed emit name differs from the declaring name), got %v", p.Nat.Ops)
	}
}
```

(For the record: before the Task 2 fix, the second test failed with `got map[addW$b95e3649c076:1]`; both were verified passing with the fix applied.)

- [ ] **Step 2: Run the full nataccel test file**

Run: `go test ./internal/session -run 'TestNatAccel' -v`

Expected: all TWELVE `TestNatAccel*` tests PASS (the eight pre-existing ones: `TestNatAccelSessionWiringDifferential`, `TestNatAccelFiresLargeNoBlowup`, `TestNatAccelDoesNotFireOnNeutral`, `TestNatAccelBridgeIsRefl`, `TestNatAccelRejectsWrongOp`, `TestNatAccelRejectsIllTyped`, `TestNatAccelRequiresBuiltinNat`, `TestNatAccelOnlyFiresForRegisteredHash`; plus the four added by Tasks 1 and 3).

- [ ] **Step 3: Run the touched-package suites**

Run: `go test ./internal/session ./codegen`

Expected: both `ok` (verified with the fix applied: `internal/session` ~4s, `codegen` ~87s; `codegen` is included because the gate changes what `NatSpec.Ops` backends receive).

- [ ] **Step 4: Commit (explicit pathspecs)**

```bash
git add internal/session/nataccel_test.go
git commit -m "test(session): lock declaring-name accel export and shadow-drop regressions

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 4: CLI conformance on the repro program + PARKING-LOT entry update

**Files:**
- Modify: `PARKING-LOT.md:580-591` (the parked cross-registration entry)
- Scratch only (not committed): `/tmp/rune-nataccel-repro/shadow_add.rune`, `/tmp/rune-nataccel-repro/prelude_add.rune`

**Interfaces:**
- Consumes: the Task 2 fix (already committed).
- Produces: the updated PARKING-LOT record; nothing else.

- [ ] **Step 1: Recreate the repro programs in a scratch dir**

```bash
mkdir -p /tmp/rune-nataccel-repro
cat > /tmp/rune-nataccel-repro/shadow_add.rune <<'EOF'
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end

add : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    case m of
    | zero -> n
    | succ k with ih -> succ ih
    end
  end
end

main : Nat is
  add (succ (succ zero)) (succ zero)
end
EOF
cat > /tmp/rune-nataccel-repro/prelude_add.rune <<'EOF'
main : Whole is
  addW 2 3
end
EOF
```

(The shadow file must NOT contain the substring `builtin nat` anywhere, including comments, or the CLI auto-disables the prelude and the scenario vanishes.)

- [ ] **Step 2: Run the shadow repro on erl, go, and js; require byte-identical correct output**

Run (from the repo root; stderr carries harmless erlc unused-variable warnings, so compare the final stdout line):

```bash
for t in erl go js; do
  echo "== $t =="
  go run ./cmd/rune run /tmp/rune-nataccel-repro/shadow_add.rune main --target $t 2>&1 | tail -1
done
```

Expected (verified with the fix applied):

```
== erl ==
succ (succ (succ zero))
== go ==
succ (succ (succ zero))
== js ==
succ (succ (succ zero))
```

This matches the `--no-prelude` reference; before the fix erl crashed with badarith, js printed `[object Object][object Object]`, and go panicked. If any of the three runtimes (escript, go, node) is missing on this machine, note it and require the remaining ones; erl is the mandatory one (it is the backend named by the parked bug).

- [ ] **Step 3: Verify the prelude's own accel still fires end to end**

```bash
go run ./cmd/rune emit /tmp/rune-nataccel-repro/prelude_add.rune main --target erl 2>/dev/null | grep '^d_main'
go run ./cmd/rune run /tmp/rune-nataccel-repro/prelude_add.rune main --target erl 2>&1 | tail -1
```

Expected (verified):

```
d_main() -> (2 + 3).
5
```

Native arithmetic retained for the declaring binding; the gate is conservative, not destructive.

- [ ] **Step 4: Rewrite the PARKING-LOT entry**

In `PARKING-LOT.md`, replace the entry at lines 580-591:

```markdown
- **`rune run` builtin-accel cross-registration via the always-on prelude.**
  Structurally identical datatypes hash equal in de Bruijn core, so a user file
  that declares its own `data Nat is zero | succ end` (WITHOUT `builtin nat`)
  plus an eliminator-shaped `add` identical to the prelude's `addW` gets the
  prelude's `builtin natAdd` acceleration cross-registered onto it under the
  always-on prelude, changing emitted arithmetic (BEAM emits native `+` on
  constructor records). `rune build`/`rune deploy` avoid it via demand-driven
  prelude loading (`sourcesNeedPrelude`); the `rune run`/`rune emit` path can
  still reach it. Parked: no current run-path consumer hits it (files that do
  succ-arithmetic declare `builtin nat`, which auto-disables the prelude), and
  the delivery-path risk (build/deploy) is closed. A future fix gates NatOp
  registration on the declaring session source rather than the hash.
```

with:

```markdown
- **`rune run` builtin-accel cross-registration via the always-on prelude
  (FIXED: declaring-name provenance gate).** Structurally identical datatypes
  and defs hash equal in de Bruijn core, so a user file declaring its own
  `data Nat is zero | succ end` (WITHOUT `builtin nat`) plus an
  eliminator-shaped `add` identical to the prelude's `addW` used to get the
  prelude's `builtin natAdd` acceleration cross-registered onto it under the
  always-on prelude: codegen emitted native arithmetic on the user's
  constructor-record data group (BEAM badarith, JS `[object Object]` concat,
  Go interface-conversion panic). Fixed in internal/session: `AddBuiltinNatOp`
  records the declaring def name (`natAccelDecl`) and `emitDefs` exports a
  `NatSpec.Ops` entry only when the hash's emitted name equals it, so
  hash-equal shadows conservatively fall back to the eliminator loop
  (nataccel_test.go locks both halves). The kernel/normalizer accel stays
  hash-keyed, sound by the registration-time differential gate. History: the
  delivery path (`rune build`/`rune deploy`) had already been closed by
  demand-driven prelude loading (`sourcesNeedPrelude`); that mechanism stays,
  and the provenance gate closes the remaining `rune run`/`rune emit` reach.
```

- [ ] **Step 5: Confirm no other doc mentions the parked bug**

Run: `grep -rn "cross-registration" README.md CLAUDE.md docs/ | grep -v superpowers`

Expected: no output (verified at plan time); if something appears, update it to match the PARKING-LOT record.

- [ ] **Step 6: Commit (explicit pathspec)**

```bash
git add PARKING-LOT.md
git commit -m "docs(parking-lot): record the nataccel provenance-gate fix

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

- [ ] **Step 7: Clean up scratch**

```bash
rm -rf /tmp/rune-nataccel-repro
```

Do NOT run the full harness here (~16 minutes); the targeted conformance in Steps 2-3 plus Task 3's package suites are the acceptance gate for this fix. Leave merging/tagging to the repo's branch-finish flow.
