## Task 9 Report: data/foreign/builtin inside module blocks

### Status: COMPLETE

**Commit:** eaa0516

### What was implemented

**1. parseModule uses parseItem (surface/parser.go)**

`parseModule` now calls `p.parseItem()` (dispatches `tForeign`/`tData`/`tBuiltin`/`tMutual`) instead of `p.parseDef()`. Return type changed from `[]Def` to `[]Item`. A two-pass approach qualifies all declaration-level names: first pass collects locally-defined names; second pass applies `prefix + name` to `Def.Name`, `DataDef.Name` + every `Ctor.Name`, `DefGroup`/`DataGroup` member names, and `qualLocal()` to `BuiltinNat`/`NatOp`/`NumInj` reference fields. Body expressions are intentionally NOT rewritten here (the session self-import resolves them).

`ParseProgram` loop already worked with `for _, d := range mod { items = append(items, d) }` since `Item` is the element type -- no change needed there.

**2. Self-import for in-module references (internal/session/session.go)**

After building `local` and creating `sc`, `LoadSource` now scans all locally-defined names for dotted forms and pre-inserts the module prefixes into `sc.imports`. This means `unqualified` references in module bodies (e.g. `on` in `lit : Flag is on end`) resolve to `M.on` via the import machinery without requiring the user to write `M.on`.

**3. primName function (codegen/ioprims.go)**

Added `func primName(n string) string` that returns the last segment after the last `.`. Updated `usesForeign` to compare `primName(x.Name) == name`. This allows `Std.Float.getFloat` to be recognized as the `getFloat` prim.

**4. Backend primName wiring (all 9 backends)**

All backends updated to use `primName(x.Name)` when emitting `IForeign`/`CForeign` calls:
- Source backends (js/py/go/beam/rust/jvm): `IForeign` emission uses `primName` so the call targets the unqualified prim body function.
- `closure.go`: `IForeign -> CForeign` conversion applies `primName` -- this fixes C, LLVM, and WASM in one place.
- `ll.go foreignNames`: collects `primName(x.Name)` for LLVM `declare` statements.
- `wasm.go wasmCheckSupported`: checks `wasmSupportedForeign[primName(x.Name)]`.
- `beam.go usesOTP`: checks `otpPrims[primName(x.Name)]`.

### Tests

- `surface/parser_module_test.go`: 2 tests -- data+foreign+def all qualify names; partial def inside module qualifies.
- `internal/session/imports_test.go`: 3 new tests -- self-import resolution, foreign inside module elaborates, data+foreign mutual self-import.
- `codegen/ioprims_test.go`: `TestPrimName` (unit), `TestUsesForeignDottedName` (gate).

### Test results

```
ok  goforge.dev/rune/v3/surface             0.005s
ok  goforge.dev/rune/v3/internal/session    0.037s
ok  goforge.dev/rune/v3/codegen             0.005s
ok  goforge.dev/rune/v3/harness (TestListingsElaborateAndCheck) 64s
```

### Concerns

None. All design decisions were clear from the brief. The self-import approach (pre-scanning `local` for dotted names) is clean and requires no new AST types. The `primName` fix is backward-compatible (unqualified names are unchanged).

---

## Fix note (review of eaa0516)

**Commit:** fix(codegen): dotted-foreign e2e run test, prim collision guard, raw-name walk sweep

**Status:** COMPLETE

### Fix 1: end-to-end run test

Added `listings/ch568_dotted_foreign_run.rune` -- a small program with `module Std.Demo is foreign printNat : Nat -> IO Nat end end` plus the Nat/builtin header, main calls `Std.Demo.printNat 42`. Added `TestDottedForeignRunJS` in `harness/io_os_test.go` that emits and runs under node, asserts "42\n42". Skips cleanly if node absent.

### Fix 2: prim collision guard

- Updated doc comment on `primName` (codegen/ioprims.go) stating the last-segment invariant and the collision risk.
- Added `CheckPrimCollisions(p Program) error` (codegen/ioprims.go): walks all defs, detects two distinct qualified foreign names sharing a primName that is in ioPrims, returns a clear error.
- Choke point: called from `EmitProgram` in `internal/session/session.go` immediately after `emitDefs()`. This is the single entry point that all backends go through before Emit.
- Added `TestPrimCollisionGuard` in `codegen/ioprims_test.go` covering: collision detected, same full name twice is ok, distinct segments are ok, unknown segment not flagged.

### Fix 3: liftBody raw-name walk

Found a latent bug in `codegen/closure.go` `liftBody` (line 558): `IForeign` inside a lifted lambda was forwarded as `CForeign{Name: x.Name}` without `primName`. The `convert` path (line 437) already used `primName`; `liftBody` did not. Fixed to `CForeign{Name: primName(x.Name)}` with a comment. This affects the C/LLVM native backends where `cName("Std.Demo.printNat")` would emit `Std_Demo_printNat()` instead of `printNat()`.

Extended `TestUsesForeignDottedName` to also verify `usesForeign(prog, "printNat")` with a `Std.Demo.printNat` IForeign (dotted name via usesFileEnv/usesStream all go through usesForeign which was already correct; the liftBody fix is the concrete raw-name walk found).

No other raw-name walks exist: all uses* helpers go through `usesForeign` (which uses primName); `usesOTP` in beam.go was already patched in eaa0516; wasm.go `wasmCheckSupported` uses primName; the only uncovered site was liftBody.

### Fix 4: builtin-in-module parser test

Added `TestModuleBuiltinNatQualifiesRefs` in `surface/parser_module_test.go`: a module with `data Nat` + `builtin nat Nat zero succ` inside produces a `BuiltinNat` whose TyName/Zero/Succ are all qualified (`M.Nat`, `M.zero`, `M.succ`) by the `qualLocal` rule in `parseModule`.

### Test results

```
ok  goforge.dev/rune/v3/codegen             (TestPrimName/TestUsesForeignDottedName/TestPrimCollisionGuard)
ok  goforge.dev/rune/v3/surface             (TestModuleBuiltinNatQualifiesRefs + existing)
ok  goforge.dev/rune/v3/internal/session    (existing module tests)
ok  goforge.dev/rune/v3/harness             (TestDottedForeignRunJS + TestListingsElaborateAndCheck)
```
