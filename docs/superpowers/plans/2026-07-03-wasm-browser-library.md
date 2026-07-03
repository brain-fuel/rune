# WASM Browser Library Shim (6d) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A browser-consumable WASM library artifact (export ABI + generated JS glue) plus the proven G-Counter state codec, gated by a node-driven two-instance convergence test.

**Architecture:** Four tasks. (1) The codec listing: proven round-trip over an in-language byte list + thin foreign Bin boundary wrappers. (2) The WASM LibraryBackend: library-mode WAT (init + export ABI, no _start) + generated glue.js. (3) The node/wabt harness substrate + single-instance smoke gate. (4) The two-instance convergence gate (the 6f flow minus WebRTC) + docs.

**Tech Stack:** rune listings (proof), Go (`codegen/`, `harness/`), WAT, JavaScript (generated glue + node test driver), wabt npm package, node v24, wasmtime.

**Design source:** `docs/superpowers/specs/2026-07-03-wasm-webrtc-shim-design.md`.

## Global Constraints

- **Kernel frozen.** No `core/` or `store/` edits; hash 0x06 unchanged.
- **Work in a fresh git worktree off main; commit with explicit pathspec only.** Other agents work this repo concurrently.
- **App-mode WASM emission stays byte-identical.** Library mode is a new path beside it; every existing WASM/ARC/Perceus/bible/bytes gate stays green. `rune build` Go library mode untouched.
- **Proof honesty (spec Decision 4, refined):** foreign ops are permanently neutral in the kernel, so the machine-checked round-trip theorem is over an IN-LANGUAGE byte-list codec (`encodeGC : GC -> List Nat`, `decodeGC : List Nat -> GC`, `decodeGC (encodeGC s) = s` proven for all s). The `List Nat <-> Bin` boundary is two thin wrappers (a binCons fold; a binAt walk) with no proof, exercised end-to-end by the Task 3/4 runtime gates. Do not claim the theorem covers the foreign edge; the docs state the layering.
- **Ownership/ABI contract (spec Decision 3):** JS owns values returned to it; `rt_apply` consumes its arguments; `def_<name>` accessor results are borrowed (retain before keeping); Bin payload bytes at `ptr+8` for `rt_bin_len(ptr)` bytes; `init` must run before any other export.
- **Skips, not failures, on missing toolchains** (node, wabt) exactly like existing gates.
- NO em or en dashes anywhere (ASCII hyphen-minus only). Conventional Commits. `go build ./...` + `go vet ./codegen/ ./harness/` clean before each commit.

## Reference: verified anchors (main @ 626eda7)

- `codegen/artifact.go:28` `BuildSpec`, `:69` `ArtifactSet`, `:79` `LibraryBackend` interface, `:84` `ErrLibraryUnsupported`, `:114-116` the dispatch (`b.(LibraryBackend)`). Go's implementation lives in `codegen/golang_library.go`; mirror its shape.
- `cmd/rune/build.go:18-74`: `--kind library`, `--export R[:H]` parsing into `[]codegen.Export` already exist; no CLI work needed beyond the WASM target resolving.
- `codegen/wasm.go:414` `emitDefThunk`, `:429`/`:445` `wasmThunkName`/`wasmStepName` (thunk = the `$def_<name>`-style accessor; confirm the exact prefix the function produces), `:83-101` module skeleton (memory export, `$codety`, table, `_start` seeding `$UNIT` then `$rune_main`).
- Runtime functions to export (all exist, none exported): `rt_apply` (`wasm_runtime.go:155`), `rt_retain`/`rt_release` (`:637`/`:645`), `rt_mkbin`/`rt_bin_len`/`rt_bin_set`/`rt_bin_at` (`:410-422`). `$live` global exists; `rt_live` accessor may need adding (one-liner, mirror `rt_hp` at `:533`).
- G-Counter: `listings/ch72_replicated_counter.rune:125-145` (`data GC`, `value`, `merge` via GCElim). The demo program for Tasks 2-4 is a NEW small listing (do not modify ch72) that re-declares GC/value/merge in the same shape plus `bump`/`initGC` and imports nothing.
- The 12 unconditional WASI imports: `wasm_runtime.go:34-66` (names + arities for the glue's stub object; read them, generate stubs mechanically).
- Radix machinery for the codec proof: search listings for existing Nat divmod/toRadix lemmas (`grep -l "divmod\|toRadix\|base" listings/*.rune`) before proving from scratch.
- Listing numbers race with concurrent agents: pick the next free `chNNN` at implementation time (`ls listings/ | sort -V | tail -3`).

---

### Task 1: The proven codec listing (byte-list round-trip + Bin boundary wrappers)

**Files:**
- Create: `listings/chNNN_gc_codec.rune` (NNN = next free)
- Test: joins `harness/listings_test.go`'s elaborate+check gate automatically (verify the file's naming convention makes the gate pick it up; check how listings are enumerated)
- Test: `harness/wasm_library_test.go` is Task 3/4's home; Task 1 adds a `TestGCCodecRuns` cross-check that the codec RUNS correctly on js + wasm (encode then decode then value, byte-for-byte expected output)

**Interfaces:**
- Consumes: ch72's GC shape (re-declared locally), the foreign bin ops (`binEmpty/binCons/binLen/binAt` declarations as in `listings/ch483_bytes.rune`), existing Nat lemmas.
- Produces (Tasks 2-4 rely on these exact names in the demo listing): `initGC : GC`, `bump : GC -> GC` (increments slot 1; the demo's local-op), `value : GC -> Nat`, `merge : GC -> GC -> GC`, `encodeGC : GC -> List Nat`, `decodeGC : List Nat -> GC`, `gcToBin : GC -> Bin`, `gcFromBin : Bin -> GC`, theorem `codecRoundTrip : (s : GC) -> Eq GC (decodeGC (encodeGC s)) s`.

- [ ] **Step 1: Survey the proof substrate**

Run: `grep -ln "divmod\|toRadix\|natDiv\|mod" listings/*.rune | head` and read the hits. Decide the encoding on this evidence:
- PREFERRED (if divmod inverse lemmas exist or are cheap): each nat as length-prefixed little-endian base-256 digits: `encodeNat n = len(digits) :: digits`, `digitsOf` by repeated divmod-256, `natOf` the Horner fold; the inverse lemma `natOf (digitsOf n) = n` by the divmod identity.
- FALLBACK (fully acceptable; the wire is a demo counter): a SIMPLER total injective encoding whose inverse lemma is easy, e.g. interleave/pair via the existing verified pairing if one exists, or unary-length-capped... NO: keep it byte-shaped. If base-256 inverse proves heavy, use base-2 digits (booleans as 0/1 bytes; divmod-2 = parity+half, whose lemmas `add (double (half n)) (parity n) = n` are standard single-induction) with the SAME length-prefixed frame. Wire size for demo counts is fine. Record the choice + why in the listing's header comment.

The GC frame either way: `encodeGC (gc a b) = encodeNat a ++ encodeNat b`, `decodeGC` parses the first frame, then the second, rebuilding `gc`. Decode of a malformed list may return `gc 0 0` (total by default case); the theorem only constrains images of encode.

- [ ] **Step 2: Write the listing**

Structure (concrete skeleton; adjust lemma names to the chosen base):

```
-- chNNN: the G-Counter wire codec. Proven round-trip over an in-language
-- byte list; the List <-> Bin edge is a thin unproven wrapper exercised by
-- the runtime gate (foreign ops are neutral in the kernel, so the theorem
-- cannot cross them). Base choice: <record decision + why here>.

data GC : U is gc : Nat -> Nat -> GC end
data NList : U is nnil : NList | ncons : Nat -> NList -> NList end

initGC : GC is gc zero zero end
bump : GC -> GC is ... (GCElim, succ on slot 1) end
value : GC -> Nat is ... (ch72 shape) end
merge : GC -> GC -> GC is ... (ch72 shape, max pointwise) end

append : NList -> NList -> NList is ... end
encodeNat : Nat -> NList is ... (length-prefixed digits) end
decodeNatFrame : NList -> Pair Nat NList is ... (digits back to nat + rest) end
encodeGC : GC -> NList is ... end
decodeGC : NList -> GC is ... end

<inverse lemmas for the chosen base>
natRoundTrip : (n : Nat) -> Eq Nat (frameNat (encodeNat n)) n is ... end
codecRoundTrip : (s : GC) -> Eq GC (decodeGC (encodeGC s)) s is ... end

foreign binEmpty : Bin end
foreign binCons : Nat -> Bin -> Bin end
foreign binLen : Bin -> Nat end
foreign binAt : Bin -> Nat -> Nat end
gcToBin : GC -> Bin is ... (fold binCons over encodeGC; note byte order) end
gcFromBin : Bin -> GC is ... (walk binAt 0..len-1 into an NList, decodeGC) end

-- runnable scenario for the cross-backend check:
demo : Nat is value (decodeGC (encodeGC (gc (succ (succ zero)) (succ zero)))) end
```

Copy the exact foreign-declaration syntax from `listings/ch483_bytes.rune`. `Pair` (or a local two-field data) per what neighboring listings use. binCons prepends, so `gcToBin` must fold in the order that makes `gcFromBin`'s left-to-right walk reconstruct the same NList; pin this with the runtime check, not by guessing.

- [ ] **Step 3: Elaborate + check + run**

Run: `go test -run TestListingsElaborateAndCheck ./harness/` (or the exact listings gate name found in Step 1's survey)
Expected: PASS with the new listing included (the round-trip theorem checks in the kernel).

Add and run the cross-backend runtime check in `harness/wasm_library_test.go`:

```go
// TestGCCodecRuns: the codec listing's demo (encode 2,1 then decode then value)
// runs to 3 on js and wasm, and gcToBin/gcFromBin round-trips at runtime on wasm
// (the foreign edge the kernel theorem cannot cover).
func TestGCCodecRuns(t *testing.T) { ... emit chNNN main/demo on js + wasm, want "3" ... }
```

Follow `harness/bytes_test.go`'s emit-and-run idioms (js via node, wasm via runWasmListing). For the foreign-edge round-trip add a second entry `demoBin : Nat is value (gcFromBin (gcToBin (gc 2 1))) end` to the listing (literal numerals if the listing declares `builtin nat`; otherwise succ-chains) and assert "3" likewise.

Expected: PASS on both backends.

- [ ] **Step 4: Commit**

```bash
git add listings/chNNN_gc_codec.rune harness/wasm_library_test.go
git commit -m "feat(listings): proven G-Counter wire codec (round-trip theorem + Bin edge)" -- listings/chNNN_gc_codec.rune harness/wasm_library_test.go
```

---

### Task 2: The WASM LibraryBackend (library-mode WAT + generated glue.js)

**Files:**
- Create: `codegen/wasm_library.go` (mirror `codegen/golang_library.go`'s shape)
- Modify: `codegen/wasm_runtime.go` (add `rt_live` accessor if absent, one-liner beside `rt_hp`)
- Test: `codegen/wasm_library_test.go`

**Interfaces:**
- Consumes: `LibraryBackend` interface (`artifact.go:79`), `Wasm{}.Emit` internals (the emitter struct; refactor-minimal: reuse the existing module assembly, swapping the `_start` block for the library export section), `wasmThunkName`, the export list from `BuildSpec.Exports`.
- Produces: `Wasm{}` implements `LibraryBackend`. `EmitLibrary(spec BuildSpec) (ArtifactSet, error)` (match the interface's real method name/signature by reading artifact.go) returns two artifacts: `<name>.wat` and `<name>.glue.js`. The WAT exports exactly: `memory`, `init`, `rt_apply`, `rt_retain`, `rt_release`, `rt_mkbin`, `rt_bin_len`, `rt_bin_set`, `rt_bin_at`, `rt_live`, and `def_<name>` per requested export. NO `_start`. `init` seeds `$UNIT` only.

- [ ] **Step 1: Write the failing test**

```go
// codegen/wasm_library_test.go
// TestWasmLibraryArtifacts: library mode emits a WAT whose export section is
// exactly the ABI plus def_<name> per export, no _start, plus a glue.js
// containing the 12-stub WASI object and the marshalling helpers.
func TestWasmLibraryArtifacts(t *testing.T) {
	p := mustProgramFromListing(t, "chNNN_gc_codec.rune") // or build the Program the way golang_library_test does
	spec := cg.BuildSpec{Kind: cg.BuildLibrary, Exports: []cg.Export{{Rune: "merge"}, {Rune: "value"}}, ...}
	arts, err := cg.Wasm{}.EmitLibrary(spec)
	if err != nil { t.Fatal(err) }
	wat := artifactNamed(arts, ".wat")
	for _, exp := range []string{`(export "init"`, `(export "rt_apply"`, `(export "rt_mkbin"`, `(export "rt_bin_len"`, `(export "rt_bin_set"`, `(export "rt_bin_at"`, `(export "rt_retain"`, `(export "rt_release"`, `(export "rt_live"`, `(export "def_merge"`, `(export "def_value"`} {
		if !strings.Contains(wat, exp) { t.Fatalf("missing export %s", exp) }
	}
	if strings.Contains(wat, `(export "_start"`) { t.Fatal("library module must not export _start") }
	glue := artifactNamed(arts, ".glue.js")
	for _, frag := range []string{"wasi_snapshot_preview1", "fd_write", "proc_exit", "mkBin", "readBin", "function call(", "release"} {
		if !strings.Contains(glue, frag) { t.Fatalf("glue missing %s", frag) }
	}
}
```

Read `codegen/golang_library.go` + its test FIRST and mirror the real BuildSpec construction, artifact-set field names, and how the Go test obtains a `Program`. Adjust the snippet to the real API; the assertions above are the contract.

- [ ] **Step 2: Run to confirm ErrLibraryUnsupported**

Run: `go test -run TestWasmLibraryArtifacts ./codegen/`
Expected: FAIL (`ErrLibraryUnsupported "wasm"` or compile error on the missing method).

- [ ] **Step 3: Implement**

`codegen/wasm_library.go`:
- Reuse the app-mode emitter: factor `Wasm{}.Emit`'s body minimally so the module assembly up to (not including) the `_start`/`$rune_main` block is shared (a private `emitModuleCore(p) (*wasmEmitter, *strings.Builder)` split; keep app-mode output byte-identical, gated by the full existing suite).
- Library tail instead of `_start`:

```wat
  (func $init (export "init")
    (global.set $UNIT (call $rt_mkcon (i32.const 0) (i32.const <unitNameOff>) (i32.const 0))))
```

(match the exact `$UNIT` seeding instruction sequence currently inside `_start`, `wasm.go:98-100`), plus export statements for the rt_* set (WAT allows standalone `(export "rt_apply" (func $rt_apply))` lines; emit them after the runtime), plus per requested export:

```wat
  (func $export_def_<name> (export "def_<name>") (result i32) (call $<thunkName>))
```

using `wasmThunkName(<rune name>)` for the internal accessor. Validate every requested export resolves to a program definition; unknown name = a clear error.
- `rt_live` accessor in `wasm_runtime.go` if absent: `(func $rt_live (result i32) (global.get $live))`.
- Glue generation: a Go string template in `wasm_library_go`. Contents: ES module; `const WASI_STUBS = { wasi_snapshot_preview1: { fd_write: () => 0, clock_time_get: () => 0, ... all 12 from wasm_runtime.go:34-66, each returning 0 } }`; `export async function load(bytes)` (WebAssembly.instantiate(bytes, WASI_STUBS), call `init`, return a handle exposing the raw exports); `mkBin(handle, u8)` (rt_mkbin + rt_bin_set loop, returns ptr); `readBin(handle, ptr)` (rt_bin_len + Uint8Array copy from memory.buffer at ptr+8; the (ptr,len) window is ABI); `call(handle, name, ...args)` (`def_<name>()` then rt_apply chain); `retain/release` passthroughs. Header comment: the ownership contract verbatim from the spec.

- [ ] **Step 4: Run the test + wasmtime smoke + the full app-mode sweep**

Run: `go test -run TestWasmLibraryArtifacts ./codegen/`
Expected: PASS.

Add and run a wasmtime smoke (no node needed): write the WAT to a temp file, `wasmtime run --invoke init <f>.wat` exits 0 (library module parses + instantiates + init runs under a WASI host):

```go
func TestWasmLibraryInitInvokes(t *testing.T) { ... skip if wasmtime absent ... }
```

Run: `go test -run 'Wasm|WASM|ARC|Perceus' ./codegen/` and `go test ./harness/ -run 'Bytes|Bible' -timeout 20m`
Expected: PASS byte-identical (the emitModuleCore refactor must not change app-mode output).

- [ ] **Step 5: Commit**

```bash
git add codegen/wasm_library.go codegen/wasm_library_test.go codegen/wasm_runtime.go codegen/wasm.go
git commit -m "feat(codegen): WASM LibraryBackend (library-mode WAT export ABI + generated JS glue)" -- codegen/wasm_library.go codegen/wasm_library_test.go codegen/wasm_runtime.go codegen/wasm.go
```

---

### Task 3: Harness substrate (wabt via setup.sh) + single-instance smoke gate

**Files:**
- Modify: `bin/setup.sh` (a wabt section following the file's have/ok/warn idiom)
- Create: `harness/browserlib/driver.mjs` (the node test driver, committed)
- Modify: `harness/wasm_library_test.go` (from Task 1; add the gate)
- Modify: `.gitignore` if `harness/node_modules` is not already covered

**Interfaces:**
- Consumes: Task 2's artifacts (built in-test via the LibraryBackend API), Task 1's listing (`value`, `bump`, `initGC` exports), node, wabt npm package.
- Produces: `runBrowserLib(t, listing, exports, scenario string) string` helper: builds the artifacts into a temp dir, copies driver.mjs, runs `node driver.mjs <wat> <glue> <scenario>`, returns trimmed stdout. Scenario `smoke` implemented here; Task 4 adds `converge` and `converge-reversed`.

- [ ] **Step 1: setup.sh + availability probe**

Append to `bin/setup.sh` (match its idiom exactly):

```sh
# 7. Browser-library gate (6d): node assembles WAT via the wabt npm package.
echo "-- wasm browser library --"
if have node; then
  if [ -d "$(dirname "$0")/../harness/node_modules/wabt" ]; then ok "wabt npm present"
  else (cd "$(dirname "$0")/../harness" && npm install --no-save wabt@1.0.37 >/dev/null 2>&1) && ok "wabt npm installed (harness/node_modules)" || warn "npm install wabt failed - TestWasmBrowserLibrary will SKIP"
  fi
else warn "node not found - the browser-library gate SKIPS"; fi
```

Pin the wabt version that `npm view wabt version` reports at implementation time; record it in the line. Ensure `harness/node_modules` is git-ignored (check `.gitignore`; add if missing).

- [ ] **Step 2: The driver**

`harness/browserlib/driver.mjs` (committed, plain node, no deps beyond wabt):

```js
// driver.mjs <module.wat> <glue.js> <scenario>
// Assembles the WAT with wabt, loads it through the generated glue exactly as
// a browser would (stub WASI imports), and runs the named scenario, printing
// one line per assertion for the Go gate to compare.
import { readFile } from "node:fs/promises";
import { pathToFileURL } from "node:url";
const [watPath, gluePath, scenario] = process.argv.slice(2);
const wabt = await (await import("wabt")).default();
const wat = await readFile(watPath, "utf8");
const mod = wabt.parseWat("m.wat", wat);
const { buffer } = mod.toBinary({});
const glue = await import(pathToFileURL(gluePath));
const h = await glue.load(buffer);
if (scenario === "smoke") {
  // init ran in load; drive value(bump(bump(initGC))) via the ABI and print it
  const s0 = glue.call(h, "initGC");
  const s1 = glue.call(h, "bump", s0);
  const s2 = glue.call(h, "bump", s1);
  const v = glue.call(h, "value", s2);
  console.log(glue.readNat(h, v)); // expect 2
  console.log(h.exports.rt_live()); // for the Go side's baseline delta check
}
```

NOTE for the implementer: `value` returns a K_BIG nat pointer, not a Bin; the glue needs a `readNat(handle, ptr)` helper (read limbs via exported accessors OR simplest: also export `rt_big_from_long`'s inverse... the pragmatic route: add `rt_nat_to_u32(v) -> i32` to the runtime+ABI in Task 2, saturating at 2^32-1, one small WAT function; adjust Task 2's export list and test to include it). If you take that route, update the Task 2 commit rather than bolting it here; the ABI list in the docs updates too. Numbers in the demo are tiny; saturation is documented.

- [ ] **Step 3: The Go gate + helper**

```go
// harness/wasm_library_test.go
func TestWasmBrowserLibrarySmoke(t *testing.T) {
	requireBrowserLib(t) // skips: node absent, or harness/node_modules/wabt absent
	out := runBrowserLib(t, "chNNN_gc_codec.rune",
		[]string{"initGC", "bump", "value", "merge", "encodeGC", "decodeGC", "gcToBin", "gcFromBin"},
		"smoke")
	// line 1: the counter value; line 2: rt_live after the calls
	lines := strings.Split(out, "\n")
	if lines[0] != "2" { t.Fatalf("smoke value: got %q want 2", lines[0]) }
}
```

`runBrowserLib`: temp dir; build the Program from the listing (the Task-1/Task-2 path); `EmitLibrary`; write both artifacts + copy `browserlib/driver.mjs`; `exec.Command("node", "driver.mjs", wat, glue, scenario)` with `cmd.Dir` = temp dir and `NODE_PATH` pointing at `harness/node_modules` (or run node from the harness dir; verify module resolution actually finds wabt and document which).

- [ ] **Step 4: Run**

Run: `bash bin/setup.sh` (installs wabt), then `go test -run TestWasmBrowserLibrarySmoke ./harness/`
Expected: PASS printing value 2 through a genuinely browser-shaped load (wabt assembly + stub WASI imports + init + rt_apply chains).

- [ ] **Step 5: Commit**

```bash
git add bin/setup.sh harness/browserlib/driver.mjs harness/wasm_library_test.go .gitignore
git commit -m "test(harness): browser-shaped WASM library gate substrate (node + wabt) + smoke" -- bin/setup.sh harness/browserlib/driver.mjs harness/wasm_library_test.go .gitignore
```

---

### Task 4: Two-instance convergence gate + docs

**Files:**
- Modify: `harness/browserlib/driver.mjs` (scenarios `converge`, `converge-reversed`)
- Modify: `harness/wasm_library_test.go` (the gate)
- Modify: `ref_docs/wootz/R-FFI.md` (class-3 section: the shipped library ABI as its first realization), `ref_docs/wootz/R-ARC.md` (Plan-6 list: 6d landed), `docs/superpowers/plans/00-INDEX.md` (6d DONE), `docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md` (Tier A item 4 DONE)

**Interfaces:**
- Consumes: everything above.
- Produces: the 6f flow minus WebRTC proven at the ABI; docs current. 6f's remaining work = WebRTC signaling + DOM + two tabs consuming glue.js verbatim.

- [ ] **Step 1: The converge scenarios**

In `driver.mjs`:

```js
if (scenario === "converge" || scenario === "converge-reversed") {
  const A = await glue.load(buffer); // two INDEPENDENT instances
  const B = await glue.load(buffer);
  const bump = (h, s) => glue.call(h, "bump", s);
  let sa = bump(A, bump(A, glue.call(A, "initGC")));    // A: 2 bumps
  let sb = bump(B, glue.call(B, "initGC"));             // B: 1 bump
  // serialize on the owner, cross the "channel" as plain bytes, decode on the peer
  const bytesA = glue.readBin(A, glue.call(A, "gcToBin", sa));
  const bytesB = glue.readBin(B, glue.call(B, "gcToBin", sb));
  const order = scenario === "converge" ? [[B, bytesA, "sb"], [A, bytesB, "sa"]]
                                        : [[A, bytesB, "sa"], [B, bytesA, "sb"]];
  // on each peer: mkBin the received bytes, gcFromBin, merge into local state
  let merged = {};
  for (const [h, bytes, slot] of order) {
    const rb = glue.mkBin(h, bytes);
    const rs = glue.call(h, "gcFromBin", rb);
    merged[slot] = glue.call(h, "merge", slot === "sa" ? sa : sb, rs);
  }
  console.log(glue.readNat(A, glue.call(A, "value", merged.sa ?? sa)));
  console.log(glue.readNat(B, glue.call(B, "value", merged.sb ?? sb)));
}
```

(The sketch's variable threading is intentionally explicit; the implementer straightens it so BOTH instances end with the merged state and both lines print. Add rt_live prints per instance after releasing every held pointer, for the balance assertion.)

- [ ] **Step 2: The gate**

```go
func TestWasmBrowserLibraryConverge(t *testing.T) {
	requireBrowserLib(t)
	for _, sc := range []string{"converge", "converge-reversed"} {
		out := runBrowserLib(t, "chNNN_gc_codec.rune", browserExports, sc)
		lines := strings.Split(out, "\n")
		if lines[0] != "3" || lines[1] != "3" {
			t.Fatalf("%s: both instances must converge to 3, got %v", sc, lines[:2])
		}
		// lines[2],[3]: per-instance rt_live after releases == post-init baseline
	}
}
```

The baseline comparison: capture rt_live right after load (before any call) as line 0 of every scenario, or have the driver print `live-delta: N` computing it internally; either way the assertion is delta == 0 for every held-and-released pointer, with the memoized def-accessor caches documented as the constant offset (they are seeded on first call, so capture the baseline AFTER one throwaway call round, mirroring the steady-harness run-2 discipline).

Run: `go test -run TestWasmBrowserLibrary ./harness/`
Expected: PASS, both scenarios, both instances at 3, live-delta 0. The commutativity the ch72 proof guarantees, observed at the browser ABI.

- [ ] **Step 3: Docs**

- R-FFI.md class-3 section: add the shipped realization (the library ABI + glue, pointer to the spec).
- R-ARC.md Plan-6 downstream list: 6d landed (commit range).
- 00-INDEX.md: Plan 6 decomposition, 6d -> DONE with range and the one-line surface description.
- beta-remaining.md: Tier A item 4 -> DONE, item-2/3 style; note 6f's remaining surface is "WebRTC signaling + DOM + two tabs; glue.js consumed verbatim".
- Dash check on ADDED lines: `git diff --cached | grep '^+' | grep -P '[\x{2013}\x{2014}]'` empty.

- [ ] **Step 4: Full gate + commit**

Run: `go build ./... && go vet ./codegen/ ./harness/ && go test ./codegen/ && go test ./harness/ -run 'Bytes|Bible|D6|Wasm|Conformance|Backend' -timeout 30m`
Expected: all green.

```bash
git add harness/browserlib/driver.mjs harness/wasm_library_test.go ref_docs/wootz/R-FFI.md ref_docs/wootz/R-ARC.md docs/superpowers/plans/00-INDEX.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
git commit -m "test(harness): two-instance browser-ABI convergence gate; 6d docs" -- harness/browserlib/driver.mjs harness/wasm_library_test.go ref_docs/wootz/R-FFI.md ref_docs/wootz/R-ARC.md docs/superpowers/plans/00-INDEX.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
```

---

## Self-Review

**Spec coverage:** Decision 1 (passive library) -> the whole shape; Decision 2 (LibraryBackend + two artifacts) -> Task 2; Decision 3 (export ABI + ownership) -> Task 2 (+ the rt_nat_to_u32 note routes any ABI addition back into Task 2's list and docs); Decision 4 (proven codec) -> Task 1, with the foreign-edge refinement stated in Global Constraints and matching the spec's own adjustment clause; Decision 5 (node+wabt gate) -> Tasks 3-4; the spec's two-instance + order-reversal + rt_live criteria -> Task 4. Non-goals respected (no WebRTC/DOM, no import gating, no MV-Register, no other-backend library work).

**Placeholder scan:** Task 1's encoding choice is deliberately evidence-gated (survey then choose, both routes fully described with their lemma shapes); the driver sketches mark exactly what the implementer straightens (variable threading) while pinning the assertions. No TBDs.

**Type consistency:** `EmitLibrary`/BuildSpec/ArtifactSet names are anchored to artifact.go with an explicit read-the-real-API instruction; the export ABI list is identical in Task 2's test, the glue, and the docs; `runBrowserLib`/`requireBrowserLib`/`browserExports` named once and reused; chNNN threaded as a single placeholder resolved at implementation time (listing races with concurrent agents).

**Honest notes:** (1) Task 1's proof is the schedule risk; the base-2 fallback keeps it bounded. (2) The readNat/rt_nat_to_u32 decision is flagged where it arises with the instruction to fold it back into Task 2, not accrete at the edge. (3) The emitModuleCore refactor touches app-mode emission; the byte-identity sweep is the guard.
