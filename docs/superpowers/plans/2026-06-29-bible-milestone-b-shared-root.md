# Bible Port — Milestone B: `build_shared_root` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add four host ops to the Go + JS backends (`jsonStrField`, the write-stream trio `openWrite`/`writeChunk`/`closeWrite`, `sortFile`, `foldDir`) and a Rune `build_shared_root` that reproduces `relations/derived/shared-root.jsonl` byte-identical to Python.

**Architecture:** New ops ride the D6 foreign-op mechanism (`codegen/ioprims.go` `ioPrims` map + per-backend bodies gated by `usesForeign`). `foldDir` is higher-order (host loop calling an erased step per file, the `foldLines`/`primSpawn` convention). Opaque file handles are carried as `any` (Go) / fd (JS), the sockConnect/sockClose precedent. The builder is a streaming 4-phase pipeline (foldDir->sortFile->foldLines group-scan->sortFile). No `core/`/`store/`/`elaborate/` change; no hash bump.

**Tech Stack:** Go (`codegen/golang.go`, the bible target), JS (`codegen/js.go`, conformance gate), `codegen/ioprims.go`, `harness/` (per-op + builder + byte-identical gates), `listings/` (ch551-555).

## Global Constraints

- Kernel FROZEN: changes ONLY in `codegen/` (golang.go, js.go, ioprims.go), `harness/`, `listings/`. No core/store/elaborate edit. No hash-format bump.
- Backends: **Go and JS only**. Do not touch py/rust/beam/jvm/C/LLVM/WASM for these ops.
- Foreign ops register in the `ioPrims` map, gate via `usesForeign(p, name)`, emit curried accessor bodies guarded by `if usesForeign(p, "<name>") {...}`. Reuse the `__s2h`/`__h2s` codec (gate via the existing `usesStream`/`streamPrims` list, extended).
- Constructor cells: Go `map[string]any{"tag":N,"name":...,"args":[]any{...}}`, JS `{tag:N,...,args:[...]}`. Polymorphic datatypes carry the erased type arg at `args[0]` (a `nil`/`null` slot). `Option`: `none`=tag 0 `args [nil]`, `some`=tag 1 `args [nil, v]` (declared `none` before `some`). `List`: `nil`=tag 0 `args [nil]`, `cons`=tag 1 `args [nil, head, tail]`.
- Opaque foreign type `Handle : U`: trivial body (Go `func Handle() any { return nil }`, JS `const Handle = () => null;`), value carried opaquely (Go `any(f)` returned, `f.(*os.File)` received; JS fd number). Mirror how `Float` is gated/emitted.
- An `IO A` value erases to a world thunk: Go `func(_u any) any` forced by `ap(action, nil)`; JS `() => A` forced by `action()`. A foreign whose type ends `IO _` takes the extra `_u` curry level; a pure foreign (`jsonStrField`) does not. A `(S:U) -> ...` foreign receives the erased type arg as the leading `nil` curry level.
- Callback ABI for an erased `S -> Bytes -> IO S` step: Go `ap(ap(ap(step, s), line), nil)`; JS `step(s)(line)()`.
- Read files `'latin1'` in JS (byte-exact with Go bytes — the Milestone-A fix). Programs using only these ops get the SYNCHRONOUS JS IO monad, so `fs.readFileSync`/`readdirSync`/`writeFileSync`/`openSync` are correct (no async).
- No em-dashes / en-dashes; use `--`. Conventional Commits; trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- No `rune check`: use `go run ./cmd/rune emit|run FILE NAME --target go|js`.
- Chapters ch551-555 are free (highest existing is ch550).

---

### Task 1: `jsonStrField` (minimal JSON field extraction, pure)

**Files:**
- Modify: `codegen/ioprims.go` (register `jsonStrField`; add it to `streamPrims`)
- Modify: `codegen/golang.go` (emit Go body)
- Modify: `codegen/js.go` (emit JS body)
- Create: `listings/ch551_json_field.rune`
- Create: `harness/bible_ops_test.go` (per-op gate scaffold; later tasks extend)

**Interfaces:**
- Produces: `jsonStrField : Bytes -> Bytes -> Option Bytes` (here `Bytes` = `Nat` packed code). Extracts a top-level string field by name; `some` its value, or `none` if absent/null.

- [ ] **Step 1: Register the op**

In `codegen/ioprims.go`, add to the `ioPrims` map:
```go
	"jsonStrField": true, // jsonStrField field doc : Nat -> Nat -> Option Nat  (top-level string field)
```
And add `"jsonStrField"` to the `streamPrims` slice (it uses the `__s2h`/`__h2s` codec):
```go
var streamPrims = []string{"foldLines", "splitOn", "byteLen", "jsonStrField"}
```

- [ ] **Step 2: Emit the Go body**

In `codegen/golang.go`, next to the `splitOn` body, add:
```go
	if usesForeign(p, "jsonStrField") {
		b.WriteString("func jsonStrField() any { return func(field any) any { return func(doc any) any { fn := __s2h(field); ds := __s2h(doc); needle := \"\\\"\" + fn + \"\\\"\"; i := strings.Index(ds, needle); none := map[string]any{\"tag\": 0, \"name\": \"none\", \"args\": []any{nil}}; if i < 0 { return none }; j := i + len(needle); for j < len(ds) && (ds[j] == ' ' || ds[j] == '\\t' || ds[j] == ':') { j++ }; if j < len(ds) && ds[j] == '\"' { j++; k := j; for k < len(ds) && ds[k] != '\"' { k++ }; return map[string]any{\"tag\": 1, \"name\": \"some\", \"args\": []any{nil, __h2s(ds[j:k])}} }; return none } } }\n")
	}
```
(No new import -- `strings` is always imported. No-escape assumption: strong/root values are ASCII alnum, no quotes/backslashes.)

- [ ] **Step 3: Emit the JS body**

In `codegen/js.go`, next to the `splitOn` body, add:
```go
	if usesForeign(p, "jsonStrField") {
		b.WriteString("const jsonStrField = () => field => doc => { const fn = __s2h(field), ds = __s2h(doc); const needle = '\"'+fn+'\"'; const none = {tag:0,name:\"none\",args:[null]}; let i = ds.indexOf(needle); if (i<0) return none; let j = i+needle.length; while (j<ds.length && (ds[j]===' '||ds[j]==='\\t'||ds[j]===':')) j++; if (ds[j]==='\"') { j++; let k=j; while(k<ds.length && ds[k]!=='\"') k++; return {tag:1,name:\"some\",args:[null,__h2s(ds.slice(j,k))]}; } return none; };\n")
	}
```

- [ ] **Step 4: Write the unit listing**

Create `listings/ch551_json_field.rune`:
```
-- Chapter 551 -- jsonStrField: extract a top-level JSON string field (Milestone B, minimal FR4).
--
-- jsonStrField field doc returns `some value` if the object has a top-level string field named
-- `field`, else `none` (absent or null). Pure foreign op over the packed-String code. Witnesses
-- (run js+go): extracting "strong" from a sample entry gives byteLen 5 ("G0026"); extracting an
-- absent field gives 0 via the none-default. Bytes/Byte are Nat; Option none is tag 0, some tag 1.

data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end
data Option : U -> U is none : (A : U) -> Option A | some : (A : U) -> A -> Option A end

foreign jsonStrField : Nat -> Nat -> Option Nat end
foreign byteLen      : Nat -> Nat end

-- byteLen of the extracted field, or 0 if none -- a runnable Nat witness.
fieldLen : Nat -> Nat -> Nat is
  fn (field : Nat) (doc : Nat) is
    OptionElim Nat (fn (w : Option Nat) is Nat end)
      zero
      (fn (v : Nat) is byteLen v end)
      (jsonStrField field doc)
  end
end

strongLen : Nat is fieldLen (codeOf "strong") (codeOf "{\"strong\": \"G0026\", \"root\": \"G0025\"}") end
missingLen : Nat is fieldLen (codeOf "nope") (codeOf "{\"strong\": \"G0026\"}") end
```

- [ ] **Step 5: Verify elaborate + witnesses (js + go)**

Run: `go run ./cmd/rune emit listings/ch551_json_field.rune strongLen --target go`
Expected: PASS (emits `jsonStrField`/`byteLen`/`__s2h`, no error, exit 0).

Run on both backends:
`go run ./cmd/rune run listings/ch551_json_field.rune strongLen --target js` -> `5`
`go run ./cmd/rune run listings/ch551_json_field.rune strongLen --target go` -> `5`
`go run ./cmd/rune run listings/ch551_json_field.rune missingLen --target js` -> `0`
`go run ./cmd/rune run listings/ch551_json_field.rune missingLen --target go` -> `0`

- [ ] **Step 6: Write the per-op gate scaffold**

Create `harness/bible_ops_test.go`:
```go
package harness

import (
	"os/exec"
	"strings"
	"testing"
)

// runBibleOp runs `rune run FILE NAME --target T` from the repo root, returns trimmed stdout.
func runBibleOp(t *testing.T, file, name, target string) string {
	t.Helper()
	cmd := exec.Command("go", "run", "./cmd/rune", "run", file, name, "--target", target)
	cmd.Dir = ".."
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("rune run %s %s --target %s failed: %v\n%s", file, name, target, err, out)
	}
	return strings.TrimSpace(string(out))
}

func TestBibleJsonStrField(t *testing.T) {
	for _, tg := range []string{"js", "go"} {
		if got := runBibleOp(t, "listings/ch551_json_field.rune", "strongLen", tg); got != "5" {
			t.Errorf("[%s] strongLen = %q, want 5", tg, got)
		}
		if got := runBibleOp(t, "listings/ch551_json_field.rune", "missingLen", tg); got != "0" {
			t.Errorf("[%s] missingLen = %q, want 0", tg, got)
		}
	}
}
```

- [ ] **Step 7: Run the gates + commit**

Run: `go test ./harness/ -run 'TestBibleJsonStrField' -count=1 -v` -> PASS.
Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch551_json_field.rune' -count=1 -v` -> PASS.

```bash
git add codegen/ioprims.go codegen/golang.go codegen/js.go listings/ch551_json_field.rune harness/bible_ops_test.go
git commit -m "$(printf 'feat(codegen): jsonStrField minimal JSON field extraction (bible Milestone B)\n\nPure foreign op extracting a top-level JSON string field over the packed code,\nGo + JS, returning Option Bytes (some/none). ch551 witnesses 5/5 and 0/0\nbyte-identical js+go (TestBibleJsonStrField). No core change, no hash bump.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 2: write-stream trio `openWrite` / `writeChunk` / `closeWrite` (+ `Handle` type)

**Files:**
- Modify: `codegen/ioprims.go` (register the three ops; add codec-using ones to `streamPrims`; add the Go `os` import gate)
- Modify: `codegen/golang.go` (emit `Handle` + the three Go bodies)
- Modify: `codegen/js.go` (emit `Handle` + the three JS bodies)
- Create: `listings/ch552_write_stream.rune`
- Modify: `harness/bible_ops_test.go` (add the write+read-back gate)

**Interfaces:**
- Produces: `openWrite : Path -> IO Handle`, `writeChunk : Handle -> Bytes -> IO Handle` (writes the chunk + a `\n`), `closeWrite : Handle -> IO Unit`. `Handle : U` is an opaque foreign type. (`Path`/`Bytes` = `Nat`.)

- [ ] **Step 1: Register the ops + gates**

In `codegen/ioprims.go`, add to `ioPrims`:
```go
	"openWrite":  true, // openWrite  path      : Nat -> IO Handle
	"writeChunk": true, // writeChunk h chunk   : Handle -> Nat -> IO Handle  (writes chunk + "\n")
	"closeWrite": true, // closeWrite h         : Handle -> IO Unit
```
Add the codec-using ones to `streamPrims` (openWrite/writeChunk use `__s2h`; closeWrite does not):
```go
var streamPrims = []string{"foldLines", "splitOn", "byteLen", "jsonStrField", "openWrite", "writeChunk"}
```
In `codegen/golang.go`, in the import section near the `foldLines` gate, add:
```go
	if usesForeign(p, "openWrite") {
		addImp("os")
	}
```

- [ ] **Step 2: Emit the Go bodies (Handle + trio)**

In `codegen/golang.go`, next to the `jsonStrField` body, add:
```go
	if usesForeign(p, "Handle") {
		b.WriteString("func Handle() any { return nil }\n")
	}
	if usesForeign(p, "openWrite") {
		b.WriteString("func openWrite() any { return func(path any) any { return func(_u any) any { f, err := os.OpenFile(__s2h(path), os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644); if err != nil { return nil }; return any(f) } } }\n")
	}
	if usesForeign(p, "writeChunk") {
		b.WriteString("func writeChunk() any { return func(h any) any { return func(c any) any { return func(_u any) any { if f, ok := h.(*os.File); ok { f.WriteString(__s2h(c)); f.WriteString(\"\\n\") }; return h } } } }\n")
	}
	if usesForeign(p, "closeWrite") {
		b.WriteString("func closeWrite() any { return func(h any) any { return func(_u any) any { if f, ok := h.(*os.File); ok { f.Close() }; return nil } } }\n")
	}
```

- [ ] **Step 3: Emit the JS bodies (Handle + trio)**

In `codegen/js.go`, next to the `jsonStrField` body, add:
```go
	if usesForeign(p, "Handle") {
		b.WriteString("const Handle = () => null;\n")
	}
	if usesForeign(p, "openWrite") {
		b.WriteString("const openWrite = () => path => () => { try { return require('fs').openSync(__s2h(path), 'w'); } catch (e) { return null; } };\n")
	}
	if usesForeign(p, "writeChunk") {
		b.WriteString("const writeChunk = () => h => c => () => { if (h !== null) require('fs').writeSync(h, __s2h(c) + '\\n'); return h; };\n")
	}
	if usesForeign(p, "closeWrite") {
		b.WriteString("const closeWrite = () => h => () => { if (h !== null) require('fs').closeSync(h); return null; };\n")
	}
```

- [ ] **Step 4: Write the listing**

Create `listings/ch552_write_stream.rune`:
```
-- Chapter 552 -- write-stream trio: openWrite/writeChunk/closeWrite (Milestone B, FR2 write path).
--
-- Open a file, write two lines (each writeChunk appends a newline), close, then read it back with
-- the Milestone-A foldLines and count the lines. Witness (run js+go): 2 lines written, foldLines
-- counts 2. Handle is an opaque foreign type. Demonstrates the write path every builder reuses.

data Nat  : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Unit : U is unit : Unit end
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end

foreign Handle     : U end
foreign openWrite  : Nat -> IO Handle end
foreign writeChunk : Handle -> Nat -> IO Handle end
foreign closeWrite : Handle -> IO Unit end
foreign foldLines  : (S : U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign printNat   : Nat -> IO Nat end

countStep : Nat -> Nat -> IO Nat is fn (acc : Nat) (line : Nat) is pureIO Nat (succ acc) end end

-- write two lines to "ch552.tmp", then read it back and count, then print the count.
main : IO Nat is
  bindIO Handle Nat (openWrite (codeOf "ch552.tmp")) (fn (h0 : Handle) is
    bindIO Handle Nat (writeChunk h0 (codeOf "alpha")) (fn (h1 : Handle) is
      bindIO Handle Nat (writeChunk h1 (codeOf "beta")) (fn (h2 : Handle) is
        bindIO Unit Nat (closeWrite h2) (fn (u : Unit) is
          bindIO Nat Nat (foldLines Nat (codeOf "ch552.tmp") countStep zero) (fn (n : Nat) is
            printNat n
          end)
        end)
      end)
    end)
  end)
end
```

- [ ] **Step 5: Verify (js + go, from a temp cwd so the relative file resolves)**

Run: `go run ./cmd/rune emit listings/ch552_write_stream.rune main --target go` -> PASS (imports include `"os"`).

Run from a writable temp dir:
```bash
( cd "$(mktemp -d)" && go run "$OLDPWD"/cmd/rune run "$OLDPWD"/listings/ch552_write_stream.rune main --target js | tail -1 )
```
Expected: `2`. Repeat with `--target go` -> `2`. (printNat double-prints, so stdout is `2\n2`; `tail -1` shows `2`.)

- [ ] **Step 6: Add the gate**

Append to `harness/bible_ops_test.go`:
```go
func TestBibleWriteStream(t *testing.T) {
	for _, tg := range []string{"js", "go"} {
		dir := t.TempDir()
		cmd := exec.Command("go", "run", "../cmd/rune", "run",
			"../listings/ch552_write_stream.rune", "main", "--target", tg)
		cmd.Dir = dir // the listing writes+reads the relative "ch552.tmp" here
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] run failed: %v\n%s", tg, err, out)
		}
		if got := strings.TrimSpace(string(out)); got != "2\n2" {
			t.Errorf("[%s] line count = %q, want 2\\n2", tg, got)
		}
	}
}
```

- [ ] **Step 7: Run the gates + commit**

Run: `go test ./harness/ -run 'TestBibleWriteStream' -count=1 -v` -> PASS.
Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch552_write_stream.rune' -count=1 -v` -> PASS.

```bash
git add codegen/ioprims.go codegen/golang.go codegen/js.go listings/ch552_write_stream.rune harness/bible_ops_test.go
git commit -m "$(printf 'feat(codegen): write-stream trio openWrite/writeChunk/closeWrite (bible Milestone B)\n\nFR2 write path: an opaque Handle foreign type + open/write/close over a host\nfile handle (Go *os.File as any; JS fd), writeChunk newline-terminates each\nchunk. ch552 writes two lines, reads them back with foldLines, counts 2\nbyte-identical js+go (TestBibleWriteStream). No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 3: `sortFile` (bytewise external line sort)

**Files:**
- Modify: `codegen/ioprims.go` (register `sortFile`; add to `streamPrims`; Go `os`/`sort` import gate)
- Modify: `codegen/golang.go` (emit Go body)
- Modify: `codegen/js.go` (emit JS body)
- Create: `listings/ch553_sort_file.rune`
- Modify: `harness/bible_ops_test.go` (add the sort gate)

**Interfaces:**
- Produces: `sortFile : Path -> Path -> IO Unit` -- read input lines, bytewise sort, write to output (LF-joined, trailing newline). The FR7 external sort.

- [ ] **Step 1: Register + gate**

In `codegen/ioprims.go`, add to `ioPrims`:
```go
	"sortFile": true, // sortFile inPath outPath : Nat -> Nat -> IO Unit  (bytewise line sort)
```
Add `"sortFile"` to `streamPrims`. In `codegen/golang.go` near the `foldLines` import gate, add:
```go
	if usesForeign(p, "sortFile") {
		addImp("os", "sort")
	}
```

- [ ] **Step 2: Emit the Go body**

In `codegen/golang.go`, next to the `closeWrite` body:
```go
	if usesForeign(p, "sortFile") {
		b.WriteString("func sortFile() any { return func(inp any) any { return func(outp any) any { return func(_u any) any { data, err := os.ReadFile(__s2h(inp)); if err != nil { os.WriteFile(__s2h(outp), []byte{}, 0644); return nil }; lines := strings.Split(string(data), \"\\n\"); if len(lines) > 0 && lines[len(lines)-1] == \"\" { lines = lines[:len(lines)-1] }; sort.Strings(lines); var sb strings.Builder; for _, ln := range lines { sb.WriteString(ln); sb.WriteByte('\\n') }; os.WriteFile(__s2h(outp), []byte(sb.String()), 0644); return nil } } } }\n")
	}
```
(`sort.Strings` is bytewise -- matches Python's tuple/string sort.)

- [ ] **Step 3: Emit the JS body**

In `codegen/js.go`, next to the `closeWrite` body:
```go
	if usesForeign(p, "sortFile") {
		b.WriteString("const sortFile = () => inp => outp => () => { const fs = require('fs'); let data; try { data = fs.readFileSync(__s2h(inp), 'latin1'); } catch (e) { fs.writeFileSync(__s2h(outp), ''); return null; } let lines = data.split('\\n'); if (lines.length && lines[lines.length-1] === '') lines.pop(); lines.sort((a,b) => a < b ? -1 : a > b ? 1 : 0); fs.writeFileSync(__s2h(outp), lines.map(l => l + '\\n').join('')); return null; };\n")
	}
```
(With `'latin1'`, each char is a byte 0-255, so the comparator is bytewise -- matches Go.)

- [ ] **Step 4: Write the listing**

Create `listings/ch553_sort_file.rune`:
```
-- Chapter 553 -- sortFile: bytewise external line sort (Milestone B, FR7 external sort).
--
-- Write three out-of-order lines, sortFile them, read the sorted file back and print its first
-- line's byte length as a witness. "apple"/"cherry"/"banana" sorted -> "apple" first (byteLen 5).
-- The external sort that scales to the 11.6M-edge miners later.

data Nat  : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Unit : U is unit : Unit end
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end

foreign Handle     : U end
foreign openWrite  : Nat -> IO Handle end
foreign writeChunk : Handle -> Nat -> IO Handle end
foreign closeWrite : Handle -> IO Unit end
foreign sortFile   : Nat -> Nat -> IO Unit end
foreign foldLines  : (S : U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign byteLen    : Nat -> Nat end
foreign printNat   : Nat -> IO Nat end

-- keep the FIRST line only (state: 0 = none yet, else that line's byteLen). First line wins.
firstStep : Nat -> Nat -> IO Nat is
  fn (acc : Nat) (line : Nat) is
    pureIO Nat (NatElim (fn (w : Nat) is Nat end) (byteLen line) (fn (k : Nat) (ih : Nat) is acc) acc)
  end
end

main : IO Nat is
  bindIO Handle Nat (openWrite (codeOf "ch553.tmp")) (fn (h0 : Handle) is
    bindIO Handle Nat (writeChunk h0 (codeOf "cherry")) (fn (h1 : Handle) is
      bindIO Handle Nat (writeChunk h1 (codeOf "apple")) (fn (h2 : Handle) is
        bindIO Handle Nat (writeChunk h2 (codeOf "banana")) (fn (h3 : Handle) is
          bindIO Unit Nat (closeWrite h3) (fn (u : Unit) is
            bindIO Unit Nat (sortFile (codeOf "ch553.tmp") (codeOf "ch553.sorted")) (fn (u2 : Unit) is
              bindIO Nat Nat (foldLines Nat (codeOf "ch553.sorted") firstStep zero) (fn (n : Nat) is
                printNat n
              end)
            end)
          end)
        end)
      end)
    end)
  end)
end
```
(NOTE: `firstStep`'s `NatElim` keeps `acc` once set -- on the first line `acc=0` so it takes `byteLen line`; subsequent lines keep `acc`. After sort the first line is "apple" -> byteLen 5.)

- [ ] **Step 5: Verify (js + go from a temp cwd)**

Run: `go run ./cmd/rune emit listings/ch553_sort_file.rune main --target go` -> PASS (imports include `"os"`, `"sort"`).
Run from a temp dir (as in Task 2 Step 5) on `--target js` and `--target go`: stdout `5\n5` (sorted first line "apple" has byteLen 5; printNat double-prints).

- [ ] **Step 6: Add the gate**

Append to `harness/bible_ops_test.go`:
```go
func TestBibleSortFile(t *testing.T) {
	for _, tg := range []string{"js", "go"} {
		dir := t.TempDir()
		cmd := exec.Command("go", "run", "../cmd/rune", "run",
			"../listings/ch553_sort_file.rune", "main", "--target", tg)
		cmd.Dir = dir
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] run failed: %v\n%s", tg, err, out)
		}
		if got := strings.TrimSpace(string(out)); got != "5\n5" {
			t.Errorf("[%s] sorted first-line byteLen = %q, want 5\\n5", tg, got)
		}
	}
}
```

- [ ] **Step 7: Run the gates + commit**

Run: `go test ./harness/ -run 'TestBibleSortFile' -count=1 -v` -> PASS.
Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch553_sort_file.rune' -count=1 -v` -> PASS.

```bash
git add codegen/ioprims.go codegen/golang.go codegen/js.go listings/ch553_sort_file.rune harness/bible_ops_test.go
git commit -m "$(printf 'feat(codegen): sortFile bytewise external line sort (bible Milestone B, FR7)\n\nRead a file, bytewise stable line sort, write to another file (LF, trailing\nnewline) -- the external sort scaling to the 11.6M-edge miners. Go sort.Strings\n/ JS latin1 comparator are both bytewise, matching Python. ch553 sorts three\nlines, reads back apple-first byteLen 5 js+go (TestBibleSortFile). No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 4: `foldDir` (higher-order fold over files in a directory)

**Files:**
- Modify: `codegen/ioprims.go` (register `foldDir`; add to `streamPrims`; Go `os`/`path/filepath` import gate)
- Modify: `codegen/golang.go` (emit Go body)
- Modify: `codegen/js.go` (emit JS body)
- Create: `listings/ch554_fold_dir.rune` + `harness/testdata/foldfix/` fixture files
- Modify: `harness/bible_ops_test.go` (add the foldDir gate)

**Interfaces:**
- Produces: `foldDir : (S:U) -> Path -> Bytes -> (S -> Bytes -> IO S) -> S -> IO S` -- recursively enumerate files under a dir whose name ends with the suffix; apply the erased step to each file's CONTENTS (read byte-exact), threading state. Higher-order.

- [ ] **Step 1: Register + gate**

In `codegen/ioprims.go`, add to `ioPrims`:
```go
	"foldDir": true, // foldDir (S:U) dir suffix step s0 : (S:U) -> Nat -> Nat -> (S -> Nat -> IO S) -> S -> IO S
```
Add `"foldDir"` to `streamPrims`. In `codegen/golang.go` near the `foldLines` import gate:
```go
	if usesForeign(p, "foldDir") {
		addImp("os", "path/filepath")
	}
```

- [ ] **Step 2: Emit the Go body**

In `codegen/golang.go`, next to the `foldLines` body:
```go
	if usesForeign(p, "foldDir") {
		b.WriteString("func foldDir() any { return func(_S any) any { return func(dir any) any { return func(suf any) any { return func(step any) any { return func(s0 any) any { return func(_u any) any { sfx := __s2h(suf); s := s0; filepath.WalkDir(__s2h(dir), func(path string, d os.DirEntry, err error) error { if err != nil || d.IsDir() { return nil }; if !strings.HasSuffix(path, sfx) { return nil }; data, e := os.ReadFile(path); if e != nil { return nil }; s = ap(ap(ap(step, s), __h2s(string(data))), nil); return nil }); return s } } } } } }\n")
	}
```
(`filepath.WalkDir` visits in deterministic lexical order; `os.DirEntry` needs no `io/fs` import. Per-file read error -> skip, matching Python's `try/except: continue`.)

- [ ] **Step 3: Emit the JS body**

In `codegen/js.go`, next to the `foldLines` body:
```go
	if usesForeign(p, "foldDir") {
		b.WriteString("const foldDir = () => _S => dir => suf => step => s0 => () => { const fs = require('fs'); const path = require('path'); const sfx = __s2h(suf); let s = s0; const walk = (dd) => { let ents; try { ents = fs.readdirSync(dd, {withFileTypes:true}); } catch (e) { return; } ents.sort((a,b) => a.name < b.name ? -1 : a.name > b.name ? 1 : 0); for (const en of ents) { const full = path.join(dd, en.name); if (en.isDirectory()) walk(full); else if (full.endsWith(sfx)) { let data; try { data = fs.readFileSync(full, 'latin1'); } catch (_) { continue; } s = step(s)(__h2s(data))(); } } }; walk(__s2h(dir)); return s; };\n")
	}
```
(Entries sorted for determinism, matching Go's lexical WalkDir order.)

- [ ] **Step 4: Create the fixture + listing**

Create three fixture files under `harness/testdata/foldfix/` (a subdir to also exercise recursion):
- `harness/testdata/foldfix/a.json` containing exactly: `{"root": "R1"}`
- `harness/testdata/foldfix/b.json` containing exactly: `{"root": "R2"}`
- `harness/testdata/foldfix/sub/c.json` containing exactly: `{"root": "R3"}`
- `harness/testdata/foldfix/ignore.txt` containing exactly: `not json`

Create `listings/ch554_fold_dir.rune`:
```
-- Chapter 554 -- foldDir: higher-order fold over files in a directory (Milestone B).
--
-- foldDir recursively visits files under a dir whose name ends with the suffix, applying a total
-- step to each file's contents. Witness (run js+go): folding "foldfix" with suffix ".json" over a
-- fixture of 3 .json files (one in a subdir) + 1 .txt counts 3 (the .txt is skipped). The data
-- plane loop lives in the host -- the Rune step stays total.

data Nat  : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end

foreign foldDir  : (S : U) -> Nat -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign printNat : Nat -> IO Nat end

countStep : Nat -> Nat -> IO Nat is fn (acc : Nat) (content : Nat) is pureIO Nat (succ acc) end end

main : IO Nat is
  bindIO Nat Nat (foldDir Nat (codeOf "foldfix") (codeOf ".json") countStep zero) (fn (n : Nat) is
    printNat n
  end)
end
```

- [ ] **Step 5: Verify (js + go from harness/testdata so "foldfix" resolves)**

Run: `go run ./cmd/rune emit listings/ch554_fold_dir.rune main --target go` -> PASS (imports include `"os"`, `"path/filepath"`).
Run from `harness/testdata`:
```bash
( cd harness/testdata && go run ../../cmd/rune run ../../listings/ch554_fold_dir.rune main --target js | tail -1 )
```
Expected `3`; same on `--target go`. (printNat double-prints -> `3\n3`.)

- [ ] **Step 6: Add the gate**

Append to `harness/bible_ops_test.go`:
```go
func TestBibleFoldDir(t *testing.T) {
	for _, tg := range []string{"js", "go"} {
		cmd := exec.Command("go", "run", "../cmd/rune", "run",
			"../listings/ch554_fold_dir.rune", "main", "--target", tg)
		cmd.Dir = "testdata" // so the listing's relative "foldfix" resolves
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] run failed: %v\n%s", tg, err, out)
		}
		if got := strings.TrimSpace(string(out)); got != "3\n3" {
			t.Errorf("[%s] .json file count = %q, want 3\\n3", tg, got)
		}
	}
}
```

- [ ] **Step 7: Run the gates + commit**

Run: `go test ./harness/ -run 'TestBibleFoldDir' -count=1 -v` -> PASS.
Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch554_fold_dir.rune' -count=1 -v` -> PASS.

```bash
git add codegen/ioprims.go codegen/golang.go codegen/js.go listings/ch554_fold_dir.rune harness/testdata/foldfix harness/bible_ops_test.go
git commit -m "$(printf 'feat(codegen): foldDir higher-order fold over a directory (bible Milestone B)\n\nRecursively visit files under a dir matching a suffix, applying an erased Rune\nstep to each file content (the foldLines/primSpawn callback convention). Go\nfilepath.WalkDir / JS recursive readdirSync, both deterministic lexical order.\nch554 folds a 3-file .json fixture (one nested, one .txt skipped) -> 3 js+go\n(TestBibleFoldDir). No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 5: `build_shared_root` builder + synthetic-fixture gate

**Files:**
- Create: `listings/ch555_build_shared_root.rune` (the 4-phase pipeline)
- Create: `harness/testdata/lexfix/` (synthetic lexicon fixture) + the expected output
- Modify: `harness/bible_ops_test.go` (add the builder gate)

**Interfaces:**
- Consumes: all four new ops + `foldLines`/`splitOn`/`byteLen` + `eqNat` (for root-group boundary detection on the packed codes).
- Produces: `main : IO Unit` -- runs the 4-phase pipeline, writing `shared-root.out` in the cwd.

> **NOTE TO IMPLEMENTER:** Unlike the op tasks, this builder cannot be pre-verified (it needs the Tasks 1-4 ops, which now exist). The code below is a REFERENCE implementation. Your acceptance is the synthetic-fixture gate (Step 4): make it produce the EXACT expected output file. Adjust the reference to the real op signatures and the rune surface (e.g. `fn ... is ... end` terminators, `BoolElim`/`OptionElim`/`ListElim` argument order) as needed; the gate is the spec. Root-group boundary detection compares the packed code of the current root to the previous via `eqNat` (Bytes IS a packed Nat). Within a sorted group the keys are ascending, so pairing `(keys[i], keys[j]) i<j` is already canonical (`src <= dst`) -- no string comparison needed.

- [ ] **Step 1: Write the builder listing**

Create `listings/ch555_build_shared_root.rune`. Reference implementation (the 4 phases; `lexDir`/intermediate paths are relative to cwd):
```
-- Chapter 555 -- build_shared_root: reproduce shared-root.jsonl byte-identical (Milestone B).
--
-- 4-phase streaming pipeline (state O(group size), no large in-language List):
--   1. foldDir the lexicon: for each entry with a non-null root, write "root<TAB>key" (key=strong).
--   2. sortFile -> groups contiguous, keys ascending within a root.
--   3. foldLines the sorted file: a group-scan emits one edge line per ascending key pair.
--   4. sortFile the edges by (src,dst) -> the byte-identical output.
-- The edge line is a constant template with only src/dst varying (rel/source/rank/etc. constant).

data Nat   : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Bool  : U is false : Bool | true : Bool end
data Unit  : U is unit : Unit end
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end
data Option : U -> U is none : (A : U) -> Option A | some : (A : U) -> A -> Option A end
data List   : U -> U is nil : (A : U) -> List A | cons : (A : U) -> A -> List A -> List A end

foreign Handle       : U end
foreign foldDir      : (S : U) -> Nat -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign foldLines    : (S : U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign splitOn      : Nat -> Nat -> List Nat end
foreign jsonStrField : Nat -> Nat -> Option Nat end
foreign openWrite    : Nat -> IO Handle end
foreign writeChunk   : Handle -> Nat -> IO Handle end
foreign closeWrite   : Handle -> IO Unit end
foreign sortFile     : Nat -> Nat -> IO Unit end

-- eqNat : decidable equality on packed codes (for root-group boundary detection).
eqNat : Nat -> Nat -> Bool is
  fn (a : Nat) is
    NatElim (fn (w : Nat) is Nat -> Bool end)
      (fn (b : Nat) is NatElim (fn (w : Nat) is Bool end) true (fn (k : Nat) (ih : Bool) is false) b end)
      (fn (k : Nat) (ih : Nat -> Bool) is
         fn (b : Nat) is NatElim (fn (w : Nat) is Bool end) false (fn (j : Nat) (ihj : Bool) is ih j) b end end)
      a
  end
end

-- headD/append helpers for the small per-group key list, plus the edge-line builder, go here.
-- (Implementer: build the "root<TAB>key" line in phase 1, and the edge JSON line in phase 3 via
-- string interpolation with the constant template -- only src/dst vary:
--   {"src": "{bytes src}", "dst": "{bytes dst}", "rel": "shared-root", "directed": false, ... }
-- The exact template MUST match Python byte-for-byte; see the spec section 6 and the fixture.)

-- main wires the 4 phases; see the spec data-flow (section 5). Writes "shared-root.out".
main : IO Unit is
  -- phase 1: foldDir lexDir ".json" extractStep (openWrite "stage1.tmp"); closeWrite
  -- phase 2: sortFile "stage1.tmp" "stage1.sorted"
  -- phase 3: foldLines "stage1.sorted" groupStep (mkSt emptyRoot nilKeys (openWrite "stage2.tmp")); flush; closeWrite
  -- phase 4: sortFile "stage2.tmp" "shared-root.out"
  pureIO Unit unit  -- REPLACE with the real wiring (this stub is a placeholder for the implementer)
end
```
**Implementer:** flesh out `main` and the helpers so the pipeline runs and the fixture gate (Step 4) passes. The constant edge-line template, byte-for-byte, is (only `src`/`dst` vary):
```
{"src": "<SRC>", "dst": "<DST>", "rel": "shared-root", "directed": false, "provenance": {"source": "strongs-root", "method": "derived"}, "rank": 65535, "note": null}
```

- [ ] **Step 2: Create the synthetic lexicon fixture**

Create under `harness/testdata/lexfix/` (entries chosen to exercise: a 2-member root group, a 3-member root group, a `root:null` skip, and a singleton root that yields no edge):
- `e1.json`: `{"strong": "G0010", "root": "R1"}`
- `e2.json`: `{"strong": "G0020", "root": "R1"}`
- `e3.json`: `{"strong": "G0005", "root": "R2"}`
- `e4.json`: `{"strong": "G0007", "root": "R2"}`
- `e5.json`: `{"strong": "G0009", "root": "R2"}`
- `e6.json`: `{"strong": "G0099", "root": null}`
- `e7.json`: `{"strong": "G0100", "root": "R3"}`

- [ ] **Step 3: Compute the expected output**

The expected edges (root groups: R1={G0010,G0020}; R2={G0005,G0007,G0009}; R3={G0100} singleton -> none):
- R1: (G0010,G0020)
- R2: (G0005,G0007), (G0005,G0009), (G0007,G0009)

Sorted by (src,dst), the expected `shared-root.out` is EXACTLY these four lines (each + `\n`, trailing newline):
```
{"src": "G0005", "dst": "G0007", "rel": "shared-root", "directed": false, "provenance": {"source": "strongs-root", "method": "derived"}, "rank": 65535, "note": null}
{"src": "G0005", "dst": "G0009", "rel": "shared-root", "directed": false, "provenance": {"source": "strongs-root", "method": "derived"}, "rank": 65535, "note": null}
{"src": "G0007", "dst": "G0009", "rel": "shared-root", "directed": false, "provenance": {"source": "strongs-root", "method": "derived"}, "rank": 65535, "note": null}
{"src": "G0010", "dst": "G0020", "rel": "shared-root", "directed": false, "provenance": {"source": "strongs-root", "method": "derived"}, "rank": 65535, "note": null}
```
Save this as `harness/testdata/lexfix_expected.jsonl` (commit it).

- [ ] **Step 4: Add the synthetic-fixture builder gate**

Append to `harness/bible_ops_test.go` (add `"os"`, `"path/filepath"` to its imports):
```go
func TestBibleBuildSharedRootFixture(t *testing.T) {
	wantBytes, err := os.ReadFile("testdata/lexfix_expected.jsonl")
	if err != nil {
		t.Fatal(err)
	}
	want := string(wantBytes)
	for _, tg := range []string{"js", "go"} {
		dir := t.TempDir()
		// copy the lexfix fixture into the run dir under "lexfix"
		copyTree(t, "testdata/lexfix", filepath.Join(dir, "lexfix"))
		listing, _ := filepath.Abs("../listings/ch555_build_shared_root.rune")
		runner, _ := filepath.Abs("../cmd/rune")
		cmd := exec.Command("go", "run", runner, "run", listing, "main", "--target", tg)
		cmd.Dir = dir // builder reads "lexfix", writes "shared-root.out" here
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("[%s] build failed: %v\n%s", tg, err, out)
		}
		got, err := os.ReadFile(filepath.Join(dir, "shared-root.out"))
		if err != nil {
			t.Fatalf("[%s] no output: %v", tg, err)
		}
		if string(got) != want {
			t.Errorf("[%s] shared-root.out mismatch:\n--- got ---\n%s\n--- want ---\n%s", tg, got, want)
		}
	}
}

// copyTree recursively copies src to dst (small fixture helper).
func copyTree(t *testing.T, src, dst string) {
	t.Helper()
	if err := os.MkdirAll(dst, 0755); err != nil {
		t.Fatal(err)
	}
	ents, err := os.ReadDir(src)
	if err != nil {
		t.Fatal(err)
	}
	for _, e := range ents {
		s := filepath.Join(src, e.Name())
		d := filepath.Join(dst, e.Name())
		if e.IsDir() {
			copyTree(t, s, d)
			continue
		}
		b, err := os.ReadFile(s)
		if err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(d, b, 0644); err != nil {
			t.Fatal(err)
		}
	}
}
```
The builder's `lexDir` constant must be `"lexfix"` for this gate (the byte-identical gate in Task 6 points it at the real lexicon via an env var or arg -- see Task 6).

- [ ] **Step 5: Iterate the builder until the gate is green**

Run: `go test ./harness/ -run 'TestBibleBuildSharedRootFixture' -count=1 -v` until PASS on both js and go. Also: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch555_build_shared_root.rune' -count=1 -v` -> PASS.

- [ ] **Step 6: Commit**

```bash
git add listings/ch555_build_shared_root.rune harness/testdata/lexfix harness/testdata/lexfix_expected.jsonl harness/bible_ops_test.go
git commit -m "$(printf 'feat(listings): ch555 build_shared_root streaming builder + fixture gate\n\nThe 4-phase pipeline (foldDir -> sortFile -> foldLines group-scan -> sortFile)\nemitting shared-root edges as the constant JSON template, src<=dst free from\nthe ascending in-group order, eqNat for group boundaries. Synthetic lexfix\nfixture -> exact 4-edge output byte-identical js+go (TestBibleBuildSharedRootFixture).\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 6: Byte-identical acceptance gate (env-gated, real lexicon)

**Files:**
- Modify: `listings/ch555_build_shared_root.rune` (make `lexDir` selectable: default `"lexfix"`, or read an env/arg pointing at the real lexicon) -- OR add a sibling `mainReal` whose `lexDir` is the real path passed via the test's cwd
- Modify: `harness/bible_ops_test.go` (add the env-gated byte-identical test)

**Interfaces:**
- Consumes: the Task-5 builder. Produces: `TestBibleSharedRootByteIdentical` -- skipped unless `BIBLE_REPO` is set; otherwise reproduces the real `shared-root.jsonl` and diffs.

- [ ] **Step 1: Make the builder point at the real lexicon under the gate**

Simplest approach (no listing change): the test sets `cmd.Dir` to a temp dir and SYMLINKS the real `lexicon` tree as `lexfix` there, so the SAME `main` (lexDir=`"lexfix"`) runs over the real data. (If symlink traversal is a concern, copy is too slow for 23k files -- prefer a symlink; `filepath.WalkDir` follows the top-level symlinked dir's entries.) Verify `foldDir` + `WalkDir` traverse a symlinked root dir; if not, instead add a `mainReal` to ch555 identical to `main` but with `lexDir = codeOf "REALLEX"` and have the test create that dir. Pick whichever the implementer verifies works; document the choice.

- [ ] **Step 2: Add the env-gated byte-identical test**

Append to `harness/bible_ops_test.go`:
```go
func TestBibleSharedRootByteIdentical(t *testing.T) {
	repo := os.Getenv("BIBLE_REPO")
	if repo == "" {
		t.Skip("set BIBLE_REPO=~/matt/bible to run the full byte-identical gate")
	}
	wantBytes, err := os.ReadFile(filepath.Join(repo, "relations/derived/shared-root.jsonl"))
	if err != nil {
		t.Fatalf("expected output not found in BIBLE_REPO: %v", err)
	}
	dir := t.TempDir()
	// expose the real lexicon as "lexfix" in the run dir (symlink; see Step 1)
	if err := os.Symlink(filepath.Join(repo, "lexicon"), filepath.Join(dir, "lexfix")); err != nil {
		t.Fatal(err)
	}
	listing, _ := filepath.Abs("../listings/ch555_build_shared_root.rune")
	runner, _ := filepath.Abs("../cmd/rune")
	cmd := exec.Command("go", "run", runner, "run", listing, "main", "--target", "go")
	cmd.Dir = dir
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("real build failed: %v\n%s", err, out)
	}
	got, err := os.ReadFile(filepath.Join(dir, "shared-root.out"))
	if err != nil {
		t.Fatal(err)
	}
	if string(got) != string(wantBytes) {
		// report line counts + first differing line for triage (do not dump 80k lines)
		gl, wl := strings.Split(string(got), "\n"), strings.Split(string(wantBytes), "\n")
		t.Errorf("byte-identical mismatch: got %d lines, want %d lines", len(gl), len(wl))
		for i := 0; i < len(gl) && i < len(wl); i++ {
			if gl[i] != wl[i] {
				t.Errorf("first diff at line %d:\n got:  %s\n want: %s", i+1, gl[i], wl[i])
				break
			}
		}
	}
}
```

- [ ] **Step 3: Run the env-gated gate against the real repo**

Run: `BIBLE_REPO="$HOME/matt/bible" go test ./harness/ -run 'TestBibleSharedRootByteIdentical' -count=1 -v -timeout 600s`
Expected: PASS -- the builder reproduces the real `shared-root.jsonl` (80,904 lines) byte-identical on the Go target. Debug any first-diff the test reports (likely a template-spacing or sort-order detail). Also confirm the skip path: `go test ./harness/ -run 'TestBibleSharedRootByteIdentical' -count=1 -v` (no env) reports SKIP.

- [ ] **Step 4: Run the full op + listings gates + commit**

Run: `go test ./harness/ -run 'TestBible' -count=1 -v` -> all `TestBible*` PASS (the byte-identical one SKIPs without env).
Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck' -count=1` -> `ok` (every listing incl ch551-555 checks).

```bash
git add listings/ch555_build_shared_root.rune harness/bible_ops_test.go
git commit -m "$(printf 'test(harness): env-gated byte-identical shared-root.jsonl acceptance gate\n\nWith BIBLE_REPO set, runs the ch555 builder over the real 23,681-file lexicon\nand diffs against the committed shared-root.jsonl (80,904 lines) on the Go\ntarget; skipped otherwise (cross-repo, like the live-service tests). Reports\nline-count + first-diff on mismatch. Milestone B acceptance.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Self-Review

**1. Spec coverage.** Spec's four ops -> Tasks 1-4 (jsonStrField / write-trio / sortFile / foldDir), each with a per-op gate (acceptance crit. 2). The streaming builder + synthetic-fixture gate -> Task 5 (acceptance crit. 3). The env-gated byte-identical gate -> Task 6 (acceptance crit. 4). No-hash-bump / Go+JS-only / no-core-edit (acceptance crit. 1) are Global Constraints + verified by each op's emit step. The listings gate (crit. 5) runs in Task 6 Step 4. The spec's determinism rules (section 6) are encoded in the constant edge template (Task 5 Step 1/3) and the bytewise `sortFile` (Task 3).

**2. Placeholder scan.** The op tasks (1-4) carry complete verbatim Go/JS bodies, listings, fixtures, and tests. Task 5's builder `main` is deliberately a REFERENCE with the wiring described + the exact template + the exact expected fixture output -- the builder genuinely cannot be pre-verified before its ops exist, so its acceptance is the byte-exact fixture gate, not verbatim code (called out in the task's NOTE, the Milestone-A precedent where implementers completed/fixed listing code against the real ops). Every other step has exact commands + expected output.

**3. Type consistency.** Op names are identical across `ioPrims`, the Go/JS bodies, the `streamPrims` list, and the `foreign` decls: `jsonStrField`/`openWrite`/`writeChunk`/`closeWrite`/`sortFile`/`foldDir`. `Handle` is a foreign type (trivial body, gated by `usesForeign(p,"Handle")`, mirroring `Float`). Import gates are op-specific (avoiding unused imports): `foldDir`->os+path/filepath, `sortFile`->os+sort, `openWrite`->os; `jsonStrField` none. The codec gate uses `usesStream` (all codec-using ops added to `streamPrims`). Option cells (`none` tag0 `[nil]`, `some` tag1 `[nil,v]`) match the ch114 declaration order and the args[0]-erased-type-arg rule. The callback ABI (Go `ap(ap(ap(step,s),x),nil)`, JS `step(s)(x)()`) matches `foldLines`. `eqNat` (Task 5) compares packed codes for the group boundary -- Bytes IS a packed Nat, so this is correct equality.
