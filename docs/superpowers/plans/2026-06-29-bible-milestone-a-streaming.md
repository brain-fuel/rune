# Bible Port — Milestone A: Streaming Foundation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add three host/foreign IO ops to the Go + JS codegen backends — `foldLines` (higher-order streaming fold), `splitOn`, `byteLen` — and prove them end to end with a CoNLL-U token counter that streams a file and prints the count, byte-identical on both backends.

**Architecture:** The three ops ride the existing D6 foreign-op mechanism (`codegen/ioprims.go` `ioPrims` map + per-backend body emission, gated by `usesForeign`). `foldLines` is higher-order: its host body calls back into an erased Rune step closure once per line, using the exact convention the existing `primSpawn` uses (`ap(ap(step,s),line)` then force in Go; `step(s)(line)()` in JS). `splitOn`/`byteLen` are pure first-order ops mirroring `readFileCode`. No `core/`/`store/`/`elaborate/` change; no hash bump.

**Tech Stack:** Go (`codegen/golang.go`, the bible port's real target), JS (`codegen/js.go`, fast conformance gate), `codegen/ioprims.go` (the prim registry + gating), `harness/` (conformance tests + fixtures), `listings/` (the demo + unit listings, kept under `TestListingsElaborateAndCheck`).

## Global Constraints

- Kernel FROZEN: changes live ONLY in `codegen/` (golang.go, js.go, ioprims.go), `harness/`, and `listings/`. No `core/`, `store/`, `elaborate/` edit. No hash-format bump.
- Backends this milestone: **Go and JS only**. Do not touch py/rust/beam/jvm/C/LLVM/WASM emitters for these ops.
- Foreign ops are registered in the `ioPrims` map and gated by `usesForeign(p, name)`; bodies are emitted as curried accessor funcs, mirroring the existing prims quoted in each task.
- The packed-String codec `__s2h`/`__h2s` (a base-256 bignum with a `1` sentinel; empty string = `big.NewInt(1)`/`1n`) is the marshalling boundary — host bodies use it and never touch the constructor encoding.
- Constructor cell shape: Go `map[string]any{"tag": N, "name": "...", "args": []any{...}}`; JS `{tag: N, name: "...", args: [...]}`. Tags are 0-indexed by declaration order. For `data List : U -> U is nil ... | cons ... end`, `nil` = tag 0, `cons` = tag 1 — LOAD-BEARING: the host `splitOn` body hardcodes these, so the listing MUST declare `nil` before `cons`.
- An `IO A` value erases to a deferred world thunk: Go `func(_u any) any` forced by `ap(action, nil)`; JS `() => A` forced by `action()`. A foreign whose type ends in `IO _` therefore takes one extra `_u` curry level; a pure foreign does not.
- A foreign whose type begins `(S : U) -> ...` receives the erased type argument as a unit (nil) in the leading curry position — see `primSpawn`'s `_M`.
- No em-dashes / en-dashes anywhere; use `--`.
- Conventional Commits; trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- There is NO `rune check`. Use `go run ./cmd/rune emit FILE NAME --target go|js` to elaborate+check+emit, and `go run ./cmd/rune run FILE NAME --target go|js` to execute.
- Chapters ch548 and ch549 are free (highest existing listing is ch547).

---

### Task 1: `byteLen` + `splitOn` (pure first-order ops, Go + JS)

**Files:**
- Modify: `codegen/ioprims.go` (register the three names + add the `usesStream` gate helper)
- Modify: `codegen/golang.go` (emit `byteLen`/`splitOn` Go bodies; gate the codec on `usesStream`)
- Modify: `codegen/js.go` (emit `byteLen`/`splitOn` JS bodies; gate the codec on `usesStream`)
- Create: `listings/ch548_string_split.rune` (unit listing exercising both ops)
- Create: `harness/io_stream_test.go` (conformance gate; Task 2 extends it)

**Interfaces:**
- Consumes: the existing `__s2h`/`__h2s` codec, `usesForeign`, the `ioPrims` map.
- Produces: foreign ops `byteLen : Bytes -> Nat` and `splitOn : Byte -> Bytes -> List Bytes` (here `Bytes`/`Byte` = `Nat`), the `usesStream(p Program) bool` helper, and the conformance-test scaffold `harness/io_stream_test.go`. Task 2 adds `foldLines` to the same `ioPrims` map and reuses `usesStream` for the codec gate.

- [ ] **Step 1: Register the three op names and add the `usesStream` gate**

In `codegen/ioprims.go`, add three entries to the `ioPrims` map (alongside the existing entries):

```go
	"foldLines": true, // foldLines (S:U) path step s0 : (S:U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S
	"splitOn":   true, // splitOn   sep line          : Nat -> Nat -> List Nat   (split packed line on a byte)
	"byteLen":   true, // byteLen   line              : Nat -> Nat               (byte length of a packed line)
```

Then add, near the other `uses*` helpers in `codegen/ioprims.go`:

```go
// streamPrims need the __s2h/__h2s String codec; foldLines additionally needs os+bufio.
var streamPrims = []string{"foldLines", "splitOn", "byteLen"}

func usesStream(p Program) bool {
	for _, n := range streamPrims {
		if usesForeign(p, n) {
			return true
		}
	}
	return false
}
```

- [ ] **Step 2: Gate the Go codec on `usesStream` and emit the Go bodies**

In `codegen/golang.go`, find the codec gate at line 176 (`if usesFileEnv(p) || usesLiveKV(p) {`) and add the stream condition:

```go
	if usesFileEnv(p) || usesLiveKV(p) || usesStream(p) {
```

Then, in the prim-body emission section (near the `readFileCode` body around line 198), add two guarded blocks:

```go
	if usesForeign(p, "byteLen") {
		b.WriteString("func byteLen() any { return func(c any) any { return big.NewInt(int64(len(__s2h(c)))) } }\n")
	}
	if usesForeign(p, "splitOn") {
		b.WriteString("func splitOn() any { return func(sep any) any { return func(c any) any { sb := byte(new(big.Int).Set(sep.(*big.Int)).Int64()); parts := strings.Split(__s2h(c), string([]byte{sb})); lst := any(map[string]any{\"tag\": 0, \"name\": \"nil\", \"args\": []any{}}); for i := len(parts) - 1; i >= 0; i-- { lst = map[string]any{\"tag\": 1, \"name\": \"cons\", \"args\": []any{__h2s(parts[i]), lst}} }; return lst } } }\n")
	}
```

(`strings` and `math/big` are always imported, so `byteLen`/`splitOn` need no new import.)

- [ ] **Step 3: Gate the JS codec on `usesStream` and emit the JS bodies**

In `codegen/js.go`, the gate guarding the `__s2h`/`__h2s` emission is at js.go:130 (`if usesFileEnv(p) || usesLiveKV(p) {`, just above the two codec `b.WriteString` lines at 133-134). OR `usesStream(p)` into it, mirroring the Go change:

```go
	if usesFileEnv(p) || usesLiveKV(p) || usesStream(p) {
```

Then, near the `readFileCode` JS body (around js.go:140), add:

```go
	if usesForeign(p, "byteLen") {
		b.WriteString("const byteLen = () => c => BigInt(__s2h(c).length);\n")
	}
	if usesForeign(p, "splitOn") {
		b.WriteString("const splitOn = () => sep => c => { const parts = __s2h(c).split(globalThis.String.fromCharCode(Number(sep))); let lst = {tag:0,name:\"nil\",args:[]}; for (let i = parts.length-1; i>=0; i--) lst = {tag:1,name:\"cons\",args:[__h2s(parts[i]), lst]}; return lst; };\n")
	}
```

- [ ] **Step 4: Write the unit listing**

Create `listings/ch548_string_split.rune` with EXACTLY this content:

```
-- Chapter 548 -- splitOn / byteLen host ops (Milestone A, FR3 slice).
--
-- splitOn splits a packed line on a byte into a List of packed parts; byteLen is the byte
-- length of a packed line. Both are PURE foreign ops over the packed-String code (a Nat),
-- marshalled by the host __s2h/__h2s codec. The witnesses run cross-backend (js + go):
-- splitting "a,b,c" on ',' (byte 44) yields 3 parts, and byteLen "abc" = 3.
--
-- (The comma separator dodges any lexer tab-escape question; the real CoNLL-U fixture in
-- ch549 exercises an actual tab read from a file. Bytes/Byte are Nat; List nil is tag 0,
-- cons is tag 1 -- declaration order is load-bearing: the host splitOn body builds those.)

data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data List : U -> U is nil : (A : U) -> List A | cons : (A : U) -> A -> List A -> List A end

-- Bytes/String/codeOf are NOT prelude-ambient: the "..." literal sugar desugars to
-- `bytes <packed-numeral>`, so the `bytes` constructor + codeOf must be in scope (as ch215).
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end

-- the three Milestone-A foreign ops (byteLen + splitOn used here; foldLines declared in ch549).
foreign splitOn : Nat -> Nat -> List Nat end
foreign byteLen : Nat -> Nat end

-- length of a List Nat, to turn "number of parts" into a runnable Nat witness.
llen : List Nat -> Nat is
  fn (xs : List Nat) is
    ListElim Nat (fn (w : List Nat) is Nat end)
      zero
      (fn (h : Nat) (t : List Nat) (ih : Nat) is succ ih end)
      xs
  end
end

-- codeOf extracts the packed Nat code of a String literal (declared above, as ch215).
partsLen : Nat is llen (splitOn 44 (codeOf "a,b,c")) end
firstLen : Nat is byteLen (codeOf "abc") end
```

- [ ] **Step 5: Verify it elaborates + checks**

Run: `go run ./cmd/rune emit listings/ch548_string_split.rune partsLen --target go`
Expected: PASS -- emits Go source defining `splitOn`/`byteLen`/`__s2h`/`__h2s`, no `expected ... got ...`, no `not in scope`, exit 0.

- [ ] **Step 6: Verify the witnesses run on both backends**

Run: `go run ./cmd/rune run listings/ch548_string_split.rune partsLen --target js`
Expected: `3`

Run: `go run ./cmd/rune run listings/ch548_string_split.rune partsLen --target go`
Expected: `3`

Run: `go run ./cmd/rune run listings/ch548_string_split.rune firstLen --target js`
Expected: `3`

Run: `go run ./cmd/rune run listings/ch548_string_split.rune firstLen --target go`
Expected: `3`

- [ ] **Step 7: Write the conformance test scaffold**

Create `harness/io_stream_test.go` with:

```go
package harness

import (
	"os/exec"
	"strings"
	"testing"
)

// runRune runs `rune run FILE NAME --target T` from the repo root and returns trimmed stdout.
func runRune(t *testing.T, file, name, target string) string {
	t.Helper()
	cmd := exec.Command("go", "run", "./cmd/rune", "run", file, name, "--target", target)
	cmd.Dir = ".."
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("rune run %s %s --target %s failed: %v\n%s", file, name, target, err, out)
	}
	return strings.TrimSpace(string(out))
}

func TestStreamSplitOn(t *testing.T) {
	for _, target := range []string{"js", "go"} {
		if got := runRune(t, "listings/ch548_string_split.rune", "partsLen", target); got != "3" {
			t.Errorf("[%s] partsLen = %q, want 3", target, got)
		}
		if got := runRune(t, "listings/ch548_string_split.rune", "firstLen", target); got != "3" {
			t.Errorf("[%s] firstLen = %q, want 3", target, got)
		}
	}
}
```

- [ ] **Step 8: Run the conformance test + the listings gate**

Run: `go test ./harness/ -run 'TestStreamSplitOn' -count=1 -v`
Expected: `--- PASS: TestStreamSplitOn`.

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch548_string_split.rune' -count=1 -v`
Expected: `--- PASS: TestListingsElaborateAndCheck/ch548_string_split.rune`.

- [ ] **Step 9: Commit**

```bash
git add codegen/ioprims.go codegen/golang.go codegen/js.go listings/ch548_string_split.rune harness/io_stream_test.go
git commit -m "$(printf 'feat(codegen): byteLen + splitOn host ops (bible Milestone A, FR3)\n\nTwo pure foreign ops over the packed-String code on Go + JS: byteLen\n(byte length) and splitOn (split a line on a byte into a List). Registered\nin ioPrims, gated by a new usesStream helper that also fires the __s2h/__h2s\ncodec. ch548 witnesses run 3/3 byte-identical js+go (TestStreamSplitOn).\nNo core/store/elaborate change, no hash bump.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 2: `foldLines` (higher-order streaming fold) + the CoNLL-U counter demo

**Files:**
- Modify: `codegen/golang.go` (emit the `foldLines` Go body; add the `os`/`bufio` import gate)
- Modify: `codegen/js.go` (emit the `foldLines` JS body)
- Create: `harness/testdata/sample.conllu` (CoNLL-U fixture, 11 token lines)
- Create: `listings/ch549_conllu_count.rune` (the streaming token counter)
- Modify: `harness/io_stream_test.go` (add the token-count conformance test)

**Interfaces:**
- Consumes: `byteLen`/`splitOn` (Task 1), the `usesStream` gate (Task 1), `__s2h`/`__h2s`, the IO runtime (`pureIO`/`bindIO`), the constructor-cell convention.
- Produces: foreign op `foldLines : (S : U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S`, the fixture `harness/testdata/sample.conllu`, and the demo `main : IO Nat`. Task 3 reuses `foldLines` + `main` for the bounded-memory gate.

- [ ] **Step 1: Add the `os`/`bufio` import gate for `foldLines`**

In `codegen/golang.go`, in the import-collection section (near lines 47-85, where `addImp` is called under various `uses*` gates), add:

```go
	if usesForeign(p, "foldLines") {
		// The streaming fold opens a file and scans it line by line.
		addImp("os", "bufio")
	}
```

(Gate on `foldLines` specifically, NOT `usesStream` -- a program using only `splitOn`/`byteLen` must not import `os`/`bufio`, since Go rejects unused imports.)

- [ ] **Step 2: Emit the `foldLines` Go body**

In `codegen/golang.go`, in the prim-body emission section (next to the Task-1 `splitOn` block), add:

```go
	if usesForeign(p, "foldLines") {
		b.WriteString("func foldLines() any { return func(_S any) any { return func(path any) any { return func(step any) any { return func(s0 any) any { return func(_u any) any { f, err := os.Open(__s2h(path)); if err != nil { return s0 }; defer f.Close(); sc := bufio.NewScanner(f); sc.Buffer(make([]byte, 0, 1024*1024), 1024*1024); s := s0; for sc.Scan() { s = ap(ap(ap(step, s), __h2s(sc.Text())), nil) }; return s } } } } } }\n")
	}
```

This opens the path (open-fail returns the seed `s0`), scans line by line with a 1MB buffer (O(1) live memory), and per line applies the erased step `ap(ap(step, s), line)` to get an `IO S` thunk then forces it with `ap(_, nil)` -- the `primSpawn`/`bindIO_d` convention. The leading `_S` absorbs the erased type argument.

- [ ] **Step 3: Emit the `foldLines` JS body**

In `codegen/js.go`, next to the Task-1 `splitOn` block, add:

```go
	if usesForeign(p, "foldLines") {
		b.WriteString("const foldLines = () => _S => path => step => s0 => () => { let data; try { data = require('fs').readFileSync(__s2h(path), 'utf8'); } catch (e) { return s0; } const lines = data.split('\\n'); if (lines.length && lines[lines.length-1] === '') lines.pop(); let s = s0; for (const ln of lines) s = step(s)(__h2s(ln))(); return s; };\n")
	}
```

NOTE: the JS body reads the whole file (correctness-parity gate only). The O(1)-live-memory guarantee is the GO body's `bufio.Scanner`; the bounded-memory gate (Task 3) runs on the Go binary, which is the bible port's real target. The trailing-empty pop matches `bufio.Scanner`, which does not yield a final empty line after a terminating newline.

- [ ] **Step 4: Write the CoNLL-U fixture**

Create `harness/testdata/sample.conllu` with EXACTLY this content (two sentences; `#`-comment lines and the blank separator do NOT count; the 11 numbered token lines DO). The column separators are real TAB characters:

```
# sent_id = 1
# text = the quick brown fox jumps
1	the	the	DET
2	quick	quick	ADJ
3	brown	brown	ADJ
4	fox	fox	NOUN
5	jumps	jump	VERB

# sent_id = 2
# text = a lazy dog sleeps here
1	a	a	DET
2	lazy	lazy	ADJ
3	dog	dog	NOUN
4	sleeps	sleep	VERB
5	here	here	ADV
6	.	.	PUNCT
```

(Token lines: 5 + 6 = 11. Ensure each column gap is a literal `\t`, the file ends with a single trailing newline, and the blank line between sentences is truly empty.)

- [ ] **Step 5: Write the counter listing**

Create `listings/ch549_conllu_count.rune` with EXACTLY this content:

```
-- Chapter 549 -- streaming CoNLL-U token counter (Milestone A capstone: FR1+FR2+FR3).
--
-- foldLines streams a file line by line in the host runtime; the Rune step is TOTAL and
-- non-recursive (the data-plane loop lives in the runtime, so nothing here needs `partial`
-- and nothing has to migrate when a real totality checker lands). A token line is non-blank
-- and not a comment: splitOn the line on '#' (byte 35) and keep it iff its first part is
-- non-empty -- a blank line "" gives an empty first part, a "#..." comment gives an empty
-- first part, a real token line "1<tab>..." gives a non-empty first part. So splitOn + byteLen
-- classify every line with no fourth host op. main streams sample.conllu and prints 11.

data Nat  : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Bool : U is false : Bool | true : Bool end
data List : U -> U is nil : (A : U) -> List A | cons : (A : U) -> A -> List A -> List A end

-- Bytes/String/codeOf (NOT prelude-ambient): the "..." literal sugar desugars to
-- `bytes <packed-numeral>`, so `bytes` + codeOf must be in scope (as ch215).
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end

foreign foldLines : (S : U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign splitOn   : Nat -> Nat -> List Nat end
foreign byteLen   : Nat -> Nat end
foreign printNat  : Nat -> IO Nat end

-- head of a List Nat with a default (splitOn always returns >= 1 part, so the default is dead).
headD : Nat -> List Nat -> Nat is
  fn (d : Nat) (xs : List Nat) is
    ListElim Nat (fn (w : List Nat) is Nat end)
      d
      (fn (h : Nat) (t : List Nat) (ih : Nat) is h)
      xs
  end
end

-- gtZero n = (n > 0): zero -> false, succ _ -> true.
gtZero : Nat -> Bool is
  fn (n : Nat) is NatElim (fn (w : Nat) is Bool end) false (fn (k : Nat) (ih : Bool) is true) n end
end

-- a token line is non-blank and not a comment: first part of (split on '#') is non-empty.
isTokenLine : Nat -> Bool is
  fn (line : Nat) is gtZero (byteLen (headD 0 (splitOn 35 line))) end
end

-- the total, non-recursive per-line step: count token lines, skip the rest.
step : Nat -> Nat -> IO Nat is
  fn (count : Nat) (line : Nat) is
    pureIO Nat (BoolElim (fn (w : Bool) is Nat end) count (succ count) (isTokenLine line))
  end
end

-- stream the fixture (path relative to the test's working dir) and print the token count.
main : IO Nat is
  bindIO Nat Nat (foldLines Nat (codeOf "sample.conllu") step zero)
    (fn (n : Nat) is printNat n end)
end
```

(If the prelude's `BoolElim` argument order differs from `motive, false-branch, true-branch, scrutinee`, adjust the `BoolElim` application to match the prelude -- verify against an existing Bool-using listing during Step 6. The intent: when `isTokenLine line` is `true`, take `succ count`; else `count`.)

- [ ] **Step 6: Verify it elaborates + checks**

Run: `go run ./cmd/rune emit listings/ch549_conllu_count.rune main --target go`
Expected: PASS -- emits Go defining `foldLines`/`splitOn`/`byteLen`/`printNat`/`__s2h`/`__h2s`, imports include `"os"` and `"bufio"`, no type error, exit 0.

- [ ] **Step 7: Verify the counter streams the fixture on both backends**

Run from the fixture's directory so the relative path resolves:

```bash
( cd harness/testdata && go run ../../cmd/rune run ../../listings/ch549_conllu_count.rune main --target js )
```
Expected: `11`

```bash
( cd harness/testdata && go run ../../cmd/rune run ../../listings/ch549_conllu_count.rune main --target go )
```
Expected: `11`

- [ ] **Step 8: Add the token-count conformance test**

Append to `harness/io_stream_test.go`:

```go
func TestStreamConlluCount(t *testing.T) {
	for _, target := range []string{"js", "go"} {
		cmd := exec.Command("go", "run", "../cmd/rune", "run",
			"../listings/ch549_conllu_count.rune", "main", "--target", target)
		cmd.Dir = "testdata" // so the listing's relative "sample.conllu" resolves
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] run failed: %v\n%s", target, err, out)
		}
		if got := strings.TrimSpace(string(out)); got != "11" {
			t.Errorf("[%s] token count = %q, want 11", target, got)
		}
	}
}
```

- [ ] **Step 9: Run the new test + the listings gate + the full suite**

Run: `go test ./harness/ -run 'TestStreamConlluCount' -count=1 -v`
Expected: `--- PASS: TestStreamConlluCount`.

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch549_conllu_count.rune' -count=1 -v`
Expected: `--- PASS`.

Run: `go test ./... 2>&1 | tail -20`
Expected: all packages `ok` (no regression from the codegen edits).

- [ ] **Step 10: Commit**

```bash
git add codegen/golang.go codegen/js.go harness/testdata/sample.conllu listings/ch549_conllu_count.rune harness/io_stream_test.go
git commit -m "$(printf 'feat(codegen): foldLines higher-order streaming fold + CoNLL-U counter\n\nfoldLines (S:U) path step s0 streams a file line by line in the host runtime\n(Go bufio.Scanner = O(1) live; JS whole-file for correctness parity), applying\nthe erased Rune step per line via the primSpawn callback convention. The Rune\ndata plane stays total -- no partial. ch549 streams an 11-token CoNLL-U fixture\nand prints 11 byte-identical js+go (TestStreamConlluCount). No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 3: Bounded-memory streaming gate (1M lines, Go binary)

**Files:**
- Modify: `harness/io_stream_test.go` (add the large-fixture streaming test)

**Interfaces:**
- Consumes: `foldLines` + the `main` of `listings/ch549_conllu_count.rune` (Task 2).
- Produces: `TestStreamBoundedMemory` -- evidence that the Go streaming path handles a file far larger than the committed fixture with an exact count.

- [ ] **Step 1: Write the bounded-memory test**

Append to `harness/io_stream_test.go` (add `"fmt"`, `"os"`, `"path/filepath"` to the import block):

```go
func TestStreamBoundedMemory(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping 1M-line streaming gate in -short")
	}
	dir := t.TempDir()
	fixture := filepath.Join(dir, "sample.conllu")
	f, err := os.Create(fixture)
	if err != nil {
		t.Fatal(err)
	}
	const n = 1_000_000
	w := bufio.NewWriter(f)
	for i := 0; i < n; i++ {
		// every line is a token line: "1<tab>word<tab>word<tab>X"
		fmt.Fprintf(w, "%d\tword\tword\tX\n", i+1)
	}
	if err := w.Flush(); err != nil {
		t.Fatal(err)
	}
	f.Close()

	// resolve the listing path absolutely, since cmd.Dir is the temp fixture dir.
	listing, err := filepath.Abs("../listings/ch549_conllu_count.rune")
	if err != nil {
		t.Fatal(err)
	}
	runner, err := filepath.Abs("../cmd/rune")
	if err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("go", "run", runner, "run", listing, "main", "--target", "go")
	cmd.Dir = dir // the listing opens the relative "sample.conllu" here
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("bounded-memory run failed: %v\n%s", err, out)
	}
	if got := strings.TrimSpace(string(out)); got != "1000000" {
		t.Errorf("1M-line count = %q, want 1000000", got)
	}
}
```

This adds `"bufio"` to the test's imports as well (used by the generator).

- [ ] **Step 2: Run the bounded-memory gate**

Run: `go test ./harness/ -run 'TestStreamBoundedMemory' -count=1 -v`
Expected: `--- PASS: TestStreamBoundedMemory` -- the Go binary streams 1,000,000 lines and prints `1000000`. (The streaming is structural: `bufio.Scanner` holds one line at a time, so live memory is flat regardless of `n`. The 10M case in the spec is the same code path; 1M is the CI-sized witness.)

- [ ] **Step 3: Run the full stream suite + the whole test suite**

Run: `go test ./harness/ -run 'TestStream' -count=1 -v`
Expected: `TestStreamSplitOn`, `TestStreamConlluCount`, `TestStreamBoundedMemory` all PASS.

Run: `go test ./... 2>&1 | tail -20`
Expected: all packages `ok`.

- [ ] **Step 4: Commit**

```bash
git add harness/io_stream_test.go
git commit -m "$(printf 'test(harness): bounded-memory 1M-line streaming gate\n\nGenerates a 1,000,000-line CoNLL-U file in a temp dir and streams it through\nch549 main on the Go binary, asserting an exact count of 1000000. Evidence\nthat foldLines holds flat live memory (bufio.Scanner, one line at a time)\nfar past the committed fixture. Skipped under -short.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Self-Review

**1. Spec coverage.** The Milestone-A spec's deliverables map to tasks: `splitOn`/`byteLen` (FR3 slice) = Task 1; `foldLines` (FR2 streaming) + the CoNLL-U token-counter demo + the token-count conformance gate (acceptance crit. 2) = Task 2; the bounded-memory evidence (acceptance crit. 4) = Task 3. Acceptance crit. 1 (the three ops elaborate/check/erase on Go+JS, no hash bump) is verified by Task-1 Step 5 and Task-2 Step 6 plus the Global Constraint forbidding core edits. Acceptance crit. 3 (`splitOn ... -> 3 parts`) = Task-1 Step 6/8. Acceptance crit. 5 (full `go test ./...` green, codegen-only) = Task-2 Step 9 and Task-3 Step 3. The spec's `isTokenLine` "plan picks the in-language route" is resolved: split on `#` and test the first part's length -- one mechanism covers both blank and comment lines, no fourth host op.

**2. Placeholder scan.** No TBD/TODO. Every host-op body, listing, fixture, and test is inline and complete. The two "verify/adjust against the prelude" notes (JS codec gate line in Task-1 Step 3; `BoolElim` argument order in Task-2 Step 5) are concrete lookups against named existing code, not deferred design -- each names the file and the exact thing to match.

**3. Type consistency.** The op names are identical across `ioPrims`, the Go/JS bodies, and the `foreign` declarations: `foldLines`/`splitOn`/`byteLen`. `usesStream` is defined once (Task 1) and reused by the Go import gate is NOT it (Task 2 deliberately gates `os`/`bufio` on `usesForeign(p,"foldLines")`, not `usesStream`, to avoid unused-import errors -- called out in Task-2 Step 1). The codec gate uses `usesStream` (all three need `__s2h`/`__h2s`). Constructor tags (`nil`=0, `cons`=1) are consistent between the host `splitOn` body (Task 1) and the `data List` declarations in ch548/ch549, which both declare `nil` before `cons`. The `foldLines` curry arity (`_S`, path, step, s0, `_u`) matches its type `(S:U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S` and the `primSpawn` precedent. `IO` forcing matches the runtime (`ap(_,nil)` Go / `()` JS).
