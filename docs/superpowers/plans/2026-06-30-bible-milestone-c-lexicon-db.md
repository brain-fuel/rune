# Bible Port -- Milestone C: `build_db` (lexicon table) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A `rune run` builds a SQLite database whose `lexicon` table is query-equivalent to Python's `tools/build_db.py` output, via two new host ops (`sqlQuote`, `dbApply`) + a JS write-encoding fix, reusing Milestone B's ops.

**Architecture:** Streaming pipeline (foldDir extract -> sortFile -> assemble DDL+INSERTs+index -> dbApply loads via the `sqlite3` CLI). Two new D6 foreign ops on Go+JS: `sqlQuote : Bytes -> Bytes` (SQL value escaper) and `dbApply : Path -> Path -> IO Unit` (shells to `sqlite3`). Plus the Milestone-B-parked JS write-encoding fix (Greek/Hebrew lemmas are the first non-ASCII write consumer). Acceptance is query-equivalence (a `.sqlite` file is not byte-deterministic); the deterministic `.sql` text keeps a byte-identical gate.

**Tech Stack:** Go (`codegen/golang.go`, the bible target), JS (`codegen/js.go`, conformance gate), `codegen/ioprims.go`, `harness/bible_ops_test.go`, `listings/` (ch556-559). The `sqlite3` CLI is a test-env tool (gracefully skipped).

## Global Constraints

- Kernel FROZEN: changes ONLY in `codegen/{ioprims,golang,js}.go`, `harness/`, `listings/`. No core/store/elaborate edit. No hash-format bump.
- Backends: **Go and JS only**. Do not touch py/rust/beam/jvm/C/LLVM/WASM for these ops.
- Foreign ops register in `ioPrims` (codegen/ioprims.go), codec-using ones also in `streamPrims` (so `__s2h`/`__h2s` emit), gate emission via `if usesForeign(p, "<name>") {...}`. Path/Bytes args are the packed-String code (a `Nat`); decode with `__s2h`, encode with `__h2s` (NOT the Bin `_binToGoBytes` marshalling -- this op family uses the packed codec, like `sortFile`/`openWrite`).
- An `IO _` op takes a trailing `_u any` (Go) / `() =>` (JS) world-token level and returns `nil`/`null` for `IO Unit`. A pure op (`sqlQuote`) has NO world token.
- Constructor cells (where needed): Go `map[string]any{"tag":N,...,"args":[]any{...}}`, JS `{tag:N,...,args:[...]}`. Option: `none`=tag0 `args [nil]`, `some`=tag1 `args [nil,v]`. List: `nil`=tag0 `args [nil]`, `cons`=tag1 `args [nil,head,tail]`. (Reused, not built fresh, in this milestone.)
- Acceptance is QUERY-EQUIVALENCE (sorted table dump + `COUNT(*)` match Python), NOT byte-identity of the `.sqlite`. The `.sql` TEXT is byte-identical js+go (the determinism gate).
- `sqlite3` CLI: the load/query gates `exec.LookPath("sqlite3")`-guard and `t.Skip` when absent; the `.sql`-text gate is always-on and sqlite3-free.
- No em-dashes / en-dashes; use `--`. Conventional Commits; trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- No `rune check`: use `go run ./cmd/rune emit|run FILE NAME --target go|js`.
- Chapters ch556-559 are free (highest existing is ch555).

---

### Task 1: JS write-encoding fix (raw bytes, not utf8)

**Files:**
- Modify: `codegen/js.go` (three write bodies: `writeFileCode`, `writeChunk`, `sortFile` output)
- Create: `listings/ch556_write_bytes.rune`
- Modify: `harness/bible_ops_test.go` (add the non-ASCII round-trip gate)

**Interfaces:**
- Produces: no new op. The JS `writeChunk`/`sortFile`/`writeFileCode` bodies now write raw bytes (`Buffer.from(s, 'latin1')`), matching the Go target on non-ASCII. (Un-parks the Milestone-B finding: Greek/Hebrew lemmas are the first non-ASCII write consumer.)

> **Why:** the JS bodies pass a latin1-decoded JS string straight to `fs.write*Sync`, which re-encodes it as utf8. For any byte > 127 (Greek/Hebrew) this double-encodes, diverging from Go's raw-byte write. Wrapping in `Buffer.from(str, 'latin1')` writes the bytes verbatim. ASCII is unaffected (utf8 == latin1 for bytes < 128), so the ch215 and Milestone-B (ASCII-only) gates stay green.

- [ ] **Step 1: Write the failing regression listing**

Create `listings/ch556_write_bytes.rune`:
```
-- Chapter 556 -- write vocabulary writes RAW BYTES (Milestone C, JS write-encoding fix).
--
-- Greek "αβγ" is 6 UTF-8 bytes. Write it with writeChunk, read it back with foldLines, print its
-- byteLen. Witness (run js+go): 6 on BOTH backends. Before the fix, JS re-encodes the latin1 string
-- as utf8 on write (12 bytes) -- this listing is the regression that pins the raw-byte write.

data Nat  : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end

foreign Handle     : U end
foreign openWrite  : Nat -> IO Handle end
foreign writeChunk : Handle -> Nat -> IO Handle end
foreign closeWrite : Handle -> IO Unit end
foreign foldLines  : (S : U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign byteLen    : Nat -> Nat end
foreign printNat   : Nat -> IO Nat end

-- read the FIRST line's byteLen (state 0 = none yet; first line sets it, rest keep acc).
firstLen : Nat -> Nat -> IO Nat is
  fn (acc : Nat) (line : Nat) is
    pureIO Nat (NatElim (fn (w : Nat) is Nat end) (byteLen line) (fn (k : Nat) (ih : Nat) is acc) acc)
  end
end

main : IO Nat is
  bindIO Handle Nat (openWrite (codeOf "ch556.tmp")) (fn (h0 : Handle) is
    bindIO Handle Nat (writeChunk h0 (codeOf "αβγ")) (fn (h1 : Handle) is
      bindIO Unit Nat (closeWrite h1) (fn (u : Unit) is
        bindIO Nat Nat (foldLines Nat (codeOf "ch556.tmp") firstLen zero) (fn (n : Nat) is
          printNat n
        end)
      end)
    end)
  end)
end
```

- [ ] **Step 2: Verify it FAILS on JS, passes on Go (the bug)**

Build the CLI and run from a temp dir on both backends:
```bash
go build -o /tmp/runeC ./cmd/rune
D=$(mktemp -d); ( cd "$D" && /tmp/runeC run "$OLDPWD/listings/ch556_write_bytes.rune" main --target js | tail -1 )
D=$(mktemp -d); ( cd "$D" && /tmp/runeC run "$OLDPWD/listings/ch556_write_bytes.rune" main --target go | tail -1 )
```
Expected BEFORE the fix: js prints `12` (double-encoded), go prints `6`. (This confirms the bug.)

- [ ] **Step 3: Apply the JS write-encoding fix**

In `codegen/js.go`, change the three write bodies to wrap the written string in `Buffer.from(..., 'latin1')`:

`writeChunk` (currently `require('fs').writeSync(h, __s2h(c) + '\n')`):
```go
	b.WriteString("const writeChunk = () => h => c => () => { if (h !== null) require('fs').writeSync(h, Buffer.from(__s2h(c) + '\\n', 'latin1')); return h; };\n")
```

`sortFile` (the terminal `writeFileSync` of the joined lines):
```go
	b.WriteString("const sortFile = () => inp => outp => () => { const fs = require('fs'); let data; try { data = fs.readFileSync(__s2h(inp), 'latin1'); } catch (e) { fs.writeFileSync(__s2h(outp), ''); return null; } let lines = data.split('\\n'); if (lines.length && lines[lines.length-1] === '') lines.pop(); lines.sort((a,b) => a < b ? -1 : a > b ? 1 : 0); fs.writeFileSync(__s2h(outp), Buffer.from(lines.map(l => l + '\\n').join(''), 'latin1')); return null; };\n")
```

`writeFileCode` (currently `require('fs').writeFileSync(__s2h(p), __s2h(c))`):
```go
	b.WriteString("const writeFileCode = () => p => c => () => { require('fs').writeFileSync(__s2h(p), Buffer.from(__s2h(c), 'latin1')); return c; };\n")
```
(Locate each by its current verbatim text and replace only the `fs.write*` argument.)

- [ ] **Step 4: Verify both backends now print 6**

Re-run Step 2's two commands. Expected: js `6`, go `6` (the listing double-prints via printNat -> `6\n6`).

- [ ] **Step 5: Add the regression gate**

Append to `harness/bible_ops_test.go`:
```go
func TestBibleWriteBytesRoundTrip(t *testing.T) {
	for _, tg := range []string{"js", "go"} {
		if _, err := exec.LookPath(map[string]string{"js": "node", "go": "go"}[tg]); err != nil {
			t.Skipf("%s runtime not in PATH", tg)
		}
		s := loadListing(t, "ch556_write_bytes.rune")
		p, err := s.EmitProgram("main")
		if err != nil {
			t.Fatalf("emit: %v", err)
		}
		var src string
		var runner func(string) *exec.Cmd
		if tg == "js" {
			src, err = codegen.JS{}.Emit(p)
			runner = func(f string) *exec.Cmd { return exec.Command("node", f) }
		} else {
			src, err = codegen.Go{}.Emit(p)
			runner = func(f string) *exec.Cmd { return exec.Command("go", "run", f) }
		}
		if err != nil {
			t.Fatalf("[%s] emit: %v", tg, err)
		}
		dir := t.TempDir()
		f := filepath.Join(dir, "main."+tg)
		if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
		cmd := runner(f)
		cmd.Dir = dir
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] run: %v\n%s", tg, err, out)
		}
		if got := strings.TrimSpace(string(out)); got != "6\n6" {
			t.Errorf("[%s] byteLen of round-tripped Greek = %q, want 6\\n6", tg, got)
		}
	}
}
```
(Confirm `harness/bible_ops_test.go` already imports `goforge.dev/rune/v3/codegen`; if not, add it -- `TestBibleBuildSharedRootFixture` already uses `codegen.JS{}`/`codegen.Go{}`, so it should be present.)

- [ ] **Step 6: Run the gates + confirm no regression + commit**

```
go test ./harness/ -run 'TestBibleWriteBytesRoundTrip' -count=1 -v   # PASS (js+go = 6\n6)
go test ./harness/ -run 'TestBibleSharedRootByteIdentical|TestBibleBuildSharedRootFixture' -count=1   # still PASS (ASCII unaffected)
go test ./harness/ -run 'TestIOFileEnvConformance' -count=1   # ch215 writeFileCode still PASS
go test ./harness/ -run 'TestListingsElaborateAndCheck/ch556_write_bytes.rune' -count=1   # PASS
```

```bash
git add codegen/js.go listings/ch556_write_bytes.rune harness/bible_ops_test.go
git commit -m "$(printf 'fix(codegen): JS write vocabulary emits raw bytes not utf8 (bible Milestone C)\n\nwriteFileCode/writeChunk/sortFile JS bodies wrap the written string in\nBuffer.from(s, latin1) so non-ASCII (Greek/Hebrew) output matches the Go\ntarget byte-for-byte -- the first non-ASCII write consumer (lexicon lemmas).\nASCII unaffected (ch215 + shared-root gates green). ch556 round-trips Greek\nto byteLen 6 js+go (TestBibleWriteBytesRoundTrip). Un-parks the Milestone-B finding.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 2: `sqlQuote` (SQL value escaper)

**Files:**
- Modify: `codegen/ioprims.go` (register `sqlQuote`; add to `streamPrims`)
- Modify: `codegen/golang.go` (Go body)
- Modify: `codegen/js.go` (JS body)
- Create: `listings/ch557_sql_quote.rune`
- Modify: `harness/bible_ops_test.go` (add the sqlQuote gate)

**Interfaces:**
- Produces: `sqlQuote : Bytes -> Bytes` -- wrap a value in single quotes and double every internal `'` (byte 0x27), passing all other bytes (incl. multi-byte UTF-8) through. Pure.

- [ ] **Step 1: Register the op**

In `codegen/ioprims.go`, add to the `ioPrims` map (near `jsonStrField`):
```go
	"sqlQuote": true, // sqlQuote v : Nat -> Nat  (SQL-escape: double ' , wrap in '...')
```
Add `"sqlQuote"` to the `streamPrims` slice:
```go
var streamPrims = []string{"foldLines", "foldDir", "splitOn", "byteLen", "jsonStrField", "openWrite", "writeChunk", "sortFile", "sqlQuote"}
```

- [ ] **Step 2: Emit the Go body**

In `codegen/golang.go`, next to the `jsonStrField` body:
```go
	if usesForeign(p, "sqlQuote") {
		b.WriteString("func sqlQuote() any { return func(s any) any { in := __s2h(s); var sb strings.Builder; sb.WriteByte('\\''); for i := 0; i < len(in); i++ { if in[i] == '\\'' { sb.WriteByte('\\'') }; sb.WriteByte(in[i]) }; sb.WriteByte('\\''); return __h2s(sb.String()) } }\n")
	}
```
(No new import -- `strings` is always imported. Byte-level: only 0x27 is doubled; every other byte passes through, so UTF-8 stays intact.)

- [ ] **Step 3: Emit the JS body**

In `codegen/js.go`, next to the `jsonStrField` body:
```go
	if usesForeign(p, "sqlQuote") {
		b.WriteString("const sqlQuote = () => s => { const str = __s2h(s); let out = \"'\"; for (let i = 0; i < str.length; i++) { if (str[i] === \"'\") out += \"'\"; out += str[i]; } out += \"'\"; return __h2s(out); };\n")
	}
```
(`__s2h` yields a string whose chars are single bytes 0-255, so `str[i] === "'"` tests byte 0x27 and the codec re-packs identically to Go.)

- [ ] **Step 4: Write the listing**

Create `listings/ch557_sql_quote.rune`:
```
-- Chapter 557 -- sqlQuote: SQL value escaper (Milestone C).
--
-- sqlQuote wraps a value in single quotes and doubles internal quotes. Witnesses (run js+go):
-- sqlQuote "xyz" = 'xyz' (byteLen 5); sqlQuote "a'b" = 'a''b' (byteLen 6). Byte-level escaping,
-- so multi-byte UTF-8 passes through untouched (the lexicon's Greek/Hebrew lemmas).

data Nat  : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end

foreign sqlQuote : Nat -> Nat end
foreign byteLen  : Nat -> Nat end

quotePlain    : Nat is byteLen (sqlQuote (codeOf "xyz")) end
quoteEmbedded : Nat is byteLen (sqlQuote (codeOf "a'b")) end
```

- [ ] **Step 5: Verify (js + go)**

```
go run ./cmd/rune run listings/ch557_sql_quote.rune quotePlain --target js   # 5
go run ./cmd/rune run listings/ch557_sql_quote.rune quotePlain --target go   # 5
go run ./cmd/rune run listings/ch557_sql_quote.rune quoteEmbedded --target js   # 6
go run ./cmd/rune run listings/ch557_sql_quote.rune quoteEmbedded --target go   # 6
```

- [ ] **Step 6: Add the gate**

Append to `harness/bible_ops_test.go`:
```go
func TestBibleSqlQuote(t *testing.T) {
	for _, tg := range []string{"js", "go"} {
		if got := runBibleOp(t, "listings/ch557_sql_quote.rune", "quotePlain", tg); got != "5" {
			t.Errorf("[%s] quotePlain = %q, want 5", tg, got)
		}
		if got := runBibleOp(t, "listings/ch557_sql_quote.rune", "quoteEmbedded", tg); got != "6" {
			t.Errorf("[%s] quoteEmbedded = %q, want 6", tg, got)
		}
	}
}
```

- [ ] **Step 7: Run the gates + commit**

```
go test ./harness/ -run 'TestBibleSqlQuote' -count=1 -v   # PASS
go test ./harness/ -run 'TestListingsElaborateAndCheck/ch557_sql_quote.rune' -count=1   # PASS
```

```bash
git add codegen/ioprims.go codegen/golang.go codegen/js.go listings/ch557_sql_quote.rune harness/bible_ops_test.go
git commit -m "$(printf 'feat(codegen): sqlQuote SQL value escaper (bible Milestone C)\n\nPure foreign op: wrap a value in single quotes, double internal quotes,\nbyte-level so UTF-8 lemmas pass through. Go + JS. ch557 witnesses 5 and 6\njs+go (TestBibleSqlQuote). No core change, no hash bump.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 3: `dbApply` (load a `.sql` script via the sqlite3 CLI)

**Files:**
- Modify: `codegen/ioprims.go` (register `dbApply`; add to `streamPrims`)
- Modify: `codegen/golang.go` (Go body + `os/exec` import gate)
- Modify: `codegen/js.go` (JS body)
- Create: `listings/ch558_db_apply.rune`
- Modify: `harness/bible_ops_test.go` (add the dbApply gate, sqlite3-guarded)

**Interfaces:**
- Produces: `dbApply : Path -> Path -> IO Unit` -- run `sqlite3 <dbPath> ".read <sqlPath>"` to load a SQL script into a database file. Makes the whole DB build one `rune run`.

- [ ] **Step 1: Register + import gate**

In `codegen/ioprims.go`, add to `ioPrims`:
```go
	"dbApply": true, // dbApply dbPath sqlPath : Nat -> Nat -> IO Unit  (sqlite3 db ".read sql")
```
Add `"dbApply"` to `streamPrims`. In `codegen/golang.go`, in the import-gating block (next to the `usesProc(p)` -> `addImp("os/exec")` gate), add:
```go
	if usesForeign(p, "dbApply") {
		addImp("os/exec")
	}
```

- [ ] **Step 2: Emit the Go body**

In `codegen/golang.go`, next to the `sqlQuote`/`closeWrite` bodies:
```go
	if usesForeign(p, "dbApply") {
		b.WriteString("func dbApply() any { return func(db any) any { return func(sql any) any { return func(_u any) any { exec.Command(\"sqlite3\", __s2h(db), \".read \"+__s2h(sql)).Run(); return nil } } } }\n")
	}
```
(Paths are ASCII temp paths; `sqlite3 <db> \".read <sql>\"` runs the script. Errors are swallowed like `procRun` -- a failed load surfaces as a failed downstream query in the gate. Returns `nil` for `IO Unit`.)

- [ ] **Step 3: Emit the JS body**

In `codegen/js.go`, next to the `sqlQuote` body:
```go
	if usesForeign(p, "dbApply") {
		b.WriteString("const dbApply = () => db => sql => () => { try { require('child_process').execFileSync('sqlite3', [__s2h(db), '.read ' + __s2h(sql)]); } catch (e) {} return null; };\n")
	}
```

- [ ] **Step 4: Write the listing**

Create `listings/ch558_db_apply.rune`:
```
-- Chapter 558 -- dbApply: load a .sql script into a SQLite db via the sqlite3 CLI (Milestone C).
--
-- main writes a 2-row .sql script (CREATE + two INSERTs), then dbApply loads it into "ch558.db".
-- The harness gate (sqlite3-guarded) then queries SELECT count(*) -> 2. Builds the db with one
-- `rune run`; no Go SQLite library. main : IO Unit (no printed witness -- the db file is the output).

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
foreign dbApply    : Nat -> Nat -> IO Unit end

main : IO Unit is
  bindIO Handle Unit (openWrite (codeOf "ch558.sql")) (fn (h0 : Handle) is
    bindIO Handle Unit (writeChunk h0 (codeOf "CREATE TABLE t (x INTEGER);")) (fn (h1 : Handle) is
      bindIO Handle Unit (writeChunk h1 (codeOf "INSERT INTO t VALUES (1);")) (fn (h2 : Handle) is
        bindIO Handle Unit (writeChunk h2 (codeOf "INSERT INTO t VALUES (2);")) (fn (h3 : Handle) is
          bindIO Unit Unit (closeWrite h3) (fn (u : Unit) is
            dbApply (codeOf "ch558.db") (codeOf "ch558.sql")
          end)
        end)
      end)
    end)
  end)
end
```

- [ ] **Step 5: Verify (emit clean + run builds the db; requires sqlite3)**

```
go run ./cmd/rune emit listings/ch558_db_apply.rune main --target go   # PASS (imports include os/exec)
```
If `sqlite3` is installed, build the db from a temp dir and query it:
```bash
go build -o /tmp/runeC ./cmd/rune
D=$(mktemp -d); ( cd "$D" && /tmp/runeC run "$OLDPWD/listings/ch558_db_apply.rune" main --target go >/dev/null 2>&1; sqlite3 "$D/ch558.db" "SELECT count(*) FROM t" )
```
Expected: `2`. Repeat with `--target js` -> `2`.

- [ ] **Step 6: Add the gate (sqlite3-guarded)**

Append to `harness/bible_ops_test.go`:
```go
func TestBibleDbApply(t *testing.T) {
	if _, err := exec.LookPath("sqlite3"); err != nil {
		t.Skip("sqlite3 CLI not in PATH")
	}
	for _, tg := range []string{"js", "go"} {
		if _, err := exec.LookPath(map[string]string{"js": "node", "go": "go"}[tg]); err != nil {
			t.Skipf("%s runtime not in PATH", tg)
		}
		s := loadListing(t, "ch558_db_apply.rune")
		p, err := s.EmitProgram("main")
		if err != nil {
			t.Fatalf("emit: %v", err)
		}
		var src string
		var runner func(string) *exec.Cmd
		if tg == "js" {
			src, err = codegen.JS{}.Emit(p)
			runner = func(f string) *exec.Cmd { return exec.Command("node", f) }
		} else {
			src, err = codegen.Go{}.Emit(p)
			runner = func(f string) *exec.Cmd { return exec.Command("go", "run", f) }
		}
		if err != nil {
			t.Fatalf("[%s] emit: %v", tg, err)
		}
		dir := t.TempDir()
		f := filepath.Join(dir, "main."+tg)
		if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
		cmd := runner(f)
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("[%s] run: %v\n%s", tg, err, out)
		}
		q := exec.Command("sqlite3", filepath.Join(dir, "ch558.db"), "SELECT count(*) FROM t")
		out, err := q.CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] query: %v\n%s", tg, err, out)
		}
		if got := strings.TrimSpace(string(out)); got != "2" {
			t.Errorf("[%s] count = %q, want 2", tg, got)
		}
	}
}
```

- [ ] **Step 7: Run the gates + commit**

```
go test ./harness/ -run 'TestBibleDbApply' -count=1 -v   # PASS (or SKIP if sqlite3 absent)
go test ./harness/ -run 'TestListingsElaborateAndCheck/ch558_db_apply.rune' -count=1   # PASS
```

```bash
git add codegen/ioprims.go codegen/golang.go codegen/js.go listings/ch558_db_apply.rune harness/bible_ops_test.go
git commit -m "$(printf 'feat(codegen): dbApply loads a .sql script via the sqlite3 CLI (bible Milestone C)\n\nIO op shelling to sqlite3 (db \".read sql\"), Go (os/exec) + JS (child_process),\nso a DB build is one rune run with no Go SQLite library. ch558 writes a 2-row\nscript, loads it, the gate queries count=2 js+go (TestBibleDbApply, sqlite3-guarded).\nNo core change, no hash bump.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 4: `build_db` lexicon builder + synthetic fixture gates

**Files:**
- Create: `listings/ch559_build_db_lexicon.rune` (the pipeline)
- Create: `harness/testdata/lexdbfix/` (synthetic lexicon fixture) + `harness/testdata/lexdbfix_expected.sql` (expected `.sql`) + `harness/testdata/lexdbfix_expected_rows.txt` (expected sorted table dump)
- Modify: `harness/bible_ops_test.go` (the `.sql`-text gate, always-on; the query gate, sqlite3-guarded)

**Interfaces:**
- Consumes: `foldDir`/`jsonStrField`/`byteLen`/the write trio/`sortFile`/`foldLines` (Milestone B), `sqlQuote`/`dbApply` (Tasks 2-3).
- Produces: `main : IO Unit` -- builds `lexicon.sql` (DDL + sorted INSERTs + index) from `./lexdb`, then `dbApply` into `./lexicon.db`.

> **NOTE TO IMPLEMENTER:** Like Milestone B's ch555, this builder cannot be pre-verified before its ops exist (they now do). The structure below is a REFERENCE; your acceptance is the two fixture gates (Steps 5-6). Make the always-on `.sql`-text gate produce the EXACT expected file. Adjust to the real op signatures and the rune surface as needed; the gates are the spec.

**Field semantics (from the spec -- get these exact):**
- Six fields per entry, all top-level strings, via `jsonStrField`: `strong`, `lemma`, `translit`, `lang`, `pos`, `root`.
- `strong`: `none -> NULL`; `some v -> (byteLen v == 0 ? NULL : sqlQuote v)` (Python's `strong or None` folds empty-string to NULL).
- the other five (`lemma`/`translit`/`lang`/`pos`/`root`): `none -> NULL`; `some v -> sqlQuote v` (empty string stays `''`, NOT null -- Python's `entry.get(field)`).
- Each entry -> one line `INSERT OR IGNORE INTO lexicon(strong,lemma,translit,lang,pos,root) VALUES (S,L,T,G,P,R);` (the six rendered terms, comma-separated, in that column order).

**Constant templates (byte-for-byte):**
- DDL line: `CREATE TABLE lexicon (strong TEXT PRIMARY KEY, lemma TEXT, translit TEXT, lang TEXT, pos TEXT, root TEXT);`
- index line: `CREATE INDEX idx_lexicon_lemma ON lexicon(lemma);`

- [ ] **Step 1: Write the builder listing**

Create `listings/ch559_build_db_lexicon.rune`. Reference structure (preamble identical to ch555 lines 10-29; add `foreign sqlQuote` + `foreign dbApply`):
```
-- Chapter 559 -- build_db (lexicon table): emit deterministic SQL, load via dbApply (Milestone C).
--
-- 4-phase pipeline + load:
--   1. foldDir lexdb ".json": per entry, jsonStrField x6 (strong/lemma/translit/lang/pos/root),
--      render each (none->NULL, some->sqlQuote; strong folds empty->NULL), write one INSERT line.
--   2. sortFile -> deterministic INSERT order.
--   3. assemble lexicon.sql: writeChunk DDL; foldLines the sorted INSERTs re-emitting each; writeChunk index.
--   4. dbApply lexicon.db lexicon.sql.
-- lexDir = "lexdb" for the fixture/real gate. The DDL+index are constant templates; only VALUES vary.

data Nat   : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Bool  : U is false : Bool | true : Bool end
data Unit  : U is unit : Unit end
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end
data Option : U -> U is none : (A : U) -> Option A | some : (A : U) -> A -> Option A end

foreign Handle       : U end
foreign foldDir      : (S : U) -> Nat -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign foldLines    : (S : U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign jsonStrField : Nat -> Nat -> Option Nat end
foreign byteLen      : Nat -> Nat end
foreign sqlQuote     : Nat -> Nat end
foreign openWrite    : Nat -> IO Handle end
foreign writeChunk   : Handle -> Nat -> IO Handle end
foreign closeWrite   : Handle -> IO Unit end
foreign sortFile     : Nat -> Nat -> IO Unit end
foreign dbApply      : Nat -> Nat -> IO Unit end

-- Implementer: build the per-field SQL term and the INSERT line, then wire the 4 phases.
-- Field -> term: sqlField (none->NULL / some->sqlQuote) for 5 cols; sqlFieldStrong adds the
-- empty->NULL guard via byteLen. Concatenate the line with the constant fragments
-- "INSERT OR IGNORE INTO lexicon(strong,lemma,translit,lang,pos,root) VALUES (" , "," , ");"
-- (string interpolation {bytes term}, or the cat2 arithmetic-concat from ch555). NO braces in the
-- template, so no \{ \} escaping is needed here.

main : IO Unit is
  -- phase 1: foldDir "lexdb" ".json" extractStep (openWrite "stage1.sql"); closeWrite
  -- phase 2: sortFile "stage1.sql" "stage1.sorted"
  -- phase 3: openWrite "lexicon.sql"; writeChunk DDL; foldLines "stage1.sorted" reChunk; writeChunk index; closeWrite
  -- phase 4: dbApply "lexicon.db" "lexicon.sql"
  pureIO Unit unit  -- REPLACE with the real wiring; make the fixture gates pass
end
```
**Implementer:** flesh out `main` + the helpers so the gates pass. The INSERT column order is exactly `strong,lemma,translit,lang,pos,root`.

- [ ] **Step 2: Create the synthetic fixture**

Create under `harness/testdata/lexdbfix/` (entries exercising: a grc entry with a Greek lemma, an hbo entry with a Hebrew lemma + null root, a NULL-strong / empty-strong entry with missing fields, and an entry with an embedded `'` to exercise escaping):
- `g1.json`: `{"strong": "G0001", "lemma": "Α", "translit": "A", "lang": "grc", "pos": "N", "root": "G0001"}`
- `h1.json`: `{"strong": "H0001", "lemma": "אב", "translit": "av", "lang": "hbo", "pos": "N", "root": null}`
- `n1.json`: `{"strong": "", "lemma": "λόγος", "lang": "grc"}`
- `q1.json`: `{"strong": "G0002", "lemma": "ab'c", "lang": "grc"}`

- [ ] **Step 3: Compute the expected `.sql`**

Render each entry per the field semantics (Step's NOTE), sort the INSERT lines bytewise, frame with DDL + index. Expected (the four INSERTs sorted; `h1`/`n1`/`q1`/`g1` columns rendered; null root/missing fields -> `NULL`; empty strong -> `NULL`; `ab'c` -> `'ab''c'`; Greek/Hebrew lemmas as raw UTF-8 bytes):
```
CREATE TABLE lexicon (strong TEXT PRIMARY KEY, lemma TEXT, translit TEXT, lang TEXT, pos TEXT, root TEXT);
INSERT OR IGNORE INTO lexicon(strong,lemma,translit,lang,pos,root) VALUES ('G0001','Α','A','grc','N','G0001');
INSERT OR IGNORE INTO lexicon(strong,lemma,translit,lang,pos,root) VALUES ('G0002','ab''c',NULL,'grc',NULL,NULL);
INSERT OR IGNORE INTO lexicon(strong,lemma,translit,lang,pos,root) VALUES ('H0001','אב','av','hbo','N',NULL);
INSERT OR IGNORE INTO lexicon(strong,lemma,translit,lang,pos,root) VALUES (NULL,'λόγος',NULL,'grc',NULL,NULL);
CREATE INDEX idx_lexicon_lemma ON lexicon(lemma);
```
Save EXACTLY this (each line + `\n`, trailing newline) as `harness/testdata/lexdbfix_expected.sql`. (NOTE: the four INSERT lines are bytewise-sorted -- `'G0001'` < `'G0002'` < `'H0001'` < `NULL` because `'`=0x27 < `N`=0x4E. Verify against your builder's actual output and reconcile; the builder's `sortFile` defines the order. If the realized order differs, regenerate this file from the builder output and confirm it is the bytewise sort.)

- [ ] **Step 4: Compute the expected table dump**

After loading via `sqlite3`, `SELECT * FROM lexicon ORDER BY strong IS NULL, strong, lemma, translit, lang, pos, root` yields pipe-separated rows (sqlite3 default). Empty/NULL render as empty fields. Save the expected dump as `harness/testdata/lexdbfix_expected_rows.txt` -- generate it by loading your builder's `lexicon.sql` with `sqlite3` and running that exact query, then VERIFY it matches the four logical rows (G0001 full; G0002 with `ab'c` un-escaped back, NULLs; H0001 null root; NULL-strong λόγος). The round-trip (`'ab''c'` -> stored `ab'c`) is the escaping correctness check.

- [ ] **Step 5: Add the always-on `.sql`-text gate**

Append to `harness/bible_ops_test.go`:
```go
func TestBibleBuildDbLexiconSql(t *testing.T) {
	want, err := os.ReadFile("testdata/lexdbfix_expected.sql")
	if err != nil {
		t.Fatal(err)
	}
	for _, tg := range []string{"js", "go"} {
		if _, err := exec.LookPath(map[string]string{"js": "node", "go": "go"}[tg]); err != nil {
			t.Skipf("%s runtime not in PATH", tg)
		}
		s := loadListing(t, "ch559_build_db_lexicon.rune")
		p, err := s.EmitProgram("main")
		if err != nil {
			t.Fatalf("emit: %v", err)
		}
		var src string
		var runner func(string) *exec.Cmd
		if tg == "js" {
			src, err = codegen.JS{}.Emit(p)
			runner = func(f string) *exec.Cmd { return exec.Command("node", f) }
		} else {
			src, err = codegen.Go{}.Emit(p)
			runner = func(f string) *exec.Cmd { return exec.Command("go", "run", f) }
		}
		if err != nil {
			t.Fatalf("[%s] emit: %v", tg, err)
		}
		dir := t.TempDir()
		copyTree(t, "testdata/lexdbfix", filepath.Join(dir, "lexdb"))
		f := filepath.Join(dir, "main."+tg)
		if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
		cmd := runner(f)
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("[%s] run: %v\n%s", tg, err, out)
		}
		got, err := os.ReadFile(filepath.Join(dir, "lexicon.sql"))
		if err != nil {
			t.Fatalf("[%s] no lexicon.sql: %v", tg, err)
		}
		if string(got) != string(want) {
			t.Errorf("[%s] lexicon.sql mismatch:\n--- got ---\n%s\n--- want ---\n%s", tg, got, want)
		}
	}
}
```
(NOTE: `main` builds `lexicon.db` too via `dbApply`; if `sqlite3` is absent the build still writes `lexicon.sql` first, and `dbApply` silently no-ops -- so this gate stays sqlite3-free as long as the `.sql` is written BEFORE `dbApply` runs. Confirm phase ordering: assemble `lexicon.sql` fully, then `dbApply`.)

- [ ] **Step 6: Add the sqlite3-guarded query gate**

Append to `harness/bible_ops_test.go`:
```go
func TestBibleBuildDbLexiconQuery(t *testing.T) {
	if _, err := exec.LookPath("sqlite3"); err != nil {
		t.Skip("sqlite3 CLI not in PATH")
	}
	want, err := os.ReadFile("testdata/lexdbfix_expected_rows.txt")
	if err != nil {
		t.Fatal(err)
	}
	for _, tg := range []string{"js", "go"} {
		if _, err := exec.LookPath(map[string]string{"js": "node", "go": "go"}[tg]); err != nil {
			t.Skipf("%s runtime not in PATH", tg)
		}
		s := loadListing(t, "ch559_build_db_lexicon.rune")
		p, err := s.EmitProgram("main")
		if err != nil {
			t.Fatalf("emit: %v", err)
		}
		var src string
		var runner func(string) *exec.Cmd
		if tg == "js" {
			src, err = codegen.JS{}.Emit(p)
			runner = func(f string) *exec.Cmd { return exec.Command("node", f) }
		} else {
			src, err = codegen.Go{}.Emit(p)
			runner = func(f string) *exec.Cmd { return exec.Command("go", "run", f) }
		}
		if err != nil {
			t.Fatalf("[%s] emit: %v", tg, err)
		}
		dir := t.TempDir()
		copyTree(t, "testdata/lexdbfix", filepath.Join(dir, "lexdb"))
		f := filepath.Join(dir, "main."+tg)
		if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
		cmd := runner(f)
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("[%s] run: %v\n%s", tg, err, out)
		}
		q := exec.Command("sqlite3", filepath.Join(dir, "lexicon.db"),
			"SELECT * FROM lexicon ORDER BY strong IS NULL, strong, lemma, translit, lang, pos, root")
		out, err := q.CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] query: %v\n%s", tg, err, out)
		}
		if strings.TrimSpace(string(out)) != strings.TrimSpace(string(want)) {
			t.Errorf("[%s] lexicon dump mismatch:\n--- got ---\n%s\n--- want ---\n%s", tg, out, want)
		}
	}
}
```

- [ ] **Step 7: Iterate until green + commit**

```
go test ./harness/ -run 'TestBibleBuildDbLexiconSql' -count=1 -v   # PASS js+go (.sql byte-identical)
go test ./harness/ -run 'TestBibleBuildDbLexiconQuery' -count=1 -v   # PASS js+go (or SKIP if sqlite3 absent)
go test ./harness/ -run 'TestListingsElaborateAndCheck/ch559_build_db_lexicon.rune' -count=1   # PASS
```

```bash
git add listings/ch559_build_db_lexicon.rune harness/testdata/lexdbfix harness/testdata/lexdbfix_expected.sql harness/testdata/lexdbfix_expected_rows.txt harness/bible_ops_test.go
git commit -m "$(printf 'feat(listings): ch559 build_db lexicon-table builder + fixture gates\n\nThe pipeline (foldDir extract 6 fields -> sortFile -> assemble DDL+INSERTs+index\n-> dbApply) emitting deterministic SQL, with the strong empty->NULL guard and\nsqlQuote escaping. Synthetic lexdbfix fixture -> exact lexicon.sql byte-identical\njs+go (always-on) + sqlite3-guarded query-equivalence on the loaded db\n(TestBibleBuildDbLexiconSql/Query). Greek/Hebrew lemmas exercise the raw-byte write.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 5: Env-gated query-equivalence acceptance gate (real lexicon)

**Files:**
- Modify: `harness/bible_ops_test.go` (the env-gated gate vs Python's `tokens.sqlite`)

**Interfaces:**
- Consumes: the Task-4 builder. Produces: `TestBibleLexiconQueryEquivalent` -- skipped unless `BIBLE_REPO` is set and `sqlite3` is present; otherwise builds the lexicon DB from the real lexicon and diffs the sorted table dump against Python's `tokens.sqlite`.

- [ ] **Step 1: Add the env-gated query-equivalence gate**

Append to `harness/bible_ops_test.go`:
```go
func TestBibleLexiconQueryEquivalent(t *testing.T) {
	repo := os.Getenv("BIBLE_REPO")
	if repo == "" {
		t.Skip("set BIBLE_REPO to run the full lexicon query-equivalence gate")
	}
	if _, err := exec.LookPath("sqlite3"); err != nil {
		t.Skip("sqlite3 CLI not in PATH")
	}
	const q = "SELECT * FROM lexicon ORDER BY strong IS NULL, strong, lemma, translit, lang, pos, root"
	// expected: Python's committed DB
	pyOut, err := exec.Command("sqlite3", filepath.Join(repo, "data/tokens.sqlite"), q).CombinedOutput()
	if err != nil {
		t.Fatalf("python db query failed: %v\n%s", err, pyOut)
	}
	pyCount, err := exec.Command("sqlite3", filepath.Join(repo, "data/tokens.sqlite"), "SELECT count(*) FROM lexicon").CombinedOutput()
	if err != nil {
		t.Fatal(err)
	}
	// build the rune DB on the GO target over the real lexicon
	s := loadListing(t, "ch559_build_db_lexicon.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	src, err := codegen.Go{}.Emit(p)
	if err != nil {
		t.Fatalf("emit go: %v", err)
	}
	dir := t.TempDir()
	cp := exec.Command("cp", "-r", filepath.Join(repo, "lexicon"), filepath.Join(dir, "lexdb"))
	if out, err := cp.CombinedOutput(); err != nil {
		t.Fatalf("copy lexicon: %v\n%s", err, out)
	}
	f := filepath.Join(dir, "main.go")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	build := exec.Command("go", "run", f)
	build.Dir = dir
	if out, err := build.CombinedOutput(); err != nil {
		t.Fatalf("rune build failed: %v\n%s", err, out)
	}
	runeOut, err := exec.Command("sqlite3", filepath.Join(dir, "lexicon.db"), q).CombinedOutput()
	if err != nil {
		t.Fatalf("rune db query failed: %v\n%s", err, runeOut)
	}
	runeCount, err := exec.Command("sqlite3", filepath.Join(dir, "lexicon.db"), "SELECT count(*) FROM lexicon").CombinedOutput()
	if err != nil {
		t.Fatal(err)
	}
	if strings.TrimSpace(string(runeCount)) != strings.TrimSpace(string(pyCount)) {
		t.Fatalf("lexicon COUNT(*) mismatch: rune %s, python %s", runeCount, pyCount)
	}
	if string(runeOut) != string(pyOut) {
		// report first differing line, not the whole dump
		rl, pl := strings.Split(string(runeOut), "\n"), strings.Split(string(pyOut), "\n")
		t.Errorf("lexicon dump mismatch: rune %d rows, python %d rows", len(rl), len(pl))
		for i := 0; i < len(rl) && i < len(pl); i++ {
			if rl[i] != pl[i] {
				t.Errorf("first diff at row %d:\n rune:   %s\n python: %s", i+1, rl[i], pl[i])
				break
			}
		}
	}
}
```

- [ ] **Step 2: Run the env-gated gate against the real repo**

```bash
BIBLE_REPO="$HOME/matt/bible" go test ./harness/ -run 'TestBibleLexiconQueryEquivalent' -count=1 -v -timeout 900s
```
Expected: PASS -- the rune-built lexicon table is query-equivalent (same `COUNT(*)` and same sorted dump) to Python's `tokens.sqlite` on the Go target. ~5min (dominated by the 23,681 file reads). Debug any first-diff the test reports (likely a field-rendering or empty-vs-NULL detail). Confirm the skip path: same command without `BIBLE_REPO` -> SKIP.

- [ ] **Step 3: Run the full bible + listings gates + commit**

```
go test ./harness/ -run 'TestBible' -count=1 -v   # all TestBible* PASS (env-gated SKIPs without env)
go test ./harness/ -run 'TestListingsElaborateAndCheck' -count=1   # ok (incl ch556-559)
```

```bash
git add harness/bible_ops_test.go
git commit -m "$(printf 'test(harness): env-gated lexicon query-equivalence gate vs python tokens.sqlite\n\nWith BIBLE_REPO + sqlite3, builds the ch559 lexicon DB from the real 23,681-file\nlexicon and diffs the sorted table dump + COUNT(*) against python build_db.py\noutput on the Go target; skipped otherwise. Query-equivalence (a .sqlite is not\nbyte-deterministic). Milestone C acceptance.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Self-Review

**1. Spec coverage.** Spec's two new ops -> Task 2 (`sqlQuote`) + Task 3 (`dbApply`), each a per-op gate js+go (acceptance crit 2; `dbApply`'s query assertion sqlite3-guarded). The JS write-encoding fix -> Task 1 (crit 1, with the non-ASCII regression). The synthetic-fixture `.sql`-text gate (always-on) + the sqlite3-guarded fixture query gate -> Task 4 (crit 3). The env-gated query-equivalence acceptance vs `tokens.sqlite` -> Task 5 (crit 4). Kernel-frozen / Go+JS-only / no-hash-bump (crit 1) are Global Constraints + each op's emit step. The listings gate (crit 5) runs in Task 5 Step 3. Determinism (spec §6): constant DDL/index templates + `sortFile` + byte-level `sqlQuote` are encoded in Tasks 2/4. The `strong` empty->NULL guard (spec §4, risk §11) is Task 4's field semantics.

**2. Placeholder scan.** Tasks 1-3 + 5 carry complete verbatim Go/JS/test/listing code + exact commands and expected output. Task 4's builder `main` is a deliberate REFERENCE (the builder cannot be pre-verified before its ops exist -- the Milestone-B precedent), but its acceptance is fully pinned: exact field semantics, the constant templates, the exact fixture files, the exact expected `.sql`, and the exact query. Every other step is concrete.

**3. Type consistency.** Op names are identical across `ioPrims`/`streamPrims`/the bodies/the `foreign` decls: `sqlQuote : Nat -> Nat` (pure, two-level: accessor + one arg, no `_u`); `dbApply : Nat -> Nat -> IO Unit` (accessor + db + sql + `_u`, returns `nil`/`null`). Paths/values use the `__s2h`/`__h2s` codec (NOT Bin marshalling) -- consistent with `sortFile`/`openWrite`. The JS write fix targets the three exact bodies (`writeFileCode`/`writeChunk`/`sortFile`) and is ASCII-safe (existing gates unaffected). The builder consumes only declared ops; the INSERT column order `strong,lemma,translit,lang,pos,root` matches the DDL and the `SELECT *` dump order. The `sqlite3`-guard (`exec.LookPath`) and `BIBLE_REPO` skip mirror the existing harness patterns. Chapters ch556-559 are sequential and free.
