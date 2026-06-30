# Bible Ops Cross-Backend -- Tier 2 (JVM) + Go foldLines divergence fix Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make Go's `foldLines` agree with the other backends on line-splitting (the last latent divergence), then port the bible host-op family to the JVM backend so all SIX source backends (js/go/py/rust/erl/jvm) produce byte-identical builder output.

**Architecture:** Part A fixes Go's `foldLines` (it uses `bufio.ScanLines`, which strips a trailing `\r` and caps lines at 1MB; the other four split on `\n` only) to slurp-and-split like the others, locked by a CRLF cross-backend fixture. Part B ports the 11 ops to `codegen/jvm.go` (Java 25) -- the byte-exact trap is that Java `String` is UTF-16, so every byte<->String conversion uses `ISO_8859_1` (never `getBytes(UTF-8)`); the existing latin1-as-String codec is byte-safe. JVM joins the Tier-1 conformance harness for a 6-way byte-identity lock.

**Tech Stack:** Go codegen (`codegen/golang.go`, `codegen/jvm.go`), `harness/{bible_conformance_test.go, backend_conformance_test.go's findJava25}`, one new listing (ch560). Toolchains: node/go/python3/rustc/escript/sqlite3 + **Java 25** (`~/.asdf/installs/java/temurin-25*` -- `findJava25()` resolves it). This is **Tier 2** of the tiered rollout (native C/LLVM and WASM are follow-on).

## Global Constraints

- Kernel FROZEN: changes ONLY in `codegen/{golang,jvm}.go` and `harness/` + `listings/`. No core/store/elaborate edit. No hash-format bump. (golang.go and jvm.go are codegen backends, editable -- the freeze is core/store/elaborate.)
- The Go+JS op bodies are the SEMANTIC CONTRACT; the JVM bodies must compute the SAME function. After Part A, all backends split foldLines on `\n` only (keep `\r`, no length cap) -- the uniform contract.
- BYTE-EXACT discipline: JVM carries the packed byte-string as a Java `String` where each `char` is one byte 0-255 (the latin1-as-String trick, like JS). The codec `__s2h`/`__h2s` is byte-safe. Every file/content byte<->String conversion MUST use `java.nio.charset.StandardCharsets.ISO_8859_1` -- NEVER `getBytes()`/`new String(bytes)` default (UTF-8), `Files.readString`, or `Files.writeString` (they corrupt non-ASCII).
- JVM codec gate: broaden the `__s2h`/`__h2s` emission from `if usesLiveKV(p)` to `if usesLiveKV(p) || usesFileEnv(p) || usesStream(p)` (mirrors golang.go) -- also fixes a latent `printStrCode` compile bug.
- JVM constructor cells: `new VCtor(tag, "name", new V[]{unit(), ...})` (erased type-arg at args[0]=`unit()`). List nil=tag0 `{unit()}`/cons=tag1 `{unit(),h,t}`; Option none=tag0 `{unit()}`/some=tag1 `{unit(),v}`.
- JVM IO `_u` world token: trailing `fun(_u -> ...)`, forced by `ap(action, unit())`. `IO Unit` returns `UNIT`; openWrite/writeChunk return the handle. Higher-order apply: `ap(ap(ap(step, s), __h2s(line)), unit())`.
- JVM opaque handle: a `VNat` token into a `static ConcurrentHashMap<Long, OutputStream> __wh` + `static long __whId` counter (the `__socks`/`__sockId` precedent). Foreign accessor methods are `static V <name>() { ... }`, called as `<name>()` (NO `_d` suffix).
- No em-dashes / en-dashes; use `--`. Conventional Commits; trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- No `rune check`: use `go run ./cmd/rune emit|run FILE NAME --target go|jvm|js|py|rs|erl`. (JVM target alias: `jvm` or `java`.)
- Chapter ch560 is free (highest existing is ch559).

---

### Task 1: Go foldLines splits on `\n` only (drop the `\r`-strip + 1MB cap) + CRLF cross-backend lock

**Files:**
- Modify: `codegen/golang.go` (the `foldLines` body)
- Create: `listings/ch560_crlf_lines.rune` + `harness/testdata/crlf.txt`
- Modify: `harness/bible_conformance_test.go` (add the CRLF agreement gate)

**Interfaces:**
- Produces: Go `foldLines` now slurps the file and splits on `\n` (keeps `\r`, no length cap), matching js/py/rust/beam exactly.

> **Why:** the final Tier-1 review found Go is the lone 1-of-5 outlier: `bufio.ScanLines` strips a trailing `\r` and `sc.Buffer(...,1MB)` caps a line at 1MB. js/py/rust/beam read the whole file and split on `\n` only. This makes Go match them. `TestStreamBoundedMemory` asserts only the printed count (not heap), so the slurp does not break it.

- [ ] **Step 1: Write the CRLF fixture + listing (the failing cross-backend lock)**

Create `harness/testdata/crlf.txt` containing exactly these bytes (two CRLF lines + one LF line, trailing LF):
```
alpha<CR><LF>beta<CR><LF>gamma<LF>
```
i.e. the literal bytes `alpha\r\nbeta\r\ngamma\n`. (Create it so the `\r` are real carriage returns -- e.g. `printf 'alpha\r\nbeta\r\ngamma\n' > harness/testdata/crlf.txt`.)

Create `listings/ch560_crlf_lines.rune`:
```
-- Chapter 560 -- foldLines splits on \n only, keeping \r (cross-backend lock).
--
-- foldLines over a CRLF fixture, summing each line's byteLen. With \n-only splitting (\r kept):
-- "alpha\r"(6) + "beta\r"(5) + "gamma"(5) = 16 on EVERY backend. The old Go bufio.ScanLines
-- stripped \r -> 14, the lone outlier this fixes. Run from harness/testdata (relative path).

data Nat  : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Bytes : U is bytes : Nat -> Bytes end
String : U is Bytes end
codeOf : Bytes -> Nat is fn (s : Bytes) is case s of | bytes n -> n end end end

add : Nat -> Nat -> Nat is
  fn (a : Nat) is
    NatElim (fn (w : Nat) is Nat -> Nat end)
      (fn (b : Nat) is b end)
      (fn (k : Nat) (ih : Nat -> Nat) is fn (b : Nat) is succ (ih b) end end)
      a
  end
end

foreign foldLines : (S : U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S end
foreign byteLen   : Nat -> Nat end
foreign printNat  : Nat -> IO Nat end

sumStep : Nat -> Nat -> IO Nat is fn (acc : Nat) (line : Nat) is pureIO Nat (add acc (byteLen line)) end end

main : IO Nat is
  bindIO Nat Nat (foldLines Nat (codeOf "crlf.txt") sumStep zero) (fn (n : Nat) is printNat n end)
end
```

- [ ] **Step 2: Verify the bug (Go differs from JS before the fix)**

```bash
go build -o /tmp/runeT ./cmd/rune
( cd harness/testdata && /tmp/runeT run ../../listings/ch560_crlf_lines.rune main --target js | tail -1 )   # 16
( cd harness/testdata && /tmp/runeT run ../../listings/ch560_crlf_lines.rune main --target go | tail -1 )   # 14 (BUG: strips \r)
```
Expected BEFORE the fix: js `16`, go `14`. Confirms the divergence.

- [ ] **Step 3: Fix the Go foldLines body**

In `codegen/golang.go`, replace the `foldLines` body (currently `os.Open` + `bufio.NewScanner` + `sc.Buffer(...)` + `sc.Text()`):
```go
	if usesForeign(p, "foldLines") {
		b.WriteString("func foldLines() any { return func(_S any) any { return func(path any) any { return func(step any) any { return func(s0 any) any { return func(_u any) any { data, err := os.ReadFile(__s2h(path)); if err != nil { return s0 }; lines := strings.Split(string(data), \"\\n\"); if len(lines) > 0 && lines[len(lines)-1] == \"\" { lines = lines[:len(lines)-1] }; s := s0; for _, ln := range lines { s = ap(ap(ap(step, s), __h2s(ln)), nil) }; return s } } } } } }\n")
	}
```
(`os` and `strings` are already imported. `bufio` stays imported -- `__resp`/TLS uses it. The import gate that adds `bufio` for foldLines can stay or be dropped; leaving it is harmless, but if a build complains of an unused import, drop `bufio` from the foldLines import gate -- verify `go vet`.)

- [ ] **Step 4: Verify Go now matches (16) + no import breakage**

```bash
go build -o /tmp/runeT ./cmd/rune 2>&1 | tail -2   # builds clean (no unused-import error)
( cd harness/testdata && /tmp/runeT run ../../listings/ch560_crlf_lines.rune main --target go | tail -1 )   # 16
```
If `go build` reports `bufio imported and not used`, remove the `if usesForeign(p, "foldLines") { addImp("os", "bufio") }` gate's `"bufio"` (keep `"os"`), or confirm another op still imports bufio.

- [ ] **Step 5: Add the CRLF cross-backend lock**

Append to `harness/bible_conformance_test.go`:
```go
func TestBibleConformanceCRLF(t *testing.T) {
	// foldLines splits on \n only and keeps \r on EVERY backend: sum of line byteLens = 16.
	assertBibleAgreeFromTestdata(t, "ch560_crlf_lines.rune", "main", "16\n16")
}
```

- [ ] **Step 6: Run the gates + commit**

```
go test ./harness/ -run 'TestBibleConformanceCRLF' -count=1 -v   # PASS (all available backends = 16\n16)
go test ./harness/ -run 'TestStreamBoundedMemory' -count=1   # still PASS (count only; -short skips it -- run without -short)
go test ./harness/ -run 'TestListingsElaborateAndCheck/ch560_crlf_lines.rune' -count=1   # PASS
go test ./harness/ -run 'TestBibleConformanceFold' -count=1   # still PASS (ch549/ch554 unaffected -- LF corpus)
```
```bash
git add codegen/golang.go listings/ch560_crlf_lines.rune harness/testdata/crlf.txt harness/bible_conformance_test.go
git commit -m "$(printf 'fix(codegen): Go foldLines splits on newline only, matching the other backends\n\nGo bufio.ScanLines stripped a trailing CR and capped lines at 1MB; js/py/rust/beam\nsplit on LF only (keep CR, no cap). Go now slurps + splits on LF to match -- the\nlast latent cross-backend divergence. ch560 CRLF fixture locks all backends to 16\n(TestBibleConformanceCRLF). TestStreamBoundedMemory (count-only) still passes.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 2: JVM codec gate + pure ops + higher-order ops

**Files:**
- Modify: `codegen/jvm.go` (broaden codec gate; add byteLen/splitOn/jsonStrField/sqlQuote/foldLines/foldDir)
- Modify: `harness/bible_conformance_test.go` (add a JVM-direct verification gate)

**Interfaces:**
- Produces: `byteLen`/`splitOn`/`jsonStrField`/`sqlQuote`/`foldLines`/`foldDir` on the JVM backend, computing identically to the Go/JS bodies. (JVM is NOT yet added to the shared `bibleBackends()` table -- that happens in Task 4 once every op exists, so the existing shared gates do not break.)

- [ ] **Step 1: Broaden the JVM codec gate**

In `codegen/jvm.go`, the `__s2h`/`__h2s` codec is emitted only under `if usesLiveKV(p)`. Change that condition to:
```go
	if usesLiveKV(p) || usesFileEnv(p) || usesStream(p) {
```
(so the codec emits whenever a bible/stream op is present; also fixes the latent `printStrCode`-without-liveKV compile gap).

- [ ] **Step 2: Emit the four pure ops on JVM**

In `codegen/jvm.go`, after the `usesLiveKV` block (before the `for _, d := range p.Datas` data-decl loop), add:
```go
	if usesForeign(p, "byteLen") {
		b.WriteString("  static V byteLen() { return fun(c -> new VNat(java.math.BigInteger.valueOf(__s2h(c).length()))); }\n")
	}
	if usesForeign(p, "splitOn") {
		b.WriteString("  static V splitOn() { return fun(sep -> fun(c -> { String data = __s2h(c); int sb = _nat(sep).intValue(); java.util.List<String> parts = new java.util.ArrayList<>(); int start = 0; for (int i = 0; i < data.length(); i++) { if (data.charAt(i) == sb) { parts.add(data.substring(start, i)); start = i + 1; } } parts.add(data.substring(start)); V lst = new VCtor(0, \"nil\", new V[]{unit()}); for (int i = parts.size() - 1; i >= 0; i--) lst = new VCtor(1, \"cons\", new V[]{unit(), __h2s(parts.get(i)), lst}); return lst; })); }\n")
	}
	if usesForeign(p, "jsonStrField") {
		b.WriteString("  static V jsonStrField() { return fun(field -> fun(doc -> { String fn = __s2h(field), ds = __s2h(doc); String needle = \"\\\"\" + fn + \"\\\"\"; V none = new VCtor(0, \"none\", new V[]{unit()}); int i = ds.indexOf(needle); if (i < 0) return none; int j = i + needle.length(); while (j < ds.length() && (ds.charAt(j) == ' ' || ds.charAt(j) == '\\t' || ds.charAt(j) == ':')) j++; if (j < ds.length() && ds.charAt(j) == '\"') { j++; int k = j; while (k < ds.length() && ds.charAt(k) != '\"') k++; return new VCtor(1, \"some\", new V[]{unit(), __h2s(ds.substring(j, k))}); } return none; })); }\n")
	}
	if usesForeign(p, "sqlQuote") {
		b.WriteString("  static V sqlQuote() { return fun(s -> { String in = __s2h(s); StringBuilder o = new StringBuilder(); o.append('\\''); for (int i = 0; i < in.length(); i++) { char ch = in.charAt(i); if (ch == '\\'') o.append('\\''); o.append(ch); } o.append('\\''); return __h2s(o.toString()); }); }\n")
	}
```
(`__s2h(c)` is a String whose chars are bytes 0-255, so `.length()` = byte count, `.charAt(i)` = the byte. `_nat(sep)` extracts the BigInteger. `unit()` is the erased type-arg slot. NOTE: confirm `fun`, `_nat`, `unit()`/`UNIT`, `VCtor`, `VNat` against jvm.go -- they are used verbatim by the existing socket/codec/ctor code; if `unit()` does not exist use the `UNIT` constant.)

- [ ] **Step 3: Emit the two higher-order ops on JVM**

In `codegen/jvm.go`, after the pure ops, add (foldDir uses a static helper `_foldwalk`):
```go
	if usesForeign(p, "foldLines") {
		b.WriteString("  static V foldLines() { return fun(_S -> fun(path -> fun(step -> fun(s0 -> fun(_u -> { byte[] data; try { data = java.nio.file.Files.readAllBytes(java.nio.file.Path.of(__s2h(path))); } catch (Exception e) { return s0; } String s = new String(data, java.nio.charset.StandardCharsets.ISO_8859_1); java.util.List<String> lines = new java.util.ArrayList<>(); int start = 0; for (int i = 0; i < s.length(); i++) { if (s.charAt(i) == '\\n') { lines.add(s.substring(start, i)); start = i + 1; } } lines.add(s.substring(start)); if (!lines.isEmpty() && lines.get(lines.size() - 1).isEmpty()) lines.remove(lines.size() - 1); V acc = s0; for (String ln : lines) acc = ap(ap(ap(step, acc), __h2s(ln)), unit()); return acc; }))))); }\n")
	}
	if usesForeign(p, "foldDir") {
		b.WriteString("  static void _foldwalk(String dir, String sfx, V step, V[] box) { java.io.File d = new java.io.File(dir); java.io.File[] ents = d.listFiles(); if (ents == null) return; java.util.Arrays.sort(ents, (a, b2) -> a.getName().compareTo(b2.getName())); for (java.io.File e : ents) { if (e.isDirectory()) _foldwalk(e.getPath(), sfx, step, box); else if (e.getPath().endsWith(sfx)) { try { byte[] data = java.nio.file.Files.readAllBytes(e.toPath()); box[0] = ap(ap(ap(step, box[0]), __h2s(new String(data, java.nio.charset.StandardCharsets.ISO_8859_1))), unit()); } catch (Exception ex) {} } } }\n")
		b.WriteString("  static V foldDir() { return fun(_S -> fun(dir -> fun(suf -> fun(step -> fun(s0 -> fun(_u -> { String sfx = __s2h(suf); V[] box = new V[]{s0}; _foldwalk(__s2h(dir), sfx, step, box); return box[0]; })))))); }\n")
	}
```
(Read RAW bytes via `Files.readAllBytes`, decode with `ISO_8859_1` (byte-exact), split on `'\n'` keeping `'\r'`, drop trailing empty. `box` is a mutable single-cell array so the static recursion threads state. Sort entries by name -- matches the other backends' lexical order. `Path.of(__s2h(path))` -- the path is ASCII.)

- [ ] **Step 4: Verify each JVM op via direct emit->javac->java**

Build the binary once, then for each demo listing emit JVM source, compile with `javac --release 25`, run with `java`. Use the java-25 toolchain (asdf temurin-25). Helper:
```bash
JAVAC=$(ls ~/.asdf/installs/java/temurin-25*/bin/javac | head -1); JAVA=$(dirname "$JAVAC")/java
runjvm() { D=$(mktemp -d); go run ./cmd/rune emit "$1" "$2" --target jvm > "$D/main.java" 2>/dev/null && "$JAVAC" --release 25 -d "$D" "$D/main.java" 2>/dev/null && ( cd "${3:-$D}" && "$JAVA" -cp "$D" main ); }
```
Run + check (pure ops from anywhere; foldLines/foldDir from harness/testdata):
```bash
runjvm listings/ch551_json_field.rune strongLen            # 5
runjvm listings/ch557_sql_quote.rune quoteEmbedded         # 6
runjvm listings/ch549_conllu_count.rune main harness/testdata   # 11\n11
runjvm listings/ch554_fold_dir.rune main harness/testdata       # 3\n3
runjvm listings/ch560_crlf_lines.rune main harness/testdata     # 16\n16
```
Each must match the cross-backend value. A mismatch is a real bug (the cell shape, the ISO_8859_1 decode, the apply ABI) -- debug it.

- [ ] **Step 5: Add a JVM-direct conformance gate**

Append to `harness/bible_conformance_test.go` (a focused JVM gate that emits+compiles+runs; uses `findJava25` from `backend_conformance_test.go`, same package):
```go
// runJVMListing emits a listing to JVM, compiles with javac --release 25, runs it from
// optional cwd, returns trimmed stdout. Skips if Java 25 is absent.
func runJVMListing(t *testing.T, listing, main, cwd string) (string, bool) {
	t.Helper()
	javac, java, ok := findJava25()
	if !ok {
		return "", false
	}
	s := loadListing(t, listing)
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("%s emit-program: %v", listing, err)
	}
	src, err := codegen.JVM{}.Emit(p)
	if err != nil {
		t.Fatalf("%s jvm emit: %v", listing, err)
	}
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "main.java"), []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command(javac, "--release", "25", "-d", dir, filepath.Join(dir, "main.java")).CombinedOutput(); err != nil {
		t.Fatalf("%s javac: %v\n%s", listing, err, out)
	}
	cmd := exec.Command(java, "-cp", dir, "main")
	if cwd != "" {
		cmd.Dir = cwd
	}
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("%s java run: %v", listing, err)
	}
	return strings.TrimSpace(string(out)), true
}

func TestBibleJVMPureFold(t *testing.T) {
	cases := []struct{ listing, main, cwd, want string }{
		{"ch551_json_field.rune", "strongLen", "", "5"},
		{"ch557_sql_quote.rune", "quoteEmbedded", "", "6"},
		{"ch549_conllu_count.rune", "main", "testdata", "11\n11"},
		{"ch554_fold_dir.rune", "main", "testdata", "3\n3"},
		{"ch560_crlf_lines.rune", "main", "testdata", "16\n16"},
	}
	ran := false
	for _, c := range cases {
		got, ok := runJVMListing(t, c.listing, c.main, c.cwd)
		if !ok {
			t.Skip("Java 25 not available")
		}
		ran = true
		if got != c.want {
			t.Errorf("jvm %s/%s = %q, want %q", c.listing, c.main, got, c.want)
		}
	}
	if !ran {
		t.Skip("Java 25 not available")
	}
}
```

- [ ] **Step 6: Run the gate + commit**

```
go test ./harness/ -run 'TestBibleJVMPureFold' -count=1 -v   # PASS (or SKIP if Java 25 absent)
```
```bash
git add codegen/jvm.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): bible pure + higher-order ops (byteLen/splitOn/jsonStrField/sqlQuote/foldLines/foldDir) on JVM\n\nPorts the pure and higher-order bible ops to the JVM backend with the codec gate\nbroadened to usesStream; byte-exact via ISO_8859_1 (Java String is UTF-16).\nTestBibleJVMPureFold verifies 5/6/11/3/16 on Java 25. No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 3: JVM write-stream (Handle + open/write/close + sortFile) + dbApply

**Files:**
- Modify: `codegen/jvm.go` (Handle + openWrite/writeChunk/closeWrite + sortFile + dbApply)
- Modify: `harness/bible_conformance_test.go` (extend the JVM-direct gate)

**Interfaces:**
- Produces: `Handle` foreign type + `openWrite`/`writeChunk`/`closeWrite` + `sortFile` + `dbApply` on JVM. Opaque handle = a `VNat` token into a static `ConcurrentHashMap<Long, OutputStream>`.

- [ ] **Step 1: Emit the write-stream + sortFile + dbApply bodies on JVM**

In `codegen/jvm.go`, after the higher-order ops, add (the handle map + counter emit alongside `openWrite`):
```go
	if usesForeign(p, "openWrite") || usesForeign(p, "writeChunk") || usesForeign(p, "closeWrite") {
		b.WriteString("  static java.util.concurrent.ConcurrentHashMap<Long, java.io.OutputStream> __wh = new java.util.concurrent.ConcurrentHashMap<>();\n")
		b.WriteString("  static long __whId = 0;\n")
	}
	if usesForeign(p, "Handle") {
		b.WriteString("  static V Handle() { return UNIT; }\n")
	}
	if usesForeign(p, "openWrite") {
		b.WriteString("  static V openWrite() { return fun(path -> fun(_u -> { try { long id = ++__whId; java.io.OutputStream os = new java.io.FileOutputStream(__s2h(path)); __wh.put(id, os); return new VNat(java.math.BigInteger.valueOf(id)); } catch (Exception e) { return new VNat(java.math.BigInteger.ZERO); } })); }\n")
	}
	if usesForeign(p, "writeChunk") {
		b.WriteString("  static V writeChunk() { return fun(h -> fun(c -> fun(_u -> { try { java.io.OutputStream os = __wh.get(_nat(h).longValue()); if (os != null) os.write((__s2h(c) + \"\\n\").getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)); } catch (Exception e) {} return h; }))); }\n")
	}
	if usesForeign(p, "closeWrite") {
		b.WriteString("  static V closeWrite() { return fun(h -> fun(_u -> { try { java.io.OutputStream os = __wh.remove(_nat(h).longValue()); if (os != null) os.close(); } catch (Exception e) {} return UNIT; })); }\n")
	}
	if usesForeign(p, "sortFile") {
		b.WriteString("  static V sortFile() { return fun(inp -> fun(outp -> fun(_u -> { try { byte[] data = java.nio.file.Files.readAllBytes(java.nio.file.Path.of(__s2h(inp))); String s = new String(data, java.nio.charset.StandardCharsets.ISO_8859_1); java.util.List<String> lines = new java.util.ArrayList<>(); int start = 0; for (int i = 0; i < s.length(); i++) { if (s.charAt(i) == '\\n') { lines.add(s.substring(start, i)); start = i + 1; } } lines.add(s.substring(start)); if (!lines.isEmpty() && lines.get(lines.size() - 1).isEmpty()) lines.remove(lines.size() - 1); java.util.Collections.sort(lines); StringBuilder o = new StringBuilder(); for (String ln : lines) { o.append(ln); o.append('\\n'); } java.nio.file.Files.write(java.nio.file.Path.of(__s2h(outp)), o.toString().getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)); } catch (Exception e) { try { java.nio.file.Files.write(java.nio.file.Path.of(__s2h(outp)), new byte[0]); } catch (Exception e2) {} } return UNIT; }))); }\n")
	}
	if usesForeign(p, "dbApply") {
		b.WriteString("  static V dbApply() { return fun(db -> fun(sql -> fun(_u -> { try { new ProcessBuilder(\"sqlite3\", __s2h(db), \".read \" + __s2h(sql)).start().waitFor(); } catch (Exception e) {} return UNIT; }))); }\n")
	}
```
(`__wh`/`__whId` mirror the existing `__socks`/`__sockId` handle pattern. `.getBytes(ISO_8859_1)` converts the latin1-as-String back to raw bytes -- byte-exact. `Collections.sort(lines)` on Strings whose chars are all 0-255 is bytewise (`String.compareTo` is unsigned-char order == byte order), matching Go `sort.Strings`. `ProcessBuilder` is in `java.lang` (no import). NOTE: confirm `UNIT` vs `unit()` -- `Float()` returns `UNIT`, so IO-Unit returns use `UNIT`.)

- [ ] **Step 2: Verify write-stream + dbApply on JVM via direct run**

Using the `runjvm` helper from Task 2 Step 4:
```bash
runjvm listings/ch552_write_stream.rune main "$(mktemp -d)"   # 2\n2  (write+readback; run from a writable temp dir)
runjvm listings/ch553_sort_file.rune main "$(mktemp -d)"      # 5\n5  (sortFile)
# dbApply: build the db then query it (sqlite3 present)
D=$(mktemp -d); go run ./cmd/rune emit listings/ch558_db_apply.rune main --target jvm > "$D/main.java" && "$JAVAC" --release 25 -d "$D" "$D/main.java" && ( cd "$D" && "$JAVA" -cp "$D" main ); sqlite3 "$D/ch558.db" "SELECT count(*) FROM t"   # 2
```
(ch552/ch553 write+read relative files in the cwd -- run from a fresh temp dir.)

- [ ] **Step 3: Extend the JVM-direct gate**

In `harness/bible_conformance_test.go`, add a JVM write-stream + dbApply gate (write-stream listings run in a temp cwd; dbApply builds + queries):
```go
func TestBibleJVMWriteStreamDb(t *testing.T) {
	if _, _, ok := findJava25(); !ok {
		t.Skip("Java 25 not available")
	}
	// ch552 / ch553 write+read relative files -- run each in its own temp cwd.
	for _, c := range []struct{ listing, want string }{
		{"ch552_write_stream.rune", "2\n2"},
		{"ch553_sort_file.rune", "5\n5"},
	} {
		got, ok := runJVMListing(t, c.listing, "main", t.TempDir())
		if !ok {
			t.Skip("Java 25 not available")
		}
		if got != c.want {
			t.Errorf("jvm %s = %q, want %q", c.listing, got, c.want)
		}
	}
	// ch558 dbApply: build ch558.db in a temp cwd, then query count(*) -> 2.
	if _, err := exec.LookPath("sqlite3"); err != nil {
		t.Skip("sqlite3 not in PATH")
	}
	javac, java, _ := findJava25()
	s := loadListing(t, "ch558_db_apply.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.JVM{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "main.java"), []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command(javac, "--release", "25", "-d", dir, filepath.Join(dir, "main.java")).CombinedOutput(); err != nil {
		t.Fatalf("javac: %v\n%s", err, out)
	}
	run := exec.Command(java, "-cp", dir, "main")
	run.Dir = dir
	if out, err := run.CombinedOutput(); err != nil {
		t.Fatalf("java run: %v\n%s", err, out)
	}
	q, err := exec.Command("sqlite3", filepath.Join(dir, "ch558.db"), "SELECT count(*) FROM t").CombinedOutput()
	if err != nil {
		t.Fatalf("query: %v\n%s", err, q)
	}
	if got := strings.TrimSpace(string(q)); got != "2" {
		t.Errorf("jvm ch558 db count = %q, want 2", got)
	}
}
```

- [ ] **Step 4: Run the gate + commit**

```
go test ./harness/ -run 'TestBibleJVMWriteStreamDb' -count=1 -v   # PASS (or SKIP)
```
```bash
git add codegen/jvm.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): bible write-stream + sortFile + dbApply on JVM\n\nHandle foreign type + open/write/close (a VNat token into a ConcurrentHashMap of\nOutputStream, the __socks precedent) + bytewise sortFile + dbApply (ProcessBuilder),\nbyte-exact via ISO_8859_1. TestBibleJVMWriteStreamDb verifies 2/5 + db count 2 on\nJava 25. No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 4: Add JVM to the divergence-lock (6-way byte-identity)

**Files:**
- Modify: `harness/bible_conformance_test.go` (`bibleBackends()` appends JVM conditionally)

**Interfaces:**
- Consumes: every JVM op (Tasks 2-3). Produces: JVM in the shared `bibleBackends()` table so ALL conformance gates (Pure/Fold/WriteStream/DbApply/Builders/CRLF/RealData) run JVM too -- the 6-way byte-identity lock.

- [ ] **Step 1: Append JVM to `bibleBackends()`**

In `harness/bible_conformance_test.go`, change `bibleBackends()` to build the slice and conditionally append JVM (using `findJava25` from `backend_conformance_test.go`, same package). The JVM `bibleBackend` mirrors the `backend_conformance` jvm entry: compile `javac --release 25 -d <dir> <src>`, run `java -cp <dir> main` (the run func derives `<dir>` from `filepath.Dir(out)`, since `runBibleBackend` passes the compiled-bin path as the run arg):
```go
func bibleBackends() []bibleBackend {
	bks := []bibleBackend{
		{"js", "node", "js", func(p codegen.Program) (codegen.TargetSource, error) { return codegen.JS{}.Emit(p) },
			func(f string) *exec.Cmd { return exec.Command("node", f) }, nil},
		{"go", "go", "go", func(p codegen.Program) (codegen.TargetSource, error) { return codegen.Go{}.Emit(p) },
			func(f string) *exec.Cmd { return exec.Command("go", "run", f) }, nil},
		{"py", "python3", "py", func(p codegen.Program) (codegen.TargetSource, error) { return codegen.Py{}.Emit(p) },
			func(f string) *exec.Cmd { return exec.Command("python3", f) }, nil},
		{"rs", "rustc", "rs", func(p codegen.Program) (codegen.TargetSource, error) { return codegen.Rust{}.Emit(p) },
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-o", out, src) }},
		{"erl", "escript", "erl", func(p codegen.Program) (codegen.TargetSource, error) { return codegen.Beam{}.Emit(p) },
			func(f string) *exec.Cmd { return exec.Command("escript", f) }, nil},
	}
	if javac, java, ok := findJava25(); ok {
		bks = append(bks, bibleBackend{"jvm", javac, "java",
			func(p codegen.Program) (codegen.TargetSource, error) { return codegen.JVM{}.Emit(p) },
			func(out string) *exec.Cmd { return exec.Command(java, "-cp", filepath.Dir(out), "main") },
			func(src, out string) *exec.Cmd { return exec.Command(javac, "--release", "25", "-d", filepath.Dir(out), src) }})
	}
	return bks
}
```
(The `bibleBackend.bin` for JVM is the `javac` absolute path -- `runBibleBackend`/the assert helpers `exec.LookPath(bk.bin)` it; an absolute path that exists passes `LookPath`. Verify `LookPath` accepts an absolute path -- it does when the file is executable. If not, the I1 skip-accumulator records it and the gate goes inconclusive rather than false-green, which is acceptable.)

- [ ] **Step 2: Verify ALL conformance gates now run 6-way**

```
go test ./harness/ -run 'TestBibleConformance' -count=1 -v   # PASS -- Pure/Fold/WriteStream/DbApply/Builders/CRLF all now include jvm (6 backends), byte-identical
```
The `TestBibleConformanceBuilders` byte-identity assertion now compares JVM's `shared-root.out` + `lexicon.sql` against the other five -- they MUST be byte-identical. A JVM divergence here is a real bug in the Task 2-3 bodies (debug the byte-exactness / cell / sort), not a Task 4 issue.

- [ ] **Step 3: Independent 6-way builder byte-check + the sampled real-data gate**

```bash
# 6-way sha256 of both builder outputs over the fixtures (js/go/py/erl/rs/jvm identical):
go test ./harness/ -run 'TestBibleConformanceBuilders' -count=1 -v
# sampled real-data, now 6 backends (jvm joins; ~real Greek/Hebrew, minutes):
BIBLE_REPO="$HOME/matt/bible" go test ./harness/ -run 'TestBibleConformanceRealData' -count=1 -v -timeout 900s   # PASS, all 6 byte-identical
```

- [ ] **Step 4: Full regression + commit**

```
go test ./harness/ -run 'TestBible' -count=1   # all bible gates (Go/JS Milestone A/B/C + conformance) PASS
go test ./harness/ -run 'TestListingsElaborateAndCheck' -count=1   # ok (incl ch560)
go test ./harness/ -run 'TestBackendConformance' -count=1   # the existing 8-backend gate unaffected
```
```bash
git add harness/bible_conformance_test.go
git commit -m "$(printf 'test(harness): add JVM to the cross-backend bible divergence-lock (6-way)\n\nJVM joins bibleBackends() so every conformance gate -- pure/fold/write-stream/\ndbApply/builders/CRLF/real-data -- now asserts byte-identity across SIX source\nbackends (js/go/py/rust/erl/jvm). Tier 2 complete: the bible builders are\nbyte-identical on all six, incl real Greek/Hebrew data.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Self-Review

**1. Spec coverage.** Part A (Go foldLines `\r`/cap divergence, the parked I2) -> Task 1, locked by the ch560 CRLF cross-backend gate. Part B (JVM port of all 11 ops) -> Task 2 (codec gate + 4 pure + 2 higher-order), Task 3 (write trio + Handle + sortFile + dbApply), each verified by a JVM-direct gate; Task 4 folds JVM into the shared `bibleBackends()` so every existing conformance gate + the builder divergence-lock + the sampled real-data gate go 6-way. The byte-exact discipline (Java UTF-16 trap) is a Global Constraint and is concretely `ISO_8859_1` at every file/content boundary.

**2. Placeholder scan.** Every op has a complete Java body; the Go fix is complete; the helpers (`_foldwalk`, the `__wh` map) and the test functions (`runJVMListing`, the three JVM gates, the `bibleBackends()` JVM append) are complete Go/Java. The NOTEs flag runtime-name confirmations (`fun`/`_nat`/`unit()`-vs-`UNIT`/`VCtor`/`VNat`, the `bufio` import after the Go fix, `LookPath` on an absolute javac path) -- verification points proven by the `javac` compile + the gates, not missing code.

**3. Type consistency.** JVM foreign accessors are `static V <name>()` called as `<name>()` (no `_d`); pure ops have no `_u`; IO ops carry the trailing `fun(_u -> ...)` and return `UNIT`/the handle; the apply ABI is `ap(ap(ap(step, s), __h2s(line)), unit())`; cells are `new VCtor(tag, "name", new V[]{unit(), ...})`. The codec gate broadening matches golang.go's `usesFileEnv || usesLiveKV || usesStream`. After Task 1 all six backends split foldLines on `\n` only -- the uniform contract the JVM body also implements. `bibleBackends()` JVM entry's compile/run pair matches the `backend_conformance_test.go` jvm wiring (which `runBibleBackend` already supports via the `compile` field). Reused listings (ch549/551/552/553/554/557/558/559) + ch560 + fixtures (sample.conllu/foldfix/lexfix/lexdbfix/crlf.txt) are named exactly; `findJava25` is reused from the same package.
