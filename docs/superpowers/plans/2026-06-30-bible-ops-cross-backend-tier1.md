# Bible Ops Cross-Backend Parity -- Tier 1 (Python / Rust / BEAM) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Port the bible host-op family (byteLen/splitOn/jsonStrField/sqlQuote/foldLines/foldDir/openWrite/writeChunk/closeWrite/sortFile/dbApply) from Go+JS to the Python, Rust, and BEAM backends, and add a cross-backend conformance gate that proves the bible builders produce byte-identical output on every source backend (the no-divergence lock).

**Architecture:** Each op already has a Go+JS body (the semantic contract). This ports each to py/rust/beam using their existing codec + IO idioms, then adds `harness/bible_conformance_test.go` that runs the demo + builder listings on all five source backends (js/go/py/rust/erl) and asserts identical stdout / byte-identical output files. Byte-exactness (raw bytes, never utf8 re-encoding) is the load-bearing discipline -- the Greek/Hebrew corpus is the consumer that exposes any divergence.

**Tech Stack:** Go codegen (`codegen/{ioprims,py,rust,beam}.go`), `harness/bible_conformance_test.go`. Reuses existing listings ch549/551/552/553/554/557/558/555/559. Toolchains: node, go, python3, rustc, escript (+ sqlite3 for the DB ops). This is **Tier 1** of a tiered rollout; JVM, native C/LLVM, and WASM are follow-on plans.

## Global Constraints

- Kernel FROZEN: changes ONLY in `codegen/{ioprims,py,rust,beam}.go` and `harness/`. No core/store/elaborate edit. No hash-format bump. No new listings (reuse existing).
- The Go+JS op bodies are the SEMANTIC CONTRACT. Each ported body must compute the SAME function (the conformance gate enforces byte-identical output across all five backends).
- **Byte-exact discipline (the divergence trap):** every backend must read/write RAW BYTES, never re-encode as utf8. Per-backend byte-string repr: Python `str` via `chr()`/`ord()` (latin1) -- file I/O MUST use binary mode + `.encode('latin1')`/`.decode('latin1')`; Rust `Vec<u8>` (already raw) -- paths via `String::from_utf8_lossy`; BEAM char-list (list of 0-255 ints) -- files via `file:read_file`->`binary_to_list` / `list_to_binary`->`file:write_file`.
- Codec (`__s2h`/`__h2s` py, `_s2h`/`_h2s` rust, `d6unpack`/`d6pack` beam) is gated per backend; the bible ops use it, so the gate must include `usesStream(p)` (currently py/beam gate on `usesFileEnv` only; rust on `usesRustStrMarshal`).
- Constructor cells (type-arg erased at args[0]): Python `{"tag":N,"name":S,"args":[None,...]}`; Rust `Rc::new(V::Ctor(N,"S".to_string(),vec![unit(),...]))`; BEAM `{c, N, s, [unit, ...]}` (name is an atom). List: nil=tag0 `[slot]`, cons=tag1 `[slot,head,tail]`. Option: none=tag0 `[slot]`, some=tag1 `[slot,v]`.
- IO `_u` world token: py trailing `lambda _u:` (forced `action(_unit)`); rust trailing `vfun(move |_u: Rc<V>|` (forced `ap(action, unit())`); beam trailing `fun(_U) ->` (forced `ap(Action, unit)`). `IO Unit` returns py `None`, rust `unit()`, beam `unit`.
- Higher-order apply: py `step(s)(line)(_unit)`; rust `ap(ap(ap(step.clone(), s.clone()), val), unit())`; beam `ap(ap(ap(Step, S), Line), unit)`.
- No em-dashes / en-dashes; use `--`. Conventional Commits; trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- No `rune check`: use `go run ./cmd/rune emit|run FILE NAME --target py|rs|erl|js|go`.

---

### Task 1: Codec gate + pure ops (byteLen / splitOn / jsonStrField / sqlQuote) on py/rust/beam

**Files:**
- Modify: `codegen/ioprims.go` (add `closeWrite` to `streamPrims`)
- Modify: `codegen/py.go` (codec gate + 4 pure op bodies)
- Modify: `codegen/rust.go` (codec gate + 4 pure op bodies + `_find` helper)
- Modify: `codegen/beam.go` (codec gate + 4 pure op bodies + split/json helpers)
- Create: `harness/bible_conformance_test.go` (the cross-backend harness + the pure-op gate)

**Interfaces:**
- Produces: `byteLen`/`splitOn`/`jsonStrField`/`sqlQuote` on py/rust/beam, computing identically to the Go/JS bodies. Plus `runBibleAcross(t, listing, main) map[string]string` -- runs a listing on every available source backend, returns backend->trimmed-stdout (used by Tasks 2-4).

- [ ] **Step 1: Fix the codec gates + the streamPrims gap**

In `codegen/ioprims.go`, add `"closeWrite"` to `streamPrims`:
```go
var streamPrims = []string{"foldLines", "foldDir", "splitOn", "byteLen", "jsonStrField", "openWrite", "writeChunk", "closeWrite", "sortFile", "sqlQuote", "dbApply"}
```
In `codegen/py.go`, change the codec gate (currently `if usesFileEnv(p) {`):
```go
	if usesFileEnv(p) || usesStream(p) {
```
In `codegen/beam.go`, change the codec gate (currently `if usesFileEnv(p) {` around the `d6unpack`/`d6pack` emission):
```go
	if usesFileEnv(p) || usesStream(p) {
```
In `codegen/rust.go`, extend `usesRustStrMarshal`:
```go
func usesRustStrMarshal(p Program) bool {
	return usesLiveKV(p) || usesStream(p) || usesForeign(p, "printStrCode") || usesForeign(p, "getEnvCode") ||
		usesForeign(p, "readFileCode") || usesForeign(p, "writeFileCode") || usesForeign(p, "argAtCode")
}
```

- [ ] **Step 2: Emit the 4 pure op bodies on Python**

In `codegen/py.go`, in the op-body section (after the fileEnv ops), add:
```go
	if usesForeign(p, "byteLen") {
		b.WriteString("def byteLen():\n    return lambda c: len(__s2h(c))\n")
	}
	if usesForeign(p, "splitOn") {
		b.WriteString("def splitOn():\n    def _f(sep):\n        def _g(c):\n            parts = __s2h(c).split(chr(sep))\n            lst = {\"tag\":0,\"name\":\"nil\",\"args\":[None]}\n            for q in reversed(parts):\n                lst = {\"tag\":1,\"name\":\"cons\",\"args\":[None, __h2s(q), lst]}\n            return lst\n        return _g\n    return _f\n")
	}
	if usesForeign(p, "jsonStrField") {
		b.WriteString("def jsonStrField():\n    def _f(field):\n        def _g(doc):\n            fn = __s2h(field); ds = __s2h(doc)\n            needle = '\"' + fn + '\"'\n            none = {\"tag\":0,\"name\":\"none\",\"args\":[None]}\n            i = ds.find(needle)\n            if i < 0: return none\n            j = i + len(needle)\n            while j < len(ds) and ds[j] in ' \\t:': j += 1\n            if j < len(ds) and ds[j] == '\"':\n                j += 1; k = j\n                while k < len(ds) and ds[k] != '\"': k += 1\n                return {\"tag\":1,\"name\":\"some\",\"args\":[None, __h2s(ds[j:k])]}\n            return none\n        return _g\n    return _f\n")
	}
	if usesForeign(p, "sqlQuote") {
		b.WriteString("def sqlQuote():\n    def _f(s):\n        inp = __s2h(s); out = \"'\"\n        for ch in inp:\n            if ch == \"'\": out += \"'\"\n            out += ch\n        out += \"'\"\n        return __h2s(out)\n    return _f\n")
	}
```

- [ ] **Step 3: Emit the 4 pure op bodies on Rust**

In `codegen/rust.go`, after the `rustStrMarshal` codec emission, add (and emit the `_find` helper guarded on jsonStrField):
```go
	if usesForeign(p, "byteLen") {
		b.WriteString("fn byteLen() -> Rc<V> { vfun(|c: Rc<V>| Rc::new(V::Nat(_big_from_u64(_s2h(&c).len() as u64)))) }\n")
	}
	if usesForeign(p, "splitOn") {
		b.WriteString("fn splitOn() -> Rc<V> { vfun(|sep: Rc<V>| { let sb = _natusize(&sep) as u8; vfun(move |c: Rc<V>| { let data = _s2h(&c); let parts: Vec<&[u8]> = data.split(|&z| z == sb).collect(); let mut lst = Rc::new(V::Ctor(0, \"nil\".to_string(), vec![unit()])); for part in parts.iter().rev() { lst = Rc::new(V::Ctor(1, \"cons\".to_string(), vec![unit(), _h2s(part), lst])); } lst }) }) }\n")
	}
	if usesForeign(p, "jsonStrField") {
		b.WriteString("fn _find(h: &[u8], n: &[u8]) -> Option<usize> { if n.is_empty() { return Some(0); } if n.len() > h.len() { return None; } for i in 0..=(h.len()-n.len()) { if &h[i..i+n.len()] == n { return Some(i); } } None }\n")
		b.WriteString("fn jsonStrField() -> Rc<V> { vfun(|field: Rc<V>| { let field = field.clone(); vfun(move |doc: Rc<V>| { let fnb = _s2h(&field); let ds = _s2h(&doc); let mut needle = vec![b'\"']; needle.extend_from_slice(&fnb); needle.push(b'\"'); let none = Rc::new(V::Ctor(0, \"none\".to_string(), vec![unit()])); match _find(&ds, &needle) { None => none, Some(i) => { let mut j = i + needle.len(); while j < ds.len() && (ds[j]==b' '||ds[j]==b'\\t'||ds[j]==b':') { j += 1; } if j < ds.len() && ds[j]==b'\"' { j += 1; let mut k = j; while k < ds.len() && ds[k]!=b'\"' { k += 1; } Rc::new(V::Ctor(1, \"some\".to_string(), vec![unit(), _h2s(&ds[j..k])])) } else { none } } } }) }) }\n")
	}
	if usesForeign(p, "sqlQuote") {
		b.WriteString("fn sqlQuote() -> Rc<V> { vfun(|s: Rc<V>| { let inp = _s2h(&s); let mut out: Vec<u8> = Vec::new(); out.push(b'\\''); for &z in inp.iter() { if z == b'\\'' { out.push(b'\\''); } out.push(z); } out.push(b'\\''); _h2s(&out) }) }\n")
	}
```
(NOTE: confirm the exact names of the Rust nat-from-byte helper (`_natusize`) and `_big_from_u64`/`_h2s`/`_s2h`/`unit`/`vfun`/`V::Ctor`/`V::Nat` against `rust.go` -- they are used verbatim by `sockConnect`/`getEnvCode`/the codec; adjust if a name differs.)

- [ ] **Step 4: Emit the 4 pure op bodies on BEAM**

In `codegen/beam.go`, in the op-body section, add (top-level Erlang functions + the helpers):
```go
	if usesForeign(p, "byteLen") {
		b.WriteString("ff_byteLen() -> fun(C) -> length(d6unpack(C)) end.\n")
	}
	if usesForeign(p, "splitOn") {
		b.WriteString("ff_splitOn() -> fun(Sep) -> fun(C) -> _bmkparts(_bsplit(Sep, d6unpack(C))) end end.\n")
		b.WriteString("_bsplit(_Sep, []) -> [[]];\n_bsplit(Sep, [H|T]) when H =:= Sep -> [[] | _bsplit(Sep, T)];\n_bsplit(Sep, [H|T]) -> [Cur|Rest] = _bsplit(Sep, T), [[H|Cur]|Rest].\n")
		b.WriteString("_bmkparts([]) -> {c,0,nil,[unit]};\n_bmkparts([P|Ps]) -> {c,1,cons,[unit, d6pack(P), _bmkparts(Ps)]}.\n")
	}
	if usesForeign(p, "jsonStrField") {
		b.WriteString("ff_jsonStrField() -> fun(Field) -> fun(Doc) -> _jsfield(d6unpack(Field), d6unpack(Doc)) end end.\n")
		b.WriteString("_jsfield(Fn, Ds) -> Needle = [$\" | Fn] ++ [$\"], case string:find(Ds, Needle) of nomatch -> {c,0,none,[unit]}; Rest -> After = lists:nthtail(length(Needle), Rest), case _jskip(After) of [$\" | R2] -> {Val, _} = _jtake(R2, []), {c,1,some,[unit, d6pack(Val)]}; _ -> {c,0,none,[unit]} end end.\n")
		b.WriteString("_jskip([C|T]) when C =:= 32; C =:= 9; C =:= $: -> _jskip(T);\n_jskip(L) -> L.\n")
		b.WriteString("_jtake([$\"|T], Acc) -> {lists:reverse(Acc), T};\n_jtake([C|T], Acc) -> _jtake(T, [C|Acc]);\n_jtake([], Acc) -> {lists:reverse(Acc), []}.\n")
	}
	if usesForeign(p, "sqlQuote") {
		b.WriteString("ff_sqlQuote() -> fun(S) -> d6pack(_bquote(d6unpack(S))) end.\n")
		b.WriteString("_bquote(L) -> [$' | _bqi(L)] ++ [$'].\n_bqi([]) -> [];\n_bqi([$'|T]) -> [$', $' | _bqi(T)];\n_bqi([H|T]) -> [H | _bqi(T)].\n")
	}
```
(NOTE: `$"` and `$'` are Erlang char literals (34 and 39). `string:find/2` over char-lists is OTP-20+; it returns the matching tail as a list or `nomatch`. Verify the foreign-call dispatch prefix is `ff_<name>` against how beam.go names existing foreign bodies.)

- [ ] **Step 5: Create the cross-backend conformance harness + the pure-op gate**

Create `harness/bible_conformance_test.go`:
```go
package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// bibleBackend is one source target in the cross-backend bible gate.
type bibleBackend struct {
	name    string
	bin     string
	ext     string
	emit    func(codegen.Program) (codegen.TargetSource, error)
	run     func(file string) *exec.Cmd
	compile func(src, out string) *exec.Cmd // nil for interpreted
}

func bibleBackends() []bibleBackend {
	return []bibleBackend{
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
}

// runBibleBackend emits+runs one listing on one backend in `dir` (cwd=dir for relative
// file paths), returns trimmed stdout. Skips via t.Skip if the toolchain is absent.
func runBibleBackend(t *testing.T, bk bibleBackend, listing, main, dir string) (string, bool) {
	t.Helper()
	if _, err := exec.LookPath(bk.bin); err != nil {
		return "", false
	}
	s := loadListing(t, listing)
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("[%s] %s emit-program: %v", bk.name, listing, err)
	}
	src, err := bk.emit(p)
	if err != nil {
		t.Fatalf("[%s] %s emit: %v", bk.name, listing, err)
	}
	f := filepath.Join(dir, "main."+bk.ext)
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	runFile := f
	if bk.compile != nil {
		bin := filepath.Join(dir, "main_"+bk.name+".bin")
		if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
			t.Fatalf("[%s] %s compile: %v\n%s", bk.name, listing, err, out)
		}
		runFile = bin
	}
	cmd := bk.run(runFile)
	cmd.Dir = dir
	out, err := cmd.Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("[%s] %s run: %v\n%s", bk.name, listing, err, stderr)
	}
	return strings.TrimSpace(string(out)), true
}

// assertBibleAgree runs (listing, main) on every available backend in its OWN temp dir
// and asserts they all produce `want`. At least two backends must actually run.
func assertBibleAgree(t *testing.T, listing, main, want string) {
	t.Helper()
	ran := 0
	for _, bk := range bibleBackends() {
		dir := t.TempDir()
		got, ok := runBibleBackend(t, bk, listing, main, dir)
		if !ok {
			t.Logf("[%s] skipped (%s not in PATH)", bk.name, bk.bin)
			continue
		}
		ran++
		if got != want {
			t.Errorf("[%s] %s/%s = %q, want %q (backends must not diverge)", bk.name, listing, main, got, want)
		}
	}
	if ran < 2 {
		t.Skipf("fewer than 2 backends available (ran %d)", ran)
	}
}

func TestBibleConformancePure(t *testing.T) {
	assertBibleAgree(t, "ch551_json_field.rune", "strongLen", "5")
	assertBibleAgree(t, "ch551_json_field.rune", "missingLen", "0")
	assertBibleAgree(t, "ch557_sql_quote.rune", "quotePlain", "5")
	assertBibleAgree(t, "ch557_sql_quote.rune", "quoteEmbedded", "6")
}
```

- [ ] **Step 6: Verify each backend witness + the gate**

Run the witnesses on each new backend:
```
go run ./cmd/rune run listings/ch551_json_field.rune strongLen --target py   # 5
go run ./cmd/rune run listings/ch551_json_field.rune strongLen --target erl  # 5
go run ./cmd/rune run listings/ch557_sql_quote.rune quoteEmbedded --target py   # 6
( D=$(mktemp -d); go run ./cmd/rune emit listings/ch557_sql_quote.rune quoteEmbedded --target rs > "$D/m.rs" && rustc --edition 2021 -o "$D/m" "$D/m.rs" && "$D/m" )   # 6
```
Then the gate: `go test ./harness/ -run 'TestBibleConformancePure' -count=1 -v` -> PASS (all available backends agree; absent toolchains skip). The bodies are new -- a mismatch is a real bug (the cell shape, the codec gate, the byte handling), debug it. Report BLOCKED only if unresolvable.

- [ ] **Step 7: Commit**

```bash
git add codegen/ioprims.go codegen/py.go codegen/rust.go codegen/beam.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): bible pure ops (byteLen/splitOn/jsonStrField/sqlQuote) on py/rust/beam\n\nPorts the four pure bible ops to Python, Rust, BEAM with the codec gate extended\nto usesStream; adds the cross-backend conformance harness asserting js/go/py/rust/erl\nagree (TestBibleConformancePure). No core change, no hash bump.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 2: Higher-order ops (foldLines / foldDir) on py/rust/beam

**Files:**
- Modify: `codegen/py.go`, `codegen/rust.go`, `codegen/beam.go` (foldLines + foldDir bodies)
- Modify: `harness/bible_conformance_test.go` (add the fold gate)

**Interfaces:**
- Consumes: the codec + apply convention. Produces: `foldLines`/`foldDir` on py/rust/beam (host loop applying an erased step per line/file, byte-exact reads).

> The reference semantics (Go): `foldLines` opens the path, scans line-by-line (newline stripped) with a >=1MB buffer, applies `step s lineCode ()` per line, returns the final state; open-fail returns `s0`. `foldDir` recursively walks `dir` in sorted order, for each file whose name ends with the suffix reads the WHOLE content and applies `step s contentCode ()`; per-file errors skip. Both read RAW BYTES.

- [ ] **Step 1: Python foldLines + foldDir**

In `codegen/py.go`:
```go
	if usesForeign(p, "foldLines") {
		b.WriteString("def foldLines():\n    def _f(_S):\n        def _g(path):\n            def _h(step):\n                def _i(s0):\n                    def _t(_u):\n                        try:\n                            data = open(__s2h(path), 'rb').read().decode('latin1')\n                        except Exception:\n                            return s0\n                        lines = data.split('\\n')\n                        if lines and lines[-1] == '': lines.pop()\n                        s = s0\n                        for ln in lines:\n                            s = step(s)(__h2s(ln))(_unit)\n                        return s\n                    return _t\n                return _i\n            return _h\n        return _g\n    return _f\n")
	}
	if usesForeign(p, "foldDir") {
		b.WriteString("def foldDir():\n    import os\n    def _f(_S):\n        def _g(dirc):\n            def _h(suf):\n                def _i(step):\n                    def _j(s0):\n                        def _t(_u):\n                            sfx = __s2h(suf)\n                            box = [s0]\n                            def walk(dd):\n                                try:\n                                    ents = sorted(os.listdir(dd))\n                                except Exception:\n                                    return\n                                for name in ents:\n                                    full = os.path.join(dd, name)\n                                    if os.path.isdir(full): walk(full)\n                                    elif full.endswith(sfx):\n                                        try:\n                                            data = open(full, 'rb').read().decode('latin1')\n                                        except Exception:\n                                            continue\n                                        box[0] = step(box[0])(__h2s(data))(_unit)\n                            walk(__s2h(dirc))\n                            return box[0]\n                        return _t\n                    return _j\n                return _i\n            return _h\n        return _g\n    return _f\n")
	}
```

- [ ] **Step 2: Rust foldLines + foldDir**

In `codegen/rust.go` (uses `std::fs::read` = raw bytes; recursive dir walk sorted):
```go
	if usesForeign(p, "foldLines") {
		b.WriteString("fn foldLines() -> Rc<V> { vfun(|_s: Rc<V>| vfun(move |path: Rc<V>| vfun(move |step: Rc<V>| vfun(move |s0: Rc<V>| { let step = step.clone(); let s0 = s0.clone(); let path = path.clone(); vfun(move |_u: Rc<V>| { let pth = String::from_utf8_lossy(&_s2h(&path)).to_string(); let data = match std::fs::read(&pth) { Ok(d) => d, Err(_) => return s0.clone() }; let mut parts: Vec<&[u8]> = data.split(|&z| z == b'\\n').collect(); if let Some(last) = parts.last() { if last.is_empty() { parts.pop(); } } let mut s = s0.clone(); for ln in parts { s = ap(ap(ap(step.clone(), s.clone()), _h2s(ln)), unit()); } s }) }))) }) }\n")
	}
	if usesForeign(p, "foldDir") {
		b.WriteString("fn _foldwalk(dir: &std::path::Path, sfx: &[u8], step: &Rc<V>, s: &mut Rc<V>) { let mut ents: Vec<_> = match std::fs::read_dir(dir) { Ok(e) => e.filter_map(|x| x.ok()).collect(), Err(_) => return }; ents.sort_by_key(|e| e.file_name()); for e in ents { let full = e.path(); if full.is_dir() { _foldwalk(&full, sfx, step, s); } else { let name = full.to_string_lossy().into_owned(); if name.as_bytes().ends_with(sfx) { if let Ok(data) = std::fs::read(&full) { *s = ap(ap(ap(step.clone(), s.clone()), _h2s(&data)), unit()); } } } } }\n")
		b.WriteString("fn foldDir() -> Rc<V> { vfun(|_s: Rc<V>| vfun(move |dirc: Rc<V>| vfun(move |suf: Rc<V>| vfun(move |step: Rc<V>| vfun(move |s0: Rc<V>| { let step = step.clone(); let s0 = s0.clone(); let dirc = dirc.clone(); let suf = suf.clone(); vfun(move |_u: Rc<V>| { let d = String::from_utf8_lossy(&_s2h(&dirc)).to_string(); let sfx = _s2h(&suf); let mut s = s0.clone(); _foldwalk(std::path::Path::new(&d), &sfx, &step, &mut s); s }) })))) }) }\n")
	}
```
(Verify `ap`/`unit`/`vfun`/`_s2h`/`_h2s` names; the closure `move`/`clone` discipline must satisfy the borrow checker -- the compile step in the gate is the check.)

- [ ] **Step 3: BEAM foldLines + foldDir**

In `codegen/beam.go` (file:read_file -> binary -> char-list; recursive filelib walk sorted):
```go
	if usesForeign(p, "foldLines") {
		b.WriteString("ff_foldLines() -> fun(_S) -> fun(Path) -> fun(Step) -> fun(S0) -> fun(_U) -> case file:read_file(d6unpack(Path)) of {ok, Bin} -> Lines = _blines(binary_to_list(Bin)), lists:foldl(fun(Ln, S) -> ap(ap(ap(Step, S), d6pack(Ln)), unit) end, S0, Lines); _ -> S0 end end end end end end.\n")
		b.WriteString("_blines(L) -> Parts = _bsplit(10, L), case lists:reverse(Parts) of [[] | R] -> lists:reverse(R); _ -> Parts end.\n")
	}
	if usesForeign(p, "foldDir") {
		b.WriteString("ff_foldDir() -> fun(_S) -> fun(Dir) -> fun(Suf) -> fun(Step) -> fun(S0) -> fun(_U) -> Sfx = d6unpack(Suf), _fdwalk(d6unpack(Dir), Sfx, Step, S0) end end end end end end.\n")
		b.WriteString("_fdwalk(Dir, Sfx, Step, S0) -> case file:list_dir(Dir) of {ok, Names} -> lists:foldl(fun(N, S) -> Full = filename:join(Dir, N), case filelib:is_dir(Full) of true -> _fdwalk(Full, Sfx, Step, S); false -> case lists:suffix(Sfx, Full) of true -> case file:read_file(Full) of {ok, B} -> ap(ap(ap(Step, S), d6pack(binary_to_list(B))), unit); _ -> S end; false -> S end end end, S0, lists:sort(Names)); _ -> S0 end.\n")
	}
```
(NOTE: `_bsplit` is reused from Task 1's splitOn helper -- if foldLines is present without splitOn, ensure `_bsplit` is still emitted; gate `_bsplit`/`_bmkparts` on `usesForeign(p,"splitOn") || usesForeign(p,"foldLines")`, or emit `_bsplit` whenever the stream codec emits. Adjust the Task-1 `_bsplit` gate accordingly.)

- [ ] **Step 4: Add the fold conformance gate**

Both ch549 (reads relative `sample.conllu`) and ch554 (reads relative `foldfix/`) READ a pre-existing fixture, so each backend must run from `harness/testdata` (cwd), NOT a temp dir -- running from a temp dir makes the open fail and `foldLines`/`foldDir` return the seed (0), a false green. Append to `harness/bible_conformance_test.go`:
```go
func TestBibleConformanceFold(t *testing.T) {
	// foldLines over harness/testdata/sample.conllu -> token count 11 (double-printed "11\n11").
	assertBibleAgreeFromTestdata(t, "ch549_conllu_count.rune", "main", "11\n11")
	// foldDir over harness/testdata/foldfix -> 3 matching .json files ("3\n3").
	assertBibleAgreeFromTestdata(t, "ch554_fold_dir.rune", "main", "3\n3")
}

// assertBibleAgreeFromTestdata runs (listing, main) on every available backend with
// cwd = harness/testdata (so the listing's relative fixture path resolves) and asserts
// they all produce `want`. The emitted source / compiled binary lives in a temp dir but
// is referenced by ABSOLUTE path so cwd can be testdata.
func assertBibleAgreeFromTestdata(t *testing.T, listing, main, want string) {
	t.Helper()
	ran := 0
	for _, bk := range bibleBackends() {
		if _, err := exec.LookPath(bk.bin); err != nil {
			continue
		}
		s := loadListing(t, listing)
		p, err := s.EmitProgram(main)
		if err != nil {
			t.Fatalf("[%s] %s emit-program: %v", bk.name, listing, err)
		}
		src, err := bk.emit(p)
		if err != nil {
			t.Fatalf("[%s] %s emit: %v", bk.name, listing, err)
		}
		dir := t.TempDir()
		f := filepath.Join(dir, "main."+bk.ext) // already absolute (t.TempDir is absolute)
		if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
		runFile := f
		if bk.compile != nil {
			bin := filepath.Join(dir, "m.bin")
			if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
				t.Fatalf("[%s] %s compile: %v\n%s", bk.name, listing, err, out)
			}
			runFile = bin
		}
		cmd := bk.run(runFile)
		cmd.Dir = "testdata"
		out, err := cmd.Output()
		if err != nil {
			stderr := ""
			if ee, ok := err.(*exec.ExitError); ok {
				stderr = string(ee.Stderr)
			}
			t.Fatalf("[%s] %s run: %v\n%s", bk.name, listing, err, stderr)
		}
		ran++
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[%s] %s/%s = %q, want %q (backends must not diverge)", bk.name, listing, main, got, want)
		}
	}
	if ran < 2 {
		t.Skipf("fewer than 2 backends (ran %d)", ran)
	}
}
```
(Confirmed: `ch549_conllu_count.rune`/`main` -> `11\n11` and `ch554_fold_dir.rune`/`main` -> `3\n3`, both from cwd=harness/testdata, on the Go target. `t.TempDir()` returns an absolute path so the emitted source/binary is reachable while cwd=testdata.)

- [ ] **Step 5: Verify + commit**

```
go run ./cmd/rune run listings/ch554_fold_dir.rune main --target py    # (from harness/testdata) 3\n3
go test ./harness/ -run 'TestBibleConformanceFold' -count=1 -v   # PASS (backends agree)
```
```bash
git add codegen/py.go codegen/rust.go codegen/beam.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): bible higher-order ops (foldLines/foldDir) on py/rust/beam\n\nHost loop applying an erased step per line/file, byte-exact raw reads, on Python,\nRust, BEAM. Cross-backend gate asserts foldLines(11)/foldDir(3) agree across\njs/go/py/rust/erl (TestBibleConformanceFold). No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 3: Write-stream trio + Handle + sortFile on py/rust/beam

**Files:**
- Modify: `codegen/py.go`, `codegen/rust.go`, `codegen/beam.go` (Handle + openWrite/writeChunk/closeWrite + sortFile)
- Modify: `harness/bible_conformance_test.go` (add the write-stream gate)

**Interfaces:**
- Consumes: foldLines (read-back). Produces: `Handle` foreign type + `openWrite`/`writeChunk`(+`\n`)/`closeWrite` + `sortFile` (bytewise) on py/rust/beam. Opaque handle carried per backend: Python a binary file object; BEAM a `file:open` IoDevice; Rust a thread-local table token.

- [ ] **Step 1: Python -- Handle + write trio + sortFile**

In `codegen/py.go`:
```go
	if usesForeign(p, "Handle") {
		b.WriteString("def Handle():\n    return None\n")
	}
	if usesForeign(p, "openWrite") {
		b.WriteString("def openWrite():\n    def _f(path):\n        def _t(_u):\n            try:\n                return open(__s2h(path), 'wb')\n            except Exception:\n                return None\n        return _t\n    return _f\n")
	}
	if usesForeign(p, "writeChunk") {
		b.WriteString("def writeChunk():\n    def _f(h):\n        def _g(c):\n            def _t(_u):\n                if h is not None: h.write((__s2h(c) + '\\n').encode('latin1'))\n                return h\n            return _t\n        return _g\n    return _f\n")
	}
	if usesForeign(p, "closeWrite") {
		b.WriteString("def closeWrite():\n    def _f(h):\n        def _t(_u):\n            if h is not None: h.close()\n            return None\n        return _t\n    return _f\n")
	}
	if usesForeign(p, "sortFile") {
		b.WriteString("def sortFile():\n    def _f(inp):\n        def _g(outp):\n            def _t(_u):\n                try:\n                    data = open(__s2h(inp), 'rb').read().decode('latin1')\n                except Exception:\n                    open(__s2h(outp), 'wb').write(b''); return None\n                lines = data.split('\\n')\n                if lines and lines[-1] == '': lines.pop()\n                lines.sort()\n                open(__s2h(outp), 'wb').write(('\\n'.join(lines) + ('\\n' if lines else '')).encode('latin1'))\n                return None\n            return _t\n        return _g\n    return _f\n")
	}
```
(`lines.sort()` on latin1 `str` orders by code point == byte value == Go `sort.Strings`. The trailing-newline shape matches Go: each line + `\n`.)

- [ ] **Step 2: Rust -- Handle + write trio + sortFile (thread-local handle table)**

In `codegen/rust.go`, declare the write-handle table near the other thread-locals (where `__SOCKS`/`__SOCKID` are declared), guarded so it only emits when openWrite is used; the simplest is to add to the runtime preamble that is always present, OR emit alongside openWrite. Emit the table + ops:
```go
	if usesForeign(p, "openWrite") || usesForeign(p, "writeChunk") || usesForeign(p, "closeWrite") {
		b.WriteString("thread_local! { static __WH: std::cell::RefCell<std::collections::HashMap<i64, std::fs::File>> = std::cell::RefCell::new(std::collections::HashMap::new()); static __WHID: std::cell::RefCell<i64> = std::cell::RefCell::new(0); }\n")
	}
	if usesForeign(p, "openWrite") {
		b.WriteString("fn openWrite() -> Rc<V> { vfun(|path: Rc<V>| { let path = path.clone(); vfun(move |_u: Rc<V>| { let pth = String::from_utf8_lossy(&_s2h(&path)).to_string(); match std::fs::File::create(&pth) { Ok(f) => { let id = __WHID.with(|c| { let mut c = c.borrow_mut(); *c += 1; *c }); __WH.with(|m| { m.borrow_mut().insert(id, f); }); _nat_of_usize(id as usize) } Err(_) => _nat_of_usize(0) } }) }) }\n")
	}
	if usesForeign(p, "writeChunk") {
		b.WriteString("fn writeChunk() -> Rc<V> { vfun(|h: Rc<V>| { let h = h.clone(); vfun(move |c: Rc<V>| { let h = h.clone(); vfun(move |_u: Rc<V>| { use std::io::Write; let id = _natusize(&h) as i64; let mut bytes = _s2h(&c); bytes.push(b'\\n'); __WH.with(|m| { if let Some(f) = m.borrow_mut().get_mut(&id) { let _ = f.write_all(&bytes); } }); h.clone() }) }) }) }\n")
	}
	if usesForeign(p, "closeWrite") {
		b.WriteString("fn closeWrite() -> Rc<V> { vfun(|h: Rc<V>| { let h = h.clone(); vfun(move |_u: Rc<V>| { let id = _natusize(&h) as i64; __WH.with(|m| { m.borrow_mut().remove(&id); }); unit() }) }) }\n")
	}
	if usesForeign(p, "sortFile") {
		b.WriteString("fn sortFile() -> Rc<V> { vfun(|inp: Rc<V>| { let inp = inp.clone(); vfun(move |outp: Rc<V>| { let inp = inp.clone(); let outp = outp.clone(); vfun(move |_u: Rc<V>| { let ip = String::from_utf8_lossy(&_s2h(&inp)).to_string(); let op = String::from_utf8_lossy(&_s2h(&outp)).to_string(); let data = match std::fs::read(&ip) { Ok(d) => d, Err(_) => { let _ = std::fs::write(&op, b\"\"); return unit(); } }; let mut lines: Vec<&[u8]> = data.split(|&z| z == b'\\n').collect(); if let Some(last) = lines.last() { if last.is_empty() { lines.pop(); } } lines.sort(); let mut out: Vec<u8> = Vec::new(); for ln in lines { out.extend_from_slice(ln); out.push(b'\\n'); } let _ = std::fs::write(&op, &out); unit() }) }) }) }\n")
	}
```
(`lines.sort()` on `&[u8]` is bytewise == Go. The handle is an `i64` token in a thread-local `File` map -- the `__SOCKS` precedent. Dropping the map entry closes the file. Verify `_nat_of_usize`/`_natusize` names.)

- [ ] **Step 3: BEAM -- Handle + write trio + sortFile**

In `codegen/beam.go` (`file:open` with `[write, raw, binary]`; the IoDevice carried directly):
```go
	if usesForeign(p, "Handle") {
		b.WriteString("ff_Handle() -> unit.\n")
	}
	if usesForeign(p, "openWrite") {
		b.WriteString("ff_openWrite() -> fun(Path) -> fun(_U) -> case file:open(d6unpack(Path), [write, raw, binary]) of {ok, Fd} -> Fd; _ -> nil end end end.\n")
	}
	if usesForeign(p, "writeChunk") {
		b.WriteString("ff_writeChunk() -> fun(H) -> fun(C) -> fun(_U) -> case H of nil -> nil; _ -> file:write(H, list_to_binary(d6unpack(C) ++ [10])) end, H end end end.\n")
	}
	if usesForeign(p, "closeWrite") {
		b.WriteString("ff_closeWrite() -> fun(H) -> fun(_U) -> case H of nil -> ok; _ -> file:close(H) end, unit end end.\n")
	}
	if usesForeign(p, "sortFile") {
		b.WriteString("ff_sortFile() -> fun(Inp) -> fun(Outp) -> fun(_U) -> case file:read_file(d6unpack(Inp)) of {ok, Bin} -> Lines = _blines(binary_to_list(Bin)), Sorted = lists:sort(Lines), Out = lists:flatmap(fun(L) -> L ++ [10] end, Sorted), file:write_file(d6unpack(Outp), list_to_binary(Out)); _ -> file:write_file(d6unpack(Outp), <<>>) end, unit end end end.\n")
	}
```
(`lists:sort` on char-lists is bytewise (standard term order on lists of ints) == Go. `_blines` reused from Task 2. `[10]` is `\n`.)

- [ ] **Step 4: Add the write-stream conformance gate**

Append to `harness/bible_conformance_test.go`:
```go
func TestBibleConformanceWriteStream(t *testing.T) {
	// ch552: write two lines, read back with foldLines, print count -> 2\n2.
	assertBibleAgree(t, "ch552_write_stream.rune", "main", "2\n2")
	// ch553: write three lines, sortFile, read back first line's byteLen -> 5\n5.
	assertBibleAgree(t, "ch553_sort_file.rune", "main", "5\n5")
}
```

- [ ] **Step 5: Verify + commit**

```
( D=$(mktemp -d); cd "$D" && go run "$OLDPWD/cmd/rune" run "$OLDPWD/listings/ch552_write_stream.rune" main --target py | tail -1 )   # 2
( D=$(mktemp -d); cd "$D" && go run "$OLDPWD/cmd/rune" run "$OLDPWD/listings/ch553_sort_file.rune" main --target erl | tail -1 )   # 5
go test ./harness/ -run 'TestBibleConformanceWriteStream' -count=1 -v   # PASS
```
```bash
git add codegen/py.go codegen/rust.go codegen/beam.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): bible write-stream + sortFile on py/rust/beam\n\nHandle foreign type + openWrite/writeChunk/closeWrite + bytewise sortFile on\nPython (binary file obj), Rust (thread-local handle table), BEAM (file IoDevice),\nbyte-exact raw writes. Cross-backend gate asserts ch552(2)/ch553(5) agree across\njs/go/py/rust/erl (TestBibleConformanceWriteStream). No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 4: `dbApply` on py/rust/beam

**Files:**
- Modify: `codegen/py.go`, `codegen/rust.go`, `codegen/beam.go` (dbApply body)
- Modify: `harness/bible_conformance_test.go` (add the dbApply gate, sqlite3-guarded)

**Interfaces:**
- Produces: `dbApply : Path -> Path -> IO Unit` on py/rust/beam, shelling `sqlite3 <db> ".read <sql>"`.

- [ ] **Step 1: Python dbApply**

In `codegen/py.go`:
```go
	if usesForeign(p, "dbApply") {
		b.WriteString("def dbApply():\n    import subprocess\n    def _f(db):\n        def _g(sql):\n            def _t(_u):\n                try:\n                    subprocess.run(['sqlite3', __s2h(db), '.read ' + __s2h(sql)], capture_output=True)\n                except Exception:\n                    pass\n                return None\n            return _t\n        return _g\n    return _f\n")
	}
```

- [ ] **Step 2: Rust dbApply**

In `codegen/rust.go`:
```go
	if usesForeign(p, "dbApply") {
		b.WriteString("fn dbApply() -> Rc<V> { vfun(|db: Rc<V>| { let db = db.clone(); vfun(move |sql: Rc<V>| { let db = db.clone(); let sql = sql.clone(); vfun(move |_u: Rc<V>| { let d = String::from_utf8_lossy(&_s2h(&db)).to_string(); let s = String::from_utf8_lossy(&_s2h(&sql)).to_string(); let _ = std::process::Command::new(\"sqlite3\").arg(&d).arg(format!(\".read {}\", s)).output(); unit() }) }) }) }\n")
	}
```

- [ ] **Step 3: BEAM dbApply**

In `codegen/beam.go` (os:cmd shell; the sql path is a temp path with no spaces, single-quote the .read arg):
```go
	if usesForeign(p, "dbApply") {
		b.WriteString("ff_dbApply() -> fun(Db) -> fun(Sql) -> fun(_U) -> os:cmd(\"sqlite3 \" ++ d6unpack(Db) ++ \" '.read \" ++ d6unpack(Sql) ++ \"'\"), unit end end end.\n")
	}
```

- [ ] **Step 4: Add the dbApply conformance gate (sqlite3-guarded)**

Append to `harness/bible_conformance_test.go`:
```go
func TestBibleConformanceDbApply(t *testing.T) {
	if _, err := exec.LookPath("sqlite3"); err != nil {
		t.Skip("sqlite3 CLI not in PATH")
	}
	// ch558: main builds ch558.db from a 2-row .sql via dbApply; the db is the output.
	// Run on each backend in its own temp dir, then query count(*) -> 2.
	ran := 0
	for _, bk := range bibleBackends() {
		if _, err := exec.LookPath(bk.bin); err != nil {
			continue
		}
		dir := t.TempDir()
		_, ok := runBibleBackend(t, bk, "ch558_db_apply.rune", "main", dir)
		if !ok {
			continue
		}
		q := exec.Command("sqlite3", filepath.Join(dir, "ch558.db"), "SELECT count(*) FROM t")
		out, err := q.CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] query: %v\n%s", bk.name, err, out)
		}
		ran++
		if got := strings.TrimSpace(string(out)); got != "2" {
			t.Errorf("[%s] ch558 db count = %q, want 2", bk.name, got)
		}
	}
	if ran < 2 {
		t.Skipf("fewer than 2 backends (ran %d)", ran)
	}
}
```

- [ ] **Step 5: Verify + commit**

```
( D=$(mktemp -d); cd "$D" && go run "$OLDPWD/cmd/rune" run "$OLDPWD/listings/ch558_db_apply.rune" main --target py >/dev/null 2>&1; sqlite3 "$D/ch558.db" "SELECT count(*) FROM t" )   # 2
go test ./harness/ -run 'TestBibleConformanceDbApply' -count=1 -v   # PASS (or SKIP if sqlite3 absent)
```
```bash
git add codegen/py.go codegen/rust.go codegen/beam.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): bible dbApply (sqlite3 shell-out) on py/rust/beam\n\ndbApply on Python (subprocess), Rust (std::process), BEAM (os:cmd). Cross-backend\ngate builds ch558.db on each backend and asserts count=2 (TestBibleConformanceDbApply,\nsqlite3-guarded). No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 5: The divergence-lock -- cross-backend builder conformance (the consumer)

**Files:**
- Modify: `harness/bible_conformance_test.go` (the builder file-output gate + env-gated cross-backend real-data gate)

**Interfaces:**
- Consumes: every op on every source backend. Produces: `TestBibleConformanceBuilders` (the shared-root `.jsonl` + lexicon `.sql` are byte-identical across all backends on the synthetic fixtures) and `TestBibleConformanceRealData` (env-gated; the lexicon `.sql` is byte-identical across backends on the real lexicon, and loads query-equivalent).

> This is the consumer that makes the whole tier non-divergent: it runs the ACTUAL builders (ch555 shared-root, ch559 lexicon-db) on every source backend and byte-compares the output files. Any per-backend encoding/sort/codec divergence fails here.

- [ ] **Step 1: Builder file-output cross-backend gate (synthetic fixtures, always-on)**

Append to `harness/bible_conformance_test.go`:
```go
// buildBibleFile runs a builder listing on one backend in a fresh temp dir seeded with
// the fixture dir (copied to `destName`), and returns the named output file's bytes.
func buildBibleFile(t *testing.T, bk bibleBackend, listing, main, fixture, destName, outFile string) ([]byte, bool) {
	t.Helper()
	if _, err := exec.LookPath(bk.bin); err != nil {
		return nil, false
	}
	dir := t.TempDir()
	copyTree(t, filepath.Join("testdata", fixture), filepath.Join(dir, destName))
	s := loadListing(t, listing)
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("[%s] emit-program: %v", bk.name, err)
	}
	src, err := bk.emit(p)
	if err != nil {
		t.Fatalf("[%s] emit: %v", bk.name, err)
	}
	f := filepath.Join(dir, "main."+bk.ext)
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	runFile := f
	if bk.compile != nil {
		bin := filepath.Join(dir, "m.bin")
		if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
			t.Fatalf("[%s] compile: %v\n%s", bk.name, err, out)
		}
		runFile = bin
	}
	cmd := bk.run(runFile)
	cmd.Dir = dir
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("[%s] run: %v\n%s", bk.name, err, out)
	}
	data, err := os.ReadFile(filepath.Join(dir, outFile))
	if err != nil {
		t.Fatalf("[%s] no output %s: %v", bk.name, outFile, err)
	}
	return data, true
}

// assertBibleFilesAgree builds the file on every backend and asserts byte-identity to
// the first backend that ran (and at least two ran).
func assertBibleFilesAgree(t *testing.T, listing, main, fixture, destName, outFile string) {
	t.Helper()
	var ref []byte
	var refName string
	ran := 0
	for _, bk := range bibleBackends() {
		data, ok := buildBibleFile(t, bk, listing, main, fixture, destName, outFile)
		if !ok {
			continue
		}
		ran++
		if ref == nil {
			ref = data
			refName = bk.name
			continue
		}
		if string(data) != string(ref) {
			t.Errorf("[%s] %s differs from [%s] (backends diverge):\n%s vs\n%s", bk.name, outFile, refName, data, ref)
		}
	}
	if ran < 2 {
		t.Skipf("fewer than 2 backends (ran %d)", ran)
	}
}

func TestBibleConformanceBuilders(t *testing.T) {
	// shared-root JSONL builder (ch555) over the lexfix fixture -> shared-root.out.
	assertBibleFilesAgree(t, "ch555_build_shared_root.rune", "main", "lexfix", "lexfix", "shared-root.out")
	// lexicon-db builder (ch559) over the lexdbfix fixture -> lexicon.sql (the deterministic text).
	assertBibleFilesAgree(t, "ch559_build_db_lexicon.rune", "main", "lexdbfix", "lexdb", "lexicon.sql")
}
```
(NOTE: ch555's fixture dir is `harness/testdata/lexfix` copied to `lexfix`; ch559's is `lexdbfix` copied to `lexdb`. Confirm those fixture names against Tasks of Milestones B/C. The compared artifact for ch559 is `lexicon.sql` -- the deterministic text -- which is byte-identical across backends; the `.db` is NOT byte-deterministic and is checked via query-equivalence in Step 2.)

- [ ] **Step 2: Env-gated cross-backend real-data gate**

Append to `harness/bible_conformance_test.go`:
```go
func TestBibleConformanceRealData(t *testing.T) {
	repo := os.Getenv("BIBLE_REPO")
	if repo == "" {
		t.Skip("set BIBLE_REPO for the cross-backend real-data gate")
	}
	if _, err := exec.LookPath("sqlite3"); err != nil {
		t.Skip("sqlite3 not in PATH")
	}
	const q = "SELECT * FROM lexicon ORDER BY strong IS NULL, strong, lemma, translit, lang, pos, root"
	var refSQL []byte
	var refName string
	ran := 0
	for _, bk := range bibleBackends() {
		if _, err := exec.LookPath(bk.bin); err != nil {
			continue
		}
		dir := t.TempDir()
		if out, err := exec.Command("cp", "-r", filepath.Join(repo, "lexicon"), filepath.Join(dir, "lexdb")).CombinedOutput(); err != nil {
			t.Fatalf("[%s] cp: %v\n%s", bk.name, err, out)
		}
		s := loadListing(t, "ch559_build_db_lexicon.rune")
		p, err := s.EmitProgram("main")
		if err != nil {
			t.Fatal(err)
		}
		src, err := bk.emit(p)
		if err != nil {
			t.Fatalf("[%s] emit: %v", bk.name, err)
		}
		f := filepath.Join(dir, "main."+bk.ext)
		if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
		runFile := f
		if bk.compile != nil {
			bin := filepath.Join(dir, "m.bin")
			if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
				t.Fatalf("[%s] compile: %v\n%s", bk.name, err, out)
			}
			runFile = bin
		}
		cmd := bk.run(runFile)
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("[%s] build: %v\n%s", bk.name, err, out)
		}
		// the .sql text must be byte-identical across backends
		sqlBytes, err := os.ReadFile(filepath.Join(dir, "lexicon.sql"))
		if err != nil {
			t.Fatalf("[%s] no lexicon.sql: %v", bk.name, err)
		}
		ran++
		if refSQL == nil {
			refSQL = sqlBytes
			refName = bk.name
		} else if string(sqlBytes) != string(refSQL) {
			t.Errorf("[%s] lexicon.sql diverges from [%s] on real data", bk.name, refName)
		}
		// and the loaded db must be query-equivalent (sanity on the first backend)
		dump, err := exec.Command("sqlite3", filepath.Join(dir, "lexicon.db"), q).CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] query: %v\n%s", bk.name, err, dump)
		}
		if n := strings.Count(strings.TrimSpace(string(dump)), "\n") + 1; n < 1000 {
			t.Errorf("[%s] lexicon dump only %d rows -- build likely failed", bk.name, n)
		}
	}
	if ran < 2 {
		t.Skipf("fewer than 2 backends (ran %d)", ran)
	}
}
```

- [ ] **Step 3: Verify the builder gate + the env gate + the full suite**

```
go test ./harness/ -run 'TestBibleConformanceBuilders' -count=1 -v   # PASS (all backends byte-identical)
BIBLE_REPO="$HOME/matt/bible" go test ./harness/ -run 'TestBibleConformanceRealData' -count=1 -v -timeout 1800s   # PASS (~5-6min per backend, byte-identical .sql across all)
go test ./harness/ -run 'TestBible' -count=1   # all bible gates (incl Go/JS Milestone A/B/C) PASS
go test ./harness/ -run 'TestListingsElaborateAndCheck' -count=1   # ok
```

- [ ] **Step 4: Commit**

```bash
git add harness/bible_conformance_test.go
git commit -m "$(printf 'test(harness): cross-backend bible builder divergence-lock (the consumer)\n\nRuns the shared-root + lexicon-db builders on js/go/py/rust/erl and asserts the\noutput files are BYTE-IDENTICAL across all backends (synthetic always-on +\nenv-gated real-lexicon). The consumer that makes the op family non-divergent:\nany per-backend encoding/sort/codec drift fails here. Tier 1 complete.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Self-Review

**1. Spec coverage.** The op family -> Tasks 1 (4 pure), 2 (2 higher-order), 3 (write trio + Handle + sortFile), 4 (dbApply), each on all three target backends. The codec-gate fix (usesStream on py/rust/beam + the closeWrite streamPrims gap) is Task 1 Step 1. The no-divergence "consumer" -> the cross-backend conformance harness, built in Task 1 and extended each task, culminating in Task 5's builder file-byte-identity gate + the env-gated real-data gate. Byte-exact discipline (the divergence trap) is a Global Constraint and is concretely encoded per op (binary mode + latin1 on py, Vec<u8> on rust, char-list/binary on beam). WASM/JVM/native are explicitly out of this tier (the user's tiered choice).

**2. Placeholder scan.** Every op has complete Python/Rust/BEAM bodies + the helpers they need (`_find` rust; `_bsplit`/`_bmkparts`/`_jsfield`/`_jskip`/`_jtake`/`_bquote`/`_blines`/`_fdwalk` beam; the `__WH` thread-local rust). The conformance harness is complete Go. The NOTEs flag the few things an implementer must confirm against the live runtime (exact Rust helper names like `_natusize`/`_nat_of_usize`/`unit`/`vfun`; the `ff_`/`_b` beam naming and `string:find` OTP version; the Milestone-A foldLines listing filename/witness) -- these are verification points, not missing code; the per-backend compile + the conformance gate are the proof.

**3. Type consistency.** Op signatures are the existing contract (unchanged): pure ops have no `_u`; IO ops carry the trailing world token and return the unit value (py `None` / rust `unit()` / beam `unit`). Constructor cells use each backend's native shape (py dict / rust `V::Ctor` / beam `{c,...}` tuple) with the erased type-arg at args[0]. The apply convention (`step s line ()`) is spelled per backend. The conformance harness's `bibleBackend` table + `assertBibleAgree`/`assertBibleFilesAgree` are defined in Task 1/Task 5 and reused consistently; `copyTree`/`loadListing` are the existing harness helpers. The reused listings (ch549/551/552/553/554/557/558/555/559) and fixtures (foldfix/lexfix/lexdbfix) are named exactly; the NOTEs require confirming any whose filename the implementer cannot see directly.
