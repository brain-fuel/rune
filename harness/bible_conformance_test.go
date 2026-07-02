package harness

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
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
	// runtime, when non-nil, produces a runtime.c written beside src before
	// compile is called.  Used by the LLVM backend (clang prog.ll runtime.c -o out).
	runtime func(codegen.Program) string
}

func bibleBackends() []bibleBackend {
	bks := []bibleBackend{
		{"js", "node", "js", func(p codegen.Program) (codegen.TargetSource, error) { return codegen.JS{}.Emit(p) },
			func(f string) *exec.Cmd { return exec.Command("node", f) }, nil, nil},
		{"go", "go", "go", func(p codegen.Program) (codegen.TargetSource, error) { return codegen.Go{}.Emit(p) },
			func(f string) *exec.Cmd { return exec.Command("go", "run", f) }, nil, nil},
		{"py", "python3", "py", func(p codegen.Program) (codegen.TargetSource, error) { return codegen.Py{}.Emit(p) },
			func(f string) *exec.Cmd { return exec.Command("python3", f) }, nil, nil},
		{"rs", "rustc", "rs", func(p codegen.Program) (codegen.TargetSource, error) { return codegen.Rust{}.Emit(p) },
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-o", out, src) }, nil},
		{"erl", "escript", "erl", func(p codegen.Program) (codegen.TargetSource, error) { return codegen.Beam{}.Emit(p) },
			func(f string) *exec.Cmd { return exec.Command("escript", f) }, nil, nil},
	}
	if javac, java, ok := findJava25(); ok {
		bks = append(bks, bibleBackend{"jvm", javac, "java",
			func(p codegen.Program) (codegen.TargetSource, error) { return codegen.JVM{}.Emit(p) },
			func(out string) *exec.Cmd { return exec.Command(java, "-cp", filepath.Dir(out), "main") },
			func(src, out string) *exec.Cmd {
				return exec.Command(javac, "--release", "25", "-d", filepath.Dir(out), src)
			}, nil})
	}
	if cc, err := exec.LookPath("cc"); err == nil {
		bks = append(bks, bibleBackend{
			name:    "c",
			bin:     cc,
			ext:     "c",
			emit:    func(p codegen.Program) (codegen.TargetSource, error) { return codegen.C{}.Emit(p) },
			run:     func(bin string) *exec.Cmd { return exec.Command(bin) },
			compile: func(src, out string) *exec.Cmd { return exec.Command(cc, "-o", out, src) },
		})
	}
	if clang, err := exec.LookPath("clang"); err == nil {
		bks = append(bks, bibleBackend{
			name: "ll",
			bin:  clang,
			ext:  "ll",
			emit: func(p codegen.Program) (codegen.TargetSource, error) { return codegen.LL{}.Emit(p) },
			run:  func(bin string) *exec.Cmd { return exec.Command(bin) },
			// compile assumes runtime.c is written beside src by the call sites below.
			compile: func(src, out string) *exec.Cmd {
				return exec.Command(clang, src, filepath.Join(filepath.Dir(src), "runtime.c"), "-o", out)
			},
			runtime: func(p codegen.Program) string { return codegen.LL{}.EmitRuntimeFor(p) },
		})
	}
	// Ninth (tier-4) backend: WASM/WAT run directly under `wasmtime run` -- no separate
	// compile step. `--dir=.` preopens the run's cwd (set by callers via cmd.Dir) as the
	// WASI sandbox root so the builders' relative file reads/writes resolve; harmless for
	// listings that touch no files. dbApply is a documented WASM no-op (wasm.go), so any
	// gate that needs a REAL .db from a WASM run must host-load the emitted .sql itself
	// (see TestBibleWasmDbViaHost) rather than relying on this generic backend entry.
	if wt := wasmtimePathHarness(); wt != "" {
		bks = append(bks, bibleBackend{
			name: "wasm",
			bin:  wt,
			ext:  "wat",
			emit: func(p codegen.Program) (codegen.TargetSource, error) { return codegen.Wasm{}.Emit(p) },
			run:  func(f string) *exec.Cmd { return exec.Command(wt, "run", "--dir=.", f) },
		})
	}
	return bks
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
		if bk.runtime != nil {
			if err := os.WriteFile(filepath.Join(dir, "runtime.c"), []byte(bk.runtime(p)), 0o644); err != nil {
				t.Fatal(err)
			}
		}
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

// assertBibleAgree runs (listing, main) on every backend in its OWN temp dir and
// asserts they all produce `want`. If ANY backend's toolchain is absent the gate is
// inconclusive -- t.Skipf is called naming the missing backend(s) so the run is
// reported as SKIP rather than a false PASS on the reduced set.
func assertBibleAgree(t *testing.T, listing, main, want string) {
	t.Helper()
	var skipped []string
	for _, bk := range bibleBackends() {
		dir := t.TempDir()
		got, ok := runBibleBackend(t, bk, listing, main, dir)
		if !ok {
			t.Logf("[%s] skipped (%s not in PATH)", bk.name, bk.bin)
			skipped = append(skipped, bk.name)
			continue
		}
		if got != want {
			t.Errorf("[%s] %s/%s = %q, want %q (backends must not diverge)", bk.name, listing, main, got, want)
		}
	}
	if len(skipped) > 0 {
		t.Skipf("divergence gate inconclusive -- missing backend toolchain(s): %v", skipped)
	}
}

func TestBibleConformancePure(t *testing.T) {
	assertBibleAgree(t, "ch551_json_field.rune", "strongLen", "5")
	assertBibleAgree(t, "ch551_json_field.rune", "missingLen", "0")
	assertBibleAgree(t, "ch557_sql_quote.rune", "quotePlain", "5")
	assertBibleAgree(t, "ch557_sql_quote.rune", "quoteEmbedded", "6")
}

func TestBibleConformanceFold(t *testing.T) {
	// foldLines over harness/testdata/sample.conllu -> token count 11 (double-printed "11\n11").
	assertBibleAgreeFromTestdata(t, "ch549_conllu_count.rune", "main", "11\n11")
	// foldDir over harness/testdata/foldfix -> 3 matching .json files ("3\n3").
	assertBibleAgreeFromTestdata(t, "ch554_fold_dir.rune", "main", "3\n3")
}

func TestBibleConformanceWriteStream(t *testing.T) {
	// ch552: write two lines, read back with foldLines, print count -> 2\n2.
	assertBibleAgree(t, "ch552_write_stream.rune", "main", "2\n2")
	// ch553: write three lines, sortFile, read back first line's byteLen -> 5\n5.
	assertBibleAgree(t, "ch553_sort_file.rune", "main", "5\n5")
}

func TestBibleConformanceDbApply(t *testing.T) {
	if _, err := exec.LookPath("sqlite3"); err != nil {
		t.Skip("sqlite3 CLI not in PATH")
	}
	// ch558: main builds ch558.db from a 2-row .sql via dbApply; the db is the output.
	// Run on each backend in its own temp dir, then query count(*) -> 2.
	// If ANY backend toolchain is absent the result is inconclusive -- skip rather than
	// silently pass on the reduced set.
	var skipped []string
	for _, bk := range bibleBackends() {
		if bk.name == "wasm" {
			// WASM's dbApply is a documented sandbox no-op (wasmtime preview1 has no
			// subprocess primitive): running ch558 under wasmtime writes ch558.sql but
			// never creates ch558.db, so the in-sandbox `sqlite3 <dir>/ch558.db` query
			// below would fail on a file that was never produced. This is NOT a missing
			// toolchain (do not add to `skipped`, which would mark the whole gate
			// inconclusive) -- it is the documented host-load split. See the dedicated
			// TestBibleWasmDbViaHost, which builds ch558 under wasmtime then has the HOST
			// run `sqlite3 ch558.db ".read ch558.sql"` and asserts the same count=2.
			t.Log("[wasm] dbApply is a sandbox no-op -- host-load path covered by TestBibleWasmDbViaHost")
			continue
		}
		if _, err := exec.LookPath(bk.bin); err != nil {
			skipped = append(skipped, bk.name)
			continue
		}
		dir := t.TempDir()
		_, ok := runBibleBackend(t, bk, "ch558_db_apply.rune", "main", dir)
		if !ok {
			skipped = append(skipped, bk.name)
			continue
		}
		q := exec.Command("sqlite3", filepath.Join(dir, "ch558.db"), "SELECT count(*) FROM t")
		out, err := q.CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] query: %v\n%s", bk.name, err, out)
		}
		if got := strings.TrimSpace(string(out)); got != "2" {
			t.Errorf("[%s] ch558 db count = %q, want 2", bk.name, got)
		}
	}
	if len(skipped) > 0 {
		t.Skipf("divergence gate inconclusive -- missing backend toolchain(s): %v", skipped)
	}
}

// assertBibleAgreeFromTestdata runs (listing, main) on every backend with
// cwd = harness/testdata (so the listing's relative fixture path resolves) and asserts
// they all produce `want`. The emitted source / compiled binary lives in a temp dir but
// is referenced by ABSOLUTE path so cwd can be testdata. If ANY backend toolchain is
// absent the gate is inconclusive -- t.Skipf names the missing backend(s).
func assertBibleAgreeFromTestdata(t *testing.T, listing, main, want string) {
	t.Helper()
	var skipped []string
	for _, bk := range bibleBackends() {
		if _, err := exec.LookPath(bk.bin); err != nil {
			skipped = append(skipped, bk.name)
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
			if bk.runtime != nil {
				if err := os.WriteFile(filepath.Join(dir, "runtime.c"), []byte(bk.runtime(p)), 0o644); err != nil {
					t.Fatal(err)
				}
			}
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
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[%s] %s/%s = %q, want %q (backends must not diverge)", bk.name, listing, main, got, want)
		}
	}
	if len(skipped) > 0 {
		t.Skipf("divergence gate inconclusive -- missing backend toolchain(s): %v", skipped)
	}
}

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
		if bk.runtime != nil {
			if err := os.WriteFile(filepath.Join(dir, "runtime.c"), []byte(bk.runtime(p)), 0o644); err != nil {
				t.Fatal(err)
			}
		}
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
// the first backend that ran. If ANY backend toolchain is absent the gate is
// inconclusive -- t.Skipf names the missing backend(s) rather than passing on the
// reduced set.
func assertBibleFilesAgree(t *testing.T, listing, main, fixture, destName, outFile string) {
	t.Helper()
	var ref []byte
	var refName string
	var skipped []string
	for _, bk := range bibleBackends() {
		data, ok := buildBibleFile(t, bk, listing, main, fixture, destName, outFile)
		if !ok {
			skipped = append(skipped, bk.name)
			continue
		}
		if ref == nil {
			ref = data
			refName = bk.name
			continue
		}
		if string(data) != string(ref) {
			t.Errorf("[%s] %s differs from [%s] (backends diverge):\n%s vs\n%s", bk.name, outFile, refName, data, ref)
		}
	}
	if len(skipped) > 0 {
		t.Skipf("divergence gate inconclusive -- missing backend toolchain(s): %v", skipped)
	}
}

func TestBibleConformanceBuilders(t *testing.T) {
	// shared-root JSONL builder (ch555) over the lexfix fixture -> shared-root.out.
	assertBibleFilesAgree(t, "ch555_build_shared_root.rune", "main", "lexfix", "lexfix", "shared-root.out")
	// lexicon-db builder (ch559) over the lexdbfix fixture -> lexicon.sql (the deterministic text).
	assertBibleFilesAgree(t, "ch559_build_db_lexicon.rune", "main", "lexdbfix", "lexdb", "lexicon.sql")
}

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
	var skipped []string
	for _, bk := range bibleBackends() {
		if _, err := exec.LookPath(bk.bin); err != nil {
			skipped = append(skipped, bk.name)
			continue
		}
		// The native backends (c/ll) are byte-identity-proven on REAL Greek+Hebrew by the
		// synthetic TestBibleConformanceBuilders gate (its lexdbfix fixture holds Hebrew/Greek
		// through the full jsonStrField/sqlQuote/sortFile path) and by construction (the codec is
		// raw-byte, so no charset re-encode is possible). They are excluded from THIS 1500-entry
		// SCALE gate only: the native GC's O(N_live) conservative stack scan makes the bignum
		// codec over 1500 files impractically slow (~30s/entry) -- a parked perf item, not a
		// correctness gap. See PARKING-LOT.md "native GC gc_find_obj O(N_live)".
		if bk.name == "c" || bk.name == "ll" {
			// Deliberate scale-only exclusion -- NOT a missing-toolchain "inconclusive" (do not
			// append to `skipped`, which would trip the partial-toolchain skip and drop the whole
			// gate's assertion). The remaining backends still form the divergence lock.
			t.Logf("real-data scale gate excludes %s (byte-identity proven by TestBibleConformanceBuilders; native GC too slow at N=1500 -- PARKING-LOT)", bk.name)
			continue
		}
		// WASM DELIBERATELY JOINS this scale gate (unlike native c/ll above): its allocator
		// is ARC + a size-classed freelist with NO scanning GC (no O(N_live) sweep), so it does
		// not share native's perf cliff. A local synthetic-scale timing (1500 lexicon-shaped
		// fixtures, same ch559 builder, since BIBLE_REPO is not available in every environment)
		// ran wasmtime in well under 1s -- an order of magnitude faster than the JS backend on
		// the same input -- so excluding WASM here would be unwarranted, not merely cautious.
		dir := t.TempDir()
		// Sample real Greek+Hebrew lexicon entries (not the whole 23,681-file corpus:
		// the per-backend bignum codec over every file totals >1hr across 5 backends).
		// A balanced sample proves cross-backend byte-identity on REAL non-ASCII data in
		// seconds; the full-corpus go-only query-equivalence is TestBibleLexiconQueryEquivalent.
		sampleLexicon(t, repo, filepath.Join(dir, "lexdb"), 1500)
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
			if bk.runtime != nil {
				if err := os.WriteFile(filepath.Join(dir, "runtime.c"), []byte(bk.runtime(p)), 0o644); err != nil {
					t.Fatal(err)
				}
			}
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
		if refSQL == nil {
			refSQL = sqlBytes
			refName = bk.name
		} else if string(sqlBytes) != string(refSQL) {
			t.Errorf("[%s] lexicon.sql diverges from [%s] on real data", bk.name, refName)
		}
		// and the loaded db must be query-equivalent (sanity on every backend). WASM's
		// dbApply is a documented sandbox no-op (see TestBibleConformanceDbApply / wasm.go
		// emitDbApplyWasm), so lexicon.db was never created in-sandbox for wasm -- HOST-load
		// the just-verified byte-identical lexicon.sql exactly as TestBibleWasmDbViaHost does
		// for ch558, then run the same query every other backend's in-sandbox dbApply produced.
		if bk.name == "wasm" {
			if out, err := exec.Command("sqlite3", filepath.Join(dir, "lexicon.db"), ".read "+filepath.Join(dir, "lexicon.sql")).CombinedOutput(); err != nil {
				t.Fatalf("[wasm] host sqlite3 .read lexicon.sql: %v\n%s", err, out)
			}
		}
		dump, err := exec.Command("sqlite3", filepath.Join(dir, "lexicon.db"), q).CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] query: %v\n%s", bk.name, err, dump)
		}
		if n := strings.Count(strings.TrimSpace(string(dump)), "\n") + 1; n < 400 {
			t.Errorf("[%s] lexicon dump only %d rows -- build likely failed", bk.name, n)
		}
	}
	if len(skipped) > 0 {
		t.Skipf("divergence gate inconclusive -- missing backend toolchain(s): %v", skipped)
	}
}

// sampleLexicon copies a deterministic, language-balanced sample of up to maxN real
// lexicon .json files from <repo>/lexicon into dest. Sorting the full file list and
// striding across it spans both grc and hbo (real Greek + Hebrew), so the cross-backend
// byte-identity gate exercises real non-ASCII content without the >1hr full-corpus build.
func sampleLexicon(t *testing.T, repo, dest string, maxN int) {
	t.Helper()
	root := filepath.Join(repo, "lexicon")
	var files []string
	if err := filepath.WalkDir(root, func(p string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && strings.HasSuffix(p, ".json") {
			files = append(files, p)
		}
		return nil
	}); err != nil {
		t.Fatalf("walk lexicon: %v", err)
	}
	sort.Strings(files)
	if err := os.MkdirAll(dest, 0o755); err != nil {
		t.Fatal(err)
	}
	stride := 1
	if len(files) > maxN {
		stride = len(files) / maxN
	}
	n := 0
	for i := 0; i < len(files); i += stride {
		b, err := os.ReadFile(files[i])
		if err != nil {
			continue
		}
		if err := os.WriteFile(filepath.Join(dest, fmt.Sprintf("e%06d.json", i)), b, 0o644); err != nil {
			t.Fatal(err)
		}
		n++
	}
	if n == 0 {
		t.Fatalf("no lexicon files sampled from %s", root)
	}
}

func TestBibleConformanceCRLF(t *testing.T) {
	// foldLines splits on \n only and keeps \r on EVERY backend: sum of line byteLens = 16.
	assertBibleAgreeFromTestdata(t, "ch560_crlf_lines.rune", "main", "16\n16")
}

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

// runNativeListing emits (listing, main) on backend "c" or "ll", compiles, runs
// from cwd (used as-is when non-empty; relative to the harness package dir), and
// returns trimmed stdout + ok=true. ok=false when the required toolchain (cc /
// clang) is absent; callers should break/skip on ok=false. Mirrors runLL
// (ll_test.go:22) for LLVM and the C compile pattern from io_os_test.go. Uses
// EmitRuntimeFor so that the codec (d6_s2h/d6_h2s) and pure bible ops land in the
// linked runtime.c.
func runNativeListing(t *testing.T, backend, listing, main, cwd string) (string, bool) {
	t.Helper()
	switch backend {
	case "c":
		if _, err := exec.LookPath("cc"); err != nil {
			return "", false
		}
	case "ll":
		if _, err := exec.LookPath("clang"); err != nil {
			return "", false
		}
	default:
		t.Fatalf("runNativeListing: unknown backend %q", backend)
	}

	s := loadListing(t, listing)
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("[%s] %s EmitProgram: %v", backend, listing, err)
	}

	dir := t.TempDir()
	bin := filepath.Join(dir, "main.bin")

	switch backend {
	case "c":
		src, err := codegen.C{}.Emit(p)
		if err != nil {
			t.Fatalf("[c] %s Emit: %v", listing, err)
		}
		f := filepath.Join(dir, "main.c")
		if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
		if out, err := exec.Command("cc", "-o", bin, f).CombinedOutput(); err != nil {
			t.Fatalf("[c] %s compile: %v\n%s", listing, err, out)
		}
	case "ll":
		ll, err := codegen.LL{}.Emit(p)
		if err != nil {
			t.Fatalf("[ll] %s Emit: %v", listing, err)
		}
		rt := codegen.LL{}.EmitRuntimeFor(p)
		llf := filepath.Join(dir, "program.ll")
		rtf := filepath.Join(dir, "runtime.c")
		if err := os.WriteFile(llf, []byte(ll), 0o644); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(rtf, []byte(rt), 0o644); err != nil {
			t.Fatal(err)
		}
		if out, err := exec.Command("clang", llf, rtf, "-o", bin).CombinedOutput(); err != nil {
			t.Fatalf("[ll] %s compile: %v\n%s\n--- .ll ---\n%s", listing, err, out, ll)
		}
	}

	runDir := dir
	if cwd != "" {
		runDir = cwd
	}
	cmd := exec.Command(bin)
	cmd.Dir = runDir
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("[%s] %s run: %v", backend, listing, err)
	}
	return strings.TrimSpace(string(out)), true
}

// TestBibleNativeWriteStreamDb verifies the write-stream ops (Handle/openWrite/writeChunk/
// closeWrite/sortFile) and dbApply on both native backends (C and LLVM). ch552/ch553 write
// relative files, so each runs in its own t.TempDir(). ch558 builds a SQLite db and is
// gated on sqlite3 being in PATH. Mirrors TestBibleJVMWriteStreamDb.
func TestBibleNativeWriteStreamDb(t *testing.T) {
	for _, be := range []string{"c", "ll"} {
		for _, c := range []struct{ listing, want string }{
			{"ch552_write_stream.rune", "2\n2"},
			{"ch553_sort_file.rune", "5\n5"},
		} {
			got, ok := runNativeListing(t, be, c.listing, "main", t.TempDir())
			if !ok {
				break
			}
			if got != c.want {
				t.Errorf("%s %s = %q, want %q", be, c.listing, got, c.want)
			}
		}
		// ch558 dbApply: build in a temp cwd, query count(*) -> 2 (skip only the dbApply case if
		// sqlite3 absent -- `continue`, not t.Skip, so the OTHER backend's write-stream/sortFile
		// cases still run rather than halting the whole test on the first iteration).
		if _, err := exec.LookPath("sqlite3"); err != nil {
			t.Logf("[%s] sqlite3 not in PATH -- skip ch558 dbApply", be)
			continue
		}
		s := loadListing(t, "ch558_db_apply.rune")
		p, err := s.EmitProgram("main")
		if err != nil {
			t.Fatalf("[%s] ch558 emit-program: %v", be, err)
		}
		dir := t.TempDir()
		bin := filepath.Join(dir, "main.bin")
		var built bool
		switch be {
		case "c":
			if _, err := exec.LookPath("cc"); err != nil {
				t.Logf("[c] cc not in PATH -- skip ch558")
				continue
			}
			src, err := codegen.C{}.Emit(p)
			if err != nil {
				t.Fatalf("[c] ch558 Emit: %v", err)
			}
			f := filepath.Join(dir, "main.c")
			if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
				t.Fatal(err)
			}
			if out, err := exec.Command("cc", "-o", bin, f).CombinedOutput(); err != nil {
				t.Fatalf("[c] ch558 compile: %v\n%s", err, out)
			}
			built = true
		case "ll":
			if _, err := exec.LookPath("clang"); err != nil {
				t.Logf("[ll] clang not in PATH -- skip ch558")
				continue
			}
			ll, err := codegen.LL{}.Emit(p)
			if err != nil {
				t.Fatalf("[ll] ch558 Emit: %v", err)
			}
			rt := codegen.LL{}.EmitRuntimeFor(p)
			llf := filepath.Join(dir, "program.ll")
			rtf := filepath.Join(dir, "runtime.c")
			if err := os.WriteFile(llf, []byte(ll), 0o644); err != nil {
				t.Fatal(err)
			}
			if err := os.WriteFile(rtf, []byte(rt), 0o644); err != nil {
				t.Fatal(err)
			}
			if out, err := exec.Command("clang", llf, rtf, "-o", bin).CombinedOutput(); err != nil {
				t.Fatalf("[ll] ch558 compile: %v\n%s\n--- .ll ---\n%s", err, out, ll)
			}
			built = true
		}
		if !built {
			continue
		}
		run := exec.Command(bin)
		run.Dir = dir
		if out, err := run.CombinedOutput(); err != nil {
			t.Fatalf("[%s] ch558 run: %v\n%s", be, err, out)
		}
		q, err := exec.Command("sqlite3", filepath.Join(dir, "ch558.db"), "SELECT count(*) FROM t").CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] ch558 query: %v\n%s", be, err, q)
		}
		if got := strings.TrimSpace(string(q)); got != "2" {
			t.Errorf("[%s] ch558 db count = %q, want 2", be, got)
		}
	}
}

// TestBibleNativeFold verifies the two higher-order bible ops (foldLines/foldDir) on
// both native backends (C and LLVM). cwd is "testdata" so relative fixture paths resolve.
// Expected values match the source-backend gates: 11\n11 / 3\n3 / 16\n16.
func TestBibleNativeFold(t *testing.T) {
	cases := []struct{ listing, main, cwd, want string }{
		{"ch549_conllu_count.rune", "main", "testdata", "11\n11"},
		{"ch554_fold_dir.rune", "main", "testdata", "3\n3"},
		{"ch560_crlf_lines.rune", "main", "testdata", "16\n16"},
	}
	for _, be := range []string{"c", "ll"} {
		for _, c := range cases {
			got, ok := runNativeListing(t, be, c.listing, c.main, c.cwd)
			if !ok {
				break
			}
			if got != c.want {
				t.Errorf("%s %s = %q, want %q", be, c.listing, got, c.want)
			}
		}
	}
}

// TestBibleNativePure verifies the 4 pure bible ops (byteLen/splitOn/jsonStrField/sqlQuote)
// on both native backends (C and LLVM). Expected values match the source-backend gates:
// strongLen=5 (byteLen of "G0026") and quoteEmbedded=6 (byteLen of "'a''b'").
func TestBibleNativePure(t *testing.T) {
	cases := []struct{ listing, main, cwd, want string }{
		{"ch551_json_field.rune", "strongLen", "", "5"},
		{"ch557_sql_quote.rune", "quoteEmbedded", "", "6"},
	}
	for _, be := range []string{"c", "ll"} {
		ran := false
		for _, c := range cases {
			got, ok := runNativeListing(t, be, c.listing, c.main, c.cwd)
			if !ok {
				break
			}
			ran = true
			if got != c.want {
				t.Errorf("%s %s/%s = %q, want %q", be, c.listing, c.main, got, c.want)
			}
		}
		if !ran {
			t.Logf("%s toolchain absent -- skipped", be)
		}
	}
}

// wasmtimePathHarness resolves the wasmtime binary (~/.wasmtime/bin/wasmtime or PATH);
// "" when absent. The ninth backend's WAT modules run directly under `wasmtime run`.
func wasmtimePathHarness() string {
	if home, err := os.UserHomeDir(); err == nil {
		cand := filepath.Join(home, ".wasmtime", "bin", "wasmtime")
		if _, err := os.Stat(cand); err == nil {
			return cand
		}
	}
	if p, err := exec.LookPath("wasmtime"); err == nil {
		return p
	}
	return ""
}

// runWasmListing emits (listing, main) to WAT (codegen.Wasm) and runs it under wasmtime,
// returning trimmed stdout + ok=true. ok=false when wasmtime is absent (callers skip).
// Pure ops need no sandbox flags; when cwd != "" it is passed as the wasmtime preopen
// (--dir) and the process cwd, which later file/env/argv tasks require.
func runWasmListing(t *testing.T, listing, main, cwd string) (string, bool) {
	t.Helper()
	wt := wasmtimePathHarness()
	if wt == "" {
		return "", false
	}
	s := loadListing(t, listing)
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("%s emit-program: %v", listing, err)
	}
	src, err := codegen.Wasm{}.Emit(p)
	if err != nil {
		t.Fatalf("%s wasm emit: %v", listing, err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "module.wat")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	var cmd *exec.Cmd
	if cwd != "" {
		cmd = exec.Command(wt, "run", "--dir=.", f)
		cmd.Dir = cwd
	} else {
		cmd = exec.Command(wt, "run", f)
	}
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("%s wasmtime run: %v", listing, err)
	}
	return strings.TrimSpace(string(out)), true
}

// runWasmListingD6 is the Task-5 sibling of runWasmListing: it additionally passes
// `--env=KEY=VAL` (one per entry) and the program's argv as trailing positional
// arguments (wasmtime's `<WASM>...` accepts the module path followed directly by its
// CLI arguments -- NO `--` separator; wasmtime's own clap parser treats a literal `--`
// as argv[1] itself, which was caught by TestD6WasmArgvExit shifting argCountCode/
// argAtCode by one entry) -- the two sandbox flags the D6 env/argv/exit layer needs
// (getEnvCode/argAtCode read them; exitWith may terminate the module with a non-zero
// status). Returns trimmed
// stdout, the process exit code, and ok=true; ok=false when wasmtime is absent (callers
// skip). Unlike runWasmListing, a non-zero exit is NOT fataled here -- exitWith
// terminating the module with a caller-chosen status is the expected, tested behavior
// (mirrors TestD6NativeArgvExit's *exec.ExitError handling for the native gate).
func runWasmListingD6(t *testing.T, listing, main, cwd string, env, args []string) (string, int, bool) {
	t.Helper()
	wt := wasmtimePathHarness()
	if wt == "" {
		return "", 0, false
	}
	s := loadListing(t, listing)
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("%s emit-program: %v", listing, err)
	}
	src, err := codegen.Wasm{}.Emit(p)
	if err != nil {
		t.Fatalf("%s wasm emit: %v", listing, err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "module.wat")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	wtArgs := []string{"run"}
	if cwd != "" {
		wtArgs = append(wtArgs, "--dir=.")
	}
	for _, e := range env {
		wtArgs = append(wtArgs, "--env="+e)
	}
	wtArgs = append(wtArgs, f)
	wtArgs = append(wtArgs, args...)
	cmd := exec.Command(wt, wtArgs...)
	if cwd != "" {
		cmd.Dir = cwd
	}
	out, err := cmd.Output()
	exitCode := 0
	if err != nil {
		ee, ok := err.(*exec.ExitError)
		if !ok {
			t.Fatalf("%s wasmtime run: %v", listing, err)
		}
		exitCode = ee.ExitCode()
	}
	return strings.TrimSpace(string(out)), exitCode, true
}

// TestBibleWasmPure gates the 4 PURE bible ops (byteLen/splitOn/jsonStrField/sqlQuote)
// on the ninth backend: the packed-String codec + op bodies produce the byte-identical
// cross-backend results under wasmtime (ch551 -> 5, ch557 -> 6). Skips if wasmtime absent.
func TestBibleWasmPure(t *testing.T) {
	if wasmtimePathHarness() == "" {
		t.Skip("wasmtime not available")
	}
	cases := []struct{ listing, main, cwd, want string }{
		{"ch551_json_field.rune", "strongLen", "", "5"},
		{"ch557_sql_quote.rune", "quoteEmbedded", "", "6"},
	}
	for _, c := range cases {
		got, ok := runWasmListing(t, c.listing, c.main, c.cwd)
		if !ok {
			t.Skip("wasmtime not available")
		}
		if got != c.want {
			t.Errorf("wasm %s/%s = %q, want %q", c.listing, c.main, got, c.want)
		}
	}
}

// TestBibleWasmWriteStream gates the Task-4 write-stream ops (openWrite/writeChunk/
// closeWrite/sortFile) on the ninth backend: the WASI file-write helper ($d6_wopen via
// path_open O_CREAT|O_TRUNC + fd_write) + the $D6WH linear-memory fd handle table run
// under `wasmtime run --dir=<tmp>` and produce the byte-identical cross-backend results:
//   ch552 -> 2\n2 (write 2 lines, foldLines counts them)
//   ch553 -> 5\n5 (write cherry/apple/banana, sortFile, foldLines first-line byteLen)
// Each case gets its own t.TempDir() as the wasmtime preopen/cwd so files created inside
// the module land in a fresh sandbox. Skips if wasmtime absent.
func TestBibleWasmWriteStream(t *testing.T) {
	if wasmtimePathHarness() == "" {
		t.Skip("wasmtime not available")
	}
	cases := []struct{ listing, want string }{
		{"ch552_write_stream.rune", "2\n2"},
		{"ch553_sort_file.rune", "5\n5"},
	}
	for _, c := range cases {
		got, ok := runWasmListing(t, c.listing, "main", t.TempDir())
		if !ok {
			t.Skip("wasmtime not available")
		}
		if got != c.want {
			t.Errorf("wasm %s = %q, want %q", c.listing, got, c.want)
		}
	}
}

// TestBibleWasmFold gates the 2 HIGHER-ORDER file/dir bible ops (foldLines/foldDir) on the
// ninth backend: the WASI file-read helper ($d6_readfile via path_open/fd_read) + the
// recursive dir walk ($d6_foldwalk via path_open O_DIRECTORY/fd_readdir) run under
// `wasmtime run --dir=<cwd>` and produce the byte-identical cross-backend results --
// ch549 -> 11\n11 (CoNLL-U token count over sample.conllu), ch554 -> 3\n3 (three .json
// files, one in a subdir, sorted depth-first), ch560 -> 16\n16 (the \n-only split KEEPING
// \r over the CRLF fixture). cwd is "testdata" so the listings' relative fixture paths
// resolve; the wasmtime preopen is set from cwd by runWasmListing. Skips if wasmtime absent.
func TestBibleWasmFold(t *testing.T) {
	if wasmtimePathHarness() == "" {
		t.Skip("wasmtime not available")
	}
	cases := []struct{ listing, main, cwd, want string }{
		{"ch549_conllu_count.rune", "main", "testdata", "11\n11"},
		{"ch554_fold_dir.rune", "main", "testdata", "3\n3"},
		{"ch560_crlf_lines.rune", "main", "testdata", "16\n16"},
	}
	for _, c := range cases {
		got, ok := runWasmListing(t, c.listing, c.main, c.cwd)
		if !ok {
			t.Skip("wasmtime not available")
		}
		if got != c.want {
			t.Errorf("wasm %s/%s = %q, want %q", c.listing, c.main, got, c.want)
		}
	}
}

// TestBibleWasmFoldDirCodecStep is a REGRESSION PIN for the $D6FDDIR/$D6FDSUF window-
// clobber bug (see emitFoldDirWasm's dedicated-window comment in wasm.go): WASM foldDir
// used to decode its PERSISTENT dir/suffix strings into the SHARED codec scratch windows
// $D6BUF/$D6BUF2, which a codec-consuming step (jsonStrField, here) clobbers mid-walk, so
// only the alphabetically-first matching file was ever folded and every subsequent file's
// suffix comparison silently failed. TestBibleWasmFold does NOT catch this (its foldDir
// case, ch554, uses a pure countStep with no codec op in the step body, so the shared
// window never gets clobbered). The only OTHER test that would catch a reintroduction is
// the 9-way TestBibleConformanceBuilders, which t.Skipf's its ENTIRE gate the moment any
// one of the 9 backend toolchains is missing -- so on a machine (or CI lane) missing e.g.
// rustc or clang, that gate never runs at all and a regression ships silent. This test
// runs ONLY the wasmtime leg, standalone, so it always executes whenever wasmtime is
// present, independent of the other 8 toolchains.
//
// Reuses the ch555 shared-root builder (foldDir -> extractStep, which calls jsonStrField
// twice per file) over the committed "lexfix" fixture (testdata/lexfix, 7 files) and diffs
// the resulting shared-root.out against the committed testdata/lexfix_expected.jsonl (4
// edge lines derived from 6 of the 7 files -- e6.json's null root is excluded). This has
// teeth: under the bug, only the alphabetically-first file (e1.json) is ever folded, its
// solo root-group produces NO pairs (flushPairs needs >=2 keys sharing a root), so
// shared-root.out comes out EMPTY instead of the 4 expected lines -- a stark, unmissable
// mismatch, not an off-by-one. Skips if wasmtime is absent.
func TestBibleWasmFoldDirCodecStep(t *testing.T) {
	wt := wasmtimePathHarness()
	if wt == "" {
		t.Skip("wasmtime not available")
	}
	want, err := os.ReadFile("testdata/lexfix_expected.jsonl")
	if err != nil {
		t.Fatal(err)
	}

	s := loadListing(t, "ch555_build_shared_root.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Wasm{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}

	dir := t.TempDir()
	copyTree(t, "testdata/lexfix", filepath.Join(dir, "lexfix"))
	f := filepath.Join(dir, "module.wat")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command(wt, "run", "--dir=.", f)
	cmd.Dir = dir // builder reads "lexfix", writes "shared-root.out" here
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("wasmtime run: %v\n%s", err, out)
	}
	got, err := os.ReadFile(filepath.Join(dir, "shared-root.out"))
	if err != nil {
		t.Fatalf("no output shared-root.out: %v", err)
	}
	if string(got) != string(want) {
		t.Errorf("wasm shared-root.out mismatch (empty or truncated to 1 file's worth of\n"+
			"output is the D6FDDIR/D6FDSUF window-clobber regression):\n--- got ---\n%s\n--- want ---\n%s",
			got, want)
	}
}

// TestBibleWasmDbViaHost is the HOST-LOAD half of the dbApply split documented at wasm.go
// (emitDbApplyWasm): WASI preview1 has no subprocess primitive, so on WASM `dbApply` is a
// no-op that leaves the .sql script on disk instead of shelling out to sqlite3. This test
// proves that host-side .sql is still correct: it runs ch558_db_apply.rune under wasmtime
// (writing ch558.sql, dbApply no-op'd), then has the HOST run
// `sqlite3 ch558.db ".read ch558.sql"` to build the db exactly the way the OTHER eight
// backends' in-sandbox dbApply would have, then asserts count(*) = 2 -- the same assertion
// TestBibleConformanceDbApply makes for every other backend. WASM is deliberately excluded
// from that shared in-sandbox loop (see the "wasm" case there) and covered here instead.
func TestBibleWasmDbViaHost(t *testing.T) {
	if wasmtimePathHarness() == "" {
		t.Skip("wasmtime not available")
	}
	if _, err := exec.LookPath("sqlite3"); err != nil {
		t.Skip("sqlite3 CLI not in PATH")
	}
	dir := t.TempDir()
	if _, ok := runWasmListing(t, "ch558_db_apply.rune", "main", dir); !ok {
		t.Skip("wasmtime not available")
	}
	sqlFile := filepath.Join(dir, "ch558.sql")
	if _, err := os.Stat(sqlFile); err != nil {
		t.Fatalf("wasm run did not produce ch558.sql: %v", err)
	}
	dbFile := filepath.Join(dir, "ch558.db")
	if out, err := exec.Command("sqlite3", dbFile, ".read "+sqlFile).CombinedOutput(); err != nil {
		t.Fatalf("host sqlite3 .read %s: %v\n%s", sqlFile, err, out)
	}
	q, err := exec.Command("sqlite3", dbFile, "SELECT count(*) FROM t").CombinedOutput()
	if err != nil {
		t.Fatalf("query: %v\n%s", err, q)
	}
	if got := strings.TrimSpace(string(q)); got != "2" {
		t.Errorf("wasm (host-loaded) ch558 db count = %q, want 2", got)
	}
}
