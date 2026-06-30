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
