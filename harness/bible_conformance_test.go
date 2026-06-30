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
