package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
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

func TestBibleWriteStream(t *testing.T) {
	s := loadListing(t, "ch552_write_stream.rune")
	type bkSpec struct {
		name string
		emit func(codegen.Program) (codegen.TargetSource, error)
		ext  string
		run  func(string) *exec.Cmd
	}
	backends := []bkSpec{
		{"js", codegen.JS{}.Emit, "js", func(f string) *exec.Cmd { return exec.Command("node", f) }},
		{"go", codegen.Go{}.Emit, "go", func(f string) *exec.Cmd { return exec.Command("go", "run", f) }},
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	for _, bk := range backends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			src, err := bk.emit(p)
			if err != nil {
				t.Fatal(err)
			}
			dir := t.TempDir()
			f := filepath.Join(dir, "main."+bk.ext)
			if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
				t.Fatal(err)
			}
			cmd := bk.run(f)
			cmd.Dir = dir // the listing writes+reads the relative "ch552.tmp" here
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("[%s] run failed: %v\n%s", bk.name, err, out)
			}
			if got := strings.TrimSpace(string(out)); got != "2\n2" {
				t.Errorf("[%s] line count = %q, want 2\\n2", bk.name, got)
			}
		})
	}
}

func TestBibleFoldDir(t *testing.T) {
	for _, tg := range []string{"js", "go"} {
		cmd := exec.Command("go", "run", "../../cmd/rune", "run",
			"../../listings/ch554_fold_dir.rune", "main", "--target", tg)
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

func TestBibleBuildSharedRootFixture(t *testing.T) {
	wantBytes, err := os.ReadFile("testdata/lexfix_expected.jsonl")
	if err != nil {
		t.Fatal(err)
	}
	want := string(wantBytes)

	s := loadListing(t, "ch555_build_shared_root.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}

	type bkSpec struct {
		name string
		emit func(codegen.Program) (codegen.TargetSource, error)
		ext  string
		run  func(string) *exec.Cmd
	}
	backends := []bkSpec{
		{"js", codegen.JS{}.Emit, "js", func(f string) *exec.Cmd { return exec.Command("node", f) }},
		{"go", codegen.Go{}.Emit, "go", func(f string) *exec.Cmd { return exec.Command("go", "run", f) }},
	}

	for _, bk := range backends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			src, err := bk.emit(p)
			if err != nil {
				t.Fatal(err)
			}
			dir := t.TempDir()
			copyTree(t, "testdata/lexfix", filepath.Join(dir, "lexfix"))
			f := filepath.Join(dir, "main."+bk.ext)
			if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
				t.Fatal(err)
			}
			cmd := bk.run(f)
			cmd.Dir = dir // builder reads "lexfix", writes "shared-root.out" here
			if out, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("[%s] run failed: %v\n%s", bk.name, err, out)
			}
			got, err := os.ReadFile(filepath.Join(dir, "shared-root.out"))
			if err != nil {
				t.Fatalf("[%s] no output: %v", bk.name, err)
			}
			if string(got) != want {
				t.Errorf("[%s] shared-root.out mismatch:\n--- got ---\n%s\n--- want ---\n%s", bk.name, got, want)
			}
		})
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

// TestBibleSharedRootByteIdentical runs the ch555 builder over the REAL bible
// lexicon (23,681 files) and diffs the output against the committed
// shared-root.jsonl (80,904 lines).  Skipped unless BIBLE_REPO is set.
// The build takes ~5 minutes on the go target.
func TestBibleSharedRootByteIdentical(t *testing.T) {
	repo := os.Getenv("BIBLE_REPO")
	if repo == "" {
		t.Skip("set BIBLE_REPO to run the full byte-identical gate")
	}

	wantBytes, err := os.ReadFile(filepath.Join(repo, "relations/derived/shared-root.jsonl"))
	if err != nil {
		t.Fatalf("expected output not found in BIBLE_REPO: %v", err)
	}

	dir := t.TempDir()

	// Copy the real lexicon tree as "lexfix" in the run dir.
	// filepath.WalkDir does not descend a symlinked root, so we must copy.
	// cp -r takes ~2s for 23,681 files.
	cpCmd := exec.Command("cp", "-r",
		filepath.Join(repo, "lexicon"),
		filepath.Join(dir, "lexfix"))
	if out, err := cpCmd.CombinedOutput(); err != nil {
		t.Fatalf("cp -r lexicon failed: %v\n%s", err, out)
	}

	s := loadListing(t, "ch555_build_shared_root.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}

	src, err := codegen.Go{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}

	f := filepath.Join(dir, "main.go")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}

	cmd := exec.Command("go", "run", f)
	cmd.Dir = dir // builder reads "./lexfix", writes "./shared-root.out"
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("real build failed: %v\n%s", err, out)
	}

	got, err := os.ReadFile(filepath.Join(dir, "shared-root.out"))
	if err != nil {
		t.Fatalf("no output file: %v", err)
	}

	if string(got) != string(wantBytes) {
		gl := strings.Split(string(got), "\n")
		wl := strings.Split(string(wantBytes), "\n")
		t.Errorf("byte-identical mismatch: got %d lines, want %d lines", len(gl), len(wl))
		for i := 0; i < len(gl) && i < len(wl); i++ {
			if gl[i] != wl[i] {
				t.Errorf("first diff at line %d:\n got:  %s\n want: %s", i+1, gl[i], wl[i])
				break
			}
		}
	}
}

func TestBibleSortFile(t *testing.T) {
	s := loadListing(t, "ch553_sort_file.rune")
	type bkSpec struct {
		name string
		emit func(codegen.Program) (codegen.TargetSource, error)
		ext  string
		run  func(string) *exec.Cmd
	}
	backends := []bkSpec{
		{"js", codegen.JS{}.Emit, "js", func(f string) *exec.Cmd { return exec.Command("node", f) }},
		{"go", codegen.Go{}.Emit, "go", func(f string) *exec.Cmd { return exec.Command("go", "run", f) }},
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	for _, bk := range backends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			src, err := bk.emit(p)
			if err != nil {
				t.Fatal(err)
			}
			dir := t.TempDir()
			f := filepath.Join(dir, "main."+bk.ext)
			if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
				t.Fatal(err)
			}
			cmd := bk.run(f)
			cmd.Dir = dir // the listing writes+reads relative "ch553.tmp"/"ch553.sorted" here
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("[%s] run failed: %v\n%s", bk.name, err, out)
			}
			if got := strings.TrimSpace(string(out)); got != "5\n5" {
				t.Errorf("[%s] sorted first-line byteLen = %q, want 5\\n5", bk.name, got)
			}
		})
	}
}

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
		var src codegen.TargetSource
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
		var src codegen.TargetSource
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
		var src codegen.TargetSource
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

// TestBibleLexiconQueryEquivalent builds the ch559 lexicon DB from the real
// 23,681-file bible lexicon and asserts its lexicon table is query-equivalent
// (same sorted dump + COUNT) to a freshly Python-built reference.
// The committed data/tokens.sqlite is a 0-byte stub; the reference is built on
// demand via Python's isolated stdlib-only _load_lexicon (no relations/tokens/
// numpy deps). Skipped unless BIBLE_REPO is set and sqlite3 + python3 are in PATH.
// The go-target build over the full lexicon takes ~6 minutes -- run with -timeout 900s.
func TestBibleLexiconQueryEquivalent(t *testing.T) {
	repo := os.Getenv("BIBLE_REPO")
	if repo == "" {
		t.Skip("set BIBLE_REPO to run the full lexicon query-equivalence gate")
	}
	if _, err := exec.LookPath("sqlite3"); err != nil {
		t.Skip("sqlite3 CLI not in PATH")
	}
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not in PATH")
	}

	const q = "SELECT * FROM lexicon ORDER BY strong IS NULL, strong, lemma, translit, lang, pos, root"

	// Build the Python reference: run build_db's _load_lexicon in isolation.
	// _load_lexicon derives ROOT from build_db.py's __file__, so sys.path.insert
	// makes it find ROOT = repo correctly, independent of cwd.
	pyDir := t.TempDir()
	pyOut := filepath.Join(pyDir, "py_lex.sqlite")
	pyScript := "import sqlite3,sys; sys.path.insert(0,'" + repo + "'); " +
		"from tools.build_db import _SCHEMA,_load_lexicon; " +
		"con=sqlite3.connect('" + pyOut + "'); " +
		"con.executescript(_SCHEMA); _load_lexicon(con); con.commit(); con.close()"
	if out, err := exec.Command("python3", "-c", pyScript).CombinedOutput(); err != nil {
		t.Fatalf("python3 build_db failed: %v\n%s", err, out)
	}

	// Query the Python reference.
	pyRowsOut, err := exec.Command("sqlite3", pyOut, q).CombinedOutput()
	if err != nil {
		t.Fatalf("python db query failed: %v\n%s", err, pyRowsOut)
	}
	pyCountOut, err := exec.Command("sqlite3", pyOut, "SELECT count(*) FROM lexicon").CombinedOutput()
	if err != nil {
		t.Fatalf("python db count failed: %v\n%s", err, pyCountOut)
	}

	// Build the rune DB on the go target over the real lexicon (~6 min for 23,681 files).
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
	// Use cp -r, not a symlink: filepath.WalkDir does NOT descend a symlinked root.
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

	// Query the rune-built DB.
	runeRowsOut, err := exec.Command("sqlite3", filepath.Join(dir, "lexicon.db"), q).CombinedOutput()
	if err != nil {
		t.Fatalf("rune db query failed: %v\n%s", err, runeRowsOut)
	}
	runeCountOut, err := exec.Command("sqlite3", filepath.Join(dir, "lexicon.db"), "SELECT count(*) FROM lexicon").CombinedOutput()
	if err != nil {
		t.Fatalf("rune db count failed: %v\n%s", err, runeCountOut)
	}

	// Assert COUNT(*) match.
	if strings.TrimSpace(string(runeCountOut)) != strings.TrimSpace(string(pyCountOut)) {
		t.Fatalf("lexicon COUNT(*) mismatch: rune %s, python %s",
			strings.TrimSpace(string(runeCountOut)), strings.TrimSpace(string(pyCountOut)))
	}

	// Assert sorted dump match; on mismatch report COUNT + first differing row only.
	if string(runeRowsOut) != string(pyRowsOut) {
		rl := strings.Split(string(runeRowsOut), "\n")
		pl := strings.Split(string(pyRowsOut), "\n")
		t.Errorf("lexicon dump mismatch: rune %d rows, python %d rows", len(rl), len(pl))
		for i := 0; i < len(rl) && i < len(pl); i++ {
			if rl[i] != pl[i] {
				t.Errorf("first diff at row %d:\n rune:   %s\n python: %s", i+1, rl[i], pl[i])
				break
			}
		}
	}
}

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
		var src codegen.TargetSource
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
