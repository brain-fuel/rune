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
