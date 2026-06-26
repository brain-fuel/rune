package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// emitRunSeed emits `mainName` from `listing` on backend `bk` ("go" or "js"),
// runs it with WAVELET_SEED set, and returns trimmed stdout. The seeded
// frontier scheduler (par) reads WAVELET_SEED, so a fixed seed pins the
// interleaving; the same seed on both backends must yield identical output.
func emitRunSeed(t *testing.T, bk, listing, mainName, seed string) string {
	t.Helper()
	s := loadListing(t, listing)
	p, err := s.EmitProgram(mainName)
	if err != nil {
		t.Fatal(err)
	}
	var (
		emit func(codegen.Program) (codegen.TargetSource, error)
		ext  string
		bin  string
		run  func(file string) *exec.Cmd
	)
	switch bk {
	case "go":
		emit, ext, bin = codegen.Go{}.Emit, "go", "go"
		run = func(f string) *exec.Cmd { return exec.Command("go", "run", f) }
	case "js":
		emit, ext, bin = codegen.JS{}.Emit, "js", "node"
		run = func(f string) *exec.Cmd { return exec.Command("node", f) }
	default:
		t.Fatalf("unknown backend %q", bk)
	}
	if _, lerr := exec.LookPath(bin); lerr != nil {
		t.Skipf("%s not in PATH", bin)
	}
	src, err := emit(p)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "main."+ext)
	if werr := os.WriteFile(f, []byte(src), 0o644); werr != nil {
		t.Fatal(werr)
	}
	cmd := run(f)
	cmd.Env = append(os.Environ(), "WAVELET_SEED="+seed)
	out, rerr := cmd.Output()
	if rerr != nil {
		stderr := ""
		if ee, ok := rerr.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("[%s] run failed: %v\n%s\n--- src ---\n%s", bk, rerr, stderr, src)
	}
	return strings.TrimSpace(string(out))
}

// TestFrontierConformsGoJS pins the cross-backend proof: under a FIXED seed, the
// Go and JS frontier runtimes produce byte-identical output for a do-block (the
// same LCG, the same ready-pick order). Mirrors Task 3's Go-only guarantee.
func TestFrontierConformsGoJS(t *testing.T) {
	for _, seed := range []string{"0", "1", "7", "42"} {
		goOut := emitRunSeed(t, "go", "ch_frontier_two_prints.rune", "main", seed)
		jsOut := emitRunSeed(t, "js", "ch_frontier_two_prints.rune", "main", seed)
		if goOut != jsOut {
			t.Fatalf("frontier not byte-identical under seed %s:\n go: %q\n js: %q", seed, goOut, jsOut)
		}
	}
}
