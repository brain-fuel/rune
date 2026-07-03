package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// TestScribeLockL3 is the first cross-language divergence lock of the scribe
// port (L3/L5 seed): ch563's exact-rational rasterizer renders the lock
// instance (32x32, RoundRect(4,4,24,24) r=6 continuous, tol 1/20, nonzero)
// and must agree ALPHA FOR ALPHA with the committed fixture produced by the
// Go engine (goforge.dev/scribe tools/lockgen). Go rasterizes in disciplined
// float64; rune in exact rationals; the masks must be byte-identical.
func TestScribeLockL3(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	want, err := os.ReadFile(filepath.Join("testdata", "scribe_lock", "lock32.txt"))
	if err != nil {
		t.Fatal(err)
	}
	s := loadListing(t, "ch563_scribe_raster.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Default().Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	js := filepath.Join(dir, "lock.js")
	if err := os.WriteFile(js, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("node", js).Output()
	if err != nil {
		t.Fatal(err)
	}
	// main's IO result is $show-printed after the 1024 alphas; drop it.
	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	if len(lines) != 1025 {
		t.Fatalf("expected 1024 alphas + result line, got %d lines", len(lines))
	}
	got := strings.Join(lines[:1024], "\n") + "\n"
	if got != string(want) {
		gl, wl := strings.Split(got, "\n"), strings.Split(string(want), "\n")
		diff := 0
		first := -1
		for i := range wl {
			if i < len(gl) && gl[i] != wl[i] {
				diff++
				if first < 0 {
					first = i
				}
			}
		}
		t.Fatalf("mask differs from the Go fixture: %d of 1024 alphas (first at index %d: got %s want %s)",
			diff, first, gl[first], wl[first])
	}
}

// TestScribeLockL5 is the CORPUS lock, the port's last rung: ch565's full
// pipeline (display-list interpreter over the exact geometry and rasterizer,
// nonzero and even-odd, transforms and state) renders both corpus scenes and
// must agree alpha for alpha with the fixtures the Go engine produced from
// the same scenes (scribe testdata/corpus/*.scr via dl.Masks + tools/lockgen).
func TestScribeLockL5(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	w1, err := os.ReadFile(filepath.Join("testdata", "scribe_lock", "corpus1.txt"))
	if err != nil {
		t.Fatal(err)
	}
	w2, err := os.ReadFile(filepath.Join("testdata", "scribe_lock", "corpus2.txt"))
	if err != nil {
		t.Fatal(err)
	}
	want := string(w1) + string(w2)
	s := loadListing(t, "ch565_scribe_corpus.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Default().Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	js := filepath.Join(dir, "corpus.js")
	if err := os.WriteFile(js, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("node", js).Output()
	if err != nil {
		t.Fatal(err)
	}
	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	wantN := strings.Count(want, "\n")
	if len(lines) != wantN+1 {
		t.Fatalf("expected %d alphas + result line, got %d lines", wantN, len(lines))
	}
	got := strings.Join(lines[:wantN], "\n") + "\n"
	if got != want {
		gl, wl := strings.Split(got, "\n"), strings.Split(want, "\n")
		diff, first := 0, -1
		for i := range wl {
			if i < len(gl) && gl[i] != wl[i] {
				diff++
				if first < 0 {
					first = i
				}
			}
		}
		t.Fatalf("corpus differs from the Go fixtures: %d alphas (first at %d: got %s want %s)",
			diff, first, gl[first], wl[first])
	}
}
