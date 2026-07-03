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
