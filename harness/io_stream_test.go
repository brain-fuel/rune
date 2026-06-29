package harness

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// runRune runs `rune run FILE NAME --target T` from the repo root and returns trimmed stdout.
func runRune(t *testing.T, file, name, target string) string {
	t.Helper()
	cmd := exec.Command("go", "run", "./cmd/rune", "run", file, name, "--target", target)
	cmd.Dir = ".."
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("rune run %s %s --target %s failed: %v\n%s", file, name, target, err, out)
	}
	return strings.TrimSpace(string(out))
}

func TestStreamSplitOn(t *testing.T) {
	for _, target := range []string{"js", "go"} {
		if got := runRune(t, "listings/ch548_string_split.rune", "partsLen", target); got != "3" {
			t.Errorf("[%s] partsLen = %q, want 3", target, got)
		}
		if got := runRune(t, "listings/ch548_string_split.rune", "firstLen", target); got != "3" {
			t.Errorf("[%s] firstLen = %q, want 3", target, got)
		}
	}
}

func TestStreamConlluCount(t *testing.T) {
	for _, target := range []string{"js", "go"} {
		cmd := exec.Command("go", "run", "../../cmd/rune", "run",
			"../../listings/ch549_conllu_count.rune", "main", "--target", target)
		cmd.Dir = "testdata" // so the listing's relative "sample.conllu" resolves
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("[%s] run failed: %v\n%s", target, err, out)
		}
		if got := strings.TrimSpace(string(out)); got != "11\n11" {
			t.Errorf("[%s] token count = %q, want 11", target, got)
		}
	}
}

func TestStreamBoundedMemory(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping 1M-line streaming gate in -short")
	}
	dir := t.TempDir()
	fixture := filepath.Join(dir, "sample.conllu")
	f, err := os.Create(fixture)
	if err != nil {
		t.Fatal(err)
	}
	const n = 1_000_000
	w := bufio.NewWriter(f)
	for i := 0; i < n; i++ {
		// every line is a token line: "1<tab>word<tab>word<tab>X"
		fmt.Fprintf(w, "%d\tword\tword\tX\n", i+1)
	}
	if err := w.Flush(); err != nil {
		t.Fatal(err)
	}
	f.Close()

	// resolve paths absolutely, since cmd.Dir will be the temp fixture dir.
	listing, err := filepath.Abs("../listings/ch549_conllu_count.rune")
	if err != nil {
		t.Fatal(err)
	}
	repoRoot, err := filepath.Abs("..")
	if err != nil {
		t.Fatal(err)
	}

	// Build the rune binary from the repo root (so go.mod is found), then exec
	// the resulting binary from the fixture dir (so "sample.conllu" resolves).
	runeExe := filepath.Join(dir, "rune_tmp")
	build := exec.Command("go", "build", "-o", runeExe, "./cmd/rune")
	build.Dir = repoRoot
	if buildOut, buildErr := build.CombinedOutput(); buildErr != nil {
		t.Fatalf("go build rune failed: %v\n%s", buildErr, buildOut)
	}

	cmd := exec.Command(runeExe, "run", listing, "main", "--target", "go")
	cmd.Dir = dir // the listing opens the relative "sample.conllu" here
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("bounded-memory run failed: %v\n%s", err, out)
	}
	if got := strings.TrimSpace(string(out)); got != "1000000\n1000000" {
		t.Errorf("1M-line count = %q, want \"1000000\\n1000000\"", got)
	}
}
