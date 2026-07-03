package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// runNatMainNode emits (listing, main) to JS and runs it under node, returning the
// trimmed stdout. Mirrors TestListingsEmitAndExecute's node idiom for a plain-Nat
// main (whose $show under builtin-nat is a decimal digit).
func runNatMainNode(t *testing.T, listing, main string) string {
	t.Helper()
	s := loadListing(t, listing)
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("%s emit-program %s: %v", listing, main, err)
	}
	out, err := codegen.Default().Emit(p)
	if err != nil {
		t.Fatalf("%s js emit %s: %v", listing, main, err)
	}
	f := filepath.Join(t.TempDir(), "main.js")
	if err := os.WriteFile(f, []byte(out), 0o644); err != nil {
		t.Fatal(err)
	}
	got, err := exec.Command("node", f).CombinedOutput()
	if err != nil {
		t.Fatalf("%s node %s: %v\n%s", listing, main, err, got)
	}
	return strings.TrimSpace(string(got))
}

// TestGCCodecRuns is the runtime cross-check for the proven G-Counter codec (ch565,
// 6d Task 1). The kernel theorem `codecRoundTrip` covers only the in-language byte
// list (List Nat), because foreign ops are permanently neutral in the kernel; this
// test exercises what the theorem cannot:
//   - `demo`    : the in-language round trip value (decodeGC (encodeGC (gc 2 1))),
//                 which must observe 3 on js AND wasm.
//   - `demoBin` : the SAME round trip driven THROUGH the foreign Bin edge
//                 (gcFromBin (gcToBin (gc 2 1))) -- the binCons fold + binAt walk
//                 wrappers the kernel says nothing about -- which must also be 3.
// js runs under node; wasm under wasmtime (skipped when absent, like the other
// wasm gates).
func TestGCCodecRuns(t *testing.T) {
	const listing = "ch565_gc_codec.rune"
	const want = "3"

	t.Run("js", func(t *testing.T) {
		if _, err := exec.LookPath("node"); err != nil {
			t.Skip("node not in PATH")
		}
		if got := runNatMainNode(t, listing, "demo"); got != want {
			t.Fatalf("[js] demo = %q, want %q", got, want)
		}
		if got := runNatMainNode(t, listing, "demoBin"); got != want {
			t.Fatalf("[js] demoBin (foreign Bin edge) = %q, want %q", got, want)
		}
	})

	t.Run("wasm", func(t *testing.T) {
		if wasmtimePathHarness() == "" {
			t.Skip("wasmtime not found")
		}
		if got, ok := runWasmListing(t, listing, "demo", ""); !ok {
			t.Skip("wasmtime not found")
		} else if got != want {
			t.Fatalf("[wasm] demo = %q, want %q", got, want)
		}
		if got, ok := runWasmListing(t, listing, "demoBin", ""); !ok {
			t.Skip("wasmtime not found")
		} else if got != want {
			t.Fatalf("[wasm] demoBin (foreign Bin edge) = %q, want %q", got, want)
		}
	})
}
