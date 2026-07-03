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

// ============================================================================
// Task 3 (6d): the node+wabt browser-shaped harness substrate + the single-instance
// smoke gate. Everything below builds a real .wat/.glue.js pair via
// codegen.Wasm{}.EmitLibrary (the Task 2 API) and drives it through
// harness/browserlib/driver.mjs, which assembles the WAT with the wabt npm package
// (bin/setup.sh section 7 installs it into harness/node_modules) and loads it through
// the generated glue exactly as a browser would (WebAssembly.instantiate against stub
// WASI imports -- no wasmtime, no filesystem).
// ============================================================================

// requireBrowserLib skips the calling test unless both node and the wabt npm package
// are available. wabt is installed by `bin/setup.sh` (section 7) into
// harness/node_modules; it is deliberately NOT vendored (npm packages are not
// committed to the repo), so its absence is a normal, silent skip like every other
// external-toolchain gate in this package (wasmtime, docker, rustc, ...).
func requireBrowserLib(t *testing.T) {
	t.Helper()
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH - the browser-library gate SKIPS")
	}
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(filepath.Join(wd, "node_modules", "wabt")); err != nil {
		t.Skip("harness/node_modules/wabt not installed - run bin/setup.sh (section 7)")
	}
}

// runBrowserLibExports builds `listing` into a WASM library artifact set for the given
// exports (module names the generated .wat/.glue.js pair, mirroring BuildSpec.Module),
// writes the pair into a temp dir, and runs the COMMITTED harness/browserlib/driver.mjs
// against them under node, returning trimmed stdout.
//
// Module-resolution note (the brief's open question, resolved by measurement):
// driver.mjs is invoked IN PLACE at its real harness/browserlib/driver.mjs location
// (cmd.Dir = the harness package directory, the script path passed relative to that
// dir) rather than copied into the temp artifact dir. This is deliberate: node's ESM
// loader resolves a bare specifier (driver.mjs's `import("wabt")`) by walking up the
// directory tree from the IMPORTING FILE's own location, not from the process's
// current working directory, and NOT via $NODE_PATH -- confirmed by hand: `NODE_PATH=
// .../harness/node_modules node -e 'import("wabt")'` from an unrelated temp directory
// fails ("Cannot find package 'wabt'"), because $NODE_PATH is a CommonJS-only
// resolution mechanism the ESM loader ignores. Running driver.mjs from
// harness/browserlib/ lets that upward walk reach harness/node_modules naturally
// (browserlib/'s parent); copying the script into a temp directory would break
// resolution unless node_modules were copied alongside it too, which the brief did not
// ask for and would make every test run pay a real npm-package copy. Only the
// GENERATED .wat/.glue.js artifacts (which have no node_modules dependency of their
// own) live in the temp dir, passed to driver.mjs as absolute paths.
func runBrowserLibExports(t *testing.T, listing string, exports []codegen.Export, module, scenario string) string {
	t.Helper()
	requireBrowserLib(t)

	s := loadListing(t, listing)
	p, err := s.EmitProgram("")
	if err != nil {
		t.Fatalf("%s emit-program: %v", listing, err)
	}
	spec := codegen.BuildSpec{Kind: codegen.BuildLibrary, Module: module, Exports: exports}
	set, err := codegen.Wasm{}.EmitLibrary(p, spec)
	if err != nil {
		t.Fatalf("%s EmitLibrary: %v", listing, err)
	}

	dir := t.TempDir()
	var watPath, gluePath string
	for _, a := range set.Artifacts {
		fp := filepath.Join(dir, a.Path)
		if err := os.WriteFile(fp, a.Data, 0o644); err != nil {
			t.Fatal(err)
		}
		switch {
		case strings.HasSuffix(a.Path, ".wat"):
			watPath = fp
		case strings.HasSuffix(a.Path, ".glue.js"):
			gluePath = fp
		}
	}
	if watPath == "" || gluePath == "" {
		t.Fatalf("EmitLibrary artifacts missing .wat/.glue.js: %+v", set.Artifacts)
	}

	harnessDir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("node", filepath.Join("browserlib", "driver.mjs"), watPath, gluePath, scenario)
	cmd.Dir = harnessDir
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("node browserlib/driver.mjs %s %s: %v\n%s", listing, scenario, err, out)
	}
	return strings.TrimSpace(string(out))
}

// runBrowserLib is the plain-Rune-name convenience wrapper: every export is
// host-visible under its bare Rune name (no HostName override). Builds the artifacts
// into a temp dir, copies (in effect; see runBrowserLibExports) driver.mjs, runs
// `node driver.mjs <wat> <glue> <scenario>`, and returns trimmed stdout. Task 4 reuses
// this for the `converge` and `converge-reversed` scenarios.
func runBrowserLib(t *testing.T, listing string, exports []string, scenario string) string {
	t.Helper()
	exp := make([]codegen.Export, len(exports))
	for i, name := range exports {
		exp[i] = codegen.Export{RuneName: name}
	}
	return runBrowserLibExports(t, listing, exp, "browserlib", scenario)
}

// TestWasmBrowserLibrarySmoke is the single-instance smoke gate (Task 3): a real WAT
// module, assembled by wabt and loaded through the generated glue exactly as a browser
// would, drives initGC -> bump -> bump -> value through the export ABI and prints the
// result. Skips (does not fail) when node or the wabt npm package is absent.
func TestWasmBrowserLibrarySmoke(t *testing.T) {
	out := runBrowserLib(t, "ch565_gc_codec.rune",
		[]string{"initGC", "bump", "value", "merge", "encodeGC", "decodeGC", "gcToBin", "gcFromBin"},
		"smoke")
	// line 1: the counter value; line 2: rt_live after the calls.
	lines := strings.Split(out, "\n")
	if lines[0] != "2" {
		t.Fatalf("smoke value: got %q want 2 (full output %q)", lines[0], out)
	}
}

// TestWasmBrowserLibraryHostNameExport verifies the HostName-override path
// end-to-end, which Task 2 built (EXPORT_MAP keys by HostName when set) but left
// untested against a real load -- Task 2's own test corpus only exercised bare
// RuneName exports. Here `value` is exported under the host-facing name "getValue"
// and the driver's "smoke-host" scenario calls it by that name, not "value".
func TestWasmBrowserLibraryHostNameExport(t *testing.T) {
	exports := []codegen.Export{
		{RuneName: "initGC"},
		{RuneName: "bump"},
		{RuneName: "value", HostName: "getValue"},
	}
	out := runBrowserLibExports(t, "ch565_gc_codec.rune", exports, "gcounterhost", "smoke-host")
	lines := strings.Split(out, "\n")
	if lines[0] != "2" {
		t.Fatalf("smoke-host value via HostName override: got %q want 2 (full output %q)", lines[0], out)
	}
}
