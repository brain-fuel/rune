package codegen_test

import (
	"os"
	"os/exec"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/session"
)

// wasmLibraryProgram loads the Task-1 G-Counter wire codec listing (ch565) and returns
// the full codegen.Program (every top-level def, no Main set), the shape EmitLibrary
// needs to resolve --export names against.
func wasmLibraryProgram(t *testing.T) codegen.Program {
	t.Helper()
	srcBytes, err := os.ReadFile("../listings/ch565_gc_codec.rune")
	if err != nil {
		t.Skipf("cannot read ch565 listing: %v", err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(srcBytes)); err != nil {
		t.Fatalf("load ch565: %v", err)
	}
	p, err := s.EmitProgram("")
	if err != nil {
		t.Fatalf("ch565 emit-program: %v", err)
	}
	return p
}

func artifactNamed(t *testing.T, arts []codegen.Artifact, suffix string) string {
	t.Helper()
	for _, a := range arts {
		if strings.HasSuffix(a.Path, suffix) {
			return string(a.Data)
		}
	}
	t.Fatalf("no artifact with suffix %q in %+v", suffix, arts)
	return ""
}

// TestWasmLibraryArtifacts pins the library-mode WAT export ABI + the generated
// glue.js shape (Task 2, 6d): the WAT exports exactly the runtime ABI (memory via the
// runtime's own export, init, rt_apply/rt_retain/rt_release, rt_mkbin/rt_bin_len/
// rt_bin_set/rt_bin_at, rt_live, rt_nat_to_u32) plus one def_<name> per requested
// export, NO _start; the glue.js carries the 12-stub WASI object and the marshalling
// helpers.
func TestWasmLibraryArtifacts(t *testing.T) {
	p := wasmLibraryProgram(t)
	spec := codegen.BuildSpec{
		Kind:    codegen.BuildLibrary,
		Module:  "gcounter",
		Exports: []codegen.Export{{RuneName: "merge"}, {RuneName: "value"}},
	}
	set, err := codegen.Wasm{}.EmitLibrary(p, spec)
	if err != nil {
		t.Fatal(err)
	}
	if set.Kind != codegen.BuildLibrary || set.Target != "wasm" {
		t.Fatalf("unexpected set metadata: %+v", set)
	}
	if len(set.Artifacts) != 2 {
		t.Fatalf("artifacts = %+v, want 2 (.wat + .glue.js)", set.Artifacts)
	}

	wat := artifactNamed(t, set.Artifacts, ".wat")
	for _, exp := range []string{
		`(export "memory"`,
		`(export "init"`,
		`(export "rt_apply"`,
		`(export "rt_retain"`,
		`(export "rt_release"`,
		`(export "rt_mkbin"`,
		`(export "rt_bin_len"`,
		`(export "rt_bin_set"`,
		`(export "rt_bin_at"`,
		`(export "rt_live"`,
		`(export "rt_nat_to_u32"`,
		`(export "def_merge"`,
		`(export "def_value"`,
	} {
		if !strings.Contains(wat, exp) {
			t.Fatalf("wat missing export %s:\n%s", exp, wat)
		}
	}
	if strings.Contains(wat, `(export "_start"`) {
		t.Fatal("library module must not export _start")
	}

	glue := artifactNamed(t, set.Artifacts, ".glue.js")
	for _, frag := range []string{
		"wasi_snapshot_preview1", "fd_write", "clock_time_get", "fd_read", "fd_close",
		"fd_seek", "fd_readdir", "path_open", "environ_sizes_get", "environ_get",
		"args_sizes_get", "args_get", "proc_exit",
		"mkBin", "readBin", "readNat", "function call(", "release", "retain",
		"rt_nat_to_u32", "def_merge", "def_value",
	} {
		if !strings.Contains(glue, frag) {
			t.Fatalf("glue missing %q:\n%s", frag, glue)
		}
	}
}

// TestWasmLibraryUnknownExportErrors is the honesty gate: an --export naming a
// definition the program never emitted is a clear error, not a broken/partial module.
func TestWasmLibraryUnknownExportErrors(t *testing.T) {
	p := wasmLibraryProgram(t)
	_, err := codegen.Wasm{}.EmitLibrary(p, codegen.BuildSpec{
		Kind:    codegen.BuildLibrary,
		Exports: []codegen.Export{{RuneName: "doesNotExist"}},
	})
	if err == nil {
		t.Fatal("expected an error for an unknown export")
	}
	if !strings.Contains(err.Error(), "doesNotExist") {
		t.Fatalf("error %v does not name the unknown export", err)
	}
}

// TestWasmLibraryNeedsExports mirrors the Go backend's "at least one export" gate
// (golang_library.go) so the two LibraryBackend implementations behave consistently.
func TestWasmLibraryNeedsExports(t *testing.T) {
	p := wasmLibraryProgram(t)
	_, err := codegen.Wasm{}.EmitLibrary(p, codegen.BuildSpec{Kind: codegen.BuildLibrary})
	if err == nil {
		t.Fatal("expected an error with zero exports")
	}
}

// TestWasmLibraryAppModeUnaffected is the byte-identity guard for the emitModuleCore
// refactor: app-mode Wasm.Emit output on a representative program is unchanged by the
// library-mode split (compares against a second, independent Emit call rather than a
// golden string, since the exact text is pinned by the many other wasm_test.go gates --
// this test's job is only to prove the refactor is deterministic/side-effect-free, not
// to re-pin the format).
func TestWasmLibraryAppModeUnaffected(t *testing.T) {
	p := wasmLibraryProgram(t)
	p.Main = "value"
	out1, err := codegen.Wasm{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	out2, err := codegen.Wasm{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	if out1 != out2 {
		t.Fatal("Wasm.Emit is not deterministic across repeated calls")
	}
	if !strings.Contains(string(out1), `(export "_start"`) {
		t.Fatal("app-mode output should still export _start")
	}
}

// wasmtimePath resolves the wasmtime binary the same way wasm_test.go does
// (~/.wasmtime/bin/wasmtime or PATH); "" when absent.
func wasmtimeLibraryPath() string {
	if home, err := os.UserHomeDir(); err == nil {
		cand := home + "/.wasmtime/bin/wasmtime"
		if _, err := os.Stat(cand); err == nil {
			return cand
		}
	}
	if p, err := exec.LookPath("wasmtime"); err == nil {
		return p
	}
	return ""
}

// TestWasmLibraryInitInvokes is the wasmtime smoke: a library-mode module (no _start)
// parses, instantiates under a WASI host, and its `init` export runs cleanly via
// `wasmtime run --invoke init <f>.wat` (wasmtime 42's supported invocation form for a
// non-_start export). Skips if wasmtime is absent, like the other wasm gates.
func TestWasmLibraryInitInvokes(t *testing.T) {
	wt := wasmtimeLibraryPath()
	if wt == "" {
		t.Skip("wasmtime not found (~/.wasmtime/bin/wasmtime or PATH)")
	}
	p := wasmLibraryProgram(t)
	set, err := codegen.Wasm{}.EmitLibrary(p, codegen.BuildSpec{
		Exports: []codegen.Export{{RuneName: "merge"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	wat := artifactNamed(t, set.Artifacts, ".wat")
	dir := t.TempDir()
	f := dir + "/module.wat"
	if err := os.WriteFile(f, []byte(wat), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command(wt, "run", "--invoke", "init", f).CombinedOutput()
	if err != nil {
		t.Fatalf("wasmtime run --invoke init: %v\n%s\n--- emitted .wat ---\n%s", err, out, wat)
	}
}
