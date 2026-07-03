package codegen_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// TestShakeDoubleRuneJS is the spec acceptance test: emit examples/double.rune
// with the shared prelude for the JS backend and assert that:
//  1. The prelude-only symbol `lebEquiv` (line 205 in prelude.rune) does NOT
//     appear in the output -- it is unreachable from `main` and must be shaken.
//  2. The `printFloat` host-body marker IS in the output -- it is reachable
//     from `main` (via getFloat/printFloat in the IO chain) and must survive.
func TestShakeDoubleRuneJS(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "examples", "double.rune"))
	if err != nil {
		t.Skip("examples/double.rune not found")
	}
	s := session.New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("loading double.rune: %v", err)
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatalf("EmitProgram: %v", err)
	}
	js, err := codegen.JS{}.Emit(p)
	if err != nil {
		t.Fatalf("JS.Emit: %v", err)
	}
	emitted := string(js)

	// Probe 1: the prelude symbol `lebEquiv` must not appear in the output.
	// jsName("lebEquiv") = "lebEquiv" (no operator characters requiring mangling).
	if strings.Contains(emitted, "lebEquiv") {
		t.Error("shake failed: prelude-only symbol 'lebEquiv' is present in the JS output")
	}

	// Probe 2: the printFloat host-body marker must appear in the output because
	// main uses printFloat via bindIO/getFloat.
	if !strings.Contains(emitted, "printFloat") {
		t.Error("shake over-pruned: 'printFloat' is absent from the JS output")
	}
}

// TestShakeCaseElimReachabilityJS verifies that a program using a datatype and
// a case expression still emits correctly after tree-shaking: the eliminator
// (generated from Datas via LowerElim at emit time) must be present, and the
// unreachable defs must be absent.
func TestShakeCaseElimReachabilityJS(t *testing.T) {
	// A minimal program: Bool data + a def using BoolElim + an unreachable def.
	const src = `
data Bool : U is
  false : Bool
| true  : Bool
end

not : Bool -> Bool is
  fn (b : Bool) is BoolElim (fn (x : Bool) is Bool end) true false b end
end

unused : Bool is false end

main : Bool is not false end
`
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatalf("EmitProgram: %v", err)
	}
	js, err := codegen.JS{}.Emit(p)
	if err != nil {
		t.Fatalf("JS.Emit: %v", err)
	}
	emitted := string(js)

	// The eliminator must be present (it is emitted from Datas, never pruned).
	if !strings.Contains(emitted, "BoolElim") {
		t.Error("eliminator 'BoolElim' missing from JS output post-shake")
	}

	// The unreachable def must not appear.
	if strings.Contains(emitted, "unused") {
		t.Error("unreachable def 'unused' present in JS output post-shake")
	}

	// Run it and verify the result.
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "main.js")
	if err := os.WriteFile(f, []byte(emitted), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("node", f).CombinedOutput()
	if err != nil {
		t.Fatalf("node: %v\n%s\n--- emitted ---\n%s", err, out, emitted)
	}
	if got := strings.TrimSpace(string(out)); got != "true" {
		t.Errorf("not false = %q, want 'true'", got)
	}
}
