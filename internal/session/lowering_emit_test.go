package session

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// After `lower sleb to sleW`, a main that calls sleb must EMIT a call to
// sleW, and the sleb definition must be tree-shaken away. This exercises the
// whole pipeline: session.AddLowering populates s.lower (Task 2), emitDefs
// wires it into the eraser (Task 3), and Shake -- which runs AFTER erasure in
// EmitProgram -- keeps only the now-reachable fast twin.
func TestLoweringRedirectsEmit(t *testing.T) {
	s := New()
	src := lowerProg +
		"foreign slebEquiv : (a : Whole) -> (b : Whole) -> Eq Bool (sleb a b) (sleW a b) end\n" +
		"lower sleb to sleW by slebEquiv\n" +
		"main : Bool is sleb (succ zero) (succ (succ zero)) end\n"
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	js, err := codegen.JS{}.Emit(p)
	if err != nil {
		t.Fatalf("js emit: %v", err)
	}
	out := string(js)
	if !strings.Contains(out, "sleW") {
		t.Fatalf("emitted program does not call sleW (redirect did not fire):\n%s", out)
	}
	// The slow def must be shaken out: no top-level `sleb` definition remains.
	for _, d := range p.Defs {
		if d.Name == "sleb" {
			t.Fatalf("sleb was not tree-shaken after redirect")
		}
	}
}
