package repl

import (
	"bytes"
	"os/exec"
	"strings"
	"testing"
)

// TestReplAcceptsDo is the REPL acceptance for `do` (Task 8): a feature is not
// done until it works in `rune repl`. The REPL shares the surface parser, so a
// `do ... end` block desugars to a `par` application, resolves the ambient `par`
// builtin, and type checks — here it normalizes to the expected IO Unit value.
func TestReplAcceptsDo(t *testing.T) {
	script := []string{
		"data Unit : U is unit : Unit end",
		"do (pureIO Unit unit) (pureIO Unit unit) end",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith returned error: %v", err)
	}
	got := out.String()
	// do desugars to a par-spine and checks at type IO Unit.
	if !strings.Contains(got, "par Unit Unit") || !strings.Contains(got, ": IO Unit") {
		t.Fatalf("do did not desugar to a typed par application:\n%s", got)
	}
}

// TestReplRunsDo is the end-to-end acceptance: `:run do ... end` lowers the
// do-block through the erased JS shadow and RUNS the frontier scheduler, so the
// two effects actually fire (default seed 0). Both numbers must appear.
func TestReplRunsDo(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	script := []string{
		"data Unit : U is unit : Unit end",
		"data Nat : U is zero : Nat | succ : Nat -> Nat end",
		"builtin nat Nat zero succ",
		"foreign printNat : Nat -> IO Nat end",
		":run do (printNat 1) (printNat 2) end",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith returned error: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "1") || !strings.Contains(got, "2") {
		t.Fatalf("repl :run did not perform the do-block's effects:\n%s", got)
	}
}
