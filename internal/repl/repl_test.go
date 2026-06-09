package repl

import (
	"bytes"
	"strings"
	"testing"
)

// TestREPLSession drives a full scripted session through Run and checks the loop reads,
// resolves, shows, accumulates definitions, runs the commands, and survives bad input.
func TestREPLSession(t *testing.T) {
	script := []string{
		"U",
		"id : (A : U) -> A -> A is",
		"  fn (A : U) (x : A) is x end",
		"end",
		"id U",
		"nope",
		":bogus",
		":core fn (A : U) (x : A) is x end",
		":hash id",
		":type id",
		":list",
		"seq let y = U; y end",
		":reset",
		":list",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()

	wants := []string{
		"defined id",                       // multi-line definition accumulated
		"id U",                             // reference printed by name, not #hash
		`unbound identifier "nope"`,        // resolution error, loop continues
		`unknown command ":bogus"`,         // unknown command, loop continues
		"(λ. (λ. #0))",                     // :core de Bruijn view
		"type checking arrives in Phase 1", // :type stub
		"id : (A : U) -> A -> A",           // :list
		"let y = U in y",                   // seq desugars and prints as let
		"session cleared",                  // :reset
		"(no definitions)",                 // :list after reset
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
}

// TestREPLContinuationThenEOF checks that an incomplete form at end of input does not
// hang or panic: Run reports the truncation and returns.
func TestREPLContinuationThenEOF(t *testing.T) {
	in := strings.NewReader("fn (x : U) is x\n") // no 'end', then EOF
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	if !strings.Contains(out.String(), "unexpected end of input") {
		t.Errorf("expected an end-of-input message, got:\n%s", out.String())
	}
}
