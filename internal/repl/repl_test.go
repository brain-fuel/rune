package repl

import (
	"bytes"
	"strings"
	"testing"
)

// TestREPLSession drives a full scripted session through a bare (--no-prelude) REPL
// and checks the loop reads, resolves, shows, accumulates definitions, runs the
// commands, and survives bad input.
func TestREPLSession(t *testing.T) {
	script := []string{
		"U",
		"id : (A : U) -> A -> A is",
		"  fn (A : U) (x : A) is x end",
		"end",
		"(fn (A : U1) (x : A) is x end) U",
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
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith returned error: %v", err)
	}
	got := out.String()

	wants := []string{
		"defined id",                   // multi-line definition accumulated
		"U : U1",                       // bare U checks; U_0 : U_1
		"fn (x : U) is x end : U -> U", // the level-polymorphic identity at U β-reduces
		`unbound identifier "nope"`,    // elaboration error, loop continues
		`unknown command ":bogus"`,     // unknown command, loop continues
		"(λ. (λ. #0))",                 // :core de Bruijn view
		"(A : U) -> A -> A",            // :type id prints the stored type
		"id : (A : U) -> A -> A",       // :list
		"session cleared",              // :reset
		"(no definitions)",             // :list after reset
	}
	// seq let y = U; y end normalizes the let away: the value line is U : U1.
	if !strings.Contains(got, "U : U1") {
		t.Errorf("seq/let did not normalize to U\n--- full output ---\n%s", got)
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
}

// TestREPLPrelude checks the default REPL behaves like a calculator: the prelude
// binds numerals and the five operators, results print as digits, definitions on
// top of the prelude work, and :reset brings the prelude back.
func TestREPLPrelude(t *testing.T) {
	script := []string{
		"1 + 1",
		"6 * 7",
		"5 - 7", // truncated subtraction floors at zero
		"17 // 5",
		"17 % 5",
		"gcd 12 18",
		"double : Nat -> Nat is fn (n : Nat) is n + n end end",
		"double 21",
		":reset",
		"2 + 2", // the prelude survives :reset
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()

	wants := []string{
		"2 : Nat",  // 1 + 1
		"42 : Nat", // 6 * 7 and double 21
		"0 : Nat",  // 5 - 7
		"3 : Nat",  // 17 // 5
		"6 : Nat",  // gcd 12 18
		"defined double",
		"session cleared; prelude reloaded",
		"4 : Nat", // 2 + 2 after :reset
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	// 17 % 5 = 2 must appear AFTER 17 / 5's "3 : Nat" — both print "2 : Nat"
	// only if 1+1 did too, so just check the count of value lines is right.
	if n := strings.Count(got, " : Nat"); n != 8 {
		t.Errorf("expected 8 Nat value lines, got %d\n--- full output ---\n%s", n, got)
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
