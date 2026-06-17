package repl

import (
	"bytes"
	"os/exec"
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
		"double : Whole -> Whole is fn (n : Whole) is n + n end end",
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
		"2 : Whole",  // 1 + 1
		"42 : Whole", // 6 * 7 and double 21
		"0 : Whole",  // 5 - 7
		"3 : Whole",  // 17 // 5
		"6 : Whole",  // gcd 12 18
		"defined double",
		"session cleared; prelude reloaded",
		"4 : Whole", // 2 + 2 after :reset
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	// 17 % 5 = 2 must appear AFTER 17 / 5's "3 : Whole" — both print "2 : Whole"
	// only if 1+1 did too, so just check the count of value lines is right.
	// Numerals bind to Whole (the tower's foundation), so results print as Whole.
	if n := strings.Count(got, " : Whole"); n != 8 {
		t.Errorf("expected 8 Whole value lines, got %d\n--- full output ---\n%s", n, got)
	}
}

// TestREPLRadixAndSigfigs is the acceptance test that the fraction / radix /
// significant-figure feature WORKS AT THE PROMPT (not just as listings): `/`
// builds a fraction, `|>` pipes it into a conversion, and the result prints in
// positional notation — exact (with the repetend bracketed) or rounded.
func TestREPLRadixAndSigfigs(t *testing.T) {
	script := []string{
		"1/3",                       // a fraction prints as a/b
		"3/4 |> to_radix",           // 0.75    (exact, terminating)
		"1/3 |> to_radix",           // 0.{3}   (exact, repeating)
		"1/6 |> to_radix",           // 0.1{6}  (non-repeating head + cycle)
		"1/7 |> to_radix",           // 0.{142857}
		"7/4 |> to_radix_sigplace 1", // 1.8    (round to 1 decimal place)
		"7/4 |> to_radix_sigfig 1",  // 2       (round to 1 significant figure)
		"7/4 |> to_radix_sigfig 2",  // 1.8
		"7/4 |> to_radix_sigfig 3",  // 1.75
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	wants := []string{
		"1/3 : Frac",
		"0.75 : RDec",
		"0.{3} : RDec",
		"0.1{6} : RDec",
		"0.{142857} : RDec",
		"1.8 : RDec",
		"2 : RDec",
		"1.75 : RDec",
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
}

// TestREPLLargeArithAndAST guards two things: (1) the prelude registers +/*/-
// for the bigint accel, so a large product like 4000*4000 returns its literal in
// one step instead of materialising 16M succ nodes and overflowing the stack at
// readback (the reported crash); (2) :ast renders the resolved core as a named
// structural tree (the hashless :core).
func TestREPLLargeArithAndAST(t *testing.T) {
	script := []string{
		"4000 * 4000", // accel: instant, was a stack overflow
		"4000 + 4000",
		"1000 - 1",
		"1000000000000000 * 10000002341234123412341234", // beyond int64: arbitrary precision
		":ast 1 + 2",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	for _, w := range []string{
		"16000000 : Whole", // 4000 * 4000, accelerated
		"8000 : Whole",     // 4000 + 4000
		"999 : Whole",      // 1000 - 1
		"10000002341234123412341234000000000000000 : Whole", // beyond int64, exact
		"((+ 1) 2)", // :ast — names, not @hash; NatLits shown, not ?
	} {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
}

// TestREPLNumericTower guards the Nat/Whole split: Whole is the foundation
// {0,1,2,…} that bare numerals bind to; Nat is the counting numbers {1,2,3,…}
// realized as the POSITIVE wholes (a Σ with a nonzero proof), a genuinely
// distinct type (a bare Peano Nat would hash-collide with Whole); wholeOf
// injects Nat ↪ Whole; and zero cannot inhabit Nat.
func TestREPLNumericTower(t *testing.T) {
	script := []string{
		"5",                   // a bare numeral is a Whole
		":type wholeOf",       // the injection Nat -> Whole (proves Nat is a distinct named type)
		"wholeOf one",         // 1, the least counting number, in Whole
		"wholeOf (nsucc one)", // 2, landed in Whole
		// zero is not a counting number: the nonzero proof is unsatisfiable.
		"bad : Nat is Pair Whole (fn (n : Whole) is Eq Bool (isPos n) true end) zero (refl true) end",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	for _, w := range []string{
		"5 : Whole",                    // numerals are Whole
		"Nat -> Whole",                 // :type wholeOf — distinct types, injection
		"1 : Whole",                    // wholeOf one
		"2 : Whole",                    // wholeOf (nsucc one)
		"cannot unify true with false", // zero rejected from Nat
	} {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
}

// TestREPLRunShadow drives `:run` through a prelude session: the expression is
// type checked by the kernel, then evaluated through the erased JS shadow under
// node — calculation without a certificate. The BigInt shadow applies (the
// prelude's builtin nat is constructor-form), so 35 * 186 is arithmetic, not
// unary chain-walking.
func TestREPLRunShadow(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	script := []string{
		":run 35 * 186",
		":run gcd 252 105",
		":run", // no argument: usage error, loop continues
		"double : Whole -> Whole is fn (n : Whole) is n + n end end",
		":run double 21", // a definition added mid-session is visible to the shadow
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()

	wants := []string{
		"6510",               // 35 * 186 through the shadow
		"21",                 // gcd 252 105
		"usage: :run <expr>", // bare :run
		"defined double",     //
		"42",                 // double 21
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
}

// TestREPLRunTypeError checks `:run` reports an ordinary type error for an
// ill-typed expression. Type checking happens BEFORE emission and before any
// node lookup, so this needs no node and must not skip.
func TestREPLRunTypeError(t *testing.T) {
	script := []string{
		":run U U", // applying a non-function
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	if !strings.Contains(out.String(), "applying a non-function") {
		t.Errorf("expected a type error from :run on an ill-typed expression, got:\n%s", out.String())
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
