package repl

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

// TestREPLReload is the D7 dev-loop gate: :reload re-reads the last :load'd file from
// disk, so an edit on disk takes effect live. Content-addressing makes it a hot reload
// — the unchanged data decl hits the cache, the edited `dv` re-elaborates and its name
// rebinds (latest-wins). We load dv = 1, edit the file to dv = 2, :reload, and see 2.
func TestREPLReload(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "m.rune")
	v1 := "data Nat : U is zero : Nat | succ : Nat -> Nat end\ndv : Nat is succ zero end\n"
	v2 := "data Nat : U is zero : Nat | succ : Nat -> Nat end\ndv : Nat is succ (succ zero) end\n"
	if err := os.WriteFile(path, []byte(v1), 0o644); err != nil {
		t.Fatal(err)
	}
	s := session.New()
	st := &replState{}
	var out bytes.Buffer
	if err := runCommand(s, Config{NoPrelude: true}, st, ":load "+path, &out); err != nil {
		t.Fatalf(":load: %v", err)
	}
	if err := os.WriteFile(path, []byte(v2), 0o644); err != nil {
		t.Fatal(err)
	}
	out.Reset()
	if err := runCommand(s, Config{NoPrelude: true}, st, ":reload", &out); err != nil {
		t.Fatalf(":reload: %v", err)
	}
	out.Reset()
	if err := runForm(s, st, "dv", &out); err != nil {
		t.Fatalf("eval dv: %v", err)
	}
	if !strings.Contains(out.String(), "succ (succ zero)") {
		t.Errorf("after :reload, dv = %q, want it to reflect the edit (succ (succ zero))", out.String())
	}
}

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
		"I can't find `nope` in scope", // elaboration error, loop continues
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

// TestREPLDeclParity checks the REPL has FULL parity with the file loader on
// top-level forms: a multi-line `data` declaration, a `foreign` axiom, a `partial`
// definition, REDEFINITION (editing — latest wins), and a `module` block. Every form
// `rune` accepts in a source file works interactively.
func TestREPLDeclParity(t *testing.T) {
	script := []string{
		"data Nat : U is",
		"  zero : Nat",
		"| succ : Nat -> Nat",
		"end",
		"foreign hp : Nat -> Nat end",
		"partial loopz : Nat -> Nat is fn (n : Nat) is loopz n end end",
		"dbl : Nat -> Nat is fn (n : Nat) is succ (succ n) end end",
		"dbl zero",
		// EDIT: redefine dbl; the new body must take effect (latest wins).
		"dbl : Nat -> Nat is fn (n : Nat) is succ (succ (succ n)) end end",
		"dbl zero",
		"module M is",
		"  k : Nat is succ zero end",
		"end",
		"M.k",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith returned error: %v", err)
	}
	got := out.String()
	wants := []string{
		"declared Nat zero succ NatElim", // multi-line data
		"defined hp",                     // foreign axiom
		"defined loopz",                  // partial definition
		"defined dbl",                    // function definition
		"succ (succ zero)",               // dbl zero with the FIRST body
		"succ (succ (succ zero))",        // dbl zero with the REDEFINED body (editing works)
		"defined M.k",                    // module block (qualified name)
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
}

// TestREPLMutualData checks a `mutual data` group (MB1) declares in the REPL and its
// per-member eliminator computes — a feature isn't done until it works in `rune repl`.
func TestREPLMutualData(t *testing.T) {
	script := []string{
		"data Nat : U is",
		"  zero : Nat",
		"| succ : Nat -> Nat",
		"end",
		"mutual",
		"  data Tree : U is node : Forest -> Tree end",
		"  data Forest : U is fnil : Forest | fcons : Tree -> Forest -> Forest end",
		"end",
		"forestLen : Forest -> Nat is fn (f : Forest) is ForestElim (fn (x : Forest) is Nat end) zero (fn (t : Tree) is fn (rest : Forest) is fn (ih : Nat) is succ ih end end end) f end end",
		"forestLen (fcons (node fnil) (fcons (node fnil) fnil))",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith returned error: %v", err)
	}
	got := out.String()
	wants := []string{
		"declared Tree node TreeElim Forest fnil fcons ForestElim", // the whole group
		"succ (succ zero)", // forestLen of a 2-tree forest
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
		"-2 : Int",   // 5 - 7 promotes to Int (Whole is not closed under subtraction)
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
	// Seven Whole value lines: 1+1, 6*7, 17//5, 17%5, gcd, double 21, 2+2. The
	// eighth expression, 5-7, now promotes out of Whole to Int (ℕ is not closed
	// under subtraction), so it is NOT counted here.
	if n := strings.Count(got, " : Whole"); n != 7 {
		t.Errorf("expected 7 Whole value lines, got %d\n--- full output ---\n%s", n, got)
	}
}

// TestREPLTowerArithmetic is the acceptance test that `+ - *` are OVERLOADED
// across the numeric tower: the same operators add/subtract/multiply wholes and
// fractions, mixed with the fraction builder `/`, with the dictionary resolved
// from the argument types (typeclass dispatch from inferred type arguments) and
// numerals defaulting only after the surrounding type is known. Results are in
// lowest terms.
func TestREPLTowerArithmetic(t *testing.T) {
	script := []string{
		"1 + 1",         // Whole addition, the foundation
		"1/3 - 2/3",     // fraction subtraction (signed), reduced
		"(1/3) - (2/3)", // parenthesised, same
		"1/3 + 2/3",     // sums to one whole
		"2/3 + 1/6",     // common denominator
		"1/3 * 2",       // fraction times a (defaulted) whole numeral
		"1/(3*2)",       // a whole product in the denominator (numerals stay whole)
		"1/3 - 1/3",     // to zero
		"2/4",           // a typed fraction reduces to lowest terms
		"2/3 * 3/2",     // product reduces to a whole-valued fraction
		"4000 * 4000",   // Semiring dispatch still hits the bigint accel post-rewire
		"(1/2) * (2/3)", // fraction multiplication through the same dispatch
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	wants := []string{
		"2 : Whole",        // 1 + 1
		"-1/3 : Frac",      // 1/3 - 2/3 (and the parenthesised form)
		"1 : Frac",         // 1/3 + 2/3
		"5/6 : Frac",       // 2/3 + 1/6
		"2/3 : Frac",       // 1/3 * 2
		"1/6 : Frac",       // 1/(3*2)
		"0 : Frac",         // 1/3 - 1/3
		"1/2 : Frac",       // 2/4 reduced
		"1/3 : Frac",       // (1/2) * (2/3)
		"16000000 : Whole", // 4000 * 4000, accelerated, through Semiring dispatch
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	if strings.Contains(got, "error") || strings.Contains(got, "mismatch") {
		t.Errorf("unexpected error in tower arithmetic\n--- full output ---\n%s", got)
	}
}

// TestREPLNegationPromotes is the acceptance gate for prefix `-`: negation PROMOTES
// out of the unsigned wholes into the signed Int rung, exactly as binary `-` does
// (`negate` is result-indexed `NegR R A`, with `Whole -> Int`, `Int -> Int`,
// `Frac -> Frac`). So a bare `-3` is an Int, NOT a Frac; `-1/3` stays a Frac (the
// operand under `/` is signed Int, and Int / Int climbs to ℚ via `divInt`).
func TestREPLNegationPromotes(t *testing.T) {
	script := []string{
		"-3",                // bare negation of a whole numeral promotes to Int
		"negate 3",          // the same, spelled out
		"-3 + 1",            // stays Int through addition
		"-3 * 2",            // stays Int through multiplication
		"negate (negate 3)", // double negation, Int -> Int
		"-3 - 2",            // Int minus Int
		"-1/3",              // (negate 1) / 3 — the operand is Int, the quotient Frac
		"-2/4",              // negated, reduced
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	wants := []string{
		"-3 : Int",    // -3
		"-2 : Int",    // -3 + 1
		"-6 : Int",    // -3 * 2
		"3 : Int",     // negate (negate 3)
		"-5 : Int",    // -3 - 2
		"-1/3 : Frac", // -1/3
		"-1/2 : Frac", // -2/4 reduced
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	if strings.Contains(got, "error") || strings.Contains(got, "mismatch") {
		t.Errorf("unexpected error in negation promotion\n--- full output ---\n%s", got)
	}
}

// TestREPLDecimalLiterals is the acceptance gate for decimal-literal INPUT: a
// decimal is parse-time sugar for an exact fraction, so it flows through the tower
// like any `Frac`. Terminating (1.3) and repeating (1.{3}, 0.1{6}) forms both work;
// results stay fractional (input-only sugar), and `|> to_radix` recovers a decimal.
func TestREPLDecimalLiterals(t *testing.T) {
	script := []string{
		"1.3 + 2.5",               // 13/10 + 5/2 = 19/5
		"1.{3} + 2/3",             // 4/3 + 2/3 = 2
		"0.1{6}",                  // repeating input reduces to 1/6
		"0.75",                    // terminating reduces to 3/4
		"-2.5",                    // prefix minus on a decimal
		"(1.3 + 2.5) |> to_radix", // back to a decimal for display
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	wants := []string{
		"19/5 : Frac", // 1.3 + 2.5
		"2 : Frac",    // 1.{3} + 2/3
		"1/6 : Frac",  // 0.1{6}
		"3/4 : Frac",  // 0.75
		"-5/2 : Frac", // -2.5
		"3.8 : RDec",  // (1.3 + 2.5) |> to_radix
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	if strings.Contains(got, "error") || strings.Contains(got, "mismatch") {
		t.Errorf("unexpected error in decimal literals\n--- full output ---\n%s", got)
	}
}

// TestREPLContractGuardSugar is the REPL acceptance test for the `with post …
// guard … blame …` contract-guard sugar (D3 / R-FFI, ch440): a feature is not done
// until it works in `rune repl`. The sugar must desugar, elaborate, and compute both
// arms in a live session, binding the guarded call once.
func TestREPLContractGuardSugar(t *testing.T) {
	script := []string{
		"data Bool : U is",
		"  true  : Bool",
		"| false : Bool",
		"end",
		"data Nat : U is",
		"  zero : Nat",
		"| succ : Nat -> Nat",
		"end",
		"builtin nat Nat zero succ",
		"data Result : U -> U -> U is",
		"  ok  : (A : U) -> (E : U) -> A -> Result A E",
		"| err : (A : U) -> (E : U) -> E -> Result A E",
		"end",
		"data Blame : U is",
		"  overBudget : Nat -> Blame",
		"end",
		"small : Nat -> Bool is fn (n : Nat) is case n of | zero -> true | succ k -> false end end end",
		"guarded : Nat -> Result Nat Blame is fn (n : Nat) is n with post r guard (small r) blame (overBudget r) end end",
		"guarded zero",               // small zero = true  -> ok
		"guarded (succ (succ zero))", // small (2) = false -> err, carrying 2
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith returned error: %v", err)
	}
	got := out.String()
	wants := []string{
		"defined guarded",              // the sugared def elaborated
		"ok Nat Blame 0",               // the ok arm computes
		"err Nat Blame (overBudget 2)", // the blame arm computes, carrying the value
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	if strings.Contains(got, "error") || strings.Contains(got, "mismatch") {
		t.Errorf("unexpected error in contract-guard sugar\n--- full output ---\n%s", got)
	}
}

// TestREPLIntTower is the acceptance test for the Int rung and PROMOTION: ℕ is
// not closed under subtraction, so `-` climbs into the signed integers (2 − 5 =
// −3 : Int), composes (the result is a first-class Int, not a stuck type), and
// stays at Frac for fractions. A provable non-negative difference can stay Whole
// (`minus` with `b ≤ a`); `monus` truncates; and division is defended by a Result.
func TestREPLIntTower(t *testing.T) {
	script := []string{
		"2 - 5",                 // promotes: Whole − Whole = Int
		"5 - 2",                 // 3, still Int (no truncation surprise)
		"(2 - 5) + 1",           // the Int composes with +
		"2 - 5 - 1",             // chained subtraction
		"intOf 7",               // the Int injection
		"1/3 - 2/3",             // fractions still subtract to Frac
		"monus 2 5",             // truncating whole subtraction floors at 0
		"minus 5 2 (refl true)", // provable b ≤ a keeps the result Whole
		"divChecked 7 2",        // checked division: ok
		"divChecked 7 0",        // checked division: err on zero divisor
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	for _, w := range []string{
		"-3 : Int",
		"3 : Int",
		"-2 : Int",
		"-4 : Int",
		"7 : Int",
		"-1/3 : Frac",
		"0 : Whole", // monus 2 5
		"3 : Whole", // minus 5 2 (refl true)
		"ok 7/2 : Result Frac ArithErr",
		"err: cannot divide 7 by 0 : Result Frac ArithErr",
	} {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
}

// TestREPLDemotion is the acceptance test for the checked DESCENT down the tower:
// `3/1` is `3 : Frac` (a rational that happens to be whole), and `toWhole`/`toInt`/
// `toNat` demote it only when it is integral (denominator 1) — `toWhole`/`toNat`
// only when non-negative, and `toNat` only when nonzero (a counting number) —
// returning a `Result A ArithErr`. The failure branch is the DISCRIMINATED UNION
// `ArithErr`, each variant carrying its operands and folded to a message at the
// print boundary. The implicit climb up is met by an explicit, checked climb down.
func TestREPLDemotion(t *testing.T) {
	script := []string{
		"3/1",                      // a whole-valued fraction stays Frac (no implicit demotion)
		"toWhole (3/1)",            // explicit, checked: ok 3
		"toWhole (6/2)",            // reduces first: ok 3
		"toWhole (1/3)",            // not integral: err
		"toWhole (0/5)",            // zero is whole: ok 0
		"toInt (6/2)",              // ok 3 : Result Int ArithErr
		"toInt ((2/3) - (8/3))",    // integral negative: ok -2 (Int holds the sign)
		"toWhole ((2/3) - (8/3))",  // negative: err (no whole answer)
		"toNat (6/2)",              // counting number: ok 3
		"toNat (0/5)",              // zero is not a counting number: err
		"toNat (1/3)",              // not integral: err
		"toNat ((2/3) - (8/3))",    // negative: err
		"divChecked 5 (monus 1 1)", // a computed-zero denominator: divByZero carries it
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	for _, w := range []string{
		"3 : Frac",                     // 3/1
		"ok 3 : Result Whole ArithErr", // toWhole (3/1) and (6/2)
		"err: 1/3 is not an integer : Result Whole ArithErr",    // toWhole (1/3)
		"ok 0 : Result Whole ArithErr",                          // toWhole (0/5)
		"ok 3 : Result Int ArithErr",                            // toInt (6/2)
		"ok -2 : Result Int ArithErr",                           // toInt of an integral negative
		"err: -2 is negative : Result Whole ArithErr",           // toWhole of a negative
		"ok 3 : Result Nat ArithErr",                            // toNat (6/2)
		"err: 0 is not a counting number : Result Nat ArithErr", // toNat (0/5)
		"err: 1/3 is not an integer : Result Nat ArithErr",      // toNat (1/3)
		"err: -2 is negative : Result Nat ArithErr",             // toNat negative
		"err: cannot divide 5 by 0 : Result Frac ArithErr",      // computed-zero divisor
	} {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
}

// TestREPLResultRefs is the acceptance test for numbered, referenceable results —
// the irb x jshell fusion. Every submitted form claims a line number N (irb-style,
// shown in the prompt); a bare-expression result is named $N (jshell-style) and
// bound so later input can recall it by $N or by a bare $ (the latest). Definitions
// consume a number but produce no $N. `:reset` zeroes the counter.
func TestREPLResultRefs(t *testing.T) {
	script := []string{
		"1 + 1",   // line 1 -> $1 ==> 2
		"$1 + $1", // recall $1 -> $2 ==> 4
		"double : Whole -> Whole is fn (n : Whole) is n + n end end", // line 3: a def, no $3
		"double $", // bare $ = last result ($2 = 4) -> $4 ==> 8
		"$ * 3",    // bare $ = $4 = 8 -> $5 ==> 24
		":reset",   // line 6: clears + zeroes the counter
		"2 + 2",    // numbering restarts -> $1 ==> 4
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	for _, w := range []string{
		"$1 ==> 2 : Whole",  // first result is $1
		"$2 ==> 4 : Whole",  // $1 + $1, recalling a prior result
		"defined double",    // a def consumes line 3 but emits no $3
		"$4 ==> 8 : Whole",  // double $ : bare $ recalled the last result (4)
		"$5 ==> 24 : Whole", // $ * 3 : bare $ chained to the new last result (8)
	} {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	// After :reset the counter restarts, so the next result is $1 again — and there
	// must be exactly two "$1 ==> " lines in the whole session (before and after reset).
	if n := strings.Count(got, "$1 ==> "); n != 2 {
		t.Errorf("expected two $1 results (pre- and post-reset), got %d\n%s", n, got)
	}
	if !strings.Contains(got, "$1 ==> 4 : Whole") {
		t.Errorf("post-reset result should renumber to $1 ==> 4\n%s", got)
	}
}

// TestREPLDivisionPromotes is the regression test that `/` PROMOTES Whole / Whole
// to Frac (ℕ is not closed under division, exactly as − promotes to ℤ) — so it works
// on bound Whole VALUES, not just bare numeral literals that inject to Frac. The bug:
// `$3/5` (a referenced Whole result) failed with "expected Frac, got Whole" while the
// literal `16/5` worked.
func TestREPLDivisionPromotes(t *testing.T) {
	script := []string{
		"3 + 5",         // $1 ==> 8 : Whole  (a bound Whole)
		"$1 / 5",        // bound Whole / numeral -> Frac
		"$1 / $1",       // bound Whole / bound Whole -> Frac (= 1)
		"16 / 5",        // both numerals still work
		"(1/2) / (1/3)", // Frac / Frac still works
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	for _, w := range []string{
		"8 : Whole",   // 3 + 5
		"8/5 : Frac",  // $1 / 5
		"1 : Frac",    // $1 / $1
		"16/5 : Frac", // 16 / 5
		"3/2 : Frac",  // (1/2) / (1/3)
	} {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	if strings.Contains(got, "mismatch") || strings.Contains(got, "got Whole") {
		t.Errorf("division of a Whole value should promote to Frac, not type-error\n%s", got)
	}
}

// TestREPLRadixAndSigfigs is the acceptance test that the fraction / radix /
// significant-figure feature WORKS AT THE PROMPT (not just as listings): `/`
// builds a fraction, `|>` pipes it into a conversion, and the result prints in
// positional notation — exact (with the repetend bracketed) or rounded.
func TestREPLRadixAndSigfigs(t *testing.T) {
	script := []string{
		"1/3",                        // a fraction prints as a/b
		"3/4 |> to_radix",            // 0.75    (exact, terminating)
		"1/3 |> to_radix",            // 0.{3}   (exact, repeating)
		"1/6 |> to_radix",            // 0.1{6}  (non-repeating head + cycle)
		"1/7 |> to_radix",            // 0.{142857}
		"7/4 |> to_radix_sigplace 1", // 1.8    (round to 1 decimal place)
		"7/4 |> to_radix_sigfig 1",   // 2       (round to 1 significant figure)
		"7/4 |> to_radix_sigfig 2",   // 1.8
		"7/4 |> to_radix_sigfig 3",   // 1.75
		"-1/3",                       // negatives: a signed fraction, not 0/3
		"-1/3 |> to_radix",           // -0.{3}
		"-3/4 |> to_radix",           // -0.75
		"-7/4 |> to_radix_sigfig 2",  // -1.8
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
		"-1/3 : Frac",
		"-0.{3} : RDec",
		"-0.75 : RDec",
		"-1.8 : RDec",
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
		"999 : Int",        // 1000 - 1 promotes to Int (ℕ not closed under subtraction)
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
	if !strings.Contains(out.String(), "it is not a function") {
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

// TestREPLParseFromString is the acceptance test for the goal: getting Wholes,
// Ints, and Rationals from the command line. A `"…"` literal is a PACKED binary
// string (a single bignum, not a [Char] list — "Haskell's binary thing only
// better"); it computes in the kernel, so parseWhole/parseInt/parseFrac run in the
// REPL. Parsing is TOTAL — a bad token or a 0 denominator is an `err` value, never
// a crash (D1) — and rationals come back reduced.
func TestREPLParseFromString(t *testing.T) {
	script := []string{
		`strHead "abc"`,    // first byte of a packed string: 'a' = 97
		`parseWhole "42"`,  // ok 42
		`parseInt "-5"`,    // ok -5 (signed)
		`parseInt "7"`,     // ok 7
		`parseFrac "3/4"`,  // ok 3/4
		`parseFrac "-7/2"`, // ok -7/2
		`parseFrac "6/4"`,  // reduced to 3/2
		`parseFrac "10"`,   // a bare integer reads as 10/1
		`parseWhole "4x"`,  // err (notNumber) — total, not a crash
		`parseFrac "1/0"`,  // err (badDenom) — total, not a crash
		`parseWhole ""`,    // err (emptyStr)
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	wants := []string{
		"97 : Whole", // strHead "abc"
		"ok 42 : Result Whole ParseErr",
		"ok -5 : Result Int ParseErr",
		"ok 7 : Result Int ParseErr",
		"ok 3/4 : Result Frac ParseErr",
		"ok -7/2 : Result Frac ParseErr",
		"ok 3/2 : Result Frac ParseErr", // 6/4 reduced
		"ok 10 : Result Frac ParseErr",  // "10" as 10/1
		"err : Result Whole ParseErr",   // "4x" and "" both
		"err : Result Frac ParseErr",    // "1/0"
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	if strings.Contains(got, "mismatch") {
		t.Errorf("unexpected error\n--- full output ---\n%s", got)
	}
}

// TestREPLStringDisplay is the acceptance test for goal-1: a string literal must
// READ BACK as a string, not as the packed bignum it desugars to. It covers a plain
// literal, escape round-tripping (\n \t \" \\), the empty string, multi-byte content
// past int64 (so the *big.Int decode path is exercised), and that the prelude string
// ops (strTail, strApp) return values that also fold to literals.
func TestREPLStringDisplay(t *testing.T) {
	script := []string{
		`"hello"`,                        // a plain literal folds to itself
		`"tab\there\nand\"quote\"\\end"`, // every escape round-trips
		`""`,                             // the empty string (just the sentinel)
		`"this is a fairly long string well past sixty-four bits of packing"`,
		`strTail "abc"`,      // a computed string still folds
		`strApp "foo" "bar"`, // concatenation folds
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	wants := []string{
		`"hello" : Bytes`,
		`"tab\there\nand\"quote\"\\end" : Bytes`,
		`"" : Bytes`,
		`"this is a fairly long string well past sixty-four bits of packing" : Bytes`,
		`"bc" : Bytes`,
		`"foobar" : Bytes`,
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	// The raw packed form must NOT leak through anywhere.
	if strings.Contains(got, "bytes ") {
		t.Errorf("packed `bytes <n>` form leaked instead of a string literal\n%s", got)
	}
}

// TestREPLBinaryRoundTrip is the acceptance test for the "both / only better" half:
// a Binary class (encode + a TOTAL decoder) over the packed-Bytes string, and the
// round-trip decode∘encode = id shown COMPUTING. `encode` dispatches on the tower
// instance; the prelude's rt* lemmas additionally PROVE the round-trip (refl) at
// load, so reaching this test at all means they held.
func TestREPLBinaryRoundTrip(t *testing.T) {
	script := []string{
		`encode 42`,             // dispatch Binary Whole -> packed "42"
		`encode (3/4)`,          // dispatch Binary Frac  -> packed "3/4"
		`roundWhole 42`,         // parse (encode 42) = ok 42
		`roundInt (2 - 5)`,      // ok -3 (signed round-trip)
		`roundFrac (1/3 - 2/3)`, // ok -1/3 (reduced, signed)
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	wants := []string{
		`"42" : Bytes`,                   // encode 42 (packed "42", folded to its literal)
		"ok 42 : Result Whole ParseErr",  // roundWhole 42
		"ok -3 : Result Int ParseErr",    // roundInt (2-5)
		"ok -1/3 : Result Frac ParseErr", // roundFrac (1/3-2/3)
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	if strings.Contains(got, "error") || strings.Contains(got, "mismatch") {
		t.Errorf("unexpected error\n--- full output ---\n%s", got)
	}
}

// TestREPLStringConcat is the acceptance test for the overloaded `++` (Magma
// dispatch) on packed-Bytes strings: concatenation COMPUTES in the REPL and folds back to
// a string literal, byte length is right, and `strEq` decides equality. The whole point
// of the user's `"hello"++"world"`.
func TestREPLStringConcat(t *testing.T) {
	script := []string{
		`"hello" ++ "world"`,              // concat, folds to "helloworld"
		`strLen "hello"`,                  // byte length 5
		`strEq ("foo" ++ "bar") "foobar"`, // decidable equality
		`strEq ("foo" ++ "bar") "foobaz"`, // ...and its negation
		`"" ++ "x"`,                       // left identity (empty string)
		`"x" ++ ""`,                       // right identity
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	wants := []string{
		`"helloworld" : Bytes`,
		"true : Bool",
		"false : Bool",
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	// both identity laws hold observationally: "x" appears on its own line for "" ++ "x".
	if !strings.Contains(got, `"x" : Bytes`) {
		t.Errorf("string identity (empty ++) did not fold to \"x\"\n--- full output ---\n%s", got)
	}
	if strings.Contains(got, "bytes ") {
		t.Errorf("packed `bytes <n>` form leaked\n%s", got)
	}
}

// TestREPLStringOps covers the Phase 3/4 string library: `show` (render to a string),
// `runeLen` (the codepoint level vs the byte level), and byte slicing strTake/strDrop.
func TestREPLStringOps(t *testing.T) {
	script := []string{
		`show 42`,              // numeric show -> "42"
		`show true`,            // bool show -> "true"
		`strLen "é"`,           // bytes: é is 2 UTF-8 bytes
		`runeLen "é"`,          // runes: é is 1 codepoint
		`strTake 3 "hello"`,    // "hel"
		`strDrop 3 "hello"`,    // "lo"
		`"a" ++ show 7 ++ "b"`, // the interpolation desugaring, written by hand
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	for _, w := range []string{
		`"42" : Bytes`,
		`"true" : Bytes`,
		`"hel" : Bytes`,
		`"lo" : Bytes`,
		`"a7b" : Bytes`,
	} {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
	// é: 2 bytes, 1 rune.
	if !strings.Contains(got, "succ (succ zero)") && !strings.Contains(got, "2 : Whole") {
		t.Errorf("strLen é should be 2\n%s", got)
	}
}

// TestREPLInterpolation covers `{expr}` string interpolation: it desugars to a `++`
// chain over `show`, embedded expressions are parsed (including arithmetic and calls),
// and `\{` is a literal brace.
func TestREPLInterpolation(t *testing.T) {
	script := []string{
		`name : Bytes is "world" end`,
		`"hi {name}!"`,        // -> "hi world!"
		`"len {strLen name}"`, // -> "len 5"   (show of a computed Whole)
		`"sum {2 + 3}"`,       // -> "sum 5"   (an arithmetic expr inside {})
		`"{7}"`,               // -> "7"       (no surrounding literal)
		`"a \{ b"`,            // -> "a { b"   (escaped literal brace, no interpolation)
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run returned error: %v", err)
	}
	got := out.String()
	for _, w := range []string{
		`"hi world!" : Bytes`,
		`"len 5" : Bytes`,
		`"sum 5" : Bytes`,
		`"7" : Bytes`,
		`"a \{ b" : Bytes`, // a literal brace folds back ESCAPED, so it round-trips
	} {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- full output ---\n%s", w, got)
		}
	}
}

// TestREPLScribeDL loads the ch561 scribe display-list codec into a REPL
// session (the whole listing, fed line by line — a feature isn't done until it
// works in `rune repl`) and evaluates a parse in-session: the ps program
// "1 2 moveto fill" parses to two ops, and a diagnostic carries its position.
func TestREPLScribeDL(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "..", "listings", "ch561_scribe_dl.rune"))
	if err != nil {
		t.Fatal(err)
	}
	script := string(src) + "\n" +
		"llenOps (case parse \"1 2 moveto fill\" of | ok os -> os | err e -> nil Op end)\n" +
		"errIs (parse \"1 2 movetoo\") 1 5 1\n" +
		":quit\n"
	in := strings.NewReader(script)
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith returned error: %v", err)
	}
	got := out.String()
	wants := []string{
		"2 : Nat",     // two ops parsed in-session (compressed numeral display)
		"true : Bool", // the diagnostic position check
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\n--- tail of output ---\n%s", w, got[max(0, len(got)-800):])
		}
	}
}

// TestREPLScribeGeom loads the ch562 exact-geometry chapter into a REPL
// session and flattens a continuous-corner roundrect in-session.
func TestREPLScribeGeom(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "..", "listings", "ch562_scribe_geom.rune"))
	if err != nil {
		t.Fatal(err)
	}
	script := string(src) + "\nanswerN\n:quit\n"
	in := strings.NewReader(script)
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith returned error: %v", err)
	}
	if got := out.String(); !strings.Contains(got, "41 : Nat") {
		t.Errorf("output missing %q\n--- tail ---\n%s", "41 : Nat", got[max(0, len(got)-600):])
	}
}

// TestREPLScribeRaster loads the ch563 exact rasterizer into a REPL session
// and evaluates the half-coverage theorem in-session: alpha exactly 128.
func TestREPLScribeRaster(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "..", "listings", "ch563_scribe_raster.rune"))
	if err != nil {
		t.Fatal(err)
	}
	script := string(src) + "\nnthN maskHalf 9\n:quit\n"
	in := strings.NewReader(script)
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith returned error: %v", err)
	}
	if got := out.String(); !strings.Contains(got, "128 : Nat") {
		t.Errorf("output missing %q\n--- tail ---\n%s", "128 : Nat", got[max(0, len(got)-600):])
	}
}

// TestREPLScribeAccel loads the ch564 fast-path chapter into a REPL session;
// the host op is neutral in the kernel, but the encoding side evaluates:
// a snapped 1/2 encodes to bias + 128.
func TestREPLScribeAccel(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "..", "listings", "ch564_scribe_accel.rune"))
	if err != nil {
		t.Fatal(err)
	}
	script := string(src) + "\nsubEnc (qn false 1 2)\n:quit\n"
	in := strings.NewReader(script)
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith returned error: %v", err)
	}
	if got := out.String(); !strings.Contains(got, "8388736 : Nat") {
		t.Errorf("output missing %q\n--- tail ---\n%s", "8388736 : Nat", got[max(0, len(got)-600):])
	}
}

// TestREPLFloatIO is the REPL acceptance test for the float IO primitives
// (parseFloat/printFloat): a feature is not done until it works in `rune repl`.
// Host ops are permanently neutral in the normalizer; they run via :run, which
// emits JS and executes under node. The session declares the ch566-style header,
// defines main using parseFloat with the packed "3.14" constant (avoids getFloat;
// stdin feeding through the REPL runner is awkward), and asserts "6.28" appears
// in the captured output.
func TestREPLFloatIO(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	// Packed "3.14": bytes LSB-first + 0x01 sentinel.
	// '3'=51 '.'=46 '1'=49 '4'=52 -> 51 + 46*256 + 49*65536 + 52*16777216 + 1*4294967296 = 5170605619
	// (Same pattern as ch566's "abc"=23290465 verified comment.)
	script := []string{
		"data Nat : U is",
		"  zero : Nat",
		"| succ : Nat -> Nat",
		"end",
		"builtin nat Nat zero succ",
		"data Option : U -> U is",
		"  none : (A : U) -> Option A",
		"| some : (A : U) -> A -> Option A",
		"end",
		"foreign Float : U end",
		"foreign fromNat : Nat -> Float end",
		"foreign fmul : Float -> Float -> Float end",
		"foreign parseFloat : Nat -> Option Float end",
		"foreign printFloat : Float -> IO Float end",
		"foreign printNat : Nat -> IO Nat end",
		"main : IO Nat is",
		"  case parseFloat 5170605619 of",
		"  | none -> printNat 999",
		"  | some x -> bindIO Float Nat (printFloat (fmul x (fromNat 2))) (fn (_y : Float) is printNat 1 end)",
		"  end",
		"end",
		":run main",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith returned error: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "6.28") {
		t.Errorf("expected \"6.28\" in :run output (parseFloat \"3.14\" * 2 = 6.28); none-branch should not fire\n--- full output ---\n%s", got)
	}
	// The none-branch must not fire: 5170605619 is the packed "3.14" which is valid.
	if strings.Contains(got, "999") {
		t.Errorf("none-branch fired (\"999\" in output); parseFloat returned none for the packed \"3.14\" constant\n--- full output ---\n%s", got)
	}
}

// TestREPLScribeCorpus loads the ch565 corpus chapter into a REPL session and
// interprets scene2's even-odd ring in-session (mask count = 3 paint ops).
func TestREPLScribeCorpus(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "..", "listings", "ch565_scribe_corpus.rune"))
	if err != nil {
		t.Fatal(err)
	}
	script := string(src) + "\nllenPl2 (interpret scene2)\n:quit\n"
	in := strings.NewReader(script)
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith returned error: %v", err)
	}
	if got := out.String(); !strings.Contains(got, "3 : Nat") {
		t.Errorf("output missing %q\n--- tail ---\n%s", "3 : Nat", got[max(0, len(got)-600):])
	}
}
