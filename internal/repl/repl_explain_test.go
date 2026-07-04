package repl

import (
	"bytes"
	"strings"
	"testing"
)

// TestREPLExplain drives :explain through a scripted bare session: a numbered
// result ($N and bare $), a named definition, and the error paths. This is
// the standing REPL acceptance test for the explainer.
func TestREPLExplain(t *testing.T) {
	script := []string{
		"data Nat : U is zero : Nat | succ : Nat -> Nat end",
		"succ zero",
		":explain $2",
		"idn : Nat -> Nat is fn (n : Nat) is n end end",
		":explain idn",
		":explain $",
		":explain nosuch",
		":explain $99",
		":explain",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith: %v", err)
	}
	got := out.String()
	wants := []string{
		// :explain $2 and :explain $ both render the recorded expression.
		"[Expression]",
		"[Apply `succ` to (zero) (takes Nat, gives Nat)]",
		// :explain idn renders the retained surface definition.
		"[Entrypoint: idn]",
		"[Given `n`:]",
		"[Result: (n)]",
		// Error paths keep the loop alive.
		"no definition named \"nosuch\"",
		"no result $99 in this session",
		"usage: :explain",
	}
	for _, w := range wants {
		if !strings.Contains(got, w) {
			t.Errorf("output missing %q\nfull output:\n%s", w, got)
		}
	}
}

// TestREPLExplainResetClearsHistory is the regression test for the :reset bug:
// after :reset, :explain $1 must report "no result $1 in this session" rather
// than rendering stale historyExps from the previous session. The expression
// (fn (x : U1) is x end) U is a lambda application that would produce
// "[Apply Function" output if historyExps were not cleared (proving the stale
// rendering path). After the fix, historyExps is nil and the error path fires.
func TestREPLExplainResetClearsHistory(t *testing.T) {
	script := []string{
		"(fn (x : U1) is x end) U", // expression at lineNo=1 -> $1 in historyExps
		":reset",
		":explain $1",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "no result $1 in this session") {
		t.Errorf("after :reset, :explain $1 should report missing result; got:\n%s", got)
	}
	if strings.Contains(got, "[Apply Function") {
		t.Errorf("after :reset, :explain $1 must not render stale history; got:\n%s", got)
	}
}

// TestREPLExplainCoreFlag: the depth dial reaches the REPL frontend.
func TestREPLExplainCoreFlag(t *testing.T) {
	script := []string{
		"idc : (A : U1) -> A -> A is fn (A : U1) (x : A) is x end end",
		":explain idc --depth core",
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith: %v", err)
	}
	got := out.String()
	for _, w := range []string{"[Entrypoint: idc]", "[Given `A`:]", "[Given `x`:]", "[Compute (x)]"} {
		if !strings.Contains(got, w) {
			t.Errorf("core output missing %q\nfull output:\n%s", w, got)
		}
	}
}
