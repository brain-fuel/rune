package explain

import (
	"strings"
	"testing"
)

const depthSrc = `data Nat : U is zero : Nat | succ : Nat -> Nat end
foreign printNat : Nat -> IO Nat end
inner : IO Nat is printNat zero end
outer : IO Nat is bindIO Nat Nat inner (fn (r : Nat) is inner end) end
top : IO Nat is outer end
`

// TestExplainDepth0Calls: at depth 0 a user call is one line, no kids.
func TestExplainDepth0Calls(t *testing.T) {
	s := load(t, depthSrc)
	want := "[Entrypoint: outer]\n" +
		"[Apply `inner` (gives IO Nat) as `r`]\n" +
		"[Apply `inner` (gives IO Nat)]\n"
	if got := explainText(t, s, "outer", Options{}); got != want {
		t.Errorf("depth 0:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainDepth1Inlines: depth 1 inlines the callee body one level.
func TestExplainDepth1Inlines(t *testing.T) {
	s := load(t, depthSrc)
	want := "[Entrypoint: outer]\n" +
		"[Apply `inner` (gives IO Nat) as `r`]\n" +
		"  [Print (zero) to Command Line]\n" +
		"[Apply `inner` (gives IO Nat)]\n" +
		"  [Print (zero) to Command Line]\n"
	if got := explainText(t, s, "outer", Options{Depth: 1}); got != want {
		t.Errorf("depth 1:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainDepthBudget: depth 1 from `top` inlines outer but NOT inner
// (budget exhausted); depth 2 reaches both levels.
func TestExplainDepthBudget(t *testing.T) {
	s := load(t, depthSrc)
	want1 := "[Entrypoint: top]\n" +
		"[Apply `outer` (gives IO Nat)]\n" +
		"  [Apply `inner` (gives IO Nat) as `r`]\n" +
		"  [Apply `inner` (gives IO Nat)]\n"
	if got := explainText(t, s, "top", Options{Depth: 1}); got != want1 {
		t.Errorf("depth 1 from top:\ngot:\n%swant:\n%s", got, want1)
	}
	want2 := "[Entrypoint: top]\n" +
		"[Apply `outer` (gives IO Nat)]\n" +
		"  [Apply `inner` (gives IO Nat) as `r`]\n" +
		"    [Print (zero) to Command Line]\n" +
		"  [Apply `inner` (gives IO Nat)]\n" +
		"    [Print (zero) to Command Line]\n"
	if got := explainText(t, s, "top", Options{Depth: 2}); got != want2 {
		t.Errorf("depth 2 from top:\ngot:\n%swant:\n%s", got, want2)
	}
}

// TestExplainAcceleratedStaysOneLine: a builtin-accelerated def never inlines
// (its eliminator body is a fuel loop, not the meaning).
func TestExplainAcceleratedStaysOneLine(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
addN : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a end
end
builtin natAdd addN
useAdd : Nat -> Nat is fn (n : Nat) is addN n n end end
`
	s := load(t, src)
	got := explainText(t, s, "useAdd", Options{Depth: 5})
	want := "[Entrypoint: useAdd]\n" +
		"[Given `n`:]\n" +
		"  [Apply `addN` to (n) and (n) (takes Nat and Nat, gives Nat)]\n"
	if got != want {
		t.Errorf("accelerated:\ngot:\n%swant:\n%s", got, want)
	}
	if strings.Contains(got, "NatElim") {
		t.Error("accelerated def body leaked into the explanation")
	}
}
