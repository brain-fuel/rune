package ledger

import (
	"strings"
	"testing"
)

func TestRenderText(t *testing.T) {
	src := "foreign hostThing : (A : U) -> A -> A end\n" +
		"idU : (A : U) -> A -> A is fn (A : U) (x : A) is x end end"
	s := mustSession(t, src)
	var b strings.Builder
	RenderText(Build(s), &b)
	out := b.String()
	if !strings.Contains(out, "assume") || !strings.Contains(out, "hostThing") {
		t.Fatalf("assume row missing:\n%s", out)
	}
	if !strings.Contains(out, "proven") || !strings.Contains(out, "idU") {
		t.Fatalf("proven row missing:\n%s", out)
	}
}
