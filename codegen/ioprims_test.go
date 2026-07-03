package codegen

import (
	"testing"
)

// TestPrimName verifies that primName extracts the last dot-segment correctly.
func TestPrimName(t *testing.T) {
	cases := []struct {
		in   string
		want string
	}{
		{"getFloat", "getFloat"},
		{"Std.Float.getFloat", "getFloat"},
		{"M.printNat", "printNat"},
		{"A.B.C.x", "x"},
		{"", ""},
	}
	for _, c := range cases {
		if got := primName(c.in); got != c.want {
			t.Errorf("primName(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

// TestUsesForeignDottedName verifies that usesForeign recognises a prim even
// when the foreign axiom was declared inside a module and carries a qualified
// name (e.g. "Std.Float.getFloat").
func TestUsesForeignDottedName(t *testing.T) {
	prog := Program{
		Defs: []DefSpec{{
			Name: "main",
			Body: IForeign{Name: "Std.Float.getFloat"},
		}},
	}
	if !usesForeign(prog, "getFloat") {
		t.Error("usesForeign should find getFloat via Std.Float.getFloat")
	}
	if usesForeign(prog, "printNat") {
		t.Error("usesForeign should not find printNat in a getFloat program")
	}
	// Unqualified name still works.
	prog2 := Program{
		Defs: []DefSpec{{
			Name: "main",
			Body: IForeign{Name: "printNat"},
		}},
	}
	if !usesForeign(prog2, "printNat") {
		t.Error("usesForeign should find unqualified printNat")
	}
}
