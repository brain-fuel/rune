package codegen

import (
	"strings"
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
	// Dotted printNat is also found by usesForeign.
	prog3 := Program{
		Defs: []DefSpec{{
			Name: "main",
			Body: IForeign{Name: "Std.Demo.printNat"},
		}},
	}
	if !usesForeign(prog3, "printNat") {
		t.Error("usesForeign should find printNat via Std.Demo.printNat")
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

// TestPrimCollisionGuard verifies that CheckPrimCollisions returns an error when
// two distinct qualified foreignnames share a last segment that is a known ioPrim.
// A.printNat and B.printNat both resolve to prim segment "printNat"; having both
// in the same program would cause one host body to silently gate for the other.
func TestPrimCollisionGuard(t *testing.T) {
	// Two distinct foreigns whose last segment is the same known prim.
	collidingProg := Program{
		Defs: []DefSpec{
			{Name: "f1", Body: IForeign{Name: "A.printNat"}},
			{Name: "f2", Body: IForeign{Name: "B.printNat"}},
		},
	}
	if err := CheckPrimCollisions(collidingProg); err == nil {
		t.Error("CheckPrimCollisions should report a collision between A.printNat and B.printNat")
	} else if !strings.Contains(err.Error(), "printNat") {
		t.Errorf("collision error should name the colliding segment, got: %v", err)
	}

	// Same segment but same full name -- no collision.
	sameProg := Program{
		Defs: []DefSpec{
			{Name: "f1", Body: IForeign{Name: "Std.Demo.printNat"}},
			{Name: "f2", Body: IForeign{Name: "Std.Demo.printNat"}},
		},
	}
	if err := CheckPrimCollisions(sameProg); err != nil {
		t.Errorf("CheckPrimCollisions should not flag same qualified name twice: %v", err)
	}

	// Collision-free: distinct segments, both known prims.
	distinctProg := Program{
		Defs: []DefSpec{
			{Name: "f1", Body: IForeign{Name: "printNat"}},
			{Name: "f2", Body: IForeign{Name: "getNat"}},
		},
	}
	if err := CheckPrimCollisions(distinctProg); err != nil {
		t.Errorf("CheckPrimCollisions should not flag distinct prim segments: %v", err)
	}

	// Unknown segment (not in ioPrims) -- no error even if names collide.
	unknownProg := Program{
		Defs: []DefSpec{
			{Name: "f1", Body: IForeign{Name: "A.myOp"}},
			{Name: "f2", Body: IForeign{Name: "B.myOp"}},
		},
	}
	if err := CheckPrimCollisions(unknownProg); err != nil {
		t.Errorf("CheckPrimCollisions should not flag unknown segments: %v", err)
	}
}

// TestCheckPrimCollisionsInCaseArm verifies that CheckPrimCollisions walks into
// ICase arm bodies and detects a prim collision nested inside a case expression.
// This covers the ICase/IField/IBounce walker paths added to the walk function.
func TestCheckPrimCollisionsInCaseArm(t *testing.T) {
	// Build a program where a colliding IForeign appears inside an ICase arm body.
	// The scrutinee carries one qualified printNat and the arm body carries another
	// distinct qualified printNat -- a collision that the walker must find.
	collidingInCase := Program{
		Defs: []DefSpec{
			{
				Name: "f",
				Body: ICase{
					Scrut: IForeign{Name: "A.printNat"},
					Arms: []ICaseArm{
						{Tag: 0, Body: IForeign{Name: "B.printNat"}},
					},
				},
			},
		},
	}
	if err := CheckPrimCollisions(collidingInCase); err == nil {
		t.Error("CheckPrimCollisions should detect collision of A.printNat/B.printNat inside ICase")
	} else if !strings.Contains(err.Error(), "printNat") {
		t.Errorf("collision error should name the prim segment, got: %v", err)
	}

	// A single qualified printNat nested in an ICase is fine (no collision).
	singleInCase := Program{
		Defs: []DefSpec{
			{
				Name: "g",
				Body: ICase{
					Scrut: IForeign{Name: "Std.Float.printFloat"},
					Arms: []ICaseArm{
						{Tag: 0, Body: IForeign{Name: "Std.IO.printNat"}},
					},
				},
			},
		},
	}
	if err := CheckPrimCollisions(singleInCase); err != nil {
		t.Errorf("CheckPrimCollisions should not flag distinct prims in ICase: %v", err)
	}

	// IField and IBounce: a collision inside an IBounce-wrapped call is also caught.
	collidingInBounce := Program{
		Defs: []DefSpec{
			{Name: "h1", Body: IForeign{Name: "A.printNat"}},
			{Name: "h2", Body: IBounce{Call: IForeign{Name: "B.printNat"}}},
		},
	}
	if err := CheckPrimCollisions(collidingInBounce); err == nil {
		t.Error("CheckPrimCollisions should detect collision inside IBounce")
	}
}
