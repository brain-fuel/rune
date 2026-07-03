package surface

import (
	"math/big"
	"strings"
	"testing"
)

func TestWalkExpCollectsVars(t *testing.T) {
	// fn (x : A) is x end; ELam with Dom=EVar{A}, Body=EVar{x}
	e := ELam{
		Param: "x",
		Dom:   EVar{Name: "A"},
		Body:  EVar{Name: "x"},
	}
	var names []string
	WalkExp(e, func(n Exp) {
		if v, ok := n.(EVar); ok {
			names = append(names, v.Name)
		}
	})
	found := map[string]bool{}
	for _, n := range names {
		found[n] = true
	}
	if !found["A"] || !found["x"] {
		t.Errorf("expected A and x, got %v", names)
	}
}

func TestWalkExpCountsNodes(t *testing.T) {
	// EApp{EVar{"f"}, EVar{"x"}} => 3 nodes: EApp, EVar{f}, EVar{x}
	e := EApp{Fn: EVar{Name: "f"}, Arg: EVar{Name: "x"}}
	count := 0
	WalkExp(e, func(_ Exp) { count++ })
	if count != 3 {
		t.Errorf("expected 3 nodes, got %d", count)
	}
}

func TestWalkExpCaseClauseBodies(t *testing.T) {
	// case s of | c x -> x end; clause body EVar{x} must be visited
	e := ECase{
		Scrut: EVar{Name: "s"},
		Clauses: []CaseClause{
			{Ctor: "c", Binders: []string{"x"}, Body: EVar{Name: "x"}},
		},
	}
	var names []string
	WalkExp(e, func(n Exp) {
		if v, ok := n.(EVar); ok {
			names = append(names, v.Name)
		}
	})
	found := map[string]bool{}
	for _, n := range names {
		found[n] = true
	}
	if !found["s"] || !found["x"] {
		t.Errorf("expected s and x, got %v", names)
	}
}

func TestWalkExpLeaves(t *testing.T) {
	// Each leaf type should not panic.
	leaves := []Exp{
		EVar{Name: "v"},
		EUniv{Lvl: 0},
		EHole{},
		EProp{},
		EEq{},
		ERefl{},
		ECast{},
		ESubst{},
		ESig{},
		EPair{},
		ENum{Val: big.NewInt(42), Pos: 0},
	}
	for _, leaf := range leaves {
		count := 0
		WalkExp(leaf, func(_ Exp) { count++ })
		if count != 1 {
			t.Errorf("leaf %T: expected 1 visit, got %d", leaf, count)
		}
	}
}

// TestMapExpNamesRenamesVars verifies that MapExpNames rewrites EVar names.
func TestMapExpNamesRenamesVars(t *testing.T) {
	ren := func(n string) string {
		if n == "x" {
			return "M.x"
		}
		return n
	}
	e := ELam{Param: "p", Dom: EVar{Name: "x"}, Body: EVar{Name: "x"}}
	got := MapExpNames(e, ren).(ELam)
	if got.Dom.(EVar).Name != "M.x" {
		t.Errorf("Dom not rewritten: %v", got.Dom)
	}
	if got.Body.(EVar).Name != "M.x" {
		t.Errorf("Body not rewritten: %v", got.Body)
	}
	if got.Param != "p" {
		t.Errorf("Param must not be rewritten, got %q", got.Param)
	}
}

// TestMapExpNamesRenamesCaseCtors verifies that MapExpNames rewrites ECase
// clause constructor heads as well as the scrutinee and clause bodies.
func TestMapExpNamesRenamesCaseCtors(t *testing.T) {
	ren := func(n string) string { return "M." + n }
	e := ECase{
		Scrut: EVar{Name: "s"},
		Clauses: []CaseClause{
			{Ctor: "c", Binders: []string{"x"}, Body: EVar{Name: "x"}},
		},
	}
	got := MapExpNames(e, ren).(ECase)
	if got.Scrut.(EVar).Name != "M.s" {
		t.Errorf("Scrut not rewritten: %v", got.Scrut)
	}
	if got.Clauses[0].Ctor != "M.c" {
		t.Errorf("Ctor not rewritten: %v", got.Clauses[0].Ctor)
	}
	if got.Clauses[0].Body.(EVar).Name != "M.x" {
		t.Errorf("Clause body not rewritten: %v", got.Clauses[0].Body)
	}
	if !strings.EqualFold(got.Clauses[0].Binders[0], "x") {
		t.Errorf("Binders must not be rewritten, got %v", got.Clauses[0].Binders)
	}
}

// TestMapExpNamesLeavesStructure verifies that non-name structural fields
// (Icit, Qty, binder names) are preserved unchanged.
func TestMapExpNamesLeavesStructure(t *testing.T) {
	ren := func(n string) string { return "R." + n }
	orig := EApp{Fn: EVar{Name: "f"}, Arg: EVar{Name: "a"}}
	got := MapExpNames(orig, ren).(EApp)
	if got.Fn.(EVar).Name != "R.f" || got.Arg.(EVar).Name != "R.a" {
		t.Errorf("MapExpNames EApp: got fn=%v arg=%v", got.Fn, got.Arg)
	}
}

// TestMapExpNamesLeafsDontPanic verifies that every leaf Exp type is handled
// without panic (the exhaustive-with-panic discipline).
func TestMapExpNamesLeafsDontPanic(t *testing.T) {
	id := func(n string) string { return n }
	leaves := []Exp{
		EUniv{Lvl: 0},
		EHole{},
		EProp{},
		EEq{},
		ERefl{},
		ECast{},
		ESubst{},
		ESig{},
		EPair{},
		ENum{Val: big.NewInt(1), Pos: 0},
	}
	for _, leaf := range leaves {
		_ = MapExpNames(leaf, id) // must not panic
	}
}
