package surface

import (
	"math/big"
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
