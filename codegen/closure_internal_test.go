package codegen

import (
	"reflect"
	"testing"
)

// closure_internal_test.go — white-box tests for the closure-conversion pass
// internals (free-variable analysis on de Bruijn terms). The semantics-
// preservation and closed-block tests live in the external closure_test.go
// (package codegen_test), which needs internal/session and so cannot be in
// package codegen (import cycle).

func TestFreeVarsLeaf(t *testing.T) {
	cases := []struct {
		name string
		term Ir
		want []int
	}{
		{"var0", IVar{0}, []int{0}},
		{"var3", IVar{3}, []int{3}},
		{"global", IGlobal{"f"}, nil},
		{"unit", IUnit{}, nil},
		// λ. 0  — the bound var is not free.
		{"id", ILam{Body: IVar{0}}, nil},
		// λ. 1  — captures the enclosing index 0.
		{"const-capture", ILam{Body: IVar{1}}, []int{0}},
		// λ. (0 1) — apply param to a captured var.
		{"app", ILam{Body: IApp{Fn: IVar{0}, Arg: IVar{1}}}, []int{0}},
		// (0 2) at top — both free.
		{"open-app", IApp{Fn: IVar{0}, Arg: IVar{2}}, []int{0, 2}},
		// let x = 1 in (0 (1 3)): val frame sees 1; body adds a binder so 0 is
		// local, 1->0, 3->2 free. Net free at top: {1} ∪ {0,2} = {0,1,2}.
		{"let", ILet{Val: IVar{1}, Body: IApp{Fn: IVar{0}, Arg: IApp{Fn: IVar{1}, Arg: IVar{3}}}}, []int{0, 1, 2}},
		// nested λλ. (0 (1 2)): inner param 0, outer param 1, capture index 0.
		{"nested", ILam{Body: ILam{Body: IApp{Fn: IVar{0}, Arg: IApp{Fn: IVar{1}, Arg: IVar{2}}}}}, []int{0}},
		// case 0 of arm-> 1: scrutinee 0 free, arm body 1 free.
		{"case", ICase{Scrut: IVar{0}, Arms: []ICaseArm{{Tag: 0, Body: IVar{1}}}}, []int{0, 1}},
		// field projection passes through.
		{"field", IField{Scrut: IVar{2}, Index: 1}, []int{2}},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			got := freeVars(c.term)
			if !reflect.DeepEqual(normEmpty(got), normEmpty(c.want)) {
				t.Fatalf("freeVars = %v, want %v", got, c.want)
			}
		})
	}
}

func normEmpty(s []int) []int {
	if len(s) == 0 {
		return nil
	}
	return s
}

// TestLiftIdentityIsClosed converts λ.0 (identity) and checks the lifted code
// block captures nothing — the simplest closed-block case.
func TestLiftIdentityIsClosed(t *testing.T) {
	p := Program{Defs: []DefSpec{{Name: "id", Body: ILam{Name: "x", Body: IVar{0}}}}}
	cp := ClosureConvert(p)
	if len(cp.Blocks) != 1 {
		t.Fatalf("want 1 block, got %d", len(cp.Blocks))
	}
	if len(cp.Blocks[0].Captures) != 0 {
		t.Fatalf("identity should capture nothing, captured %v", cp.Blocks[0].Captures)
	}
	mk, ok := cp.Defs[0].Body.(MkClosure)
	if !ok {
		t.Fatalf("id body should be a MkClosure, got %T", cp.Defs[0].Body)
	}
	if len(mk.Env) != 0 {
		t.Fatalf("identity closure env should be empty, got %v", mk.Env)
	}
}

// TestLiftCaptureRecordsFreeVar converts λx.λy.x (const) and checks the inner
// lambda captures exactly the outer parameter into one env slot.
func TestLiftCaptureRecordsFreeVar(t *testing.T) {
	// const = λx. λy. x   (inner body IVar{1} references the outer x)
	body := ILam{Name: "x", Body: ILam{Name: "y", Body: IVar{1}}}
	cp := ClosureConvert(Program{Defs: []DefSpec{{Name: "const", Body: body}}})
	// Two code blocks: outer (captures nothing) and inner (captures x).
	if len(cp.Blocks) != 2 {
		t.Fatalf("want 2 blocks, got %d", len(cp.Blocks))
	}
	var inner *CodeBlock
	for i := range cp.Blocks {
		if len(cp.Blocks[i].Captures) == 1 {
			inner = &cp.Blocks[i]
		}
	}
	if inner == nil {
		t.Fatalf("expected one block capturing the outer param; blocks=%+v", cp.Blocks)
	}
	// The inner block body must read the captured var from the env (CEnv), not a
	// CVar — the whole point of conversion.
	if _, ok := inner.Body.(CEnv); !ok {
		t.Fatalf("inner const body should be CEnv (the captured x), got %T", inner.Body)
	}
}
