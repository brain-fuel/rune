package surface_test

import (
	"testing"

	"goforge.dev/rune/v3/surface"
)

// headIdentDo walks a surface.Exp down EApp.Fn to the leftmost EVar
// and returns its Name. Returns "" if the root is not an EVar.
func headIdentDo(e surface.Exp) string {
	for {
		switch v := e.(type) {
		case surface.EApp:
			e = v.Fn
		case surface.EVar:
			return v.Name
		default:
			return ""
		}
	}
}

// TestParseDoDesugarsToParTwoItems checks that `do a b end` desugars to
// a right-nested `par` application whose head identifier is "par".
func TestParseDoDesugarsToParTwoItems(t *testing.T) {
	e, err := surface.ParseExpr("do printHello printWorld end")
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if got := headIdentDo(e); got != "par" {
		t.Fatalf("do did not desugar to a par application; head = %q", got)
	}
}

// TestParseDoSingleItem checks that `do a end` desugars to `a` (no par wrapping).
func TestParseDoSingleItem(t *testing.T) {
	e, err := surface.ParseExpr("do onlyAction end")
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if got := headIdentDo(e); got != "onlyAction" {
		t.Fatalf("do a end should desugar to a; head = %q, want %q", got, "onlyAction")
	}
}

// TestDoUsableAsIdentifier checks that `do` is a contextual keyword: it can
// still appear as an ordinary variable name when not in the block position.
func TestDoUsableAsIdentifier(t *testing.T) {
	// `do` used as a plain identifier (variable) in function application position.
	// If `do` were a hard reserved word, this would be a parse error.
	e, err := surface.ParseExpr("f do")
	if err != nil {
		t.Fatalf("`do` as identifier caused parse error: %v", err)
	}
	app, ok := e.(surface.EApp)
	if !ok {
		t.Fatalf("expected EApp, got %T", e)
	}
	arg, ok := app.Arg.(surface.EVar)
	if !ok {
		t.Fatalf("expected EVar for arg, got %T", app.Arg)
	}
	if arg.Name != "do" {
		t.Fatalf("expected arg Name %q, got %q", "do", arg.Name)
	}
}
