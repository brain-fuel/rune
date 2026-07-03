package surface_test

import (
	"testing"

	"goforge.dev/rune/v3/surface"
)

// TestModuleQualifiesDataAndForeign checks that a module block containing a
// datatype and a foreign axiom qualifies all declaration-level names.
// Inner body references (e.g. `on` in `lit`) are NOT rewritten by the parser;
// resolution happens in the session via the self-import mechanism.
func TestModuleQualifiesDataAndForeign(t *testing.T) {
	src := `module M is
data Flag : U is on : Flag | off : Flag end
foreign mystery : Flag end
lit : Flag is on end
end
`
	items, err := surface.ParseProgram(src)
	if err != nil {
		t.Fatal(err)
	}
	// Expect 3 items: DataDef "M.Flag", Def "M.mystery" (foreign), Def "M.lit".
	if len(items) != 3 {
		t.Fatalf("want 3 items, got %d: %v", len(items), items)
	}

	dd, ok := items[0].(surface.DataDef)
	if !ok {
		t.Fatalf("item[0] want DataDef, got %T", items[0])
	}
	if dd.Name != "M.Flag" {
		t.Errorf("DataDef name: want M.Flag, got %q", dd.Name)
	}
	if len(dd.Ctors) != 2 {
		t.Fatalf("want 2 ctors, got %d", len(dd.Ctors))
	}
	if dd.Ctors[0].Name != "M.on" {
		t.Errorf("ctor[0] name: want M.on, got %q", dd.Ctors[0].Name)
	}
	if dd.Ctors[1].Name != "M.off" {
		t.Errorf("ctor[1] name: want M.off, got %q", dd.Ctors[1].Name)
	}

	mystery, ok := items[1].(surface.Def)
	if !ok {
		t.Fatalf("item[1] want Def, got %T", items[1])
	}
	if mystery.Name != "M.mystery" {
		t.Errorf("foreign name: want M.mystery, got %q", mystery.Name)
	}
	if !mystery.IsForeign {
		t.Error("mystery should be marked IsForeign")
	}

	lit, ok := items[2].(surface.Def)
	if !ok {
		t.Fatalf("item[2] want Def, got %T", items[2])
	}
	if lit.Name != "M.lit" {
		t.Errorf("def name: want M.lit, got %q", lit.Name)
	}
}

// TestModuleQualifiesDefGroup checks that a module containing a partial def
// and a datatype qualifies the definition names.
func TestModuleQualifiesDefGroup(t *testing.T) {
	src := `module M is
data Nat : U is zero : Nat | succ : Nat -> Nat end
partial even : Nat -> Nat is fn (n : Nat) is zero end end
end
`
	items, err := surface.ParseProgram(src)
	if err != nil {
		t.Fatal(err)
	}
	// 2 items: DataDef M.Nat, Def M.even
	if len(items) != 2 {
		t.Fatalf("want 2 items, got %d", len(items))
	}
	dd, ok := items[0].(surface.DataDef)
	if !ok {
		t.Fatalf("item[0]: want DataDef, got %T", items[0])
	}
	if dd.Name != "M.Nat" {
		t.Errorf("data name: want M.Nat, got %q", dd.Name)
	}
	def, ok := items[1].(surface.Def)
	if !ok {
		t.Fatalf("item[1]: want Def, got %T", items[1])
	}
	if def.Name != "M.even" {
		t.Errorf("def name: want M.even, got %q", def.Name)
	}
}

// TestModuleBuiltinNatQualifiesRefs checks that a `builtin nat` declaration
// inside a module block has its type/zero/succ references qualified to refer to
// the module-local names (the qualLocal rule in parseModule). After parsing,
// BuiltinNat.TyName/Zero/Succ must all carry the module prefix.
func TestModuleBuiltinNatQualifiesRefs(t *testing.T) {
	src := `module M is
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
end
`
	items, err := surface.ParseProgram(src)
	if err != nil {
		t.Fatal(err)
	}
	// Expect 2 items: DataDef "M.Nat" and BuiltinNat with qualified fields.
	if len(items) != 2 {
		t.Fatalf("want 2 items, got %d: %v", len(items), items)
	}
	dd, ok := items[0].(surface.DataDef)
	if !ok {
		t.Fatalf("item[0]: want DataDef, got %T", items[0])
	}
	if dd.Name != "M.Nat" {
		t.Errorf("DataDef name: want M.Nat, got %q", dd.Name)
	}
	bn, ok := items[1].(surface.BuiltinNat)
	if !ok {
		t.Fatalf("item[1]: want BuiltinNat, got %T", items[1])
	}
	if bn.TyName != "M.Nat" {
		t.Errorf("BuiltinNat.TyName: want M.Nat, got %q", bn.TyName)
	}
	if bn.Zero != "M.zero" {
		t.Errorf("BuiltinNat.Zero: want M.zero, got %q", bn.Zero)
	}
	if bn.Succ != "M.succ" {
		t.Errorf("BuiltinNat.Succ: want M.succ, got %q", bn.Succ)
	}
}
