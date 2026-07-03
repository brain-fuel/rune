package surface

import "testing"

func TestParseImport(t *testing.T) {
	items, err := ParseProgram("import Std.Float\n")
	if err != nil {
		t.Fatal(err)
	}
	imp, ok := items[0].(Import)
	if !ok || imp.Module != "Std.Float" {
		t.Fatalf("got %#v", items[0])
	}
}

func TestParseAlias(t *testing.T) {
	items, err := ParseProgram("alias Math.Geometry\nalias Math.Geometry as G\n")
	if err != nil {
		t.Fatal(err)
	}
	a := items[0].(Alias)
	if a.Module != "Math.Geometry" || a.As != "Geometry" {
		t.Fatalf("bare alias: %#v", a)
	}
	b := items[1].(Alias)
	if b.Module != "Math.Geometry" || b.As != "G" {
		t.Fatalf("as-alias: %#v", b)
	}
}

// Contextual: `import` used as an ordinary definition name keeps parsing.
func TestImportStaysContextual(t *testing.T) {
	src := "import : Nat -> Nat is fn (n : Nat) is n end end\n"
	items, err := ParseProgram(src)
	if err != nil {
		t.Fatalf("contextual fallthrough broke: %v", err)
	}
	d, ok := items[0].(Def)
	if !ok || d.Name != "import" {
		t.Fatalf("got %#v", items[0])
	}
}

// Contextual: `alias` used as an ordinary definition name keeps parsing.
func TestAliasStaysContextual(t *testing.T) {
	src := "alias : Nat -> Nat is fn (n : Nat) is n end end\n"
	items, err := ParseProgram(src)
	if err != nil {
		t.Fatalf("contextual fallthrough broke: %v", err)
	}
	d, ok := items[0].(Def)
	if !ok || d.Name != "alias" {
		t.Fatalf("got %#v", items[0])
	}
}
