package surface

import "testing"

func TestParsePostulate(t *testing.T) {
	items, err := ParseProgram(`postulate inRegion : U because "cloud API not yet modeled" end`)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if len(items) != 1 {
		t.Fatalf("want 1 item, got %d", len(items))
	}
	d, ok := items[0].(Def)
	if !ok {
		t.Fatalf("want a Def, got %T", items[0])
	}
	if d.Name != "inRegion" || !d.IsPostulate || !d.IsForeign {
		t.Fatalf("postulate flags wrong: %+v", d)
	}
	if d.Why != "cloud API not yet modeled" {
		t.Fatalf("reason wrong: %q", d.Why)
	}
}
