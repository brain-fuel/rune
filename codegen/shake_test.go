package codegen

import "testing"

// TestShakeDropsUnreachable verifies that definitions unreachable from Main
// are pruned and reachable ones are kept, in original order.
func TestShakeDropsUnreachable(t *testing.T) {
	p := Program{
		Main: "main",
		Defs: []DefSpec{
			{Name: "main", Body: IGlobal{Name: "helper"}},
			{Name: "helper", Body: IUnit{}},
			{Name: "unused", Body: IGlobal{Name: "alsoUnused"}},
			{Name: "alsoUnused", Body: IUnit{}},
		},
	}
	q := Shake(p)
	names := map[string]bool{}
	for _, d := range q.Defs {
		names[d.Name] = true
	}
	if !names["main"] || !names["helper"] {
		t.Fatalf("reachable defs dropped: %v", names)
	}
	if names["unused"] || names["alsoUnused"] {
		t.Fatalf("unreachable defs kept: %v", names)
	}
}

// TestShakeNoMainIsIdentity verifies that a program with no Main is returned
// unchanged -- the REPL/emit-all paths must not be affected.
func TestShakeNoMainIsIdentity(t *testing.T) {
	p := Program{Defs: []DefSpec{{Name: "a", Body: IUnit{}}}}
	if got := Shake(p); len(got.Defs) != 1 {
		t.Fatal("no-Main shake must keep everything")
	}
}

// TestShakeKeepsTransitiveDeps verifies multi-hop reachability: a chain
// main -> f -> g -> h all survive, while an unreachable branch is pruned.
func TestShakeKeepsTransitiveDeps(t *testing.T) {
	p := Program{
		Main: "main",
		Defs: []DefSpec{
			{Name: "main", Body: IGlobal{Name: "f"}},
			{Name: "f", Body: IGlobal{Name: "g"}},
			{Name: "g", Body: IGlobal{Name: "h"}},
			{Name: "h", Body: IUnit{}},
			{Name: "dead", Body: IUnit{}},
		},
	}
	q := Shake(p)
	have := map[string]bool{}
	for _, d := range q.Defs {
		have[d.Name] = true
	}
	for _, want := range []string{"main", "f", "g", "h"} {
		if !have[want] {
			t.Errorf("reachable def %q was dropped", want)
		}
	}
	if have["dead"] {
		t.Error("unreachable def 'dead' was kept")
	}
}

// TestShakePreservesOrder verifies that surviving defs are kept in their
// original declaration order.
func TestShakePreservesOrder(t *testing.T) {
	p := Program{
		Main: "main",
		Defs: []DefSpec{
			{Name: "a", Body: IUnit{}},
			{Name: "b", Body: IGlobal{Name: "a"}},
			{Name: "main", Body: IApp{Fn: IGlobal{Name: "b"}, Arg: IGlobal{Name: "a"}}},
			{Name: "z", Body: IUnit{}}, // unreachable
		},
	}
	q := Shake(p)
	if len(q.Defs) != 3 {
		t.Fatalf("expected 3 defs, got %d: %v", len(q.Defs), q.Defs)
	}
	if q.Defs[0].Name != "a" || q.Defs[1].Name != "b" || q.Defs[2].Name != "main" {
		t.Errorf("wrong order: %v", []string{q.Defs[0].Name, q.Defs[1].Name, q.Defs[2].Name})
	}
}

// TestShakeDatasUntouched verifies that Datas is always kept intact regardless
// of which defs are reachable (constructor tags are positional).
func TestShakeDatasUntouched(t *testing.T) {
	ds := []DataSpec{
		{ElimName: "BoolElim", Ctors: []CtorSpec{{Name: "false", Tag: 0}, {Name: "true", Tag: 1}}},
	}
	p := Program{
		Main:  "main",
		Datas: ds,
		Defs: []DefSpec{
			{Name: "main", Body: IUnit{}},
			{Name: "unused", Body: IGlobal{Name: "BoolElim"}},
		},
	}
	q := Shake(p)
	if len(q.Datas) != 1 {
		t.Fatalf("Datas was modified; want 1, got %d", len(q.Datas))
	}
	if q.Datas[0].ElimName != "BoolElim" {
		t.Errorf("DataSpec was replaced: %v", q.Datas[0])
	}
}

// TestShakeElimReachableViaDef verifies that a def referencing an eliminator
// via IGlobal is kept, and Datas is still intact (the eliminator is emitted
// from Datas at runtime, not from Defs).
func TestShakeElimReachableViaDef(t *testing.T) {
	p := Program{
		Main: "main",
		Datas: []DataSpec{
			{ElimName: "BoolElim", Ctors: []CtorSpec{{Name: "false", Tag: 0}, {Name: "true", Tag: 1}}},
		},
		Defs: []DefSpec{
			// main references the eliminator, which is in Datas not Defs
			{Name: "main", Body: IGlobal{Name: "BoolElim"}},
			{Name: "unreachable", Body: IUnit{}},
		},
	}
	q := Shake(p)
	// BoolElim is not in Defs so it cannot appear there
	for _, d := range q.Defs {
		if d.Name == "unreachable" {
			t.Error("unreachable def survived")
		}
	}
	// Datas must remain intact
	if len(q.Datas) != 1 || q.Datas[0].ElimName != "BoolElim" {
		t.Errorf("Datas was modified: %v", q.Datas)
	}
}

// TestShakePrunesPartials verifies that Partials is pruned to only surviving
// def names.
func TestShakePrunesPartials(t *testing.T) {
	p := Program{
		Main: "main",
		Defs: []DefSpec{
			{Name: "main", Body: IGlobal{Name: "loop"}},
			{Name: "loop", Body: IBounce{Call: IGlobal{Name: "loop"}}},
			{Name: "dead", Body: IBounce{Call: IGlobal{Name: "dead"}}},
		},
		Partials: map[string]bool{"loop": true, "dead": true},
	}
	q := Shake(p)
	if !q.Partials["loop"] {
		t.Error("reachable partial 'loop' was pruned")
	}
	if q.Partials["dead"] {
		t.Error("unreachable partial 'dead' was kept")
	}
}

// TestShakeMutualGroupKeptWhole verifies that discovering any member of a
// mutual-partial group makes ALL group members reachable.
func TestShakeMutualGroupKeptWhole(t *testing.T) {
	p := Program{
		Main: "main",
		Defs: []DefSpec{
			{Name: "main", Body: IGlobal{Name: "even"}},
			{Name: "even", Body: IBounce{Call: IGlobal{Name: "odd"}}},
			{Name: "odd", Body: IBounce{Call: IGlobal{Name: "even"}}},
			{Name: "unused", Body: IUnit{}},
		},
		Partials:      map[string]bool{"even": true, "odd": true},
		PartialGroups: [][]string{{"even", "odd"}},
	}
	q := Shake(p)
	have := map[string]bool{}
	for _, d := range q.Defs {
		have[d.Name] = true
	}
	if !have["even"] || !have["odd"] {
		t.Errorf("mutual group member dropped: %v", have)
	}
	if have["unused"] {
		t.Error("unreachable def 'unused' survived")
	}
	if len(q.PartialGroups) != 1 {
		t.Errorf("PartialGroups not preserved: %v", q.PartialGroups)
	}
}
