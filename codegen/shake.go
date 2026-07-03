package codegen

// Shake prunes the program to only the definitions reachable from p.Main,
// walking IGlobal references in DefSpec bodies. If p.Main is empty the program
// is returned unchanged -- REPL and emit-all paths are unaffected.
//
// Datas is kept intact: constructor tags are assigned by declaration order and
// backends emit a case-dispatch table over ALL constructors in a DataSpec.
// Pruning individual DataSpec entries would corrupt tag numbering for every
// program that uses those constructors. Nat is kept intact for the same reason
// (zero/succ/elim are interdependent NatSpec fields).
//
// Eliminators are NOT in Defs -- they are generated at emit time from Datas via
// LowerElim. A def body that uses a case expression carries an
// IGlobal{Name: "FooElim"} reference; Shake marks that name reachable but
// finds it absent from Defs (it comes from Datas) and so moves on without error.
// Because Datas is kept intact the eliminator is always emitted.
//
// Partials and PartialGroups are pruned to the surviving def names so the
// trampoline driver is not emitted for dead partial defs. CAUTION: if any
// member of a mutual-recursion group is reachable, every other member of that
// group is added to the worklist -- the group's shared driver may call any
// member, and an orphaned sibling would be undefined at runtime.
func Shake(p Program) Program {
	if p.Main == "" {
		return p
	}

	// Build a lookup table from def name to DefSpec for fast worklist access.
	defByName := make(map[string]DefSpec, len(p.Defs))
	for _, d := range p.Defs {
		defByName[d.Name] = d
	}

	// Build a map from partial-group member name to its full group slice, so
	// discovering any member immediately enqueues all its siblings.
	groupOf := make(map[string][]string)
	for _, grp := range p.PartialGroups {
		for _, name := range grp {
			groupOf[name] = grp
		}
	}

	// Reachability: worklist-driven BFS starting from p.Main.
	reachable := make(map[string]bool, len(p.Defs))
	worklist := make([]string, 0, len(p.Defs))

	reach := func(name string) {
		if !reachable[name] {
			reachable[name] = true
			worklist = append(worklist, name)
		}
	}
	reach(p.Main)

	// walkIr visits every node in an Ir tree and calls reach for every IGlobal
	// encountered. The type switch is EXHAUSTIVE: every Ir node defined in ir.go
	// must appear here. An unknown node type causes an immediate panic so that a
	// future addition to the Ir type set fails loudly in tests rather than
	// silently dropping reachability edges.
	var walkIr func(Ir)
	walkIr = func(ir Ir) {
		switch x := ir.(type) {
		case IUnit:
			// leaf -- no nested Ir, no global references
		case IVar:
			// leaf -- bound variable, no global references
		case ILit:
			// leaf -- native host literal, no global references
		case IGlobal:
			reach(x.Name)
		case IForeign:
			// host-provided axiom -- no body in Defs; foreign resolution is done
			// at emit time by the backend runtime (e.g. `const printFloat = ...`
			// is gated by usesForeign, which walks the surviving Defs). We do
			// NOT add the foreign name to the reachable-def set.
		case ILam:
			walkIr(x.Body)
		case IApp:
			walkIr(x.Fn)
			walkIr(x.Arg)
		case ILet:
			walkIr(x.Val)
			walkIr(x.Body)
		case IPair:
			walkIr(x.A)
			walkIr(x.B)
		case IFst:
			walkIr(x.P)
		case ISnd:
			walkIr(x.P)
		case IField:
			walkIr(x.Scrut)
		case ICase:
			walkIr(x.Scrut)
			for _, arm := range x.Arms {
				walkIr(arm.Body)
			}
		case IBounce:
			walkIr(x.Call)
		default:
			// A future Ir node type was added without updating this switch.
			// Panic loudly so the test suite catches the omission immediately.
			panic("codegen.Shake: unknown Ir node type; update walkIr in shake.go")
		}
	}

	for len(worklist) > 0 {
		name := worklist[len(worklist)-1]
		worklist = worklist[:len(worklist)-1]

		d, ok := defByName[name]
		if !ok {
			// name is either a DataSpec eliminator/constructor name (present in
			// Datas, not Defs) or a foreign/builtin reference. All of these are
			// kept by other means; no body to walk.
			continue
		}
		walkIr(d.Body)

		// Mutual-partial groups: discovering any member makes all siblings
		// reachable, because the shared trampoline driver may call any of them.
		if grp, inGroup := groupOf[name]; inGroup {
			for _, sibling := range grp {
				reach(sibling)
			}
		}
	}

	// Rebuild Defs preserving declaration order, keeping only reachable entries.
	kept := make([]DefSpec, 0, len(reachable))
	for _, d := range p.Defs {
		if reachable[d.Name] {
			kept = append(kept, d)
		}
	}
	p.Defs = kept

	// Prune Partials to surviving names.
	if len(p.Partials) > 0 {
		pruned := make(map[string]bool, len(p.Partials))
		for name := range p.Partials {
			if reachable[name] {
				pruned[name] = true
			}
		}
		p.Partials = pruned
	}

	// Prune PartialGroups to groups with at least two surviving members.
	// A self-only partial has no PartialGroups entry (it was never recorded);
	// a mutual group with only one surviving member degenerates to self-only
	// and its entry is dropped (the trampoline works without a group record).
	if len(p.PartialGroups) > 0 {
		prunedGroups := p.PartialGroups[:0:0]
		for _, grp := range p.PartialGroups {
			var survivors []string
			for _, name := range grp {
				if reachable[name] {
					survivors = append(survivors, name)
				}
			}
			if len(survivors) >= 2 {
				prunedGroups = append(prunedGroups, survivors)
			}
		}
		p.PartialGroups = prunedGroups
	}

	return p
}
