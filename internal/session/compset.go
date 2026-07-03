package session

import (
	"fmt"
	"strings"

	"goforge.dev/rune/v3/surface"
)

// NamedSource is a source file together with its display name (path).
type NamedSource struct{ Name, Src string }

// LoadSet parses every source, topologically sorts the files by
// defined-name -> referenced-name edges, and loads them in order.
// Module blocks already qualify their defs at parse time, so "defined
// names" fall out of the parsed items directly.
//
// A dependency cycle is reported by naming all files involved.
func LoadSet(s *Session, sources []NamedSource) error {
	type parsed struct {
		src   NamedSource
		items []surface.Item
		defs  map[string]bool
		refs  map[string]bool
	}
	ps := make([]*parsed, 0, len(sources))
	owner := map[string]int{} // defined name -> file index
	for i, ns := range sources {
		items, err := surface.ParseProgram(ns.Src)
		if err != nil {
			return fmt.Errorf("%s: %s", ns.Name, surface.RenderParseError(ns.Src, err))
		}
		p := &parsed{
			src:   ns,
			items: items,
			defs:  definedNames(items),
			refs:  referencedNames(items),
		}
		for d := range p.defs {
			owner[d] = i
		}
		ps = append(ps, p)
	}

	// Build the dependency graph. File i depends on file j when i references
	// a name that j defines (and j != i; self-references do not create edges).
	adj := make([][]int, len(ps))   // adj[j] = files that depend on j
	indeg := make([]int, len(ps))
	for i, p := range ps {
		seen := map[int]bool{}
		for r := range p.refs {
			if j, ok := owner[r]; ok && j != i && !seen[j] {
				seen[j] = true
				adj[j] = append(adj[j], i)
				indeg[i]++
			}
		}
	}

	// Kahn's algorithm for topological sort.
	var order []int
	queue := []int{}
	for i := range ps {
		if indeg[i] == 0 {
			queue = append(queue, i)
		}
	}
	for len(queue) > 0 {
		i := queue[0]
		queue = queue[1:]
		order = append(order, i)
		for _, j := range adj[i] {
			indeg[j]--
			if indeg[j] == 0 {
				queue = append(queue, j)
			}
		}
	}

	if len(order) != len(ps) {
		var cyc []string
		for i := range ps {
			if indeg[i] > 0 {
				cyc = append(cyc, ps[i].src.Name)
			}
		}
		return fmt.Errorf("import cycle between files: %s", strings.Join(cyc, " -> "))
	}

	for _, i := range order {
		if _, err := s.LoadSource(ps[i].src.Src); err != nil {
			return fmt.Errorf("%s: %w", ps[i].src.Name, err)
		}
	}
	return nil
}

// definedNames returns the set of top-level names that a parsed file declares
// (former names, constructor names, eliminator names, and def names).
func definedNames(items []surface.Item) map[string]bool {
	defs := map[string]bool{}
	for _, it := range items {
		switch d := it.(type) {
		case surface.Def:
			defs[d.Name] = true
		case surface.DataDef:
			defs[d.Name] = true
			for _, c := range d.Ctors {
				defs[c.Name] = true
			}
			defs[d.Name+"Elim"] = true
		case surface.DefGroup:
			for _, m := range d.Members {
				defs[m.Name] = true
			}
		case surface.DataGroup:
			for _, m := range d.Members {
				defs[m.Name] = true
				for _, c := range m.Ctors {
					defs[c.Name] = true
				}
				defs[m.Name+"Elim"] = true
			}
		case surface.BuiltinNat, surface.BuiltinNatOp, surface.BuiltinNumInj:
			// session-state declarations: no new store entries
		}
	}
	return defs
}

// referencedNames returns the set of free names that a parsed file references.
// Constructor names appearing in case-pattern positions count as references.
func referencedNames(items []surface.Item) map[string]bool {
	refs := map[string]bool{}
	addExp := func(e surface.Exp) {
		surface.WalkExp(e, func(n surface.Exp) {
			if v, ok := n.(surface.EVar); ok {
				refs[v.Name] = true
			}
			// Case-pattern constructor names are references even though they
			// appear as strings in the clause, not as EVar expressions.
			if c, ok := n.(surface.ECase); ok {
				for _, cl := range c.Clauses {
					refs[cl.Ctor] = true
				}
			}
		})
	}
	for _, it := range items {
		switch d := it.(type) {
		case surface.Def:
			if d.Ty != nil {
				addExp(d.Ty)
			}
			if d.Body != nil {
				addExp(d.Body)
			}
		case surface.DataDef:
			if d.Ty != nil {
				addExp(d.Ty)
			}
			for _, c := range d.Ctors {
				if c.Ty != nil {
					addExp(c.Ty)
				}
			}
		case surface.DefGroup:
			for _, m := range d.Members {
				if m.Ty != nil {
					addExp(m.Ty)
				}
				if m.Body != nil {
					addExp(m.Body)
				}
			}
		case surface.DataGroup:
			for _, m := range d.Members {
				if m.Ty != nil {
					addExp(m.Ty)
				}
				for _, c := range m.Ctors {
					if c.Ty != nil {
						addExp(c.Ty)
					}
				}
			}
		case surface.BuiltinNat, surface.BuiltinNatOp, surface.BuiltinNumInj:
			// no expressions to walk.
			// Assumption: builtin declarations are co-located with the definitions
			// they name; a cross-file builtin would require reference extraction here.
		}
	}
	return refs
}
