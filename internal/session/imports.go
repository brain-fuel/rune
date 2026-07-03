package session

import (
	"fmt"
	"strings"

	"goforge.dev/rune/v3/surface"
)

// importScope tracks the active import/alias directives for one LoadSource call.
// Imports and aliases are per-source-file; they reset at the start of each call
// and are never session state.
type importScope struct {
	sess    *Session
	imports []string          // module names from 'import M' directives, in order
	aliases map[string]string // short-name -> full module name from 'alias M as G'
	local   map[string]bool   // all names defined in THIS source file
}

// newImportScope builds a fresh importScope for a LoadSource call.
// local must be the set of all names defined in the items being loaded (from
// definedNames), so that an import referencing a same-file module is accepted.
func newImportScope(s *Session, local map[string]bool) *importScope {
	return &importScope{
		sess:    s,
		aliases: map[string]string{},
		local:   local,
	}
}

// has reports whether name is visible in the session. Both definitions and
// constructors live in s.refs (AddData's bindName writes constructors there),
// so a single ref-map lookup suffices.
func (s *Session) has(n string) bool {
	_, ok := s.refs[n]
	return ok
}

// moduleKnown reports whether the session or the local name set has at least
// one definition whose name starts with mod+".". This is the validity check
// for both 'import' and 'alias' directives.
func (sc *importScope) moduleKnown(mod string) bool {
	prefix := mod + "."
	for n := range sc.sess.refs {
		if strings.HasPrefix(n, prefix) {
			return true
		}
	}
	for n := range sc.local {
		if strings.HasPrefix(n, prefix) {
			return true
		}
	}
	return false
}

// addImport records an 'import M' directive after validating the module.
func (sc *importScope) addImport(mod string) error {
	if !sc.moduleKnown(mod) {
		return fmt.Errorf("import %q: no definitions found for module %q", mod, mod)
	}
	sc.imports = append(sc.imports, mod)
	return nil
}

// addAlias records an 'alias M as G' directive after validating the module.
func (sc *importScope) addAlias(mod, as string) error {
	if !sc.moduleKnown(mod) {
		return fmt.Errorf("alias %q as %q: no definitions found for module %q", mod, as, mod)
	}
	sc.aliases[as] = mod
	return nil
}

// resolveName maps a surface reference through the active alias/import scope.
// Local (this-source) and session-global names win untouched.
// An alias prefix rewrites 'G.x' to 'Full.Module.x'.
// An unqualified name that matches exactly one import module is qualified.
// Two matches produce an ambiguity error listing both fully-qualified candidates.
// Zero matches return n unchanged so the elaborator reports the unknown name.
func (sc *importScope) resolveName(n string) (string, error) {
	// Local and session-global names always win without rewriting.
	if sc.local[n] || sc.sess.has(n) {
		return n, nil
	}
	// Alias prefix rewrite: 'G.x' where aliases['G'] = 'Full.Module' -> 'Full.Module.x'
	if i := strings.IndexByte(n, '.'); i > 0 {
		if full, ok := sc.aliases[n[:i]]; ok {
			return full + n[i:], nil
		}
	}
	// Unqualified import search: qualify 'n' with each imported module.
	var hits []string
	for _, m := range sc.imports {
		q := m + "." + n
		if sc.sess.has(q) || sc.local[q] {
			hits = append(hits, q)
		}
	}
	switch len(hits) {
	case 0:
		return n, nil // leave unchanged; elaboration reports the unknown name
	case 1:
		return hits[0], nil
	default:
		return "", fmt.Errorf("%q is ambiguous: could be %s", n, strings.Join(hits, " or "))
	}
}

// makeRen returns a renaming function that calls resolveName and accumulates the
// first error. After MapExpNames returns, the caller checks *errp.
func (sc *importScope) makeRen(errp *error) func(string) string {
	return func(n string) string {
		result, err := sc.resolveName(n)
		if err != nil && *errp == nil {
			*errp = err
		}
		return result
	}
}

// rewriteDef rewrites all name references in a Def through the scope.
func (sc *importScope) rewriteDef(d surface.Def) (surface.Def, error) {
	var renErr error
	ren := sc.makeRen(&renErr)
	if d.Ty != nil {
		d.Ty = surface.MapExpNames(d.Ty, ren)
	}
	if d.Body != nil {
		d.Body = surface.MapExpNames(d.Body, ren)
	}
	return d, renErr
}

// rewriteDataDef rewrites all name references in a DataDef through the scope.
func (sc *importScope) rewriteDataDef(d surface.DataDef) (surface.DataDef, error) {
	var renErr error
	ren := sc.makeRen(&renErr)
	if d.Ty != nil {
		d.Ty = surface.MapExpNames(d.Ty, ren)
	}
	for i, c := range d.Ctors {
		if c.Ty != nil {
			d.Ctors[i].Ty = surface.MapExpNames(c.Ty, ren)
		}
	}
	return d, renErr
}

// rewriteDefGroup rewrites all name references in a DefGroup through the scope.
func (sc *importScope) rewriteDefGroup(g surface.DefGroup) (surface.DefGroup, error) {
	var renErr error
	ren := sc.makeRen(&renErr)
	for i, m := range g.Members {
		if m.Ty != nil {
			g.Members[i].Ty = surface.MapExpNames(m.Ty, ren)
		}
		if m.Body != nil {
			g.Members[i].Body = surface.MapExpNames(m.Body, ren)
		}
	}
	return g, renErr
}

// rewriteDataGroup rewrites all name references in a DataGroup through the scope.
func (sc *importScope) rewriteDataGroup(g surface.DataGroup) (surface.DataGroup, error) {
	var renErr error
	ren := sc.makeRen(&renErr)
	for i, m := range g.Members {
		if m.Ty != nil {
			g.Members[i].Ty = surface.MapExpNames(m.Ty, ren)
		}
		for j, c := range m.Ctors {
			if c.Ty != nil {
				g.Members[i].Ctors[j].Ty = surface.MapExpNames(c.Ty, ren)
			}
		}
	}
	return g, renErr
}
