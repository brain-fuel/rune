// Command rune is the Phase-0 CLI for goforge.dev/rune.
//
//	rune fmt <file>   parse, resolve to core, and pretty-print back to surface
//	rune hash <file>  print the content hash of each definition
//
// Both parse the surface, resolve it to locally-nameless core (name resolution is the
// only elaboration in Phase 0 — no type checking), and either round-trip it through
// the pretty-printer or report its structural content hash.
package main

import (
	"fmt"
	"os"

	"goforge.dev/rune/core"
	"goforge.dev/rune/store"
	"goforge.dev/rune/surface"
)

func main() {
	if len(os.Args) < 3 {
		usage()
		os.Exit(2)
	}
	cmd, path := os.Args[1], os.Args[2]
	src, err := os.ReadFile(path)
	if err != nil {
		fatal(err)
	}
	switch cmd {
	case "fmt":
		err = runFmt(string(src))
	case "hash":
		err = runHash(string(src))
	default:
		usage()
		os.Exit(2)
	}
	if err != nil {
		fatal(err)
	}
}

// resolved is one definition after name resolution: its core type (optional), body,
// and content hash.
type resolved struct {
	name string
	ty   core.Tm
	body core.Tm
	hash core.Hash
}

// resolveFile parses and resolves every definition top-to-bottom. References to
// earlier definitions become content-hash Refs; a reference to a not-yet-defined name
// (forward or recursive) is rejected — Phase 0 handles only the acyclic case, while
// store.HashSCC lays down the recursive-group machinery for Phase 1.
func resolveFile(src string) ([]resolved, *store.Store, map[core.Hash]string, error) {
	defs, err := surface.ParseFile(src)
	if err != nil {
		return nil, nil, nil, err
	}
	st := store.New()
	refs := map[string]core.Hash{}
	refNames := map[core.Hash]string{}
	r := &surface.Resolver{Refs: refs}
	out := make([]resolved, 0, len(defs))
	for _, d := range defs {
		var ty core.Tm
		if d.Ty != nil {
			ty, err = r.ResolveExp(d.Ty)
			if err != nil {
				return nil, nil, nil, fmt.Errorf("%s: %w", d.Name, err)
			}
		}
		body, err := r.ResolveExp(d.Body)
		if err != nil {
			return nil, nil, nil, fmt.Errorf("%s: %w", d.Name, err)
		}
		h := st.Add(d.Name, ty, body)
		refs[d.Name] = h
		refNames[h] = d.Name
		out = append(out, resolved{name: d.Name, ty: ty, body: body, hash: h})
	}
	return out, st, refNames, nil
}

func runFmt(src string) error {
	defs, _, refNames, err := resolveFile(src)
	if err != nil {
		return err
	}
	for _, d := range defs {
		if d.ty != nil {
			fmt.Printf("%s : %s = %s\n", d.name,
				surface.PrettyWith(d.ty, refNames),
				surface.PrettyWith(d.body, refNames))
		} else {
			fmt.Printf("%s = %s\n", d.name, surface.PrettyWith(d.body, refNames))
		}
	}
	return nil
}

func runHash(src string) error {
	defs, _, _, err := resolveFile(src)
	if err != nil {
		return err
	}
	for _, d := range defs {
		fmt.Printf("%s  %s\n", d.hash, d.name)
	}
	return nil
}

func usage() {
	fmt.Fprintln(os.Stderr, "usage: rune (fmt|hash) <file>")
}

func fatal(err error) {
	fmt.Fprintln(os.Stderr, "rune:", err)
	os.Exit(1)
}
