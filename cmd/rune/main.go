// Command rune is the Phase-0 CLI for goforge.dev/rune.
//
//	rune fmt <file>   parse, resolve to core, and pretty-print back to surface
//	rune hash <file>  print the content hash of each definition
//	rune repl         read -> resolve -> show loop (no evaluation; see internal/repl)
//
// All three parse the surface, resolve it to locally-nameless core (name resolution is
// the only elaboration in Phase 0 — no type checking), and either round-trip it through
// the pretty-printer, report its structural content hash, or do so interactively.
package main

import (
	"fmt"
	"os"

	"goforge.dev/rune/internal/repl"
	"goforge.dev/rune/internal/session"
	"goforge.dev/rune/surface"
)

func main() {
	if len(os.Args) < 2 {
		usage()
		os.Exit(2)
	}
	switch os.Args[1] {
	case "repl":
		if err := repl.Run(os.Stdin, os.Stdout); err != nil {
			fatal(err)
		}
		return
	case "fmt", "hash":
		if len(os.Args) < 3 {
			usage()
			os.Exit(2)
		}
		src, err := os.ReadFile(os.Args[2])
		if err != nil {
			fatal(err)
		}
		if os.Args[1] == "fmt" {
			err = runFmt(string(src))
		} else {
			err = runHash(string(src))
		}
		if err != nil {
			fatal(err)
		}
	default:
		usage()
		os.Exit(2)
	}
}

func runFmt(src string) error {
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		return err
	}
	names := s.RefNames()
	for _, d := range s.Defs() {
		fmt.Printf("%s : %s is\n  %s\nend\n",
			d.Name,
			surface.PrettyWith(d.Ty, names),
			surface.PrettyWith(d.Body, names))
	}
	return nil
}

func runHash(src string) error {
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		return err
	}
	for _, d := range s.Defs() {
		fmt.Printf("%s  %s\n", d.Hash, d.Name)
	}
	return nil
}

func usage() {
	fmt.Fprintln(os.Stderr, "usage: rune (fmt|hash) <file> | rune repl")
}

func fatal(err error) {
	fmt.Fprintln(os.Stderr, "rune:", err)
	os.Exit(1)
}
