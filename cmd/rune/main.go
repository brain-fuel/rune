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
	"io"
	"os/exec"
	"strings"

	"fmt"
	"goforge.dev/rune/v3/codegen"
	"os"

	"goforge.dev/rune/v3/internal/repl"
	"goforge.dev/rune/v3/internal/session"
)

func main() {
	if len(os.Args) < 2 {
		usage()
		os.Exit(2)
	}
	switch os.Args[1] {
	case "repl":
		cfg := repl.Config{}
		for _, a := range os.Args[2:] {
			if a == "--no-prelude" {
				cfg.NoPrelude = true
				continue
			}
			usage()
			os.Exit(2)
		}
		if err := repl.RunWith(os.Stdin, os.Stdout, cfg); err != nil {
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
	case "emit", "run":
		if len(os.Args) < 3 {
			usage()
			os.Exit(2)
		}
		src, err := os.ReadFile(os.Args[2])
		if err != nil {
			fatal(err)
		}
		main := ""
		if len(os.Args) > 3 {
			main = os.Args[3]
		}
		if os.Args[1] == "emit" {
			err = runEmit(string(src), main, os.Stdout)
		} else {
			err = runNode(string(src), main)
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
	for _, d := range s.Defs() {
		fmt.Printf("%s : %s is\n  %s\nend\n",
			d.Name, s.Pretty(d.Ty), s.Pretty(d.Body))
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
	fmt.Fprintln(os.Stderr, "usage: rune (fmt|hash) <file> | rune repl [--no-prelude]")
}

func fatal(err error) {
	fmt.Fprintln(os.Stderr, "rune:", err)
	os.Exit(1)
}

// runEmit type checks a file and writes its erased JavaScript shadow to w.
func runEmit(src, main string, w io.Writer) error {
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		return err
	}
	p, err := s.EmitProgram(main)
	if err != nil {
		return err
	}
	out, err := codegen.Default().Emit(p)
	if err != nil {
		return err
	}
	_, err = io.WriteString(w, string(out))
	return err
}

// runNode emits the program to a temporary file and executes it with node.
func runNode(src, main string) error {
	if main == "" {
		return fmt.Errorf("rune run needs a definition to evaluate: rune run FILE NAME")
	}
	if _, err := exec.LookPath("node"); err != nil {
		return fmt.Errorf("the js backend needs node in PATH to run (use `rune emit` to inspect the output)")
	}
	var buf strings.Builder
	if err := runEmit(src, main, &buf); err != nil {
		return err
	}
	f, err := os.CreateTemp("", "rune-*.js")
	if err != nil {
		return err
	}
	defer os.Remove(f.Name())
	if _, err := io.WriteString(f, buf.String()); err != nil {
		return err
	}
	f.Close()
	cmd := exec.Command("node", f.Name())
	cmd.Stdout, cmd.Stderr = os.Stdout, os.Stderr
	return cmd.Run()
}
