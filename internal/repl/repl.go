// Package repl is the rune REPL: a read -> elaborate -> check -> normalize ->
// show loop over the shared session pipeline. Definitions are type checked on
// entry (and their certificates cached); a bare expression is elaborated, its
// type inferred, and its βδ-normal form printed alongside that type.
package repl

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	"goforge.dev/rune/core"
	"goforge.dev/rune/internal/session"
	"goforge.dev/rune/surface"
)

const (
	prompt     = "rune> "
	contPrompt = "...> "
)

var errQuit = errors.New("quit")

// Run drives the REPL until EOF or :quit. It never returns an error for bad user
// input — only for an I/O failure on the input stream.
func Run(in io.Reader, out io.Writer) error {
	sc := bufio.NewScanner(in)
	sc.Buffer(make([]byte, 0, 64*1024), 1024*1024)
	s := session.New()

	fmt.Fprintln(out, "rune repl — expressions are type checked and normalized; definitions are checked and cached.")
	fmt.Fprintln(out, "type :help for commands, :quit to exit.")

	for {
		fmt.Fprint(out, prompt)
		line, ok := readLine(sc)
		if !ok {
			fmt.Fprintln(out)
			return sc.Err()
		}
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			continue
		}
		if strings.HasPrefix(trimmed, ":") {
			if err := runCommand(s, trimmed, out); err != nil {
				if errors.Is(err, errQuit) {
					return nil
				}
				fmt.Fprintln(out, "error:", err)
			}
			continue
		}

		// Accumulate lines until a complete top-level form parses (or the stream ends).
		buf := line
		for {
			err := runForm(s, buf, out)
			if err == nil {
				break
			}
			if !errors.Is(err, surface.ErrIncomplete) {
				fmt.Fprintln(out, "error:", err)
				break
			}
			fmt.Fprint(out, contPrompt)
			more, ok := readLine(sc)
			if !ok {
				fmt.Fprintln(out)
				fmt.Fprintln(out, "error: unexpected end of input")
				return sc.Err()
			}
			buf += "\n" + more
		}
	}
}

func readLine(sc *bufio.Scanner) (string, bool) {
	if !sc.Scan() {
		return "", false
	}
	return sc.Text(), true
}

// runForm interprets a complete (or incomplete) input as either definitions or a bare
// expression. The choice is by shape: a `name :` head is a definition (parsed as a
// file), anything else is an expression. ErrIncomplete propagates so the caller can
// prompt for continuation.
func runForm(s *session.Session, src string, out io.Writer) error {
	if looksLikeDef(src) {
		defs, err := surface.ParseFile(src)
		if err != nil {
			return err
		}
		for _, d := range defs {
			rd, err := s.AddDef(d)
			if err != nil {
				return err
			}
			fmt.Fprintf(out, "defined %s\n", rd.Name)
		}
		return nil
	}
	e, err := surface.ParseExpr(src)
	if err != nil {
		return err
	}
	return runExpr(s, e, out)
}

// runExpr is the SINGLE dispatch point for "what to do with a complete expression".
//
// Phase 1: elaborate (bidirectional, annotation-guided), then normalize (full βδ
// via NbE), then print `normal-form : type`.
func runExpr(s *session.Session, e surface.Exp, out io.Writer) error {
	tm, ty, err := s.ElabExpr(e)
	if err != nil {
		return err
	}
	nf := s.NormalizeExpr(tm)
	fmt.Fprintf(out, "%s : %s\n",
		surface.PrettyWith(nf, s.RefNames()), surface.PrettyWith(ty, s.RefNames()))
	return nil
}

// looksLikeDef reports whether src has the shape `Ident :` — an identifier head
// followed by a colon — so a malformed definition is diagnosed with the file parser's
// error rather than the expression parser's.
func looksLikeDef(src string) bool {
	t := strings.TrimSpace(src)
	i := 0
	for i < len(t) && isIdentByte(t[i]) {
		i++
	}
	if i == 0 {
		return false
	}
	for i < len(t) && (t[i] == ' ' || t[i] == '\t') {
		i++
	}
	return i < len(t) && t[i] == ':'
}

func isIdentByte(b byte) bool {
	return b == '_' || b == '\'' ||
		(b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || (b >= '0' && b <= '9')
}

func runCommand(s *session.Session, line string, out io.Writer) error {
	cmd := line
	arg := ""
	if i := strings.IndexAny(line, " \t"); i >= 0 {
		cmd, arg = line[:i], strings.TrimSpace(line[i+1:])
	}
	switch cmd {
	case ":quit", ":q":
		return errQuit
	case ":help", ":h":
		printHelp(out)
		return nil
	case ":list":
		listDefs(s, out)
		return nil
	case ":reset":
		s.Reset()
		fmt.Fprintln(out, "session cleared")
		return nil
	case ":load":
		return loadFile(s, arg, out)
	case ":core":
		return showCore(s, arg, out)
	case ":hash":
		return showHash(s, arg, out)
	case ":type", ":t":
		return showType(s, arg, out)
	default:
		return fmt.Errorf("unknown command %q (try :help)", cmd)
	}
}

func resolveArg(s *session.Session, arg string) (core.Tm, error) {
	if strings.TrimSpace(arg) == "" {
		return nil, fmt.Errorf("expected an expression")
	}
	e, err := surface.ParseExpr(arg)
	if err != nil {
		return nil, err
	}
	return s.ResolveExpr(e)
}

func showCore(s *session.Session, arg string, out io.Writer) error {
	c, err := resolveArg(s, arg)
	if err != nil {
		return err
	}
	fmt.Fprintln(out, surface.DebugCore(c))
	return nil
}

func showHash(s *session.Session, arg string, out io.Writer) error {
	c, err := resolveArg(s, arg)
	if err != nil {
		return err
	}
	fmt.Fprintln(out, core.HashTerm(c))
	return nil
}

func loadFile(s *session.Session, path string, out io.Writer) error {
	if path == "" {
		return fmt.Errorf("usage: :load <path>")
	}
	src, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	added, err := s.LoadSource(string(src))
	for _, n := range added {
		fmt.Fprintf(out, "defined %s\n", n)
	}
	return err
}

func listDefs(s *session.Session, out io.Writer) {
	defs := s.Defs()
	if len(defs) == 0 {
		fmt.Fprintln(out, "(no definitions)")
		return
	}
	names := s.RefNames()
	for _, d := range defs {
		if d.Ty != nil {
			fmt.Fprintf(out, "%s : %s\n", d.Name, surface.PrettyWith(d.Ty, names))
		} else {
			fmt.Fprintln(out, d.Name)
		}
	}
}

func printHelp(out io.Writer) {
	fmt.Fprintln(out, "commands:")
	fmt.Fprintln(out, "  <expr>          resolve and pretty-print (no evaluation yet)")
	fmt.Fprintln(out, "  <name> : T is e end   add a definition to the session")
	fmt.Fprintln(out, "  :core <expr>    show the resolved core in explicit de Bruijn form")
	fmt.Fprintln(out, "  :hash <expr>    show the content hash of the resolved core")
	fmt.Fprintln(out, "  :type <expr>    (:t) type checking arrives in Phase 1")
	fmt.Fprintln(out, "  :list           list session definitions")
	fmt.Fprintln(out, "  :load <path>    load definitions from a file")
	fmt.Fprintln(out, "  :reset          clear the session")
	fmt.Fprintln(out, "  :help           (:h) show this help")
	fmt.Fprintln(out, "  :quit           (:q) exit (Ctrl-D also exits)")
}

// showType elaborates an expression and prints only its inferred type.
func showType(s *session.Session, arg string, out io.Writer) error {
	if strings.TrimSpace(arg) == "" {
		return fmt.Errorf("expected an expression")
	}
	e, err := surface.ParseExpr(arg)
	if err != nil {
		return err
	}
	_, ty, err := s.ElabExpr(e)
	if err != nil {
		return err
	}
	fmt.Fprintln(out, surface.PrettyWith(ty, s.RefNames()))
	return nil
}
