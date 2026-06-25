// Package repl is the rune REPL: a read -> elaborate -> check -> normalize ->
// show loop over the shared session pipeline. Definitions are type checked on
// entry (and their certificates cached); a bare expression is elaborated, its
// type inferred, and its βδ-normal form printed alongside that type.
package repl

import (
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"regexp"
	"strings"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/surface"
)

const (
	prompt     = "rune> "
	contPrompt = "...> "
)

var errQuit = errors.New("quit")

// Config selects REPL startup behavior.
type Config struct {
	// NoPrelude starts a bare session: no Whole, no Nat, no numerals, no
	// operators — exactly the book's discipline of owning every definition.
	NoPrelude bool
}

// Run drives the REPL with the default configuration (prelude loaded).
func Run(in io.Reader, out io.Writer) error {
	return RunWith(in, out, Config{})
}

// RunWith drives the REPL until EOF or :quit. It never returns an error for bad
// user input — only for an I/O failure on the input stream or a broken prelude.
func RunWith(in io.Reader, out io.Writer, cfg Config) error {
	rd := newLineReader(in, out)
	defer rd.Close()
	s := session.New()

	fmt.Fprintln(out, "rune repl — expressions are type checked and normalized; definitions are checked and cached.")
	if !cfg.NoPrelude {
		if err := loadPrelude(s); err != nil {
			return err
		}
		fmt.Fprintln(out, "prelude: the numeric tower Whole < Int < Frac with overloaded `+ - *`. Subtraction PROMOTES — ℕ is not closed under it, so 2 - 5 = -3 : Int while 5 - 2 = 3 : Int; `monus` truncates at 0 and `minus a b (pf : b ≤ a)` stays Whole when provable. 1/3 - 2/3 = -1/3 : Frac. `/` builds a fraction; `divChecked a b` returns a `Result Frac ArithErr` (err carries `divByZero a b`); descend explicitly with `toNat`/`toWhole`/`toInt`, each a checked `Result _ ArithErr`; `// %` and gcd are integer ops at Whole. Pipe a fraction with `|>` into `to_radix` (1/3 |> to_radix = 0.{3}), `to_radix_sigplace n`, or `to_radix_sigfig n`. `rune repl --no-prelude` for a bare session.")
	}
	fmt.Fprintln(out, "results are numbered ($N) and referenceable; on a terminal: up/down history, Ctrl-R reverse search. type :help for commands, :quit to exit.")

	st := &replState{}
	for {
		line, ok, err := rd.ReadLine(fmt.Sprintf("rune[%d]> ", st.lineNo+1))
		if err != nil {
			return err
		}
		if !ok {
			fmt.Fprintln(out)
			return nil
		}
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			continue
		}
		// A non-blank submission claims the next line number (irb-style); a bare
		// expression result is then named $N for that N (jshell-style), while defs
		// and commands consume the number without producing a $N.
		st.lineNo++
		if strings.HasPrefix(trimmed, ":") {
			rd.AddHistory(line)
			if err := runCommand(s, cfg, st, trimmed, out); err != nil {
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
			err := runForm(s, st, buf, out)
			if err == nil {
				break
			}
			if !errors.Is(err, surface.ErrIncomplete) {
				fmt.Fprintln(out, "error:", err)
				break
			}
			more, ok, rerr := rd.ReadLine(contPrompt)
			if rerr != nil {
				return rerr
			}
			if !ok {
				fmt.Fprintln(out)
				fmt.Fprintln(out, "error: unexpected end of input")
				return nil
			}
			buf += "\n" + more
		}
		rd.AddHistory(buf)
	}
}

// replState carries the REPL's result numbering across the read loop. lineNo is the
// irb-style entry counter (drives the prompt and the $N result id); lastResult is the
// line number of the most recent expression result, recalled by a bare `$`.
type replState struct {
	lineNo     int
	lastResult int
	lastLoad   string // the most recent :load path, for :reload (the D7 dev-loop)
}

// resultRefRe matches a numbered result reference $N (jshell-style). It is rewritten
// to the internal binding name __resN before parsing, since `$` is not a legal rune
// identifier character — the numbering skin stays out of the core language.
var resultRefRe = regexp.MustCompile(`\$(\d+)`)

// expandResultRefs rewrites REPL result references into the internal names they were
// bound under: `$N` -> `__resN`, and a bare `$` -> the last result (`__res<last>`).
func expandResultRefs(src string, last int) string {
	src = resultRefRe.ReplaceAllString(src, "__res$1")
	return strings.ReplaceAll(src, "$", fmt.Sprintf("__res%d", last))
}

// runForm interprets a complete (or incomplete) input as either definitions or a bare
// expression. The choice is by shape: a `name :` head is a definition (parsed as a
// file), anything else is an expression. ErrIncomplete propagates so the caller can
// prompt for continuation.
func runForm(s *session.Session, st *replState, src string, out io.Writer) error {
	src = expandResultRefs(src, st.lastResult)
	if looksLikeDecl(src) {
		items, err := surface.ParseProgram(src)
		if err != nil {
			return locateParse(src, err)
		}
		for _, it := range items {
			if err := addItem(s, it, out); err != nil {
				return err
			}
		}
		return nil
	}
	e, err := s.ParseSrcExpr(src)
	if err != nil {
		return locateParse(src, err)
	}
	return runExpr(s, st, e, out)
}

// locateParse renders a terminal parse error with a source caret. An ErrIncomplete
// is passed through UNCHANGED so the read loop still recognises it and asks for more
// input (the multi-line continuation); only a real, terminal parse error is located.
func locateParse(src string, err error) error {
	if err == nil || errors.Is(err, surface.ErrIncomplete) {
		return err
	}
	return errors.New(surface.RenderParseError(src, err))
}

// addItem adds one parsed top-level form to the session, using the SAME session
// methods as the file loader (session.LoadSource), so the REPL has full parity with
// the compiler/interpreter: data types, functions, `foreign`/`instance`/`partial`
// definitions, `builtin` bindings, and `module` blocks all work, and a redefinition
// rebinds the name (AddDef overwrites s.refs — editing is latest-wins).
func addItem(s *session.Session, it surface.Item, out io.Writer) error {
	switch d := it.(type) {
	case surface.Def:
		rd, err := s.AddDef(d)
		if err != nil {
			return err
		}
		fmt.Fprintf(out, "defined %s\n", rd.Name)
	case surface.DataDef:
		names, err := s.AddData(d)
		if err != nil {
			return err
		}
		fmt.Fprintf(out, "declared %s\n", strings.Join(names, " "))
	case surface.DefGroup:
		names, err := s.AddDefGroup(d)
		if err != nil {
			return err
		}
		fmt.Fprintf(out, "defined %s\n", strings.Join(names, " "))
	case surface.DataGroup:
		names, err := s.AddDataGroup(d)
		if err != nil {
			return err
		}
		fmt.Fprintf(out, "declared %s\n", strings.Join(names, " "))
	case surface.BuiltinNat:
		if err := s.AddBuiltinNat(d); err != nil {
			return err
		}
		fmt.Fprintf(out, "registered builtin nat %s\n", d.TyName)
	case surface.BuiltinNatOp:
		if err := s.AddBuiltinNatOp(d); err != nil {
			return err
		}
		fmt.Fprintln(out, "registered builtin accel")
	case surface.BuiltinNumInj:
		if err := s.AddBuiltinNumInj(d); err != nil {
			return err
		}
		fmt.Fprintln(out, "registered builtin numinj")
	default:
		return fmt.Errorf("unsupported top-level form")
	}
	return nil
}

// runExpr is the SINGLE dispatch point for "what to do with a complete expression".
//
// Phase 1: elaborate (bidirectional, annotation-guided), then normalize (full βδ
// via NbE), then print `normal-form : type`.
func runExpr(s *session.Session, st *replState, e surface.Exp, out io.Writer) error {
	tm, ty, err := s.ElabExpr(e)
	if err != nil {
		return err
	}
	nf := s.NormalizeExpr(tm)
	// Normalize the type too: an overloaded operator's result type can be a stuck
	// projection of its (now-resolved) dictionary, e.g. `2 - 5` infers `Fst subWhole`
	// which βδι-reduces to `Int`. Showing the reduced type is what the user means.
	n := st.lineNo
	fmt.Fprintf(out, "$%d ==> %s : %s\n", n, s.Pretty(nf), s.Pretty(s.NormalizeExprFolded(ty)))
	// Bind the result so later input can name it ($N, or a bare $ for the latest).
	s.BindResult(fmt.Sprintf("__res%d", n), tm, ty)
	st.lastResult = n
	return nil
}

// looksLikeDecl reports whether src begins a top-level DECLARATION (so it is routed
// to the program parser, not the expression parser): any of the declaration-leading
// keywords, or the `name :` definition shape. This is what gives the REPL parity with
// the file loader — every form `rune` accepts in a source file works here too.
func looksLikeDecl(src string) bool {
	t := strings.TrimSpace(src)
	i := 0
	for i < len(t) && isIdentByte(t[i]) {
		i++
	}
	switch t[:i] {
	case "data", "foreign", "builtin", "instance", "partial", "module", "mutual":
		return true
	}
	return looksLikeDef(src)
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

func runCommand(s *session.Session, cfg Config, st *replState, line string, out io.Writer) error {
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
		st.lineNo, st.lastResult = 0, 0
		if !cfg.NoPrelude {
			if err := loadPrelude(s); err != nil {
				return err
			}
			fmt.Fprintln(out, "session cleared; prelude reloaded")
			return nil
		}
		fmt.Fprintln(out, "session cleared")
		return nil
	case ":load":
		if err := loadFile(s, arg, out); err != nil {
			return err
		}
		st.lastLoad = arg
		return nil
	case ":reload":
		// The D7 dev-loop: re-read the last-loaded file from disk. Content-addressing
		// makes it a hot reload — unchanged definitions hit the proof cache (same hash,
		// no re-check), edited ones re-elaborate and overwrite their name (latest-wins).
		if st.lastLoad == "" {
			return fmt.Errorf(":reload: nothing loaded yet (use :load <path> first)")
		}
		return loadFile(s, st.lastLoad, out)
	case ":core":
		return showCore(s, arg, out)
	case ":ast":
		return showAST(s, arg, out)
	case ":hash":
		return showHash(s, arg, out)
	case ":type", ":t":
		return showType(s, arg, out)
	case ":run":
		return runShadow(s, arg, out)
	default:
		return fmt.Errorf("unknown command %q (try :help)", cmd)
	}
}

func resolveArg(s *session.Session, arg string) (core.Tm, error) {
	if strings.TrimSpace(arg) == "" {
		return nil, fmt.Errorf("expected an expression")
	}
	e, err := s.ParseSrcExpr(arg)
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

// showAST renders the resolved core as a human-readable structural tree (:ast): the
// same breakdown as :core, but references print as their definition NAMES and binders
// as freshened names — the hashless twin of :core.
func showAST(s *session.Session, arg string, out io.Writer) error {
	c, err := resolveArg(s, arg)
	if err != nil {
		return err
	}
	fmt.Fprintln(out, surface.DebugCoreNamed(c, s.RefNames()))
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
	fmt.Fprintln(out, "  <expr>          elaborate, normalize, and print as $N ==> value : type")
	fmt.Fprintln(out, "  $N  /  $        recall the result of line N, or a bare $ for the latest")
	fmt.Fprintln(out, "  <name> : T is e end   add/REDEFINE a definition (latest wins; multi-line ok)")
	fmt.Fprintln(out, "  data … end            declare a datatype (+ its constructors and eliminator)")
	fmt.Fprintln(out, "  foreign x : T end     declare a typed FFI axiom; partial/instance/builtin/module too")
	fmt.Fprintln(out, "  :core <expr>    show the resolved core in explicit de Bruijn form")
	fmt.Fprintln(out, "  :ast <expr>     show the resolved core as a named structural tree (hashless :core)")
	fmt.Fprintln(out, "  :hash <expr>    show the content hash of the resolved core")
	fmt.Fprintln(out, "  :type <expr>    (:t) type checking arrives in Phase 1")
	fmt.Fprintln(out, "  :run <expr>    evaluate through the erased shadow (fast, computation only — no certificate)")
	fmt.Fprintln(out, "  :list           list session definitions")
	fmt.Fprintln(out, "  :load <path>    load definitions from a file")
	fmt.Fprintln(out, "  :reload         re-load the last :load'd file (hot reload; content-addressed cache)")
	fmt.Fprintln(out, "  :reset          clear the session (reloads the prelude unless --no-prelude)")
	fmt.Fprintln(out, "  :help           (:h) show this help")
	fmt.Fprintln(out, "  :quit           (:q) exit (Ctrl-D also exits)")
}

// runShadow is `:run <expr>`: the kernel evaluator PROVES, the erased shadow
// PERFORMS. The expression is elaborated and type checked against the session
// exactly as plain evaluation is (the kernel stays the authority on typing),
// then — instead of NbE-normalizing — lowered through the session's erased-JS
// shadow and executed under node, computation only, no certificate. Type
// checking and emission happen BEFORE the node lookup, so an ill-typed
// expression reports its type error whether or not node is installed.
func runShadow(s *session.Session, arg string, out io.Writer) error {
	if strings.TrimSpace(arg) == "" {
		return fmt.Errorf("usage: :run <expr>")
	}
	e, err := s.ParseSrcExpr(arg)
	if err != nil {
		return err
	}
	p, err := s.EmitExpr(e)
	if err != nil {
		return err
	}
	if _, err := exec.LookPath("node"); err != nil {
		return fmt.Errorf("the erased shadow needs node in PATH to run; plain evaluation (the expression without :run) still works")
	}
	src, err := codegen.Default().Emit(p)
	if err != nil {
		return err
	}
	f, err := os.CreateTemp("", "rune-repl-*.js")
	if err != nil {
		return err
	}
	defer os.Remove(f.Name())
	if _, err := io.WriteString(f, string(src)); err != nil {
		f.Close()
		return err
	}
	f.Close()
	cmd := exec.Command("node", f.Name())
	cmd.Stdout, cmd.Stderr = out, out
	return cmd.Run()
}

// showType elaborates an expression and prints only its inferred type.
func showType(s *session.Session, arg string, out io.Writer) error {
	if strings.TrimSpace(arg) == "" {
		return fmt.Errorf("expected an expression")
	}
	e, err := s.ParseSrcExpr(arg)
	if err != nil {
		return err
	}
	_, ty, err := s.ElabExpr(e)
	if err != nil {
		return err
	}
	fmt.Fprintln(out, s.Pretty(ty))
	return nil
}
