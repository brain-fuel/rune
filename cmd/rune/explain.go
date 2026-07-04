package main

import (
	"fmt"
	"io"
	"strconv"
	"strings"

	"goforge.dev/rune/v3/internal/explain"
	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// explainArgs is the parsed command line of `rune explain`.
type explainArgs struct {
	files     []string
	main      string
	opts      explain.Options
	noPrelude bool
}

// parseExplainArgs mirrors parseEmitArgs: positional <path...> [name] with
// non-recursive directory expansion, plus --depth <n|core> (or --depth=),
// --core (alias for --depth core), and --no-prelude. The last positional is
// the target name iff an earlier positional names an existing path and the
// last one does not; the name defaults to "main".
func parseExplainArgs(args []string) (explainArgs, error) {
	var ea explainArgs
	var pos []string
	for i := 0; i < len(args); i++ {
		a := args[i]
		switch {
		case a == "--no-prelude":
			ea.noPrelude = true
		case a == "--core":
			ea.opts.Core = true
		case a == "--depth" || strings.HasPrefix(a, "--depth="):
			v := strings.TrimPrefix(a, "--depth=")
			if a == "--depth" {
				if i+1 >= len(args) {
					return ea, fmt.Errorf("--depth needs a value (a number or core)")
				}
				i++
				v = args[i]
			}
			if v == "core" {
				ea.opts.Core = true
				break
			}
			n, err := strconv.Atoi(v)
			if err != nil || n < 0 {
				return ea, fmt.Errorf("--depth needs a non-negative number or core, got %q", v)
			}
			ea.opts.Depth = n
		default:
			pos = append(pos, a)
		}
	}
	if len(pos) == 0 {
		return ea, fmt.Errorf("explain needs a file")
	}
	paths := pos
	if len(pos) > 1 {
		lastExists := pathExists(pos[len(pos)-1])
		anyEarlierExists := false
		for _, p := range pos[:len(pos)-1] {
			if pathExists(p) {
				anyEarlierExists = true
				break
			}
		}
		if !lastExists && anyEarlierExists {
			ea.main = pos[len(pos)-1]
			paths = pos[:len(pos)-1]
		}
	}
	for _, p := range paths {
		expanded, err := expandRunePath(p)
		if err != nil {
			return ea, err
		}
		ea.files = append(ea.files, expanded...)
	}
	if len(ea.files) == 0 {
		return ea, fmt.Errorf("no .rune source files found in the given paths")
	}
	if ea.main == "" {
		ea.main = "main"
	}
	return ea, nil
}

// runExplainCLI drives `rune explain`: load the compilation set exactly as
// emit/run do (prelude on-demand, LoadSet topo sort), render the named
// definition as English steps, print.
func runExplainCLI(args []string, w io.Writer) error {
	ea, err := parseExplainArgs(args)
	if err != nil {
		return err
	}
	sources, err := readNamedSources(ea.files)
	if err != nil {
		return err
	}
	s := session.New()
	if !ea.noPrelude && !sourcesHaveBuiltinNat(sources) {
		if _, err := s.LoadSource(prelude.Source()); err != nil {
			return fmt.Errorf("loading prelude: %w", err)
		}
	}
	if err := session.LoadSet(s, sources); err != nil {
		return err
	}
	root, err := explain.Explain(s, ea.main, ea.opts)
	if err != nil {
		return err
	}
	_, err = io.WriteString(w, explain.RenderText(root))
	return err
}
