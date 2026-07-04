package repl

import (
	"fmt"
	"io"
	"strconv"
	"strings"

	"goforge.dev/rune/v3/internal/explain"
	"goforge.dev/rune/v3/internal/session"
)

// runExplain is `:explain <name|$N|$> [--depth n|core] [--core] [--annotate]
// [--width n]`: the REPL frontend of internal/explain. A $N target renders
// the recorded surface expression of result N (bare $ = the latest); a name
// renders the retained surface definition. Same flags as the CLI; width
// defaults to 80 (deterministic, no TTY probing).
func runExplain(s *session.Session, st *replState, arg string, out io.Writer) error {
	fields := strings.Fields(arg)
	if len(fields) == 0 {
		return fmt.Errorf("usage: :explain <name|$N> [--depth n|core] [--annotate] [--width n]")
	}
	target := fields[0]
	var opts explain.Options
	annotate := false
	width := 80
	for i := 1; i < len(fields); i++ {
		switch fields[i] {
		case "--core":
			opts.Core = true
		case "--depth":
			i++
			if i >= len(fields) {
				return fmt.Errorf("--depth needs a value (a number or core)")
			}
			if fields[i] == "core" {
				opts.Core = true
				break
			}
			n, err := strconv.Atoi(fields[i])
			if err != nil || n < 0 {
				return fmt.Errorf("--depth needs a non-negative number or core, got %q", fields[i])
			}
			opts.Depth = n
		case "--annotate":
			annotate = true
		case "--width":
			i++
			if i >= len(fields) {
				return fmt.Errorf("--width needs a value")
			}
			n, err := strconv.Atoi(fields[i])
			if err != nil || n < 1 {
				return fmt.Errorf("--width needs a positive number, got %q", fields[i])
			}
			width = n
		default:
			return fmt.Errorf("unknown :explain flag %q", fields[i])
		}
	}
	var root explain.Step
	var err error
	if strings.HasPrefix(target, "$") {
		n := st.lastResult
		if len(target) > 1 {
			n, err = strconv.Atoi(target[1:])
			if err != nil {
				return fmt.Errorf(":explain: bad result reference %q", target)
			}
		}
		e, ok := st.historyExps[n]
		if !ok {
			return fmt.Errorf(":explain: no result $%d in this session", n)
		}
		root, err = explain.ExplainExp(s, e, opts)
	} else {
		root, err = explain.Explain(s, target, opts)
	}
	if err != nil {
		return err
	}
	if annotate {
		fmt.Fprint(out, explain.RenderAnnotate(root, width))
		return nil
	}
	fmt.Fprint(out, explain.RenderText(root))
	return nil
}
