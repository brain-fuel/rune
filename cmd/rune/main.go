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
	"path/filepath"
	"strconv"
	"strings"

	"fmt"
	"goforge.dev/rune/v3/codegen"
	"os"

	"goforge.dev/rune/v3/internal/repl"
	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/internal/sim"
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
		file, main, target, err := parseEmitArgs(os.Args[2:])
		if err != nil {
			fmt.Fprintln(os.Stderr, "rune:", err)
			usage()
			os.Exit(2)
		}
		src, err := os.ReadFile(file)
		if err != nil {
			fatal(err)
		}
		if os.Args[1] == "emit" {
			err = runEmit(string(src), main, target, os.Stdout)
		} else {
			err = runTarget(string(src), main, target)
		}
		if err != nil {
			fatal(err)
		}
	case "simulate":
		if len(os.Args) < 3 {
			usage()
			os.Exit(2)
		}
		src, err := os.ReadFile(os.Args[2])
		if err != nil {
			fatal(err)
		}
		n := 2
		if len(os.Args) >= 4 {
			parsed, perr := strconv.Atoi(os.Args[3])
			if perr != nil || parsed < 1 {
				fmt.Fprintln(os.Stderr, "rune: replica count must be a positive integer")
				os.Exit(2)
			}
			n = parsed
		}
		if err := runSimulate(string(src), n, os.Stdout); err != nil {
			fatal(err)
		}
	default:
		usage()
		os.Exit(2)
	}
}

// runSimulate runs the better-than-Winglang simulator (E4) over a protocol file by
// convention: the file defines `init` (a replica's start state), `merge` (the join),
// `value` (the observable), and one local op per replica `op0`, `op1`, .... The
// canned scenario makes every replica act, then a PARTITIONED gossip round (the
// network is cut in half) so divergence is visible, then a HEALED gossip round; the
// rendered trace shows whether the protocol re-converges. Full scenario/`protocol`
// surface sugar is a later contained pass.
func runSimulate(src string, n int, out io.Writer) error {
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		return err
	}
	rounds := []sim.Round{{Local: map[int]string{}}}
	for i := 0; i < n; i++ {
		rounds[0].Local[i] = "op" + strconv.Itoa(i)
	}
	// During the partition, replica 0 acts again so the cut halves visibly diverge
	// (a value function can otherwise mask one-op-each as equal); then heal.
	rounds = append(rounds,
		sim.Round{Local: map[int]string{0: "op0"}, Gossip: true}, // partitioned (see policy)
		sim.Round{Gossip: true},                                  // healed
	)
	half := n / 2
	pol := sim.FaultPolicy{
		Partitioned: func(step, i, j int) bool {
			return step == 1 && (i < half) != (j < half)
		},
	}
	run, err := sim.Simulate(s, "init", "merge", "value", n, rounds, pol)
	if err != nil {
		return err
	}
	fmt.Fprint(out, sim.Render(run, n))
	// Diagnose the merge laws observationally. The LINTER is authoritative: it proves
	// (by counterexample) that a non-join cannot converge under every schedule, even
	// when a particular run happened to look fine. The better-than-Winglang catch.
	ops := make([]string, n)
	for i := range ops {
		ops[i] = "op" + strconv.Itoa(i)
	}
	rep, derr := sim.Diagnose(s, "init", "merge", ops)
	if derr == nil {
		fmt.Fprint(out, "\n"+sim.RenderReport(rep))
	}
	switch {
	case derr == nil && !rep.IsCvRDT():
		fmt.Fprintf(out, "\nverdict: NOT GUARANTEED to converge - merge is not a join (see the failed law above). "+
			"This run ended %v, but that is schedule luck, not a property.\n", run.Final)
	case run.Converged():
		fmt.Fprintf(out, "\nverdict: CONVERGED to %s on all %d replicas (and the join laws hold, so under any schedule).\n", run.Final[0], n)
	default:
		fmt.Fprintf(out, "\nverdict: did NOT converge (final %v).\n", run.Final)
	}
	return nil
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
	fmt.Fprintln(os.Stderr, "usage:")
	fmt.Fprintln(os.Stderr, "  rune (fmt|hash) <file>")
	fmt.Fprintln(os.Stderr, "  rune repl [--no-prelude]")
	fmt.Fprintln(os.Stderr, "  rune emit <file> [name] [--target js|py|go|rs|erl|jvm]")
	fmt.Fprintln(os.Stderr, "  rune run  <file> <name> [--target js|py|go|rs|erl|jvm]")
	fmt.Fprintln(os.Stderr, "  rune simulate <file> [replicas]   (defines init/merge/value/op0..opN)")
	fmt.Fprintf(os.Stderr, "  targets: %s (aliases: python, rust, golang, javascript)\n",
		strings.Join(codegen.Targets(), ", "))
}

// parseEmitArgs reads the positional <file> [name] and an optional
// `--target NAME` (or `--target=NAME`) flag, in any order, for emit/run.
func parseEmitArgs(args []string) (file, main, target string, err error) {
	target = codegen.Default().Target()
	var pos []string
	for i := 0; i < len(args); i++ {
		a := args[i]
		switch {
		case a == "--target", a == "-target":
			if i+1 >= len(args) {
				return "", "", "", fmt.Errorf("--target needs a value")
			}
			i++
			target = args[i]
		case strings.HasPrefix(a, "--target="):
			target = strings.TrimPrefix(a, "--target=")
		default:
			pos = append(pos, a)
		}
	}
	if len(pos) == 0 {
		return "", "", "", fmt.Errorf("emit/run needs a file")
	}
	file = pos[0]
	if len(pos) > 1 {
		main = pos[1]
	}
	if _, ok := codegen.ByTarget(target); !ok {
		return "", "", "", fmt.Errorf("unknown target %q (have %s)", target, strings.Join(codegen.Targets(), ", "))
	}
	return file, main, target, nil
}

func fatal(err error) {
	fmt.Fprintln(os.Stderr, "rune:", err)
	os.Exit(1)
}

// runEmit type checks a file and writes its erased shadow for the named target
// to w (default target is JavaScript).
func runEmit(src, main, target string, w io.Writer) error {
	out, _, err := emitFor(src, main, target)
	if err != nil {
		return err
	}
	_, err = io.WriteString(w, out)
	return err
}

// emitFor checks the source and emits the erased shadow for the target backend,
// returning the source and the resolved backend.
func emitFor(src, main, target string) (string, codegen.Backend, error) {
	bk, ok := codegen.ByTarget(target)
	if !ok {
		return "", nil, fmt.Errorf("unknown target %q", target)
	}
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		return "", nil, err
	}
	p, err := s.EmitProgram(main)
	if err != nil {
		return "", nil, err
	}
	out, err := bk.Emit(p)
	if err != nil {
		return "", nil, err
	}
	return string(out), bk, nil
}

// targetRunner names, per backend Target(), the temp-file extension and the
// command that executes the emitted source. A nil compile means the run command
// takes the source file directly; otherwise compile produces a binary that runs.
var targetRunner = map[string]struct {
	ext     string
	bin     string // tool that must be in PATH
	run     func(file string) *exec.Cmd
	compile func(file, out string) *exec.Cmd // nil for interpreted backends
}{
	"js": {ext: "js", bin: "node", run: func(f string) *exec.Cmd { return exec.Command("node", f) }},
	"py": {ext: "py", bin: "python3", run: func(f string) *exec.Cmd { return exec.Command("python3", f) }},
	"go": {ext: "go", bin: "go", run: func(f string) *exec.Cmd { return exec.Command("go", "run", f) }},
	"rs": {ext: "rs", bin: "rustc",
		run:     func(bin string) *exec.Cmd { return exec.Command(bin) },
		compile: func(f, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-O", "-o", out, f) }},
	"erl": {ext: "erl", bin: "escript", run: func(f string) *exec.Cmd { return exec.Command("escript", f) }},
	"jvm": {ext: "java", bin: javac25Path(),
		run: func(out string) *exec.Cmd {
			return exec.Command(java25Path(), "-cp", filepath.Dir(out), "main")
		},
		compile: func(f, out string) *exec.Cmd {
			return exec.Command(javac25Path(), "--release", "25", "-d", filepath.Dir(out), f)
		}},
}

// javac25Path / java25Path resolve a JDK 25+ toolchain for the JVM backend
// (records/sealed/pattern-switch/virtual-threads). Prefer an asdf-installed
// temurin-25; else a PATH javac if it is recent enough. Empty string ⇒ not found
// (runTarget then reports it cannot run the JVM target).
func javac25Path() string { jc, _ := resolveJava25(); return jc }
func java25Path() string  { _, jv := resolveJava25(); return jv }

func resolveJava25() (javac, java string) {
	home, _ := os.UserHomeDir()
	if m, _ := filepath.Glob(filepath.Join(home, ".asdf/installs/java/temurin-25*/bin/javac")); len(m) > 0 {
		dir := filepath.Dir(m[0])
		return m[0], filepath.Join(dir, "java")
	}
	if jc, err := exec.LookPath("javac"); err == nil {
		jv, _ := exec.LookPath("java")
		return jc, jv
	}
	return "", ""
}

// runTarget emits the program for the target and executes it, compiling first
// for the backends that need it (Rust).
func runTarget(src, main, target string) error {
	if main == "" {
		return fmt.Errorf("rune run needs a definition to evaluate: rune run FILE NAME")
	}
	out, bk, err := emitFor(src, main, target)
	if err != nil {
		return err
	}
	r, ok := targetRunner[bk.Target()]
	if !ok {
		return fmt.Errorf("no runner for target %q (use `rune emit` to inspect the output)", bk.Target())
	}
	if _, err := exec.LookPath(r.bin); err != nil {
		return fmt.Errorf("the %s backend needs %s in PATH to run (use `rune emit` to inspect the output)", bk.Target(), r.bin)
	}
	dir, err := os.MkdirTemp("", "rune-run-")
	if err != nil {
		return err
	}
	defer os.RemoveAll(dir)
	srcFile := dir + "/main." + r.ext
	if err := os.WriteFile(srcFile, []byte(out), 0o644); err != nil {
		return err
	}
	runFile := srcFile
	if r.compile != nil {
		binFile := dir + "/main.bin"
		c := r.compile(srcFile, binFile)
		c.Stderr = os.Stderr
		if err := c.Run(); err != nil {
			return fmt.Errorf("compiling the %s shadow failed: %w", bk.Target(), err)
		}
		runFile = binFile
	}
	cmd := r.run(runFile)
	cmd.Stdout, cmd.Stderr = os.Stdout, os.Stderr
	return cmd.Run()
}
