// Command rune is the CLI for goforge.dev/rune.
//
//	rune fmt <file>                 parse, resolve to core, pretty-print back to surface
//	rune hash <file>                print the content hash of each definition
//	rune repl [--no-prelude]        elaborate/normalize/print loop (internal/repl)
//	rune emit <file> [name]         emit target source for a definition (--target)
//	rune run <file> <name>          emit and execute on a backend (--target)
//	rune explain <file> [name]      render a definition as English steps (--depth n|core)
//	rune build <file> [name]        ship host artifacts (--kind app|library)
//	rune simulate <file> ...        drive a protocol under fault schedules (internal/sim)
//	rune deploy ...                 infra emit / workload run / --apply lifecycle
//	rune ledger <file>              assurance-ledger report (--json, --check)
//	rune calm <emit|validate> ...   FINOS CALM projection round-trip
//
// Every command runs the shared parse -> resolve -> hash pipeline (internal/session);
// the checking, codegen, infra, and ledger strata sit behind it.
package main

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/infra"
	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/repl"
	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/internal/sim"
	"goforge.dev/rune/v3/ledger"
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
		files, mainName, target, noPrelude, parseErr := parseEmitArgs(os.Args[2:])
		if parseErr != nil {
			fmt.Fprintln(os.Stderr, "rune:", parseErr)
			usage()
			os.Exit(2)
		}
		// Read all source files.
		sources, readErr := readNamedSources(files)
		if readErr != nil {
			fatal(readErr)
		}
		// Determine whether to load the prelude: skip when --no-prelude is set
		// or when any user source declares `builtin nat` (so existing listings
		// that define their own numeric tower continue to work unchanged).
		usePrelude := !noPrelude && !sourcesHaveBuiltinNat(sources)
		var runErr error
		if os.Args[1] == "emit" {
			runErr = runEmitSet(sources, mainName, target, os.Stdout, usePrelude)
		} else {
			runErr = runTargetSet(sources, mainName, target, usePrelude)
		}
		if runErr != nil {
			fatal(runErr)
		}
	case "explain":
		if err := runExplainCLI(os.Args[2:], os.Stdout); err != nil {
			fatal(err)
		}
	case "build":
		if err := runBuildCLI(os.Args[2:], os.Stdout); err != nil {
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
	case "ledger":
		if err := runLedger(os.Args[2:], os.Stdout); err != nil {
			fatal(err)
		}
	case "deploy":
		if err := runDeploy(os.Args[2:], os.Stdout); err != nil {
			fatal(err)
		}
	case "calm":
		if err := runCalm(os.Args[2:], os.Stdout); err != nil {
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
// runLedger type-checks a file and renders the assurance ledger. It parses
// --json/--check/--baseline flags from args; the first non-flag argument is the
// file path. Default (no flags) renders a text table.
func runLedger(args []string, w io.Writer) error {
	var (
		file     string
		asJSON   bool
		check    bool
		baseline string
	)
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--json":
			asJSON = true
		case "--check":
			check = true
		case "--baseline":
			i++
			if i < len(args) {
				baseline = args[i]
			}
		default:
			file = args[i]
		}
	}
	if file == "" {
		return fmt.Errorf("usage: rune ledger <file> [--json] [--check] [--baseline <ledger.json>]")
	}
	src, err := os.ReadFile(file)
	if err != nil {
		return err
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		return err
	}
	es := ledger.BuildWithSource(s, file, string(src))

	if check {
		violations := ledger.Gate(es, nil, ledger.GateConfig{})
		for _, v := range violations {
			fmt.Fprintln(os.Stderr, v.Error())
		}
		if len(violations) > 0 {
			return fmt.Errorf("assurance gate failed: %d violation(s)", len(violations))
		}
		return nil
	}
	if asJSON {
		return ledger.RenderJSON(es, w)
	}
	ledger.RenderText(es, w)
	_ = baseline // reserved for --baseline upgrade reporting
	return nil
}

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
	case derr == nil && !rep.Inflationary:
		fmt.Fprintf(out, "\nverdict: NOT GUARANTEED to converge - merge is a join, but an update is not "+
			"inflationary (it can move a replica down the lattice; see above). This run ended %v, but that is "+
			"schedule luck, not a property.\n", run.Final)
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
	fmt.Fprintln(os.Stderr, "  rune emit <path...> [name] [--target js|py|go|rs|erl|jvm] [--no-prelude]  (dirs expand to their .rune files)")
	fmt.Fprintln(os.Stderr, "  rune run  <path...> <name> [--target js|py|go|rs|erl|jvm] [--no-prelude]  (dirs expand to their .rune files)")
	fmt.Fprintln(os.Stderr, "  rune explain <path...> [name] [--depth n|core] [--no-prelude]   (English step view; name defaults to main)")
	fmt.Fprintln(os.Stderr, "  rune build <file> [name] [--target T] [--kind app|library] [--module M] [--export Rune[:Host]] [--out dir]")
	fmt.Fprintln(os.Stderr, "  rune simulate <file> [replicas]   (defines init/merge/value/op0..opN)")
	fmt.Fprintln(os.Stderr, "  rune deploy <file> [name] --target <backend>   (deploy + RUN a verified protocol)")
	fmt.Fprintln(os.Stderr, "  rune deploy --resource <kind> --name <n> --backend <b> [--out dir] [--replicas N] [--image ref] [--fifo]")
	fmt.Fprintln(os.Stderr, "  rune deploy --manifest <file> --backend <b> [--out dir]   (a whole resource graph)")
	fmt.Fprintln(os.Stderr, "    add --apply to STAND IT UP (docker compose for FOSS; terraform for cloud),")
	fmt.Fprintln(os.Stderr, "    --localstack[=URL] applies a cloud manifest to LocalStack (no account), --destroy tears down after")
	fmt.Fprintf(os.Stderr, "    kinds: %s\n", strings.Join(infra.Kinds(), " "))
	fmt.Fprintf(os.Stderr, "  targets: %s (aliases: python, rust, golang, javascript)\n",
		strings.Join(codegen.Targets(), ", "))
	fmt.Fprintf(os.Stderr, "  deploy backends: %s\n", strings.Join(infra.Targets(), ", "))
}

// parseEmitArgs reads positional <path...> [name] and optional flags
// `--target NAME` (or `--target=NAME`) and `--no-prelude`, for emit/run.
// The last positional is the main name iff at least one earlier positional
// names an existing file or directory and the last one does not.
// Directory paths expand non-recursively to their *.rune files, sorted.
func parseEmitArgs(args []string) (files []string, main, target string, noPrelude bool, err error) {
	target = codegen.Default().Target()
	var pos []string
	for i := 0; i < len(args); i++ {
		a := args[i]
		switch {
		case a == "--no-prelude":
			noPrelude = true
		case a == "--target", a == "-target":
			if i+1 >= len(args) {
				return nil, "", "", false, fmt.Errorf("--target needs a value")
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
		return nil, "", "", false, fmt.Errorf("emit/run needs a file")
	}
	// Determine whether the last positional is a main name or a file/dir path.
	// It is treated as a name when at least one earlier positional is an existing
	// file or directory and the last one is not.
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
			main = pos[len(pos)-1]
			paths = pos[:len(pos)-1]
		}
	}
	// Expand each path: directories become their *.rune files (sorted, non-recursive).
	for _, p := range paths {
		expanded, expErr := expandRunePath(p)
		if expErr != nil {
			return nil, "", "", false, expErr
		}
		files = append(files, expanded...)
	}
	if len(files) == 0 {
		return nil, "", "", false, fmt.Errorf("no .rune source files found in the given paths")
	}
	if _, ok := codegen.ByTarget(target); !ok {
		return nil, "", "", false, fmt.Errorf("unknown target %q (have %s)", target, strings.Join(codegen.Targets(), ", "))
	}
	return files, main, target, noPrelude, nil
}

// pathExists reports whether p names an existing file or directory.
func pathExists(p string) bool {
	_, err := os.Stat(p)
	return err == nil
}

// expandRunePath returns the .rune files for path p: a directory expands to its
// *.rune entries sorted by name (non-recursive); a plain file returns itself.
func expandRunePath(p string) ([]string, error) {
	info, err := os.Stat(p)
	if err != nil {
		return nil, fmt.Errorf("%s: no such file or directory", p)
	}
	if !info.IsDir() {
		return []string{p}, nil
	}
	entries, err := os.ReadDir(p)
	if err != nil {
		return nil, err
	}
	var out []string
	for _, e := range entries {
		if !e.IsDir() && strings.HasSuffix(e.Name(), ".rune") {
			out = append(out, filepath.Join(p, e.Name()))
		}
	}
	// os.ReadDir returns entries sorted by name, so out is already sorted.
	return out, nil
}

func fatal(err error) {
	fmt.Fprintln(os.Stderr, "rune:", err)
	os.Exit(1)
}

// emitFor checks the source and emits the erased shadow for the target backend,
// returning the source and the resolved backend.
func emitFor(src, main, target string) (string, codegen.Backend, error) {
	bk, ok := codegen.ByTarget(target)
	if !ok {
		return "", nil, fmt.Errorf("unknown target %q", target)
	}
	p, err := programFor(src, main)
	if err != nil {
		return "", nil, err
	}
	out, err := bk.Emit(p)
	if err != nil {
		return "", nil, err
	}
	return string(out), bk, nil
}

func programFor(src, main string) (codegen.Program, error) {
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		return codegen.Program{}, err
	}
	p, err := s.EmitProgram(main)
	if err != nil {
		return codegen.Program{}, err
	}
	return p, nil
}

// readNamedSources reads each file path and returns it as a NamedSource.
func readNamedSources(files []string) ([]session.NamedSource, error) {
	sources := make([]session.NamedSource, 0, len(files))
	for _, f := range files {
		data, err := os.ReadFile(f)
		if err != nil {
			return nil, err
		}
		sources = append(sources, session.NamedSource{Name: f, Src: string(data)})
	}
	return sources, nil
}

// sourcesHaveBuiltinNat returns true when any source text contains the token
// sequence "builtin nat", indicating the user's set declares its own numeric
// tower. In that case the CLI skips the always-on prelude to avoid shadowing
// conflicts, preserving the behaviour of listings like ch211 that define their
// own Nat type with builtin acceleration.
func sourcesHaveBuiltinNat(sources []session.NamedSource) bool {
	for _, ns := range sources {
		if strings.Contains(ns.Src, "builtin nat") {
			return true
		}
	}
	return false
}

// sourcesNeedPrelude returns true when any source text contains an explicit
// "import Std" or "alias Std" directive, indicating that the source requires
// names from the standard prelude (e.g. Std.Float, Std.IO). Used by rune build
// and rune deploy to load the prelude on-demand: only when actually needed,
// avoiding hash collisions when user sources define their own Nat-like types.
func sourcesNeedPrelude(sources []session.NamedSource) bool {
	for _, ns := range sources {
		if strings.Contains(ns.Src, "import Std") || strings.Contains(ns.Src, "alias Std") {
			return true
		}
	}
	return false
}

// programForSet topo-sorts and loads a set of named sources via session.LoadSet,
// then emits the erased program for the named main (may be "").
// When withPrelude is true the standard prelude is loaded first (before the topo
// sort), so user files can shadow prelude names by latest-wins.
func programForSet(sources []session.NamedSource, main string, withPrelude bool) (codegen.Program, error) {
	s := session.New()
	if withPrelude {
		if _, err := s.LoadSource(prelude.Source()); err != nil {
			return codegen.Program{}, fmt.Errorf("loading prelude: %w", err)
		}
	}
	if err := session.LoadSet(s, sources); err != nil {
		return codegen.Program{}, err
	}
	return s.EmitProgram(main)
}

// runEmitSet emits the erased shadow for a set of source files via LoadSet.
func runEmitSet(sources []session.NamedSource, main, target string, w io.Writer, withPrelude bool) error {
	bk, ok := codegen.ByTarget(target)
	if !ok {
		return fmt.Errorf("unknown target %q", target)
	}
	p, err := programForSet(sources, main, withPrelude)
	if err != nil {
		return err
	}
	out, err := bk.Emit(p)
	if err != nil {
		return err
	}
	_, err = io.WriteString(w, string(out))
	return err
}

// runTargetSet emits and executes a set of source files via LoadSet.
func runTargetSet(sources []session.NamedSource, main, target string, withPrelude bool) error {
	if main == "" {
		return fmt.Errorf("rune run needs a definition to evaluate: rune run FILE NAME")
	}
	bk, ok := codegen.ByTarget(target)
	if !ok {
		return fmt.Errorf("unknown target %q", target)
	}
	p, err := programForSet(sources, main, withPrelude)
	if err != nil {
		return err
	}
	out, err := bk.Emit(p)
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
	cmd.Stdin, cmd.Stdout, cmd.Stderr = os.Stdin, os.Stdout, os.Stderr
	return cmd.Run()
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
	cmd.Stdin, cmd.Stdout, cmd.Stderr = os.Stdin, os.Stdout, os.Stderr
	return cmd.Run()
}
