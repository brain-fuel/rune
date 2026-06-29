package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"goforge.dev/rune/v3/codegen"
)

type buildArgs struct {
	file    string
	main    string
	target  string
	kind    codegen.BuildKind
	module  string
	exports []codegen.Export
	outDir  string
}

func parseBuildArgs(args []string) (buildArgs, error) {
	cfg := buildArgs{
		target: codegen.Default().Target(),
		kind:   codegen.BuildApp,
	}
	var pos []string
	for i := 0; i < len(args); i++ {
		a := args[i]
		val := func(name string) (string, error) {
			if i+1 >= len(args) {
				return "", fmt.Errorf("%s needs a value", name)
			}
			i++
			return args[i], nil
		}
		switch {
		case a == "--target" || a == "-target":
			v, err := val("--target")
			if err != nil {
				return buildArgs{}, err
			}
			cfg.target = v
		case strings.HasPrefix(a, "--target="):
			cfg.target = strings.TrimPrefix(a, "--target=")
		case a == "--kind":
			v, err := val("--kind")
			if err != nil {
				return buildArgs{}, err
			}
			cfg.kind = codegen.BuildKind(v)
		case strings.HasPrefix(a, "--kind="):
			cfg.kind = codegen.BuildKind(strings.TrimPrefix(a, "--kind="))
		case a == "--module":
			v, err := val("--module")
			if err != nil {
				return buildArgs{}, err
			}
			cfg.module = v
		case strings.HasPrefix(a, "--module="):
			cfg.module = strings.TrimPrefix(a, "--module=")
		case a == "--export":
			v, err := val("--export")
			if err != nil {
				return buildArgs{}, err
			}
			ex, err := parseBuildExport(v)
			if err != nil {
				return buildArgs{}, err
			}
			cfg.exports = append(cfg.exports, ex)
		case strings.HasPrefix(a, "--export="):
			ex, err := parseBuildExport(strings.TrimPrefix(a, "--export="))
			if err != nil {
				return buildArgs{}, err
			}
			cfg.exports = append(cfg.exports, ex)
		case a == "--out":
			v, err := val("--out")
			if err != nil {
				return buildArgs{}, err
			}
			cfg.outDir = v
		case strings.HasPrefix(a, "--out="):
			cfg.outDir = strings.TrimPrefix(a, "--out=")
		default:
			pos = append(pos, a)
		}
	}
	if len(pos) == 0 {
		return buildArgs{}, fmt.Errorf("build needs a file")
	}
	cfg.file = pos[0]
	if len(pos) > 1 {
		cfg.main = pos[1]
	}
	if _, ok := codegen.ByTarget(cfg.target); !ok {
		return buildArgs{}, fmt.Errorf("unknown target %q (have %s)", cfg.target, strings.Join(codegen.Targets(), ", "))
	}
	switch cfg.kind {
	case codegen.BuildApp, codegen.BuildLibrary:
	default:
		return buildArgs{}, fmt.Errorf("unknown build kind %q", cfg.kind)
	}
	return cfg, nil
}

func parseBuildExport(text string) (codegen.Export, error) {
	if text == "" {
		return codegen.Export{}, fmt.Errorf("--export needs a Rune name")
	}
	runeName, hostName, ok := strings.Cut(text, ":")
	if !ok {
		runeName, hostName, _ = strings.Cut(text, "=")
	}
	if runeName == "" {
		return codegen.Export{}, fmt.Errorf("--export needs a Rune name")
	}
	return codegen.Export{RuneName: runeName, HostName: hostName}, nil
}

// runBuildCLI is the raw-argv entry the `rune build` dispatch uses: parse flags,
// read the source file, then emit. It mirrors runDeploy's handler shape so the
// whole CLI path (flag parse + file IO + emit) is covered by one test seam.
func runBuildCLI(args []string, out io.Writer) error {
	cfg, err := parseBuildArgs(args)
	if err != nil {
		return err
	}
	src, err := os.ReadFile(cfg.file)
	if err != nil {
		return err
	}
	return runBuild(string(src), cfg, out)
}

func runBuild(src string, cfg buildArgs, out io.Writer) error {
	p, err := programFor(src, cfg.main)
	if err != nil {
		return err
	}
	bk, ok := codegen.ByTarget(cfg.target)
	if !ok {
		return fmt.Errorf("unknown target %q", cfg.target)
	}
	set, err := codegen.Build(bk, p, codegen.BuildSpec{
		Kind:    cfg.kind,
		Module:  cfg.module,
		Exports: cfg.exports,
	})
	if err != nil {
		return err
	}
	return writeCodegenArtifacts(set, cfg.outDir, out)
}

func writeCodegenArtifacts(set codegen.ArtifactSet, outDir string, out io.Writer) error {
	arts := append([]codegen.Artifact(nil), set.Artifacts...)
	sort.Slice(arts, func(i, j int) bool { return arts[i].Path < arts[j].Path })
	if outDir == "" {
		for _, a := range arts {
			fmt.Fprintf(out, "# %s\n%s", a.Path, string(a.Data))
			if !strings.HasSuffix(string(a.Data), "\n") {
				fmt.Fprintln(out)
			}
		}
		return nil
	}
	for _, a := range arts {
		path := filepath.Join(outDir, a.Path)
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			return err
		}
		if err := os.WriteFile(path, a.Data, 0o644); err != nil {
			return err
		}
	}
	fmt.Fprintf(out, "wrote %d file(s) to %s\n", len(arts), outDir)
	return nil
}
