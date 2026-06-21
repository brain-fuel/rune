package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/infra"
)

// runDeploy is the `rune deploy` verb (E4 / the wavelet infra layer): it lowers an
// agnostic resource to a concrete deployment artifact for the chosen backend — an
// OpenTofu/Terraform main.tf for a cloud (aws|azure|gcp), or a Podman Compose spec +
// connection.env for a self-hosted FOSS backend (rabbitmq|nats|valkey|garage). With
// --out the files are written to a directory; otherwise they print to stdout. One
// agnostic config yields an equivalent deployment on every backend.
//
// Slice 1 drives the resource from flags (--resource/--name); declaring resources
// in-language (an `infra`/`protocol` block in FILE) is the next step.
func runDeploy(args []string, out io.Writer) error {
	var resource, name, backend, outDir, image, target string
	fifo := false
	replicas := 1
	var positional []string
	for i := 0; i < len(args); i++ {
		a := args[i]
		val := func() (string, error) {
			if i+1 >= len(args) {
				return "", fmt.Errorf("%s needs a value", a)
			}
			i++
			return args[i], nil
		}
		var err error
		switch {
		case a == "--resource":
			resource, err = val()
		case a == "--name":
			name, err = val()
		case a == "--backend":
			backend, err = val()
		case a == "--target":
			target, err = val()
		case a == "--out":
			outDir, err = val()
		case a == "--fifo":
			fifo = true
		case a == "--image":
			image, err = val()
		case a == "--replicas":
			var v string
			v, err = val()
			if err == nil {
				if n, perr := strconv.Atoi(v); perr == nil && n >= 1 {
					replicas = n
				} else {
					err = fmt.Errorf("--replicas needs a positive integer")
				}
			}
		case strings.HasPrefix(a, "--image="):
			image = strings.TrimPrefix(a, "--image=")
		case strings.HasPrefix(a, "--replicas="):
			if n, perr := strconv.Atoi(strings.TrimPrefix(a, "--replicas=")); perr == nil && n >= 1 {
				replicas = n
			} else {
				err = fmt.Errorf("--replicas needs a positive integer")
			}
		case strings.HasPrefix(a, "--resource="):
			resource = strings.TrimPrefix(a, "--resource=")
		case strings.HasPrefix(a, "--name="):
			name = strings.TrimPrefix(a, "--name=")
		case strings.HasPrefix(a, "--backend="):
			backend = strings.TrimPrefix(a, "--backend=")
		case strings.HasPrefix(a, "--target="):
			target = strings.TrimPrefix(a, "--target=")
		case strings.HasPrefix(a, "--out="):
			outDir = strings.TrimPrefix(a, "--out=")
		case strings.HasPrefix(a, "--"):
			return fmt.Errorf("rune deploy: unexpected flag %q", a)
		default:
			positional = append(positional, a)
		}
		if err != nil {
			return err
		}
	}

	// WORKLOAD MODE: `rune deploy FILE [NAME] --target beam` deploys + RUNS a verified
	// protocol's actor system on a real backend (the Lambert "it runs" gate). A code
	// target or a positional FILE selects it.
	if resource == "" && (len(positional) > 0 || isCodeTarget(target)) {
		return runWorkloadDeploy(positional, target, out)
	}

	// INFRA MODE: lower an agnostic resource to a deployment artifact.
	if resource == "" || name == "" || backend == "" {
		return fmt.Errorf("rune deploy needs either FILE [NAME] --target <backend> (run a protocol), "+
			"or --resource <queue|kv|object|compute> --name <name> --backend <%s> (emit infra)",
			strings.Join(infra.Targets(), "|"))
	}

	r, err := resourceFor(resource, name, fifo, image, replicas)
	if err != nil {
		return err
	}
	e, ok := infra.ByTarget(backend)
	if !ok {
		return fmt.Errorf("rune deploy: unknown backend %q (have %s)", backend,
			strings.Join(infra.Targets(), ", "))
	}
	art, err := e.Emit([]infra.Resource{r})
	if err != nil {
		return err
	}
	return writeArtifact(art, outDir, out)
}

// isCodeTarget reports whether t names a codegen backend (a runnable workload target)
// rather than an infra backend.
func isCodeTarget(t string) bool {
	if t == "" {
		return false
	}
	_, ok := codegen.ByTarget(t)
	return ok
}

// runWorkloadDeploy deploys + RUNS a verified protocol's actor system on a real
// backend: `rune deploy FILE [NAME] --target beam`. This is the Lambert gate — a
// proven CvRDT does not just emit config, it runs (e.g. live gossiping BEAM actors
// via the serveG projection). It reuses the emit-and-execute path (runTarget); BEAM
// (erl) is the default distributed target.
func runWorkloadDeploy(positional []string, target string, out io.Writer) error {
	if len(positional) == 0 {
		return fmt.Errorf("rune deploy FILE [NAME] --target <backend>: needs a source file")
	}
	file := positional[0]
	mainName := "main"
	if len(positional) > 1 {
		mainName = positional[1]
	}
	if target == "" {
		target = "erl" // BEAM, the distributed default
	}
	src, err := os.ReadFile(file)
	if err != nil {
		return err
	}
	fmt.Fprintf(out, "deploying %s (%s) on target %q…\n", file, mainName, target)
	return runTarget(string(src), mainName, target)
}

// resourceFor builds an agnostic Resource from the CLI flags.
func resourceFor(kind, name string, fifo bool, image string, replicas int) (infra.Resource, error) {
	switch kind {
	case "queue":
		return infra.Queue{Name: name, FIFO: fifo}, nil
	case "kv":
		return infra.KV{Name: name}, nil
	case "object":
		return infra.Bucket{Name: name}, nil
	case "compute":
		return infra.Compute{Name: name, Image: image, Replicas: replicas}, nil
	case "database":
		return infra.Database{Name: name}, nil
	case "secret":
		return infra.Secret{Name: name}, nil
	case "nosql":
		return infra.NoSQL{Name: name}, nil
	default:
		return nil, fmt.Errorf("rune deploy: unknown resource %q (queue|kv|object|compute|database|secret|nosql)", kind)
	}
}

// writeArtifact writes the emitted files to outDir (creating it), or prints them to
// out with `# file:` banners when no directory is given. Files are emitted in sorted
// order for determinism.
func writeArtifact(art infra.Artifact, outDir string, out io.Writer) error {
	names := make([]string, 0, len(art.Files))
	for n := range art.Files {
		names = append(names, n)
	}
	sort.Strings(names)
	if outDir == "" {
		for _, n := range names {
			fmt.Fprintf(out, "# %s\n%s", n, art.Files[n])
			if !strings.HasSuffix(art.Files[n], "\n") {
				fmt.Fprintln(out)
			}
		}
		return nil
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return err
	}
	for _, n := range names {
		if err := os.WriteFile(filepath.Join(outDir, n), []byte(art.Files[n]), 0o644); err != nil {
			return err
		}
	}
	fmt.Fprintf(out, "wrote %d file(s) to %s\n", len(names), outDir)
	return nil
}
