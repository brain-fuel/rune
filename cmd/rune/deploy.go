package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"

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
	var resource, name, backend, outDir string
	fifo := false
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
		case a == "--backend", a == "--target":
			backend, err = val()
		case a == "--out":
			outDir, err = val()
		case a == "--fifo":
			fifo = true
		case strings.HasPrefix(a, "--resource="):
			resource = strings.TrimPrefix(a, "--resource=")
		case strings.HasPrefix(a, "--name="):
			name = strings.TrimPrefix(a, "--name=")
		case strings.HasPrefix(a, "--backend="), strings.HasPrefix(a, "--target="):
			backend = a[strings.IndexByte(a, '=')+1:]
		case strings.HasPrefix(a, "--out="):
			outDir = strings.TrimPrefix(a, "--out=")
		default:
			return fmt.Errorf("rune deploy: unexpected argument %q", a)
		}
		if err != nil {
			return err
		}
	}
	if resource == "" || name == "" || backend == "" {
		return fmt.Errorf("rune deploy needs --resource <queue|kv|object> --name <name> --backend <%s>",
			strings.Join(infra.Targets(), "|"))
	}

	r, err := resourceFor(resource, name, fifo)
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

// resourceFor builds an agnostic Resource from the CLI flags.
func resourceFor(kind, name string, fifo bool) (infra.Resource, error) {
	switch kind {
	case "queue":
		return infra.Queue{Name: name, FIFO: fifo}, nil
	case "kv":
		return infra.KV{Name: name}, nil
	case "object":
		return infra.Bucket{Name: name}, nil
	default:
		return nil, fmt.Errorf("rune deploy: unknown resource %q (queue|kv|object)", kind)
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
