package codegen

import (
	"errors"
	"fmt"
)

// BuildKind selects whether a checked Rune program is emitted as a runnable app
// or as reusable host-language artifacts.
type BuildKind string

const (
	BuildApp     BuildKind = "app"
	BuildLibrary BuildKind = "library"
)

// Export names a Rune definition that should be visible at the host-language
// boundary. HostName may be empty, in which case the backend chooses a stable
// target-native spelling from RuneName.
type Export struct {
	RuneName string
	HostName string
}

// BuildSpec is the target-neutral contract for codebase coexistence: the same
// checked program may become a Go package, a Rust crate/module, a JVM package,
// a JS/Python module, a BEAM module, a C library, or a WASM component.
type BuildSpec struct {
	Kind BuildKind
	// Module is the host ecosystem's module/package namespace when the target has one
	// (for example a Go import path, JVM package, Rust crate, Python package, or JS
	// package/module name).
	Module string
	// Exports is meaningful for BuildLibrary. BuildApp still uses Program.Main.
	Exports []Export
}

// ArtifactRole classifies files in an artifact set without baking in one host
// ecosystem's package manager.
type ArtifactRole string

const (
	ArtifactSource   ArtifactRole = "source"
	ArtifactManifest ArtifactRole = "manifest"
	ArtifactHeader   ArtifactRole = "header"
	ArtifactRuntime  ArtifactRole = "runtime"
)

// Artifact is one emitted file.
type Artifact struct {
	Path string
	Role ArtifactRole
	Data []byte
}

// RunHint is a best-effort command shape for app artifacts. It is advisory: host
// build tools may replace it.
type RunHint struct {
	Args []string
}

// LinkHint is a best-effort link/install shape for library artifacts.
type LinkHint struct {
	Args []string
}

// ArtifactSet is the eventual backend contract. A target may emit one source file,
// multiple source files plus a manifest, or binary/module artifacts.
type ArtifactSet struct {
	Target    string
	Kind      BuildKind
	Artifacts []Artifact
	Run       *RunHint
	Link      *LinkHint
}

// LibraryBackend is implemented by targets that can emit reusable library artifacts.
// The basic Backend interface remains app-compatible while this grows target by target.
type LibraryBackend interface {
	Backend
	EmitLibrary(p Program, spec BuildSpec) (ArtifactSet, error)
}

var ErrLibraryUnsupported = errors.New("library artifact emission is not implemented for target")

// Build emits target artifacts. App mode is supported for every existing Backend
// by wrapping Emit. Library mode is target-neutral at the API level and becomes
// available as individual backends implement LibraryBackend.
func Build(b Backend, p Program, spec BuildSpec) (ArtifactSet, error) {
	if b == nil {
		return ArtifactSet{}, fmt.Errorf("nil backend")
	}
	if spec.Kind == "" {
		spec.Kind = BuildApp
	}
	switch spec.Kind {
	case BuildApp:
		src, err := b.Emit(p)
		if err != nil {
			return ArtifactSet{}, err
		}
		target := b.Target()
		return ArtifactSet{
			Target: target,
			Kind:   BuildApp,
			Artifacts: []Artifact{{
				Path: appArtifactPath(target),
				Role: ArtifactSource,
				Data: []byte(src),
			}},
			Run: appRunHint(target),
		}, nil
	case BuildLibrary:
		lb, ok := b.(LibraryBackend)
		if !ok {
			return ArtifactSet{}, fmt.Errorf("%w %q", ErrLibraryUnsupported, b.Target())
		}
		return lb.EmitLibrary(p, spec)
	default:
		return ArtifactSet{}, fmt.Errorf("unknown build kind %q", spec.Kind)
	}
}

func appArtifactPath(target string) string {
	switch target {
	case "js":
		return "main.js"
	case "py":
		return "main.py"
	case "go":
		return "main.go"
	case "rs":
		return "main.rs"
	case "erl":
		return "main.erl"
	case "jvm":
		return "main.java"
	case "c":
		return "main.c"
	case "ll":
		return "main.ll"
	case "wasm":
		return "module.wat"
	default:
		return "main." + target
	}
}

func appRunHint(target string) *RunHint {
	switch target {
	case "js":
		return &RunHint{Args: []string{"node", "main.js"}}
	case "py":
		return &RunHint{Args: []string{"python3", "main.py"}}
	case "go":
		return &RunHint{Args: []string{"go", "run", "main.go"}}
	case "rs":
		return &RunHint{Args: []string{"rustc", "--edition", "2021", "-o", "main", "main.rs"}}
	case "erl":
		return &RunHint{Args: []string{"escript", "main.erl"}}
	case "jvm":
		return &RunHint{Args: []string{"javac", "--release", "25", "main.java"}}
	case "wasm":
		return &RunHint{Args: []string{"wasmtime", "run", "module.wat"}}
	default:
		return nil
	}
}
