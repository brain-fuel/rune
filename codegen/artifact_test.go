package codegen_test

import (
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

func TestBuildAppArtifactAllBackends(t *testing.T) {
	p := codegen.Program{
		Defs: []codegen.DefSpec{{Name: "main", Body: codegen.IUnit{}}},
		Main: "main",
	}
	for _, bk := range codegen.All() {
		bk := bk
		t.Run(bk.Target(), func(t *testing.T) {
			set, err := codegen.Build(bk, p, codegen.BuildSpec{})
			if err != nil {
				t.Fatalf("Build app: %v", err)
			}
			if set.Target != bk.Target() {
				t.Fatalf("target = %q, want %q", set.Target, bk.Target())
			}
			if set.Kind != codegen.BuildApp {
				t.Fatalf("kind = %q, want %q", set.Kind, codegen.BuildApp)
			}
			if len(set.Artifacts) != 1 {
				t.Fatalf("artifacts = %d, want 1", len(set.Artifacts))
			}
			a := set.Artifacts[0]
			if a.Path == "" {
				t.Fatalf("artifact path is empty")
			}
			if a.Role != codegen.ArtifactSource {
				t.Fatalf("artifact role = %q, want %q", a.Role, codegen.ArtifactSource)
			}
			if len(a.Data) == 0 {
				t.Fatalf("artifact data is empty")
			}
		})
	}
}

func TestBuildLibraryUsesSharedUnsupportedError(t *testing.T) {
	_, err := codegen.Build(codegen.JS{}, codegen.Program{}, codegen.BuildSpec{
		Kind:    codegen.BuildLibrary,
		Module:  "@acme/furnacecore",
		Exports: []codegen.Export{{RuneName: "Furnace.parse", HostName: "Parse"}},
	})
	if !errors.Is(err, codegen.ErrLibraryUnsupported) {
		t.Fatalf("error = %v, want ErrLibraryUnsupported", err)
	}
}

func TestBuildRejectsUnknownKind(t *testing.T) {
	_, err := codegen.Build(codegen.JS{}, codegen.Program{}, codegen.BuildSpec{Kind: "plugin"})
	if err == nil {
		t.Fatal("expected error")
	}
}

func TestBuildGoLibraryArtifactCompiles(t *testing.T) {
	p := codegen.Program{
		Defs: []codegen.DefSpec{{
			Name:  "id",
			Body:  codegen.ILam{Name: "x", Body: codegen.IVar{Idx: 0}},
			Arity: 1,
		}},
	}
	set, err := codegen.Build(codegen.Go{}, p, codegen.BuildSpec{
		Kind:    codegen.BuildLibrary,
		Module:  "example.com/acme/furnacecore",
		Exports: []codegen.Export{{RuneName: "id", HostName: "Identity"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	if set.Kind != codegen.BuildLibrary || set.Target != "go" {
		t.Fatalf("unexpected set metadata: %+v", set)
	}
	if len(set.Artifacts) != 1 || set.Artifacts[0].Path != "furnacecore.go" {
		t.Fatalf("artifacts = %+v", set.Artifacts)
	}
	src := string(set.Artifacts[0].Data)
	for _, want := range []string{"package furnacecore", "func Identity(args ...any) any", "func Show(v any) string"} {
		if !strings.Contains(src, want) {
			t.Fatalf("go library missing %q:\n%s", want, src)
		}
	}
	if strings.Contains(src, "func main()") {
		t.Fatalf("go library should not emit func main:\n%s", src)
	}

	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "go.mod"), []byte("module example.com/acme/furnacecore\n\ngo 1.24\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, set.Artifacts[0].Path), set.Artifacts[0].Data, 0o644); err != nil {
		t.Fatal(err)
	}
	testSrc := `package furnacecore

import "testing"

func TestIdentity(t *testing.T) {
	if got := Identity(42); got != 42 {
		t.Fatalf("Identity(42) = %#v", got)
	}
	if got := Show(nil); got != "()" {
		t.Fatalf("Show(nil) = %q", got)
	}
}
`
	if err := os.WriteFile(filepath.Join(dir, "furnacecore_test.go"), []byte(testSrc), 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("go", "test", "./...")
	cmd.Dir = dir
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("generated Go library did not compile: %v\n%s", err, out)
	}
}
