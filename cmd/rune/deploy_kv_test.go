package main

import (
	"bytes"
	"io"
	"os"
	"os/exec"
	"strings"
	"testing"
)

// dataPlaneBackends are the source targets with an in-process data-plane binding +
// their runtime binary.
var dataPlaneBackends = []struct{ target, bin string }{
	{"js", "node"}, {"py", "python3"}, {"go", "go"}, {"erl", "escript"},
}

// runProgramCapturing runs a source program's `main` on a target, capturing stdout.
func runProgramCapturing(t *testing.T, src, target string) string {
	t.Helper()
	f, err := os.CreateTemp("", "wavelet-*.rune")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(f.Name())
	if _, err := f.WriteString(src); err != nil {
		t.Fatal(err)
	}
	f.Close()

	old := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w
	done := make(chan string)
	go func() {
		var buf bytes.Buffer
		io.Copy(&buf, r)
		done <- buf.String()
	}()
	var banner bytes.Buffer
	derr := runDeploy([]string{f.Name(), "main", "--target", target}, &banner)
	w.Close()
	os.Stdout = old
	got := <-done
	if derr != nil {
		t.Fatalf("[%s] run failed: %v", target, derr)
	}
	return got
}

// readExample reads a committed example program.
func readExample(t *testing.T, name string) string {
	t.Helper()
	b, err := os.ReadFile("../../examples/" + name)
	if err != nil {
		t.Fatalf("read example %s: %v", name, err)
	}
	return string(b)
}

// TestDataPlaneRunsCrossBackend is the "it runs" gate for ALL THREE data-plane
// abstractions on every source backend with a local in-process binding (js/py/go/erl).
// It runs the COMMITTED examples (kv_demo / object_demo / queue_demo), so those
// artifacts are gated cross-backend. Each is skipped where its runtime is absent.
func TestDataPlaneRunsCrossBackend(t *testing.T) {
	kv := readExample(t, "kv_demo.rune")
	obj := readExample(t, "object_demo.rune")
	q := readExample(t, "queue_demo.rune")
	for _, bk := range dataPlaneBackends {
		t.Run(bk.target, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runProgramCapturing(t, kv, bk.target); !strings.Contains(got, "42") {
				t.Errorf("[%s] kv get-after-put != 42:\n%s", bk.target, got)
			}
			if got := runProgramCapturing(t, obj, bk.target); !strings.Contains(got, "99") {
				t.Errorf("[%s] object get-after-put != 99:\n%s", bk.target, got)
			}
			got := runProgramCapturing(t, q, bk.target)
			i7, i8 := strings.Index(got, "7"), strings.Index(got, "8")
			if i7 < 0 || i8 < 0 || i7 > i8 {
				t.Errorf("[%s] queue not FIFO (7 before 8):\n%s", bk.target, got)
			}
		})
	}
}

// TestKVStringDemo gates examples/kv_string_demo.rune: a real String ("wootz") put
// under a String key round-trips through the data plane on every present backend.
func TestKVStringDemo(t *testing.T) {
	src, err := os.ReadFile("../../examples/kv_string_demo.rune")
	if err != nil {
		t.Fatalf("read kv_string_demo.rune: %v", err)
	}
	for _, bk := range dataPlaneBackends {
		t.Run(bk.target, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runProgramCapturing(t, string(src), bk.target); !strings.Contains(got, "wootz") {
				t.Errorf("[%s] String round-trip did not return \"wootz\":\n%s", bk.target, got)
			}
		})
	}
}
