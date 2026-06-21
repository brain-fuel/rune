package main

import (
	"bytes"
	"io"
	"os"
	"os/exec"
	"strings"
	"testing"
)

// kvProgram is a self-contained program over the wavelet KV data plane: it puts a
// value under a key and reads it back. With the in-process JS backend bound
// (codegen/js.go), it RUNS unaided — the storage abstraction is not just config.
const kvProgram = `data Unit : U is unit : Unit end
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
foreign kvPutCode : Nat -> Nat -> IO Nat end
foreign kvGetCode : Nat -> IO Nat end
foreign printNat  : Nat -> IO Nat end
main : IO Nat is
  bindIO Nat Nat (kvPutCode 1 42) (fn (a : Nat) is
    bindIO Nat Nat (kvGetCode 1) (fn (v : Nat) is
      printNat v
    end)
  end)
end`

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

// TestKVRunsCrossBackend is the "it runs" gate for the data plane: a get-after-put on
// the wavelet KV abstraction returns the stored value on every source backend with a
// local in-process binding (js/py/go/erl). Each is skipped where its runtime is absent.
func TestKVRunsCrossBackend(t *testing.T) {
	for _, bk := range dataPlaneBackends {
		t.Run(bk.target, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			got := runProgramCapturing(t, kvProgram, bk.target)
			if !strings.Contains(got, "42") {
				t.Errorf("[%s] get-after-put did not return 42; output:\n%s", bk.target, got)
			}
		})
	}
}
