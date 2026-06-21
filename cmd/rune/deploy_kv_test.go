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

// objectProgram puts a blob under a key and reads it back (the object data plane).
const objectProgram = `data Unit : U is unit : Unit end
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
foreign objPutCode : Nat -> Nat -> IO Nat end
foreign objGetCode : Nat -> IO Nat end
foreign printNat   : Nat -> IO Nat end
main : IO Nat is
  bindIO Nat Nat (objPutCode 5 99) (fn (a : Nat) is
    bindIO Nat Nat (objGetCode 5) (fn (v : Nat) is printNat v end)
  end)
end`

// queueProgram enqueues two messages then dequeues them, checking FIFO order.
const queueProgram = `data Unit : U is unit : Unit end
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
foreign enqueueCode : Nat -> Nat -> IO Nat end
foreign dequeueCode : Nat -> IO Nat end
foreign printNat    : Nat -> IO Nat end
main : IO Nat is
  bindIO Nat Nat (enqueueCode 1 7) (fn (a : Nat) is
    bindIO Nat Nat (enqueueCode 1 8) (fn (b : Nat) is
      bindIO Nat Nat (dequeueCode 1) (fn (x : Nat) is
        bindIO Nat Nat (printNat x) (fn (p : Nat) is
          bindIO Nat Nat (dequeueCode 1) (fn (y : Nat) is printNat y end)
        end)
      end)
    end)
  end)
end`

// TestDataPlaneRunsCrossBackend is the "it runs" gate for ALL THREE data-plane
// abstractions (kv / object / queue) on every source backend with a local in-process
// binding (js/py/go/erl): a get-after-put returns the value, and a queue preserves
// FIFO order. Each is skipped where its runtime is absent.
func TestDataPlaneRunsCrossBackend(t *testing.T) {
	for _, bk := range dataPlaneBackends {
		t.Run(bk.target, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runProgramCapturing(t, kvProgram, bk.target); !strings.Contains(got, "42") {
				t.Errorf("[%s] kv get-after-put != 42:\n%s", bk.target, got)
			}
			if got := runProgramCapturing(t, objectProgram, bk.target); !strings.Contains(got, "99") {
				t.Errorf("[%s] object get-after-put != 99:\n%s", bk.target, got)
			}
			got := runProgramCapturing(t, queueProgram, bk.target)
			i7, i8 := strings.Index(got, "7"), strings.Index(got, "8")
			if i7 < 0 || i8 < 0 || i7 > i8 {
				t.Errorf("[%s] queue not FIFO (7 before 8):\n%s", bk.target, got)
			}
		})
	}
}
