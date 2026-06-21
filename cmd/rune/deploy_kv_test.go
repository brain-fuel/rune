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

// TestKVRunsOnJS is the "it runs" gate for the data plane: a get-after-put on the
// wavelet KV abstraction returns the stored value when run on the JS backend (the
// local in-process binding). Skipped where node is absent.
func TestKVRunsOnJS(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	f, err := os.CreateTemp("", "kv-*.rune")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(f.Name())
	if _, err := f.WriteString(kvProgram); err != nil {
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
	derr := runDeploy([]string{f.Name(), "main", "--target", "js"}, &banner)
	w.Close()
	os.Stdout = old
	got := <-done

	if derr != nil {
		t.Fatalf("kv program on js failed: %v", derr)
	}
	if !strings.Contains(got, "42") {
		t.Errorf("get-after-put did not return the stored value; output:\n%s", got)
	}
}
