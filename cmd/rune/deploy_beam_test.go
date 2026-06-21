package main

import (
	"bytes"
	"io"
	"os"
	"os/exec"
	"strings"
	"testing"
)

// TestDeployBeamRunsVerifiedCvRDT is the Lambert "it runs" gate for `rune deploy`:
// a VERIFIED generic CvRDT replica (ch436, serveG over a G-Counter) deploys and RUNS
// as live gossiping BEAM actors via `rune deploy FILE main --target beam`, converging
// to its certified value. Not a model, not config — a proven protocol, deployed and
// running on a real backend. Skipped where escript is absent.
func TestDeployBeamRunsVerifiedCvRDT(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not in PATH")
	}
	// runTarget writes the program's output to os.Stdout; capture it via a pipe.
	old := os.Stdout
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	os.Stdout = w
	done := make(chan string)
	go func() {
		var buf bytes.Buffer
		io.Copy(&buf, r)
		done <- buf.String()
	}()

	var banner bytes.Buffer
	derr := runDeploy([]string{"../../listings/ch436_generic_replica.rune", "main", "--target", "beam"}, &banner)
	w.Close()
	os.Stdout = old
	got := <-done

	if derr != nil {
		t.Fatalf("deploy on beam failed: %v", derr)
	}
	// The generic replica deployed at a G-Counter converges to 4.
	if !strings.Contains(got, "succ (succ (succ (succ zero)))") {
		t.Errorf("deployed CvRDT did not converge as certified; output:\n%s", got)
	}
}
