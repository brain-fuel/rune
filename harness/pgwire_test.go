package harness

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"
)

// TestPGWire gates the LIVE PostgreSQL wire-protocol driver (ch524): a pure-wootz
// frontend speaking the real v3 protocol over the Phase-1 socket FFI to a running
// PostgreSQL. It sends a StartupMessage (user=postgres, trust auth), drains the auth
// + ParameterStatus + ReadyForQuery burst, sends a simple Query "SELECT 42", and
// parses the backend message stream for the DataRow, extracting the field value. The
// gate spins a postgres:15 container (trust auth), waits for readiness, feeds the
// mapped port on stdin, and asserts the driver returns "42". Run on the fast socket
// backends (js/go); the wire framing is pure Bin arithmetic, identical on all.
func TestPGWire(t *testing.T) {
	if testing.Short() {
		t.Skip("-short skips the docker-backed live Postgres driver")
	}
	if _, err := exec.LookPath("docker"); err != nil {
		t.Skip("docker not in PATH")
	}
	if err := exec.Command("docker", "info").Run(); err != nil {
		t.Skip("docker daemon not available")
	}
	// Pid-unique name: a fixed name lets two concurrent suites (parallel agent
	// sessions, a worktree run beside a checkout run) docker-rm each other's
	// container mid-test, which surfaces as a spurious `gave ""` failure.
	cname := fmt.Sprintf("rune_pgwire_test_%d", os.Getpid())
	_ = exec.Command("docker", "rm", "-f", cname).Run()
	run := exec.Command("docker", "run", "-d", "--name", cname,
		"-e", "POSTGRES_HOST_AUTH_METHOD=trust", "-e", "POSTGRES_USER=postgres",
		"-p", "0:5432", "postgres:15")
	if out, err := run.CombinedOutput(); err != nil {
		t.Skipf("could not start postgres container: %v\n%s", err, out)
	}
	defer exec.Command("docker", "rm", "-f", cname).Run()

	// Resolve the mapped host port.
	var port string
	for i := 0; i < 30; i++ {
		out, err := exec.Command("docker", "port", cname, "5432/tcp").Output()
		if err == nil && len(out) > 0 {
			// e.g. "0.0.0.0:49153\n"
			line := strings.TrimSpace(strings.SplitN(string(out), "\n", 2)[0])
			if i := strings.LastIndex(line, ":"); i >= 0 {
				port = line[i+1:]
			}
		}
		if port != "" {
			break
		}
		time.Sleep(time.Second)
	}
	if port == "" {
		t.Skip("could not resolve postgres mapped port")
	}
	// Wait for the server to accept connections.
	ready := false
	for i := 0; i < 40; i++ {
		if err := exec.Command("docker", "exec", cname, "pg_isready", "-U", "postgres").Run(); err == nil {
			ready = true
			break
		}
		time.Sleep(time.Second)
	}
	if !ready {
		t.Skip("postgres did not become ready in time")
	}

	for _, bk := range ioOSBackends {
		bk := bk
		if bk.name != "js" && bk.name != "go" {
			continue // the wire framing is pure Bin arithmetic; js/go cover it fast
		}
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			got := runIOListing(t, bk, "ch524_pgwire.rune", "main", port+"\n")
			// First line is the DataRow value printBin'd as `"42"`.
			line := strings.SplitN(got, "\n", 2)[0]
			if strings.Trim(line, "\"") != "42" {
				t.Fatalf("[%s] live PG driver gave %q, want \"42\"", bk.name, got)
			}
		})
	}
}
