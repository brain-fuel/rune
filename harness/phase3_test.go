package harness

import (
	"net"
	"net/http"
	"net/http/httptest"
	"os/exec"
	"testing"
)

// TestCrypto is the Phase-3 crypto gate: host sha256 over Bin (ch497), hex-encoded
// and checked against the NIST vector sha256("abc"). Runs on the backends with a
// built-in sha256 (js/py/go/erl + JVM); Rust + native C/LLVM are excluded (no
// built-in digest, and a pure-wootz sha256 needs the bignum-division accel they lack).
func TestCrypto(t *testing.T) {
	const want = "\"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\"\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		if bk.name == "rs" {
			continue
		}
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch497_crypto.rune", "main", ""); got != want {
				t.Fatalf("[%s] sha256 gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch497_crypto.rune", want) })
}

// TestURL is the Phase-3 net/url gate: percent encode/decode over Bin (ch498),
// pure-wootz, byte-identical on all 8 backends.
func TestURL(t *testing.T) {
	const want = "\"a%20b%2Fc\"\n\"a b/c\"\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch498_url.rune", "main", ""); got != want {
				t.Fatalf("[%s] url gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch498_url.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch498_url.rune", want) })
}

// TestHTTP is the Phase-3 net/http gate: a wootz HTTP/1.0 client (ch499) over the
// Phase-1 sockets GETs from a harness http server returning "hello", parses the 200
// status and the body — byte-identical across all 8 backends (sockets everywhere).
func TestHTTP(t *testing.T) {
	const want = "200\n\"hello\"\nunit"
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("hello"))
	}))
	t.Cleanup(srv.Close)
	_, port, err := net.SplitHostPort(srv.Listener.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch499_http.rune", "main", port+"\n"); got != want {
				t.Fatalf("[%s] http gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch499_http.rune")
	prog, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		if got := runCmdStdin(t, exec.Command(compileC(t, prog)), port+"\n"); got != want {
			t.Fatalf("[c] http gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if got := runCmdStdin(t, exec.Command(compileLL(t, prog)), port+"\n"); got != want {
			t.Fatalf("[ll] http gave %q, want %q", got, want)
		}
	})
	t.Run("jvm", func(t *testing.T) {
		javac25, java25, ok := findJava25()
		if !ok {
			t.Skip("no JDK 25 (asdf temurin-25)")
		}
		dir := compileJVM(t, "ch499_http.rune", javac25)
		if got := runCmdStdin(t, exec.Command(java25, "-cp", dir, "main"), port+"\n"); got != want {
			t.Fatalf("[jvm] http gave %q, want %q", got, want)
		}
	})
}

// TestTLS is the Phase-3 TLS gate: a wootz HTTPS client (ch500, tlsGet) GETs from a
// harness self-signed TLS server returning "secure", via the host TLS stack. Covered
// on go/py/js (cert verification skipped); jvm/erl addable, Rust + native C/LLVM
// excluded (no TLS lib, same gap as crypto).
func TestTLS(t *testing.T) {
	const want = "\"secure\"\nunit"
	srv := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("secure"))
	}))
	t.Cleanup(srv.Close)
	_, port, err := net.SplitHostPort(srv.Listener.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	for _, bk := range ioOSBackends {
		bk := bk
		if bk.name != "go" && bk.name != "py" && bk.name != "js" {
			continue // tls host body only on go/py/js so far
		}
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch500_tls.rune", "main", port+"\n"); got != want {
				t.Fatalf("[%s] tls gave %q, want %q", bk.name, got, want)
			}
		})
	}
}
