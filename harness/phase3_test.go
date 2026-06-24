package harness

import (
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// TestCryptoNative is the Phase-3 crypto gate for the backends that ship NO host
// digest of their own: native C, LLVM, and Rust now get a real, fast sha256 from
// the vendored BearSSL static lib (bin/build-bearssl.sh). Computes sha256("abc")
// = the NIST vector, matching the source backends + the pure-wootz oracle (ch514).
// Skips cleanly when BearSSL is not built.
func TestCryptoNative(t *testing.T) {
	if !bearsslReady() {
		t.Skip("BearSSL not built — run bin/build-bearssl.sh")
	}
	const want = "\"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\"\nunit"
	s := loadListing(t, "ch497_crypto.rune")
	prog, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		if got := runCmdStdin(t, exec.Command(compileC(t, prog)), ""); got != want {
			t.Fatalf("[c] sha256 = %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if got := runCmdStdin(t, exec.Command(compileLL(t, prog)), ""); got != want {
			t.Fatalf("[ll] sha256 = %q, want %q", got, want)
		}
	})
	t.Run("rs", func(t *testing.T) {
		if _, err := exec.LookPath("rustc"); err != nil {
			t.Skip("rustc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.Rust{}.Emit(prog)
		f := filepath.Join(dir, "main.rs")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		out, err := exec.Command("rustc", "--edition", "2021",
			"-L", "native="+filepath.Dir(bearsslLib()), "-l", "static=bearssl",
			"-o", bin, f).CombinedOutput()
		if err != nil {
			t.Fatalf("[rs] compile: %v\n%s", err, out)
		}
		if got := runCmdStdin(t, exec.Command(bin), ""); got != want {
			t.Fatalf("[rs] sha256 = %q, want %q", got, want)
		}
	})
}

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
// on go/py/js/erl/jvm (cert verification skipped); Rust + native C/LLVM excluded
// (no TLS lib, same gap as crypto). erl uses inets httpc, jvm HttpsURLConnection.
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
		if bk.name == "rs" {
			continue // rust TLS goes through the BearSSL shim path below
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
	t.Run("jvm", func(t *testing.T) {
		javac25, java25, ok := findJava25()
		if !ok {
			t.Skip("no JDK 25 (asdf temurin-25)")
		}
		dir := compileJVM(t, "ch500_tls.rune", javac25)
		if got := runCmdStdin(t, exec.Command(java25, "-cp", dir, "main"), port+"\n"); got != want {
			t.Fatalf("[jvm] tls gave %q, want %q", got, want)
		}
	})

	// Native backends (C, LLVM, Rust) via the vendored BearSSL TLS shim — the
	// backends that ship no TLS of their own now reach the same self-signed server.
	if !bearsslReady() {
		t.Run("native", func(t *testing.T) { t.Skip("BearSSL not built — run bin/build-bearssl.sh") })
		return
	}
	s := loadListing(t, "ch500_tls.rune")
	prog, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		if got := runCmdStdin(t, exec.Command(compileC(t, prog)), port+"\n"); got != want {
			t.Fatalf("[c] tls gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if got := runCmdStdin(t, exec.Command(compileLL(t, prog)), port+"\n"); got != want {
			t.Fatalf("[ll] tls gave %q, want %q", got, want)
		}
	})
	t.Run("rs", func(t *testing.T) {
		if _, err := exec.LookPath("rustc"); err != nil {
			t.Skip("rustc not in PATH")
		}
		dir := t.TempDir()
		shimO := filepath.Join(dir, "shim.o")
		if out, err := exec.Command("cc", "-c", "-I", bearsslInc(), bearsslShim(), "-o", shimO).CombinedOutput(); err != nil {
			t.Fatalf("[rs] shim compile: %v\n%s", err, out)
		}
		src, _ := codegen.Rust{}.Emit(prog)
		f := filepath.Join(dir, "main.rs")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		out, err := exec.Command("rustc", "--edition", "2021", "-o", bin, f,
			"-C", "link-arg="+shimO, "-C", "link-arg="+bearsslLib()).CombinedOutput()
		if err != nil {
			t.Fatalf("[rs] compile: %v\n%s", err, out)
		}
		if got := runCmdStdin(t, exec.Command(bin), port+"\n"); got != want {
			t.Fatalf("[rs] tls gave %q, want %q", got, want)
		}
	})
}
