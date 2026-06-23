package harness

import (
	"bytes"
	"io"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"goforge.dev/rune/v3/codegen"
)

// TestFS is the Phase-1 expanded-OS gate: the Bin-native filesystem layer
// (fsWrite/fsRead/fsExists/fsRemove, ch490) writes a file with an embedded NUL,
// reads it back, and removes it — byte-identically across the source backends, run
// in a temp cwd. Subsumes the common syscall fs surface over the real byte carrier.
func TestFS(t *testing.T) {
	const want = "8\n1\n\"hi\\x00bytes\"\n1\n0\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			cmd := buildIOListingCmd(t, bk, "ch490_fs.rune")
			cmd.Dir = t.TempDir()
			out, err := cmd.Output()
			if err != nil {
				stderr := ""
				if ee, ok := err.(*exec.ExitError); ok {
					stderr = string(ee.Stderr)
				}
				t.Fatalf("[%s] run failed: %v\n%s", bk.name, err, stderr)
			}
			if got := strings.TrimSpace(string(out)); got != want {
				t.Fatalf("[%s] fs gave %q, want %q", bk.name, got, want)
			}
		})
	}
}

const fsWant = "8\n1\n\"hi\\x00bytes\"\n1\n0\nunit"

// runInDir runs cmd with cwd=dir (no stdin) and returns trimmed stdout.
func runInDir(t *testing.T, cmd *exec.Cmd, dir string) string {
	t.Helper()
	cmd.Dir = dir
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("run failed: %v", err)
	}
	return strings.TrimSpace(string(out))
}

// TestFSNative + TestFSJVM carry the expanded OS/filesystem layer to the native
// (C/LLVM, POSIX stdio+stat) and JVM (java.nio.file) backends. Run in a temp cwd.
func TestFSNative(t *testing.T) {
	s := loadListing(t, "ch490_fs.rune")
	prog, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		bin := compileC(t, prog)
		if got := runInDir(t, exec.Command(bin), t.TempDir()); got != fsWant {
			t.Fatalf("[c] fs gave %q, want %q", got, fsWant)
		}
	})
	t.Run("ll", func(t *testing.T) {
		bin := compileLL(t, prog)
		if got := runInDir(t, exec.Command(bin), t.TempDir()); got != fsWant {
			t.Fatalf("[ll] fs gave %q, want %q", got, fsWant)
		}
	})
}

func TestFSJVM(t *testing.T) {
	javac25, java25, ok := findJava25()
	if !ok {
		t.Skip("no JDK 25 (asdf temurin-25)")
	}
	dir := compileJVM(t, "ch490_fs.rune", javac25)
	if got := runInDir(t, exec.Command(java25, "-cp", dir, "main"), t.TempDir()); got != fsWant {
		t.Fatalf("[jvm] fs gave %q, want %q", got, fsWant)
	}
}

// buildIOListingCmd emits `listing` on `bk` (compiling if needed) and returns an
// *exec.Cmd ready to run (stdin/stdout not yet set). Used where the harness must
// interleave with the running program (e.g. connect to a rune TCP server).
func buildIOListingCmd(t *testing.T, bk ioBackend, listing string) *exec.Cmd {
	t.Helper()
	s := loadListing(t, listing)
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := bk.emit(p)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "main."+bk.ext)
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	runFile := f
	if bk.compile != nil {
		bin := filepath.Join(dir, "main.bin")
		if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
			t.Fatalf("[%s] compile: %v\n%s", bk.name, err, out)
		}
		runFile = bin
	}
	return bk.run(runFile)
}

// TestNetServer is the Phase-1 server-side gate: a rune TCP echo server (ch489,
// sockListen/sockAccept) accepts a harness client, reads "ping", replies "pong", and
// prints what it read — across the source backends. The harness picks a free port,
// starts the server (port on stdin), connect-retries until it binds, exchanges, then
// checks both the client's "pong" and the server's "ping"\nunit stdout.
func TestNetServer(t *testing.T) {
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			serverRoundTrip(t, bk.name, func() *exec.Cmd { return buildIOListingCmd(t, bk, "ch489_net_server.rune") })
		})
	}
}

// TestNetServerNative + TestNetServerJVM carry the server-side socket FFI to the
// native (C/LLVM, POSIX bind/listen/accept) and JVM (ServerSocket) backends.
func TestNetServerNative(t *testing.T) {
	s := loadListing(t, "ch489_net_server.rune")
	prog, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		bin := compileC(t, prog)
		serverRoundTrip(t, "c", func() *exec.Cmd { return exec.Command(bin) })
	})
	t.Run("ll", func(t *testing.T) {
		bin := compileLL(t, prog)
		serverRoundTrip(t, "ll", func() *exec.Cmd { return exec.Command(bin) })
	})
}

func TestNetServerJVM(t *testing.T) {
	javac25, java25, ok := findJava25()
	if !ok {
		t.Skip("no JDK 25 (asdf temurin-25)")
	}
	dir := compileJVM(t, "ch489_net_server.rune", javac25)
	serverRoundTrip(t, "jvm", func() *exec.Cmd { return exec.Command(java25, "-cp", dir, "main") })
}

// serverRoundTrip picks a free port, starts a rune TCP server (newCmd, port on stdin),
// connect-retries until it binds, sends "ping", expects "pong", and checks the
// server's "ping"\nunit stdout. newCmd must return a fresh runnable command.
func serverRoundTrip(t *testing.T, name string, newCmd func() *exec.Cmd) {
	t.Helper()
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	_, port, _ := net.SplitHostPort(l.Addr().String())
	l.Close() // free it for the rune server to bind (tiny race, acceptable)

	cmd := newCmd()
	cmd.Stdin = strings.NewReader(port + "\n")
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	var conn net.Conn
	for i := 0; i < 100; i++ {
		conn, err = net.Dial("tcp", "127.0.0.1:"+port)
		if err == nil {
			break
		}
		time.Sleep(50 * time.Millisecond)
	}
	if conn == nil {
		cmd.Process.Kill()
		cmd.Wait()
		t.Fatalf("[%s] could not connect to rune server on %s", name, port)
	}
	conn.Write([]byte("ping"))
	buf := make([]byte, 4)
	io.ReadFull(conn, buf)
	conn.Close()
	cmd.Wait()
	if string(buf) != "pong" {
		t.Errorf("[%s] client got %q from server, want \"pong\"", name, string(buf))
	}
	if got := strings.TrimSpace(out.String()); got != netEchoWant {
		t.Errorf("[%s] server stdout = %q, want %q", name, got, netEchoWant)
	}
}

// compileC/compileLL/compileJVM emit `prog`/`listing` and compile to a runnable
// artifact, skipping if the toolchain is absent. Shared by the native/JVM server +
// fs round-trip tests.
func compileC(t *testing.T, prog codegen.Program) string {
	t.Helper()
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skip("cc not in PATH")
	}
	dir := t.TempDir()
	src, _ := codegen.C{}.Emit(prog)
	f := filepath.Join(dir, "main.c")
	bin := filepath.Join(dir, "main.bin")
	os.WriteFile(f, []byte(src), 0o644)
	if out, err := exec.Command("cc", "-o", bin, f).CombinedOutput(); err != nil {
		t.Fatalf("[c] compile: %v\n%s", err, out)
	}
	return bin
}

func compileLL(t *testing.T, prog codegen.Program) string {
	t.Helper()
	if _, err := exec.LookPath("clang"); err != nil {
		t.Skip("clang not in PATH")
	}
	dir := t.TempDir()
	ll, _ := codegen.LL{}.Emit(prog)
	rt := codegen.LL{}.EmitRuntimeFor(prog)
	f := filepath.Join(dir, "main.ll")
	rtf := filepath.Join(dir, "runtime.c")
	bin := filepath.Join(dir, "main.bin")
	os.WriteFile(f, []byte(ll), 0o644)
	os.WriteFile(rtf, []byte(rt), 0o644)
	if out, err := exec.Command("clang", f, rtf, "-o", bin).CombinedOutput(); err != nil {
		t.Fatalf("[ll] compile: %v\n%s", err, out)
	}
	return bin
}

func compileJVM(t *testing.T, listing, javac25 string) string {
	t.Helper()
	s := loadListing(t, listing)
	prog, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.JVM{}.Emit(prog)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "main.java")
	os.WriteFile(f, []byte(src), 0o644)
	if out, err := exec.Command(javac25, "--release", "25", "-d", dir, f).CombinedOutput(); err != nil {
		t.Fatalf("javac: %v\n%s\n--- emitted ---\n%s", err, out, src)
	}
	return dir
}

const netEchoWant = "\"ping\"\nunit"

// runCmdStdin runs cmd feeding `stdin` and returns trimmed stdout.
func runCmdStdin(t *testing.T, cmd *exec.Cmd, stdin string) string {
	t.Helper()
	cmd.Stdin = strings.NewReader(stdin)
	out, err := cmd.Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("run failed: %v\n%s", err, stderr)
	}
	return strings.TrimSpace(string(out))
}

// startEchoServer starts a loopback TCP echo server on an ephemeral port and returns
// the port string. It echoes whatever it reads until the peer closes. The server is
// owned by the test (no docker, no external dependency), so TestNetEcho is
// deterministic — the non-flaky-network pattern of TestLiveKVRoundTrip, minus Valkey.
func startEchoServer(t *testing.T) string {
	t.Helper()
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { ln.Close() })
	go func() {
		for {
			c, err := ln.Accept()
			if err != nil {
				return
			}
			go func(c net.Conn) {
				defer c.Close()
				buf := make([]byte, 1024)
				for {
					n, e := c.Read(buf)
					if n > 0 {
						c.Write(buf[:n])
					}
					if e != nil {
						return
					}
				}
			}(c)
		}
	}()
	_, port, err := net.SplitHostPort(ln.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	return port
}

// TestNetEcho is the Phase-1 socket-FFI gate: a TCP echo client (ch488) over the
// uniform sockConnect/sockWrite/sockRead/sockClose vocabulary round-trips "ping"
// against a harness-owned loopback echo server, byte-identically across backends.
// The port is fed on stdin (getNat), avoiding any packed-string env coupling.
func TestNetEcho(t *testing.T) {
	port := startEchoServer(t)
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch488_net_echo.rune", "main", port+"\n"); got != netEchoWant {
				t.Fatalf("[%s] net echo gave %q, want %q", bk.name, got, netEchoWant)
			}
		})
	}
}

// TestNetEchoNative carries the socket FFI to the native backends: the echo client
// runs on C (POSIX sockets, fd-in-nat handle, self-contained) and LLVM-IR (same in
// the linked runtime.c, clang), round-tripping "ping" against the harness server.
func TestNetEchoNative(t *testing.T) {
	port := startEchoServer(t)
	s := loadListing(t, "ch488_net_echo.rune")
	prog, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		if _, err := exec.LookPath("cc"); err != nil {
			t.Skip("cc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.C{}.Emit(prog)
		f := filepath.Join(dir, "main.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		if out, err := exec.Command("cc", "-o", bin, f).CombinedOutput(); err != nil {
			t.Fatalf("[c] compile: %v\n%s", err, out)
		}
		if got := runCmdStdin(t, exec.Command(bin), port+"\n"); got != netEchoWant {
			t.Fatalf("[c] net echo gave %q, want %q", got, netEchoWant)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if _, err := exec.LookPath("clang"); err != nil {
			t.Skip("clang not in PATH")
		}
		dir := t.TempDir()
		ll, _ := codegen.LL{}.Emit(prog)
		rt := codegen.LL{}.EmitRuntimeFor(prog)
		f := filepath.Join(dir, "main.ll")
		rtf := filepath.Join(dir, "runtime.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(ll), 0o644)
		os.WriteFile(rtf, []byte(rt), 0o644)
		if out, err := exec.Command("clang", f, rtf, "-o", bin).CombinedOutput(); err != nil {
			t.Fatalf("[ll] compile: %v\n%s", err, out)
		}
		if got := runCmdStdin(t, exec.Command(bin), port+"\n"); got != netEchoWant {
			t.Fatalf("[ll] net echo gave %q, want %q", got, netEchoWant)
		}
	})
}

// TestNetEchoJVM carries the socket FFI to the JVM backend (java.net + a handle
// table). Needs Java 25.
func TestNetEchoJVM(t *testing.T) {
	javac25, java25, ok := findJava25()
	if !ok {
		t.Skip("no JDK 25 (asdf temurin-25)")
	}
	port := startEchoServer(t)
	s := loadListing(t, "ch488_net_echo.rune")
	prog, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.JVM{}.Emit(prog)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "main.java")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command(javac25, "--release", "25", "-d", dir, f).CombinedOutput(); err != nil {
		t.Fatalf("javac: %v\n%s\n--- emitted ---\n%s", err, out, src)
	}
	if got := runCmdStdin(t, exec.Command(java25, "-cp", dir, "main"), port+"\n"); got != netEchoWant {
		t.Fatalf("[jvm] net echo gave %q, want %q", got, netEchoWant)
	}
}
