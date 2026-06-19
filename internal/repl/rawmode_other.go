//go:build !linux

package repl

// On non-Linux platforms the hand-rolled raw-mode editor is not built; the REPL
// uses the bufio fallback. (The project targets Linux/WSL; other platforms can be
// added with their termios ioctl constants — see rawmode_linux.go.)

type rawState struct{}

func isTerminal(uintptr) bool { return false }

func enterRaw(uintptr) (*rawState, error) { return nil, errNoEditor }

func restoreRaw(uintptr, *rawState) error { return nil }
