//go:build linux

package repl

import "golang.org/x/sys/unix"

// rawState holds the terminal settings saved on entering raw mode, restored on exit.
type rawState struct {
	termios unix.Termios
}

// isTerminal reports whether fd is a terminal (a TCGETS ioctl succeeds).
func isTerminal(fd uintptr) bool {
	_, err := unix.IoctlGetTermios(int(fd), unix.TCGETS)
	return err == nil
}

// enterRaw puts fd into raw mode (no echo, no canonical line buffering, no signal
// generation, no output post-processing) and returns the prior settings so the
// caller can restore them. The REPL editor owns every byte while editing; results
// are printed only after restoreRaw, in cooked mode.
func enterRaw(fd uintptr) (*rawState, error) {
	t, err := unix.IoctlGetTermios(int(fd), unix.TCGETS)
	if err != nil {
		return nil, err
	}
	saved := *t
	raw := *t
	raw.Iflag &^= unix.IGNBRK | unix.BRKINT | unix.PARMRK | unix.ISTRIP |
		unix.INLCR | unix.IGNCR | unix.ICRNL | unix.IXON
	raw.Oflag &^= unix.OPOST
	raw.Lflag &^= unix.ECHO | unix.ECHONL | unix.ICANON | unix.ISIG | unix.IEXTEN
	raw.Cflag &^= unix.CSIZE | unix.PARENB
	raw.Cflag |= unix.CS8
	raw.Cc[unix.VMIN] = 1
	raw.Cc[unix.VTIME] = 0
	if err := unix.IoctlSetTermios(int(fd), unix.TCSETS, &raw); err != nil {
		return nil, err
	}
	return &rawState{termios: saved}, nil
}

// restoreRaw restores the terminal settings captured by enterRaw.
func restoreRaw(fd uintptr, st *rawState) error {
	if st == nil {
		return nil
	}
	return unix.IoctlSetTermios(int(fd), unix.TCSETS, &st.termios)
}
