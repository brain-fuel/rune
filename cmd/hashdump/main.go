// Command hashdump elaborates every listing (and example) through the
// current kernel and prints one line per definition: file, name, content
// hash — sorted, deterministic. It is the hash-stability harness: run it
// before and after a kernel change and diff the outputs. The content
// hash format (core.hashFormatVersion) is SACRED; any diff here is a
// kernel regression, not noise.
//
// Usage: go run ./cmd/hashdump [dirs...]   (default: listings examples)
package main

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"goforge.dev/rune/v3/internal/session"
)

func main() {
	dirs := os.Args[1:]
	if len(dirs) == 0 {
		dirs = []string{"listings", "examples"}
	}
	var lines []string
	for _, dir := range dirs {
		entries, err := os.ReadDir(dir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "hashdump: %v\n", err)
			os.Exit(1)
		}
		for _, e := range entries {
			if e.IsDir() || !strings.HasSuffix(e.Name(), ".rune") {
				continue
			}
			path := filepath.Join(dir, e.Name())
			src, err := os.ReadFile(path)
			if err != nil {
				fmt.Fprintf(os.Stderr, "hashdump: %v\n", err)
				os.Exit(1)
			}
			s := session.New()
			if _, err := s.LoadSource(string(src)); err != nil {
				// Load failures are part of the fingerprint: a kernel
				// change that breaks a listing must show in the diff.
				lines = append(lines, fmt.Sprintf("%s\tLOAD-ERROR\t%s", path, firstLine(err.Error())))
				continue
			}
			for h, name := range s.RefNames() {
				lines = append(lines, fmt.Sprintf("%s\t%s\t%s", path, name, h.String()))
			}
		}
	}
	sort.Strings(lines)
	for _, l := range lines {
		fmt.Println(l)
	}
}

func firstLine(s string) string {
	if i := strings.IndexByte(s, '\n'); i >= 0 {
		return s[:i]
	}
	return s
}
