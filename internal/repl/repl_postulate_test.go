package repl

import (
	"bytes"
	"strings"
	"testing"
)

func TestReplAcceptsPostulate(t *testing.T) {
	script := []string{
		`postulate inRegion : U because "not yet modeled" end`,
		`:type inRegion`,
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "defined inRegion") && !strings.Contains(got, "inRegion") {
		t.Fatalf("repl did not accept postulate:\n%s", got)
	}
}
