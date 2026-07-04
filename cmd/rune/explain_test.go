package main

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"
)

// TestParseExplainArgs pins the flag surface: positional paths + [name],
// --depth n|core, --core, --no-prelude; name defaults to main.
func TestParseExplainArgs(t *testing.T) {
	ea, err := parseExplainArgs([]string{"../../examples/double.rune", "main", "--depth", "2"})
	if err != nil {
		t.Fatal(err)
	}
	if len(ea.files) != 1 || ea.main != "main" || ea.opts.Depth != 2 || ea.opts.Core {
		t.Errorf("got %+v", ea)
	}
	ea, err = parseExplainArgs([]string{"../../examples/double.rune", "--core"})
	if err != nil {
		t.Fatal(err)
	}
	if ea.main != "main" || !ea.opts.Core {
		t.Errorf("got %+v, want default main + core", ea)
	}
	if _, err := parseExplainArgs([]string{"../../examples/double.rune", "--depth", "banana"}); err == nil {
		t.Error("want error for --depth banana")
	}
	if _, err := parseExplainArgs(nil); err == nil {
		t.Error("want error for no files")
	}
}

// TestExplainGoldens locks the phrasing (spec acceptance items 1 and 2):
// depth 0 / depth 1 / core over the double demo and listings ch566-ch568.
func TestExplainGoldens(t *testing.T) {
	cases := []struct {
		name string
		args []string
	}{
		{"double_depth0", []string{"../../examples/double.rune", "main"}},
		{"double_depth1", []string{"../../examples/double.rune", "main", "--depth", "1"}},
		{"double_core", []string{"../../examples/double.rune", "main", "--core"}},
		{"ch566_depth0", []string{"../../listings/ch566_float_io.rune", "main"}},
		{"ch566_depth1", []string{"../../listings/ch566_float_io.rune", "main", "--depth", "1"}},
		{"ch566_core", []string{"../../listings/ch566_float_io.rune", "main", "--core"}},
		{"ch567_depth0", []string{"../../listings/ch567_float_format.rune", "main"}},
		{"ch568_depth0", []string{"../../listings/ch568_dotted_foreign_run.rune", "main"}},
		{"ch568_core", []string{"../../listings/ch568_dotted_foreign_run.rune", "main", "--core"}},
		{"double_annotate_wide", []string{"../../examples/double.rune", "main", "--annotate", "--width", "120"}},
		{"double_annotate_narrow", []string{"../../examples/double.rune", "main", "--annotate", "--width", "60"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			var buf bytes.Buffer
			if err := runExplainCLI(tc.args, &buf); err != nil {
				t.Fatalf("runExplainCLI: %v", err)
			}
			want, err := os.ReadFile(filepath.Join("testdata", "explain", tc.name+".golden"))
			if err != nil {
				t.Fatal(err)
			}
			if buf.String() != string(want) {
				t.Errorf("golden mismatch:\ngot:\n%s\nwant:\n%s", buf.String(), want)
			}
		})
	}
}
