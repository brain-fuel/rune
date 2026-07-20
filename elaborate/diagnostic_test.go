package elaborate

import "testing"

func TestSuggestDidYouMean(t *testing.T) {
	cands := []string{"strHead", "strTail", "strApp", "codeOf", "Bytes", "bytes"}
	cases := []struct {
		typo string
		want string // the first suggestion, or "" for none
	}{
		{"strHed", "strHead"}, // one deletion
		{"codeof", "codeOf"},  // case slip
		{"byts", "Bytes"},     // tie broken alphabetically (Bytes < bytes)
		{"strApp", ""},        // exact name is not a suggestion for itself
		{"zzzqqqxxx", ""},     // nothing close
	}
	for _, c := range cases {
		got := suggest(c.typo, cands)
		if c.want == "" {
			if len(got) != 0 {
				t.Errorf("suggest(%q): want none, got %v", c.typo, got)
			}
			continue
		}
		if len(got) == 0 || got[0] != c.want {
			t.Errorf("suggest(%q): want first=%q, got %v", c.typo, c.want, got)
		}
	}
}

func TestDiagnosticRenders(t *testing.T) {
	d := &Diagnostic{
		Summary: "I can't find `foo` in scope.",
		Body:    []string{"You used `foo` here, but nothing with that name is in scope."},
		Hints:   []string{"Did you mean `for`?"},
	}
	got := d.Error()
	for _, want := range []string{
		"I can't find `foo` in scope.",
		"nothing with that name is in scope",
		"help: Did you mean `for`?",
	} {
		if !contains(got, want) {
			t.Errorf("rendered diagnostic missing %q\n--- got ---\n%s", want, got)
		}
	}
}

func TestOrList(t *testing.T) {
	cases := []struct {
		in   []string
		want string
	}{
		{nil, ""},
		{[]string{"a"}, "`a`"},
		{[]string{"a", "b"}, "`a` or `b`"},
		{[]string{"a", "b", "c"}, "`a`, `b`, or `c`"},
	}
	for _, c := range cases {
		if got := orList(append([]string(nil), c.in...)); got != c.want {
			t.Errorf("orList(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}

func contains(haystack, needle string) bool {
	return len(needle) == 0 || (len(haystack) >= len(needle) && indexOf(haystack, needle) >= 0)
}

func indexOf(s, sub string) int {
	for i := 0; i+len(sub) <= len(s); i++ {
		if s[i:i+len(sub)] == sub {
			return i
		}
	}
	return -1
}
