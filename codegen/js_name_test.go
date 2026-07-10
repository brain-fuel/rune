package codegen

import "testing"

// isJSIdentShape reports whether s looks like a valid (simplified) JS
// identifier: non-empty, and built only from letters, digits, '$' and '_'.
// This is a shape check for the sanitizer's output, not a full ECMAScript
// IdentifierName grammar (no need to accept unicode escapes etc. here).
func isJSIdentShape(s string) bool {
	if s == "" {
		return false
	}
	for _, r := range s {
		switch {
		case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9', r == '$', r == '_':
			// ok
		default:
			return false
		}
	}
	return true
}

// TestJSNameSanitizesDots pins the fix for jsName: a module-qualified rune
// identifier like "Std.Float.fle" contains '.', which is invalid inside a JS
// identifier ("const Std.Float.fle = ..." is a syntax error). jsName must
// produce a dot-free, letters/digits/$/_-only name, and distinct dotted names
// must not collapse onto the same sanitized identifier.
func TestJSNameSanitizesDots(t *testing.T) {
	got := jsName("Std.Float.fle")
	if got == "" {
		t.Fatalf("jsName(%q) returned empty string", "Std.Float.fle")
	}
	for _, r := range got {
		if r == '.' {
			t.Fatalf("jsName(%q) = %q still contains a dot", "Std.Float.fle", got)
		}
	}
	if !isJSIdentShape(got) {
		t.Fatalf("jsName(%q) = %q is not a valid-shape JS identifier", "Std.Float.fle", got)
	}

	a := jsName("A.b")
	b := jsName("A.c")
	if a == b {
		t.Fatalf("jsName(\"A.b\") and jsName(\"A.c\") both mangled to %q; distinct dotted names must stay distinct", a)
	}
}

// TestJSNamePlainIdentifiersUnaffected guards against a regression from the
// dot-sanitizing fix: ordinary (non-dotted, non-reserved) names must still
// map to themselves.
func TestJSNamePlainIdentifiersUnaffected(t *testing.T) {
	for _, n := range []string{"foo", "bar123", "x", "_y", "myVar"} {
		if got := jsName(n); got != n {
			t.Fatalf("jsName(%q) = %q, want unchanged %q", n, got, n)
		}
	}
}
