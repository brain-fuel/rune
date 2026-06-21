package surface

import (
	"strings"
	"testing"
)

// a protocol body with every required member present (stub bodies — the parser only
// checks PRESENCE; elaboration checks types).
const protoComplete = `protocol GCounter is
  init : Nat is zero end
  merge : Nat is zero end
  value : Nat is zero end
  op0 : Nat is zero end
  mergeComm : Nat is zero end
  mergeIdem : Nat is zero end
  mergeAssoc : Nat is zero end
end`

// TestProtocolComplete: a block with the full CvRDT contract parses, and its members
// pass through as ordinary top-level items (bare names, so the simulator finds them).
func TestProtocolComplete(t *testing.T) {
	items, err := ParseProgram(protoComplete)
	if err != nil {
		t.Fatalf("complete protocol rejected: %v", err)
	}
	got := map[string]bool{}
	for _, it := range items {
		if d, ok := it.(Def); ok {
			got[d.Name] = true
		}
	}
	for _, want := range []string{"init", "merge", "value", "op0", "mergeComm", "mergeIdem", "mergeAssoc"} {
		if !got[want] {
			t.Errorf("member %q did not pass through (bare); got %v", want, got)
		}
	}
}

// TestProtocolRejectsMissingProof: omitting a convergence proof is rejected with a
// teaching diagnostic — convergence is structural, not convention.
func TestProtocolRejectsMissingProof(t *testing.T) {
	src := `protocol Bad is
  init : Nat is zero end
  merge : Nat is zero end
  value : Nat is zero end
  op0 : Nat is zero end
  mergeComm : Nat is zero end
  mergeIdem : Nat is zero end
end`
	_, err := ParseProgram(src)
	if err == nil {
		t.Fatal("a protocol missing mergeAssoc was accepted")
	}
	if !strings.Contains(err.Error(), "mergeAssoc") || !strings.Contains(err.Error(), "join-semilattice") {
		t.Errorf("diagnostic should name the missing law + teach: %v", err)
	}
}

// TestProtocolRejectsMissingOp: a protocol with no local update is rejected.
func TestProtocolRejectsMissingOp(t *testing.T) {
	src := `protocol NoOp is
  init : Nat is zero end
  merge : Nat is zero end
  value : Nat is zero end
  mergeComm : Nat is zero end
  mergeIdem : Nat is zero end
  mergeAssoc : Nat is zero end
end`
	_, err := ParseProgram(src)
	if err == nil || !strings.Contains(err.Error(), "op0") {
		t.Fatalf("a protocol with no op should be rejected naming op0; got %v", err)
	}
}

// TestProtocolIsContextualKeyword: `protocol` stays a valid IDENTIFIER (e.g. a def
// named `protocol`, as in ch71's M0 vertical slice) — it is a block keyword only in
// the `protocol Name is …` form. Regression guard for the keyword collision.
func TestProtocolIsContextualKeyword(t *testing.T) {
	// `protocol` as a definition name (followed by `:`), not a block.
	items, err := ParseProgram(`protocol : Nat is zero end`)
	if err != nil {
		t.Fatalf("`protocol` as an identifier was rejected: %v", err)
	}
	if len(items) != 1 {
		t.Fatalf("expected 1 def, got %d items", len(items))
	}
	d, ok := items[0].(Def)
	if !ok || d.Name != "protocol" {
		t.Errorf("expected a def named `protocol`, got %#v", items[0])
	}
	// `protocol` as a reference inside another def's body still resolves to an ident.
	if _, err := ParseProgram(`uses : Nat is protocol end`); err != nil {
		t.Errorf("`protocol` as a reference was rejected: %v", err)
	}
}

// TestProtocolMissingEnd: an unterminated block is incomplete (drives the REPL's
// continuation loop), not a hard error.
func TestProtocolMissingEnd(t *testing.T) {
	src := `protocol P is
  init : Nat is zero end`
	_, err := ParseProgram(src)
	if err == nil {
		t.Fatal("unterminated protocol should error")
	}
}
