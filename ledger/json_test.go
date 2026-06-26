// Package ledger - JSON render tests.
package ledger

import (
	"encoding/json"
	"strings"
	"testing"
)

func TestRenderJSON(t *testing.T) {
	es := []Entry{{Name: "inRegion", Tier: Postulate, PropHash: mkHash(1), Why: "not modeled"}}
	var b strings.Builder
	if err := RenderJSON(es, &b); err != nil {
		t.Fatalf("render: %v", err)
	}
	var rows []map[string]any
	if err := json.Unmarshal([]byte(b.String()), &rows); err != nil {
		t.Fatalf("invalid JSON: %v\n%s", err, b.String())
	}
	if len(rows) != 1 || rows[0]["tier"] != "postulate" || rows[0]["name"] != "inRegion" {
		t.Fatalf("bad row: %v", rows)
	}
	if rows[0]["proposition"] == "" {
		t.Fatalf("proposition hash must be a hex string")
	}
}
