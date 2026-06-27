// calm/emit_test.go
package calm

import (
	"sort"
	"strings"
	"testing"

	"goforge.dev/rune/v3/infra"
)

func TestEmitNodesEqualLogicalResources(t *testing.T) {
	m := demoModel(t)
	var b strings.Builder
	if err := Emit(m, &b); err != nil {
		t.Fatalf("emit: %v", err)
	}
	out := b.String()
	if !strings.Contains(out, `"unique-id": "store"`) || !strings.Contains(out, `"node-type": "database"`) {
		t.Fatalf("emitted CALM missing the store database node:\n%s", out)
	}
	if !strings.Contains(out, `"web->relay"`) || !strings.Contains(out, `"connects"`) {
		t.Fatalf("emitted CALM missing the web->relay connects relationship:\n%s", out)
	}

	// equivalence tie: the CALM node ids are exactly the resource graph's logical names.
	rs := []infra.Resource{infra.PaaS{Name: "web"}, infra.Compute{Name: "relay"}, infra.KV{Name: "store"}}
	wantIDs := []string{}
	for _, r := range rs {
		wantIDs = append(wantIDs, r.LogicalName())
	}
	sort.Strings(wantIDs)
	gotIDs := []string{}
	for _, n := range m.Nodes {
		gotIDs = append(gotIDs, n.ID)
	}
	if strings.Join(gotIDs, ",") != strings.Join(wantIDs, ",") {
		t.Fatalf("CALM node ids %v must equal the logical resource set %v", gotIDs, wantIDs)
	}
}
