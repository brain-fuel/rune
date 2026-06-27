// control/control_test.go
package control

import "testing"

func TestFlagshipsMatchCatalog(t *testing.T) {
	cat := Catalog()
	if len(cat) == 0 {
		t.Fatal("catalog is empty")
	}
	// every flagship name must be a proven control in the catalog
	provenNames := map[string]bool{}
	for _, c := range cat {
		if c.Kind != "tail" {
			provenNames[c.Name] = true
		}
	}
	for _, f := range Flagships() {
		if !provenNames[f] {
			t.Fatalf("flagship %q is not a non-tail control in the catalog", f)
		}
	}
	// the four flagship classes are present and named correctly
	want := map[string]string{
		"inRegionProof":  "in-region",
		"encryptedProof": "encrypted-in-transit",
		"leastPrivProof": "least-privilege-iam",
		"convergesProof": "convergence",
	}
	got := map[string]string{}
	for _, c := range cat {
		got[c.Name] = c.Kind
	}
	for name, kind := range want {
		if got[name] != kind {
			t.Fatalf("control %q: want kind %q, got %q", name, kind, got[name])
		}
	}
}

func TestAllowedPostulatesAreTailControls(t *testing.T) {
	tail := map[string]bool{}
	for _, c := range Catalog() {
		if c.Kind == "tail" {
			tail[c.Name] = true
		}
	}
	for _, p := range AllowedPostulates() {
		if !tail[p] {
			t.Fatalf("allowed postulate %q is not a tail control", p)
		}
	}
}
