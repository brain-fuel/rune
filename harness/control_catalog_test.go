package harness

import "testing"

// TestControlCatalogElaborates pins that the catalog listing loads and that the
// in-region control's fold computes to true (so its refl is a real proof).
func TestControlCatalogElaborates(t *testing.T) {
	s := loadListing(t, "ch538_control_catalog.rune")
	normalizesTo(t, s, `allInRegion usEast demoServices`, "true")
}
