package infra

import (
	"strings"
	"testing"
)

func TestAzureIdentityScopedRole(t *testing.T) {
	art, err := Azure{}.Emit([]Resource{Identity{Name: "relay_role", Grants: []string{"kv:Get", "kv:Set"}}})
	if err != nil {
		t.Fatal(err)
	}
	tf := art.Files["main.tf"]
	for _, want := range []string{
		`resource "azurerm_role_definition" "relay_role_role"`,
		`actions     = ["kv:Get", "kv:Set"]`,
	} {
		if !strings.Contains(tf, want) {
			t.Fatalf("Azure role definition missing %q\n%s", want, tf)
		}
	}
}

func TestAzureIdentityNoGrantsUnchanged(t *testing.T) {
	art, err := Azure{}.Emit([]Resource{Identity{Name: "worker_role"}})
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(art.Files["main.tf"], "azurerm_role_definition") {
		t.Fatalf("a grant-less identity must not emit a role definition\n%s", art.Files["main.tf"])
	}
}
