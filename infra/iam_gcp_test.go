package infra

import (
	"strings"
	"testing"
)

func TestGCPIdentityScopedRole(t *testing.T) {
	art, err := GCP{}.Emit([]Resource{Identity{Name: "relay_role", Grants: []string{"kv:Get", "kv:Set"}}})
	if err != nil {
		t.Fatal(err)
	}
	tf := art.Files["main.tf"]
	for _, want := range []string{
		`resource "google_project_iam_custom_role" "relay_role_role"`,
		`permissions = ["kv:Get", "kv:Set"]`,
	} {
		if !strings.Contains(tf, want) {
			t.Fatalf("GCP custom role missing %q\n%s", want, tf)
		}
	}
}

func TestGCPIdentityNoGrantsUnchanged(t *testing.T) {
	art, err := GCP{}.Emit([]Resource{Identity{Name: "worker_role"}})
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(art.Files["main.tf"], "google_project_iam_custom_role") {
		t.Fatalf("a grant-less identity must not emit a custom role\n%s", art.Files["main.tf"])
	}
}
