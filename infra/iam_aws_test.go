package infra

import (
	"strings"
	"testing"
)

func TestAWSIdentityScopedPolicy(t *testing.T) {
	art, err := AWS{}.Emit([]Resource{Identity{Name: "relay_role", Grants: []string{"kv:Get", "kv:Set"}}})
	if err != nil {
		t.Fatal(err)
	}
	tf := art.Files["main.tf"]
	for _, want := range []string{
		`resource "aws_iam_role_policy" "relay_role_policy"`,
		`aws_iam_role.relay_role.id`,
		`\"Action\":[\"kv:Get\",\"kv:Set\"]`,
	} {
		if !strings.Contains(tf, want) {
			t.Fatalf("AWS iam policy missing %q\n%s", want, tf)
		}
	}
}

func TestAWSIdentityNoGrantsUnchanged(t *testing.T) {
	// an Identity with no grants must NOT emit a policy resource (backward compat)
	art, err := AWS{}.Emit([]Resource{Identity{Name: "worker_role"}})
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(art.Files["main.tf"], "aws_iam_role_policy") {
		t.Fatalf("a grant-less identity must not emit a policy resource\n%s", art.Files["main.tf"])
	}
}
