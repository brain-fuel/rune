// cmd/rune/azure_iam_noaccount_test.go
package main

import (
	"os/exec"
	"strings"
	"testing"

	"goforge.dev/rune/v3/infra"
)

// TestAzureDemoIAMNoAccount pins the honest no-account ceiling for the demo's Azure
// IAM. Unlike AWS (LocalStack implements IAM, so the scoped policy is applied-and-
// observed no-account, see infra/iam_localstack_apply_test.go), Azure has no
// terraform emulator: azurerm targets ARM (the control plane) and Azurite emulates
// only the storage data plane. So the no-account Azure IAM story is exactly two facts,
// asserted here:
//  1. the scoped azurerm_role_definition (actions = kv:Get, kv:Set) is provider-VALID
//     with no account (terraform validate), and
//  2. an emulator apply is cleanly REFUSED with a specific error.
//
// The no-account path for Azure's DATA shapes (kv/object/queue) is FOSS-via-Podman;
// IAM is a cloud control-plane concept with no FOSS equivalent (see the doc note).
func TestAzureDemoIAMNoAccount(t *testing.T) {
	e, ok := infra.ByTarget("azure")
	if !ok {
		t.Fatal("no azure emitter")
	}
	art, err := e.Emit([]infra.Resource{infra.Identity{Name: "relay_role", Grants: []string{"kv:Get", "kv:Set"}}})
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	tf := art.Files["main.tf"]

	// Sanity: the scoped role with exactly the grants is what we are validating.
	if !strings.Contains(tf, "kv:Get") || !strings.Contains(tf, "kv:Set") {
		t.Fatalf("azure demo IAM HCL missing the scoped grants:\n%s", tf)
	}

	// Fact 1: schema-valid with no account.
	validateClean(t, tf)

	// Fact 2: an emulator apply is cleanly refused (azurerm has no emulator endpoint).
	if _, err := exec.LookPath("terraform"); err != nil {
		t.Skip("terraform not in PATH (needed to reach the apply dispatch)")
	}
	_, aerr := infra.Apply(e, art, infra.ApplyOptions{WorkDir: t.TempDir(), LocalStack: "http://localhost:4566"})
	if aerr == nil {
		t.Fatal("azure emulator apply must be refused (azurerm has no emulator endpoint)")
	}
	if !strings.Contains(aerr.Error(), "no terraform emulator endpoint") {
		t.Fatalf("azure emulator apply error should name the missing emulator endpoint, got: %v", aerr)
	}
}
