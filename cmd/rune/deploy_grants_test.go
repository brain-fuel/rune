package main

import (
	"os"
	"path/filepath"
	"testing"

	"goforge.dev/rune/v3/infra"
)

func TestManifestGrants(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "m.rune")
	if err := os.WriteFile(path, []byte("iam relay_role grants=kv:Get,kv:Set\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	rs, err := parseManifest(path)
	if err != nil {
		t.Fatalf("parseManifest: %v", err)
	}
	if len(rs) != 1 {
		t.Fatalf("want 1 resource, got %d", len(rs))
	}
	id, ok := rs[0].(infra.Identity)
	if !ok {
		t.Fatalf("want an Identity, got %T", rs[0])
	}
	if len(id.Grants) != 2 || id.Grants[0] != "kv:Get" || id.Grants[1] != "kv:Set" {
		t.Fatalf("grants not parsed: %v", id.Grants)
	}
}

func TestManifestGrantsOnlyIAM(t *testing.T) {
	// grants on a non-iam resource is a clear error
	dir := t.TempDir()
	path := filepath.Join(dir, "m.rune")
	if err := os.WriteFile(path, []byte("kv store grants=kv:Get\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if _, err := parseManifest(path); err == nil {
		t.Fatalf("grants on a non-iam resource must be an error")
	}
}
