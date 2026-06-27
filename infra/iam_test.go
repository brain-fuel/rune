package infra

import "testing"

func TestIAMPolicyJSON(t *testing.T) {
	got := iamPolicyJSON([]string{"kv:Get", "kv:Set"})
	want := `{"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":["kv:Get","kv:Set"],"Resource":"*"}]}`
	if got != want {
		t.Fatalf("policy JSON\n got: %s\nwant: %s", got, want)
	}
}

func TestIAMPolicyJSONEmpty(t *testing.T) {
	// no grants => an empty (deny-all) statement list, still a well-formed document
	got := iamPolicyJSON(nil)
	want := `{"Version":"2012-10-17","Statement":[]}`
	if got != want {
		t.Fatalf("empty policy JSON\n got: %s\nwant: %s", got, want)
	}
}

func TestHCLList(t *testing.T) {
	if got := hclList([]string{"kv:Get", "kv:Set"}); got != `["kv:Get", "kv:Set"]` {
		t.Fatalf("hclList = %s", got)
	}
	if got := hclList(nil); got != `[]` {
		t.Fatalf("empty hclList = %s", got)
	}
}
