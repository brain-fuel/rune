package infra

import (
	"encoding/json"
	"fmt"
	"strings"
)

// iamPolicyStatement is one Allow statement of an AWS-style policy document.
type iamPolicyStatement struct {
	Effect   string   `json:"Effect"`
	Action   []string `json:"Action"`
	Resource string   `json:"Resource"`
}

// iamPolicyDoc is the compact policy document an aws_iam_role_policy carries.
type iamPolicyDoc struct {
	Version   string               `json:"Version"`
	Statement []iamPolicyStatement `json:"Statement"`
}

// iamPolicyJSON renders the least-privilege policy for a set of agnostic grant
// tokens as a compact JSON string (one HCL token, terraform-fmt-stable). With no
// grants it is a well-formed empty (deny-all) document. The grants are the policy's
// only actions, so a least-privilege policy is auditable as exactly the declared
// capabilities and nothing more; an over-broad policy would carry extra actions.
func iamPolicyJSON(grants []string) string {
	doc := iamPolicyDoc{Version: "2012-10-17", Statement: []iamPolicyStatement{}}
	if len(grants) > 0 {
		doc.Statement = []iamPolicyStatement{{Effect: "Allow", Action: grants, Resource: "*"}}
	}
	b, _ := json.Marshal(doc)
	return string(b)
}

// hclList renders a single-line HCL list literal of quoted strings, fmt-stable for
// the short permission lists the least-privilege roles carry.
func hclList(items []string) string {
	q := make([]string, len(items))
	for i, s := range items {
		q[i] = fmt.Sprintf("%q", s)
	}
	return "[" + strings.Join(q, ", ") + "]"
}
