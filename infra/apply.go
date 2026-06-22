package infra

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

// The APPLY side of the wavelet layer: `rune deploy` emits an Artifact; Apply STANDS
// IT UP. Two lifecycles behind one verb, dispatched on Emitter.Cloud():
//
//   - FOSS backends (valkey/postgres/rabbitmq/…): `docker compose up -d` on the
//     emitted compose.yaml, then `down -v` on destroy. Runs with NO cloud account.
//   - Cloud backends (aws/azure/gcp): `terraform init/apply` on the emitted HCL. For
//     AWS we point it at LOCALSTACK via a generated `_override.tf` (terraform's
//     override-file merge replaces the base provider's endpoints + creds), so the
//     SAME emitter HCL applies against a local fake cloud with NO account and NO bill.
//
// This is the deploy-side dual of running a codegen backend: emit then execute. The
// shadow rule holds — Apply writes throwaway files into a work dir and shells out to
// the standard tools (docker, terraform); it never touches core/store.

// ApplyOptions configures one apply.
type ApplyOptions struct {
	// WorkDir is where the artifact files are written and the tools run. If empty a
	// temp dir is created (and removed on destroy).
	WorkDir string
	// Destroy tears the deployment down immediately after standing it up — the
	// apply-then-destroy lifecycle a CI gate or a free-tier demo wants (nothing left
	// running, nothing billed).
	Destroy bool
	// LocalStack, when set for a CLOUD target, is the LocalStack endpoint (e.g.
	// "http://localhost:4566") the AWS provider is redirected to. Empty means a real
	// cloud apply (credentials from the environment). For a FOSS target it is ignored.
	LocalStack string
	// Out receives human-readable progress.
	Out io.Writer
}

// ApplyResult records what an apply did.
type ApplyResult struct {
	Target   string            // the emitter target ("aws", "valkey", …)
	Cloud    bool              // a cloud (terraform) apply vs a FOSS (compose) apply
	WorkDir  string            // where the artifact + state live (for teardown/verify)
	Endpoint string            // where to reach the standing resources, if known
	Logical  []LogicalResource // the realized logical set (the equivalence witness)
	Standing bool              // true if resources are up (false after a Destroy)
}

// Apply emits-then-stands-up: it writes art's files into the work dir and runs the
// lifecycle for the emitter's kind. A cloud emitter with opts.LocalStack set applies
// against LocalStack; a FOSS emitter applies via docker compose.
func Apply(e Emitter, art Artifact, opts ApplyOptions) (ApplyResult, error) {
	out := opts.Out
	if out == nil {
		out = io.Discard
	}
	dir := opts.WorkDir
	cleanup := false
	if dir == "" {
		d, err := os.MkdirTemp("", "wavelet-apply-*")
		if err != nil {
			return ApplyResult{}, err
		}
		dir = d
		cleanup = true
	}
	if err := writeFiles(dir, art.Files); err != nil {
		return ApplyResult{}, err
	}
	res := ApplyResult{Target: e.Target(), Cloud: e.Cloud(), WorkDir: dir, Logical: art.Logical}

	var err error
	if e.Cloud() {
		err = applyCloud(dir, opts, &res, out)
	} else {
		err = applyFOSS(dir, art, opts, &res, out)
	}
	if err != nil {
		return res, err
	}
	if cleanup && opts.Destroy {
		os.RemoveAll(dir)
	}
	return res, nil
}

// writeFiles writes an artifact's files into dir (creating it).
func writeFiles(dir string, files map[string]string) error {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return err
	}
	for name, body := range files {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o644); err != nil {
			return err
		}
	}
	return nil
}

// applyFOSS stands the FOSS backend up with docker compose. The emitted compose.yaml
// is the spec; connection.env carries the endpoint. On Destroy it tears down with
// `down -v` (volumes too — nothing persists).
func applyFOSS(dir string, art Artifact, opts ApplyOptions, res *ApplyResult, out io.Writer) error {
	if _, ok := art.Files["compose.yaml"]; !ok {
		return fmt.Errorf("infra apply: FOSS target %q emitted no compose.yaml", res.Target)
	}
	res.Endpoint = connectionURL(art.Files["connection.env"])

	fmt.Fprintf(out, "apply[%s]: docker compose up -d in %s\n", res.Target, dir)
	if err := composeCmd(dir, "up", "-d").Run(); err != nil {
		return fmt.Errorf("docker compose up: %w", err)
	}
	res.Standing = true
	if err := waitComposeRunning(dir, 30*time.Second); err != nil {
		composeCmd(dir, "down", "-v").Run()
		res.Standing = false
		return err
	}
	fmt.Fprintf(out, "apply[%s]: up%s\n", res.Target, endpointNote(res.Endpoint))

	if opts.Destroy {
		fmt.Fprintf(out, "apply[%s]: docker compose down -v\n", res.Target)
		if err := composeCmd(dir, "down", "-v").Run(); err != nil {
			return fmt.Errorf("docker compose down: %w", err)
		}
		res.Standing = false
	}
	return nil
}

// applyCloud stands the cloud backend up with terraform. When opts.LocalStack is set
// it writes a localstack_override.tf so the AWS provider hits the local fake cloud,
// so the apply needs NO real account. terraform init then apply; destroy on Destroy.
func applyCloud(dir string, opts ApplyOptions, res *ApplyResult, out io.Writer) error {
	if _, err := exec.LookPath("terraform"); err != nil {
		return fmt.Errorf("terraform not in PATH")
	}
	if opts.LocalStack != "" {
		if res.Target != "aws" {
			return fmt.Errorf("infra apply: --localstack supports the aws target only (got %q)", res.Target)
		}
		ovr := localStackOverrideTF(opts.LocalStack)
		if err := os.WriteFile(filepath.Join(dir, "localstack_override.tf"), []byte(ovr), 0o644); err != nil {
			return err
		}
		res.Endpoint = opts.LocalStack
		fmt.Fprintf(out, "apply[%s]: targeting LocalStack at %s (no cloud account)\n", res.Target, opts.LocalStack)
	}

	fmt.Fprintf(out, "apply[%s]: terraform init in %s\n", res.Target, dir)
	if err := tfCmd(dir, opts, "init", "-input=false", "-no-color").Run(); err != nil {
		return fmt.Errorf("terraform init: %w", err)
	}
	fmt.Fprintf(out, "apply[%s]: terraform apply\n", res.Target)
	if err := tfCmd(dir, opts, "apply", "-auto-approve", "-input=false", "-no-color").Run(); err != nil {
		return fmt.Errorf("terraform apply: %w", err)
	}
	res.Standing = true
	fmt.Fprintf(out, "apply[%s]: applied%s\n", res.Target, endpointNote(res.Endpoint))

	if opts.Destroy {
		fmt.Fprintf(out, "apply[%s]: terraform destroy\n", res.Target)
		if err := tfCmd(dir, opts, "destroy", "-auto-approve", "-input=false", "-no-color").Run(); err != nil {
			return fmt.Errorf("terraform destroy: %w", err)
		}
		res.Standing = false
	}
	return nil
}

// composeCmd builds a `docker compose <args>` command rooted at dir.
func composeCmd(dir string, args ...string) *exec.Cmd {
	c := exec.Command("docker", append([]string{"compose"}, args...)...)
	c.Dir = dir
	return c
}

// tfCmd builds a terraform command rooted at dir. For a LocalStack apply it injects
// the dummy AWS credentials the provider still demands (validation is skipped in the
// override, but the SDK wants non-empty keys).
func tfCmd(dir string, opts ApplyOptions, args ...string) *exec.Cmd {
	c := exec.Command("terraform", args...)
	c.Dir = dir
	if opts.LocalStack != "" {
		c.Env = append(os.Environ(),
			"AWS_ACCESS_KEY_ID=test",
			"AWS_SECRET_ACCESS_KEY=test",
			"AWS_DEFAULT_REGION=us-east-1",
		)
	}
	return c
}

// waitComposeRunning polls `docker compose ps` until every service is running or the
// timeout elapses. compose `up -d` returns before the container is necessarily ready.
func waitComposeRunning(dir string, timeout time.Duration) error {
	deadline := time.Now().Add(timeout)
	for time.Now().Before(deadline) {
		out, err := composeCmd(dir, "ps", "--status", "running", "--quiet").Output()
		if err == nil && len(strings.TrimSpace(string(out))) > 0 {
			return nil
		}
		time.Sleep(300 * time.Millisecond)
	}
	return fmt.Errorf("compose services not running after %s", timeout)
}

// connectionURL extracts the WAVELET_*_URL value from a connection.env body (the
// endpoint the FOSS backend listens on), or "" if none.
func connectionURL(env string) string {
	sc := bufio.NewScanner(strings.NewReader(env))
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		k, v, ok := strings.Cut(line, "=")
		if ok && strings.HasSuffix(k, "_URL") {
			return v
		}
	}
	return ""
}

// endpointNote renders " (endpoint X)" when an endpoint is known, else "".
func endpointNote(ep string) string {
	if ep == "" {
		return ""
	}
	return " (endpoint " + ep + ")"
}

// localStackOverrideTF is a terraform OVERRIDE file: a file ending in `_override.tf`
// whose blocks are merged attribute-by-attribute into the matching base blocks. This
// one overrides the emitter's `provider "aws"` to point every service endpoint at
// LocalStack, supply dummy credentials, and skip the validations that would otherwise
// reach the real AWS metadata service — so the unchanged emitter HCL applies against a
// local fake cloud with no account. (The endpoint set covers the LocalStack Community
// services our resource matrix lowers to.)
func localStackOverrideTF(endpoint string) string {
	services := []string{
		"s3", "sqs", "sns", "dynamodb", "secretsmanager", "kms", "route53",
		"ec2", "rds", "elasticache", "iam", "sts", "cloudwatch", "logs",
		"lambda", "ecr", "kinesis", "ssm",
	}
	var b strings.Builder
	b.WriteString("# Generated by `rune deploy --apply --localstack`: redirect the AWS\n")
	b.WriteString("# provider to LocalStack so the emitter HCL applies with no cloud account.\n")
	b.WriteString("provider \"aws\" {\n")
	b.WriteString("  access_key                  = \"test\"\n")
	b.WriteString("  secret_key                  = \"test\"\n")
	b.WriteString("  region                      = \"us-east-1\"\n")
	b.WriteString("  skip_credentials_validation = true\n")
	b.WriteString("  skip_metadata_api_check     = true\n")
	b.WriteString("  skip_requesting_account_id  = true\n")
	b.WriteString("  s3_use_path_style           = true\n")
	b.WriteString("  endpoints {\n")
	for _, s := range services {
		b.WriteString(fmt.Sprintf("    %-15s = %q\n", s, endpoint))
	}
	b.WriteString("  }\n")
	b.WriteString("}\n")
	return b.String()
}
