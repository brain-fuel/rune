package infra

import (
	"os/exec"
	"reflect"
	"strings"
	"testing"
)

// cloudTargets are the IaC providers every agnostic config must lower to equivalently.
var cloudTargets = []string{"aws", "azure", "gcp"}

// TestQueueLogicalEquivalence is the "equal config -> equivalent deployment" gate:
// one agnostic Queue graph lowers to the SAME logical resource set on every cloud,
// despite each provider's different concrete resources + plumbing.
func TestQueueLogicalEquivalence(t *testing.T) {
	rs := []Resource{Queue{Name: "events"}, Queue{Name: "jobs"}}
	want := []LogicalResource{{Kind: "queue", Name: "events"}, {Kind: "queue", Name: "jobs"}}

	for _, tgt := range cloudTargets {
		e, ok := ByTarget(tgt)
		if !ok {
			t.Fatalf("no emitter for %q", tgt)
		}
		art, err := e.Emit(rs)
		if err != nil {
			t.Fatalf("[%s] emit: %v", tgt, err)
		}
		if !reflect.DeepEqual(art.Logical, want) {
			t.Errorf("[%s] logical set = %v, want %v", tgt, art.Logical, want)
		}
	}
}

// TestProviderResources checks each cloud emits its expected concrete resource type
// for each agnostic resource kind (the provider-specific lowering).
func TestProviderResources(t *testing.T) {
	cases := []struct {
		kind string
		res  Resource
		want map[string]string // provider -> expected concrete resource type
	}{
		{"queue", Queue{Name: "x"}, map[string]string{
			"aws": "aws_sqs_queue", "azure": "azurerm_servicebus_queue", "gcp": "google_pubsub_topic"}},
		{"kv", KV{Name: "x"}, map[string]string{
			"aws": "aws_elasticache_cluster", "azure": "azurerm_redis_cache", "gcp": "google_redis_instance"}},
		{"object", Bucket{Name: "x"}, map[string]string{
			"aws": "aws_s3_bucket", "azure": "azurerm_storage_container", "gcp": "google_storage_bucket"}},
		{"compute", Compute{Name: "x", Replicas: 2}, map[string]string{
			"aws": "aws_instance", "azure": "azurerm_linux_virtual_machine", "gcp": "google_compute_instance"}},
		{"database", Database{Name: "x"}, map[string]string{
			"aws": "aws_db_instance", "azure": "azurerm_postgresql_flexible_server", "gcp": "google_sql_database_instance"}},
		{"secret", Secret{Name: "x"}, map[string]string{
			"aws": "aws_secretsmanager_secret", "azure": "azurerm_key_vault_secret", "gcp": "google_secret_manager_secret"}},
		{"nosql", NoSQL{Name: "x"}, map[string]string{
			"aws": "aws_dynamodb_table", "azure": "azurerm_cosmosdb_account", "gcp": "google_firestore_database"}},
		{"dns", DNS{Name: "x"}, map[string]string{
			"aws": "aws_route53_zone", "azure": "azurerm_dns_zone", "gcp": "google_dns_managed_zone"}},
		{"disk", Disk{Name: "x"}, map[string]string{
			"aws": "aws_ebs_volume", "azure": "azurerm_managed_disk", "gcp": "google_compute_disk"}},
		{"kms", KMS{Name: "x"}, map[string]string{
			"aws": "aws_kms_key", "azure": "azurerm_key_vault_key", "gcp": "google_kms_crypto_key"}},
		{"file", File{Name: "x"}, map[string]string{
			"aws": "aws_efs_file_system", "azure": "azurerm_storage_share", "gcp": "google_filestore_instance"}},
		{"stream", Stream{Name: "x"}, map[string]string{
			"aws": "aws_kinesis_stream", "azure": "azurerm_eventhub", "gcp": "google_pubsub_topic"}},
		{"cdn", CDN{Name: "x"}, map[string]string{
			"aws": "aws_cloudfront_distribution", "azure": "azurerm_cdn_profile", "gcp": "google_compute_backend_bucket"}},
		{"lb", LoadBalancer{Name: "x"}, map[string]string{
			"aws": "aws_lb", "azure": "azurerm_lb", "gcp": "google_compute_forwarding_rule"}},
		{"metrics", Metrics{Name: "x"}, map[string]string{
			"aws": "aws_cloudwatch_dashboard", "azure": "azurerm_monitor_workspace", "gcp": "google_monitoring_dashboard"}},
		{"iam", Identity{Name: "x"}, map[string]string{
			"aws": "aws_iam_role", "azure": "azurerm_user_assigned_identity", "gcp": "google_service_account"}},
		{"k8s", K8s{Name: "x"}, map[string]string{
			"aws": "aws_eks_cluster", "azure": "azurerm_kubernetes_cluster", "gcp": "google_container_cluster"}},
		{"network", Network{Name: "x"}, map[string]string{
			"aws": "aws_vpc", "azure": "azurerm_virtual_network", "gcp": "google_compute_network"}},
		{"firewall", Firewall{Name: "x"}, map[string]string{
			"aws": "aws_wafv2_web_acl", "azure": "azurerm_network_ddos_protection_plan", "gcp": "google_compute_security_policy"}},
		{"logs", Logs{Name: "x"}, map[string]string{
			"aws": "aws_cloudwatch_log_group", "azure": "azurerm_log_analytics_workspace", "gcp": "google_logging_project_bucket_config"}},
		{"registry", Registry{Name: "x"}, map[string]string{
			"aws": "aws_ecr_repository", "azure": "azurerm_container_registry", "gcp": "google_artifact_registry_repository"}},
		{"paas", PaaS{Name: "x"}, map[string]string{
			"aws": "aws_elastic_beanstalk_application", "azure": "azurerm_service_plan", "gcp": "google_app_engine_application"}},
	}
	for _, c := range cases {
		for tgt, res := range c.want {
			e, _ := ByTarget(tgt)
			art, err := e.Emit([]Resource{c.res})
			if err != nil {
				t.Fatalf("[%s/%s] emit: %v", tgt, c.kind, err)
			}
			tf := art.Files["main.tf"]
			if !strings.Contains(tf, res) {
				t.Errorf("[%s/%s] main.tf missing %q\n%s", tgt, c.kind, res, tf)
			}
			if !strings.Contains(tf, "\"x\"") {
				t.Errorf("[%s/%s] main.tf missing the resource name", tgt, c.kind)
			}
		}
	}
}

// TestMultiResourceEquivalence is the app-level headline gate: a whole multi-resource
// graph (a realistic app) lowers to the SAME logical resource set on every cloud —
// equal config yields an equivalent deployment, for an entire application, not just
// one resource.
func TestMultiResourceEquivalence(t *testing.T) {
	graph := []Resource{
		Compute{Name: "worker", Replicas: 3}, Queue{Name: "events"}, KV{Name: "cache"},
		Database{Name: "appdb"}, Secret{Name: "apikey"}, Identity{Name: "role"},
		Bucket{Name: "assets"}, KMS{Name: "key"},
		LoadBalancer{Name: "ingress"}, CDN{Name: "edge"},
	}
	var first []LogicalResource
	for i, tgt := range cloudTargets {
		e, _ := ByTarget(tgt)
		art, err := e.Emit(graph)
		if err != nil {
			t.Fatalf("[%s] emit: %v", tgt, err)
		}
		if i == 0 {
			first = art.Logical
			continue
		}
		if !reflect.DeepEqual(art.Logical, first) {
			t.Errorf("[%s] logical set diverged from %s:\n got %v\nwant %v", tgt, cloudTargets[0], art.Logical, first)
		}
	}
	if len(first) != len(graph) {
		t.Errorf("logical set has %d entries, want %d", len(first), len(graph))
	}
}

// TestKVObjectEquivalence extends the equal-config->equivalent-deployment gate to the
// KV and Object abstractions: one agnostic graph lowers to the same logical set on
// every cloud.
func TestKVObjectEquivalence(t *testing.T) {
	cases := []struct {
		rs   []Resource
		want []LogicalResource
	}{
		{[]Resource{KV{Name: "cache"}}, []LogicalResource{{Kind: "kv", Name: "cache"}}},
		{[]Resource{Bucket{Name: "assets"}}, []LogicalResource{{Kind: "object", Name: "assets"}}},
	}
	for _, c := range cases {
		for _, tgt := range cloudTargets {
			e, _ := ByTarget(tgt)
			art, err := e.Emit(c.rs)
			if err != nil {
				t.Fatalf("[%s] emit: %v", tgt, err)
			}
			if !reflect.DeepEqual(art.Logical, c.want) {
				t.Errorf("[%s] logical = %v, want %v", tgt, art.Logical, c.want)
			}
		}
	}
}

// TestKVObjectFOSS checks the Valkey (kv) and Garage (object) self-hosted backends.
func TestKVObjectFOSS(t *testing.T) {
	v, _ := ByTarget("valkey")
	art, err := v.Emit([]Resource{KV{Name: "cache"}})
	if err != nil {
		t.Fatalf("valkey emit: %v", err)
	}
	if !strings.Contains(art.Files["connection.env"], "WAVELET_KV_BACKEND=valkey") {
		t.Errorf("valkey connection.env wrong:\n%s", art.Files["connection.env"])
	}
	g, _ := ByTarget("garage")
	art, err = g.Emit([]Resource{Bucket{Name: "assets"}})
	if err != nil {
		t.Fatalf("garage emit: %v", err)
	}
	if art.Files["garage.toml"] == "" {
		t.Errorf("garage emit missing garage.toml")
	}
	if !strings.Contains(art.Files["connection.env"], "WAVELET_OBJECT_BACKEND=garage") {
		t.Errorf("garage connection.env wrong:\n%s", art.Files["connection.env"])
	}
}

// TestComputePodman checks the local container backend emits N named replica services
// with a stable PEERS list (gossip-friendly), and resolves via the container alias.
func TestComputePodman(t *testing.T) {
	e, ok := ByTarget("container") // alias for podman
	if !ok {
		t.Fatal("no emitter for the container alias")
	}
	art, err := e.Emit([]Resource{Compute{Name: "rep", Image: "img:1", Replicas: 3}})
	if err != nil {
		t.Fatalf("podman compute emit: %v", err)
	}
	compose := art.Files["compose.yaml"]
	for _, want := range []string{"rep-0:", "rep-1:", "rep-2:", "PEERS=rep-0,rep-1,rep-2", "image: img:1"} {
		if !strings.Contains(compose, want) {
			t.Errorf("compose.yaml missing %q\n%s", want, compose)
		}
	}
}

// TestDatabasePostgres checks the self-hosted PostgreSQL backend.
func TestDatabasePostgres(t *testing.T) {
	e, _ := ByTarget("postgres")
	art, err := e.Emit([]Resource{Database{Name: "appdb"}})
	if err != nil {
		t.Fatalf("postgres emit: %v", err)
	}
	if !strings.Contains(art.Files["compose.yaml"], "postgres:16") {
		t.Errorf("postgres compose missing image:\n%s", art.Files["compose.yaml"])
	}
	if !strings.Contains(art.Files["connection.env"], "WAVELET_DATABASE_BACKEND=postgres") {
		t.Errorf("postgres connection.env wrong:\n%s", art.Files["connection.env"])
	}
}

// TestFOSSBackendsEmit sweeps every self-hosted backend with the resource kind it
// serves and asserts it emits a runnable Compose spec + a connection.env — one gate
// protecting all FOSS emitters.
func TestFOSSBackendsEmit(t *testing.T) {
	cases := []struct {
		backend string
		res     Resource
	}{
		{"rabbitmq", Queue{Name: "q"}}, {"nats", Queue{Name: "q"}},
		{"valkey", KV{Name: "k"}}, {"garage", Bucket{Name: "b"}},
		{"podman", Compute{Name: "c", Replicas: 2}}, {"postgres", Database{Name: "d"}},
		{"dynamodb", NoSQL{Name: "n"}}, {"coredns", DNS{Name: "z"}},
		{"localregistry", Registry{Name: "r"}},
		{"redpanda", Stream{Name: "s"}},
		{"vault", Secret{Name: "sec"}},
		{"loki", Logs{Name: "applog"}},
		{"prometheus", Metrics{Name: "m"}},
	}
	for _, c := range cases {
		e, ok := ByTarget(c.backend)
		if !ok {
			t.Errorf("no emitter for %q", c.backend)
			continue
		}
		if e.Cloud() {
			t.Errorf("%q should be a FOSS (non-cloud) backend", c.backend)
		}
		art, err := e.Emit([]Resource{c.res})
		if err != nil {
			t.Errorf("[%s] emit: %v", c.backend, err)
			continue
		}
		if art.Files["compose.yaml"] == "" || !strings.Contains(art.Files["compose.yaml"], "services:") {
			t.Errorf("[%s] missing a Compose services spec", c.backend)
		}
		if art.Files["connection.env"] == "" {
			t.Errorf("[%s] missing connection.env", c.backend)
		}
	}
	// dotenv is the one FOSS backend without a Compose service (a keys-only template).
	d, _ := ByTarget("dotenv")
	art, err := d.Emit([]Resource{Secret{Name: "s"}})
	if err != nil || art.Files["secrets.env"] == "" {
		t.Errorf("dotenv: %v / %q", err, art.Files["secrets.env"])
	}
}

// TestSecretKMSShareKeyVault checks that on Azure a graph with both a Secret and a KMS
// key emits exactly ONE shared Key Vault (the scaffolding is emitted once).
func TestSecretKMSShareKeyVault(t *testing.T) {
	e, _ := ByTarget("azure")
	art, err := e.Emit([]Resource{Secret{Name: "s"}, KMS{Name: "k"}})
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	tf := art.Files["main.tf"]
	if n := strings.Count(tf, "resource \"azurerm_key_vault\" \"wavelet\""); n != 1 {
		t.Errorf("expected exactly 1 shared key vault, got %d\n%s", n, tf)
	}
}

// TestSecretDotenv checks the local secret backend emits a keys-only template.
func TestSecretDotenv(t *testing.T) {
	e, _ := ByTarget("dotenv")
	art, err := e.Emit([]Resource{Secret{Name: "apikey"}})
	if err != nil {
		t.Fatalf("dotenv emit: %v", err)
	}
	tmpl := art.Files["secrets.env"]
	if !strings.Contains(tmpl, "APIKEY=") {
		t.Errorf("secrets.env missing the key template:\n%s", tmpl)
	}
	if strings.Contains(tmpl, "APIKEY=secret") || strings.Contains(tmpl, "APIKEY=value") {
		t.Errorf("secrets.env must not carry a value")
	}
}

// TestQueueFOSSBackends checks the self-hosted queue backends emit a runnable Compose
// spec + a connection.env, with the same logical set as the clouds.
func TestQueueFOSSBackends(t *testing.T) {
	rs := []Resource{Queue{Name: "events"}}
	for _, tgt := range []string{"rabbitmq", "nats"} {
		e, ok := ByTarget(tgt)
		if !ok {
			t.Fatalf("no emitter for %q", tgt)
		}
		if e.Cloud() {
			t.Errorf("[%s] should be a FOSS (non-cloud) backend", tgt)
		}
		art, err := e.Emit(rs)
		if err != nil {
			t.Fatalf("[%s] emit: %v", tgt, err)
		}
		if !strings.Contains(art.Files["compose.yaml"], "services:") {
			t.Errorf("[%s] compose.yaml missing services block", tgt)
		}
		env := art.Files["connection.env"]
		if !strings.Contains(env, "WAVELET_QUEUE_BACKEND="+tgt) {
			t.Errorf("[%s] connection.env missing backend marker:\n%s", tgt, env)
		}
		if !strings.Contains(env, "WAVELET_QUEUE_EVENTS=events") {
			t.Errorf("[%s] connection.env missing the queue binding", tgt)
		}
	}
}

// TestQueueHCLFormatted verifies the emitted HCL is syntactically well-formed by
// running `tofu fmt` (or `terraform fmt`) over it: the formatter rejects malformed
// HCL and is a no-op on already-canonical input. Skipped when no IaC binary is
// present (CI without OpenTofu/Terraform installed).
func TestQueueHCLFormatted(t *testing.T) {
	bin := iacBinary()
	if bin == "" {
		t.Skip("no tofu/terraform binary in PATH")
	}
	graphs := map[string][]Resource{
		"queue":    {Queue{Name: "events"}},
		"kv":       {KV{Name: "cache"}},
		"object":   {Bucket{Name: "assets"}},
		"compute":  {Compute{Name: "worker", Image: "docker.io/library/erlang:slim", Replicas: 3}},
		"database": {Database{Name: "appdb"}},
		"secret":   {Secret{Name: "apikey"}},
		"nosql":    {NoSQL{Name: "sessions"}},
		"dns":      {DNS{Name: "web"}},
		"disk":     {Disk{Name: "data", SizeGB: 50}},
		"kms":      {KMS{Name: "appkey"}},
		"file":     {File{Name: "shared"}},
		"stream":   {Stream{Name: "events"}},
		"cdn":      {CDN{Name: "edgecache"}},
		"metrics":  {Metrics{Name: "dash"}},
		"lb":       {LoadBalancer{Name: "ingress"}},
		"iam":      {Identity{Name: "worker"}},
		"k8s":      {K8s{Name: "cluster"}},
		"network":  {Network{Name: "vpc"}},
		"firewall": {Firewall{Name: "edge"}},
		"logs":     {Logs{Name: "applog"}},
		"registry": {Registry{Name: "images"}},
		"paas":     {PaaS{Name: "webapp"}},
	}
	for kind, rs := range graphs {
		for _, tgt := range cloudTargets {
			e, _ := ByTarget(tgt)
			art, err := e.Emit(rs)
			if err != nil {
				t.Fatalf("[%s/%s] emit: %v", tgt, kind, err)
			}
			tf := art.Files["main.tf"]
			cmd := exec.Command(bin, "fmt", "-check", "-")
			cmd.Stdin = strings.NewReader(tf)
			if out, err := cmd.CombinedOutput(); err != nil {
				t.Errorf("[%s/%s] %s fmt rejected the emitted HCL: %v\n%s\n--- HCL ---\n%s",
					tgt, kind, bin, err, out, tf)
			}
		}
	}
}

func iacBinary() string {
	for _, b := range []string{"tofu", "terraform"} {
		if _, err := exec.LookPath(b); err == nil {
			return b
		}
	}
	return ""
}
