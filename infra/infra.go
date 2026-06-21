// Package infra is the INFRA STRATUM (the "wavelet" cloud-abstraction layer): a
// provider-agnostic resource model lowered to a concrete deployment artifact behind
// the Emitter interface, so a new cloud or self-hosted backend is one plugin, not a
// fork. It is the deploy-side dual of codegen/: where codegen lowers the erased IR
// to target SOURCE, infra lowers a resource graph to target INFRASTRUCTURE (OpenTofu
// / Terraform HCL for a cloud, or a run/compose spec for a self-hosted FOSS backend).
//
// The wavelet telos ("better than Winglang"): one agnostic configuration yields an
// EQUIVALENT deployment on every target — the equivalence is the gate (a config's
// LogicalResource set must match across providers). Foundational abstractions land
// first (Queue, Key/Value, Object Storage), each with a permissive self-hosted
// backend (RabbitMQ/NATS, Valkey, Garage) that runs locally under Podman, so the
// layer is exercisable with no cloud account.
//
// THE SHADOW RULE (CLAUDE.md): infra is a CONSUMER of the kernel, never core. It
// reads checked definitions and emits throwaway deployment artifacts; it adds no
// core constructor and mutates nothing in core/store.
package infra

import "sort"

// Resource is a provider-agnostic infrastructure capability requested by a program
// — a Queue, a KV store, an object Bucket — described by WHAT it provides, not which
// vendor product backs it. Concrete resources are the small sealed set below.
type Resource interface {
	isResource()
	// Kind names the agnostic capability ("queue", "kv", "object").
	Kind() string
	// LogicalName is the user's name for this instance; it becomes the HCL local
	// resource name and the FOSS container/connection name.
	LogicalName() string
}

// Queue is an at-least-once message queue/topic — the agnostic transport. The
// message TYPE is a wootz-side concern (lib/infra/queue.rune); the resource is the
// channel the cloud/backend provisions.
type Queue struct {
	Name string // logical name
	FIFO bool   // ordered, exactly-once-ish delivery where the backend supports it
}

func (Queue) isResource()           {}
func (Queue) Kind() string          { return "queue" }
func (q Queue) LogicalName() string { return q.Name }

// KV is a key/value store keyed on the Redis/Valkey wire protocol (managed Redis is
// the cloud default; Valkey is the FOSS backend).
type KV struct {
	Name       string
	TTLSeconds int // 0 = no default expiry
}

func (KV) isResource()           {}
func (KV) Kind() string          { return "kv" }
func (k KV) LogicalName() string { return k.Name }

// Bucket is object storage on the S3 API equivalence class (S3 / Blob / GCS native,
// Garage self-hosted).
type Bucket struct {
	Name string
}

func (Bucket) isResource()           {}
func (Bucket) Kind() string          { return "object" }
func (b Bucket) LogicalName() string { return b.Name }

// Compute is N replicas of a container Image — the workload substrate a verified
// protocol's actors run on (EC2 / Azure VM / GCE running the OCI image, or local
// Podman). The image is the portable unit; the substrate is the deploy-time choice.
type Compute struct {
	Name     string
	Image    string // OCI image reference (e.g. docker.io/library/erlang:slim)
	Replicas int    // number of replica instances (>= 1)
}

func (Compute) isResource()           {}
func (Compute) Kind() string          { return "compute" }
func (c Compute) LogicalName() string { return c.Name }

// replicas returns a sane replica count (>= 1).
func (c Compute) replicaCount() int {
	if c.Replicas < 1 {
		return 1
	}
	return c.Replicas
}

// image returns the image or a sane default.
func (c Compute) imageRef() string {
	if c.Image == "" {
		return "docker.io/library/erlang:slim"
	}
	return c.Image
}

// Database is a managed relational (SQL) database — a control-plane resource the app
// connects to via the emitted connection config (no data-plane .rune interface; the
// SQL surface is a separate design). PostgreSQL is the anchor engine (RDS / Azure
// Flexible Server / Cloud SQL native, PostgreSQL self-hosted).
type Database struct {
	Name string
}

func (Database) isResource()           {}
func (Database) Kind() string          { return "database" }
func (d Database) LogicalName() string { return d.Name }

// Secret is a managed secret (a named credential the value of which is supplied out
// of band, never in the config). Control-plane: AWS Secrets Manager / Azure Key Vault
// / GCP Secret Manager native, a local dotenv file self-hosted.
type Secret struct {
	Name string
}

func (Secret) isResource()           {}
func (Secret) Kind() string          { return "secret" }
func (s Secret) LogicalName() string { return s.Name }

// NoSQL is a managed key-value / document store (distinct from KV's Redis cache):
// AWS DynamoDB / Azure Cosmos DB / GCP Firestore native, dynamodb-local self-hosted.
type NoSQL struct {
	Name string
}

func (NoSQL) isResource()           {}
func (NoSQL) Kind() string          { return "nosql" }
func (n NoSQL) LogicalName() string { return n.Name }

// DNS is a managed DNS zone (Route 53 / Azure DNS / Cloud DNS native, CoreDNS
// self-hosted). Domain is the zone's domain; it defaults to "<name>.example.com".
type DNS struct {
	Name   string
	Domain string
}

func (DNS) isResource()           {}
func (DNS) Kind() string          { return "dns" }
func (d DNS) LogicalName() string { return d.Name }

// domain returns the zone domain or a sane default.
func (d DNS) domain() string {
	if d.Domain == "" {
		return d.Name + ".example.com"
	}
	return d.Domain
}

// Disk is a block-storage volume (EBS / Azure Managed Disk / GCP Persistent Disk).
// Control-plane, cloud-only (the local equivalent is a plain Podman volume). SizeGB
// defaults to 20.
type Disk struct {
	Name   string
	SizeGB int
}

func (Disk) isResource()           {}
func (Disk) Kind() string          { return "disk" }
func (d Disk) LogicalName() string { return d.Name }

// KMS is a managed encryption key (AWS KMS / Azure Key Vault key / GCP KMS). Control-
// plane, cloud-only. On Azure it shares the Key Vault scaffolding with Secret.
type KMS struct {
	Name string
}

func (KMS) isResource()           {}
func (KMS) Kind() string          { return "kms" }
func (k KMS) LogicalName() string { return k.Name }

// File is a managed shared file system (AWS EFS / Azure Files / GCP Filestore).
// Control-plane, cloud-only. On Azure it shares the storage account with Bucket.
type File struct {
	Name string
}

func (File) isResource()           {}
func (File) Kind() string          { return "file" }
func (f File) LogicalName() string { return f.Name }

// Stream is a managed event stream — distinct from Queue's point-to-point delivery:
// AWS Kinesis / Azure Event Hubs / GCP Pub/Sub. Control-plane, cloud-only.
type Stream struct {
	Name string
}

func (Stream) isResource()           {}
func (Stream) Kind() string          { return "stream" }
func (s Stream) LogicalName() string { return s.Name }

// CDN is a managed content-delivery network (AWS CloudFront / Azure CDN profile /
// GCP Cloud CDN backend bucket) — edge caching in front of an origin. Control-plane,
// cloud-only.
type CDN struct {
	Name string
}

func (CDN) isResource()           {}
func (CDN) Kind() string          { return "cdn" }
func (c CDN) LogicalName() string { return c.Name }

// LoadBalancer is a managed L4/L7 load balancer (AWS ELBv2 / Azure Load Balancer /
// GCP forwarding rule) — traffic distribution across compute replicas. Control-plane,
// cloud-only.
type LoadBalancer struct {
	Name string
}

func (LoadBalancer) isResource()           {}
func (LoadBalancer) Kind() string          { return "lb" }
func (l LoadBalancer) LogicalName() string { return l.Name }

// Metrics is a managed metrics/monitoring workspace (AWS CloudWatch dashboard / Azure
// Monitor workspace = managed Prometheus / GCP Monitoring dashboard) — the metrics
// pillar of observability, complementing Logs. Prometheus is the FOSS backend.
type Metrics struct {
	Name string
}

func (Metrics) isResource()           {}
func (Metrics) Kind() string          { return "metrics" }
func (m Metrics) LogicalName() string { return m.Name }

// Identity is a managed workload identity (AWS IAM role / Azure user-assigned managed
// identity / GCP service account) — the principal a workload runs as. Cloud-only.
type Identity struct {
	Name string
}

func (Identity) isResource()           {}
func (Identity) Kind() string          { return "iam" }
func (i Identity) LogicalName() string { return i.Name }

// K8s is a managed Kubernetes cluster (AWS EKS / Azure AKS / GCP GKE) — the managed-
// containers compute substrate. Control-plane, cloud-only.
type K8s struct {
	Name string
}

func (K8s) isResource()           {}
func (K8s) Kind() string          { return "k8s" }
func (k K8s) LogicalName() string { return k.Name }

// Network is a managed virtual network / VPC (AWS VPC / Azure VNet / GCP VPC).
// Control-plane, cloud-only.
type Network struct {
	Name string
}

func (Network) isResource()           {}
func (Network) Kind() string          { return "network" }
func (n Network) LogicalName() string { return n.Name }

// Firewall is managed edge protection (AWS WAF / Azure DDoS Protection Plan / GCP
// Cloud Armor security policy). Control-plane, cloud-only.
type Firewall struct {
	Name string
}

func (Firewall) isResource()           {}
func (Firewall) Kind() string          { return "firewall" }
func (f Firewall) LogicalName() string { return f.Name }

// Logs is a managed log/observability sink (AWS CloudWatch log group / Azure Log
// Analytics workspace / GCP Cloud Logging bucket). Control-plane, cloud-only.
type Logs struct {
	Name string
}

func (Logs) isResource()           {}
func (Logs) Kind() string          { return "logs" }
func (l Logs) LogicalName() string { return l.Name }

// Registry is a managed container image registry (AWS ECR / Azure ACR / GCP Artifact
// Registry) — where Compute's OCI images live. Control-plane, cloud-only.
type Registry struct {
	Name string
}

func (Registry) isResource()           {}
func (Registry) Kind() string          { return "registry" }
func (r Registry) LogicalName() string { return r.Name }

// PaaS is a managed application platform (AWS Elastic Beanstalk / Azure App Service
// plan / GCP App Engine) — the host a web app runs on. Control-plane, cloud-only.
type PaaS struct {
	Name string
}

func (PaaS) isResource()           {}
func (PaaS) Kind() string          { return "paas" }
func (p PaaS) LogicalName() string { return p.Name }

func (d Disk) sizeGB() int {
	if d.SizeGB < 1 {
		return 20
	}
	return d.SizeGB
}

// LogicalResource is the abstract shape an emitter claims to realize for a given
// resource: the agnostic kind + name, INDEPENDENT of the concrete provider type.
// Two targets are EQUIVALENT for a configuration when their LogicalResource sets are
// equal — this is the "equal config -> equivalent deployment" gate.
type LogicalResource struct {
	Kind string // "queue", "kv", "object"
	Name string
}

// Artifact is an emitted deployment artifact for one target: a set of named files
// (e.g. "main.tf" -> HCL, or "compose.yaml" -> a Podman compose spec) plus the
// logical resource set it realizes (the equivalence witness).
type Artifact struct {
	Files   map[string]string
	Logical []LogicalResource
}

// Emitter lowers a resource graph to an Artifact for one TARGET — a cloud provider
// via OpenTofu/Terraform HCL, or a self-hosted FOSS backend via a run/compose spec.
// One implementation ships per target; an emitter switches on each resource's Kind.
type Emitter interface {
	// Target names the provider/backend ("aws", "azure", "gcp", "rabbitmq",
	// "nats", "valkey", "garage").
	Target() string
	// Cloud reports whether this target is an IaC cloud provider (HCL output) as
	// opposed to a self-hosted FOSS backend (run/compose output).
	Cloud() bool
	// Emit lowers the resources; an unsupported Kind is a clear error, not a panic.
	Emit(rs []Resource) (Artifact, error)
}

// All returns every emitter the infra stratum ships, in registration order: the
// three clouds, then the foundational FOSS backends.
func All() []Emitter {
	return []Emitter{
		AWS{}, Azure{}, GCP{},
		RabbitMQ{}, NATS{}, Valkey{}, Garage{}, Podman{}, Postgres{}, Dotenv{}, DynamoLocal{}, CoreDNS{}, LocalRegistry{}, Redpanda{}, Vault{}, Loki{}, Prometheus{},
	}
}

// targetAliases maps friendly names to canonical Target() values.
var targetAliases = map[string]string{
	"amazon": "aws", "ec2": "aws",
	"azurerm": "azure", "az": "azure",
	"google": "gcp", "gcloud": "gcp", "googlecloud": "gcp",
	"rabbit": "rabbitmq", "amqp": "rabbitmq",
	"container": "podman", "oci": "podman", "docker": "podman",
}

// ByTarget returns the emitter selected by a provider/backend name (canonical or a
// friendly alias), and whether one matched.
func ByTarget(name string) (Emitter, bool) {
	if canon, ok := targetAliases[name]; ok {
		name = canon
	}
	for _, e := range All() {
		if e.Target() == name {
			return e, true
		}
	}
	return nil, false
}

// Targets lists the canonical target names of every shipped emitter.
func Targets() []string {
	out := make([]string, 0, len(All()))
	for _, e := range All() {
		out = append(out, e.Target())
	}
	return out
}

// logicalSet builds the equivalence witness for a resource graph: the agnostic
// (kind, name) pairs, sorted, independent of any provider. Every cloud emitter must
// realize EXACTLY this set for the same input — the equivalence gate compares it.
func logicalSet(rs []Resource) []LogicalResource {
	out := make([]LogicalResource, 0, len(rs))
	for _, r := range rs {
		out = append(out, LogicalResource{Kind: r.Kind(), Name: r.LogicalName()})
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].Kind != out[j].Kind {
			return out[i].Kind < out[j].Kind
		}
		return out[i].Name < out[j].Name
	})
	return out
}
