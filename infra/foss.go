package infra

import (
	"fmt"
	"sort"
	"strings"
)

// The FOSS emitters lower the agnostic resource graph to a self-hosted backend that
// runs locally under Podman (Buildah/Podman, Apache-2.0): a Compose spec to bring
// the service up plus a connection.env the wootz program reads. These make the whole
// layer exercisable with NO cloud account — the Savage on-ramp and the CI gate. Each
// realizes the same agnostic LogicalResource set as the clouds, so equal config
// yields an equivalent (here, local) deployment.

// composeService renders one podman-compose / docker-compose service stanza.
func composeService(name, image string, ports []string, command string) string {
	var b strings.Builder
	fmt.Fprintf(&b, "  %s:\n", name)
	fmt.Fprintf(&b, "    image: %s\n", image)
	if command != "" {
		fmt.Fprintf(&b, "    command: %s\n", command)
	}
	if len(ports) > 0 {
		b.WriteString("    ports:\n")
		for _, p := range ports {
			fmt.Fprintf(&b, "      - %q\n", p)
		}
	}
	return b.String()
}

// envFile renders a deterministic KEY=value connection file.
func envFile(kv map[string]string) string {
	keys := make([]string, 0, len(kv))
	for k := range kv {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	var b strings.Builder
	for _, k := range keys {
		fmt.Fprintf(&b, "%s=%s\n", k, kv[k])
	}
	return b.String()
}

// onlyKind checks every resource is the backend's one supported kind, returning
// their logical names (each FOSS backend serves exactly one abstraction).
func onlyKind(target, kind string, rs []Resource) ([]string, error) {
	var names []string
	for _, r := range rs {
		if r.Kind() != kind {
			return nil, unsupported(target, r)
		}
		names = append(names, r.LogicalName())
	}
	return names, nil
}

// RabbitMQ is the self-hosted AMQP queue backend.
type RabbitMQ struct{}

func (RabbitMQ) Target() string { return "rabbitmq" }
func (RabbitMQ) Cloud() bool    { return false }

func (RabbitMQ) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("rabbitmq", "queue", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - RabbitMQ backend for the wavelet \"queue\" abstraction.\n" +
		"# bring up:  podman-compose up -d\n" +
		"services:\n" +
		composeService("rabbitmq", "docker.io/library/rabbitmq:3-management",
			[]string{"5672:5672", "15672:15672"}, "")
	env := map[string]string{
		"WAVELET_QUEUE_BACKEND": "rabbitmq",
		"WAVELET_QUEUE_URL":     "amqp://guest:guest@localhost:5672/",
	}
	for _, n := range names {
		env["WAVELET_QUEUE_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// NATS is the self-hosted queue backend (JetStream enabled for persistence).
type NATS struct{}

func (NATS) Target() string { return "nats" }
func (NATS) Cloud() bool    { return false }

func (NATS) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("nats", "queue", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - NATS (JetStream) backend for the wavelet \"queue\" abstraction.\n" +
		"# bring up:  podman-compose up -d\n" +
		"services:\n" +
		composeService("nats", "docker.io/library/nats:latest",
			[]string{"4222:4222", "8222:8222"}, "[\"-js\"]")
	env := map[string]string{
		"WAVELET_QUEUE_BACKEND": "nats",
		"WAVELET_QUEUE_URL":     "nats://localhost:4222",
	}
	for _, n := range names {
		env["WAVELET_QUEUE_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// Podman is the local-container compute backend: it runs the workload's OCI image as
// N named replica services (stable names + a PEERS list, gossip-friendly), so a
// verified protocol's actors run locally with no cloud account — the Savage on-ramp
// and the CI gate for the compute substrate. Buildah/Podman are Apache-2.0.
type Podman struct{}

func (Podman) Target() string { return "podman" }
func (Podman) Cloud() bool    { return false }

func (Podman) Emit(rs []Resource) (Artifact, error) {
	var services strings.Builder
	env := map[string]string{"WAVELET_COMPUTE_BACKEND": "podman"}
	for _, r := range rs {
		c, ok := r.(Compute)
		if !ok {
			return Artifact{}, unsupported("podman", r)
		}
		n := c.replicaCount()
		peers := make([]string, n)
		for i := 0; i < n; i++ {
			peers[i] = fmt.Sprintf("%s-%d", c.Name, i)
		}
		peerList := strings.Join(peers, ",")
		for i := 0; i < n; i++ {
			fmt.Fprintf(&services, "  %s-%d:\n", c.Name, i)
			fmt.Fprintf(&services, "    image: %s\n", c.imageRef())
			services.WriteString("    environment:\n")
			fmt.Fprintf(&services, "      - REPLICA_ID=%d\n", i)
			fmt.Fprintf(&services, "      - PEERS=%s\n", peerList)
		}
		env["WAVELET_COMPUTE_"+strings.ToUpper(c.Name)+"_REPLICAS"] = fmt.Sprintf("%d", n)
		env["WAVELET_COMPUTE_"+strings.ToUpper(c.Name)+"_PEERS"] = peerList
	}
	compose := "# podman-compose spec - local compute backend for the wavelet \"compute\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (N replica services, peers wired by env)\n" +
		"services:\n" + services.String()
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// Postgres is the self-hosted relational-database backend (the PostgreSQL anchor
// engine — the same wire protocol/driver serves managed RDS/Flexible Server/Cloud
// SQL and self-hosted Postgres).
type Postgres struct{}

func (Postgres) Target() string { return "postgres" }
func (Postgres) Cloud() bool    { return false }

func (Postgres) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("postgres", "database", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - PostgreSQL backend for the wavelet \"database\" abstraction.\n" +
		"# bring up:  podman-compose up -d\n" +
		"services:\n" +
		"  postgres:\n" +
		"    image: docker.io/library/postgres:16\n" +
		"    environment:\n" +
		"      - POSTGRES_USER=wavelet\n" +
		"      - POSTGRES_PASSWORD=wavelet\n" +
		"    ports:\n" +
		"      - \"5432:5432\"\n"
	env := map[string]string{
		"WAVELET_DATABASE_BACKEND": "postgres",
		"WAVELET_DATABASE_URL":     "postgres://wavelet:wavelet@localhost:5432/wavelet",
	}
	for _, n := range names {
		env["WAVELET_DATABASE_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// Dotenv is the local self-hosted secret backend: a `secrets.env` template the
// developer fills in, no service to run. The data stays out of the committed config
// (the template carries only the keys), matching the cloud secret stores' contract.
type Dotenv struct{}

func (Dotenv) Target() string { return "dotenv" }
func (Dotenv) Cloud() bool    { return false }

func (Dotenv) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("dotenv", "secret", rs)
	if err != nil {
		return Artifact{}, err
	}
	var secrets strings.Builder
	secrets.WriteString("# wavelet secrets template - fill in values, do NOT commit the filled file.\n")
	env := map[string]string{"WAVELET_SECRET_BACKEND": "dotenv"}
	for _, n := range names {
		fmt.Fprintf(&secrets, "%s=\n", strings.ToUpper(n))
		env["WAVELET_SECRET_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"secrets.env": secrets.String(), "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// DynamoLocal is the self-hosted NoSQL backend: Amazon's DynamoDB-local emulator
// (Apache-2.0), which speaks the real DynamoDB API, so dev matches the AWS target.
type DynamoLocal struct{}

func (DynamoLocal) Target() string { return "dynamodb" }
func (DynamoLocal) Cloud() bool    { return false }

func (DynamoLocal) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("dynamodb", "nosql", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - DynamoDB-local backend for the wavelet \"nosql\" abstraction.\n" +
		"# bring up:  podman-compose up -d\n" +
		"services:\n" +
		composeService("dynamodb", "docker.io/amazon/dynamodb-local:latest",
			[]string{"8000:8000"}, "")
	env := map[string]string{
		"WAVELET_NOSQL_BACKEND":  "dynamodb",
		"WAVELET_NOSQL_ENDPOINT": "http://localhost:8000",
	}
	for _, n := range names {
		env["WAVELET_NOSQL_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// CoreDNS is the self-hosted DNS backend (CoreDNS, Apache-2.0). The compose mounts a
// minimal Corefile serving the zone(s) from a hosts file.
type CoreDNS struct{}

func (CoreDNS) Target() string { return "coredns" }
func (CoreDNS) Cloud() bool    { return false }

func (CoreDNS) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("coredns", "dns", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - CoreDNS backend for the wavelet \"dns\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (Corefile is mounted from this dir)\n" +
		"services:\n" +
		"  coredns:\n" +
		"    image: docker.io/coredns/coredns:latest\n" +
		"    command: [\"-conf\", \"/etc/coredns/Corefile\"]\n" +
		"    ports:\n" +
		"      - \"5353:53/udp\"\n" +
		"    volumes:\n" +
		"      - ./Corefile:/etc/coredns/Corefile:ro\n"
	corefile := ".:53 {\n    health\n    log\n    errors\n    forward . 1.1.1.1\n}\n"
	env := map[string]string{
		"WAVELET_DNS_BACKEND": "coredns",
		"WAVELET_DNS_ADDR":    "localhost:5353",
	}
	for _, n := range names {
		env["WAVELET_DNS_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files: map[string]string{
			"compose.yaml":   compose,
			"Corefile":       corefile,
			"connection.env": envFile(env),
		},
		Logical: logicalSet(rs),
	}, nil
}

// Valkey is the self-hosted key/value backend (Redis wire protocol — the same
// client serves managed Redis and self-hosted Valkey).
type Valkey struct{}

func (Valkey) Target() string { return "valkey" }
func (Valkey) Cloud() bool    { return false }

func (Valkey) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("valkey", "kv", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - Valkey backend for the wavelet \"kv\" abstraction.\n" +
		"# bring up:  podman-compose up -d\n" +
		"services:\n" +
		composeService("valkey", "docker.io/valkey/valkey:8",
			[]string{"6379:6379"}, "")
	env := map[string]string{
		"WAVELET_KV_BACKEND": "valkey",
		"WAVELET_KV_URL":     "redis://localhost:6379",
	}
	for _, n := range names {
		env["WAVELET_KV_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// Garage is the self-hosted object-storage backend (S3-compatible — the same S3
// client serves AWS S3 and self-hosted Garage). It needs a small config file, which
// the compose spec mounts.
type Garage struct{}

func (Garage) Target() string { return "garage" }
func (Garage) Cloud() bool    { return false }

func (Garage) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("garage", "object", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - Garage (S3-compatible) backend for the wavelet \"object\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (garage.toml is mounted from this dir)\n" +
		"services:\n" +
		"  garage:\n" +
		"    image: docker.io/dxflrs/garage:v1.0.1\n" +
		"    ports:\n" +
		"      - \"3900:3900\"\n" +
		"      - \"3903:3903\"\n" +
		"    volumes:\n" +
		"      - ./garage.toml:/etc/garage.toml:ro\n"
	garageToml := "# minimal single-node Garage config for local development.\n" +
		"metadata_dir = \"/var/lib/garage/meta\"\n" +
		"data_dir = \"/var/lib/garage/data\"\n" +
		"db_engine = \"sqlite\"\n" +
		"replication_factor = 1\n" +
		"rpc_bind_addr = \"[::]:3901\"\n" +
		"[s3_api]\n" +
		"s3_region = \"garage\"\n" +
		"api_bind_addr = \"[::]:3900\"\n" +
		"root_domain = \".s3.garage.localhost\"\n" +
		"[admin]\n" +
		"api_bind_addr = \"[::]:3903\"\n"
	env := map[string]string{
		"WAVELET_OBJECT_BACKEND":  "garage",
		"WAVELET_OBJECT_ENDPOINT": "http://localhost:3900",
		"WAVELET_OBJECT_REGION":   "garage",
	}
	for _, n := range names {
		env["WAVELET_OBJECT_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files: map[string]string{
			"compose.yaml":   compose,
			"garage.toml":    garageToml,
			"connection.env": envFile(env),
		},
		Logical: logicalSet(rs),
	}, nil
}

// Redpanda is the self-hosted event-stream backend (a Kafka-API-compatible single
// binary — the same Kafka client serves managed Kinesis/Event Hubs/Pub-Sub-via-Kafka
// and this local broker). It backs the "stream" abstraction with NO cloud account.
type Redpanda struct{}

func (Redpanda) Target() string { return "redpanda" }
func (Redpanda) Cloud() bool    { return false }

func (Redpanda) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("redpanda", "stream", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - Redpanda (Kafka-API) backend for the wavelet \"stream\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (Kafka API on localhost:9092)\n" +
		"services:\n" +
		composeService("redpanda", "docker.io/redpandadata/redpanda:v24.2.7",
			[]string{"9092:9092", "9644:9644"},
			"redpanda start --overprovisioned --smp 1 --memory 1G --reserve-memory 0M --node-id 0 --check=false")
	env := map[string]string{
		"WAVELET_STREAM_BACKEND": "redpanda",
		"WAVELET_STREAM_BROKERS": "localhost:9092",
	}
	for _, n := range names {
		env["WAVELET_STREAM_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// Loki is the self-hosted log-aggregation backend (Grafana Loki — a single-binary
// log store, the local equivalent of CloudWatch Logs / Log Analytics / Cloud Logging).
// It backs the "logs" abstraction with NO cloud account.
type Loki struct{}

func (Loki) Target() string { return "loki" }
func (Loki) Cloud() bool    { return false }

func (Loki) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("loki", "logs", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - Grafana Loki backend for the wavelet \"logs\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (push logs to http://localhost:3100/loki/api/v1/push)\n" +
		"services:\n" +
		composeService("loki", "docker.io/grafana/loki:3.2.0",
			[]string{"3100:3100"}, "-config.file=/etc/loki/local-config.yaml")
	env := map[string]string{
		"WAVELET_LOGS_BACKEND": "loki",
		"WAVELET_LOGS_URL":     "http://localhost:3100",
	}
	for _, n := range names {
		env["WAVELET_LOGS_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// VaultKMS is the self-hosted KMS backend (HashiCorp Vault's transit secrets engine —
// encryption-as-a-service, the canonical self-hosted equivalent of AWS KMS / Azure Key
// Vault key / GCP KMS). It backs the "kms" abstraction with NO cloud account; enable
// the transit engine after start with `vault secrets enable transit`.
type VaultKMS struct{}

func (VaultKMS) Target() string { return "vaultkms" }
func (VaultKMS) Cloud() bool    { return false }

func (VaultKMS) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("vaultkms", "kms", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - Vault transit (encryption-as-a-service) backend for the wavelet \"kms\" abstraction.\n" +
		"# bring up:  podman-compose up -d   then  vault secrets enable transit  (API on http://localhost:8200, token \"root\")\n" +
		"services:\n" +
		composeService("vault-kms", "docker.io/hashicorp/vault:1.18",
			[]string{"8200:8200"},
			"server -dev -dev-root-token-id=root -dev-listen-address=0.0.0.0:8200")
	env := map[string]string{
		"WAVELET_KMS_BACKEND": "vaulttransit",
		"WAVELET_KMS_ADDR":    "http://localhost:8200",
		"WAVELET_KMS_TOKEN":   "root",
	}
	for _, n := range names {
		env["WAVELET_KMS_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// NFS is the self-hosted shared-filesystem backend (an NFS server container — the
// local equivalent of EFS / Azure Files / Filestore). It backs the "file" abstraction
// with NO cloud account. Emitted inline (the server needs `privileged: true` and an
// exported directory).
type NFS struct{}

func (NFS) Target() string { return "nfs" }
func (NFS) Cloud() bool    { return false }

func (NFS) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("nfs", "file", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - NFS server backend for the wavelet \"file\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (mount localhost:/exports from a client)\n" +
		"services:\n" +
		"  nfs:\n" +
		"    image: docker.io/itsthenetwork/nfs-server-alpine:12\n" +
		"    privileged: true\n" +
		"    ports:\n" +
		"      - \"2049:2049\"\n" +
		"    environment:\n" +
		"      - SHARED_DIRECTORY=/exports\n" +
		"    volumes:\n" +
		"      - ./exports:/exports\n"
	env := map[string]string{
		"WAVELET_FILE_BACKEND": "nfs",
		"WAVELET_FILE_EXPORT":  "localhost:/exports",
	}
	for _, n := range names {
		env["WAVELET_FILE_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// K3s is the self-hosted Kubernetes backend (Rancher k3s — a single-binary, fully
// conformant Kubernetes distribution; the local equivalent of EKS/AKS/GKE). It backs
// the "k8s" abstraction with NO cloud account. Emitted inline (not via composeService)
// because the k3s server needs `privileged: true` and a token.
type K3s struct{}

func (K3s) Target() string { return "k3s" }
func (K3s) Cloud() bool    { return false }

func (K3s) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("k3s", "k8s", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - k3s (lightweight Kubernetes) backend for the wavelet \"k8s\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (API server on https://localhost:6443, token \"wavelet\")\n" +
		"services:\n" +
		"  k3s-server:\n" +
		"    image: docker.io/rancher/k3s:v1.30.5-k3s1\n" +
		"    command: server\n" +
		"    privileged: true\n" +
		"    ports:\n" +
		"      - \"6443:6443\"\n" +
		"    environment:\n" +
		"      - K3S_TOKEN=wavelet\n"
	env := map[string]string{
		"WAVELET_K8S_BACKEND": "k3s",
		"WAVELET_K8S_SERVER":  "https://localhost:6443",
		"WAVELET_K8S_TOKEN":   "wavelet",
	}
	for _, n := range names {
		env["WAVELET_K8S_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// Prometheus is the self-hosted metrics backend (Prometheus — the de-facto metrics
// store, the local equivalent of CloudWatch / Azure Monitor / Cloud Monitoring). It
// backs the "metrics" abstraction with NO cloud account.
type Prometheus struct{}

func (Prometheus) Target() string { return "prometheus" }
func (Prometheus) Cloud() bool    { return false }

func (Prometheus) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("prometheus", "metrics", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - Prometheus backend for the wavelet \"metrics\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (UI + API on http://localhost:9090)\n" +
		"services:\n" +
		composeService("prometheus", "docker.io/prom/prometheus:v2.54.1",
			[]string{"9090:9090"}, "")
	env := map[string]string{
		"WAVELET_METRICS_BACKEND": "prometheus",
		"WAVELET_METRICS_URL":     "http://localhost:9090",
	}
	for _, n := range names {
		env["WAVELET_METRICS_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// Vault is the self-hosted secrets backend (HashiCorp Vault in dev mode — a real
// running secret store, richer than the keys-only `Dotenv` template). It backs the
// "secret" abstraction with NO cloud account; the API surface mirrors the managed
// Secrets Manager / Key Vault / Secret Manager clients.
type Vault struct{}

func (Vault) Target() string { return "vault" }
func (Vault) Cloud() bool    { return false }

func (Vault) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("vault", "secret", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - HashiCorp Vault (dev mode) backend for the wavelet \"secret\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (API + UI on http://localhost:8200, token \"root\")\n" +
		"services:\n" +
		composeService("vault", "docker.io/hashicorp/vault:1.18",
			[]string{"8200:8200"},
			"server -dev -dev-root-token-id=root -dev-listen-address=0.0.0.0:8200")
	env := map[string]string{
		"WAVELET_SECRET_BACKEND": "vault",
		"WAVELET_SECRET_ADDR":    "http://localhost:8200",
		"WAVELET_SECRET_TOKEN":   "root",
	}
	for _, n := range names {
		env["WAVELET_SECRET_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// LocalRegistry is the self-hosted container-registry backend (the CNCF Distribution
// `registry:2` image — the same OCI client serves ECR/ACR/Artifact Registry and this
// local registry). It backs the "registry" abstraction with NO cloud account, so a
// workload's images can be pushed/pulled entirely on the dev host.
type LocalRegistry struct{}

func (LocalRegistry) Target() string { return "localregistry" }
func (LocalRegistry) Cloud() bool    { return false }

func (LocalRegistry) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("localregistry", "registry", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - Distribution (registry:2) backend for the wavelet \"registry\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (push to localhost:5000/<image>)\n" +
		"services:\n" +
		composeService("registry", "docker.io/library/registry:2",
			[]string{"5000:5000"}, "")
	env := map[string]string{
		"WAVELET_REGISTRY_BACKEND": "localregistry",
		"WAVELET_REGISTRY_URL":     "localhost:5000",
	}
	for _, n := range names {
		env["WAVELET_REGISTRY_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// ClickHouse is the self-hosted analytical-warehouse backend (the ClickHouse OLAP
// engine, Apache-2.0) — a columnar SQL warehouse that backs the "warehouse"
// abstraction with NO cloud account, the self-hosted form of Redshift/Kusto/BigQuery.
// HTTP on 8123, native protocol on 9000.
type ClickHouse struct{}

func (ClickHouse) Target() string { return "clickhouse" }
func (ClickHouse) Cloud() bool    { return false }

func (ClickHouse) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("clickhouse", "warehouse", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - ClickHouse backend for the wavelet \"warehouse\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (HTTP localhost:8123, native localhost:9000)\n" +
		"services:\n" +
		"  clickhouse:\n" +
		"    image: docker.io/clickhouse/clickhouse-server:latest\n" +
		"    environment:\n" +
		"      - CLICKHOUSE_USER=wavelet\n" +
		"      - CLICKHOUSE_PASSWORD=wavelet\n" +
		"    ports:\n" +
		"      - \"8123:8123\"\n" +
		"      - \"9000:9000\"\n"
	env := map[string]string{
		"WAVELET_WAREHOUSE_BACKEND": "clickhouse",
		"WAVELET_WAREHOUSE_URL":     "http://wavelet:wavelet@localhost:8123",
	}
	for _, n := range names {
		env["WAVELET_WAREHOUSE_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// Ollama is the self-hosted inference backend (Ollama, MIT) — a single container that
// serves local models over an OpenAI-compatible HTTP API on port 11434, the self-hosted
// form of SageMaker/Azure-ML/Vertex endpoints with NO cloud account and no GPU service.
type Ollama struct{}

func (Ollama) Target() string { return "ollama" }
func (Ollama) Cloud() bool    { return false }

func (Ollama) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("ollama", "inference", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - Ollama backend for the wavelet \"inference\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (OpenAI-compatible API on localhost:11434)\n" +
		"services:\n" +
		"  ollama:\n" +
		"    image: docker.io/ollama/ollama:latest\n" +
		"    ports:\n" +
		"      - \"11434:11434\"\n"
	env := map[string]string{
		"WAVELET_INFERENCE_BACKEND": "ollama",
		"WAVELET_INFERENCE_URL":     "http://localhost:11434",
	}
	for _, n := range names {
		env["WAVELET_INFERENCE_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// MinIO is the self-hosted archival/object backend (MinIO, AGPL-3) — an S3-API server in
// a single container, the self-hosted form of Glacier / Azure Archive / GCS ARCHIVE. The
// same S3 client serves the cloud cold tiers and this local server, NO cloud account.
type MinIO struct{}

func (MinIO) Target() string { return "minio" }
func (MinIO) Cloud() bool    { return false }

func (MinIO) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("minio", "archive", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - MinIO backend for the wavelet \"archive\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (S3 API on localhost:9000, console on 9001)\n" +
		"services:\n" +
		"  minio:\n" +
		"    image: docker.io/minio/minio:latest\n" +
		"    command: server /data --console-address \":9001\"\n" +
		"    environment:\n" +
		"      - MINIO_ROOT_USER=wavelet\n" +
		"      - MINIO_ROOT_PASSWORD=wavelet123\n" +
		"    ports:\n" +
		"      - \"9000:9000\"\n" +
		"      - \"9001:9001\"\n"
	env := map[string]string{
		"WAVELET_ARCHIVE_BACKEND": "minio",
		"WAVELET_ARCHIVE_URL":     "http://wavelet:wavelet123@localhost:9000",
	}
	for _, n := range names {
		env["WAVELET_ARCHIVE_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// Fn is the self-hosted serverless backend (Fn Project, Apache-2.0) — a single-container
// function runtime exposing an HTTP API on port 8080, the self-hosted form of Lambda /
// Azure Functions / Cloud Functions with NO cloud account and no orchestrator.
type Fn struct{}

func (Fn) Target() string { return "fn" }
func (Fn) Cloud() bool    { return false }

func (Fn) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("fn", "serverless", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - Fn Project backend for the wavelet \"serverless\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (function API on localhost:8080)\n" +
		"services:\n" +
		"  fnserver:\n" +
		"    image: docker.io/fnproject/fnserver:latest\n" +
		"    ports:\n" +
		"      - \"8080:8080\"\n"
	env := map[string]string{
		"WAVELET_SERVERLESS_BACKEND": "fn",
		"WAVELET_SERVERLESS_URL":     "http://localhost:8080",
	}
	for _, n := range names {
		env["WAVELET_SERVERLESS_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// Woodpecker is the self-hosted CI/CD backend (Woodpecker CI, Apache-2.0) — a single-
// container pipeline server on port 8000, the self-hosted form of CodeBuild / Azure
// Pipelines / Cloud Build with NO cloud account.
type Woodpecker struct{}

func (Woodpecker) Target() string { return "woodpecker" }
func (Woodpecker) Cloud() bool    { return false }

func (Woodpecker) Emit(rs []Resource) (Artifact, error) {
	names, err := onlyKind("woodpecker", "devops", rs)
	if err != nil {
		return Artifact{}, err
	}
	compose := "# podman-compose spec - Woodpecker CI backend for the wavelet \"devops\" abstraction.\n" +
		"# bring up:  podman-compose up -d   (CI server on localhost:8000)\n" +
		"services:\n" +
		"  woodpecker:\n" +
		"    image: docker.io/woodpeckerci/woodpecker-server:latest\n" +
		"    environment:\n" +
		"      - WOODPECKER_OPEN=true\n" +
		"    ports:\n" +
		"      - \"8000:8000\"\n"
	env := map[string]string{
		"WAVELET_DEVOPS_BACKEND": "woodpecker",
		"WAVELET_DEVOPS_URL":     "http://localhost:8000",
	}
	for _, n := range names {
		env["WAVELET_DEVOPS_"+strings.ToUpper(n)] = n
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}
