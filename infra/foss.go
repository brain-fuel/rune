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
