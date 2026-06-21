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
