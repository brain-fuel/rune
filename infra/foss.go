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

// queueOnly extracts the Queue resources or errors on any other kind (the FOSS
// queue backends, RabbitMQ/NATS, serve only the queue abstraction).
func queueOnly(target string, rs []Resource) ([]Queue, error) {
	var qs []Queue
	for _, r := range rs {
		q, ok := r.(Queue)
		if !ok {
			return nil, unsupported(target, r)
		}
		qs = append(qs, q)
	}
	return qs, nil
}

// RabbitMQ is the self-hosted AMQP queue backend.
type RabbitMQ struct{}

func (RabbitMQ) Target() string { return "rabbitmq" }
func (RabbitMQ) Cloud() bool    { return false }

func (RabbitMQ) Emit(rs []Resource) (Artifact, error) {
	qs, err := queueOnly("rabbitmq", rs)
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
	for _, q := range qs {
		env["WAVELET_QUEUE_"+strings.ToUpper(q.Name)] = q.Name
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
	qs, err := queueOnly("nats", rs)
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
	for _, q := range qs {
		env["WAVELET_QUEUE_"+strings.ToUpper(q.Name)] = q.Name
	}
	return Artifact{
		Files:   map[string]string{"compose.yaml": compose, "connection.env": envFile(env)},
		Logical: logicalSet(rs),
	}, nil
}

// Valkey is the self-hosted key/value backend (Redis wire protocol). Its lowering
// lands with the KV abstraction (slice 1, task 5); registered now so the target
// resolves.
type Valkey struct{}

func (Valkey) Target() string { return "valkey" }
func (Valkey) Cloud() bool    { return false }

func (Valkey) Emit(rs []Resource) (Artifact, error) {
	for _, r := range rs {
		return Artifact{}, unsupported("valkey", r)
	}
	return Artifact{Files: map[string]string{}, Logical: logicalSet(rs)}, nil
}

// Garage is the self-hosted object-storage backend (S3-compatible). Its lowering
// lands with the Object abstraction (slice 1, task 5); registered now so the target
// resolves.
type Garage struct{}

func (Garage) Target() string { return "garage" }
func (Garage) Cloud() bool    { return false }

func (Garage) Emit(rs []Resource) (Artifact, error) {
	for _, r := range rs {
		return Artifact{}, unsupported("garage", r)
	}
	return Artifact{Files: map[string]string{}, Logical: logicalSet(rs)}, nil
}
