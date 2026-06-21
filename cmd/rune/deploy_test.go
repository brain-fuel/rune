package main

import (
	"bytes"
	"strings"
	"testing"
)

// TestDeployCloud checks `rune deploy` emits provider HCL for a cloud queue.
func TestDeployCloud(t *testing.T) {
	var out bytes.Buffer
	err := runDeploy([]string{"--resource", "queue", "--name", "orders", "--backend", "aws"}, &out)
	if err != nil {
		t.Fatalf("deploy aws queue: %v", err)
	}
	s := out.String()
	for _, want := range []string{"# main.tf", "aws_sqs_queue", "\"orders\""} {
		if !strings.Contains(s, want) {
			t.Errorf("output missing %q\n%s", want, s)
		}
	}
}

// TestDeployFOSS checks a self-hosted backend emits a Compose spec + connection env.
func TestDeployFOSS(t *testing.T) {
	var out bytes.Buffer
	if err := runDeploy([]string{"--resource", "queue", "--name", "orders", "--backend", "nats"}, &out); err != nil {
		t.Fatalf("deploy nats queue: %v", err)
	}
	s := out.String()
	for _, want := range []string{"# compose.yaml", "nats:latest", "# connection.env", "WAVELET_QUEUE_BACKEND=nats"} {
		if !strings.Contains(s, want) {
			t.Errorf("output missing %q\n%s", want, s)
		}
	}
}

// TestDeployErrors checks the verb rejects bad input clearly.
func TestDeployErrors(t *testing.T) {
	cases := []struct {
		name string
		args []string
		want string
	}{
		{"missing flags", []string{"--resource", "queue"}, "needs --resource"},
		{"unknown backend", []string{"--resource", "queue", "--name", "q", "--backend", "nope"}, "unknown backend"},
		{"unknown resource", []string{"--resource", "blob", "--name", "q", "--backend", "aws"}, "unknown resource"},
		{"unsupported kind", []string{"--resource", "kv", "--name", "c", "--backend", "rabbitmq"}, "does not yet support"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			var out bytes.Buffer
			err := runDeploy(c.args, &out)
			if err == nil || !strings.Contains(err.Error(), c.want) {
				t.Errorf("err = %v, want containing %q", err, c.want)
			}
		})
	}
}
