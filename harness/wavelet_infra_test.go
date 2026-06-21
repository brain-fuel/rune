package harness

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

// TestWaveletQueueInterface is the acceptance gate for the agnostic QUEUE surface
// (lib/infra/queue.rune): it elaborates and type-checks on top of the REPL prelude,
// so the wavelet queue abstraction is real, checked wootz — not a sketch. The foreign
// queue ops are assume-tier axioms here; `rune deploy` binds them per backend (SQS /
// Service Bus / Pub-Sub / RabbitMQ / NATS).
func TestWaveletQueueInterface(t *testing.T) {
	s := session.New()
	prelude, err := os.ReadFile(filepath.Join("..", "internal", "repl", "prelude.rune"))
	if err != nil {
		t.Fatalf("read prelude: %v", err)
	}
	if _, err := s.LoadSource(string(prelude)); err != nil {
		t.Fatalf("prelude failed to load: %v", err)
	}
	src, err := os.ReadFile(filepath.Join("..", "lib", "infra", "queue.rune"))
	if err != nil {
		t.Fatalf("read queue.rune: %v", err)
	}
	names, err := s.LoadSource(string(src))
	if err != nil {
		t.Fatalf("lib/infra/queue.rune did not type-check: %v", err)
	}
	got := strings.Join(names, " ")
	for _, want := range []string{"enqueue", "dequeue", "QMsg"} {
		if !strings.Contains(got, want) {
			t.Errorf("queue interface missing %q (bound: %s)", want, got)
		}
	}
}
