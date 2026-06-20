package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/surface"
)

// The v1.0.0 freeze criterion (ref_docs/rune-v1-design.md): every listing in
// the book ELABORATES, CHECKS, and RUNS against this core. listings/ holds the
// book's code; this file is the gate that enforces the criterion. A listing
// that stops loading, a marked expression that stops normalizing to its
// expected form, or an emitted chapter that stops running is a v1 regression.

// loadListing reads and loads one listing file into a fresh session.
func loadListing(t *testing.T, name string) *session.Session {
	t.Helper()
	src, err := os.ReadFile(filepath.Join("..", "listings", name))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("%s does not check: %v", name, err)
	}
	return s
}

// normalizesTo asserts that expr, elaborated against the session, normalizes
// to want.
func normalizesTo(t *testing.T, s *session.Session, expr, want string) {
	t.Helper()
	e, err := s.ParseSrcExpr(expr)
	if err != nil {
		t.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatalf("%s does not elaborate: %v", expr, err)
	}
	got := surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames())
	if got != want {
		t.Fatalf("%s normalized to %q, want %q", expr, got, want)
	}
}

func TestListingsElaborateAndCheck(t *testing.T) {
	entries, err := os.ReadDir(filepath.Join("..", "listings"))
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) < 5 {
		t.Fatalf("expected the book's chapters in listings/, found %d files", len(entries))
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".rune") {
			continue
		}
		t.Run(e.Name(), func(t *testing.T) { loadListing(t, e.Name()) })
	}
}

func TestListingsRun(t *testing.T) {
	t.Run("ch01", func(t *testing.T) {
		s := loadListing(t, "ch01_functions.rune")
		// idU maps small types; applied to nothing it δβ-normalizes to the
		// identity lambda (U itself is not a member of U).
		normalizesTo(t, s, `idU`, "fn (x : U) is x end")
	})
	t.Run("ch02", func(t *testing.T) {
		s := loadListing(t, "ch02_implicits.rune")
		normalizesTo(t, s, `id U`, "U")
	})
	t.Run("ch03", func(t *testing.T) {
		s := loadListing(t, "ch03_equality.rune")
		// cast between convertible endpoints reduces away under the binder.
		normalizesTo(t, s, `transport U U (refl U)`, "fn (x : U) is x end")
	})
	t.Run("ch04", func(t *testing.T) {
		s := loadListing(t, "ch04_data.rune")
		normalizesTo(t, s, `four`, "succ (succ (succ (succ zero)))")
		normalizesTo(t, s, `not (not true)`, "true")
		normalizesTo(t, s, `length Nat (cons Nat zero (nil Nat))`, "succ zero")
		// The induction proof COMPUTES at canonical numerals.
		normalizesTo(t, s, `addZeroRight (succ zero)`, "refl (succ zero)")
	})
	t.Run("ch05", func(t *testing.T) {
		s := loadListing(t, "ch05_quantities.rune")
		normalizesTo(t, s, `eid U`, "U")
	})
	t.Run("ch06", func(t *testing.T) {
		s := loadListing(t, "ch06_quotients.rune")
		// The quotient ι-rule: a lift computes on points.
		normalizesTo(t, s, `parityOfTwo`, "true")
		normalizesTo(t, s, `parity odd`, "false")
	})
	t.Run("ch07", func(t *testing.T) {
		s := loadListing(t, "ch07_integers.rune")
		// Lifted arithmetic reduces through the quotient on representatives.
		normalizesTo(t, s, `fst (padd (npair (succ zero) zero) (npair (succ zero) zero))`,
			"succ (succ zero)")
	})
	t.Run("ch08", func(t *testing.T) {
		s := loadListing(t, "ch08_truncation.rune")
		// The squash eliminator is plain application: it β-reduces away. (The
		// printer's lambda annotations are canonically U — GRAMMAR.md §8.)
		normalizesTo(t, s, `squashElim (Squash Nat) (fn (x : Nat) is squash x end) someNat`,
			"fn (P : U) (k : U) is k zero end")
	})
	t.Run("ch09", func(t *testing.T) {
		s := loadListing(t, "ch09_two_level.rune")
		// Decoding computes: an inner function is a plain function.
		normalizesTo(t, s, `four`, "succ (succ (succ (succ zero)))")
		// The inner J computes on preflF: sym(refl) is refl, one level in.
		normalizesTo(t, s, `psymComputes`, "preflF (fib Nat) (succ (succ zero))")
	})
	t.Run("ch10", func(t *testing.T) {
		s := loadListing(t, "ch10_univalence.rune")
		// Transport THROUGH a postulated ua-path computes (the v3 idiom)...
		normalizesTo(t, s, `flipped`, "false")
		normalizesTo(t, s, `backAgain`, "true")
		// ...and along reflexivity it is the identity.
		normalizesTo(t, s, `same`, "true")
	})
	t.Run("ch11", func(t *testing.T) {
		s := loadListing(t, "ch11_arithmetic.rune")
		// The whole ergonomics ladder computing at once: literals, infix,
		// case, fuel-style Euclid — conversion does arithmetic.
		// C7 / R-NUM: closed nat results normalize to a CANONICAL NatLit (the
		// Decision-0 folding ι folds succ-of-literal back into the literal), so
		// they print as a digit rather than a succ-chain.
		normalizesTo(t, s, `17 // 5`, "3")
		normalizesTo(t, s, `gcd 12 18`, "6")
	})
	t.Run("ch12", func(t *testing.T) {
		s := loadListing(t, "ch12_integer_division.rune")
		// Floor vs truncate, live: −7 // 2 is −4 (canonical pair (0,4)),
		// quot −7 2 is −3 — conversion computing through the quotient.
		normalizesTo(t, s, `obs (zneg (intOf 7) // intOf 2)`,
			"npair 0 4")
		normalizesTo(t, s, `obs (quot (zneg (intOf 7)) (intOf 2))`,
			"npair 0 3")
		normalizesTo(t, s, `natAbs (zneg (intOf 7) % intOf 2)`, "1")
	})
	t.Run("ch13", func(t *testing.T) {
		s := loadListing(t, "ch13_rationals.rune")
		// Exact division through the quotient: (1/2) / (1/2) computes to
		// the representative 2/2 — the class of one.
		normalizesTo(t, s, `posR (rdivP (rp 1 0 1) (rp 1 0 1))`,
			"2")
		normalizesTo(t, s, `denR (rdivP (rp 1 0 1) (rp 1 0 1))`,
			"1")
		// The flooring quotient at Rat: the floor numerator of (-5/2) // 1 is
		// 7 against negative 10 — the class of -3, rounding toward -infinity.
		// (// and % are well-defined on the quotient; ch16/floorUnique proves it.)
		normalizesTo(t, s, `floorNum (rmulP (rp 0 5 0) (rflipP (rp 2 0 0)))`,
			"7")
	})
	t.Run("ch14", func(t *testing.T) {
		s := loadListing(t, "ch14_binary.rune")
		// Binary multiplication agrees with the unary spec by THEOREM
		// (bigMul checked on load); here conversion computes a small
		// product through the binary side and reads it back.
		normalizesTo(t, s, `toNatP (pmul (pI (pO pH)) (pI pH))`,
			"15")
	})
	t.Run("ch15", func(t *testing.T) {
		s := loadListing(t, "ch15_binary_division.rune")
		// Long division runs on the bits: 17 = 3·5 + 2, quotient and remainder
		// both computed in binary and read back to the unary spec. Inputs cross
		// into BN via fromNat (the `builtin bin` literal binding is retired).
		normalizesTo(t, s, `toNat (bndiv (fromNat 17) (fromNat 5))`, "3")
		normalizesTo(t, s, `toNat (bnmod (fromNat 17) (fromNat 5))`, "2")
	})
	t.Run("ch58", func(t *testing.T) {
		s := loadListing(t, "ch58_pathalgebra.rune")
		// The identity equivalence's forward map is `id` (postulate-free, via
		// isoToEquiv) — the i1 endpoint of the univalence Glue line.
		normalizesTo(t, s, `fn (B : UF) is equivFun B B (idEquivF B) end`,
			"fn (B : U) (x : U) is x end")
		// The ua T-system (A8 `sysU`) reduces to the domain at i0 and the base at i1.
		normalizesTo(t, s, `fn (A : UF) (B : UF) is uaTSystem A B i0 htop end`,
			"fn (A : U) (B : U) is A end")
		normalizesTo(t, s, `fn (A : UF) (B : UF) is uaTSystem A B i1 htop end`,
			"fn (A : U) (B : U) is B end")
		// fsplitD (A8 dependent face split) selects a face-conditional equivalence;
		// on ⊤ it picks the branch, whose forward map is `id`.
		normalizesTo(t, s, `fn (B : UF) is equivFun B B (pickEquiv B ftop ftop htop) end`,
			"fn (B : U) (x : U) is x end")
		// The DERIVED (postulate-free) univalence line: transport along uaGlue of the
		// identity iso applies its forward map (id) — univalence COMPUTING, no `ua`.
		normalizesTo(t, s,
			`fn (B : UF) (x : El B) is `+
				`castU B B (uaGlue B B (fn (y : El B) is y end) (fn (y : El B) is y end) `+
				`(fn (y : El B) is refl end) (fn (y : El B) is refl end)) x end`,
			"fn (B : U) (x : U) is x end")
		// Univalence in a DEPENDENT FAMILY: transport in P = λX. X along the identity
		// ua is the identity (via apU/pappU + castU). The frontier capability beyond
		// bare castU — univalence usable inside an arbitrary fibrant family.
		normalizesTo(t, s,
			`fn (B : UF) (x : El B) is `+
				`transportF (fn (X : UF) is X end) B B `+
				`(uaGlue B B (fn (y : El B) is y end) (fn (y : El B) is y end) `+
				`(fn (y : El B) is refl end) (fn (y : El B) is refl end)) x end`,
			"fn (B : U) (x : U) is x end")
		// Type-path algebra: transport along the REVERSED identity ua is still the
		// identity (the inverse of id is id) — pappU/symU computing through transp.
		normalizesTo(t, s,
			`fn (B : UF) (x : El B) is `+
				`castU B B (symU B B (uaGlue B B (fn (y : El B) is y end) (fn (y : El B) is y end) `+
				`(fn (y : El B) is refl end) (fn (y : El B) is refl end))) x end`,
			"fn (B : U) (x : U) is x end")
	})
}

// TestInnerLayerDoesNotDeploy: the v3 release criterion for the fibrant
// layer is "elaborates and checks", not "runs" — transport along a ua-path
// has no erased meaning yet (§F), so emission must refuse rather than
// silently compute the wrong function.
func TestInnerLayerDoesNotDeploy(t *testing.T) {
	s := loadListing(t, "ch10_univalence.rune")
	// `notPath` is a bare inner type-path (ua boolF boolF not not …) — a pathU
	// VALUE whose normal form is the postulated `ua` neutral, with no erased outer
	// meaning. It must be refused. (`flipped = castU … notPath true` now DEPLOYS:
	// the B5/R-ERASE2 slice erases its normal form, which the castU-through-ua
	// programming idiom computes to the outer Bool `false` — see the chapter's
	// `transportIsNot` proof. Inner scaffolding that computes away to an outer
	// value has a genuine runtime meaning; a bare inner path does not.)
	if _, err := s.EmitProgram("notPath"); err == nil {
		t.Fatal("a bare inner type-path (no erased meaning) must be refused in v3")
	}
}

// TestListingsEmitAndExecute: the data and quotient chapters survive erasure
// and run on the JS backend (the "runs" of the freeze criterion, in the
// deployed sense). For the quotient chapters this is also the shadow rule's
// promise made visible: qin is the identity at runtime, a lift is a plain
// call, and every respect proof is gone.
func TestListingsEmitAndExecute(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	cases := []struct {
		listing, main, want string
	}{
		{"ch04_data.rune", "four", "succ (succ (succ (succ zero)))"},
		// ch10: B5 / R-ERASE2 — `flipped = castU … notPath true` uses the inner
		// layer in its SOURCE, but its normal form computes (castU-through-ua) to
		// the outer Bool `false`, so it now DEPLOYS and RUNS. Inner univalence
		// transport that lands on an outer value has a genuine erased meaning.
		{"ch10_univalence.rune", "flipped", "false"},
		{"ch06_quotients.rune", "parityOfTwo", "true"},
		{"ch07_integers.rune", "zresult", "npair (succ (succ zero)) (succ zero)"},
		// ch11 runs on the BigInt shadow: gcd 252 105 in milliseconds, with
		// case-shaped eliminations emitted as constant-time dispatch.
		{"ch11_arithmetic.rune", "answer", "21"},
		// ch12: the floor convention through the quotient and the shadow
		// agree — |−7 // 2| is 4, not 3.
		{"ch12_integer_division.rune", "answer", "4"},
		// ch13: the numerator representative of (7/2) / (3/5) under the
		// erased shadow — division through the flip, no proofs at runtime.
		{"ch13_rationals.rune", "answer", "105"},
		// ch14: 35 · 186 computed in binary, read back through toNat.
		{"ch14_binary.rune", "answer", "6510"},
		// ch15: gcd(1071, 462) by Euclid over binary long division, read back.
		{"ch15_binary_division.rune", "answer", "21"},
		// ch16: the structural flooring quotient, 100 // 7 = 14.
		{"ch16_division_algorithm.rune", "answer", "14"},
		// ch32: a dependent pair (Σ) erases to a tuple and RUNS — the second
		// projection of (true, false) is false. Σ is outer, so it deploys.
		{"ch32_sigma_run.rune", "answer", "false"},
		// ch39: a `partial` general-recursive function RUNS via codegen (its head
		// is neutral in the checker, unfolded at runtime) — countdown 3 = zero.
		{"ch39_partial.rune", "answer", "zero"},
		// ch43: an IO program RUNS — the world thunk is forced, pureIO/bindIO
		// sequence, the result is succ (succ zero).
		{"ch43_io_run.rune", "prog", "succ (succ zero)"},
		// ch71: a DEPLOYED distributed protocol (M0 spirit) — a sender ‖ receiver
		// rendezvous. One step synchronises the peers (some (par halt halt))...
		{"ch71_distributed.rune", "answer", "some (par halt halt)"},
		// ...and running to completion idles at the synchronised state.
		{"ch71_distributed.rune", "final", "par halt halt"},
		// ch72: a replicated counter (G-Counter CRDT). After replica A (count 2) and
		// replica B (count 1) gossip (merge = pointwise max), the observed value is 3.
		// Deployed eventual consistency; mergeComm certifies gossip-order independence.
		{"ch72_replicated_counter.rune", "converged", "succ (succ (succ zero))"},
		// ch104: a grow-only set CRDT (G-Set). replica1 {1,2} and replica2 {2,3}
		// gossip (merge = set union); element 3 (added on replica2) becomes
		// observable in the merged view. Deployed eventual consistency, its
		// convergence proof inherited from the verified set lattice (ch97).
		{"ch104_gset_crdt.rune", "observed3", "true"},
		// ch106: a PN-Counter CRDT (counter supporting both increment AND decrement, a
		// PAIR of G-Counters). replica1 (+3 −1) and replica2 (+2 −2) gossip (merge =
		// pointwise max on P and N); the converged increment-tally P = max 3 2 = 3.
		// The observed integer value P − N = +1 is certified in-checker (qsound on ℤ);
		// the deployed shadow is the merged P-count. Convergence lifts componentwise
		// from ch72's verified G-Counter join.
		{"ch106_pn_counter.rune", "mergedP", "succ (succ (succ zero))"},
		// ch114: a gen_server-style state machine (D5 OTP-class concurrency). A counter
		// server posts [Inc,Inc,Inc] into its FIFO mailbox (ch111 queue) and runs; the
		// final state is 3. fifoProcessingOrder certifies that `run` folds `handle`
		// left-to-right over the mailbox (FIFO / enqueue order); the deployed `answer`
		// runs that drain on a real backend.
		{"ch114_gen_server.rune", "answer", "succ (succ (succ zero))"},
		// ch120: an ACTOR MAILBOX in IO (D6 deployable OTP concurrency). The ch114 counter
		// gen_server is wired to the IO monad — `runActorIO` drives the mailbox drain as an
		// EFFECT and lifts the final state into IO (an OUTER, deployable IO program, ch43). The
		// purity bridge (`runActorIOPure`) reads the IO actor's result back as the pure `run`'s
		// state, and `actorFifoProcessingOrder` transfers ch114's FIFO theorem to the effectful
		// actor. The deployed `answer` is the IO actor's observed reply after draining
		// [Inc,Inc,Inc] — 3, run on a real backend. The IO actor IS the verified FIFO gen_server.
		{"ch120_io_actor.rune", "answer", "succ (succ (succ zero))"},
		// ch109: an OR-Set CRDT (Observed-Remove Set — supports BOTH add and remove, a
		// PAIR of G-Sets: adds and removes, tokens as Nats). Replica A adds token 1 then
		// removes it (absent); replica B concurrently adds the FRESH token 2. After gossip
		// (merge = componentwise union), the element is PRESENT — carried by token 2, the
		// fresh add no remove observed (add-wins / observed-remove). Convergence lifts
		// componentwise from ch104's verified G-Set join.
		{"ch109_or_set.rune", "observedPresent", "true"},
		// ch138: E4 — a DEPLOYED, VERIFIED consensus decision. Three replicas vote
		// (0,1 = true the majority; 2 = false); the computable 3-replica majority rule
		// maj3 decides `true`, run on a real backend. maj3TwoAgree certifies the agreement
		// property (two agreeing replicas fix the value) and decisionIsMajorityValue ties
		// the deployed value to it — provably correct AND deployed from the same source.
		{"ch138_deployed_consensus.rune", "decision", "true"},
		// ch139: E4 scaled — a MULTI-VALUE (parameterized) consensus decision. The
		// 3-replica majority maj3Gen over any type with decidable equality; the ℕ instance
		// (votes 2,2,1) decides 2, run on a real backend. maj3GenTwoAgree proves the
		// agreement property parametrically for all such V; ndecisionIsMajorityValue
		// instantiates it at ℕ — provably correct AND deployed from the same source.
		{"ch139_multivalue_consensus.rune", "ndecision", "succ (succ zero)"},
		// ch183: telos-4 log-matching — a verified replicated-log prefix check. isPrefix
		// is reflexive (prefixRefl) and a concrete prefixDecision (a follower log [1,2] is a
		// prefix of the leader log [1,2,3]) deploys and runs to true.
		{"ch183_log_prefix.rune", "prefixDecision", "true"},
		// ch187: telos-4 append-entries — a leader appends entry 3 to log [1,2] and the
		// old log stays a prefix. The prefix check after the append deploys and runs to true.
		{"ch187_append_entries.rune", "appendDecision", "true"},
		// ch188: telos-4 replicated state machine — applyLog folds the SMR step over the
		// committed log. A replica applies [1,2,3] to state 0 and deploys/runs to 6.
		{"ch188_smr.rune", "smrState", "succ (succ (succ (succ (succ (succ zero)))))"},
		// ch189: telos-4 SMR consistency — a follower that applied prefix [1,2] catches up
		// on suffix [3]; running the concatenated log from state 0 deploys/runs to 6.
		{"ch189_smr_consistency.rune", "catchUpState", "succ (succ (succ (succ (succ (succ zero)))))"},
		// ch190: telos-4 replica actor (gen_server) over state (log, applied). A fresh
		// replica processes [1,2,3]; its applied state (Snd of the Σ-state) deploys/runs to 6.
		{"ch190_replica_actor.rune", "deployedApplied", "succ (succ (succ (succ (succ (succ zero)))))"},
		// ch191: telos-4 replica agreement — two replicas reach the same applied state by
		// different histories. Replica B (mid-stream catch-up) deploys/runs to 6, same as A.
		{"ch191_actor_agreement.rune", "deployedB", "succ (succ (succ (succ (succ (succ zero)))))"},
		// ch194: fuel-bounded Euclidean division by repeated subtraction. 7 ÷ 3 yields
		// quotient 2; the divmod result (a Σ pair) deploys/runs on a backend.
		{"ch194_divmod.rune", "quotient", "succ (succ zero)"},
	}
	for _, tc := range cases {
		t.Run(tc.listing, func(t *testing.T) {
			s := loadListing(t, tc.listing)
			p, err := s.EmitProgram(tc.main)
			if err != nil {
				t.Fatal(err)
			}
			out, err := codegen.Default().Emit(p)
			if err != nil {
				t.Fatal(err)
			}
			f, err := os.CreateTemp(t.TempDir(), "*.js")
			if err != nil {
				t.Fatal(err)
			}
			if _, err := f.WriteString(string(out)); err != nil {
				t.Fatal(err)
			}
			f.Close()
			got, err := exec.Command("node", f.Name()).CombinedOutput()
			if err != nil {
				t.Fatalf("node: %v\n%s", err, got)
			}
			if strings.TrimSpace(string(got)) != tc.want {
				t.Fatalf("emitted chapter printed %q, want %q", got, tc.want)
			}
		})
	}
}

// TestListingsOTPLiveBeam is the D5 / R-OTP LIVE-runtime gate (Layer R0+R1): the
// OTP process primitives are not a functional model — they RUN as genuine BEAM
// processes. ch205 spawns a stateful worker, the client fires three `bump`s and a
// `report` carrying its own address, and the worker's looped mailbox drain
// (a `partial` receive loop) replies with the count. The whole thing executes on
// escript through `spawn`/`!`/`receive`/`self` (codegen.Beam ships beamOTPRuntime),
// and the FIFO-deterministic answer is 3. BEAM-only: the concurrency primitives
// have a real runtime on the BEAM (their natural home); the non-BEAM cooperative
// scheduler shim is parked (PARKING-LOT.md), so this is not in the cross-backend
// JS conformance corpus.
func TestListingsOTPLiveBeam(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not in PATH")
	}
	s := loadListing(t, "ch205_otp_live.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	out, err := codegen.Beam{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "ch205.erl")
	if err := os.WriteFile(f, []byte(out), 0o644); err != nil {
		t.Fatal(err)
	}
	// stdout only — escript prints compile warnings (unused helpers) to stderr.
	got, err := exec.Command("escript", f).Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("escript run failed: %v\n%s\n--- emitted ---\n%s", err, stderr, out)
	}
	if want := "succ (succ (succ zero))"; strings.TrimSpace(string(got)) != want {
		t.Fatalf("live OTP actor on BEAM printed %q, want %q", got, want)
	}
}

// TestListingsReplicatedActorsBeam is the E4 live-actor projection gate (built on D5):
// the verified replicated counter runs as TWO genuine BEAM processes that hold replica
// state, gossip it, and merge. ch433 spawns replicas A and B, ticks each replica's own
// slot, cross-feeds their states as merges, then collects both reported values - after
// gossip both replicas converged to 2, so the sum is 4. This is the deploy half of the
// better-than-Winglang thesis on real distributed actors, not a model. BEAM-only, like
// ch205 (the concurrency primitives have their runtime on the BEAM).
func TestListingsReplicatedActorsBeam(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not in PATH")
	}
	s := loadListing(t, "ch433_replicated_actors.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	out, err := codegen.Beam{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "ch433.erl")
	if err := os.WriteFile(f, []byte(out), 0o644); err != nil {
		t.Fatal(err)
	}
	got, err := exec.Command("escript", f).Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("escript run failed: %v\n%s\n--- emitted ---\n%s", err, stderr, out)
	}
	if want := "succ (succ (succ (succ zero)))"; strings.TrimSpace(string(got)) != want {
		t.Fatalf("live replicated actors on BEAM printed %q, want %q (both replicas should converge to 2)", got, want)
	}
}

// TestListingsFaultTolerantReplicaBeam is the E4 fault-tolerant-replication gate (E4 + D5
// faults): replica A increments and gossips its state to replica B, then A is CRASHED
// (primExit) and its death DETECTed (primMonitor). Because A's increment was already
// anti-entropied into B, B still reports it - the value survives the replica. ch434 runs
// this on genuine BEAM processes and prints succ zero (1): durability under a crash, the
// real distributed-systems claim. BEAM-only, like ch205/ch214.
func TestListingsFaultTolerantReplicaBeam(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not in PATH")
	}
	s := loadListing(t, "ch434_fault_tolerant_replica.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	out, err := codegen.Beam{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "ch434.erl")
	if err := os.WriteFile(f, []byte(out), 0o644); err != nil {
		t.Fatal(err)
	}
	got, err := exec.Command("escript", f).Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("escript run failed: %v\n%s\n--- emitted ---\n%s", err, stderr, out)
	}
	if want := "succ zero"; strings.TrimSpace(string(got)) != want {
		t.Fatalf("fault-tolerant replica on BEAM printed %q, want %q (gossiped increment should survive the crash)", got, want)
	}
}

// TestListingsReplicaRecoveryBeam is the E4 recovery gate (E4 + D5 faults): a crashed
// replica is RESTARTED fresh and catches up to the full converged state by anti-entropy
// from a live peer. ch435 has A and B tick and gossip (B holds {1,1}), crashes A
// (primExit) and detects it (primMonitor), then restarts a fresh A that pulls B's state
// and recovers the full value 2 - not just its own increment. Runs on escript and prints
// succ (succ zero). BEAM-only, like ch205/ch214/ch433/ch434.
func TestListingsReplicaRecoveryBeam(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not in PATH")
	}
	s := loadListing(t, "ch435_replica_recovery.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	out, err := codegen.Beam{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "ch435.erl")
	if err := os.WriteFile(f, []byte(out), 0o644); err != nil {
		t.Fatal(err)
	}
	got, err := exec.Command("escript", f).Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("escript run failed: %v\n%s\n--- emitted ---\n%s", err, stderr, out)
	}
	if want := "succ (succ zero)"; strings.TrimSpace(string(got)) != want {
		t.Fatalf("recovered replica on BEAM printed %q, want %q (should re-sync the full state from its peer)", got, want)
	}
}

// TestListingsGenericReplicaBeam is the E4 generic-projection gate: ONE replica loop
// (serveG), parametric over the state type S and its (merge, tick, value), deployed live.
// ch436 instantiates it at the G-Counter and runs two replicas to convergence on genuine
// BEAM processes (sum 4), while gsetReplica instantiates the SAME serveG at a G-Set - so
// the protocol->actors projection is uniform across CvRDTs, expressed as a library rather
// than gated surface syntax. BEAM-only.
func TestListingsGenericReplicaBeam(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not in PATH")
	}
	s := loadListing(t, "ch436_generic_replica.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	out, err := codegen.Beam{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "ch436.erl")
	if err := os.WriteFile(f, []byte(out), 0o644); err != nil {
		t.Fatal(err)
	}
	got, err := exec.Command("escript", f).Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("escript run failed: %v\n%s\n--- emitted ---\n%s", err, stderr, out)
	}
	if want := "succ (succ (succ (succ zero)))"; strings.TrimSpace(string(got)) != want {
		t.Fatalf("generic replica on BEAM printed %q, want %q (two G-Counter replicas via the generic serveG should converge)", got, want)
	}
	// The SAME serveG, deployed at a G-Set: confirm the projection RUNS for a second CvRDT,
	// not merely type-checks. Two replicas add different elements and converge to the union.
	pg, err := s.EmitProgram("mainGS")
	if err != nil {
		t.Fatal(err)
	}
	outg, err := codegen.Beam{}.Emit(pg)
	if err != nil {
		t.Fatal(err)
	}
	fg := filepath.Join(t.TempDir(), "ch436gs.erl")
	if err := os.WriteFile(fg, []byte(outg), 0o644); err != nil {
		t.Fatal(err)
	}
	gotg, err := exec.Command("escript", fg).Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("escript run (mainGS) failed: %v\n%s", err, stderr)
	}
	if want := "succ (succ (succ (succ zero)))"; strings.TrimSpace(string(gotg)) != want {
		t.Fatalf("generic G-Set replica on BEAM printed %q, want %q (the same serveG should run for a G-Set too)", gotg, want)
	}
}

// TestListingsOTPFaultLiveBeam is the D5 / R-OTP Layer-R2 LIVE-fault gate: the
// fault primitives ch206 SPECIFIED (CRASH/DETECT + bounded restart-liveness) now
// RUN as real BEAM signals. ch214 spawns a worker that crashes itself
// (primExit ~> exit(self, crashed)), the supervisor detects the death through the
// monitor's DOWN (primMonitor ~> erlang:monitor + receive DOWN), then RESTARTS a
// fresh worker that takes one bump and reports — so the observed total is
// `succ zero` (1): the crashed original never replied, recovery came from the
// restart. This makes the ch206 `eventuallyRestarted` (witness k=1) executable, and
// is the LIVE tie ch207/ch209 adequacy was proven against. BEAM-only, like ch205.
func TestListingsOTPFaultLiveBeam(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not in PATH")
	}
	s := loadListing(t, "ch214_otp_fault_live.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	out, err := codegen.Beam{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "ch214.erl")
	if err := os.WriteFile(f, []byte(out), 0o644); err != nil {
		t.Fatal(err)
	}
	got, err := exec.Command("escript", f).Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("escript run failed: %v\n%s\n--- emitted ---\n%s", err, stderr, out)
	}
	if want := "succ zero"; strings.TrimSpace(string(got)) != want {
		t.Fatalf("live fault-tolerant OTP on BEAM printed %q, want %q (crash→detect→restart→1)", got, want)
	}
}

// TestCongConsGLaterGeneralizesCongConsG pins the per-clock E2-converse increment
// (telos-4/M7): `congConsGLater` — the DELAYED-tail consG-congruence — is a strict
// generalization of `congConsG`. The guarded recursive call yields the tail-path
// DELAYED (a `▹κ (pathF gStr ra rb)`), not present; `congConsGLater` consumes that
// delayed path directly. This check loads the live ch69 listing and verifies that
// `congConsG` is recovered as the `next`-instantiation of `congConsGLater` — i.e.
// `congConsGLater … (next k _ p)` type-checks at `congConsG`'s declared codomain
// (the lmap-papp endpoints collapse to the cons-of-next form by `lmapPappNextI0`).
// If the slot/endpoint coherence regressed, this derivation would fail to check.
func TestCongConsGLaterGeneralizesCongConsG(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "listings", "ch69_guarded_types.rune"))
	if err != nil {
		t.Fatal(err)
	}
	derive := `
congConsGViaLater : (k : Clock) -> (A : UF) -> (a : El A) -> (b : El A) -> (hp : El (pathF A a b))
   -> (ra : El (gStr k A)) -> (rb : El (gStr k A)) -> (p : El (pathF (gStr k A) ra rb))
   -> El (pathF (gStr k A)
            (consG k A a (next k (gStr k A) ra))
            (consG k A b (next k (gStr k A) rb))) is
  fn (k : Clock) (A : UF) (a : El A) (b : El A) (hp : El (pathF A a b))
     (ra : El (gStr k A)) (rb : El (gStr k A)) (p : El (pathF (gStr k A) ra rb)) is
    congConsGLater k A a b hp ra rb (next k (pathF (gStr k A) ra rb) p)
  end
end
`
	s := session.New()
	if _, err := s.LoadSource(string(src) + derive); err != nil {
		t.Fatalf("congConsGLater should generalize congConsG (next-instantiation must check): %v", err)
	}
	if _, ok := s.Lookup("congConsGLater"); !ok {
		t.Fatal("congConsGLater missing from ch69")
	}
	if _, ok := s.Lookup("bisimStepLater"); !ok {
		t.Fatal("bisimStepLater missing from ch69")
	}
	if !s.Certified("bisimStepLater") {
		t.Fatal("bisimStepLater should be certified (the productive corecursion step checks)")
	}
}

// TestGBisimForceTailCashesDelayedBisim pins the TAIL-half E2-converse increment
// (telos-4/M7): `gBisimForceTail` is the `forceD`-cash of the DELAYED tail-
// bisimilarity `∀κ. ▹κ (gBisim κ A (pair (ra κ)(rb κ)))` into the per-clock
// global bisimilarity `∀κ. gBisim κ A (pair (ra κ)(rb κ))` — the tail-half
// analogue of `gtailG`'s forceD-cash of the delayed STREAM tail. The relation's
// type is genuinely clock-VARYING (the index mentions κ AND `gBisim κ` is a
// `gfixF`-built code that varies with the clock), so plain `force` cannot cash it;
// `forceD` can, discharging the plan's KEY RISK (that a `gfixF`/UF-valued family
// might not be a clean `forceD` argument — it is). The coherence `gBisimForceTailNext`
// proves the cash COMPUTES (`refl`): cashing a `next`-built delayed bisim recovers
// the witness at every clock. The NEGATIVE probe confirms the refl is load-bearing:
// claiming the cash equals a DISTINCT witness is rejected.
func TestGBisimForceTailCashesDelayedBisim(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "listings", "ch69_guarded_types.rune"))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("ch69 should load (gBisimForceTail + coherence): %v", err)
	}
	if _, ok := s.Lookup("gBisimForceTail"); !ok {
		t.Fatal("gBisimForceTail missing from ch69")
	}
	if !s.Certified("gBisimForceTail") {
		t.Fatal("gBisimForceTail should be certified (forceD types at the gfixF/UF-valued bisim family)")
	}
	if !s.Certified("gBisimForceTailNext") {
		t.Fatal("gBisimForceTailNext should be certified (the next-built cash computes by refl)")
	}

	// NEGATIVE PROBE: the cash of a `next`-built bisim of `bw` recovers `bw`, NOT a
	// distinct witness `bw2`. Claiming equality with `bw2` must FAIL — proving the
	// `refl` in `gBisimForceTailNext` is genuinely load-bearing (computing), not a
	// vacuous identity that any RHS would satisfy.
	neg := `
gBisimForceTailNeg : (A : UF)
   -> (ra : (k : Clock) -> El (gStr k A)) -> (rb : (k : Clock) -> El (gStr k A))
   -> (bw : (k : Clock) -> El
            (gBisim k A (pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end) (ra k) (rb k))))
   -> (bw2 : (k : Clock) -> El
            (gBisim k A (pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end) (ra k) (rb k))))
   -> (k : Clock)
   -> Eq (El (gBisim k A (pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end) (ra k) (rb k))))
        (gBisimForceTail A ra rb
           (fn (j : Clock) is
              next j (gBisim j A (pairF (gStr j A) (fn (_s : El (gStr j A)) is gStr j A end) (ra j) (rb j)))
                (bw j) end)
           k)
        (bw2 k) is
  fn (A : UF)
     (ra : (k : Clock) -> El (gStr k A)) (rb : (k : Clock) -> El (gStr k A))
     (bw : (k : Clock) -> El
            (gBisim k A (pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end) (ra k) (rb k))))
     (bw2 : (k : Clock) -> El
            (gBisim k A (pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end) (ra k) (rb k))))
     (k : Clock) is refl end
end
`
	sNeg := session.New()
	if _, err := sNeg.LoadSource(string(src) + neg); err == nil {
		t.Fatal("gBisimForceTail cash must NOT equate a distinct witness bw2 (refl is not load-bearing)")
	}
}

// TestGBisimTailGlueClosesPerClockConverse pins the M7 HEADLINE increment
// (telos-4/M7 E2 converse): the per-clock guarded bisimilarity is bridged ALL THE
// WAY to a GLOBAL stream path on cons-built streams. Two new connectives close it:
//   - `gBisimTailGlue`: the TAIL-half per-clock→global bridge. It feeds `bisimToPathStr`'s
//     `tp` (tail-path) slot via the documented chain — `bisimTail` (codomain `≡ Later κ
//     (gBisim κ (pair (ra κ)(rb κ)))` by `bisimTailSlotCons`, no cast) ▸ `gBisimForceTail`
//     (forceD-cash to the global tail-bisim) ▸ the recursive converter `conv`. The KEY
//     finding: step 3 closes DEFINITIONALLY on cons-form — `tail (glue gs) ≡ glue ra` ON
//     THE NOSE (glueTail ▸ gtailCons/glueConsTail, refl), so `conv`'s output `pathF (Str A)
//     (glue ra)(glue rb)` directly inhabits the `tp` slot with NO Str-level congConsGLater.
//   - `gBisimToPathGlue`: COMPOSES `gBisimHeadGlue` + `gBisimTailGlue` into `bisimToPathStr`,
//     producing `pathF (Str A) (glue gs)(glue gt)` from the per-clock witness + recursive
//     descent. This is the per-clock→global E2 converse CLOSED on cons-built streams.
//
// Both must certify. The NEGATIVE probe confirms `gBisimTailGlue`'s definitional re-aim is
// load-bearing: claiming the SECOND stream's tail (rb) where the FIRST (ra) is expected — a
// genuinely different glued tail — must be REJECTED.
func TestGBisimTailGlueClosesPerClockConverse(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "listings", "ch69_guarded_types.rune"))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("ch69 should load (gBisimTailGlue + gBisimToPathGlue): %v", err)
	}
	if _, ok := s.Lookup("gBisimTailGlue"); !ok {
		t.Fatal("gBisimTailGlue missing from ch69")
	}
	if !s.Certified("gBisimTailGlue") {
		t.Fatal("gBisimTailGlue should be certified (bisimTail ▸ slot-coherence ▸ forceD-cash ▸ conv, re-aimed by glueTail/gtailCons)")
	}
	if !s.Certified("gBisimToPathGlue") {
		t.Fatal("gBisimToPathGlue should be certified (the per-clock→global E2 converse, headed by bisimToPathStr)")
	}

	// NEGATIVE PROBE: gBisimTailGlue's tail-path target endpoint `tail (glue gs)` is
	// `glue ra` ON THE NOSE (glueTail ▸ gtailCons). If that definitional re-aim were
	// vacuous, the converter's `pathF (Str A)(glue ra)(glue rb)` would inhabit ANY
	// endpoint shape — including one where the FIRST endpoint claims the SECOND stream's
	// tail `rb`. It must NOT: a path with endpoint `glue rb` cannot fill a slot whose
	// declared endpoint is `tail (glue gs) ≡ glue ra` (distinct glued streams).
	neg := `
gBisimTailGlueNeg : (A : UF)
   -> (a : El A) -> (ra : (k : Clock) -> El (gStr k A))
   -> (b : El A) -> (rb : (k : Clock) -> El (gStr k A))
   -> (bsf : (k : Clock) -> El (gBisim k A
            (pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end)
               (consG k A a (next k (gStr k A) (ra k)))
               (consG k A b (next k (gStr k A) (rb k))))))
   -> (conv : (bgt : (k : Clock) -> El (gBisim k A
                 (pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end) (ra k) (rb k))))
              -> El (pathF (Str A) (glue A ra) (glue A rb)))
   -> El (pathF (Str A)
            (tail A (glue A (fn (j : Clock) is consG j A b (next j (gStr j A) (rb j)) end)))
            (tail A (glue A (fn (j : Clock) is consG j A b (next j (gStr j A) (rb j)) end)))) is
  fn (A : UF)
     (a : El A) (ra : (k : Clock) -> El (gStr k A))
     (b : El A) (rb : (k : Clock) -> El (gStr k A))
     (bsf : (k : Clock) -> El (gBisim k A
            (pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end)
               (consG k A a (next k (gStr k A) (ra k)))
               (consG k A b (next k (gStr k A) (rb k))))))
     (conv : (bgt : (k : Clock) -> El (gBisim k A
                 (pairF (gStr k A) (fn (_s : El (gStr k A)) is gStr k A end) (ra k) (rb k))))
              -> El (pathF (Str A) (glue A ra) (glue A rb))) is
    gBisimTailGlue A a ra b rb bsf conv
  end
end
`
	sNeg := session.New()
	if _, err := sNeg.LoadSource(string(src) + neg); err == nil {
		t.Fatal("gBisimTailGlue must NOT inhabit a tail-path whose first endpoint is glue rb (the glue ra re-aim is load-bearing)")
	}
}

// TestHcompBisimHeadKanEnabler pins the CUBICAL E2-converse Kan enabler (telos-4/M7,
// the cont.48 increment): `hcomp` over the `gfixF`-built bisimilarity relation `gBisim
// k A d` now COMPUTES componentwise. The kernel unfolds an applied `gfixF k D Φ d` one
// step (the convGfixF equation, now reachable by eval's Kan rules, fired only on a
// `Later`-guarded/productive code so NbE terminates), exposing the relation's `sigmaF`
// body so the structural-Σ Kan rule fires. `hcompBisimHead` is the refl pin that
// observing the head-path component of an hcomp'd bisimilarity = the hcomp of the head-
// path components. This is the building block the cubical converse's open-box repair
// composes from — over `gBisim`, not just `gStr` (the previously-shipped gfix enabler
// did NOT reach the indexed gfixF relation). If the gfixF Kan unfold regressed (stayed
// stuck), this refl would fail to check.
func TestHcompBisimHeadKanEnabler(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "listings", "ch69_guarded_types.rune"))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("ch69 should load (hcompBisimHead): %v", err)
	}
	if _, ok := s.Lookup("hcompBisimHead"); !ok {
		t.Fatal("hcompBisimHead missing from ch69")
	}
	if !s.Certified("hcompBisimHead") {
		t.Fatal("hcompBisimHead should be certified (hcomp over gBisim unfolds the gfixF and the Σ Kan rule fires componentwise, by refl)")
	}

	// NEGATIVE PROBE: the componentwise law is load-bearing — replacing the genuine
	// head-system walls by the constant `u0` system is NOT the hcomp of the real walls
	// unless `u` is degenerate. If the gfixF unfold did not actually compute (left the
	// hcomp stuck), conversion could not tell these apart; it must REJECT.
	neg := `
hcompBisimHeadNeg : (k : Clock) -> (A : UF) -> (d : El (Dpair k A)) -> (phi : F)
   -> (u : I -> holds phi -> El (gBisim k A d)) -> (u0 : El (gBisim k A d))
   -> Eq (El (pathF A (headG k A (Sfst k A d)) (headG k A (Ssnd k A d))))
        (bisimHead k A d (hcomp (gBisim k A d) phi u u0))
        (hcomp (pathF A (headG k A (Sfst k A d)) (headG k A (Ssnd k A d))) phi
           (fn (i : I) (h : holds phi) is bisimHead k A d u0 end)
           (bisimHead k A d u0)) is
  fn (k : Clock) (A : UF) (d : El (Dpair k A)) (phi : F)
     (u : I -> holds phi -> El (gBisim k A d)) (u0 : El (gBisim k A d)) is refl end
end
`
	sNeg := session.New()
	if _, err := sNeg.LoadSource(string(src) + neg); err == nil {
		t.Fatal("hcompBisimHead's componentwise law must NOT hold against the constant-u0 system (the gfixF Kan unfold must genuinely compute the real walls)")
	}
}

// TestHcompBisimTailCompletesKanSubstrate pins the TAIL analogue of hcompBisimHead
// (telos-4/M7) — the SECOND component of the cubical Kan-filling over the
// bisimilarity relation `gBisim`, completing the componentwise repair the E2
// converse composes from. The extended `unfoldGfixType` unfolds the INDEXED
// `gfixF`-built `gBisim k A d` to its `sigmaF` body `(head-path) × ▹κ (tail-bisim)`,
// and the structural-Σ Kan rule fires on BOTH Σ components. `hcompBisimHead` reads
// the FIRST projection (the head-path); this `hcompBisimTail` reads the SECOND (the
// delayed tail-bisimilarity, via `bisimTail`/`sndF`). With both certified, `hcomp`
// over the `gfixF`-built relation is FULLY componentwise — the open-box repair acts
// on the head-path and the delayed tail-bisim INDEPENDENTLY, each by a genuine
// REDUCTION (not a stuck term). This is the Kan substrate over `gBisim` completed on
// both projections; the tail repair the total converter would compose with the head
// repair fires by the SAME Σ Kan rule. If the gfixF Kan unfold failed to reach the
// second component (left the tail hcomp stuck), this refl would fail to check.
func TestHcompBisimTailCompletesKanSubstrate(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "listings", "ch69_guarded_types.rune"))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("ch69 should load (hcompBisimTail): %v", err)
	}
	if _, ok := s.Lookup("hcompBisimTail"); !ok {
		t.Fatal("hcompBisimTail missing from ch69")
	}
	if !s.Certified("hcompBisimTail") {
		t.Fatal("hcompBisimTail should be certified (hcomp over gBisim unfolds the gfixF and the Σ Kan rule fires on the SECOND/tail component too, by refl)")
	}

	// NEGATIVE PROBE: the tail componentwise law is load-bearing — replacing the
	// genuine tail-system walls by the constant `u0` system is NOT the hcomp of the
	// real walls unless `u` is degenerate. If the gfixF unfold did not actually
	// compute the second component (left the tail hcomp stuck), conversion could not
	// tell these apart; it must REJECT.
	neg := `
hcompBisimTailNeg : (k : Clock) -> (A : UF) -> (d : El (Dpair k A)) -> (phi : F)
   -> (u : I -> holds phi -> El (gBisim k A d)) -> (u0 : El (gBisim k A d))
   -> Eq (El (laterApp k (Dpair k A) (gBisim k A) (gstepPair k A d)))
        (bisimTail k A d (hcomp (gBisim k A d) phi u u0))
        (hcomp (laterApp k (Dpair k A) (gBisim k A) (gstepPair k A d)) phi
           (fn (i : I) (h : holds phi) is bisimTail k A d u0 end)
           (bisimTail k A d u0)) is
  fn (k : Clock) (A : UF) (d : El (Dpair k A)) (phi : F)
     (u : I -> holds phi -> El (gBisim k A d)) (u0 : El (gBisim k A d)) is refl end
end
`
	sNeg := session.New()
	if _, err := sNeg.LoadSource(string(src) + neg); err == nil {
		t.Fatal("hcompBisimTail's componentwise law must NOT hold against the constant-u0 system (the gfixF Kan unfold must genuinely compute the real tail walls)")
	}
}
