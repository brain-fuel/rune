# Head to head: Wootz vs the field

The honest framing first: Terraform, Pulumi, AWS CDK, and Winglang are mature, adopted, and
well-funded. Wootz is a kernel plus 460+ proofs plus a working pipeline, built by one
person. On ecosystem and polish, they win today. Wootz wins on one axis that none of them
can cross without rebuilding from the type theory up: **it proves the behavior of the system
it deploys.** That axis is the entire pitch, and it is the axis that matters most as AI
starts writing the infrastructure.

## The category error every existing tool makes

All four existing tools operate on **structure**: they describe what resources exist and how
they are wired. None of them operate on **behavior**: what the running system does over time,
under concurrency, under failure. A Terraform plan can be perfectly valid and provision a
distributed counter that loses increments under a partition. The plan is correct. The system
is wrong. No structural tool can see the difference, because the difference is not in the
structure. It is in the algebra of the protocol, and only a tool that models that algebra
can check it.

Wootz models the algebra. The G-Counter converges because its merge is a proven
join-semilattice. The supervisor recovers because the supervision protocol is proven to
reach a restarted state. These are theorems the kernel checks, not tests that happened to
pass.

## The comparison table

| Capability | Terraform / OpenTofu | Pulumi | AWS CDK | Winglang | **Wootz** |
|---|---|---|---|---|---|
| Provision cloud resources | Yes | Yes | Yes | Yes | Yes (HCL emit + `--apply` lifecycle, gated live on LocalStack/FOSS; billed-account apply is funded work) |
| Multi-cloud from one source | Partial (per-provider config) | Partial | No (AWS-first) | Partial | **Yes, one agnostic graph to AWS/Azure/GCP + 15 FOSS** |
| Real general-purpose language | No (HCL) | Yes (TS/Go/Py/...) | Yes (TS/...) | Yes (Wing) | **Yes (dependently typed)** |
| Unifies app + infra | No | Partial | No | **Yes** | **Yes (app + infra + protocol, one source)** |
| Local simulator | No | No | No | Yes (testing) | **Yes (fault injection + CvRDT law linter)** |
| Proves distributed properties (convergence, safety, liveness) | **No** | **No** | **No** | **No** | **Yes, machine-checked** |
| Rejects an incorrect system at compile time | No | No | No | No | **Yes (the proof failing is a build error)** |
| Proof-carrying code generation | No | No | No | No | **Yes (erased IR to 8 backends, contracts at the FFI boundary)** |
| Verified fault tolerance | No | No | No | No | **Yes (proven supervision projected to live OTP actors)** |
| Foreign-code trust discipline | n/a | Trusted | Trusted | Trusted | **Furnace: tested then provable, with blame at the boundary** |
| Ecosystem maturity / adoption | **High** | **High** | **High** | **Medium** | Low (this is what funding builds) |
| Backing | Company + foundation | Company | Amazon | Company | One builder (this is the ask) |

## Tool by tool

### Terraform / OpenTofu
The incumbent. Declarative HCL, plan and apply, drift detection, an enormous provider
ecosystem. It is excellent at making infrastructure reproducible. It says nothing about
runtime behavior. A `terraform apply` that succeeds tells you the resources exist, not that
the distributed system built from them is correct. There is no notion of a protocol, a
proof, or a safety property anywhere in the model. Wootz emits the same HCL (so it
interoperates with the entire OpenTofu world) but adds the layer above: a proof that the
system those resources host actually works.

### Pulumi
The best ergonomics in the category: real languages, real abstractions, real testing.
Pulumi fixed HCL's expressiveness problem. It did not, and structurally cannot, add
verification, because its languages (TypeScript, Go, Python) have no dependent types and no
proof obligation. A Pulumi program is imperative infrastructure scripting with good
tooling. Wootz is the same idea pushed to its conclusion: if you are going to write infra in
a real language, make it a language that can prove the infra correct.

### AWS CDK
Synthesizes CloudFormation from code. AWS-first, deeply integrated, and structurally the
same as Pulumi for our purposes: code generates config, config has no behavioral guarantees,
and the multi-cloud story is weak by design (it exists to sell AWS). Wootz is cloud-neutral
by construction and adds the verification CDK cannot.

### Winglang
The closest competitor and the right one to take seriously. Wing's insight is correct and
important: unify infrastructure and application code in one language, with a local simulator
so you can test the whole system without deploying. Wootz agrees with all of that and goes
two steps further:

1. **Wing's simulator tests; Wootz's proves.** Wing's local sim runs your system and you
   observe it. That is testing: it samples behavior on the schedules you happened to try.
   Wootz's simulator does the same observational job AND the kernel checks the protocol's
   algebra, so "converges" means "converges under every schedule," not "converged on the
   one I ran." The `lww.rune` example passes Wing-style observation and is correctly
   rejected by Wootz's law linter. That gap is the whole point.
2. **Wing targets one runtime model; Wootz targets eight backends from a proven erased IR,**
   and the app/infra/protocol unification carries machine-checked contracts to the FFI
   boundary.

Where Wing genuinely beats Wootz today: it is a real product with documentation, a company,
users, and polish. That is a maturity gap, not a capability gap, and maturity is precisely
what funding buys. The capability gap, verification, runs the other way and cannot be closed
by Wing without rebuilding on a dependent type theory.

## Adjacent: the proof assistants (Coq/Lean/Agda/F\*/Dafny)
A fair objection is "formal verification already exists." It does, and Wootz stands on that
tradition. The distinction: those systems prove properties of mathematics and of programs in
the abstract. None of them deploy real multi-cloud infrastructure, project a verified
protocol to live actors on the BEAM, or give you a `deploy` verb. Wootz aims verification at
the specific, enormous, and currently unverified domain of distributed infrastructure and
AI-generated systems, and ships the deploy pipeline that makes the proof pay off in
production. It is formal methods pointed at the place the world actually breaks.

## The one-sentence positioning
Every other tool checks that your infrastructure is well-formed. Wootz proves that the
system it builds is well-behaved, and deploys the exact source it proved.
