#!/usr/bin/env bash
# bin/setup.sh — provision every environment dependency goforge.dev/rune needs, the
# project-specific way. Idempotent: safe to re-run. Language toolchains come from asdf
# (pinned in .tool-versions); Python libs from pip; the rest are system packages this
# script CHECKS and tells you how to install (only those need sudo).
#
#   ./bin/setup.sh          provision + report
#   ./bin/setup.sh --check  report only (no installs)
set -uo pipefail
cd "$(dirname "$0")/.."

CHECK_ONLY=0
[ "${1:-}" = "--check" ] && CHECK_ONLY=1

ok()   { printf '  \033[32m✓\033[0m %s\n' "$1"; }
warn() { printf '  \033[33m!\033[0m %s\n' "$1"; }
miss() { printf '  \033[31m✗\033[0m %s\n' "$1"; }
have() { command -v "$1" >/dev/null 2>&1; }

echo "== goforge.dev/rune — environment setup =="

# 1. asdf toolchains (java-25 for the JVM backend, opentofu for infra fmt). The JVM
#    conformance gate (harness.findJava25) resolves temurin-25 by globbing
#    ~/.asdf/installs/java/temurin-25*, independent of the active PATH java.
echo "-- asdf language toolchains (.tool-versions) --"
if have asdf; then
  if [ "$CHECK_ONLY" = 0 ]; then
    asdf plugin add java       2>/dev/null || true
    asdf plugin add opentofu   2>/dev/null || true
    asdf install              2>&1 | tail -1 || true
  fi
  for t in java opentofu; do
    if asdf where "$t" >/dev/null 2>&1; then ok "asdf $t -> $(asdf current "$t" 2>/dev/null | awk 'NR==2{print $2}')"
    else miss "asdf $t (run: asdf plugin add $t && asdf install)"; fi
  done
  case ":$PATH:" in
    *":$HOME/.asdf/shims:"*) ok "asdf shims on PATH" ;;
    *) warn "asdf shims NOT on PATH — add \$HOME/.asdf/shims so 'java'/'tofu' resolve to the pinned versions (the test gates still find java-25 via the asdf glob regardless)" ;;
  esac
else
  miss "asdf not found — install asdf (https://asdf-vm.com), then re-run. Provides java-25 + opentofu."
fi

# 2. Python libraries for the D4 numeric/ML interop + plotting tier.
echo "-- python libraries (D4 interop) --"
PY=$(command -v python3 || true)
if [ -n "$PY" ]; then
  if [ "$CHECK_ONLY" = 0 ]; then "$PY" -m pip install --quiet numpy matplotlib 2>/dev/null || true; fi
  for m in numpy matplotlib; do
    if "$PY" -c "import $m" 2>/dev/null; then ok "python: $m $("$PY" -c "import $m;print($m.__version__)" 2>/dev/null)"
    else miss "python: $m (pip install $m)"; fi
  done
  if "$PY" -c 'import sysconfig,os;exit(0 if os.path.exists(os.path.join(sysconfig.get_path("include"),"Python.h")) else 1)' 2>/dev/null; then
    ok "python dev headers (Python.h) — CPython embed"
  else warn "Python.h missing (apt-get install python3-dev) — only needed for the CPython-embed D4 path"; fi
else miss "python3 not found"; fi

# 3. Native numeric stack for the D3 BLAS tier (C + LLVM backends link -lopenblas).
echo "-- native BLAS (D3, C/LLVM backends) --"
LDCONFIG=$(command -v ldconfig || echo /sbin/ldconfig)
if { "$LDCONFIG" -p 2>/dev/null | grep -qi openblas; } || ls /lib/*/libopenblas.so* /usr/lib/*/libopenblas.so* >/dev/null 2>&1; then
  ok "libopenblas present"; else miss "libopenblas (apt-get install libopenblas-dev)"; fi
if [ -n "$(find /usr/include -name cblas.h -print -quit 2>/dev/null)" ]; then ok "cblas.h present"; else miss "cblas.h (apt-get install libopenblas-dev)"; fi
for c in cc clang; do have "$c" && ok "$c present" || warn "$c not found (cc: build-essential, clang: clang)"; done

# 4. BEAM runtime for the live OTP/distributed tier (escript runs the BEAM backend).
echo "-- BEAM runtime (D5 live OTP) --"
have escript && ok "escript present (OTP $(erl -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().' -noshell 2>/dev/null))" || warn "escript not found (asdf erlang, or apt-get install erlang)"

# 5. Infra-as-code: a container runtime for the E4 live round-trip + an HCL fmt tool.
echo "-- infra-as-code (E4 wavelet) --"
if have docker; then ok "docker present ($(docker --version 2>/dev/null | awk '{print $3}' | tr -d ,)) — runs the FOSS compose specs"
elif have podman; then ok "podman present — runs the FOSS compose specs"
else warn "no container runtime — install docker OR podman (rootless podman also needs newuidmap/newgidmap: apt-get install uidmap). The emitted compose.yaml is OCI-standard, so either works."; fi
have docker-compose || docker compose version >/dev/null 2>&1 && ok "docker compose present" || warn "docker compose plugin missing (only needed to bring FOSS backends up locally)"
if have tofu; then ok "tofu present (HCL fmt-check)"
elif have terraform; then ok "terraform present (HCL fmt-check; iacBinary prefers tofu, falls back to terraform)"
else warn "no tofu/terraform — the fmt-check sweep SKIPS without one (asdf install provides opentofu; ensure ~/.asdf/shims on PATH)"; fi

# 6. Rust toolchain (the Rust source backend) — managed by rustup, not asdf.
echo "-- rust backend --"
have rustc && ok "rustc $(rustc --version 2>/dev/null | awk '{print $2}')" || warn "rustc not found (rustup: https://rustup.rs) — only needed for the Rust source backend"

echo "== done =="
