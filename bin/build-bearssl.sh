#!/usr/bin/env bash
# Build the vendored BearSSL static lib (third_party/bearssl/build/libbearssl.a).
# Needed for the Phase-3 crypto/TLS host bodies on the native backends (C, LLVM,
# Rust) — they ship no digest of their own. The source is vendored; this just
# compiles it. The harness skips the crypto-native tests when the lib is absent.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
dir="$here/third_party/bearssl"
if [ ! -d "$dir" ]; then
  echo "bearssl source missing at $dir" >&2
  exit 1
fi
make -C "$dir" -j"$(nproc 2>/dev/null || echo 4)" build/libbearssl.a
# Drop the shared lib so native links pick the static archive (no runtime .so dep).
rm -f "$dir/build/libbearssl.so"
echo "built $dir/build/libbearssl.a"
