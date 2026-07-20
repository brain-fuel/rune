//go:generate go run goforge.dev/goplus/cmd/goplus@v0.14.0 gen .
package core

import (
	"encoding/binary"
	"encoding/hex"
	"hash"

	"goforge.dev/blake3sum/components/blake3"
)

// Hash is a content hash: the Merkle digest of a piece of elaborated core.
//
// Hashing uses BLAKE3 (goforge.dev/blake3sum); blake3.New(32, nil) is an unkeyed
// 256-bit streaming hasher implementing hash.Hash. The digest width and algorithm
// are an implementation detail behind this type — changing them changes every
// content hash, which is what the preimage tag (hashFormatVersion) versions.
type Hash [32]byte

// newHasher returns the streaming hasher backing every content digest.
func newHasher() hash.Hash { return blake3.New(32, nil) }

// String renders the hash as lowercase hex.
func (h Hash) String() string { return hex.EncodeToString(h[:]) }

// Short renders the first 12 hex characters, for human-facing listings.
func (h Hash) Short() string { return hex.EncodeToString(h[:])[:12] }

// hashFormatVersion is the preimage tag that prefixes every term digest. Bumping it
// changes every content hash, which is the intended migration lever if the core's
// serialization or hash algorithm changes. NEVER reuse a tag across incompatible
// encodings. 0x06: bare numerals lower to a compressed core numeral NatLit by
// DEFAULT (C7 / R-NUM, Decision 2) — every numeral-bearing def re-hashes; the
// deliberate one-time cache event paired with the default-lowering switch.
// 0x05: dependent pairs (Sig/Pair/Fst/Snd) joined the core (C1/R-SUM).
// 0x04: universe levels joined the Univ preimage (Phase 6).
// 0x03 added quantity; 0x02 plicity; 0x01 BLAKE3; 0x00 sha256.
const hashFormatVersion byte = 0x06

// Constructor tags. Each core constructor gets a distinct, stable byte so that
// differently-shaped terms cannot collide by accident. Append-only: never renumber.
const (
	tagVar byte = iota + 1
	tagRef
	tagUniv
	tagPi
	tagLam
	tagApp
	tagLet
	tagAnn
	tagProp
	tagEq
	tagRefl
	tagCast
	tagSubst
	tagSig
	tagPair
	tagFst
	tagSnd
	tagNatLit
)

// HashTerm and writeTerm live in hash.gp: writeTerm is a match over the Tm
// enum. The preimage layout they produce is FROZEN — byte-for-byte what the
// pre-goplus switch wrote (cmd/hashdump is the gate).

// writeScope hashes a binder body. Scope.Name is NOT written: it is a display hint,
// not identity. This is what makes alpha-equivalent terms hash equal.
func writeScope(h hash.Hash, s Scope) {
	writeTerm(h, s.Body)
}

func writeOptTerm(h hash.Hash, t Tm) {
	if t == nil {
		h.Write([]byte{0})
		return
	}
	h.Write([]byte{1})
	writeTerm(h, t)
}

func writeInt(h hash.Hash, n int) {
	var buf [8]byte
	binary.LittleEndian.PutUint64(buf[:], uint64(n))
	h.Write(buf[:])
}
