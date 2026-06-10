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
// encodings. 0x04: universe levels joined the Univ preimage (Phase 6).
// 0x03 added quantity; 0x02 plicity; 0x01 BLAKE3; 0x00 sha256.
const hashFormatVersion byte = 0x04

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
)

// HashTerm computes the structural Merkle hash of a core term.
//
// THE STANDING RULE (CLAUDE.md): hashing operates structurally on the core term and
// MUST NEVER call eval, normalize, or a future conversion routine. A definition's
// identity is its SYNTAX, never its meaning modulo conversion. Because the core is de
// Bruijn, alpha-equivalent terms are literally equal here and therefore hash equal;
// Scope.Name is a pretty-printing hint and is deliberately not fed to the digest.
func HashTerm(t Tm) Hash {
	h := newHasher()
	h.Write([]byte{hashFormatVersion})
	writeTerm(h, t)
	var out Hash
	copy(out[:], h.Sum(nil))
	return out
}

func writeTerm(h hash.Hash, t Tm) {
	switch tm := t.(type) {
	case Var:
		h.Write([]byte{tagVar})
		writeInt(h, tm.Idx)
	case Ref:
		h.Write([]byte{tagRef})
		h.Write(tm.Hash[:])
	case Univ:
		h.Write([]byte{tagUniv})
		writeInt(h, tm.Lvl)
	case Pi:
		h.Write([]byte{tagPi, byte(tm.Icit), byte(tm.Qty)})
		writeTerm(h, tm.Dom)
		writeScope(h, tm.Cod)
	case Lam:
		h.Write([]byte{tagLam, byte(tm.Icit), byte(tm.Qty)})
		writeScope(h, tm.Body)
	case App:
		h.Write([]byte{tagApp, byte(tm.Icit)})
		writeTerm(h, tm.Fn)
		writeTerm(h, tm.Arg)
	case Let:
		h.Write([]byte{tagLet})
		writeOptTerm(h, tm.Ty)
		writeTerm(h, tm.Val)
		writeScope(h, tm.Body)
	case Ann:
		h.Write([]byte{tagAnn})
		writeTerm(h, tm.Term)
		writeTerm(h, tm.Ty)
	case Prop:
		h.Write([]byte{tagProp})
	case Eq:
		h.Write([]byte{tagEq})
		writeTerm(h, tm.Ty)
		writeTerm(h, tm.L)
		writeTerm(h, tm.R)
	case Refl:
		h.Write([]byte{tagRefl})
		writeTerm(h, tm.Tm)
	case Cast:
		h.Write([]byte{tagCast})
		writeTerm(h, tm.A)
		writeTerm(h, tm.B)
		writeTerm(h, tm.P)
		writeTerm(h, tm.X)
	case Subst:
		h.Write([]byte{tagSubst})
		writeTerm(h, tm.A)
		writeTerm(h, tm.X)
		writeTerm(h, tm.Y)
		writeTerm(h, tm.Prf)
		writeTerm(h, tm.P)
		writeTerm(h, tm.Px)
	case Meta:
		// A metavariable has no content identity. Reaching one here means a
		// definition was stored before being zonked — a checker bug, not data.
		panic("core.HashTerm: metavariable in hashable core (unzonked term)")
	default:
		panic("core.HashTerm: unknown Tm constructor")
	}
}

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
