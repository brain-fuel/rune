package store

import (
	"encoding/binary"

	"goforge.dev/blake3sum/components/blake3"
	"goforge.dev/rune/v3/core"
)

// newHasher returns the streaming BLAKE3 hasher backing every definition digest,
// matching the algorithm core.HashTerm uses for terms.
func newHasher() *blake3.Hasher { return blake3.New(32, nil) }

// content is the hashable content of one definition: its (optional) type and its
// body. A definition's identity is the Merkle hash of this content, with internal
// references already being the content hashes of their targets — so the hash
// transitively summarizes the definition's entire syntactic content.
type content struct {
	Type core.Tm // nil if the definition carried no type annotation
	Body core.Tm
}

// defFormatVersion is the preimage tag for definition and SCC digests. 0x01: BLAKE3
// (0x00 was sha256).
const defFormatVersion byte = 0x01

// hashContent hashes a single, self-contained definition (no intra-group
// references). This is the acyclic singleton case and what Store.Add uses.
//
// NEVER hash modulo conversion: hashContent calls only core.HashTerm, which is
// structural. Identity is syntax.
func hashContent(c content) core.Hash {
	h := newHasher()
	h.Write([]byte{defFormatVersion, 'd'})
	if c.Type == nil {
		h.Write([]byte{0})
	} else {
		h.Write([]byte{1})
		th := core.HashTerm(c.Type)
		h.Write(th[:])
	}
	bh := core.HashTerm(c.Body)
	h.Write(bh[:])
	var out core.Hash
	copy(out[:], h.Sum(nil))
	return out
}

// HashSCC hashes a strongly-connected group of mutually-recursive definitions as a
// unit (Unison-style), returning one content hash per member in input order.
//
// Mutually-recursive definitions cannot reference each other by content hash —
// that would be a cycle in the Merkle DAG. The group is therefore hashed together:
// intra-group references must already be rewritten to the positional placeholder
// Placeholder(i), so the group's content is self-contained. The group hash digests
// every member's content in order; each member's hash is then derived from the group
// hash and its position, so the whole SCC shares one identity yet members stay
// distinguishable.
//
// A singleton group with no self-reference reduces to hashContent, so the common
// acyclic case keeps its plain content hash (and alpha-equivalent definitions still
// hash equal). Phase 0 exercises only that case; the group machinery is laid down so
// Phase 1's recursive definitions slot in without a redesign.
func HashSCC(group []content) []core.Hash {
	if len(group) == 1 && !referencesPlaceholder(group[0]) {
		return []core.Hash{hashContent(group[0])}
	}
	g := newHasher()
	g.Write([]byte{defFormatVersion, 's'})
	writeUint(g, uint64(len(group)))
	for _, c := range group {
		ch := hashContent(c)
		g.Write(ch[:])
	}
	var groupHash core.Hash
	copy(groupHash[:], g.Sum(nil))

	out := make([]core.Hash, len(group))
	for i := range group {
		m := newHasher()
		m.Write([]byte{defFormatVersion, 'm'})
		m.Write(groupHash[:])
		writeUint(m, uint64(i))
		var h core.Hash
		copy(h[:], m.Sum(nil))
		out[i] = h
	}
	return out
}

// Placeholder is the stand-in content hash for the i-th member of an SCC while the
// group is being hashed. Intra-group references are rewritten to Placeholder(i)
// before HashSCC so the group content is self-contained and position-stable.
func Placeholder(i int) core.Hash {
	h := newHasher()
	h.Write([]byte{defFormatVersion, 'p'})
	writeUint(h, uint64(i))
	var out core.Hash
	copy(out[:], h.Sum(nil))
	return out
}

func referencesPlaceholder(c content) bool {
	placeholders := map[core.Hash]bool{}
	for i := 0; i < 64; i++ {
		placeholders[Placeholder(i)] = true
	}
	found := false
	var walk func(t core.Tm)
	walk = func(t core.Tm) {
		if found || t == nil {
			return
		}
		switch x := t.(type) {
		case core.Ref:
			if placeholders[x.Hash] {
				found = true
			}
		case core.App:
			walk(x.Fn)
			walk(x.Arg)
		case core.Lam:
			walk(x.Body.Body)
		case core.Pi:
			walk(x.Dom)
			walk(x.Cod.Body)
		case core.Let:
			walk(x.Ty)
			walk(x.Val)
			walk(x.Body.Body)
		case core.Ann:
			walk(x.Term)
			walk(x.Ty)
		}
	}
	walk(c.Type)
	walk(c.Body)
	return found
}

func writeUint(h interface{ Write([]byte) (int, error) }, n uint64) {
	var buf [8]byte
	binary.LittleEndian.PutUint64(buf[:], n)
	h.Write(buf[:])
}
