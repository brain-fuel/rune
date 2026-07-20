package core

import "hash"

// HashTerm computes the structural Merkle hash of a core term.
//
// THE STANDING RULE (CLAUDE.md): hashing operates structurally on the core term
// and MUST NEVER call eval, normalize, or a conversion routine. A definition's
// identity is its SYNTAX, never its meaning modulo conversion. Because the core
// is de Bruijn, alpha-equivalent terms are literally equal here and therefore
// hash equal; Scope.Name is a pretty-printing hint and is deliberately not fed
// to the digest.
//
// The preimage layout is FROZEN at hashFormatVersion 0x06: this match must
// write byte-for-byte what the pre-goplus writeTerm switch wrote. The hash-corpus
// harness (cmd/hashdump) is the gate.
func HashTerm(t Tm) Hash {
	h := newHasher()
	h.Write([]byte{hashFormatVersion})
	writeTerm(h, t)
	var out Hash
	copy(out[:], h.Sum(nil))
	return out
}

func writeTerm(h hash.Hash, t Tm) {
	match t {
	case Var(idx):
		h.Write([]byte{tagVar})
		writeInt(h, idx)
	case Ref(rh):
		h.Write([]byte{tagRef})
		h.Write(rh[:])
	case Univ(lvl):
		h.Write([]byte{tagUniv})
		writeInt(h, lvl)
	case Pi(icit, qty, dom, cod):
		h.Write([]byte{tagPi, byte(icit), byte(qty)})
		writeTerm(h, dom)
		writeScope(h, cod)
	case Lam(icit, qty, body):
		h.Write([]byte{tagLam, byte(icit), byte(qty)})
		writeScope(h, body)
	case App(fn, arg, icit):
		h.Write([]byte{tagApp, byte(icit)})
		writeTerm(h, fn)
		writeTerm(h, arg)
	case Let(ty, val, body):
		h.Write([]byte{tagLet})
		writeOptTerm(h, ty)
		writeTerm(h, val)
		writeScope(h, body)
	case Ann(term, ty):
		h.Write([]byte{tagAnn})
		writeTerm(h, term)
		writeTerm(h, ty)
	case Prop:
		h.Write([]byte{tagProp})
	case Eq(ty, l, r):
		h.Write([]byte{tagEq})
		writeTerm(h, ty)
		writeTerm(h, l)
		writeTerm(h, r)
	case Refl(tm):
		h.Write([]byte{tagRefl})
		writeTerm(h, tm)
	case Cast(a, b, p, x):
		h.Write([]byte{tagCast})
		writeTerm(h, a)
		writeTerm(h, b)
		writeTerm(h, p)
		writeTerm(h, x)
	case Subst(a, x, y, prf, p, px):
		h.Write([]byte{tagSubst})
		writeTerm(h, a)
		writeTerm(h, x)
		writeTerm(h, y)
		writeTerm(h, prf)
		writeTerm(h, p)
		writeTerm(h, px)
	case Sig(qty, dom, cod):
		h.Write([]byte{tagSig, byte(qty)})
		writeTerm(h, dom)
		writeScope(h, cod)
	case Pair(dom, cod, a, b):
		h.Write([]byte{tagPair})
		writeTerm(h, dom)
		writeScope(h, cod)
		writeTerm(h, a)
		writeTerm(h, b)
	case Fst(p):
		h.Write([]byte{tagFst})
		writeTerm(h, p)
	case Snd(p):
		h.Write([]byte{tagSnd})
		writeTerm(h, p)
	case NatLit(n, zero, succ):
		// A compressed numeral hashes its number (canonical big-endian,
		// sign-free for Nat) framed by an explicit length, plus the nat
		// binding's zero/succ constructor hashes (a literal is RELATIVE to a
		// particular nat, so two bindings give distinct terms). Structural
		// and eval-free, per the standing rule. tagNatLit is append-only.
		h.Write([]byte{tagNatLit})
		b := n.Bytes() // big-endian magnitude; empty for 0
		writeInt(h, len(b))
		h.Write(b)
		h.Write(zero[:])
		h.Write(succ[:])
	case Meta(_):
		// A metavariable has no content identity. Reaching one here means a
		// definition was stored before being zonked — a checker bug, not data.
		panic("core.HashTerm: metavariable in hashable core (unzonked term)")
	}
}
