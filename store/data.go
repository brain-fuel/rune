package store

import (
	"goforge.dev/rune/core"
)

// Datatype declarations (Phase 4). A `data` declaration introduces a GROUP of
// definitions that exist only together: the type former, its constructors, and
// its eliminator. None has a body — the former and constructors are canonical
// (permanently neutral heads), and the eliminator computes by the ι-rule the
// evaluator applies, not by δ-unfolding. Identity is content: the group hash
// digests the former's type and every constructor's type (with intra-group
// references rewritten to positional placeholders, like HashSCC), and each
// member's hash derives from the group hash and its role, so structurally
// identical declarations are the same datatype.

// DataDecl is the elaborated content of one datatype declaration.
type DataDecl struct {
	Name      string
	Ty        core.Tm // the former's type: a parameter telescope ending in U
	NumParams int     // length of that telescope
	CtorNames []string
	CtorTys   []core.Tm      // constructor types, with the former as Placeholder(0)
	Sigs      []core.CtorSig // per-constructor argument arity + recursive positions
	ElimTy    core.Tm        // generated eliminator type, former as Placeholder(0)
}

// dataEntry is the stored, hash-resolved form of a declaration.
type dataEntry struct {
	decl  DataDecl
	data  core.Hash
	ctors []core.Hash
	elim  core.Hash
}

// AddData stores a datatype declaration group and binds its names. It returns
// the hashes of the former, the constructors, and the eliminator (bound as
// Name, the constructor names, and Name+"Elim").
func (s *Store) AddData(d DataDecl) (data core.Hash, ctors []core.Hash, elim core.Hash) {
	g := newHasher()
	g.Write([]byte{defFormatVersion, 'D'})
	th := core.HashTerm(d.Ty)
	g.Write(th[:])
	writeUint(g, uint64(d.NumParams))
	writeUint(g, uint64(len(d.CtorTys)))
	for _, ct := range d.CtorTys {
		ch := core.HashTerm(ct)
		g.Write(ch[:])
	}
	var group core.Hash
	copy(group[:], g.Sum(nil))

	derive := func(role byte, i int) core.Hash {
		h := newHasher()
		h.Write([]byte{defFormatVersion, role})
		h.Write(group[:])
		writeUint(h, uint64(i))
		var out core.Hash
		copy(out[:], h.Sum(nil))
		return out
	}
	data = derive('d', 0)
	elim = derive('e', 0)
	ctors = make([]core.Hash, len(d.CtorTys))
	for i := range d.CtorTys {
		ctors[i] = derive('c', i)
	}

	subst := func(t core.Tm) core.Tm {
		t = replaceRef(t, Placeholder(0), data)
		for i, ch := range ctors {
			t = replaceRef(t, Placeholder(i+1), ch)
		}
		return t
	}

	// The former: type as declared, no body.
	s.defs[data] = Def{Type: subst(d.Ty)}
	s.names[d.Name] = data
	// Constructors: types with the former resolved, no bodies.
	for i, ct := range d.CtorTys {
		s.defs[ctors[i]] = Def{Type: subst(ct)}
		s.names[d.CtorNames[i]] = ctors[i]
	}
	// The eliminator: generated type, computes by ι.
	s.defs[elim] = Def{Type: subst(d.ElimTy)}
	s.names[d.Name+"Elim"] = elim

	e := dataEntry{decl: d, data: data, ctors: ctors, elim: elim}
	s.dataByHash[data] = e
	for i, ch := range ctors {
		s.ctorRole[ch] = ctorRole{data: data, idx: i}
		_ = i
	}
	s.elimRole[elim] = data
	return data, ctors, elim
}

type ctorRole struct {
	data core.Hash
	idx  int
}

// CtorOf implements core.DataInfo: is h a constructor, and of what.
func (s *Store) CtorOf(h core.Hash) (core.Hash, int, bool) {
	r, ok := s.ctorRole[h]
	if !ok {
		return core.Hash{}, 0, false
	}
	return r.data, r.idx, true
}

// ElimOf implements core.DataInfo: is h an eliminator, and the data it
// eliminates with its parameter count and constructor signatures.
func (s *Store) ElimOf(h core.Hash) (core.ElimSig, bool) {
	dh, ok := s.elimRole[h]
	if !ok {
		return core.ElimSig{}, false
	}
	e := s.dataByHash[dh]
	return core.ElimSig{Data: dh, NumParams: e.decl.NumParams, Ctors: e.decl.Sigs}, true
}

// DataDeclOf returns the stored declaration group for a former's hash.
func (s *Store) DataDeclOf(h core.Hash) (DataDecl, []core.Hash, core.Hash, bool) {
	e, ok := s.dataByHash[h]
	if !ok {
		return DataDecl{}, nil, core.Hash{}, false
	}
	return e.decl, e.ctors, e.elim, true
}

// replaceRef substitutes every Ref to `from` with a Ref to `to`.
func replaceRef(t core.Tm, from, to core.Hash) core.Tm {
	switch x := t.(type) {
	case nil:
		return nil
	case core.Var, core.Univ, core.Prop, core.Meta:
		return t
	case core.Ref:
		if x.Hash == from {
			return core.Ref{Hash: to}
		}
		return t
	case core.Pi:
		return core.Pi{Icit: x.Icit, Qty: x.Qty, Dom: replaceRef(x.Dom, from, to),
			Cod: core.Scope{Name: x.Cod.Name, Body: replaceRef(x.Cod.Body, from, to)}}
	case core.Lam:
		return core.Lam{Icit: x.Icit, Qty: x.Qty,
			Body: core.Scope{Name: x.Body.Name, Body: replaceRef(x.Body.Body, from, to)}}
	case core.App:
		return core.App{Fn: replaceRef(x.Fn, from, to), Arg: replaceRef(x.Arg, from, to), Icit: x.Icit}
	case core.Let:
		var ty core.Tm
		if x.Ty != nil {
			ty = replaceRef(x.Ty, from, to)
		}
		return core.Let{Ty: ty, Val: replaceRef(x.Val, from, to),
			Body: core.Scope{Name: x.Body.Name, Body: replaceRef(x.Body.Body, from, to)}}
	case core.Ann:
		return core.Ann{Term: replaceRef(x.Term, from, to), Ty: replaceRef(x.Ty, from, to)}
	case core.Eq:
		return core.Eq{Ty: replaceRef(x.Ty, from, to), L: replaceRef(x.L, from, to), R: replaceRef(x.R, from, to)}
	case core.Refl:
		return core.Refl{Tm: replaceRef(x.Tm, from, to)}
	case core.Cast:
		return core.Cast{A: replaceRef(x.A, from, to), B: replaceRef(x.B, from, to),
			P: replaceRef(x.P, from, to), X: replaceRef(x.X, from, to)}
	case core.Subst:
		return core.Subst{A: replaceRef(x.A, from, to), X: replaceRef(x.X, from, to),
			Y: replaceRef(x.Y, from, to), Prf: replaceRef(x.Prf, from, to),
			P: replaceRef(x.P, from, to), Px: replaceRef(x.Px, from, to)}
	default:
		panic("store.replaceRef: unknown core term")
	}
}
