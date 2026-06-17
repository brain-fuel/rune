package store

import "goforge.dev/rune/v3/core"

// The face lattice builtin group (§F phase 3a): cofibrations — the formulas that
// say WHERE on the interval a partial element is defined. Shipped as a builtin
// group like the interval and path kits, on its own hash space (fib/interval/
// path hashes untouched). It is the substrate the partial elements (3b) and the
// Kan operations (hcomp/comp, 3d–3e) restrict against.
//
//	F     : U              a cofibration / face formula
//	ieq0  : I -> F         the atomic constraint (i = 0)
//	ieq1  : I -> F         the atomic constraint (i = 1)
//	fand  : F -> F -> F    conjunction (meet)
//	for   : F -> F -> F    disjunction (join)
//	ftop  : F              true (the whole cube)
//	fbot  : F              false (the empty face)
//
// The distributive lattice computes on endpoints and on ⊤/⊥ by ι-rule
// (core.FaceInfo / tryFaceIota), exactly analogous to phase 1's De Morgan
// algebra on the interval: `ieq0 i0 ~> ftop`, `ieq0 i1 ~> fbot`, `fand ftop φ ~>
// φ`, `for fbot φ ~> φ`, … A neutral face stays stuck; the lattice LAWS
// (idempotence, full distributivity) on neutrals are not definitional — they
// become provable, not reductions, the same honesty the interval and path
// algebras keep. No hash-format bump: no new core constructor. `holds : F -> Prop`
// (when does a face hold) is phase 3b, not here — 3a is the formula algebra only.

var faceNames = [7]string{"F", "ieq0", "ieq1", "fand", "for", "ftop", "fbot"}

type faceEntry struct {
	hashes [7]core.Hash
}

// AddFace registers the face-lattice builtin group against the already-
// registered interval group (for ieq0/ieq1 : I -> F), binding its names and
// returning the member hashes in faceNames order.
func (s *Store) AddFace(iv [6]core.Hash) [7]core.Hash {
	tys := faceTypes(iv)

	g := newHasher()
	g.Write([]byte{defFormatVersion, 'C'}) // C: cofibration
	for _, ty := range tys {
		th := core.HashTerm(ty)
		g.Write(th[:])
	}
	var group core.Hash
	copy(group[:], g.Sum(nil))

	var hs [7]core.Hash
	for i := range hs {
		h := newHasher()
		h.Write([]byte{defFormatVersion, 'c'})
		h.Write(group[:])
		writeUint(h, uint64(i))
		copy(hs[i][:], h.Sum(nil))
	}

	subst := func(t core.Tm) core.Tm {
		for i := range hs {
			t = replaceRef(t, Placeholder(i), hs[i])
		}
		return t
	}
	for i, ty := range tys {
		s.defs[hs[i]] = Def{Type: subst(ty)}
		s.names[faceNames[i]] = hs[i]
	}
	s.fc = &faceEntry{hashes: hs}
	return hs
}

// FaceRoleOf implements core.FaceInfo.
func (s *Store) FaceRoleOf(h core.Hash) core.FaceRole {
	if s.fc == nil {
		return core.CRoleNone
	}
	roles := [7]core.FaceRole{
		core.CRoleF, core.CRoleEq0, core.CRoleEq1,
		core.CRoleAnd, core.CRoleOr, core.CRoleTop, core.CRoleBot,
	}
	for i, fh := range s.fc.hashes {
		if fh == h {
			return roles[i]
		}
	}
	return core.CRoleNone
}

// FaceHash implements core.FaceInfo: the hash of the member with the given role,
// for the ι-rules to produce ftop/fbot.
func (s *Store) FaceHash(role core.FaceRole) (core.Hash, bool) {
	if s.fc == nil {
		return core.Hash{}, false
	}
	idx := map[core.FaceRole]int{
		core.CRoleF: 0, core.CRoleEq0: 1, core.CRoleEq1: 2,
		core.CRoleAnd: 3, core.CRoleOr: 4, core.CRoleTop: 5, core.CRoleBot: 6,
	}
	i, ok := idx[role]
	if !ok {
		return core.Hash{}, false
	}
	return s.fc.hashes[i], true
}

// FaceHashes returns the registered group's hashes (and whether it exists).
func (s *Store) FaceHashes() ([7]core.Hash, bool) {
	if s.fc == nil {
		return [7]core.Hash{}, false
	}
	return s.fc.hashes, true
}

// FaceNames returns the surface names of the group members, in hash order.
func FaceNames() [7]string { return faceNames }

// faceTypes builds the seven member types, own members as Placeholder(i) and the
// interval I = iv[0] by its registered hash. Scope names are display hints only.
func faceTypes(iv [6]core.Hash) [7]core.Tm {
	pi := func(name string, dom core.Tm, cod core.Tm) core.Tm {
		return core.Pi{Dom: dom, Cod: core.Scope{Name: name, Body: cod}}
	}
	fTy := core.Tm(core.Ref{Hash: Placeholder(0)})
	ivTy := core.Tm(core.Ref{Hash: iv[0]})

	// F : U
	fSort := core.Tm(core.Univ{})
	// ieq0, ieq1 : I -> F
	ieqTy := pi("i", ivTy, fTy)
	// fand, for : F -> F -> F
	binTy := pi("p", fTy, pi("q", fTy, fTy))
	// ftop, fbot : F
	topTy := fTy
	botTy := fTy
	return [7]core.Tm{fSort, ieqTy, ieqTy, binTy, binTy, topTy, botTy}
}
