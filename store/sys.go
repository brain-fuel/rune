package store

import "goforge.dev/rune/v3/core"

// The cofibration-validity builtin group (§F phase 3b): WHEN a face holds, and
// the partial elements / systems that live under such an assumption. Shipped as
// a builtin group against the face lattice (3a). A partial element of A on φ is
// just `holds φ -> A` (a Pi — no new former needed); a system is a function on a
// disjunction that dispatches by which face holds.
//
//	holds : F -> Prop                     the proposition "this face is true"
//	htop  : holds ftop                    ⊤ always holds
//	hand  : (φ ψ : F) -> holds φ -> holds ψ -> holds (fand φ ψ)
//	horl  : (φ ψ : F) -> holds φ -> holds (for φ ψ)
//	horr  : (φ ψ : F) -> holds ψ -> holds (for φ ψ)
//
// holds is proof-irrelevant (its values live in Prop), which is exactly what
// makes a system's overlap-agreement obligation hold definitionally at the
// canonical level — two values on an overlapping face are equal because the
// proofs of holding are. The intros are canonical (permanently neutral heads,
// like datatype constructors); there are no ι-rules of their own — computation
// happens at the Kan operations (3d hcomp), which inspect the face and feed the
// system `htop` when it is ⊤. holds at a satisfied atomic face computes through
// the 3a lattice rules: `ieq0 i0 ~> ftop`, so `holds (ieq0 i0) ≡ holds ftop`,
// inhabited by `htop`. There is no intro for `holds fbot`: ⊥ never holds.
//
// No hash-format bump; own hash space (face/interval/path/fib untouched).

var sysNames = [5]string{"holds", "htop", "hand", "horl", "horr"}

type sysEntry struct {
	hashes [5]core.Hash
}

// AddSystems registers the cofibration-validity group against the registered
// face group, binding its names and returning the member hashes in sysNames
// order.
func (s *Store) AddSystems(face [7]core.Hash) [5]core.Hash {
	tys := sysTypes(face)

	g := newHasher()
	g.Write([]byte{defFormatVersion, 'S'})
	for _, ty := range tys {
		th := core.HashTerm(ty)
		g.Write(th[:])
	}
	var group core.Hash
	copy(group[:], g.Sum(nil))

	var hs [5]core.Hash
	for i := range hs {
		h := newHasher()
		h.Write([]byte{defFormatVersion, 's'})
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
		s.names[sysNames[i]] = hs[i]
	}
	s.sy = &sysEntry{hashes: hs}
	return hs
}

// SysRoleOf implements core.SysInfo.
func (s *Store) SysRoleOf(h core.Hash) core.SysRole {
	if s.sy == nil {
		return core.SRoleNone
	}
	roles := [5]core.SysRole{
		core.SRoleHolds, core.SRoleTop, core.SRoleAnd, core.SRoleOrL, core.SRoleOrR,
	}
	for i, sh := range s.sy.hashes {
		if sh == h {
			return roles[i]
		}
	}
	return core.SRoleNone
}

// SysHash implements core.SysInfo: the hash of the member with the given role,
// for the Kan ι-rules to produce a validity proof (e.g. htop for a ⊤ system).
func (s *Store) SysHash(role core.SysRole) (core.Hash, bool) {
	if s.sy == nil {
		return core.Hash{}, false
	}
	idx := map[core.SysRole]int{
		core.SRoleHolds: 0, core.SRoleTop: 1, core.SRoleAnd: 2, core.SRoleOrL: 3, core.SRoleOrR: 4,
	}
	i, ok := idx[role]
	if !ok {
		return core.Hash{}, false
	}
	return s.sy.hashes[i], true
}

// SysHashes returns the registered group's hashes (and whether it exists).
func (s *Store) SysHashes() ([5]core.Hash, bool) {
	if s.sy == nil {
		return [5]core.Hash{}, false
	}
	return s.sy.hashes, true
}

// SysNames returns the surface names of the group members, in hash order.
func SysNames() [5]string { return sysNames }

// sysTypes builds the five member types: holds as Placeholder(0), and the face
// members F = face[0], fand = face[3], for = face[4], ftop = face[5].
func sysTypes(face [7]core.Hash) [5]core.Tm {
	v := func(i int) core.Tm { return core.Var{Idx: i} }
	pi := func(name string, dom core.Tm, cod core.Tm) core.Tm {
		return core.Pi{Dom: dom, Cod: core.Scope{Name: name, Body: cod}}
	}
	app := func(f core.Tm, xs ...core.Tm) core.Tm {
		for _, x := range xs {
			f = core.App{Fn: f, Arg: x, Icit: core.Expl}
		}
		return f
	}
	fTy := core.Tm(core.Ref{Hash: face[0]})
	fand := core.Tm(core.Ref{Hash: face[3]})
	forF := core.Tm(core.Ref{Hash: face[4]})
	ftop := core.Tm(core.Ref{Hash: face[5]})
	holds := core.Tm(core.Ref{Hash: Placeholder(0)})
	h := func(x core.Tm) core.Tm { return app(holds, x) }

	// holds : F -> Prop
	holdsTy := pi("phi", fTy, core.Prop{})
	// htop : holds ftop
	htopTy := h(ftop)
	// hand : (φ : F) -> (ψ : F) -> holds φ -> holds ψ -> holds (fand φ ψ)
	handTy := pi("phi", fTy, pi("psi", fTy,
		pi("hp", h(v(1)), pi("hq", h(v(1)),
			h(app(fand, v(3), v(2)))))))
	// horl : (φ : F) -> (ψ : F) -> holds φ -> holds (for φ ψ)
	horlTy := pi("phi", fTy, pi("psi", fTy,
		pi("hp", h(v(1)), h(app(forF, v(2), v(1))))))
	// horr : (φ : F) -> (ψ : F) -> holds ψ -> holds (for φ ψ)
	horrTy := pi("phi", fTy, pi("psi", fTy,
		pi("hq", h(v(0)), h(app(forF, v(2), v(1))))))

	return [5]core.Tm{holdsTy, htopTy, handTy, horlTy, horrTy}
}
