package store

import "goforge.dev/rune/v3/core"

// The DEPENDENT face-split builtin group (§F / R-BOX / A8): the motive-carrying
// counterpart of `fsplit`. Where `fsplit A φ ψ u v h : El A` returns an element of
// a FIXED type, `fsplitD` returns an element of a type that may DEPEND on which
// face holds, via a motive `C : holds (for φ ψ) -> UF`:
//
//	fsplitD : (φ : F) -> (ψ : F)
//	            -> (C : holds (for φ ψ) -> UF)
//	            -> (u : (h : holds φ) -> El (C (horl φ ψ h)))
//	            -> (v : (h : holds ψ) -> El (C (horr φ ψ h)))
//	            -> (h : holds (for φ ψ)) -> El (C h)
//
// This is the primitive a DEPENDENT element-system needs — exactly the e-component
// of a univalence Glue line, `e : (h : holds φ) -> El (Equiv (T h) B)`, whose type
// varies per face (Equiv A B at one face, Equiv B B at the other) and which neither
// the non-dependent `fsplit` nor the type-level `sysU` can express. With `sysU`
// (the T-component) it completes the A8 split-former pair. A separate group on its
// own hash space, built against the fibrant, face, and system groups, so their
// hashes — and every earlier chapter — stay stable.
//
// fsplitD COMPUTES by face-ι (core.SplitDInfo / the evaluator's trySplitDIota):
//
//	fsplitD φ ψ C u v h  ~>  u htop   when φ ≡ ⊤
//	fsplitD φ ψ C u v h  ~>  v htop   when ψ ≡ ⊤
//
// stuck when both faces are proper. The motive C is carried only for typing — it is
// ι-irrelevant to the reduct (the same way fstF's type arguments are). Overlap
// (both ⊤) takes the φ-branch; agreement is the caller's obligation (vacuous for
// disjoint faces like ieq0/ieq1). No hash-format bump (no new core constructor).

var splitDNames = [1]string{"fsplitD"}

type splitDEntry struct {
	hashes [1]core.Hash
}

// AddSplitD registers the dependent face-split group against the already-registered
// fibrant, face, and system groups, binding its name and returning the member hash.
func (s *Store) AddSplitD(fib [10]core.Hash, face [7]core.Hash, sys [5]core.Hash) [1]core.Hash {
	tys := splitDTypes(fib, face, sys)

	g := newHasher()
	g.Write([]byte{defFormatVersion, 'W'})
	for _, ty := range tys {
		th := core.HashTerm(ty)
		g.Write(th[:])
	}
	var group core.Hash
	copy(group[:], g.Sum(nil))

	var hs [1]core.Hash
	for i := range hs {
		h := newHasher()
		h.Write([]byte{defFormatVersion, 'w'})
		h.Write(group[:])
		writeUint(h, uint64(i))
		copy(hs[i][:], h.Sum(nil))
	}
	for i, ty := range tys {
		s.defs[hs[i]] = Def{Type: ty}
		s.names[splitDNames[i]] = hs[i]
	}
	s.sd = &splitDEntry{hashes: hs}
	return hs
}

// SplitDRoleOf implements core.SplitDInfo.
func (s *Store) SplitDRoleOf(h core.Hash) core.SplitDRole {
	if s.sd == nil {
		return core.SDRoleNone
	}
	if h == s.sd.hashes[0] {
		return core.SDRoleSplitD
	}
	return core.SDRoleNone
}

// SplitDHash implements core.SplitDInfo: the member hash, for callers to construct
// dependent element-systems.
func (s *Store) SplitDHash() (core.Hash, bool) {
	if s.sd == nil {
		return core.Hash{}, false
	}
	return s.sd.hashes[0], true
}

// SplitDHashes returns the registered group's hashes (and whether it exists).
func (s *Store) SplitDHashes() ([1]core.Hash, bool) {
	if s.sd == nil {
		return [1]core.Hash{}, false
	}
	return s.sd.hashes, true
}

// SplitDNames returns the surface names of the group members, in hash order.
func SplitDNames() [1]string { return splitDNames }

// splitDTypes builds the one member type, referencing the fibrant group (UF =
// fib[0], El = fib[1]), the face former (F = face[0]) and disjunction (for =
// face[4]), and the holds group (holds = sys[0], horl = sys[3], horr = sys[4]).
func splitDTypes(fib [10]core.Hash, face [7]core.Hash, sys [5]core.Hash) [1]core.Tm {
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
	uf := core.Tm(core.Ref{Hash: fib[0]})
	el := func(x core.Tm) core.Tm { return app(core.Ref{Hash: fib[1]}, x) }
	fTy := core.Tm(core.Ref{Hash: face[0]})
	holds := func(x core.Tm) core.Tm { return app(core.Ref{Hash: sys[0]}, x) }
	forC := func(x, y core.Tm) core.Tm { return app(core.Ref{Hash: face[4]}, x, y) }
	horl := func(p, q, h core.Tm) core.Tm { return app(core.Ref{Hash: sys[3]}, p, q, h) }
	horr := func(p, q, h core.Tm) core.Tm { return app(core.Ref{Hash: sys[4]}, p, q, h) }

	// u : (h : holds φ) -> El (C (horl φ ψ h))    in ctx [C,ψ,φ] (C=0,ψ=1,φ=2)
	uTy := pi("h", holds(v(2)), // holds φ
		el(app(v(1), horl(v(3), v(2), v(0))))) // El (C (horl φ ψ h)); under h: C=1,φ=3,ψ=2,h=0
	// v : (h : holds ψ) -> El (C (horr φ ψ h))    in ctx [u,C,ψ,φ] (u=0,C=1,ψ=2,φ=3)
	vTy := pi("h", holds(v(2)), // holds ψ
		el(app(v(2), horr(v(4), v(3), v(0))))) // El (C (horr φ ψ h)); under h: C=2,φ=4,ψ=3,h=0

	// fsplitD : (φ : F) -> (ψ : F) -> (C : holds (for φ ψ) -> UF)
	//             -> u -> v -> (h : holds (for φ ψ)) -> El (C h)
	splitDTy := pi("phi", fTy, //                                   [φ]: φ=0
		pi("psi", fTy, //                                           [ψ,φ]: ψ=0,φ=1
			pi("C", pi("_", holds(forC(v(1), v(0))), uf), //        C: holds (for φ ψ) -> UF
				pi("u", uTy, //                                     [u,C,ψ,φ]
					pi("v", vTy, //                                 [v,u,C,ψ,φ]
						pi("h", holds(forC(v(4), v(3))), //         h: holds (for φ ψ); φ=4,ψ=3
							el(app(v(3), v(0))))))))) //            -> El (C h); C=3,h=0

	return [1]core.Tm{splitDTy}
}
