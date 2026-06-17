package store

import "goforge.dev/rune/v3/core"

// The set-truncation HIT ‖A‖₀ (§F / R-HIT / A9, the dim-2 slice). It freely adds
// to A a proof that it is a SET: a DIM-2 path constructor `squash` identifying any
// two paths between the same points. A builtin group on its own hash space (tag
// X/x), built against the fibrant group. Four members:
//
//	Trunc0    : UF -> UF                                      the fibrant former
//	inc       : (A : UF) -> El A -> El (Trunc0 A)             the point constructor
//	squash    : (A : UF) -> (x y : El (Trunc0 A))
//	              -> (p q : El (pathF (Trunc0 A) x y))
//	              -> El (pathF (pathF (Trunc0 A) x y) p q)     the DIM-2 path ctor
//	trunc0Rec : (A B : UF)
//	              -> (setB : (x y : El B) -> (p q : El (pathF B x y))
//	                    -> El (pathF (pathF B x y) p q))       B is a set
//	              -> (f : El A -> El B)
//	              -> El (Trunc0 A) -> El B                      the recursor (into a set)
//
// Trunc0/inc/squash are permanently-neutral heads. trunc0Rec COMPUTES on the POINT
// constructor (core.TruncInfo / tryTruncIota): `trunc0Rec A B setB f (inc A a) ~>
// f a`. It ALSO computes along the DIM-2 INTERIOR — the `squash` SQUARE: a squash
// 2-cell observed at two interval coordinates reduces to the corresponding square
// in B built from the `setB` witness, observed at the same coordinates (the dim-2
// analog of the circle's dim-1 loop rule; boundary-coherent on the nose at both
// dimensions; see core/eval.go tryTruncIota). This makes set-truncation STATABLE
// and its recursor compute on points AND its dim-2 generator; propositional
// truncation already lives in Prop's impredicativity (CLAUDE.md), so this is the
// genuinely-new truncation. No hash-format bump.

var truncNames = [4]string{"Trunc0", "inc", "squash", "trunc0Rec"}

type truncEntry struct {
	hashes [4]core.Hash
}

// AddTrunc registers the set-truncation group against the registered fibrant group.
func (s *Store) AddTrunc(fib [10]core.Hash) [4]core.Hash {
	tys := truncTypes(fib)

	g := newHasher()
	g.Write([]byte{defFormatVersion, 'X'})
	for _, ty := range tys {
		th := core.HashTerm(ty)
		g.Write(th[:])
	}
	var group core.Hash
	copy(group[:], g.Sum(nil))

	var hs [4]core.Hash
	for i := range hs {
		h := newHasher()
		h.Write([]byte{defFormatVersion, 'x'})
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
		s.names[truncNames[i]] = hs[i]
	}
	s.tr = &truncEntry{hashes: hs}
	return hs
}

// TruncRoleOf implements core.TruncInfo.
func (s *Store) TruncRoleOf(h core.Hash) core.TruncRole {
	if s.tr == nil {
		return core.TRoleNone
	}
	roles := [4]core.TruncRole{core.TRoleTrunc, core.TRoleInc, core.TRoleSquash, core.TRoleElim}
	for i, hh := range s.tr.hashes {
		if hh == h {
			return roles[i]
		}
	}
	return core.TRoleNone
}

// TruncHash implements core.TruncInfo.
func (s *Store) TruncHash(role core.TruncRole) (core.Hash, bool) {
	if s.tr == nil {
		return core.Hash{}, false
	}
	idx := map[core.TruncRole]int{
		core.TRoleTrunc: 0, core.TRoleInc: 1, core.TRoleSquash: 2, core.TRoleElim: 3,
	}
	i, ok := idx[role]
	if !ok {
		return core.Hash{}, false
	}
	return s.tr.hashes[i], true
}

// TruncHashes returns the registered group's hashes (and whether it exists).
func (s *Store) TruncHashes() ([4]core.Hash, bool) {
	if s.tr == nil {
		return [4]core.Hash{}, false
	}
	return s.tr.hashes, true
}

// TruncNames returns the surface names of the group members, in hash order.
func TruncNames() [4]string { return truncNames }

func truncTypes(fib [10]core.Hash) [4]core.Tm {
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
	pathF := func(a, x, y core.Tm) core.Tm { return app(core.Ref{Hash: fib[4]}, a, x, y) }
	trunc := func(a core.Tm) core.Tm { return app(core.Ref{Hash: Placeholder(0)}, a) }

	// Trunc0 : UF -> UF
	truncTy := pi("A", uf, uf)
	// inc : (A : UF) -> El A -> El (Trunc0 A)   ([a,A]: A=v(1))
	incTy := pi("A", uf, pi("a", el(v(0)), el(trunc(v(1)))))
	// squash : (A) -> (x y : El (Trunc0 A)) -> (p q : El (pathF (Trunc0 A) x y))
	//            -> El (pathF (pathF (Trunc0 A) x y) p q)
	squashTy := pi("A", uf,
		pi("x", el(trunc(v(0))), // A=v0
			pi("y", el(trunc(v(1))), // A=v1
				// p : El (pathF (Trunc0 A) x y) — [y,x,A]: A=v2,x=v1,y=v0
				pi("p", el(pathF(trunc(v(2)), v(1), v(0))),
					// q : same — [p,y,x,A]: A=v3,x=v2,y=v1
					pi("q", el(pathF(trunc(v(3)), v(2), v(1))),
						// result — [q,p,y,x,A]: A=v4,x=v3,y=v2,p=v1,q=v0
						el(pathF(pathF(trunc(v(4)), v(3), v(2)), v(1), v(0))))))))
	// the "B is a set" predicate, as a type built under a context where B is at
	// de Bruijn index bIdx (with the surrounding binders already in scope).
	isSet := func(bIdx int) core.Tm {
		// (x y : El B) -> (p q : El (pathF B x y)) -> El (pathF (pathF B x y) p q)
		// under nested binders the B index shifts; compute relative to entry.
		return pi("x", el(v(bIdx)), // B at entry
			pi("y", el(v(bIdx+1)), // +1 binder (x)
				pi("p", el(pathF(v(bIdx+2), v(1), v(0))), // +2 (x,y): B=bIdx+2, x=v1,y=v0
					pi("q", el(pathF(v(bIdx+3), v(2), v(1))), // +3: B=bIdx+3, x=v2,y=v1
						el(pathF(pathF(v(bIdx+4), v(3), v(2)), v(1), v(0))))))) // +4
	}
	// trunc0Rec : (A B : UF) -> (setB : isSet B) -> (f : El A -> El B)
	//               -> El (Trunc0 A) -> El B
	//   stacks: [A]; [B,A]; [setB,B,A]; [f,setB,B,A]; [x,f,setB,B,A]
	elimTy := pi("A", uf,
		pi("B", uf,
			// setB : isSet B — under [B,A], B=v(0)
			pi("setB", isSet(0),
				// f : El A -> El B — dom El A under [setB,B,A]: A=v(2); cod El B: B=v(2)
				pi("f", pi("a", el(v(2)), el(v(2))),
					// x : El (Trunc0 A) — under [f,setB,B,A]: A=v(3)
					pi("x", el(trunc(v(3))),
						// result El B — under [x,f,setB,B,A]: B=v(3)
						el(v(3)))))))

	return [4]core.Tm{truncTy, incTy, squashTy, elimTy}
}
