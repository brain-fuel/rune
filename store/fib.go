package store

import "goforge.dev/rune/core"

// The fibrant builtin group (v3): two-level type theory as a second equality
// stratum, shipped the way v2 shipped quotients — a GROUP of bodiless,
// content-addressed definitions, not new core syntax. The presentation is a
// fibrant universe à la Tarski, embedded in the outer MLTT core:
//
//	UF     : U1                                     the universe of fibrant types (codes)
//	El     : UF -> U                                the underlying outer type of a code
//	fib    : U -> UF                                small outer types (sets) are fibrant
//	piF    : (A : UF) -> (El A -> UF) -> UF         closure under Pi
//	pathF  : (A : UF) -> El A -> El A -> UF         the INNER identity type (a fibrant type)
//	preflF : (A : UF) -> (x : El A) -> El (pathF A x x)
//	pathJ  : path induction; COMPUTES on preflF (ι)
//	pathU  : UF -> UF -> U1                         inner paths between fibrant types
//	ureflU : (A : UF) -> pathU A A
//	ua     : (A B : UF) -> (f : El A -> El B) -> (g : El B -> El A)
//	         -> ((x : El A) -> Eq (El A) (g (f x)) x)
//	         -> ((y : El B) -> Eq (El B) (f (g y)) y) -> pathU A B   POSTULATED univalence
//	castU  : (A B : UF) -> pathU A B -> El A -> El B                 transport; COMPUTES
//	                                                                 on ureflU AND on ua
//
// The two equalities coexist without contradiction because they are never the
// same relation: the strict Eq is a proof-irrelevant Prop and validates UIP;
// pathU A B is an ordinary U1 type whose elements are DATA — `ua not …` and
// `ureflU bool` are distinct neutrals that the outer layer can neither prove
// equal (they do not convert) nor prove distinct (no such axiom exists). The
// strict layer reasons ABOUT the fibrant layer; UIP never touches inner paths
// because inner paths are not Props.
//
// What computes and what is postulated (the honest v3 line): El decodes
// (El (fib A) is A; El (piF A B) is the real Pi), pathJ computes on preflF,
// and castU computes on ureflU and through ua (transport across an
// equivalence as a programming idiom). What does NOT compute: pathJ on
// ua-paths and every higher coherence — that is the §F cubical frontier
// (ref_docs/rune-v3-design.md), postulated here, not sold as computing.

// fibNames are the surface names the group binds, in member order.
var fibNames = [11]string{
	"UF", "El", "fib", "piF", "pathF", "preflF", "pathJ",
	"pathU", "ureflU", "ua", "castU",
}

type fibEntry struct {
	hashes [11]core.Hash
}

// AddFib registers the fibrant builtin group and binds its names, returning
// the member hashes in fibNames order. Hashes are pure functions of the
// (fixed) member types, so every session agrees on them.
func (s *Store) AddFib() [11]core.Hash {
	tys := fibTypes()

	g := newHasher()
	g.Write([]byte{defFormatVersion, 'F'})
	for _, ty := range tys {
		th := core.HashTerm(ty)
		g.Write(th[:])
	}
	var group core.Hash
	copy(group[:], g.Sum(nil))

	var hs [11]core.Hash
	for i := range hs {
		h := newHasher()
		h.Write([]byte{defFormatVersion, 'f'})
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
		s.names[fibNames[i]] = hs[i]
	}
	s.fib = &fibEntry{hashes: hs}
	return hs
}

// FibRoleOf implements core.FibInfo.
func (s *Store) FibRoleOf(h core.Hash) core.FibRole {
	if s.fib == nil {
		return core.FRoleNone
	}
	roles := [11]core.FibRole{
		core.FRoleUF, core.FRoleEl, core.FRoleFib, core.FRolePiF,
		core.FRolePathF, core.FRolePrefl, core.FRoleJ,
		core.FRolePathU, core.FRoleUrefl, core.FRoleUa, core.FRoleCastU,
	}
	for i, fh := range s.fib.hashes {
		if fh == h {
			return roles[i]
		}
	}
	return core.FRoleNone
}

// FibHashes returns the registered group's hashes (and whether it exists),
// in fibNames order.
func (s *Store) FibHashes() ([11]core.Hash, bool) {
	if s.fib == nil {
		return [11]core.Hash{}, false
	}
	return s.fib.hashes, true
}

// FibNames returns the surface names of the group members, in hash order.
func FibNames() [11]string { return fibNames }

// fibTypes builds the eleven member types as core terms, with member i as
// Placeholder(i). Scope names are display hints only and are not hashed.
func fibTypes() [11]core.Tm {
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
	uf := core.Tm(core.Ref{Hash: Placeholder(0)})
	el := func(x core.Tm) core.Tm { return app(core.Ref{Hash: Placeholder(1)}, x) }
	pathF := core.Tm(core.Ref{Hash: Placeholder(4)})
	preflF := core.Tm(core.Ref{Hash: Placeholder(5)})
	pathU := core.Tm(core.Ref{Hash: Placeholder(7)})

	// UF : U1
	ufTy := core.Tm(core.Univ{Lvl: 1})

	// El : UF -> U
	elTy := pi("A", uf, core.Univ{})

	// fib : U -> UF
	fibTy := pi("A", core.Univ{}, uf)

	// piF : (A : UF) -> (El A -> UF) -> UF
	piFTy := pi("A", uf,
		pi("B", pi("x", el(v(0)), uf),
			uf))

	// pathF : (A : UF) -> El A -> El A -> UF
	pathFTy := pi("A", uf,
		pi("x", el(v(0)),
			pi("y", el(v(1)),
				uf)))

	// preflF : (A : UF) -> (x : El A) -> El (pathF A x x)
	preflFTy := pi("A", uf,
		pi("x", el(v(0)),
			el(app(pathF, v(1), v(0), v(0)))))

	// pathJ : (A : UF) -> (x : El A)
	//         -> (P : (y : El A) -> El (pathF A x y) -> UF)
	//         -> El (P x (preflF A x))
	//         -> (y : El A) -> (p : El (pathF A x y)) -> El (P y p)
	motiveTy := pi("y", el(v(1)), // scope [x, A]: A = 1
		pi("pp", el(app(pathF, v(2), v(1), v(0))), // scope [y, x, A]
			uf))
	pathJTy := pi("A", uf,
		pi("x", el(v(0)),
			pi("P", motiveTy,
				pi("d", el(app(v(0), v(1), app(preflF, v(2), v(1)))), // scope [P, x, A]
					pi("y", el(v(3)), // scope [d, P, x, A]: A = 3
						pi("p", el(app(pathF, v(4), v(3), v(0))), // scope [y, d, P, x, A]
							el(app(v(3), v(1), v(0))))))))) // scope [p, y, d, P, x, A]: P=3, y=1, p=0

	// pathU : UF -> UF -> U1
	pathUTy := pi("A", uf, pi("B", uf, core.Univ{Lvl: 1}))

	// ureflU : (A : UF) -> pathU A A
	ureflUTy := pi("A", uf, app(pathU, v(0), v(0)))

	// ua : (A : UF) -> (B : UF) -> (f : El A -> El B) -> (g : El B -> El A)
	//      -> ((x : El A) -> Eq (El A) (g (f x)) x)
	//      -> ((y : El B) -> Eq (El B) (f (g y)) y)
	//      -> pathU A B
	uaTy := pi("A", uf,
		pi("B", uf,
			pi("f", pi("x", el(v(1)), el(v(1))), // scope [B, A]; under x: B = 1
				pi("g", pi("y", el(v(1)), el(v(3))), // scope [f, B, A]; under y: B = 1+1, A = 2+1
					pi("s", pi("x", el(v(3)), // scope [g, f, B, A]: A = 3
						core.Eq{
							Ty: el(v(4)),                   // A under x
							L:  app(v(1), app(v(2), v(0))), // g (f x)
							R:  v(0),
						}),
						pi("t", pi("y", el(v(3)), // scope [s, g, f, B, A]: B = 3
							core.Eq{
								Ty: el(v(4)),                   // B under y
								L:  app(v(3), app(v(2), v(0))), // f (g y)
								R:  v(0),
							}),
							app(pathU, v(5), v(4))))))))

	// castU : (A : UF) -> (B : UF) -> pathU A B -> El A -> El B
	castUTy := pi("A", uf,
		pi("B", uf,
			pi("p", app(pathU, v(1), v(0)),
				pi("x", el(v(2)), // A, scope [p, B, A]
					el(v(2)))))) // B, scope [x, p, B, A]

	return [11]core.Tm{ufTy, elTy, fibTy, piFTy, pathFTy, preflFTy, pathJTy,
		pathUTy, ureflUTy, uaTy, castUTy}
}
