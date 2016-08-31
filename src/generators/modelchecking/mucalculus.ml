type 'p 'v 'a mucalc = Prop of 'p
		     | Var of 'v
		     | Neg of 'p 'v 'a mucalc
		     | And of 'p 'v 'a mucalc * 'p 'v 'a mucalc
		     | Or of 'p 'v 'a mucalc * 'p 'v 'a mucalc
		     | Imp of 'p 'v 'a mucalc * 'p 'v 'a mucalc
		     | Diam of 'a * ('p 'v 'a mucalc)
		     | Box of 'a * ('p 'v 'a mucalc)
		     | Mu of 'v * ('p 'v 'a mucalc)
		     | Nu of 'v * ('p 'v 'a mucalc)

let substitute_free_occurrences v f g =
  let rec subst h = match h with Var(u) -> if u=v then g else h
			       | Neg(h') -> Neg(subst h')
			       | And(h',h'') -> And(subst h', h'')
			       | Or(h',h'') -> Or(subst h',h'')
			       | Imp(h',h'') -> Imp(subst h', subst h'')
			       | Diam(a,h') -> Diam(a, subst h')
			       | Box(a,h') -> Box(a, subst h')
			       | Mu(u,h') -> if v=u then h else Mu(u, subst h')
			       | Nu(u,h') -> if v=u then h else Nu(u, subst h')
  in
  subst f
					 
let rec pnf = function Imp(f,g) -> Or(pnf (Neg f), pnf g)
		     | And(f,g) -> And(pnf f, pnf g)
		     | Or(f,g) -> Or(pnf f, pnf g)
		     | Diam(a,f) -> Diam(a, pnf f)
		     | Box(a,f) -> Box(a, pnf f)
		     | Mu(v,f) -> Mu(v, pnf f)
		     | Nu(v,f) -> Nu(v, pnf f)
		     | Neg(Neg(f)) -> pnf f
		     | Neg(And(f,g)) -> Or(pnf (Neg f), pnf (Neg g))
		     | Neg(Or(f,g)) -> And(pnf (Neg f), pnf (Neg g))
		     | Neg(Imp(f,g)) -> And(pnf f, pnf (Neg g))
		     | Neg(Diam(a,f)) -> Box(a,pnf (Neg f))
		     | Neg(Box(a,f)) -> Diam(a,pnf (Neg f))
		     | Neg(Mu(v,f)) -> Nu(v,pnf (Neg (substitute_free_occurrences v f (Neg (Var v)))))
		     | Neg(Nu(v,f)) -> Mu(v,pnf (Neg (substitute_free_occurrences v f (Neg (Var v)))))
		     | f -> f
			      
