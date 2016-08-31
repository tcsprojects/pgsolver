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

val pnf : 'p 'v 'a mucalc -> 'p 'v 'a mucalc 
