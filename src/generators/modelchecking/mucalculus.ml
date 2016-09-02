open Tcsset;;
open Tcsarray;;
open Paritygame;;
  
type ('p,'a) mucalc = Prop of 'p
		    | Var of string
		    | Neg of ('p,'a) mucalc
		    | Imp of ('p,'a) mucalc * ('p,'a) mucalc
		    | Conj of ('p,'a) mucalc list 
		    | Disj of ('p,'a) mucalc list
		    | Diam of 'a option * (('p,'a) mucalc)
		    | Box  of 'a option * (('p,'a) mucalc)
		    | Mu of string * (('p,'a) mucalc)
		    | Nu of string * (('p,'a) mucalc)

let rec show_formula = function Prop _ -> "prop .."
			  | Var x -> "var " ^ x
			  | Neg f -> "-" ^ show_formula f ^ ""
			  | Imp(f,g) -> "(" ^ show_formula f ^ "->" ^ show_formula g ^ ")"
			  | Conj(fs) -> "(" ^ String.concat "/\\" (List.map show_formula fs) ^ ")"
			  | Disj(fs) -> "(" ^ String.concat "\\/" (List.map show_formula fs) ^ ")"
			  | Diam(None,f) -> "<>" ^ show_formula f
			  | Diam(Some _,f) -> "<?>" ^ show_formula f
			  | Box(None,f) -> "[]" ^ show_formula f
			  | Box(Some _,f) -> "[?]" ^ show_formula f
			  | Mu(x,f) -> "mu " ^ x ^ "." ^ show_formula f
			  | Nu(x,f) -> "nu " ^ x ^ "." ^ show_formula f
								
let subformulas phi =
  let rec subfs psi = TreeSet.add psi (match psi with Imp(f,g)   -> TreeSet.union (subfs f) (subfs g)
						    | Conj(fs)   -> List.fold_left TreeSet.union TreeSet.empty_def (List.map subfs fs) 
						    | Disj(fs)   -> List.fold_left TreeSet.union TreeSet.empty_def (List.map subfs fs)
						    | Diam(_,f)  -> subfs f
						    | Box(_,f)   -> subfs f
						    | Mu(_,f)    -> subfs f
						    | Nu(_,f)    -> subfs f
						    | Neg(f)     -> subfs f
						    | _          -> TreeSet.empty_def) 
  in
  subfs phi

let rec has_free_occ x = function Var y -> x=y
				| Conj(fs) -> List.exists (has_free_occ x) fs
				| Disj(fs) -> List.exists (has_free_occ x) fs 
				| Diam(_,f) -> has_free_occ x f 
				| Box(_,f) -> has_free_occ x f 
				| Mu(y,f) -> x != y && (has_free_occ x f) 
				| Nu(y,f) -> x != y && (has_free_occ x f)
				| _ -> false


let normalise phi = 
  let substitute_free_occurrences v f g =
    let rec subst h = match h with Var(u) -> if u=v then g else h
				 | Neg(h') -> Neg(subst h')
				 | Conj(hs) -> Conj(List.map subst hs)
				 | Disj(hs) -> Disj(List.map subst hs)
				 | Imp(h',h'') -> Imp(subst h', subst h'')
				 | Diam(a,h') -> Diam(a, subst h')
				 | Box(a,h') -> Box(a, subst h')
				 | Mu(u,h') -> if v=u then h else Mu(u, subst h')
				 | Nu(u,h') -> if v=u then h else Nu(u, subst h')
				 | _ -> h
    in
    subst f
  in
  let rec pnf = function Imp(f,g) -> Disj[pnf (Neg f); pnf g]
		       | Conj(fs) -> Conj(List.map pnf fs)
		       | Disj(fs) -> Disj(List.map pnf fs)
		       | Diam(a,f) -> Diam(a, pnf f)
		       | Box(a,f) -> Box(a, pnf f)
		       | Mu(v,f) -> Mu(v, pnf f)
		       | Nu(v,f) -> Nu(v, pnf f)
		       | Neg(Neg(f)) -> pnf f
		       | Neg(Imp(f,g)) -> Conj[pnf f; pnf (Neg g)]
		       | Neg(Conj(fs)) -> Disj(List.map (fun f -> pnf (Neg(f))) fs) 
		       | Neg(Disj(fs)) -> Conj(List.map (fun f -> pnf (Neg(f))) fs)
		       | Neg(Diam(a,f)) -> Box(a,pnf (Neg f))
		       | Neg(Box(a,f)) -> Diam(a,pnf (Neg f))
		       | Neg(Mu(v,f)) -> Nu(v, pnf (Neg (substitute_free_occurrences v f (Neg (Var v)))))
		       | Neg(Nu(v,f)) -> Mu(v, pnf (Neg (substitute_free_occurrences v f (Neg (Var v)))))
		       | f -> f
  in
  let delete_obsolete_fixpoints phi =
    let rec del = function Mu(x,f) -> if has_free_occ x f then Mu(x, del f) else del f
			 | Nu(x,f) -> if has_free_occ x f then Nu(x, del f) else del f
			 | Conj(fs) -> Conj(List.map del fs)
			 | Disj(fs) -> Disj(List.map del fs)
			 | Diam(a,f) -> Diam(a, del f)
			 | Box(a,f)  -> Box(a, del f)
			 | f -> f
    in
    del phi
  in
  let rename_variables phi =
    let rec renvars used = function Conj(fs) -> Conj(List.map (renvars used) fs)
				  | Disj(fs) -> Disj(List.map (renvars used) fs)
				  | Diam(a,f) -> Diam(a, renvars used f)
				  | Box(a,f)  -> Box(a, renvars used f)
				  | Mu(x,f) -> if TreeSet.mem x used then
						 let nx = String.length x in 
						 let howmany = TreeSet.fold (fun z i -> let i' = (String.length z) - nx in
											max i' i)
									    (TreeSet.filter (fun z -> Str.string_match (Str.regexp ("^" ^ x ^ "'+")) z 0) used)
									    0
						 in
						 let y = x ^ String.make (howmany+1) '\'' in
						 Mu(y, renvars (TreeSet.add y used) (substitute_free_occurrences x f (Var y)))
					       else
						 Mu(x, renvars (TreeSet.add x used) f)
				  | Nu(x,f) -> if TreeSet.mem x used then
						 let nx = String.length x in 
						 let howmany = TreeSet.fold (fun z i -> let i' = (String.length z) - nx in
											max i' i)
									    (TreeSet.filter (fun z -> Str.string_match (Str.regexp ("^" ^ x ^ "'+")) z 0) used)
									    0
						 in
						 let y = x ^ String.make (howmany+1) '\'' in
						 Nu(y, renvars (TreeSet.add y used) (substitute_free_occurrences x f (Var y)))
					       else
						 Nu(x, renvars (TreeSet.add x used) f)
				  | f -> f
    in
    renvars TreeSet.empty_def phi
  in

  let simplify phi = phi (* for further ideas to simplify formulas *)
  in
    
  rename_variables (simplify (delete_obsolete_fixpoints (pnf phi)))

(* TODO: variable renaming is not safe yet
 * 
 * Example:    mu X. <>X || (mu X'. mu X. <>X || <>X') || (mu X. <>X) 
 *          produces
 *             mu X. <>X || (mu X'. mu X'' <>X'' || <>X') || (mu X'.<> X')
 *
 * solution: need to check that new names are not used anywhere else in the formula
 * problem:  should identify multiple occurrences of the same subformula and not give them different names *)

		   
module VarPrioMap = Map.Make(String);;

type ('p,'a) formulaType = PLIT of 'p
			 | NLIT of 'p
			 | BOOL of player * int list
			 | MOD of player * 'a * int
			 | FP of priority * int

type ('p,'a) mes = ((('p,'a) formulaType) array) * int

let toMES phi =
  let phi = normalise phi in
  
  let admap =
    let rec containing_free x = function Mu(y,f) -> let vs = containing_free x f in
						    if has_free_occ x f then TreeSet.add y vs else vs
				       | Nu(y,f) -> let vs = containing_free x f in
						    if has_free_occ x f then TreeSet.add y vs else vs
				       | Conj(fs) -> List.fold_left TreeSet.union TreeSet.empty_def (List.map (containing_free x) fs) 
				       | Disj(fs) -> List.fold_left TreeSet.union TreeSet.empty_def (List.map (containing_free x) fs)
				       | Diam(_,f) -> containing_free x f
				       | Box(_,f)  -> containing_free x f
				       | _ -> TreeSet.empty_def
    in
    let rec ad = function Conj(fs) -> List.fold_left (VarPrioMap.union (fun x _ _ -> failwith ("Mucalculus.toMES.admap: variable " ^ x ^ " not unique!")))
						     VarPrioMap.empty
						     (List.map ad fs) 
			| Disj(fs) -> List.fold_left (VarPrioMap.union (fun x _ _ -> failwith ("Mucalculus.toMES.admap: variable " ^ x ^ " not unique!")))
						     VarPrioMap.empty
						     (List.map ad fs)
			| Diam(_,f) -> ad f 
			| Box(_,f)  -> ad f
			| Prop(_)   -> VarPrioMap.empty
			| Neg(Prop(_)) -> VarPrioMap.empty
			| Mu(x,f) -> let priof = ad f in
				     let lowervars = TreeSet.elements (containing_free x f) in
				     let p' = List.fold_left max 0 (List.map (fun v -> VarPrioMap.find v priof) lowervars) in
				     let p = if odd p' then p' else p'+1 in
				     VarPrioMap.add x p priof
			| Nu(x,f) -> let priof = ad f in
				     let lowervars = TreeSet.elements (containing_free x f) in
				     let p' = List.fold_left max 0 (List.map (fun v -> VarPrioMap.find v priof) lowervars) in
				     let p = if even p' then p' else p'+1 in
				     VarPrioMap.add x p priof
			| Var(x) -> VarPrioMap.empty
			| f -> failwith ("Mucalculus.toMES.altdepth: formula " ^ show_formula f ^ " not in closed PNF!")
    in
    ad phi
  in
  let altdepth x = try
                     VarPrioMap.find x admap
                   with
		     Not_found -> failwith "Mucalculus.toMES.altdepth: lookup for undefined variable in alternation-depth table"
  in

  let fpdef x =
    let rec makemap = function Conj(fs) -> (fun x -> List.fold_left (fun b f -> if b = None then makemap f x else b) None fs) 
			     | Disj(fs) -> (fun x -> List.fold_left (fun b f -> if b = None then makemap f x else b) None fs)
			     | Diam(_,f) -> makemap f
			     | Box(_,f) -> makemap f
			     | Mu(y,f) -> (fun x -> if x=y then Some f else makemap f x)
			     | Nu(y,f) -> (fun x -> if x=y then Some f else makemap f x)
			     | _ -> (fun _ -> None)
    in
    match makemap phi x with
      Some f -> f
    | None -> raise Not_found
  in
  
  let relevant_subfs = Array.of_list (TreeSet.elements (TreeSet.filter (function Mu(_,_) -> false
									       | Nu(_,_) -> false
									       | _       -> true)
								       (subformulas phi)))
  in
  let n = Array.length relevant_subfs in
  let mes = Array.make n (BOOL(plr_Odd,[])) in

  let rec find f =
    try
      ArrayUtils.index_of relevant_subfs f
    with
      Not_found -> (match f with
		     Mu(x,g) -> find g
		   | Nu(x,g) -> find g
		   | _ -> failwith "Mucalculus.toMES.find: looking for truly absent formula!")
  in 
      
  for i=0 to n-1 do
    mes.(i) <- (match relevant_subfs.(i) with
		 Prop(p)      -> PLIT(p)
	       | Neg(Prop(p)) -> NLIT(p)
	       | Conj(fs)     -> BOOL(plr_Odd, List.map find fs)
	       | Disj(fs)     -> BOOL(plr_Even, List.map find fs)
	       | Diam(a,f)    -> MOD(plr_Even, a, find f)
	       | Box(a,f)     -> MOD(plr_Odd, a, find f)
	       | Var(x)       -> FP(altdepth x, find (fpdef x))
	       | _ -> failwith "Mucalculus.toMES: trying to squeeze unexpected formula type into MES!")
  done;

  (mes, find phi)



module type VerificationProblem =
  sig
    type state
    type proposition
    type action

    val actions : unit -> action list				

    val initstate : unit -> state
    val labels : state -> proposition -> bool 
    val successors : state -> action -> state list 

    val show_state : state -> string
    val show_proposition : proposition -> string
    val show_action : action -> string
				  
    val property : unit -> (proposition,action) mucalc
  end;;

module Make = functor (T: VerificationProblem) ->
  Build(struct
	 type gamenode = T.state * int
				  
	 let compare = compare

	 let eqs = ref (Array.make 0 (BOOL(plr_Even,[])))
	 let start = ref 0
	 let initialized = ref false
			       
	 let initialize _ = 
	   let (eqs',start') = toMES (T.property ()) in
	   eqs := eqs';
	   start := start';
	   initialized := true 

	 let initnodes _ = if not !initialized then initialize ();
			   [ (T.initstate (), !start) ]
			  
	 let owner (_,i) = if not !initialized then initialize ();
			   match !eqs.(i) with
			        BOOL(pl,_)  -> pl
			      | MOD(pl,_,_) -> pl
			      | _           -> plr_Even
	                        
	 let priority (s,i) = if not !initialized then initialize ();
			      match !eqs.(i) with
	                           PLIT(p)     -> if T.labels s p then 0 else 1
	                         | NLIT(p)     -> if T.labels s p then 1 else 0
				 | FP(pr,_)    -> pr
				 | BOOL(pl,[]) -> if pl=plr_Odd then 0 else 1
				 | MOD(pl,a,_) -> let ts = match a with
						      Some b -> T.successors s b
						    | None   -> List.concat (List.map (T.successors s) (T.actions ()))
						  in
						  (match ts with
						     [] -> if pl=plr_Odd then 0 else 1
						   | _ -> 0)
				 | _           -> 0
						 

	 let successors (s,i) = if not !initialized then initialize ();
				match !eqs.(i) with
	                              BOOL(_,[]) -> [ (s,i) ] 
	                            | BOOL(_,js) -> List.map (fun j -> (s,j)) js 
	                            | MOD(_,a,j) -> let ts = match a with
							Some b -> T.successors s b
						      | None -> List.concat (List.map (T.successors s) (T.actions ()))  
						    in
						    if ts = [] then
						      [ (s,i) ]
						    else
						      List.map (fun t -> (t,j)) ts
				    | FP(_,j)    -> [ (s,j) ]
				    | _          -> [ (s,i) ]

	 let show_node (s,i) = if not !initialized then initialize ();
			       Some(T.show_state s ^ " |= " ^ (match !eqs.(i) with
								 BOOL(pl,[]) -> if pl=plr_Even then "ff" else "tt"
 							       | BOOL(pl,js) -> let op = if pl=plr_Even then "\\/" else "/\\" in
										String.concat op (List.map string_of_int js) 
							       | MOD(pl,a,j) -> let (op,cl) = if pl=plr_Even then ("<",">") else ("[","]") in
										let ac = match a with Some b -> T.show_action b
												    | None -> ""
										in
										op ^ ac ^ cl ^ string_of_int j
							       | FP(pr,j) -> "fp(" ^ string_of_int pr ^ ")." ^ string_of_int j
							       | PLIT(p) -> T.show_proposition p
							       | NLIT(p) -> "-" ^ T.show_proposition p))

       end);;
  
