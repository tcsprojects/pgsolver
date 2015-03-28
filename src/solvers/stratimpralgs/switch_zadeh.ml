open Basics;;
open Stratimpralgs;;
open Paritygame;;
open Tcsset;;
open Tcsbasedata;;
open Univsolve;;
open Tcsarray;;
open Tcslist;;
open Tcsstrings;;
open Analyze_temp;;



let improvement_policy_optimize_fair_default_tie_break game node_total_ordering _ _ valu =
	ListUtils.min_elt (fun (_, _, k) (_, _, k') ->
		node_valuation_ordering game node_total_ordering valu.(k') valu.(k)
	)


  
let improvement_policy_optimize_fair tie_break
                                     game node_total_ordering occ old_strategy valu =
    let msg_tagged_nl v = message_autotagged_newline v (fun _ -> "STRIMPR_FAIR") in
	let desc i = match (pg_get_desc game i) with Some s -> s | None -> string_of_int i in
  
    msg_tagged_nl 3 (fun _ ->
    	"Occ: " ^ 
    	ArrayUtils.formati (fun i a -> desc i ^ ":" ^
    		let tr = pg_get_tr game i in
    		ArrayUtils.formati (fun j k ->
    			desc tr.(j) ^ ":" ^ string_of_int k
    		) a
    	) occ
    	^ "\n"
    );
	
    let strategy = Array.copy old_strategy in
	let l = ref [] in
	let minvalue = ref (-1) in
	Array.iteri (fun i (_, pl, tr, _) ->
		if pl = 0 then
			Array.iteri (fun j k ->		
				if node_valuation_ordering game node_total_ordering valu.(strategy.(i)) valu.(k) < 0 then (
					if !minvalue = -1 then minvalue := occ.(i).(j);
					if !minvalue = occ.(i).(j) then l := (i,j,k)::!l
					else if !minvalue > occ.(i).(j) then (
						l := [(i,j,k)];
						minvalue := occ.(i).(j)
					)
				)
			) tr
	) game;
	msg_tagged_nl 3 (fun _ -> "Occurrence-Arena: " ^ ListUtils.format (fun (i,_,k) -> desc i ^ "->" ^ desc k) (List.rev !l) ^ "\n");
	if !l != [] then (
		let (i,j,k) = tie_break game node_total_ordering occ old_strategy valu !l in 
		strategy.(i) <- k;
		occ.(i).(j) <- occ.(i).(j) + 1
	);
	(strategy, occ)	

				
let improvement_policy_optimize_fair_sub_exp_tie_break game _ occ old_strategy valu l =
	let desc i = OptionUtils.get_some (pg_get_desc game i) in
	let find s =
		let i = ref 0 in
		while (!i < pg_size game) && (desc !i <> s) do
			incr i
		done;
		if !i < pg_size game then Some !i else None
	in
	let leadsto i j =
		let (_, path, _) = valu.(OptionUtils.get_some i) in
		TreeSet.mem (OptionUtils.get_some j) path
	in
	
	let n = ref 0 in
	while (find ("E" ^ string_of_int !n) != None) do incr n done;
	let n = !n in
	let s = ref "" in
	
	let state = Array.make n (0,0) in
	
	for i = 0 to n - 1 do
		let fndfst = ref 0 in
		if (leadsto (find ("a" ^ string_of_int i)) (find ("E" ^ string_of_int i))) &&
		   (leadsto (find ("b" ^ string_of_int i)) (find ("E" ^ string_of_int i))) then
		(
		   fndfst := 1;
           s := "1" ^ !s;
		)
        else s := "0" ^ !s;
		let fndsnd = ref 0 in
		if (i < n-1) && (leadsto (find ("v" ^ string_of_int i)) (find ("X" ^ string_of_int i))) &&
		   (leadsto (find ("w" ^ string_of_int i)) (find ("X" ^ string_of_int i))) then
		(
		   fndsnd := 1;
           s := "1" ^ !s;
		)
        else s := "0" ^ !s;
		s := "|" ^ !s;
		state.(i) <- (!fndsnd,!fndfst)
    done;
	
	let r = ref "" in
	let state' = Array.make (n+1) 0 in
	state'.(n) <- 0;
	for i = n - 1 downto 0 do
		let (s,f) = state.(i) in
		state'.(i) <- state'.(i+1)*s + (1-state'.(i+1)) * f;
		r := !r ^ string_of_int state'.(i)
	done;
	let idxmap = Array.make n 0 in
	let zeros = ref 0 in
	for i = 0 to n - 1 do
		if state'.(i) = 0 then incr zeros;
	done;
	let zeroidx = ref 0 in
	let oneidx = ref !zeros in
	for i = 0 to n - 1 do
		if state'.(i) = 1 then (
			idxmap.(i) <- !oneidx;
			incr oneidx
		)
		else (
			idxmap.(i) <- !zeroidx;
			incr zeroidx
		)
	done;
	
	let compare_nodes k (soulab0, souidx0) (tarlab0, taridx0) (oldlab0, oldidx0) (soulab1, souidx1) (tarlab1, taridx1) (oldlab1, oldidx1)
					  state idxmap strat =
		let nn = 2 * k in
		let f i = 2 * idxmap.(i) + state.(i+1) in
		let g i = 2 * idxmap.(i) + 1 - state.(i+1) in
		let h c d i = if fst (strat (c ^ string_of_int i)) = d then 1 else 0 in
		let mp = function
		|	(('m',_),_,_)       (* c *)            		    			-> -2
(*		|	(('p',_),_,_)                       		    			-> -1
		|	(('q',_),_,_)                       		    			-> -1 *)
		|	(('d',i),_,_)       (* g *)           		    			-> 5 + 2 * nn         
  	|	(('a',_),_,('E',_)) (* e *) 		    			        -> -3
		|	(('w',_),_,('X',_)) (* e *)  		 				        -> -3
		|	(('b',_),_,('E',_)) (* d *)  		    			        -> -3
		|	(('v',_),_,('X',_)) (* d *)  		    			        -> -3
  	|	(('o',i),_,(_,_)) (* e *) 		    			        -> if (h "a" 'E' i) = 1 then -4 else 1
		|	(('p',i),_,(_,_)) (* e *)  		 				        -> if (h "b" 'E' i) = 1 then -4 else 1
		|	(('q',i),_,(_,_)) (* d *)  		    			        -> if (h "w" 'X' i) = 1 then -4 else 1
		|	(('r',i),_,(_,_)) (* d *)  		    			        -> if (h "v" 'X' i) = 1 then -4 else 1
		|	(('a',i),('E',_),_) (* e *)                                 -> 2 + (h "b" 'E' i) * nn + f i
		|	(('w',i),('X',_),_) (* e *)                                 -> 2 + (h "v" 'X' i) * nn + g i
		|	(('b',i),('E',_),_) (* d *)                                 -> 2 + (h "a" 'E' i) * nn + f i
		|	(('v',i),('X',_),_) (* d *)                                 -> 2 + (h "w" 'X' i) * nn + g i
		|	(('a',_),_,('Y',_)) (* e *) 		    			        -> -3
		|	(('w',_),_,('Y',_)) (* e *)  		 				        -> -3
		|	(('b',_),_,('Y',_)) (* d *)  		    			        -> -3
		|	(('v',_),_,('Y',_)) (* d *)  		    			        -> -3
  	|	(('a',_),_,_)       (* e *) 		 		      		    -> 1
		|	(('w',_),_,_)       (* e *)  		 		      			-> 1
		|	(('b',_),_,_)       (* d *)  		        				-> 1
		|	(('v',_),_,_)       (* d *)  		    				    -> 1
		|	(('l',_),_,_)           		    						-> 4 + 2 * nn
		|	(('g',_),_,_)       (* s *)		    					    -> 4 + 2 * nn
		|	(('s',_),_,_)       (* s *)		    					    -> 4 + 2 * nn
		|	(('f',_),_,_)       (* b *) 					            -> 3 + 2 * nn
		|	(('c',_),_,_)       (* h *) 					            -> 4 + 2 * nn
		|	(('u',_),_,_)       (* h *) 					            -> 4 + 2 * nn
		|	((x,_),_,_) -> failwith ("imp:" ^ Char.escaped x)
		in	
		compare (mp ((soulab0, souidx0), (tarlab0, taridx0), (oldlab0, oldidx0))) (mp ((soulab1, souidx1), (tarlab1, taridx1), (oldlab1, oldidx1)))
	in
	let f i =
		match pg_get_desc game i with
			None -> ('!', 0)
		|	Some s -> 
				if String.length s = 1
				then (String.get s 0, 0)
				else if String.get s 1 = '('
				then (String.get s 0, 0)
				else (String.get s 0, int_of_string (StringUtils.rest_string s 1))
	in
	let (i,j,k) = ListUtils.min_elt (fun (i0,j0,k0) (i1,j1,k1) ->
		compare_nodes (pg_size game) (f i0) (f k0) (f old_strategy.(i0)) (f i1) (f k1) (f old_strategy.(i1)) state' idxmap
		   (fun s -> f old_strategy.(OptionUtils.get_some (find s)))
	) l in
	switch_zadeh_exp_tie_break_callback n game old_strategy valu i k !r !s;
	(i,j,k)


		   

let strategy_improvement_optimize_fair_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	                     (improvement_policy_optimize_fair improvement_policy_optimize_fair_default_tie_break) (
		Array.map (fun (_, pl, tr, _) ->
			if pl = 1 then [||]
			else Array.make (Array.length tr) 0
		) game
	) false "STRIMPR_FAIR";;

let strategy_improvement_optimize_fair_sub_exp_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position 
                         (improvement_policy_optimize_fair improvement_policy_optimize_fair_sub_exp_tie_break) (
		Array.map (fun (_, pl, tr, _) ->
			if pl = 1 then [||]
			else Array.make (Array.length tr) 0
		) game
	) false "STRIMPR_FAIRSE";;






register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_fair_policy g)
	"switchfair" "sf" "Zadeh's fair policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_fair_sub_exp_policy g)
	"switchfairse" "sfse" "Zadeh's fair policy iteration with lower bound breaking ties";;

