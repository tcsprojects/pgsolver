open Stratimpralgs;;
open Paritygame;;
open Tcsset;;
open Tcsbasedata;;
open Univsolve;;
open Tcsarray;;
open Tcslist;;
open Tcsgraph;;


let default_snare_sel _ _ _ _ snares =
	match snares with
		[] -> None
	|	x::_ -> Some x
    
let rec improvement_policy_snare_based sub_policy snare_sel game node_total_ordering (snares, enforce) old_strategy valu =
	let cmp i j = node_valuation_ordering game node_total_ordering valu.(i) valu.(j) in
	let valx i = let (_, x, _) = valu.(i) in x in
	match enforce with
		Some (sna,str,esc) -> (
			let fnd = ref None in
			TreeSet.iter (fun (i,j) ->
				if cmp j old_strategy.(i) > 0 then fnd := Some (i,j)
			) str;
			match !fnd with
				Some (i,j) -> (
					let new_strat = Array.copy old_strategy in
					new_strat.(i) <- j;
					(new_strat, (snares, enforce))
				)
			|	None -> improvement_policy_snare_based sub_policy snare_sel game node_total_ordering (snares, None) old_strategy valu
		)
	|	None -> (
			(* Update set of snares *)
			let snares = ref snares in
			pg_iterate (fun i (_, pl, tr, _, _) ->
				if pl = plr_Even then (
					ns_iter (fun j ->
						if (cmp j old_strategy.(i) > 0) && (TreeSet.mem i (valx j)) then (
							let sna = ref TreeSet.empty_def in
							let todo = ref (TreeSet.singleton_def j) in
							while not (TreeSet.is_empty !todo) do
								let k = TreeSet.min_elt !todo in
								todo := TreeSet.remove k !todo;
								if (not (TreeSet.mem k !sna)) && (TreeSet.mem i (valx k)) then (
									sna := TreeSet.add k !sna;
									let pl = pg_get_owner game k in
									let tr = pg_get_successors game k in 
									if pl = plr_Even then todo := TreeSet.add old_strategy.(k) !todo
									else ns_iter (fun j -> todo := TreeSet.add j !todo) tr
								)
							done;
							let esc = ref TreeSet.empty_def in
							let str = ref TreeSet.empty_def in
							TreeSet.iter (fun k ->
								      let pl = pg_get_owner game k in
								      let tr = pg_get_successors game k in 
								      if i = k then str := TreeSet.add (i, j) !str
								      else if pl = plr_Even then str := TreeSet.add (k,old_strategy.(k)) !str
								      else ns_iter (fun j -> if not (TreeSet.mem j !sna) then esc := TreeSet.add (k,j) !esc) tr
								     ) !sna;
							snares := (!sna,!str,!esc)::!snares
						)
					) tr
				)
			) game;
			(* Filter set of applicable snares *)
			let appsnares = List.filter (fun (_,_,esc) ->
				TreeSet.for_all (fun (i,j) ->
					not (TreeSet.equal (valx i) (TreeSet.add i (valx j)))
				) esc
			) !snares in
			match (snare_sel game node_total_ordering old_strategy valu appsnares) with
				Some snare -> (
					improvement_policy_snare_based sub_policy snare_sel game node_total_ordering (!snares, Some snare) old_strategy valu
				)
			|	None -> (sub_policy game node_total_ordering old_strategy valu, (!snares, None))
		)
	

	
let strategy_improvement_snare_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_snare_based improvement_policy_optimize_all_locally default_snare_sel) ([], None) true "STRIMPR_SNARE";;




let register _ =
    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_snare_policy g)
        "snarememo" "sm" "Snare memorization policy iteration";;

