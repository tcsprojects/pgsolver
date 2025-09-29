open Stratimpralgs;;
open Paritygame;;
open Tcsset;;
open Tcsbasedata;;
open Univsolve;;
open Tcsarray;;
open Tcslist;;

let improvement_policy_optimize_fair_default_tie_break game node_total_ordering _ _ valu =
	ListUtils.min_elt (fun (_, _, k) (_, _, k') ->
		node_valuation_ordering game node_total_ordering valu.(k') valu.(k)
	)

let improvement_policy_optimize_least_basic_iterations tie_break game node_total_ordering occ old_strategy valu =
	Array.iteri (fun i j ->
		     let pl = pg_get_owner game i in
		     let tr = pg_get_successors game i in
		     if pl = plr_Even then Array.iteri (fun k l ->
							if l = j then occ.(i).(k) <- occ.(i).(k) + 1
						       ) (Array.of_list (ns_nodes tr))
		    ) old_strategy;
	let strategy = Array.copy old_strategy in
	let l = ref [] in
	let minvalue = ref (-1) in
	pg_iterate (fun i (_, pl, tr, _, _) ->
		if pl = plr_Even then
			Array.iteri (fun j k ->		
				if node_valuation_ordering game node_total_ordering valu.(strategy.(i)) valu.(k) < 0 then (
					if !minvalue = -1 then minvalue := occ.(i).(j);
					if !minvalue = occ.(i).(j) then l := (i,j,k)::!l
					else if !minvalue > occ.(i).(j) then (
						l := [(i,j,k)];
						minvalue := occ.(i).(j)
					)
				)
			) (Array.of_list (ns_nodes tr))
	) game;
	if !l != [] then (
		let (i,j,k) = tie_break game node_total_ordering occ old_strategy valu !l in 
		strategy.(i) <- k
	);
	(strategy, occ)	
	
(* Select the improving edge that left the strategy least-recently. *)		
let improvement_policy_optimize_least_recently_basic tie_break game node_total_ordering occ old_strategy valu =
	Array.iteri (fun i ->
		Array.iteri (fun j k ->
			occ.(i).(j) <- if old_strategy.(i) = k then 0 else occ.(i).(j) + 1
		)
	) occ;
    let strategy = Array.copy old_strategy in
	let l = ref [] in
	let minvalue = ref (-1) in
	pg_iterate (fun i (_, pl, tr, _, _) ->
		if pl = plr_Even then
			Array.iteri (fun j k ->		
				if node_valuation_ordering game node_total_ordering valu.(strategy.(i)) valu.(k) < 0 then (
					if !minvalue = -1 then minvalue := occ.(i).(j);
					if !minvalue = occ.(i).(j) then l := (i,j,k)::!l
					else if !minvalue < occ.(i).(j) then (
						l := [(i,j,k)];
						minvalue := occ.(i).(j)
					)
				)
			) (Array.of_list (ns_nodes tr))
	) game;
	if !l != [] then (
		let (i,j,k) = tie_break game node_total_ordering occ old_strategy valu !l in 
		strategy.(i) <- k
	);
	(strategy, occ)	
		
(* Select the improving edge that entered the strategy least-recently thus far.  *)	
let improvement_policy_optimize_least_recently_entered tie_break game node_total_ordering occ old_strategy valu =
    let strategy = Array.copy old_strategy in
	let l = ref [] in
	let minvalue = ref (-1) in
	let maxvalue = ref (-1) in
	pg_iterate (fun i (_, pl, tr, _, _) ->
		if pl = plr_Even then
			Array.iteri (fun j k ->		
				if node_valuation_ordering game node_total_ordering valu.(strategy.(i)) valu.(k) < 0 then (
					if !minvalue = -1 then minvalue := occ.(i).(j);
					maxvalue := max !maxvalue occ.(i).(j);
					if !minvalue = occ.(i).(j) then l := (i,j,k)::!l
					else if !minvalue > occ.(i).(j) then (
						l := [(i,j,k)];
						minvalue := occ.(i).(j)
					)
				)
			) (Array.of_list (ns_nodes tr))
	) game;
	if !l != [] then (
		let (i,j,k) = tie_break game node_total_ordering occ old_strategy valu !l in 
		strategy.(i) <- k;
		occ.(i).(j) <- !maxvalue + 1
	);
	(strategy, occ)	
		
	
let strategy_improvement_optimize_least_basic_iterations_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	                     (improvement_policy_optimize_least_basic_iterations improvement_policy_optimize_fair_default_tie_break) (
		pg_map2 (fun _ (_, pl, tr, _, _) ->
			if pl = plr_Odd then [||]
			else Array.make (ns_size tr) 0
		) game
	) false "STRIMPR_LBI";;

let strategy_improvement_optimize_least_recently_basic_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	                     (improvement_policy_optimize_least_recently_basic improvement_policy_optimize_fair_default_tie_break) (
		pg_map2 (fun _ (_, pl, tr, _, _) ->
			if pl = plr_Odd then [||]
			else Array.make (ns_size tr) 0
		) game
	) false "STRIMPR_LRB";;

let strategy_improvement_optimize_least_recently_entered_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	                     (improvement_policy_optimize_least_recently_entered improvement_policy_optimize_fair_default_tie_break) (
		pg_map2 (fun _ (_, pl, tr, _, _) ->
			if pl = plr_Odd then [||]
			else Array.make (ns_size tr) 0
		) game
	) false "STRIMPR_LRE";;


let register _ =
    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_least_basic_iterations_policy g)
        "switchlbi" "slbi" "Least basic iterations policy iteration";

    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_least_recently_basic_policy g)
        "switchlrb" "slrb" "Least recently basic policy iteration";

    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_least_recently_entered_policy g)
        "switchlre" "slre" "Least recently entered policy iteration";;

