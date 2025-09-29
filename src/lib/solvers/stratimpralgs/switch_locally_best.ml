open Stratimpralgs;;
open Paritygame;;
open Tcsset;;
open Tcsbasedata;;
open Univsolve;;
open Tcsarray;;
open Tcslist;;
open Tcsgraph;;

let array_max a less = ArrayUtils.max_elt (fun x y -> if less x y then -1 else 1) a
let list_max a less = ListUtils.max_elt (fun x y -> if less x y then -1 else 1) a


let evaluate_player1_strategy game node_compare strategy =
	let game' = pg_copy game in
	for i = 0 to pg_size game' - 1 do
		pg_set_priority game' i (1 + pg_get_priority game' i);
		pg_set_owner game' i (plr_opponent (pg_get_owner game' i))
	done;
	evaluate_strategy game' node_compare strategy

let improvement_policy_by_counterstrategy game node_compare old_strategy valu =
	let n = pg_size game in
	let tau = winning_strategies game node_compare (Array.make n (-1)) valu in
	let valutau = evaluate_player1_strategy game node_compare tau in
	let find i =
		let ordering_valu x y = node_valuation_total_ordering game node_compare valu x y >= 0 in
		let ordering_valutau x y = node_valuation_total_ordering game node_compare valutau x y >= 0 in
		let tr = pg_get_successors game i in
		let a = ns_filter (fun j -> ordering_valu j old_strategy.(i)) tr in
		ns_max a (fun x y -> ordering_valutau y x)
	in
	let strategy = Array.mapi (fun i j ->
		if j = -1 then -1
		else let k = find i in
			 if node_valuation_ordering game node_compare valu.(j) valu.(k) < 0
			 then k
			 else j
	) old_strategy in
	let fnd = ref false in
	for i = 0 to n - 1 do
		fnd := !fnd || (strategy.(i) != old_strategy.(i))
	done;
	if !fnd
	then strategy
	else improvement_policy_optimize_all_locally game node_compare old_strategy valu



let improvement_policy_optimize_best_locally game node_total_ordering old_strategy valu =
	let n = pg_size game in
	let l = ref [] in
	for i = 0 to n - 1 do
		if old_strategy.(i) > -1
		then let k = best_decision_by_valuation_ordering game node_total_ordering valu i in
			 if node_valuation_ordering game node_total_ordering valu.(old_strategy.(i)) valu.(k) < 0
			 then l := (i, k)::!l
	done;
	let strategy = Array.copy old_strategy in
	let v i a =
	(*
		let (x, y, z) = valu.(a) in
		(x, TreeSet.add i y, z)
	*)
		valu.(a)
	in
	if not (!l = []) then (
		let (i, k) = list_max !l (fun (i, a) (j, b) -> node_valuation_ordering game node_total_ordering (v i a) (v j b) < 0) in
		strategy.(i) <- k
	);
	strategy




let improvement_policy_optimize_worst_locally game node_total_ordering old_strategy valu =
	let n = pg_size game in
	let l = ref [] in
	for i = 0 to n - 1 do
		if old_strategy.(i) > -1
		then let k = best_decision_by_valuation_ordering game node_total_ordering valu i in
			 if node_valuation_ordering game node_total_ordering valu.(old_strategy.(i)) valu.(k) < 0
			 then l := (i, k)::!l
	done;
	let strategy = Array.copy old_strategy in
	let v i a =
		let (x, y, z) = valu.(a) in
		(x, TreeSet.add i y, z)
	in
	if not (!l = []) then (
		let (i, k) = list_max !l (fun (i, a) (j, b) -> node_valuation_ordering game node_total_ordering (v i a) (v j b) > 0) in
		strategy.(i) <- k
	);
	strategy






let strategy_improvement_by_counterstrategy_policy game =
	strategy_improvement_by_policy game (improvement_policy_no_user_data improvement_policy_by_counterstrategy) () false "STRIMPR_BYCOU";;


let strategy_improvement_optimize_best_locally_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_no_user_data improvement_policy_optimize_best_locally) () false "STRIMPR_LOCOPTBEST";;

let strategy_improvement_optimize_worst_locally_policy game =
	strategy_improvement_by_policy game (improvement_policy_no_user_data improvement_policy_optimize_worst_locally) () false "STRIMPR_LOCOPTWORST";;



	 
	
	 
let register _ =
    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_best_locally_policy g)
        "strimprlocbest" "sibe" "use strategy improvement w. single best local optimization";
    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_worst_locally_policy g)
        "strimprlocwrst" "siwo" "use strategy improvement w. single worst local optimization";;

