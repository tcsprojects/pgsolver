open Stratimpralgs;;
open Paritygame;;
open Tcsset;;
open Tcsbasedata;;
open Univsolve;;
open Tcsarray;;
open Tcslist;;
open Tcsgraph;;

let list_max a less = ListUtils.max_elt (fun x y -> if less x y then -1 else 1) a


let improvement_policy_optimize_all_globally game' node_total_ordering old_strategy valu =
	let game = pg_copy game' in
	let n = pg_size game in
	let valu_ord = node_valuation_ordering game' node_total_ordering in
	pg_iterate (fun i (pr, pl, tr, _, _) ->
		if pl = plr_Even
		then
		  ns_iter (fun w -> pg_del_edge game i w) (ns_filter (fun j -> not (valu_ord valu.(j) valu.(old_strategy.(i)) >= 0)) tr) 
	) game;

	let strategy = Array.copy old_strategy in
	let valu_ord = node_valuation_ordering game node_total_ordering in
	let graph = paritygame_to_dynamic_paritygame game in

	let valu2 = Array.copy valu in
	let valued = Array.make n false in

	let graph' = DynamicGraph.copy_graph graph in
	let check = ref (TreeSet.empty compare) in
	for v = 0 to n - 1 do
		let edges = DynamicGraph.get_node_succ v graph in
		if pg_get_owner game v = plr_Odd then (
			let reference = valu.(best_decision_by_valuation_ordering game node_total_ordering valu v) in
			TreeSet.iter (fun u ->
				if valu_ord valu.(u) reference > 0 then DynamicGraph.del_edge v u graph'
			) edges;
			check := TreeSet.add v !check;
		)
		else if (TreeSet.cardinal edges > 1) then DynamicGraph.del_node v graph'
		else check := TreeSet.add v !check
	done;
	while (not (TreeSet.is_empty !check)) do
		let v = TreeSet.min_elt !check in
		check := TreeSet.remove v !check;
		if TreeSet.cardinal (DynamicGraph.get_node_succ v graph') = 0 then (
			check := TreeSet.union !check (DynamicGraph.get_node_pred v graph');
			DynamicGraph.del_node v graph'
		)
	done;
	DynamicGraph.iter (fun v _ -> valued.(v) <- true) graph';

	let todo = ref (TreeSet.empty compare) in
	let rest = ref (TreeSet.empty compare) in

	for v = 0 to n - 1 do
		if valued.(v)
		then todo := TreeSet.union !todo (DynamicGraph.get_node_pred v graph)
		else rest := TreeSet.add v !rest
	done;

	let compute_update' v u = let (a, b, c) = valu2.(u) in (a, TreeSet.add v b, c + 1) in
	let compute_update v =
		let p = TreeSet.filter (fun u -> valued.(u)) (DynamicGraph.get_node_succ v graph) in
		let pl = pg_get_owner game v in
		let cmp x y = if pl = plr_Even then valu_ord valu2.(x) valu2.(y) else valu_ord valu2.(y) valu2.(x) in
		TreeSet.fold (fun u mx -> if cmp u mx > 0 then u else mx) p (TreeSet.min_elt p)
	in

	while (not (TreeSet.is_empty !rest)) do
		while (not (TreeSet.is_empty !todo)) do
			let v = TreeSet.min_elt !todo in
			todo := TreeSet.remove v !todo;
			let succs = DynamicGraph.get_node_succ v graph in
			if not valued.(v) then (
				if TreeSet.for_all (fun u -> valued.(u)) succs then (
					valued.(v) <- true;
					let u = compute_update v in
					valu2.(v) <- compute_update' v u;
					if pg_get_owner game v = plr_Even then strategy.(v) <- u
				)
				else if (pg_get_owner game v = plr_Odd) &&
				        (TreeSet.exists (fun u -> valued.(u) &&
				                                   (valu_ord (compute_update' v u) valu2.(v) = 0)) succs)
				then (valued.(v) <- true; failwith "impossible");
				if valued.(v) then (
					todo := TreeSet.union !todo (DynamicGraph.get_node_pred v graph);
					rest := TreeSet.remove v !rest
				)
			)
		done;
		if not (TreeSet.is_empty !rest) then (
            let temp = TreeSet.filter (fun v -> (pg_get_owner game v = plr_Odd) && (TreeSet.exists (fun u -> valued.(u)) (DynamicGraph.get_node_succ v graph))) !rest in
            if TreeSet.is_empty temp
            then failwith "crap"
            else let l = TreeSet.fold (fun v l -> let u = compute_update v in (v, compute_update' v u)::l) temp [] in
                 let (v, va) = list_max l (fun (v, a) (w, b) -> valu_ord a b > 0) in (
                    todo := TreeSet.union !todo (DynamicGraph.get_node_pred v graph);
                    rest := TreeSet.remove v !rest;
                    valu2.(v) <- va;
                    valued.(v) <- true
                 )
        )
	done;

	strategy
	
	
	

let strategy_improvement_optimize_all_globally_policy game =
	strategy_improvement_by_policy game (improvement_policy_no_user_data improvement_policy_optimize_all_globally) () false "STRIMPR_GLOOPT";;


let register _ =
    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_all_globally_policy g)
        "switchbest" "sb" "best combination of switches policy iteration";;

