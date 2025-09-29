open Basics;;
open Stratimpralgs;;
open Paritygame;;
open Tcsset;;
open Tcsbasedata;;
open Univsolve;;
open Tcslist;;
open Tcsarray;;

let list_upfront l i =
	let rec tile f t =
        let j = List.hd t in
        if j = i then (f, t) else tile (j::f) (List.tl t)
    in
    let (f, t) = tile [] l in
    t @ (List.rev f)
    
let list_max a less = ListUtils.max_elt (fun x y -> if less x y then -1 else 1) a

let improvement_policy_learn_strategies game node_total_ordering strategy_set old_strategy valu =
	(* Step 1: Update strategy_set *)
	let strategy_set = ref strategy_set in
	let add_to_strategy_set strat =
		strategy_set := TreeSet.add strat !strategy_set
	in
	add_to_strategy_set old_strategy;
	let n = pg_size game in
	for i = 0 to n - 1 do
	  let pl = pg_get_owner game i in
	  let tr = pg_get_successors game i in
		if pl = plr_Even then (
			ns_iter (fun j ->
				if node_valuation_ordering game node_total_ordering valu.(j) valu.(old_strategy.(i)) > 0 then (
					let s = Array.copy old_strategy in
					s.(i) <- j;
					add_to_strategy_set s
				)
			) tr;
		)
	done;
	(* Step 2: Build improvement set *)
	let improvement_set = ref (TreeSet.empty (fun x y -> compare (Array.to_list x) (Array.to_list y))) in
	let add_to_improvement_set strat =
		improvement_set := TreeSet.add strat !improvement_set
	in
	let morph base target node =
		let base_counter = compute_counter_strategy game base in
		let rec helper set v =
			if TreeSet.mem v set then None
			else if pg_get_owner game v = plr_Odd then helper (TreeSet.add v set) base_counter.(v)
			else if base.(v) = target.(v) then helper (TreeSet.add v set) base.(v)
			else let a = Array.copy base in
			     a.(v) <- target.(v);
				 Some a
		in
			helper TreeSet.empty_def node
	in
	TreeSet.iter (fun strategy ->
		let n = pg_size game in
		for i = 0 to n - 1 do
			let current = ref (morph old_strategy strategy i) in
			while !current != None do
				match !current with
					Some cur -> (
						add_to_improvement_set cur;
						current := morph cur strategy i
					)
				|	None -> ()
			done;
		done;
	) !strategy_set;
	(* Step3: Build combination strategy *)
	let improvement_set_array = Array.make (TreeSet.cardinal !improvement_set) ([||], valu) in
	let i = ref 0 in
	TreeSet.iter (fun st ->
		improvement_set_array.(!i) <- (st, evaluate_strategy game node_total_ordering st);
		incr i
	) !improvement_set;
	let best_strategies = Array.make (pg_size game) 0 in
	Array.iteri (fun i (st, va) ->
		let m = pg_size game in
		for v = 0 to m - 1 do
			if node_valuation_ordering game node_total_ordering va.(v) (snd improvement_set_array.(best_strategies.(v))).(v) > 0
			then best_strategies.(v) <- i;
		done;
	) improvement_set_array;
	let strategy = Array.init (pg_size game) (fun v ->
		if pg_get_owner game v = plr_Odd then -1
		else (fst improvement_set_array.(best_strategies.(v))).(v)
	) in
	(strategy, !strategy_set)
	
	
	
let improvement_policy_learn_cycles sub_policy game node_total_ordering (cycles, u) old_strategy valu =
	let (strategy, u') = sub_policy game node_total_ordering u old_strategy valu cycles in
	let combined_strategy =
        Array.init (Array.length valu) (fun i ->
        	if pg_get_owner game i = plr_Even
        	then strategy.(i)
        	else best_decision_by_valuation_ordering game node_total_ordering valu i
        )
    in
    let game' = subgame_by_edge_pred game (fun i j -> combined_strategy.(i) = j) in
    let (sccs, sccindex, topology, roots) = strongly_connected_components game' in
    let cycles = ref cycles in
    let normalize l =
    	ns_make (list_upfront (ns_nodes l) (list_max (ns_nodes l) (fun x y -> pg_get_priority game x < pg_get_priority game y)))
    in
    Array.iteri (fun i scc ->
    	if (ns_size scc > 1) && (topology.(i) = []) then (
    		let c = normalize scc in
    		if (pg_get_priority game (ns_first c) mod 2 = 0) && (not (TreeSet.mem c !cycles)) then (
    			cycles := TreeSet.add c !cycles;
    			let fmt k =
					match (pg_get_desc game k) with
		                None -> string_of_int k
		            |   Some t -> t
		        in    			
    			message 2 (fun _ -> "\nLearned cycle #" ^ string_of_int (TreeSet.cardinal !cycles) ^ " : " ^ ListUtils.format fmt (ns_nodes c) ^ "\n")
    		)
    	)
    ) sccs;
    (strategy, (!cycles, u'))
	
	







let improvement_policy_level game node_total_ordering data old_strategy valu =
	let (level) = data in
	let n = Array.length valu in
	if (level == 0) then (
		(improvement_policy_optimize_all_locally game node_total_ordering old_strategy valu, 1)
	) else (
		
		(* Contains nodes that have been identified as non final;
		   We don't use cycles with potential escape edges leading to non-final-nodes *)
		let non_final_nodes = ref TreeSet.empty_def in
		(* Contains edges that we have used as escape edges;
		   We don't use cycles with potential escape edges included in the set *)
		let used_escape_edges = ref TreeSet.empty_def in
	  let counter_strategy = compute_counter_strategy game old_strategy in
		let next_counter = ref (Array.copy counter_strategy) in

		let new_strategy = Array.copy old_strategy in
				
								
		
		
		let running = ref true in
		let changed = ref false in
		while !running do
			  let graph = subgame_by_edge_pred game (fun v w ->
					let pl = pg_get_owner game v in
					(pl = plr_Even) || (counter_strategy.(v) = w && !next_counter.(v) = w)
				) in 
				
				let m = pg_size game in
				for i = 0 to m - 1 do
				  let pl = pg_get_owner game i in
				  let tr = pg_get_successors game i in
				  if (pl = plr_Odd) && ns_exists (fun j -> counter_strategy.(i) != j && (TreeSet.mem j !non_final_nodes || TreeSet.mem (i,j) !used_escape_edges)) tr
				  then ns_iter (fun w -> pg_del_edge graph i w) (pg_get_successors graph i)
				done;
				

				let cycle = ref None in
				let i = ref 0 in
				while (!cycle = None && !i < n) do
					let pr = pg_get_priority game !i in
					if (pr mod 2 == 0) then (
						let s = ref TreeSet.empty_def in
						let rec build j cyc =
							if (j = !i && cyc != []) then (
								cycle := Some cyc
							) else if not (TreeSet.mem j !s) && (pg_get_priority game j <= pr) then (
								s := TreeSet.add j !s;
								let tr = pg_get_successors graph j in
								ns_iter (fun k ->
									if (!cycle = None)
									then build k ((j,k)::cyc);
								) tr;
							)
						in
						build !i [];
					);
					incr i;
				done;
				
				match !cycle with
				| None -> running := false
				| Some cycle -> (
					changed := true;
					List.iter (fun (i,j) ->
						if (pg_get_owner game i = plr_Even)
						then new_strategy.(i) <- j
					) cycle;
					List.iter (fun (i,_) ->
						non_final_nodes := TreeSet.add i !non_final_nodes;
					) cycle;
					next_counter := compute_counter_strategy game new_strategy;
					List.iter (fun (i,j) ->
						if (pg_get_owner game i = plr_Odd && !next_counter.(i) != j)
						then used_escape_edges := TreeSet.add (i, !next_counter.(i)) !used_escape_edges
					) cycle;
					);
		done;
		if !changed
		then (new_strategy, 0)
		else 	(improvement_policy_optimize_all_locally game node_total_ordering old_strategy valu, 1)
	)


let improvement_policy_smart game node_total_ordering todo old_strategy valu cycles =
    let combined_strategy =
        Array.init (Array.length valu) (fun i ->
            if pg_get_owner game i = plr_Even
            then old_strategy.(i)
            else best_decision_by_valuation_ordering game node_total_ordering valu i
        )
    in
    let improv_edge x y =
    	node_valuation_ordering game node_total_ordering valu.(old_strategy.(x)) valu.(y) <= 0
    in
	let cycle_applies cycle =
        let x = ref (List.hd cycle) in
        let cycle' = ref ((List.tl cycle) @ [!x]) in
        let applies1 = ref true in
        let applies0 = ref true in
        while !applies1 && (not (!cycle' = [])) do
            let z = List.hd !cycle' in
            cycle' := List.tl !cycle';
            if pg_get_owner game !x = plr_Odd
            then applies1 := combined_strategy.(!x) = z
            else applies0 := !applies0 && ((combined_strategy.(!x) = z) || (not (improv_edge !x z)));
            x := z
        done;
        !applies1 && (not !applies0)
	in

	let todo = TreeSet.filter cycle_applies (if TreeSet.is_empty todo then cycles else todo) in

	if TreeSet.is_empty todo then (
		let strat = improvement_policy_optimize_all_locally game node_total_ordering old_strategy valu in
		(strat, todo)
	)
	else (
        let strategy = Array.copy old_strategy in
		TreeSet.iter (fun cycle ->
   			let fmt k =
				match (pg_get_desc game k) with
	                None -> string_of_int k
	            |   Some t -> t
	        in    			
  			message 2 (fun _ -> "\nApply cycle : " ^ ListUtils.format fmt cycle ^ "\n");
            let x = ref (List.hd cycle) in
            let cycle' = ref ((List.tl cycle) @ [!x]) in
            while (not (!cycle' = [])) do
                let z = List.hd !cycle' in
                cycle' := List.tl !cycle';
                if (pg_get_owner game !x = plr_Even) && (improv_edge !x z) then strategy.(!x) <- z;
                x := z
            done
		) todo;
		(strategy, todo)
	)


let improvement_policy_cycle_avoid game node_total_ordering old_strategy valu =
	let new_strategy = Array.copy old_strategy in
	let counter_strategy = 
        Array.init (Array.length valu) (fun i ->
        	if pg_get_owner game i = plr_Even
        	then -1
        	else best_decision_by_valuation_ordering game node_total_ordering valu i
        )
    in
	let allowed v w =
		let s = ref TreeSet.empty_def in
		let current = ref w in
		let finished = ref false in
		while not !finished do
			s := TreeSet.add !current !s;
			if pg_get_owner game !current = plr_Even
			then current := new_strategy.(!current)
			else current := counter_strategy.(!current);
			finished := TreeSet.mem !current !s;
		done;
		not (TreeSet.mem v !s)
	in
	let deford x y = node_valuation_total_ordering game node_total_ordering valu x y in
	let ordering base x y =
		let ax = allowed base x in
		let ay = allowed base y in
		if ax = ay then deford x y
		else if ax then 1 else -1
	in
	let changed = ref false in
	let n = Array.length old_strategy in
	for i = 0 to n - 1 do
		if (pg_get_owner game i = plr_Even) then (
			let w = best_decision_by_ordering game (ordering i) i in
			if (w != new_strategy.(i)) && (deford new_strategy.(i) w < 0) then (
				new_strategy.(i) <- w;
				changed := true;
			)
		);
	done;
	if !changed
	then new_strategy
	else improvement_policy_optimize_all_locally game node_total_ordering old_strategy valu
	

type ('a, 'b) ab = A of 'a | B of 'b



let cycle_enforce_cycles_compare (node0, node1, edge0, edge1) (node0', node1', edge0', edge1') =
	let c0 = ns_compare node0 node0' in
	let c1 = ns_compare node1 node1' in
	let c2 = TreeMap.compare compare edge0 edge0' in
	let c3 = TreeMap.compare compare edge1 edge1' in
	if c0 != 0 then c0
	else if c1 != 0 then c1
	else if c2 != 0 then c2
	else c3

let improvement_policy_cycle_enforce game node_total_ordering (cycles, idx) old_strategy valu =
	let n = Array.length valu in
	
	let get_cycles strategy =
		let cycles = ref (TreeSet.empty cycle_enforce_cycles_compare) in
		let combined_strategy =
			Array.init (Array.length valu) (fun i ->
				if pg_get_owner game i = plr_Even
				then strategy.(i)
				else best_decision_by_valuation_ordering game node_total_ordering valu i
			)
		in
		let game' = subgame_by_edge_pred game (fun i j -> combined_strategy.(i) = j) in
		let (sccs, sccindex, topology, roots) = strongly_connected_components game' in
		Array.iteri (fun i scc ->
			if (ns_size scc > 1) && (topology.(i) = []) then (
				let node0 = ref ns_empty in
				let node1 = ref ns_empty in
				let edge0 = ref TreeMap.empty_def in
				let edge1 = ref TreeMap.empty_def in
				ns_iter (fun v ->
					if pg_get_owner game v = plr_Even then (
						node0 := ns_add v !node0;
						edge0 := TreeMap.add v combined_strategy.(v) !edge0;
					)
					else (
						node1 := ns_add v !node1;
						edge1 := TreeMap.add v combined_strategy.(v) !edge1;
					)
				) scc;
				cycles := TreeSet.add (!node0, !node1, !edge0, !edge1) !cycles;
			)
		) sccs;
		!cycles
	in
	
	let cyc_value v (node0, node1, edge0, edge1) =
		let valworst = ref None in
		let valcur = ref (empty_descending_relevance_ordered_set game node_total_ordering) in
		let m = ref (ns_size node1) in
		let nodecur = ref v in
		while !m > 0 do
			while (pg_get_owner game !nodecur = plr_Even) do
				valcur := TreeSet.add !nodecur !valcur;
				nodecur := TreeMap.find !nodecur edge0
			done;

			ns_iter (fun w ->
				if not (TreeMap.find !nodecur edge1 = w) then (
					let (e, pth, f) = valu.(w) in
					let valw = (e, TreeSet.union pth !valcur, f) in
					match !valworst with
						None -> valworst := Some valw;
					|	Some valw' -> if node_valuation_ordering game node_total_ordering valw valw' < 0
					                  then valworst := Some valw;
				);
			) (pg_get_successors game !nodecur);
			
			valcur := TreeSet.add !nodecur !valcur;
			nodecur := TreeMap.find !nodecur edge1;
			decr m;
		done;
		OptionUtils.get_some !valworst
	in
	
	let c = ref 0 in
	let i = ref idx in
	let finished = ref false in
	let new_strategy = Array.copy old_strategy in
	while (not !finished) && (!c <= n) do
		if (pg_get_owner game !i = plr_Even) then (
			let pots = ref [] in
			ns_iter (fun w ->
				new_strategy.(!i) <- w;
				if TreeSet.subset (get_cycles new_strategy) cycles
				then pots := (A w, valu.(w))::!pots;
				new_strategy.(!i) <- old_strategy.(!i);
			) (pg_get_successors game !i);
			TreeSet.iter (fun ((node0, node1, edge0, edge1) as cyc) ->
				if (ns_elem !i node0) then (
					TreeMap.iter (fun v w ->
						new_strategy.(v) <- w;
					) edge0;
					if TreeSet.subset (get_cycles new_strategy) cycles
					then pots := (B cyc, cyc_value !i cyc)::!pots;
					ns_iter (fun v ->
						new_strategy.(v) <- old_strategy.(v);
					) node0;
				);
			) cycles;
			let best = ref None in
			List.iter (fun (q, valw) ->
				match !best with
					None -> if node_valuation_ordering game node_total_ordering valw valu.(old_strategy.(!i)) > 0
					        then best := Some (q, valw)
				|	Some (q', valw') -> if node_valuation_ordering game node_total_ordering valw valw' > 0
										then best := Some (q, valw);
			) !pots;
			match !best with
				None -> ()
			|	Some (A w, _) -> (
					new_strategy.(!i) <- w;
					finished := true;
				)
			|	Some (B (_, _, edge0, _), _) -> (
					TreeMap.iter (fun v w ->
						new_strategy.(v) <- w;
					) edge0;
					finished := true;
				);
		);
		incr c;
		i := (!i + 1) mod n;
	done;
	if !finished
	then (new_strategy, (cycles, !i))
	else (
		let new_strategy = improvement_policy_optimize_all_locally game node_total_ordering old_strategy valu in
		(new_strategy, (TreeSet.union cycles (get_cycles new_strategy), idx))
	)



let strategy_improvement_cycle_avoid game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_no_user_data improvement_policy_cycle_avoid) () false "STRIMPR_INTONE";;

let strategy_improvement_cycle_enforce game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position improvement_policy_cycle_enforce ((TreeSet.empty cycle_enforce_cycles_compare), 0) false "STRIMPR_INTTWO";;


let strategy_improvement_learn_strategies game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position improvement_policy_learn_strategies (TreeSet.empty (fun x y -> compare (Array.to_list x) (Array.to_list y))) true "STRIMPR_STRLEA";;

let strategy_improvement_smart_policy game =
	(*strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_learn_cycles improvement_policy_smart) (TreeSet.empty compare, TreeSet.empty compare) true "STRIMPR_SMART";; *)
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_level) (0) true "STRIMPR_SMART";;

let strategy_improvement_justlearn_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_learn_cycles (fun a b c d e f -> (improvement_policy_optimize_all_locally a b d e, c))) (TreeSet.empty compare, TreeSet.empty compare) true "STRIMPR_JULE";;


register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_smart_policy g)
	"smartstratimpr" "ssi" "use smart strategy improvement";;


register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_learn_strategies g)
	"learnstratimpr" "lsi" "use strategy-learning strategy improvement";;

	

(*
register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_justlearn_policy g)
	"julestratimpr" "siju" "use just learn strategy improvement";;
*)
(*
register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_by_counterstrategy_policy g)
	"strimprbyco" "sibc" "use strategy improvement by counterstrategy";;
*)



let register _ =
    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_cycle_avoid g)
        "switchint" "swint" "switch internal #1";

    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_cycle_enforce g)
        "switchintx" "swintx" "switch internal #2";;

