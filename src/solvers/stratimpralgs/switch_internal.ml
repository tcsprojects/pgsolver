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
		if pl = 0 then (
			Array.iter (fun j ->
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
			else if pg_get_pl game v = 1 then helper (TreeSet.add v set) base_counter.(v)
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
		if pg_get_pl game v = 1 then -1
		else (fst improvement_set_array.(best_strategies.(v))).(v)
	) in
	(strategy, !strategy_set)
	
	
	
let improvement_policy_learn_cycles sub_policy game node_total_ordering (cycles, u) old_strategy valu =
	let (strategy, u') = sub_policy game node_total_ordering u old_strategy valu cycles in
	let combined_strategy =
        Array.init (Array.length valu) (fun i ->
        	if pg_get_pl game i = 0
        	then strategy.(i)
        	else best_decision_by_valuation_ordering game node_total_ordering valu i
        )
    in
    let game' = subgame_by_edge_pred game (fun i j -> combined_strategy.(i) = j) in
    let (sccs, sccindex, topology, roots) = strongly_connected_components game' in
    let cycles = ref cycles in
    let normalize l =
    	list_upfront l (list_max l (fun x y -> pg_get_pr game x < pg_get_pr game y))
    in
    Array.iteri (fun i scc ->
    	if (List.length scc > 1) && (topology.(i) = []) then (
    		let c = normalize scc in
    		if (pg_get_pr game (List.hd c) mod 2 = 0) && (not (TreeSet.mem c !cycles)) then (
    			cycles := TreeSet.add c !cycles;
    			let fmt k =
					match (pg_get_desc game k) with
		                None -> string_of_int k
		            |   Some t -> t
		        in    			
    			message 2 (fun _ -> "\nLearned cycle #" ^ string_of_int (TreeSet.cardinal !cycles) ^ " : " ^ ListUtils.format fmt c ^ "\n")
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
					let pl = pg_get_pl game v in
					(pl = 0) || (counter_strategy.(v) = w && !next_counter.(v) = w)
				) in 
				
				let m = pg_size game in
				for i = 0 to m - 1 do
				  let pr = pg_get_priority game i in
				  let pl = pg_get_owner game i in
				  let tr = pg_get_successors game i in
				  if (pl = 1) && ArrayUtils.exists tr (fun _ j -> counter_strategy.(i) != j && (TreeSet.mem j !non_final_nodes || TreeSet.mem (i,j) !used_escape_edges))
				  then pg_set_tr graph i [||]
				done;
				

				let cycle = ref None in
				let i = ref 0 in
				while (!cycle = None && !i < n) do
					let pr = pg_get_pr game !i in
					if (pr mod 2 == 0) then (
						let s = ref TreeSet.empty_def in
						let rec build j cyc =
							if (j = !i && cyc != []) then (
								cycle := Some cyc
							) else if not (TreeSet.mem j !s) && (pg_get_pr game j <= pr) then (
								s := TreeSet.add j !s;
								let tr = pg_get_tr graph j in
								Array.iter (fun k ->
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
						if (pg_get_pl game i = 0)
						then new_strategy.(i) <- j
					) cycle;
					List.iter (fun (i,_) ->
						non_final_nodes := TreeSet.add i !non_final_nodes;
					) cycle;
					next_counter := compute_counter_strategy game new_strategy;
					List.iter (fun (i,j) ->
						if (pg_get_pl game i = 1 && !next_counter.(i) != j)
						then used_escape_edges := TreeSet.add (i, !next_counter.(i)) !used_escape_edges
					) cycle;
					);
		done;
		if !changed
		then (new_strategy, 0)
		else 	(improvement_policy_optimize_all_locally game node_total_ordering old_strategy valu, 1)
	)




























	
(*
let improvement_policy_smart game' node_total_ordering _ old_strategy valu cycles =
    let msg_tagged v = message_autotagged v (fun _ -> "STRIMPR_SMART") in
	let game = pg_copy game' in
	let n = Array.length game in
	let valu_ord = node_valuation_ordering game' node_total_ordering in
	Array.iteri (fun i (pr, pl, tr, _) ->
		if pl = 0
		then pg_set_tr game i (Array.of_list (List.filter (fun j -> valu_ord valu.(j) valu.(old_strategy.(i)) >= 0) (Array.to_list tr)))
	) game;

	let strategy = Array.copy old_strategy in
	let valu_ord = node_valuation_ordering game node_total_ordering in
	let graph = paritygame_to_dynamic_paritygame game in

	let valu2 = Array.copy valu in
	let valued = Array.make n false in

	for i = n - 4 to n - 1 do
		valued.(i) <- true
	done;

	let todo = ref (TreeSet.empty compare) in
	let rest = ref (TreeSet.empty compare) in

	for v = 0 to n - 1 do
		if valued.(v)
		then todo := TreeSet.union !todo (DynamicGraph.get_node_pred v graph)
		else rest := TreeSet.add v !rest
	done;

	let cycle_array = Array.make (TreeSet.cardinal cycles) [] in
	let cycle_ptr = Array.make n (TreeSet.empty compare) in
	let ctr = ref 0 in
	TreeSet.iter (fun c ->
		cycle_array.(!ctr) <- c;
		List.iter (fun i -> cycle_ptr.(i) <- TreeSet.add !ctr cycle_ptr.(i)) c;
		incr ctr;
	) cycles;
	let remove_cycle c =
		List.iter (fun i -> cycle_ptr.(i) <- TreeSet.remove c cycle_ptr.(i)) cycle_array.(c);
		cycle_array.(c) <- []
	in
	let propagate_edge v u =
		TreeSet.iter (fun c ->
			let cycle = cycle_array.(c) in
			let cycle' = ref cycle in
			while not ((List.hd !cycle' = v)) do
				cycle' := List.tl !cycle'
			done;
			if ((!cycle' = []) && (not (List.hd cycle = u))) ||
			   ((not (!cycle' = [])) && (not (List.hd !cycle' = u)))
			then remove_cycle c
		) cycle_ptr.(v);
	in
	let cycle_avail =
		List.for_all (fun v -> (pg_get_pl game v = 0) || valued.(v))
	in
	let cycle_value cycle v =
		let cycle = ref (list_upfront cycle v) in
		let valus = ref [] in
		let path = ref (TreeSet.empty compare) in
		while not (!cycle = []) do
			let u = List.hd !cycle in
			cycle := List.tl !cycle;
			if (pg_get_pl game u = 1) && (not valued.(u)) then (
				let tr = pg_get_tr game' u in
				let ordering x y = valu_ord valu2.(x) valu2.(y) in
				let w = array_max tr (fun x y -> ordering x y >= 0) in
				let (a, b, c) = valu2.(w) in
				valus := (a, TreeSet.union !path b, c)::!valus
			);
			path := TreeSet.add u !path
		done;
		list_max !valus (fun x y -> valu_ord x y >= 0)
	in


	let compute_update' v u = let (a, b, c) = valu2.(u) in (a, TreeSet.add v b, c + 1) in
	let compute_update v =
		let p = TreeSet.filter (fun u -> valued.(u)) (DynamicGraph.get_node_succ v graph) in
		let pl = pg_get_pl game v in
		let cmp x y = if pl = 0 then valu_ord valu2.(x) valu2.(y) else valu_ord valu2.(y) valu2.(x) in
		TreeSet.fold (fun u mx -> if cmp u mx > 0 then u else mx) p (TreeSet.min_elt p)
	in

	while (not (TreeSet.is_empty !rest)) do
		while (not (TreeSet.is_empty !todo)) do
			let v = TreeSet.min_elt !todo in
			todo := TreeSet.remove v !todo;
			if not valued.(v) then (
				let succs = DynamicGraph.get_node_succ v graph in
				if TreeSet.for_all (fun u -> valued.(u)) succs then (
					if (pg_get_pl game v = 1) then (
                        let u = compute_update v in
                        let va = compute_update' v u in
                        valued.(v) <- true;
                        valu2.(v) <- va;
					)
					else (
                        let u = compute_update v in
                        let va = ref (compute_update' v u) in
                        let c = ref (-1) in
                        TreeSet.iter (fun d ->
                        	if cycle_avail (cycle_array.(d)) then (
                                let va2 = cycle_value (cycle_array.(d)) v in
                                if valu_ord va2 !va > 0 then (
                                    c := d;
                                    va := va2
                                )
                            )
                        ) cycle_ptr.(v);
                        if !c = -1 then (
                            valued.(v) <- true;
                            valu2.(v) <- !va;
                            strategy.(v) <- u
                        )
                        else (
                            let cycle = cycle_array.(!c) in
                            msg_tagged 3 (fun _ -> "Applying cycle: " ^ intListUtils.format cycle ^ "\n");
                            let x = ref (List.hd cycle) in
                            let y = ref (List.tl cycle @ [!x]) in
                            while not (!y = []) do
                                let z = List.hd !y in
                                y := List.tl !y;
                                if pg_get_pl game' !x = 0 then (
                                    Array.iter (fun j ->
                                        DynamicGraph.del_edge !x j graph
                                    ) (pg_get_tr game' !x);
                                    DynamicGraph.add_edge !x z graph;
                                    pg_set_tr game !x [|z|]
                                );
                                todo := TreeSet.add !x !todo;
                                x := z
                            done;
                            remove_cycle !c
                        )
                    )
				)
				else if pg_get_pl game v = 1 then (
					let u = compute_update v in
					if valu_ord (compute_update' v u) valu2.(v) = 0 then (
						valued.(v) <- true;
						propagate_edge v u
					)
				);
				if valued.(v) then (
					todo := TreeSet.union !todo (DynamicGraph.get_node_pred v graph);
					rest := TreeSet.remove v !rest
				)
			)
		done;
		if not (TreeSet.is_empty !rest) then (
            let temp = TreeSet.filter (fun v -> (pg_get_pl game v = 1) && (TreeSet.exists (fun u -> valued.(u)) (DynamicGraph.get_node_succ v graph))) !rest in
            if TreeSet.is_empty temp
            then failwith "crap"
            else let l = TreeSet.fold (fun v l -> let u = compute_update v in (v, u, compute_update' v u)::l) temp [] in
                 let (v, u, va) = list_max l (fun (v, _, a) (w, _, b) -> valu_ord a b > 0) in (
                    todo := TreeSet.union !todo (DynamicGraph.get_node_pred v graph);
                    rest := TreeSet.remove v !rest;
                    valu2.(v) <- va;
                    valued.(v) <- true;
                    propagate_edge v u
                 )
        )
	done;

	(strategy, ())
*)

let improvement_policy_smart game node_total_ordering todo old_strategy valu cycles =
    let combined_strategy =
        Array.init (Array.length valu) (fun i ->
            if pg_get_pl game i = 0
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
            if pg_get_pl game !x = 1
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
                if (pg_get_pl game !x = 0) && (improv_edge !x z) then strategy.(!x) <- z;
                x := z
            done
		) todo;
		(strategy, todo)
	)


let improvement_policy_cycle_avoid game node_total_ordering old_strategy valu =
	let new_strategy = Array.copy old_strategy in
	let counter_strategy = 
        Array.init (Array.length valu) (fun i ->
        	if pg_get_pl game i = 0
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
			if pg_get_pl game !current = 0
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
		if (pg_get_pl game i = 0) then (
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
	let c0 = TreeSet.compare node0 node0' in
	let c1 = TreeSet.compare node1 node1' in
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
				if pg_get_pl game i = 0
				then strategy.(i)
				else best_decision_by_valuation_ordering game node_total_ordering valu i
			)
		in
		let game' = subgame_by_edge_pred game (fun i j -> combined_strategy.(i) = j) in
		let (sccs, sccindex, topology, roots) = strongly_connected_components game' in
		Array.iteri (fun i scc ->
			if (List.length scc > 1) && (topology.(i) = []) then (
				let node0 = ref TreeSet.empty_def in
				let node1 = ref TreeSet.empty_def in
				let edge0 = ref TreeMap.empty_def in
				let edge1 = ref TreeMap.empty_def in
				List.iter (fun v ->
					if pg_get_pl game v = 0 then (
						node0 := TreeSet.add v !node0;
						edge0 := TreeMap.add v combined_strategy.(v) !edge0;
					)
					else (
						node1 := TreeSet.add v !node1;
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
		let m = ref (TreeSet.cardinal node1) in
		let nodecur = ref v in
		while !m > 0 do
			while (pg_get_pl game !nodecur = 0) do
				valcur := TreeSet.add !nodecur !valcur;
				nodecur := TreeMap.find !nodecur edge0
			done;

			Array.iter (fun w ->
				if not (TreeMap.find !nodecur edge1 = w) then (
					let (e, pth, f) = valu.(w) in
					let valw = (e, TreeSet.union pth !valcur, f) in
					match !valworst with
						None -> valworst := Some valw;
					|	Some valw' -> if node_valuation_ordering game node_total_ordering valw valw' < 0
					                  then valworst := Some valw;
				);
			) (pg_get_tr game !nodecur);
			
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
		if (pg_get_pl game !i = 0) then (
			let pots = ref [] in
			Array.iter (fun w ->
				new_strategy.(!i) <- w;
				if TreeSet.subset (get_cycles new_strategy) cycles
				then pots := (A w, valu.(w))::!pots;
				new_strategy.(!i) <- old_strategy.(!i);
			) (pg_get_tr game !i);
			TreeSet.iter (fun ((node0, node1, edge0, edge1) as cyc) ->
				if (TreeSet.mem !i node0) then (
					TreeMap.iter (fun v w ->
						new_strategy.(v) <- w;
					) edge0;
					if TreeSet.subset (get_cycles new_strategy) cycles
					then pots := (B cyc, cyc_value !i cyc)::!pots;
					TreeSet.iter (fun v ->
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



register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_cycle_avoid g)
	"switchint" "swint" "switch internal #1";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_cycle_enforce g)
	"switchintx" "swintx" "switch internal #2";;

