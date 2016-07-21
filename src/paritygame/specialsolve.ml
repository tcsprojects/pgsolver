open Basics;;
open Tcsset;;
open Tcsqueue;;
open Paritygame;;


type compact_sol_strat = (int * int * int) list;;

let compact_sol_strat_to_sol_strat game comp =
	let n = pg_size game in
	let sol = Array.make n (-1) in
	let strat = Array.make n (-1) in
	List.iter (fun (i, pl, j) ->
		sol.(i) <- pl;
		strat.(i) <- j
	) comp;
	(sol, strat);;



let find_useful_self_cycles game =
    let n = pg_size game in
    let ret = ref [] in
    for i = 0 to n - 1 do
			  let pr = pg_get_pr game i in
				let pl = pg_get_pl game i in
        if pr mod 2 = pl then (
					  ns_iter (fun j ->
							if j = i then ret := (i, pl, i)::!ret
						) (pg_get_tr game i)
        )
    done;
    !ret;;


let solve_cycle_scc game =
    let n = pg_size game in
    let max_pr_node = pg_max_prio_node game in
    let max_pr_pl = (pg_get_pr game max_pr_node) mod 2 in
    let solution = Array.make n max_pr_pl in
    let strategy = Array.make n (-1) in

    if pg_get_pl game max_pr_node = max_pr_pl
    then strategy.(max_pr_node) <- ns_some (pg_get_tr game max_pr_node);
    let _ = attr_closure_inplace game strategy max_pr_pl [max_pr_node] in
    (solution, strategy)


type process_result = PlayerWin of strategy * (int array)
                    | PlayerLoss
                    | Undecided of int

let solve_single_player_scc game player =
	let process_scc sccgame sccnodes =
		let n = pg_size sccgame in
		let p_pl = ref (-1) in
		let p_op = ref (-1) in
		for i = 0 to n - 1 do
			let pr = pg_get_pr sccgame i in
			let arg = if pr mod 2 = player then p_pl else p_op in
			arg := max !arg pr
		done;
		if !p_pl > !p_op then (
            let strategy = Array.make n (-1) in
			let vs = pg_prio_nodes sccgame !p_pl in
			let vsset = TreeSet.of_list_def vs in
			List.iter (fun i ->
				if pg_get_pl sccgame i = player then strategy.(i) <- ns_some (pg_get_tr sccgame i)
			) vs;
			let _ = attr_closure_inplace' sccgame strategy player vsset true (fun _ -> true) true in
			PlayerWin (strategy, sccnodes)
		)
		else if !p_pl < 0 then PlayerLoss
		else Undecided (!p_pl + 1)
	in
	let rec process_undecided sccgame sccnodes p =
		let n = pg_size sccgame in
		let strategy = Array.make n (-1) in
		let vs = collect_nodes_by_prio sccgame (fun pr -> pr >= p) in
		let vsset = TreeSet.of_list_def vs in
		let attr = attr_closure_inplace' sccgame strategy (1-player) vsset true (fun _ -> true) true in
		let backup = ref (TreeSet.of_list_def attr) in
		List.iter (fun i ->
			   ns_iter (fun j -> backup := TreeSet.add j !backup) (pg_get_successors sccgame i);
			   ns_iter (fun j -> backup := TreeSet.add j !backup) (pg_get_predecessors sccgame i);
			                 (* it looked like it was "pg_get_predecessors game i" instead but I think "... sccgame ..." is correct - ML *)
			  ) attr;
		let backuped = ref [] in
		TreeSet.iter (fun i ->
			      let pr = pg_get_priority game i in
			      let pl = pg_get_owner game i in
			      let succs = pg_get_successors game i in
			      let desc = pg_get_desc game i in 
			      backuped := (i, (pr,pl,succs,desc))::!backuped
			     ) !backup;
		pg_remove_nodes sccgame attr;
		let (newsccs,_,_,_) = strongly_connected_components sccgame in
		let scccount = Array.length newsccs in
		let todo = ref 0 in
		let undecided_stack = ref [] in
		let success = ref None in
		while (!todo < scccount) && (!success = None) do
		  let current = newsccs.(!todo) in
		  let curgame = subgame_by_list sccgame current in
		  if (pg_size curgame > 1) || (ns_size (pg_get_tr curgame 0) > 0) then (
            	    let nodelist = Array.of_list current in
            	    match process_scc curgame nodelist with
            	      PlayerWin (s, t) -> success := Some (s, t)
            	     |	Undecided p -> undecided_stack := (curgame, nodelist, p)::!undecided_stack
            	     |	PlayerLoss -> ()
		  );
		  incr todo
		done;
		if !success = None then (
        	  while (!undecided_stack != []) && (!success = None) do
        	    let (curgame, nodelist, p) = List.hd !undecided_stack in
        	    undecided_stack := List.tl !undecided_stack;
        	    success := process_undecided curgame nodelist p
        	  done
		);
		List.iter (fun (i, (pr,pl,succs,desc)) -> pg_set_priority sccgame i pr;
							  pg_set_owner sccgame i pl;
							  pg_set_desc sccgame i desc;
							  ns_iter (fun w -> pg_add_edge sccgame i w) succs
 			  ) !backuped;
		match !success with
		  None -> None
		|   Some (strat, nodes) -> 
                     Some (strat, Array.map (fun j -> sccnodes.(j)) nodes)
	in
	let rec solve_single_player_scc' sccgame sccnodes =
	  match process_scc sccgame sccnodes with
	    Undecided p -> process_undecided sccgame sccnodes p
	   |	PlayerWin (s, t) -> Some (s, t)
	   |	PlayerLoss -> None
	in
	let n = pg_size game in
	match solve_single_player_scc' game (Array.init n (fun i -> i)) with
	  Some (strat, sccnodes) -> (let strategy = Array.make n (-1) in
				     Array.iteri (fun i j ->
						  strategy.(sccnodes.(i)) <- if j >= 0 then sccnodes.(j) else -1
						 ) strat;
				     let vsset = TreeSet.of_array_def sccnodes in
				     let _ = attr_closure_inplace' game strategy player vsset true (fun _ -> true) true in
				     (Array.make n player, strategy)
				    )
	 |	None -> let strat = Array.init n (fun j ->
						  if pg_get_pl game j = player then -1 else ns_some (pg_get_tr game j)
						 )
			in
			(Array.make n (1 - player), strat);;
  
  
let solve_single_parity_scc game player =
    let n = pg_size game in
	let solution = Array.make n player in
	let strategy = Array.make n (-1) in
	for i = 0 to n - 1 do
		if (pg_get_pr game i >= 0) && (pg_get_pl game i = player)
		then strategy.(i) <- ns_some (pg_get_tr game i) 
	done;
	(solution, strategy);;







(* Takes a game in which only player pl can make decisions (the algorithm doesn't distinguish positions for player pl and player 1 - pl.
   Computes winning sets for both players and strategy for player pl *)
let compute_winning_nodes_for_direct (game: paritygame) pl =
	let compute_strat = true in

    let subnodes_by_list game li =
        let n = List.length li in
        let g = Array.make n (-1, -1, ref TreeSet.empty_def) in
        let i = ref 0 in
        List.iter (fun arri ->
					  let pr = pg_get_pr game arri in
						let pl = pg_get_pl game arri in
						(* This is serious black magic here. Abusing types. Oliver, why would you do something like this? *)
						pg_set_priority game arri (-2);
						pg_set_owner game arri !i;
            g.(!i) <- (pr, pl, ref TreeSet.empty_def);
            i := !i + 1
        ) li;
        let i = ref 0 in
        List.iter (fun arri ->
            let (pr, pl, _) = g.(!i) in
						let delta = pg_get_tr game arri in
            let l = ref TreeSet.empty_def in
							ns_iter (fun j ->
								if pg_get_pr  game j = -2 then l := TreeSet.add (pg_get_pl game j) !l
							) delta;
            g.(!i) <- (pr, pl, l);
            i := !i + 1
        ) li;
        let i = ref 0 in
        List.iter (fun arri ->
            let (pr, pl, _) = g.(!i) in
						pg_set_priority game arri pr;
						pg_set_owner game arri pl;
            i := !i + 1
        ) li;
        g
    in

    let list_attractor transp getpl strat source_set_ref todo =
    	while not (Queue.is_empty todo) do
    		let x = Queue.take todo in
            transp x (fun y -> if TreeSet.mem y !source_set_ref then (
            	source_set_ref := TreeSet.remove y !source_set_ref;
                Queue.add y todo;
                if (getpl y = pl) && (strat.(y) < 0)
                then strat.(y) <- x;
            ) )
    	done
    in

	let solve_scc comp pl =
		let game' = subnodes_by_list game comp in
		let strat = Array.make (if compute_strat then Array.length game' else 0) (-1) in
		let new_edges = Queue.create () in
        Array.iteri (fun i (_, _, delta) -> TreeSet.iter (fun j -> Queue.add (i, j) new_edges) !delta) game';
        let winner = ref (1 - pl) in
        while (!winner != pl) && not (Queue.is_empty new_edges) do
        	let (u, v) = Queue.take new_edges in
        	if u = v
        	then let (pr_u, _, _) = game'.(u) in (
        		if pr_u mod 2 = pl then (
        			winner := pl;
        			if compute_strat then (
        				let (_, _, delta_u) = game'.(u) in
                        let game'' = subnodes_by_list game comp in
                        let g = Array.make (Array.length game'') [] in
  						Array.iteri (fun i -> fun (_,_,ws) -> TreeSet.iter (fun w -> g.(w) <- i::g.(w)) !ws) game'';
        				let target_set = ref (TreeSet.singleton_def u) in
        				let todo = Queue.create () in
        				TreeSet.iter (fun x ->
        					let (pr_x, _, _) = game'.(x) in
        					if pr_x <= pr_u
        					then Queue.add x todo
        				) !delta_u;
        				let finished = ref false in
        				while not !finished do
        					let x = Queue.take todo in
        					let (_, pl', d) = game''.(x) in
        					let inters = TreeSet.inter !d !target_set in
        					if TreeSet.is_empty (inters)
        					then Queue.add x todo
        					else (
        						target_set := TreeSet.add x !target_set;
        						if pl' = pl then strat.(x) <- TreeSet.min_elt inters;
        						if x = u then finished := true;
        					)
        				done;
        				let s = ref TreeSet.empty_def in
        				for i = 0 to (Array.length game') - 1 do
        					if not (TreeSet.mem i !target_set)
        					then s := TreeSet.add i !s
        				done;
        				let todo = Queue.create () in
        				TreeSet.iter (fun x -> Queue.add x todo) !target_set;
        				list_attractor (fun x f -> List.iter f g.(x)) (fun i -> let (_, pl, _) = game''.(i) in pl) strat s todo
        			)
        		)
        	)
        	else let (pr_u, _, delta_u) = game'.(u) in
        		 let (pr_v, _, delta_v) = game'.(v) in
        		 if (pr_u mod 2 = pl) && (pr_u >= pr_v)
        		 then TreeSet.iter (fun w ->
        		 		if not (TreeSet.mem w !delta_u) then (
                            Queue.add (u, w) new_edges;
                            delta_u := TreeSet.add w !delta_u
                         )
        		 ) !delta_v
        done;
        (!winner, strat)
    in

    let n = pg_size game in
    let strategy = Array.make (if compute_strat then n else 0) (-1) in

    let max_prio_queue comp pr =
    	let q = Queue.create () in
    	List.iter (fun x -> 
    		if pg_get_pr game x = pr then Queue.add x q
    	) comp;
    	q
    in

	let (sccs, sccindex, topology, roots) = strongly_connected_components game in
	let marked = Array.make (Array.length sccs) (-1) in

	let max_prio_for l pl =
		List.fold_left (fun p el ->
			let pr = pg_get_pr game el in
				if pr mod 2 = pl then max p pr else p
		) (-1) l
	in

	let rec process_root r =
		let getpl x = pg_get_pl game x in
		if marked.(r) < 0 then (
            List.iter process_root topology.(r);
            let c = List.fold_left (fun c c' -> if c != -1 && marked.(c) = pl then c else c') (-1) topology.(r) in
            if c != -1 && marked.(c) = pl
            then (
            	marked.(r) <- pl;
            	if compute_strat
            	then list_attractor (fun x f -> ns_iter f (pg_get_predecessors game x)) getpl strategy (ref (TreeSet.of_list_def sccs.(r))) (QueueUtils.of_list sccs.(c));
            )
            else let comp = sccs.(r) in
                 if List.length comp = 1
                 then let x = List.hd comp in
								      let pr = pg_get_pr game x in
											let pl' = pg_get_pl game x in
											let delta = pg_get_tr game x in
                      if ns_size delta = 0
                      then marked.(r) <- 1 - pl'
                      else if (ns_exists (fun y -> x = y) delta) && (pr mod 2 = pl)
                      then (
                      	marked.(r) <- pl;
                      	if (compute_strat && getpl x = pl) then strategy.(x) <- x
                      )
                      else marked.(r) <- 1 - pl
                 else let pl_max = max_prio_for comp pl in
                      let plo_max = max_prio_for comp (1 - pl) in
                      if pl_max > plo_max
                      then (
                      	marked.(r) <- pl;
                      	if compute_strat
                      	then (
                      		list_attractor (fun x f -> ns_iter f (pg_get_predecessors game x)) getpl strategy (ref (TreeSet.of_list_def sccs.(r))) (max_prio_queue comp pl_max);
                      		List.iter (fun q ->
														let pl' = pg_get_pl game q in
														let d = pg_get_tr game q in
                      			if (pl' = pl) then
															ns_iter (fun di ->
																if (strategy.(q) < 0) then (
																	if sccindex.(di) = r then strategy.(q) <- di
																);
															) d;
                      		) comp
                      	)
                      )
                      else marked.(r) <- if pl_max < 0
                                         then 1 - pl
                                         else let (winner, strat) = solve_scc comp pl in (
                                         	if compute_strat && (winner = pl) then (
                                         		let arr = Array.of_list comp in
                                         		for i = 0 to (Array.length arr) - 1 do
                                         			if strat.(i) >= 0 then strategy.(arr.(i)) <- arr.(strat.(i))
                                         		done;
                                         	);
                                         	winner
                                         )
		)
	in

	List.iter (fun r -> let _ = process_root r in ()) roots;
	let solution = Array.make n (-1) in
	for i = 0 to (Array.length sccs) - 1 do
		List.iter (fun u -> solution.(u) <- marked.(i)) sccs.(i)
	done;
	(solution, strategy);;


let compute_winning_nodes_direct game strat pl =
	let sol = fst (compute_winning_nodes_for_direct (subgame_by_strat game strat) (1 - pl)) in
	let l = ref [] in
	for i = 0 to (Array.length sol) - 1 do
		if sol.(i) = pl then l := i::!l
	done;
	!l;;
