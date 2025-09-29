open Basics;;
open Tcsset;;
open Tcsqueue;;
open Paritygame;;


type compact_sol_strat = (node * player * node) list;;

let compact_sol_strat_to_sol_strat game comp =
	let n = pg_size game in
	let sol = sol_make n in
	let strat = str_make n in
	List.iter (fun (i, pl, j) -> sol_set sol i pl;
				     str_set strat i j
		  ) comp;
	(sol, strat);;



let find_useful_self_cycles game =
    let ret = ref [] in
    pg_iterate (fun i -> fun (pr,pl,succs,_,_) -> if prio_good_for_player pr pl then (
						    ns_iter (fun j ->
							     if j = i then ret := (i, pl, i)::!ret
							    ) succs
						  )
	       ) game;
    !ret;;


let solve_cycle_scc game =
    let n = pg_size game in
    let max_pr_node = pg_max_prio_node game in
    let max_pr_pl = plr_benefits (pg_get_priority game max_pr_node) in
    let solution = sol_init game (fun _ -> max_pr_pl) in
    let strategy = str_make n in

    if pg_get_owner game max_pr_node = max_pr_pl
    then str_set strategy (max_pr_node) (ns_some (pg_get_successors game max_pr_node));
    let _ = attr_closure_inplace game strategy max_pr_pl (ns_make [max_pr_node]) in
    (solution, strategy)



let solve_single_player_scc game player =
  let n = pg_size game in
  let strategy = str_make n in
  let temp_strat = str_make n in
  let solution = sol_make n in
  
  let clear_visited _ = str_iter (fun v _ -> str_set temp_strat v nd_undef) temp_strat in
  
  let can_reach_itself v pr =
    let queue = SingleOccQueue.create () in
    let rec can_reach _ = if SingleOccQueue.is_empty queue then
			     false
			   else
			     begin
			       let (w,u) = SingleOccQueue.take queue in
			       if str_get temp_strat w != nd_undef || (let pr' = pg_get_priority game w in pr' > pr && not (prio_good_for_player pr' player)) then
				 can_reach ()
			       else
				 begin
				   str_set temp_strat w u;
				   if w=v then
				     begin
				       str_iter (fun i w -> if w != nd_undef then (
							      sol_set solution i player;
							      if pg_get_owner game i = player then str_set strategy i (str_get temp_strat i))
						) temp_strat;
				       true
				     end
				   else
				     begin
				       ns_iter (fun x -> SingleOccQueue.add (x,w) queue) (pg_get_predecessors game w);
				       can_reach ()
				     end
				 end
			     end
    in
    clear_visited ();
    ns_iter (fun w -> SingleOccQueue.add (w,v) queue) (pg_get_predecessors game v);
    can_reach ()
  in

  let complete_solution_and_strategy _ =
    clear_visited ();
    let queue = SingleOccQueue.create () in
    sol_iter (fun v -> fun pl -> if pl != plr_undef then (
				   ns_iter (fun w -> if sol_get solution w = plr_undef then SingleOccQueue.add (w,v) queue) (pg_get_predecessors game v)) 
	     ) solution;
    while not (SingleOccQueue.is_empty queue) do
      let (w,v) = SingleOccQueue.take queue in
      sol_set solution w player;
      if pg_get_owner game w = player then str_set strategy w v;
      ns_iter (fun u -> if sol_get solution u = plr_undef then SingleOccQueue.add (u,w) queue) (pg_get_predecessors game w)
    done
  in

  let good_prios = ref (List.sort (fun p -> fun q -> (-1) * (compare p q)) (pg_get_selected_priorities game (fun pr -> prio_good_for_player pr player))) in
  let found = ref false in
  while not !found && !good_prios != [] do
    let pr = List.hd !good_prios in
    good_prios := List.tl !good_prios;

    let vs = pg_prio_nodes game pr in
    found := ns_fold (fun b -> fun v -> b || can_reach_itself v pr) false vs;
  done;
  (if !found then
    complete_solution_and_strategy ()
   else
     begin
       let pl' = plr_opponent player in 
       sol_iter (fun i -> fun _ -> sol_set solution i pl';
				   if pg_get_owner game i = pl' then str_set strategy i (ns_first (pg_get_successors game i))
		) solution
     end);
  (solution, strategy)

      
let solve_single_parity_scc game player =
    let solution = sol_init game (fun _ -> player) in
    let strategy = str_create game in
    pg_iterate (fun i -> fun (_,ow,succs,_,_) -> if ow = player then str_set strategy i (ns_first (pg_get_successors game i))) game;
    (solution, strategy);;




(* Takes a game in which only player pl can make decisions (the algorithm doesn't distinguish positions for player pl and player 1 - pl.
   Computes winning sets for both players and strategy for player pl *)
let compute_winning_nodes_for_direct (game: paritygame) pl =
	let compute_strat = true in
	
	let subnodes_by_list game li =
	  let small_to_big = Array.of_list li in
	  let big_to_small = ref TreeMap.empty_def in
	  Array.iteri (fun small big ->
		       big_to_small := TreeMap.add big small !big_to_small
		      ) small_to_big;
	  let big_to_small = !big_to_small in
	  Array.init (Array.length small_to_big) (fun small ->
						  let big = small_to_big.(small) in
						  let pr = pg_get_priority game big in
						  let pl = pg_get_owner game big in
						  let delta_big = pg_get_successors game big in
						  let delta_small = ref TreeSet.empty_def in
						  ns_iter (fun succ_big ->
							   try
							     delta_small := TreeSet.add (TreeMap.find succ_big big_to_small) !delta_small;
							   with
							   | Not_found -> ()
							  ) delta_big;
						  (pr, pl, delta_small)
						 ) 
	in
	let list_attractor transp getpl strat source_set_ref todo =
    	  while not (Queue.is_empty todo) do
    	    let x = Queue.take todo in
            transp x (fun y -> if ns_elem y !source_set_ref then (
            			 source_set_ref := ns_del y !source_set_ref;
				 Queue.add y todo;
				 if (getpl y = pl) && (str_get strat y = nd_undef)
				 then str_set strat y x
			       )
		     )
    	  done
	in
	
	let solve_scc comp pl =
	  let game' = subnodes_by_list game (ns_nodes comp) in
	  let strat = str_make (if compute_strat then Array.length game' else 0) in
	  let new_edges = Queue.create () in
	  Array.iteri (fun i (_, _, delta) -> TreeSet.iter (fun j -> Queue.add (i, j) new_edges) !delta) game';
	  let winner = ref (plr_opponent pl) in
	  while (!winner != pl) && not (Queue.is_empty new_edges) do
            let (u, v) = Queue.take new_edges in
            if u = v
            then let (pr_u, _, _) = game'.(u) in (
		   if prio_good_for_player pr_u pl then (
        	     winner := pl;
        	     if compute_strat then (
        	       let (_, _, delta_u) = game'.(u) in
                       let game'' = subnodes_by_list game (ns_nodes comp) in
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
        		   if pl' = pl then str_set strat x (TreeSet.min_elt inters);
        		   if x = u then finished := true;
        		 )
        	       done;
        	       let s = ref ns_empty in
        	       for i = 0 to (Array.length game') - 1 do
        		 if not (TreeSet.mem i !target_set)
        		 then s := ns_add i !s
        	       done;
        	       let todo = Queue.create () in
        	       TreeSet.iter (fun x -> Queue.add x todo) !target_set;
                       list_attractor (fun x f -> List.iter f g.(x)) (fun i -> let (_, pl, _) = game''.(i) in pl) strat s todo
        	     )
		   )
		 )
            else let (pr_u, _, delta_u) = game'.(u) in
		 let (pr_v, _, delta_v) = game'.(v) in
		 if (prio_good_for_player pr_u pl) && (pr_u >= pr_v)
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
	let strategy = str_make (if compute_strat then n else 0) in
	
	let max_prio_queue comp pr =
	  let q = Queue.create () in
	  ns_iter (fun x ->
    		     if pg_get_priority game x = pr then Queue.add x q
    		    ) comp;
	  q
	in
	
	let (sccs, sccindex, topology, roots) = strongly_connected_components game in
	let marked = Array.make (Array.length sccs) plr_undef in
	
	let max_prio_for l pl =
	  ns_fold (fun p el ->
			  let pr = pg_get_priority game el in
			  if prio_good_for_player pr pl then max p pr else p
			 ) (-1) l
	in
	
	let rec process_root r =
	  let getpl x = pg_get_owner game x in
	  if marked.(r) = plr_undef then (
            List.iter process_root topology.(r);
            let c = List.fold_left (fun c c' -> if c != -1 && marked.(c) = pl then c else c') (-1) topology.(r) in
            if c != -1 && marked.(c) = pl
            then (
              marked.(r) <- pl;
              if compute_strat
              then list_attractor (fun x f -> ns_iter f (pg_get_predecessors game x)) getpl strategy (ref sccs.(r)) (QueueUtils.of_list (ns_nodes sccs.(c)));
            )
            else let comp = sccs.(r) in
		 if ns_size comp = 1
		 then let x = ns_first comp in
		      let pr = pg_get_priority game x in
		      let pl' = pg_get_owner game x in
		      let delta = pg_get_successors game x in
                      if ns_size delta = 0
                      then marked.(r) <- plr_opponent pl'
                      else if (ns_exists (fun y -> x = y) delta) && (prio_good_for_player pr pl)
                      then (
			marked.(r) <- pl;
			if (compute_strat && getpl x = pl) then str_set strategy x x
                      )
                      else marked.(r) <- plr_opponent pl
		 else let pl_max = max_prio_for comp pl in
                      let plo_max = max_prio_for comp (plr_opponent pl) in
                      if pl_max > plo_max
                      then (
			marked.(r) <- pl;
			if compute_strat
			then (
			  list_attractor (fun x f -> ns_iter f (pg_get_predecessors game x)) getpl strategy (ref sccs.(r)) (max_prio_queue comp pl_max);
			  ns_iter (fun q ->
				     let pl' = pg_get_owner game q in
				     let d = pg_get_successors game q in
                      		     if (pl' = pl) then
				       ns_iter (fun di ->
						if (str_get strategy q = nd_undef) then (
						  if sccindex.(di) = r then str_set strategy q di
						);
					       ) d;
                      		    ) comp
			)
                      )
                      else marked.(r) <- if pl_max < 0
					 then plr_opponent pl
					 else let (winner, strat) = solve_scc comp pl in (
						if compute_strat && (winner = pl) then (
						  let arr = Array.of_list (ns_nodes comp) in
						  for i = 0 to (Array.length arr) - 1 do
                                         	    if str_get strat i <> nd_undef then str_set strategy arr.(i) arr.(str_get strat i)
						  done;
						);
						winner
                                              )
	  )
	in
	
	List.iter (fun r -> let _ = process_root r in ()) roots;
	let solution = sol_make n in
	for i = 0 to (Array.length sccs) - 1 do
	  ns_iter (fun u -> sol_set solution u marked.(i)) sccs.(i)
	done;
	(solution, strategy);;
  
  
let compute_winning_nodes_direct game strat pl =
  let sol = fst (compute_winning_nodes_for_direct (subgame_by_strat game strat) (plr_opponent pl)) in
  let l = ref [] in
  sol_iter (fun i -> fun pl' -> if pl' = pl then l := i::!l) sol;
  !l;;
  
  
