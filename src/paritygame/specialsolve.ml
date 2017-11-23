open Basics;;
open Tcsset;;
open Tcsqueue;;
open Paritygame;;
open Pgnode;;
open Pgnodeset;;
open Pgplayer;;
open Pgpriority;;
open Pgsolution;;
open Arrayparitygame;;
open Pgstrategy;;



type compact_sol_strat = (node * player * node) list;;

let compact_sol_strat_to_sol_strat game comp =
	let n = game#size in
	let sol = new array_solution n in
	let strat = new array_strategy n in
	List.iter (fun (i, pl, j) -> sol#set i pl;
				     strat#set i j
		  ) comp;
	(sol, strat);;



let find_useful_self_cycles game =
    let ret = ref [] in
    game#iterate (fun i -> fun (pr,pl,succs,_,_) -> if prio_good_for_player pr pl then (
						    ns_iter (fun j ->
							     if j = i then ret := (i, pl, i)::!ret
							    ) succs
						  )
	       );
    !ret;;


let solve_cycle_scc game =
    let n = game#size in
    let max_pr_node = game#get_max_prio_node in
    let max_pr_pl = plr_benefits (game#get_priority max_pr_node) in
    let solution = sol_init game (fun _ -> max_pr_pl) in
    let strategy = new array_strategy n in

    if game#get_owner max_pr_node = max_pr_pl
    then strategy#set (max_pr_node) (ns_some (game#get_successors max_pr_node));
    let _ = game#attr_closure_inplace strategy max_pr_pl (ns_make [max_pr_node]) in
    (solution, strategy)



let solve_single_player_scc game player =
  let n = game#size in
  let strategy = new array_strategy n in
  let temp_strat = new array_strategy n in
  let solution = new array_solution n in
  
  let clear_visited _ = temp_strat#iter (fun v _ -> temp_strat#set v nd_undef) in
  
  let can_reach_itself v pr =
    let queue = SingleOccQueue.create () in
    let rec can_reach _ = if SingleOccQueue.is_empty queue then
			     false
			   else
			     begin
			       let (w,u) = SingleOccQueue.take queue in
			       if temp_strat#get w != nd_undef || (let pr' = game#get_priority w in pr' > pr && not (prio_good_for_player pr' player)) then
				 can_reach ()
			       else
				 begin
				   temp_strat#set w u;
				   if w=v then
				     begin
				       temp_strat#iter (fun i w -> if w != nd_undef then (
							      solution#set i player;
							      if game#get_owner i = player then strategy#set i (temp_strat#get i))
						);
				       true
				     end
				   else
				     begin
				       ns_iter (fun x -> SingleOccQueue.add (x,w) queue) (game#get_predecessors w);
				       can_reach ()
				     end
				 end
			     end
    in
    clear_visited ();
    ns_iter (fun w -> SingleOccQueue.add (w,v) queue) (game#get_predecessors v);
    can_reach ()
  in

  let complete_solution_and_strategy _ =
    clear_visited ();
    let queue = SingleOccQueue.create () in
    solution#iter (fun v -> fun pl -> if pl != plr_undef then (
				   ns_iter (fun w -> if solution#get w = plr_undef then SingleOccQueue.add (w,v) queue) (game#get_predecessors v))
	     );
    while not (SingleOccQueue.is_empty queue) do
      let (w,v) = SingleOccQueue.take queue in
      solution#set w player;
      if game#get_owner w = player then strategy#set w v;
      ns_iter (fun u -> if solution#get u = plr_undef then SingleOccQueue.add (u,w) queue) (game#get_predecessors w)
    done
  in

  let good_prios = ref (List.sort (fun p -> fun q -> (-1) * (compare p q)) (game#get_selected_priorities (fun pr -> prio_good_for_player pr player))) in
  let found = ref false in
  while not !found && !good_prios != [] do
    let pr = List.hd !good_prios in
    good_prios := List.tl !good_prios;

    let vs = game#get_prio_nodes pr in
    found := ns_fold (fun b -> fun v -> b || can_reach_itself v pr) false vs;
  done;
  (if !found then
    complete_solution_and_strategy ()
   else
     begin
       let pl' = plr_opponent player in 
       solution#iter (fun i -> fun _ -> solution#set i pl';
				   if game#get_owner i = pl' then strategy#set i (ns_first (game#get_successors i))
		)
     end);
  (solution, strategy)

      
let solve_single_parity_scc game player =
    let solution = sol_init game (fun _ -> player) in
    let strategy = new array_strategy game#size in
    game#iterate (fun i -> fun (_,ow,succs,_,_) -> if ow = player then strategy#set i (ns_first (game#get_successors i)));
    (solution, strategy);;




(* Takes a game in which only player pl can make decisions (the algorithm doesn't distinguish positions for player pl and player 1 - pl.
   Computes winning sets for both players and strategy for player pl *)
let compute_winning_nodes_for_direct (game: paritygame) pl =
	let compute_strat = true in
	let list_attractor transp getpl strat source_set_ref todo =
    	  while not (Queue.is_empty todo) do
    	    let x = Queue.take todo in
            transp x (fun y -> if ns_elem y !source_set_ref then (
            			 source_set_ref := ns_del y !source_set_ref;
				 Queue.add y todo;
				 if (getpl y = pl) && (strat#get y = nd_undef)
				 then strat#set y x
			       )
		     )
    	  done
	in
	
	let solve_scc comp pl =
	  let (game', map1, map2) = game#subgame_by_list comp in
	  let strat = new array_strategy (if compute_strat then game'#size else 0) in
	  let new_edges = Queue.create () in
	  game'#edge_iterate (fun i j -> Queue.add (i, j) new_edges);
	  let winner = ref (plr_opponent pl) in
	  while (!winner != pl) && not (Queue.is_empty new_edges) do
            let (u, v) = Queue.take new_edges in
            if u = v
            then let pr_u = game'#get_priority u in (
		   if prio_good_for_player pr_u pl then (
        	     winner := pl;
        	     if compute_strat then (
        	       let delta_u = game'#get_successors u in
                       let (game'', _, _) = game#subgame_by_list comp in
                       let g = ref TreeMap.empty_def in
                       game''#edge_iterate (fun i w ->
                        try
                          g := TreeMap.add w (i::TreeMap.find w !g) !g
                        with Not_found ->
                          g := TreeMap.add w [i] !g
                       );
        	       let target_set = ref (ns_make [u]) in
        	       let todo = Queue.create () in
        	       ns_iter (fun x ->
        			     let pr_x = game'#get_priority x in
        			     if pr_x <= pr_u
        			     then Queue.add x todo
        			    ) delta_u;
        	       let finished = ref false in
        	       while not !finished do
        		 let x = Queue.take todo in
        		 let pl' = game''#get_owner x in
        		 let d = game''#get_successors x in
        		 let inters = ns_inter d !target_set in
        		 if ns_isEmpty inters
        		 then Queue.add x todo
        		 else (
        		   target_set := ns_add x !target_set;
        		   if pl' = pl then strat#set x (ns_some inters);
        		   if x = u then finished := true
        		 )
        	       done;
        	       let s = ref ns_empty in
        	       game'#iterate (fun i _ ->
                     if not (ns_elem i !target_set)
                     then s := ns_add i !s
                    );
        	       let todo = Queue.create () in
        	       ns_iter (fun x -> Queue.add x todo) !target_set;
                       list_attractor (fun x f -> List.iter f (TreeMap.find x !g)) game''#get_owner strat s todo
        	     )
		   )
		 )
            else let (pr_u, _, delta_u, _, _) = game'#get_node u in
		 let (pr_v, _, delta_v, _, _) = game'#get_node v in
		 if (prio_good_for_player pr_u pl) && (pr_u >= pr_v)
		 then ns_iter (fun w ->
        		 	    if not (ns_elem w delta_u) then (
				      Queue.add (u, w) new_edges;
				      game'#add_edge u w
				    )
        			   ) delta_v
	  done;
	  (!winner, strat, map1, map2)
	in
	
	let n = game#size in
	let strategy = new array_strategy (if compute_strat then n else 0) in
	
	let max_prio_queue comp pr =
	  let q = Queue.create () in
	  ns_iter (fun x ->
    		     if game#get_priority x = pr then Queue.add x q
    		    ) comp;
	  q
	in
	
	let (sccs, sccindex, topology, roots) = game#strongly_connected_components in
	let marked = Array.make (Array.length sccs) plr_undef in
	
	let max_prio_for l pl =
	  ns_fold (fun p el ->
			  let pr = game#get_priority el in
			  if prio_good_for_player pr pl then max p pr else p
			 ) (-1) l
	in
	
	let rec process_root r =
	  let getpl x = game#get_owner x in
	  if marked.(r) = plr_undef then (
            List.iter process_root topology.(r);
            let c = List.fold_left (fun c c' -> if c != -1 && marked.(c) = pl then c else c') (-1) topology.(r) in
            if c != -1 && marked.(c) = pl
            then (
              marked.(r) <- pl;
              if compute_strat
              then list_attractor (fun x f -> ns_iter f (game#get_predecessors x)) getpl strategy (ref sccs.(r)) (QueueUtils.of_list (ns_nodes sccs.(c)));
            )
            else let comp = sccs.(r) in
		 if ns_size comp = 1
		 then let x = ns_first comp in
		      let pr = game#get_priority x in
		      let pl' = game#get_owner x in
		      let delta = game#get_successors x in
                      if ns_size delta = 0
                      then marked.(r) <- plr_opponent pl'
                      else if (ns_exists (fun y -> x = y) delta) && (prio_good_for_player pr pl)
                      then (
			marked.(r) <- pl;
			if (compute_strat && getpl x = pl) then strategy#set x x
                      )
                      else marked.(r) <- plr_opponent pl
		 else let pl_max = max_prio_for comp pl in
                      let plo_max = max_prio_for comp (plr_opponent pl) in
                      if pl_max > plo_max
                      then (
			marked.(r) <- pl;
			if compute_strat
			then (
			  list_attractor (fun x f -> ns_iter f (game#get_predecessors x)) getpl strategy (ref sccs.(r)) (max_prio_queue comp pl_max);
			  ns_iter (fun q ->
				     let pl' = game#get_owner q in
				     let d = game#get_successors q in
                      		     if (pl' = pl) then
				       ns_iter (fun di ->
						if (strategy#get q = nd_undef) then (
						  if (sccindex di) = r then strategy#set q di
						);
					       ) d;
                      		    ) comp
			)
                      )
                      else marked.(r) <- if pl_max < 0
					 then plr_opponent pl
					 else let (winner, strat, _, innerToOuter) = solve_scc comp pl in (
						if compute_strat && (winner = pl) then (
						strat#iter (fun i j ->
						    if j <> nd_undef then strategy#set (innerToOuter i) (innerToOuter j)
						)
						);
						winner
                                              )
	  )
	in
	
	List.iter (fun r -> let _ = process_root r in ()) roots;
	let solution = new array_solution n in
	for i = 0 to (Array.length sccs) - 1 do
	  ns_iter (fun u -> solution#set u marked.(i)) sccs.(i)
	done;
	(solution, strategy);;
  
  
let compute_winning_nodes_direct game strat pl =
  let sol = fst (compute_winning_nodes_for_direct (game#subgame_by_strat (new array_pg game#size) strat) (plr_opponent pl)) in
  let l = ref [] in
  sol#iter (fun i -> fun pl' -> if pl' = pl then l := i::!l);
  !l;;
  
  
