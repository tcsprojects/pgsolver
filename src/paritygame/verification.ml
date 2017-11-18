open Basics;;
open Tcsarray;;
open Paritygame;;
open Univsolve;;
open Specialsolve;;
open Pgnode;;
open Pgnodeset;;
open Pgplayer;;
open Pgpriority;;
open Arrayparitygame;;
open Pgsolution;;
open Pgstrategy;;
open Tcsset;;

type verifier = paritygame -> solution -> strategy -> (node list * string) option

let verify_solution_strategy_custom (game: paritygame) (sol: solution) (strat: strategy) compute_winning_n =
	let build_cycle game (strategy: strategy) node =
		let marker = ref TreeSet.empty_def in
		let rec helper node =
			if TreeSet.mem node !marker then [node] else (
			     marker := TreeSet.add node !marker;
				 if strategy#get node != nd_undef
				 then node::(helper (strategy#get node))
				 else node::(helper (ns_some (game#get_successors node)))
			)
		in
		let rec getprio node l pr =
			let (h, t) = (List.hd l, List.tl l) in
			let pr = max pr (game#get_priority h) in
			if h = node
			then pr
			else getprio node t pr
		in
			let l = helper node in
			let lr = List.rev l in
				(l, getprio (List.hd lr) (List.tl lr) 0)
	in

	let n = game#size in

	let sanity_check_node i =
	    let pl = game#get_owner i in
	    let delta = game#get_successors i in
	    (* if sol.(i) < -1 || sol.(i) > 1
			then Some ([i], "Solution for node " ^ string_of_int i ^ " is corrupt (" ^ string_of_int sol.(i) ^ ")")
			else *)
	    if (strat#get i != nd_undef && not (ns_elem (strat#get i) delta))
	    then Some ([i], "Strategy for node " ^ nd_show i ^ " is corrupt (" ^ nd_show (strat#get i) ^ ")")
	    else if sol#get i = plr_undef
	    then if strat#get i != nd_undef
		 then Some (i::[strat#get i], "Strategy for node " ^ nd_show i ^ " is defined (" ^ (nd_show (strat#get i)) ^ ") but solution is not")
		 else None
	    else if sol#get i != pl
	    then if strat#get i != nd_undef
		 then Some (i::[strat#get i], "Strategy for node " ^ nd_show i ^ " is defined (" ^ (nd_show (strat#get i)) ^ ") but node is not in the winning set of player " ^ plr_show pl)
		 else try
                     let j = ns_find (fun j -> sol#get j != sol#get i) delta in
                     Some ([i;j], "Node " ^ nd_show i ^ " can escape to " ^ nd_show j ^ " from the winning set of player " ^ plr_show pl)
          	   with Not_found ->
                     None
	    else if strat#get i != nd_undef
	    then if sol#get (strat#get i) != sol#get i
		 then Some(i::[strat#get i], "Strategy for node " ^ nd_show i ^ " leads to node " ^ (nd_show (strat#get i)) ^ " which is out of the winning set of player " ^ plr_show pl)
		 else None
	    else Some (i::[strat#get i], "Strategy for node " ^ nd_show i ^ " is undefined (" ^ (nd_show (strat#get i)) ^ ")")
	in

	let sanity_check =
	  let result = ref None in
	  game#iterate (fun i _ ->
	    if !result = None
	    then result := sanity_check_node i
	  );
	  !result
    in

	let sophisticated_check pl =
		let strat' = new array_strategy n in
		let badnodes = ref ns_empty in
		game#iterate (fun i (_, pl', _, _, _) ->
			if pl' = pl	then strat'#set i (strat#get i);
			if sol#get i != pl then badnodes := ns_add i !badnodes
		);
		let game' = game#subgame_by_strat (new array_pg game#size) strat' in
		game'#remove_nodes !badnodes;
		let op = plr_opponent pl in
		let (sol', strat'') = compute_winning_n game' op in
		match (sol'#find (fun _ entry -> entry = op)) with
		|	None -> None
		|	Some i -> let (cycle, pr) = build_cycle game' strat'' i in
				      Some (cycle, "Cycle winner failure - " ^ plr_show op ^ " wins with priority " ^ string_of_int pr ^ " but " ^ plr_show pl ^ " should...")
	in

	match sanity_check with
		Some err -> Some err
	|	None -> match (sophisticated_check plr_Even) with
					Some err -> Some err
		     	|   None -> sophisticated_check plr_Odd;;
























let verify_solution_strategy_generic (game: paritygame) (sol: solution) (strat: strategy) =
	let message _ _ = () in
	(* 0 = no information, 1 = currently tracking, 2 = marked during cycle check, 3 = verified *)
	let table = ref TreeMap.empty_def in

	let has arr el =
		let rec has' arr el i =
			(i < Array.length arr) && (arr.(i) = el || has' arr el (i + 1))
		in
			has' arr el 0
	in

	let wins_cycle i trace pl =
		let rec helper i pr l =
			let h = List.hd l in
			let maxpr = max pr (game#get_priority h) in
				if h = i
				then prio_good_for_player maxpr pl
				else (
					table := TreeMap.add h 2 !table;
					helper i maxpr (List.tl l)
				)
		in
			helper i 0 trace
	in

	let rec expand pl i =
		message 3 (fun _ -> "   Expanding node " ^ nd_show i ^ "\n");
		if not (TreeMap.find_opt i !table = Some 3) then (
			table := TreeMap.add i 3 !table;
			if pl = game#get_owner i
			then expand pl (strat#get i)
			else ns_iter (expand pl) (game#get_successors i)
		)
	in

	let rec test pl i trace =
		message 3 (fun _ -> "   Testing node " ^ nd_show i ^
		                    " current table entry " ^ string_of_int (match TreeMap.find_opt i !table with Some x -> x | None -> 0) ^ "\n");
		if TreeMap.find_opt i !table = Some 3
	    then None
		else if (TreeMap.find_opt i !table = Some 1) || (TreeMap.find_opt i !table = Some 2)
		then (
			let res = wins_cycle i trace pl in
			message 3 (fun _ -> "   Wins cycle? " ^ (if res then "Yes" else "No") ^ "\n");
			if res
			then None
			else Some (i::trace, "Cycle winner failure - " ^ plr_show (plr_opponent pl) ^ " wins but " ^ plr_show pl ^ " should...")
		)
		else let delta = game#get_successors i in
		     if not (game#is_defined i) || (sol#get i = plr_undef)
		     then Some (i::trace, "Reached undefined position starting in a defined position")
		     else if sol#get i != pl
		     then Some (i::trace, "Reached winning set of " ^ plr_show (sol#get i) ^ " starting in winning set of " ^ plr_show pl ^ " following the strategy of " ^ plr_show pl)
		     else if game#get_owner i = pl
		     then (
					if not (has (Array.of_list (ns_nodes delta )) (strat#get i))
					then Some (i::trace, "Strategy failure at the end of the trace w.r.t. player " ^ plr_show pl)
					else (
                        table := TreeMap.add i 1 !table;
                        match (test pl (strat#get i) (i::trace)) with
                          Some err -> Some err
                        | None -> (
                        	if not (TreeMap.find_opt i !table = Some 2)
                        	then expand pl i
                        	else table := TreeMap.remove i !table;
                        	None
                        )
					)
		     )
		     else (
		     	table := TreeMap.add i 1 !table;
		     	match (test_list pl (Array.of_list (ns_nodes delta)) 0 (i::trace)) with
                          Some err -> Some err
                        | None -> (
                        	if not (TreeMap.find_opt i !table = Some 2)
                        	then expand pl i
                        	else table := TreeMap.remove i !table;
                        	None
                        )
		     )
	and test_list pl delta j trace =
		if j >= Array.length delta
		then None
		else match (test pl delta.(j) trace) with
			   Some err -> Some err
			 | None -> test_list pl delta (j + 1) trace
	in

	let testnextnode i =
		message 3 (fun _ -> "   Iterating node " ^ nd_show i ^
		                    " current table entry " ^ string_of_int (match TreeMap.find_opt i !table with Some x -> x | None -> 0) ^ "\n");
		if TreeMap.find_opt i !table = Some 3
		then None
		else
				if not (game#is_defined i) || (sol#get i = plr_undef)
				then None
				else match (test (sol#get i) i []) with
                   None -> (expand (sol#get i) i;
                            None)
                 | Some (tr, s) -> Some (List.rev tr, s)
	in

  let result = ref None in
  game#iterate (fun i _ ->
    if !result = None
    then result := testnextnode i
  );
  !result








let verify_solution_strategy_univ gm (sol: solution) (strat: strategy) = verify_solution_strategy_custom gm sol strat (fun g _ -> universal_solve_trivial verbosity_level_default g);;
let verify_solution_strategy_direct gm sol strat = verify_solution_strategy_custom gm sol strat compute_winning_nodes_for_direct;;

let verify_solution_strategy = verify_solution_strategy_univ;;
