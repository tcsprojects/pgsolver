open Basics;;
open Tcsarray;;
open Paritygame;;
open Univsolve;;
open Specialsolve;;


let verify_solution_strategy_custom (game: paritygame) (sol: solution) (strat: strategy) compute_winning_n =
	let build_cycle game strategy node =
		let marker = Array.make (Array.length game) false in
		let rec helper node =
			if marker.(node) then [node] else (
			     marker.(node) <- true;
				 if strategy.(node) >= 0
				 then node::(helper strategy.(node))
				 else node::(helper (pg_get_tr game node).(0))
			)
		in
		let rec getprio node l pr =
			let (h, t) = (List.hd l, List.tl l) in
			let pr = max pr (pg_get_pr game h) in
			if h = node
			then pr
			else getprio node t pr
		in
			let l = helper node in
			let lr = List.rev l in
				(l, getprio (List.hd lr) (List.tl lr) 0)
	in

	let n = Array.length game in

	let rec sanity_check i =
		if i >= n then None else (
			let (_, pl, delta, _) = game.(i) in
			if sol.(i) < -1 || sol.(i) > 1
			then Some ([i], "Solution for node " ^ string_of_int i ^ " is corrupt (" ^ string_of_int sol.(i) ^ ")")
			else if strat.(i) < -1 || (strat.(i) != -1 && not (ArrayUtils.exists delta (fun _ j -> j = strat.(i))))
			then Some ([i], "Strategy for node " ^ string_of_int i ^ " is corrupt (" ^ string_of_int strat.(i) ^ ")")
			else if sol.(i) < 0
			then if strat.(i) >= 0
				 then Some (i::[strat.(i)], "Strategy for node " ^ string_of_int i ^ " is defined (" ^ string_of_int strat.(i) ^ ") but solution is not")
				 else sanity_check (i + 1)
			else if sol.(i) != pl
			then if strat.(i) >= 0
				 then Some (i::[strat.(i)], "Strategy for node " ^ string_of_int i ^ " is defined (" ^ string_of_int strat.(i) ^ ") but node is not in the winning set of player " ^ string_of_int pl)
				 else sanity_check (i + 1)
			else if strat.(i) >= 0
				 then if sol.(strat.(i)) != sol.(i)
				 	  then Some(i::[strat.(i)], "Strategy for node " ^ string_of_int i ^ " leads to node " ^ string_of_int strat.(i) ^ " which is out of the winning set of player " ^ string_of_int pl)
				 	  else sanity_check (i + 1)
				 else Some (i::[strat.(i)], "Strategy for node " ^ string_of_int i ^ " is undefined (" ^ string_of_int strat.(i) ^ ")")
		)
	in

	let arrf f a =
      let i = ref 0 in
      let found = ref (-1) in
      while !found = -1 && !i < Array.length a do
        if f a.(!i) then found := !i;
        incr i
      done;
      if !found = -1 then None else Some !found
    in

	let sophisticated_check pl =
		let strat' = Array.make n (-1) in
		let badnodes = ref [] in
		for i = 0 to n - 1 do
			let (_, pl', _, _) = game.(i) in
			if pl' = pl	then strat'.(i) <- strat.(i);
			if sol.(i) != pl then badnodes := i::!badnodes
		done;
		let game' = subgame_by_strat game strat' in
		pg_remove_nodes game' !badnodes;
		let (sol', strat'') = compute_winning_n game' (1 - pl) in
		match (arrf (fun entry -> entry = 1 - pl) sol') with
		|	None -> None
		|	Some i -> let (cycle, pr) = build_cycle game' strat'' i in
				      Some (cycle, "Cycle winner failure - " ^ (string_of_int (1 - pl)) ^ " wins with priority " ^ string_of_int pr ^ " but " ^ (string_of_int pl) ^ " should...")
	in

	match (sanity_check 0) with
		Some err -> Some err
	|	None -> match (sophisticated_check 0) with
					Some err -> Some err
		     	|   None -> sophisticated_check 1;;










let verify_solution_strategy_univ gm sol strat = verify_solution_strategy_custom gm sol strat (fun g _ -> universal_solve_trivial verbosity_level_default g);;







let verify_solution_strategy_direct gm sol strat = verify_solution_strategy_custom gm sol strat compute_winning_nodes_for_direct;;














let verify_solution_strategy_generic (game: paritygame) (sol: solution) (strat: strategy) =
	let message _ _ = () in
	let n = Array.length game in
	(* 0 = no information, 1 = currently tracking, 2 = marked during cycle check, 3 = verified *)
	let table = Array.make n 0 in

	let has arr el =
		let rec has' arr el i =
			(i < Array.length arr) && (arr.(i) = el || has' arr el (i + 1))
		in
			has' arr el 0
	in

	let wins_cycle i trace pl =
		let rec helper i pr l =
			let h = List.hd l in
			let (pr', _, _, _) = game.(h) in
			let maxpr = max pr pr' in
				if h = i
				then maxpr mod 2 = pl
				else (
					table.(h) <- 2;
					helper i maxpr (List.tl l)
				)
		in
			helper i 0 trace
	in

	let rec expand pl i =
		message 3 (fun _ -> "   Expanding node " ^ string_of_int i ^ "\n");
		if table.(i) != 3 then (
			let (_, pl', delta, _) = game.(i) in
			table.(i) <- 3;
			if pl = pl'
			then expand pl (strat.(i))
			else Array.iter (expand pl) delta
		)
	in

	let rec test pl i trace =
		message 3 (fun _ -> "   Testing node " ^ string_of_int i ^
		                    " current table entry " ^ string_of_int table.(i) ^ "\n");
		if table.(i) = 3
	    then None
		else if (table.(i) = 1) || (table.(i) = 2)
		then (
			let res = wins_cycle i trace pl in
			message 3 (fun _ -> "   Wins cycle? " ^ (if res then "Yes" else "No") ^ "\n");
			if res
			then None
			else Some (i::trace, "Cycle winner failure - " ^ (string_of_int (1 - pl)) ^ " wins but " ^ (string_of_int pl) ^ " should...")
		)
		else let (pr, pl', delta, _) = game.(i) in
		     if (pr < 0) || (sol.(i) < 0)
		     then Some (i::trace, "Reached undefined position starting in a defined position")
		     else if sol.(i) != pl
		     then Some (i::trace, "Reached winning set of " ^ (string_of_int sol.(i)) ^ " starting in winning set of " ^ (string_of_int pl) ^ " following the strategy of " ^ (string_of_int pl))
		     else if pl' = pl
		     then (
					if not (has delta strat.(i))
					then Some (i::trace, "Strategy failure at the end of the trace w.r.t. player " ^ (string_of_int pl))
					else (
                        table.(i) <- 1;
                        match (test pl strat.(i) (i::trace)) with
                          Some err -> Some err
                        | None -> (
                        	if table.(i) != 2
                        	then expand pl i
                        	else table.(i) <- 0;
                        	None
                        )
					)
		     )
		     else (
		     	table.(i) <- 1;
		     	match (test_list pl delta 0 (i::trace)) with
                          Some err -> Some err
                        | None -> (
                        	if table.(i) != 2
                        	then expand pl i
                        	else table.(i) <- 0;
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

	let rec testnext i =
		message 3 (fun _ -> "   Iterating node " ^ string_of_int i ^
		                    " current table entry " ^ string_of_int table.(i) ^ "\n");
		if i >= n
		then None
		else if table.(i) = 3
		then testnext (i + 1)
		else let (pr, pl, _, _) = game.(i) in
				if (pr < 0) || (sol.(i) < 0)
				then testnext (i + 1)
				else match (test sol.(i) i []) with
                   None -> (expand sol.(i) i;
                            testnext (i + 1))
                 | Some (tr, s) -> Some (List.rev tr, s)
	in
		testnext 0;;









let verify_solution_strategy = verify_solution_strategy_univ;;