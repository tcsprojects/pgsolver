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
        let (pr, pl, delta, _) = game.(i) in
            if pr mod 2 = pl then (
                for j = 0 to Array.length delta - 1 do
                    if delta.(j) = i then ret := (i, pl, i)::!ret
                done
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
    then strategy.(max_pr_node) <- (pg_get_tr game max_pr_node).(0);
    let _ = attr_closure_inplace game strategy max_pr_pl [max_pr_node] in
    (solution, strategy)

(*
let solve_single_player_scc game player =
    let n = pg_size game in
    let otherplayer = 1 - player in
    let max_pr_pl_node = pg_max_rew_node_for game player in
    let max_pr_ot_node = pg_max_rew_node_for game otherplayer in
    let max_pr_pl = pg_get_pr game max_pr_pl_node in
    let max_pr_ot = pg_get_pr game max_pr_ot_node in

    if max_pr_pl > max_pr_ot then (
        let solution = Array.make n player in
        let strategy = Array.make n (-1) in
        if pg_get_pl game max_pr_pl_node = player
        then strategy.(max_pr_pl_node) <- (pg_get_tr game max_pr_pl_node).(0);
        let _ = attr_closure_inplace game strategy player [max_pr_pl_node] in
        (solution, strategy)
    )
    else (
        let solution = Array.make n (-1) in
        let strategy = Array.make n (-1) in
        let deltaTbl = Array.make n (ref IntMap.empty) in
        for i = 0 to n - 1 do
            let (_, _, delta, _) = game.(i) in
            let ht = ref IntMap.empty in
            Array.iter (fun i -> ht := IntMap.add i (-1) !ht) delta;
            deltaTbl.(i) <- ht
        done;
        let new_edges = Queue.create () in
        Array.iteri (fun i (_, _, delta, _) -> Array.iter (fun j -> Queue.add (i, j) new_edges) delta) game;
        let winner = ref (1 - player) in
        while (!winner != player) && not (Queue.is_empty new_edges) do
            let (u, v) = Queue.take new_edges in
            if u != v then (
                let (pr_u, _, _, _) = game.(u) in
                let (pr_v, _, _, _) = game.(v) in
                let delta_u = deltaTbl.(u) in
                let delta_v = deltaTbl.(v) in
                if (pr_u mod 2 = player) && (pr_u >= pr_v)
                then IntMap.iter (fun w r ->
                        if not (IntMap.mem w !delta_u) then (
                            Queue.add (u, w) new_edges;
                            delta_u := IntMap.add w v !delta_u
                         )
                     ) !delta_v
            )
            else let (pr_u, _, _, _) = game.(u) in
                 if pr_u mod 2 = player then (
                    winner := player;
                    let cycle = ref [] in
                    let rec expand fromnode tonode =
                        if solution.(fromnode) != player then (
                            let fromdelta = deltaTbl.(fromnode) in
                            let reason = IntMap.find tonode !fromdelta in
                            if reason = -1 then (
                                let (_, pl, _, _) = game.(fromnode) in
                                solution.(fromnode) <- player;
                                cycle := fromnode::!cycle;
                                if pl = player
                                then strategy.(fromnode) <- tonode
                            )
                            else (
                                expand fromnode reason;
                                expand reason tonode
                            )
                        )
                    in
                    expand u u;
			        let attr = attr_closure_inplace game strategy player !cycle in
   			        List.iter (fun i -> solution.(i) <- player) attr
                 )
        done;
        if (!winner != player) then (
            for i = 0 to n - 1 do
                let (_, pl, delta, _) = game.(i) in
                solution.(i) <- !winner;
                if pl = !winner
                then strategy.(i) <- delta.(0)
            done
        );
        (solution, strategy)
    );;
*)

(*
let solve_single_player_scc game player =
  let gm = Array.copy game in
  let tr = game_to_transposed_graph gm in
  let n = pg_size game in
  let strategy = Array.make n (-1) in
  let dummy = Array.make n (-1) in
  let sccs = ref [(collect_nodes game (fun _ -> fun _ -> true))] in

  let greatest_nodes scc =
    let vs = ref [] in
    let p = ref (-1) in
    List.iter (fun v -> let pr = pg_get_pr gm v in
                        if pr = !p then
                          vs := v :: !vs
                        else if pr > !p then
                          begin
                            vs := [v];
                            p := pr
                          end
                        else
                          ()) scc;
    (!vs,!p)
  in

  let subgame vs =
    let g = pg_create n in
    List.iter (fun v -> let pr = pg_get_pr gm v in
                        let pl = pg_get_pl gm v in
                        let tr = pg_get_tr gm v in
                        pg_set_node g v pr pl tr None) vs;
    List.iter (fun v -> let tr = Array.of_list (List.filter (fun w -> pg_get_pr g w > -1)
                                                            (Array.to_list (pg_get_tr g v)))
                        in
                        pg_set_tr g v tr) vs;
    g
  in

  let is_proper_scc =
   function [v] -> let ws = pg_get_tr gm v in
                   array_exists ws (fun _ w -> v = w)
          | _   -> true
  in

  let rec check_for_player_in_next_scc _ =
    if !sccs = [] then
      begin
        (* no (recusively constructed) SCC in which player wins, therefore the opponent wins
           everywhere with no strategy at all (because he has nothing to choose) *)
        let opponent = 1-player in
        let solution = Array.make n opponent in
        (* let strategy = Array.make n (-1) in *)
        for i=0 to n-1 do
          let pl = pg_get_pl game i in
          strategy.(i) <- if pl = opponent then (pg_get_tr game i).(0) else -1
        done;
        (solution, strategy)
      end
    else
      begin
        let scc = List.hd !sccs in
        sccs := List.tl !sccs;
         (* print_string ("[SPS] Now considering SCC {" ^ String.concat "," (List.map string_of_int scc) ^ "}\n"); *)
         let (vs,p) = greatest_nodes scc in
         (* print_string ("[SPS] Nodes with highest priority " ^ string_of_int p ^ " are {" ^ String.concat "," (List.map string_of_int vs) ^ "}\n"); *)
         if (p mod 2) = player && is_proper_scc scc then
           begin
             let attr = attr_closure_inplace gm strategy player vs in
             let _    = attr_closure_inplace game strategy player attr in
             (* print_string ("[SPS] " ^ string_of_int player ^ "-attractor is {" ^ String.concat "," (List.map string_of_int attr) ^ "}\n"); *)
             List.iter (fun v -> let pl = pg_get_pl game v in
                                 if pl = player then
                                   begin
                                     let ws = List.filter (fun w -> List.mem w scc) (Array.to_list (pg_get_tr gm v)) in
                                     let w = try
                                               List.hd ws
                                             with _ -> failwith ("Paritygame.solve_single_player_scc: fatal error, node has no successor within SCC!")
	          		     in
	       			     strategy.(v) <- w
                                   end
		       ) vs;
	     (Array.make n player, strategy)
	   end
	 else
	   begin
	     let attr = attr_closure_inplace gm dummy (1-player) vs in
	       (* print_string ("[SPS] " ^ string_of_int (1-player) ^ "-attractor is {" ^ String.concat "," (List.map string_of_int attr) ^ "}, remaining game after removal is\n"); *)
	       pg_remove_nodes gm attr;
	       (* print_game gm; *)
	       let (newsccs,_,_,_) = strongly_connected_components (subgame scc) in
	       sccs := (Array.to_list newsccs) @ !sccs;
	       check_for_player_in_next_scc ()
	   end
      end
  in
  check_for_player_in_next_scc ()
*)

(*
let solve_single_player_scc game player =
	let rec solve_single_player_scc' sccgame sccnodes tgraph =
		let n = pg_size sccgame in
		let p = pg_max_prio sccgame in
		let strategy = Array.make n (-1) in
		if p mod 2 = player then (
			let vs = pg_prio_nodes sccgame p in
			let vsset = TreeSet.of_list_def vs in
			List.iter (fun i ->	strategy.(i) <- (pg_get_tr sccgame i).(0)) vs;
			let _ = attr_closure_inplace' sccgame strategy player vsset true tgraph (fun _ -> true) true in
			Some (strategy, sccnodes)
		)
		else let p' = pg_max_prio_for sccgame player in
             if p' < 0 then None else (
				let p = if p' < 0 then p else p' + 1 in
				let vs = collect_nodes sccgame (fun _ (pr, _, _, _) -> pr >= p) in
				let vsset = TreeSet.of_list_def vs in
				let attr = attr_closure_inplace' sccgame strategy (1-player) vsset true tgraph (fun _ -> true) true in
				let backup = ref (TreeSet.of_list_def attr) in
				List.iter (fun i ->
					Array.iter (fun j -> backup := TreeSet.add j !backup) (pg_get_tr sccgame i);
					List.iter (fun j -> backup := TreeSet.add j !backup) (tgraph.(i));
				) attr;
				let backuped = ref [] in
				TreeSet.iter (fun i ->
					backuped := (i, game.(i), tgraph.(i))::!backuped
				) !backup;
				pg_with_graph_remove_nodes sccgame tgraph attr;
				let (newsccs,_,_,_) = strongly_connected_components' sccgame tgraph in
				let scccount = Array.length newsccs in
				let todo = ref 0 in
				let success = ref None in
				while (!todo < scccount) && (!success = None) do
					let current = newsccs.(!todo) in
					let (curgame, curgraph) = subgame_and_subgraph_by_list sccgame tgraph current in
					if (Array.length curgame > 1) || (Array.length (pg_get_tr curgame 0) > 0)
					then success := solve_single_player_scc' curgame (Array.of_list current) curgraph;
					incr todo
				done;
				List.iter (fun (i, g, t) ->
					sccgame.(i) <- g;
					tgraph.(i) <- t
				) !backuped;
				decr todo;
				match !success with
					None -> None
				|	Some (strat, nodes) -> (
						Some (strat, Array.map (fun j -> sccnodes.(j)) nodes)
				)
		)
	in
	let n = pg_size game in
	let tgraph = game_to_transposed_graph game in
	match solve_single_player_scc' game	(Array.init n (fun i -> i)) tgraph with
		Some (strat, sccnodes) -> (
			let strategy = Array.make n (-1) in
			Array.iteri (fun i j ->
				strategy.(sccnodes.(i)) <- if j >= 0 then sccnodes.(j) else -1
			) strat;
			let vsset = TreeSet.of_array_def sccnodes in
			let _ = attr_closure_inplace' game strategy player vsset true tgraph (fun _ -> true) true in
			(Array.make n player, strategy)
		)
	|	None ->
			let strat = Array.init n (fun j ->
				let (_, pl, tr, _) = game.(j) in
				if pl = player then -1 else tr.(0)
			)
			in
			(Array.make n (1 - player), strat);;
*)


type process_result = PlayerWin of strategy * (int array)
                    | PlayerLoss
                    | Undecided of int

let solve_single_player_scc game player =
	let process_scc sccgame sccnodes tgraph =
		let n = pg_size sccgame in
		let p_pl = ref (-1) in
		let p_op = ref (-1) in
		for i = 0 to n - 1 do
			let (pr, _, _, _) = sccgame.(i) in
			let arg = if pr mod 2 = player then p_pl else p_op in
			arg := max !arg pr
		done;
		if !p_pl > !p_op then (
            let strategy = Array.make n (-1) in
			let vs = pg_prio_nodes sccgame !p_pl in
			let vsset = TreeSet.of_list_def vs in
			List.iter (fun i ->
				if pg_get_pl sccgame i = player then strategy.(i) <- (pg_get_tr sccgame i).(0)
			) vs;
			let _ = attr_closure_inplace' sccgame strategy player vsset true tgraph (fun _ -> true) true in
			PlayerWin (strategy, sccnodes)
		)
		else if !p_pl < 0 then PlayerLoss
		else Undecided (!p_pl + 1)
	in
	let rec process_undecided sccgame sccnodes tgraph p =
		let n = pg_size sccgame in
        let strategy = Array.make n (-1) in
        let vs = collect_nodes sccgame (fun _ (pr, _, _, _) -> pr >= p) in
        let vsset = TreeSet.of_list_def vs in
        let attr = attr_closure_inplace' sccgame strategy (1-player) vsset true tgraph (fun _ -> true) true in
        let backup = ref (TreeSet.of_list_def attr) in
        List.iter (fun i ->
            Array.iter (fun j -> backup := TreeSet.add j !backup) (pg_get_tr sccgame i);
            List.iter (fun j -> backup := TreeSet.add j !backup) (tgraph.(i));
        ) attr;
        let backuped = ref [] in
        TreeSet.iter (fun i ->
            backuped := (i, game.(i), tgraph.(i))::!backuped
        ) !backup;
        pg_with_graph_remove_nodes sccgame tgraph attr;
        let (newsccs,_,_,_) = strongly_connected_components' sccgame tgraph in
        let scccount = Array.length newsccs in
        let todo = ref 0 in
        let undecided_stack = ref [] in
        let success = ref None in
        while (!todo < scccount) && (!success = None) do
            let current = newsccs.(!todo) in
            let (curgame, curgraph) = subgame_and_subgraph_by_list sccgame tgraph current in
            if (Array.length curgame > 1) || (Array.length (pg_get_tr curgame 0) > 0) then (
            	let nodelist = Array.of_list current in
            	match process_scc curgame nodelist curgraph with
            		PlayerWin (s, t) -> success := Some (s, t)
            	|	Undecided p -> undecided_stack := (curgame, nodelist, curgraph, p)::!undecided_stack
            	|	PlayerLoss -> ()
            );
            incr todo
        done;
        if !success = None then (
        	while (!undecided_stack != []) && (!success = None) do
        		let (curgame, nodelist, curgraph, p) = List.hd !undecided_stack in
        		undecided_stack := List.tl !undecided_stack;
        		success := process_undecided curgame nodelist curgraph p
        	done
        );
        List.iter (fun (i, g, t) ->
            sccgame.(i) <- g;
            tgraph.(i) <- t
        ) !backuped;
        match !success with
            None -> None
        |   Some (strat, nodes) -> 
                Some (strat, Array.map (fun j -> sccnodes.(j)) nodes)
	in
	let rec solve_single_player_scc' sccgame sccnodes tgraph =
		match process_scc sccgame sccnodes tgraph with
			Undecided p -> process_undecided sccgame sccnodes tgraph p
		|	PlayerWin (s, t) -> Some (s, t)
		|	PlayerLoss -> None
	in
	let n = pg_size game in
	let tgraph = game_to_transposed_graph game in
	match solve_single_player_scc' game	(Array.init n (fun i -> i)) tgraph with
		Some (strat, sccnodes) -> (
			let strategy = Array.make n (-1) in
			Array.iteri (fun i j ->
				strategy.(sccnodes.(i)) <- if j >= 0 then sccnodes.(j) else -1
			) strat;
			let vsset = TreeSet.of_array_def sccnodes in
			let _ = attr_closure_inplace' game strategy player vsset true tgraph (fun _ -> true) true in
			(Array.make n player, strategy)
		)
	|	None ->
			let strat = Array.init n (fun j ->
				let (_, pl, tr, _) = game.(j) in
				if pl = player then -1 else tr.(0)
			)
			in
			(Array.make n (1 - player), strat);;


let solve_single_parity_scc game player =
    let n = pg_size game in
	let solution = Array.make n player in
	let strategy = Array.make n (-1) in
	for i = 0 to n - 1 do
		let (pr, pl, delta, _) = game.(i) in
		if (pr >= 0) && (pl = player)
		then strategy.(i) <- delta.(0)
	done;
	(solution, strategy);;






(********************************)














(*
let compute_winning_nodes_for game pl =

    let subnodes_by_list game li =
        let n = List.length li in
        let g = Array.make n (-1, -1, ref TreeSet.empty_def) in
        let i = ref 0 in
        List.iter (fun arri ->
            let (pr, pl, delta, desc) = game.(arri) in
            game.(arri) <- (-2, !i, delta, desc);
            g.(!i) <- (pr, pl, ref TreeSet.empty_def);
            i := !i + 1
        ) li;
        let i = ref 0 in
        List.iter (fun arri ->
            let (pr, pl, _) = g.(!i) in
            let (_, _, delta, _) = game.(arri) in
            let l = ref TreeSet.empty_def in
                for j = 0 to (Array.length delta) - 1 do
                    let (h, k, _, _) = game.(delta.(j)) in
                        if h = -2 then l := TreeSet.add k !l
                done;
            g.(!i) <- (pr, pl, l);
            i := !i + 1
        ) li;
        let i = ref 0 in
        List.iter (fun arri ->
            let (_, _, delta, desc) = game.(arri) in
            let (pr, pl, _) = g.(!i) in
            game.(arri) <- (pr, pl, delta, desc);
            i := !i + 1
        ) li;
        g
    in

	let solve_scc comp game pl =
		let game' = subnodes_by_list game comp in
		let new_edges = Queue.create () in
        Array.iteri (fun i (_, _, delta) -> TreeSet.iter (fun j -> Queue.add (i, j) new_edges) !delta) game';
        let winner = ref (1 - pl) in
        while (!winner != pl) && not (Queue.is_empty new_edges) do
        	let (u, v) = Queue.take new_edges in
        	if u = v
        	then let (pr_u, _, _) = game'.(u) in (
        		if pr_u mod 2 = pl then winner := pl
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
        !winner
    in

	let (sccs, sccindex, topology, roots) = strongly_connected_components game in
	let marked = Array.make (Array.length sccs) (-1) in

	let max_prio_for game l pl =
		List.fold_left (fun p el ->
			let (pr, _, _, _) = game.(el) in
				if pr mod 2 = pl then max p pr else p
		) (-1) l
	in

	let rec process_root r =
		if marked.(r) < 0 then (
            List.iter process_root topology.(r);
            if List.exists (fun c -> marked.(c) = pl) topology.(r)
            then marked.(r) <- pl
            else let comp = sccs.(r) in
                 if List.length comp = 1
                 then let x = List.hd comp in
                      let (pr, pl', delta, _) = game.(x) in
                      if Array.length delta = 0
                      then marked.(r) <- 1 - pl'
                      else if (List.exists (fun y -> x = y) (Array.to_list delta)) && (pr mod 2 = pl)
                      then marked.(r) <- pl
                      else marked.(r) <- 1 - pl
                 else let pl_max = max_prio_for game comp pl in
                      let plo_max = max_prio_for game comp (1 - pl) in
                      if pl_max > plo_max
                      then marked.(r) <- pl
                      else marked.(r) <- if pl_max < 0 then 1 - pl else solve_scc comp game pl
		)
	in

	List.iter (fun r -> let _ = process_root r in ()) roots;
	let (nodes_pl, nodes_pl') = (ref [], ref []) in
	for i = 0 to (Array.length sccs) - 1 do
		let nodesref = if marked.(i) = pl then nodes_pl else nodes_pl' in
		List.iter (fun u -> nodesref := u::!nodesref) sccs.(i)
	done;
	(!nodes_pl, !nodes_pl');;

let compute_winning_nodes game strat pl =
	snd (compute_winning_nodes_for (subgame_by_strat game strat) (1 - pl));;
*)


(*
let compute_winning_nodes_for game pl compute_strat =

    let TreeSet.of_list_def l =
    	let s = ref TreeSet.empty_def in
    	List.iter (fun x -> s := TreeSet.add x !s) l;
    	s
    in

	let max_prio_for game l pl =
		List.fold_left (fun p el ->
			let (pr, _, _, _) = game.(el) in
				if pr mod 2 = pl then max p pr else p
		) (-1) l
	in

    let max_prio_queue game comp pr =
    	let q = Queue.create () in
    	List.iter (fun x -> let (pr', _, _, _) = game.(x) in
    		if pr' = pr then Queue.add x q
    	) comp;
    	q
    in

	let getpl game x = let (_, pl', _, _) = game.(x) in pl' in

    let list_attractor transp getpl strat source_set_ref todo =
    	while not (Queue.is_empty todo) do
    		let x = Queue.take todo in
            List.iter (fun y -> if TreeSet.mem y !source_set_ref then (
            	source_set_ref := TreeSet.remove y !source_set_ref;
                Queue.add y todo;
                if (getpl y = pl) && (strat.(y) < 0)
                then strat.(y) <- x;
            ) ) transp.(x)
    	done
    in

    let list_of_main_prios game p0 p1 =
        let p = if p0 < 0 then p1 else if p1 < 0 then p0
                else if p0 < p1 then p0 + 1 else p1 + 1 in
        let l = ref [] in
        for i = 0 to Array.length game - 1 do
            let (pr, _, _, _) = game.(i) in
            if (pr >= p) && (pr >= 0) then l := i::!l
        done;
        !l
    in

    let queue_of_list comp =
    	let q = Queue.create () in
    	List.iter (fun x -> Queue.add x q) comp;
    	q
    in

	let rec dominion_find_scc game comp pl =
		let n = Array.length game in
		let m = List.length comp in
		if m = 1 then (
			let x = List.hd comp in
			let (pr, pl', delta, _) = game.(x) in
			if (List.exists (fun y -> x = y) (Array.to_list delta)) && (pr mod 2 = pl)
			then Some (comp, if compute_strat
			                 then let s = Array.make n (-1) in (s.(x) <- x; s)
			                 else [||])
			else None
		)
        else let pl_max = max_prio_for game comp pl in
             let plo_max = max_prio_for game comp (1 - pl) in
             if pl_max > plo_max then (
                if compute_strat then (
                	let strategy = Array.make n (-1) in
                    list_attractor (game_to_transposed_graph game) (getpl game)
                                   strategy (TreeSet.of_list_def comp) (max_prio_queue game comp pl_max);
                    Some (comp, strategy)
                )
                else Some(comp, [||])
              )
              else if pl_max < 0 then None
              else (
              	pg_remove_nodes game (list_of_main_prios game pl_max plo_max);
              	let (sccs, _, _, _) = strongly_connected_components game in
              	let found = ref None in
              	for i = 0 to Array.length sccs - 1 do
              		if !found = None
              		then found := dominion_find_scc game sccs.(i) pl
              	done;
              	!found
              )
    in

	let solve_scc comp pl =
		let game' = subgame_by_list game comp in
		let game'' = pg_copy game' in
		let n = Array.length game' in
		let l = ref [] in
		for i = 0 to n - 1 do
			l := i::!l
		done;
		match (dominion_find_scc game'' !l pl) with
			None -> (1 - pl, [||])
		|   Some (dominion, strat) -> (
					if compute_strat
					then list_attractor (game_to_transposed_graph game') (getpl game')
					                    strat (TreeSet.of_list_def comp) (queue_of_list dominion);
					(pl, strat)
			)
	in

    let n = Array.length game in
    let transp = if compute_strat then game_to_transposed_graph game else Array.make 0 [] in
    let strategy = Array.make (if compute_strat then n else 0) (-1) in

	let (sccs, sccindex, topology, roots) = strongly_connected_components game in
	let marked = Array.make (Array.length sccs) (-1) in

	let rec process_root r =
		if marked.(r) < 0 then (
            List.iter process_root topology.(r);
            let c = List.fold_left (fun c c' -> if c != -1 && marked.(c) = pl then c else c') (-1) topology.(r) in
            if c != -1 && marked.(c) = pl
            then (
            	marked.(r) <- pl;
            	if compute_strat
            	then list_attractor transp (getpl game) strategy (TreeSet.of_list_def sccs.(r)) (queue_of_list sccs.(c));
            )
            else let comp = sccs.(r) in
                 if List.length comp = 1
                 then let x = List.hd comp in
                      let (pr, pl', delta, _) = game.(x) in
                      if Array.length delta = 0
                      then marked.(r) <- 1 - pl'
                      else if (List.exists (fun y -> x = y) (Array.to_list delta)) && (pr mod 2 = pl)
                      then (
                      	marked.(r) <- pl;
                      	if (compute_strat && getpl game x = pl) then strategy.(x) <- x
                      )
                      else marked.(r) <- 1 - pl
            else (
            	let (winner, strat) = solve_scc comp pl in
            	marked.(r) <- winner;
                if compute_strat && (winner = pl) then (
                    let arr = Array.of_list comp in
                    for i = 0 to (Array.length arr) - 1 do
                        if strat.(i) >= 0 then strategy.(arr.(i)) <- arr.(strat.(i))
                    done
                )
            )
		)
	in

	List.iter (fun r -> let _ = process_root r in ()) roots;
	let solution = Array.make n (-1) in
	for i = 0 to (Array.length sccs) - 1 do
		List.iter (fun u -> solution.(u) <- marked.(i)) sccs.(i)
	done;
	(solution, strategy);;
*)

(* Takes a game in which only player pl can make decisions (the algorithm doesn't distinguish positions for player pl and player 1 - pl.
   Computes winning sets for both players and strategy for player pl *)
let compute_winning_nodes_for_direct game pl =
	let compute_strat = true in

    let subnodes_by_list game li =
        let n = List.length li in
        let g = Array.make n (-1, -1, ref TreeSet.empty_def) in
        let i = ref 0 in
        List.iter (fun arri ->
            let (pr, pl, delta, desc) = game.(arri) in
            game.(arri) <- (-2, !i, delta, desc);
            g.(!i) <- (pr, pl, ref TreeSet.empty_def);
            i := !i + 1
        ) li;
        let i = ref 0 in
        List.iter (fun arri ->
            let (pr, pl, _) = g.(!i) in
            let (_, _, delta, _) = game.(arri) in
            let l = ref TreeSet.empty_def in
                for j = 0 to (Array.length delta) - 1 do
                    let (h, k, _, _) = game.(delta.(j)) in
                        if h = -2 then l := TreeSet.add k !l
                done;
            g.(!i) <- (pr, pl, l);
            i := !i + 1
        ) li;
        let i = ref 0 in
        List.iter (fun arri ->
            let (_, _, delta, desc) = game.(arri) in
            let (pr, pl, _) = g.(!i) in
            game.(arri) <- (pr, pl, delta, desc);
            i := !i + 1
        ) li;
        g
    in

    let list_attractor transp getpl strat source_set_ref todo =
    	while not (Queue.is_empty todo) do
    		let x = Queue.take todo in
            List.iter (fun y -> if TreeSet.mem y !source_set_ref then (
            	source_set_ref := TreeSet.remove y !source_set_ref;
                Queue.add y todo;
                if (getpl y = pl) && (strat.(y) < 0)
                then strat.(y) <- x;
            ) ) transp.(x)
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
        				list_attractor g (fun i -> let (_, pl, _) = game''.(i) in pl) strat s todo
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

    let n = Array.length game in
    let transp = if compute_strat then game_to_transposed_graph game else Array.make 0 [] in
    let strategy = Array.make (if compute_strat then n else 0) (-1) in

    let max_prio_queue comp pr =
    	let q = Queue.create () in
    	List.iter (fun x -> let (pr', _, _, _) = game.(x) in
    		if pr' = pr then Queue.add x q
    	) comp;
    	q
    in

	let (sccs, sccindex, topology, roots) = strongly_connected_components game in
	let marked = Array.make (Array.length sccs) (-1) in

	let max_prio_for l pl =
		List.fold_left (fun p el ->
			let (pr, _, _, _) = game.(el) in
				if pr mod 2 = pl then max p pr else p
		) (-1) l
	in

	let rec process_root r =
		let getpl x = let (_, pl', _, _) = game.(x) in pl' in
		if marked.(r) < 0 then (
            List.iter process_root topology.(r);
            let c = List.fold_left (fun c c' -> if c != -1 && marked.(c) = pl then c else c') (-1) topology.(r) in
            if c != -1 && marked.(c) = pl
            then (
            	marked.(r) <- pl;
            	if compute_strat
            	then list_attractor transp getpl strategy (ref (TreeSet.of_list_def sccs.(r))) (QueueUtils.of_list sccs.(c));
            )
            else let comp = sccs.(r) in
                 if List.length comp = 1
                 then let x = List.hd comp in
                      let (pr, pl', delta, _) = game.(x) in
                      if Array.length delta = 0
                      then marked.(r) <- 1 - pl'
                      else if (List.exists (fun y -> x = y) (Array.to_list delta)) && (pr mod 2 = pl)
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
                      		list_attractor transp getpl strategy (ref (TreeSet.of_list_def sccs.(r))) (max_prio_queue comp pl_max);
                      		List.iter (fun q ->
                      			let (_, pl', d, _) = game.(q) in
                      			if (pl' = pl) then
                      				let i = ref 0 in
                      				while (strategy.(q) < 0) do
                      					if sccindex.(d.(!i)) = r
                      					then strategy.(q) <- d.(!i)
                      					else i := !i + 1
                      				done
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