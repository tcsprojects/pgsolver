open Basics;;
open Paritygame;;
open Solvers;;
open Univsolve;;
open Transformations;;
open Tcsset;;
open Tcsarray;;
open Tcslist;;
open Tcsstrings;;
open Tcsbasedata;;
open Tcsgraph;;
open Arg;;
open Tcsargs;;


let array_max a less = ArrayUtils.max_elt (fun x y -> if less x y then -1 else 1) a
let list_max a less = ListUtils.max_elt (fun x y -> if less x y then -1 else 1) a

let multiple_edge_transformation game =
	let n = pg_size game in
	let mult = Array.make n [||] in
	let game' = pg_copy game in
	for i = 0 to n - 1 do
		let (pr, _, tr, _) = game.(i) in
		if (pr = 0) && (Array.length tr = 1)
		then game'.(i) <- (-1, -1, [||], None)
	done;
	for i = 0 to n - 1 do
		let (pr, _, tr, _) = game'.(i) in
		if (pr >= 0) then (
			let s = ref TreeMap.empty_def in
			Array.iter (fun r ->
				let r = ref r in
				while (pg_get_pr game' !r < 0) do
					r := (pg_get_tr game !r).(0)
				done;
				try
					let q = TreeMap.find !r !s in
					s := TreeMap.add !r (q + 1) !s
				with
					Not_found -> s := TreeMap.add !r 1 !s
			) tr;
			let (tr, mu) = (ref [], ref []) in
			TreeMap.iter (fun r m ->
				tr := r::!tr;
				mu := m::!mu
			) !s;
			pg_set_tr game' i (Array.of_list !tr);
			mult.(i) <- Array.of_list !mu
		)
	done;
	(game', mult)

let multiple_edge_backtransformation game solution strategy =
	Array.iteri (fun i (pr, pl, tr, _) ->
		if (pr >= 0) && (solution.(i) < 0) then (
			solution.(i) <- solution.(tr.(0));
			if solution.(i) = pl
			then strategy.(i) <- tr.(0)
		)
	) game

let multiple_edge_solve game solver =
	let (game', mult) = multiple_edge_transformation game in
	let (game'', new2old, old2new) = compress_nodes game' in
	let n = Array.length game' in
	let n' = Array.length game'' in
	let mult' = Array.make n' [||] in
	for i = 0 to n' - 1 do
		mult'.(i) <- mult.(new2old.(i))
	done;
	let (sol', strat') = solver game'' mult' in
	let sol = Array.init n (fun i ->
		if old2new.(i) = -1 then -1 else sol'.(old2new.(i))
	) in
	let strat = Array.init n (fun i ->
		if old2new.(i) = -1 then -1
		else let j = strat'.(old2new.(i)) in
		     if j = -1 then -1 else new2old.(j)
	) in
	multiple_edge_backtransformation game sol strat;
	(sol, strat)


let enable_exp_bit_count = ref false

let last_exp_bit_count = ref 0

let get_last_exp_bit_count _ = !last_exp_bit_count

let last_iteration_count = ref 0

let get_last_iteration_count _ = !last_iteration_count

let _strat_impr_callback = ref None

type dynamic_paritygame = (int * int * string option) DynamicGraph.dynamic_graph

let paritygame_to_dynamic_paritygame game =
	let graph = DynamicGraph.make () in
	Array.iteri (fun i (pr, pl, _, desc) ->
		DynamicGraph.add_node i (pr, pl, desc) graph
	) game;
	Array.iteri (fun i (_, _, tr, _) ->
		Array.iter (fun j -> DynamicGraph.add_edge i j graph) tr
	) game;
	graph

let dynamic_subgame_by_strategy graph strat =
	DynamicGraph.sub_graph_by_edge_pred (fun v w ->
		strat.(v) = -1 || strat.(v) = w
	) graph

let paritygame_to_dynamic_paritygame_by_strategy game strat =
	let graph = DynamicGraph.make () in
	Array.iteri (fun i (pr, pl, _, desc) ->
		DynamicGraph.add_node i (pr, pl, desc) graph
	) game;
	Array.iteri (fun i (_, _, tr, _) ->
		if strat.(i) = -1
		then Array.iter (fun j -> DynamicGraph.add_edge i j graph) tr
		else DynamicGraph.add_edge i strat.(i) graph
	) game;
	graph


type node_valuation = int * int TreeSet.t * int

type game_valuation = node_valuation array

type initial_strategy_fun = paritygame -> strategy

type node_total_ordering_fun = paritygame -> int -> int -> int

type 'a improvement_policy_fun = paritygame -> node_total_ordering_fun -> 'a -> strategy -> game_valuation -> strategy * 'a

let format_node_valuation (a, b, c) =
    let t = TreeSet.fold (fun el s -> s ^ (string_of_int el) ^", ") b "" in
    (string_of_int a) ^ " [" ^ t ^ "] " ^ (string_of_int c)

let format_game_valuation valu =
    let s = ref "" in
    for i = 0 to Array.length valu - 1 do
        s := !s ^ format_node_valuation valu.(i) ^ "\n"
    done;
    !s

let flip f x y = f y x

let tuple_total_ordering cmp f (x, r) (y, s) =
	let c = cmp x y in
	if c = 0 then f r s else c

let node_total_ordering_by_position _ i j =
	compare i j

let node_total_ordering_by_priority_and_position game i j =
	tuple_total_ordering compare compare (pg_get_pr game i, i) (pg_get_pr game j, j)

let relevance_total_ordering game node_total_ordering i j =
	tuple_total_ordering compare (node_total_ordering game) (pg_get_pr game i, i) (pg_get_pr game j, j)

let reward_total_ordering game node_total_ordering i j =
	let ri = reward 0 (pg_get_pr game i) in
	let rj = reward 0 (pg_get_pr game j) in
	let c = compare ri rj in
	if c != 0 then c
	else node_total_ordering game (if ri mod 2 = 0 then i else j) (if ri mod 2 = 0 then j else i)

let initial_strategy_by_first_edge game =
	Array.init (Array.length game) (fun i ->
		let (pr, pl, tr, _) = game.(i) in
		if (pr < 0) || (pl = 1) || (Array.length tr = 0) then -1 else tr.(0)
	)

let initial_strategy_by_last_edge game =
	Array.init (Array.length game) (fun i ->
		let (pr, pl, tr, _) = game.(i) in
		if (pr < 0) || (pl = 1) || (Array.length tr = 0) then -1 else tr.(Array.length tr - 1)
	)

let initial_strategy_by_random_edge game =
	Random.self_init ();
	Array.init (Array.length game) (fun i ->
		let (pr, pl, tr, _) = game.(i) in
		if (pr < 0) || (pl = 1) || (Array.length tr = 0) then -1 else tr.(Random.int (Array.length tr))
	)

let initial_strategy_by_best_reward game =
	Array.init (Array.length game) (fun i ->
		let (pr, pl, tr, _) = game.(i) in
		if (pr < 0) || (pl = 1) || (Array.length tr = 0) then -1
		else array_max tr (fun i j -> reward_total_ordering game node_total_ordering_by_position i j <= 0)
	)

let empty_descending_relevance_ordered_set game node_total_ordering =
	TreeSet.empty (flip (relevance_total_ordering game node_total_ordering))

let node_valuation_ordering game node_total_ordering (u, p, e) (v, q, f) =
    let is_odd x = pg_get_pr game x mod 2 = 1 in
    let is_even x = pg_get_pr game x mod 2 = 0 in
	let cycle_ordering = reward_total_ordering game node_total_ordering in
	let length_ordering (u, e) (v, f) =
		if e = f then 0
		else if ((is_odd v) && e < f) || ((is_even v) && e > f) then -1 else 1 in
	let path_ordering p q =
		let r = TreeSet.sym_diff p q in
		if TreeSet.cardinal r = 0 then 0
		else let w = TreeSet.min_elt r in
		     if ((is_even w) && TreeSet.mem w q) || ((is_odd w) && TreeSet.mem w p) then -1 else 1
    in
    tuple_total_ordering cycle_ordering
    	                 (tuple_total_ordering path_ordering length_ordering)
    	                 (u, (p, (u, e))) (v, (q, (v, f)))

let node_valuation_total_ordering game node_total_ordering valu x y =
	tuple_total_ordering (node_valuation_ordering game node_total_ordering)
						 (node_total_ordering game)
						 (valu.(x), x) (valu.(y), y)

let best_decision_by_ordering game ordering v =
	let ordering x y = ordering x y >= 0 in
    let (_, pl, tr, _) = game.(v) in
    array_max tr (fun x y -> if pl = 1 then ordering x y else ordering y x)

let best_decision_by_valuation_ordering game node_total_ordering valu v =
	best_decision_by_ordering game (node_valuation_total_ordering game node_total_ordering valu) v

let strategy_improvable game node_total_ordering strat valu =
    ArrayUtils.exists strat (fun i j ->
        (pg_get_pl game i = 0) &&
        (node_valuation_ordering game node_total_ordering valu.(j)
        						 valu.(best_decision_by_valuation_ordering game node_total_ordering valu i) < 0)
    )

let winning_regions game valu =
    Array.init (Array.length valu) (fun i ->
        let (v, _, _) = valu.(i) in
        pg_get_pr game v mod 2
    )

let winning_strategies game node_total_ordering strat valu =
    Array.init (Array.length valu) (fun i ->
        let (v, _, _) = valu.(i) in
        let winner = pg_get_pr game v mod 2 in
        let player = pg_get_pl game i in
        if winner = player
        then if player = 0 then strat.(i)
             else best_decision_by_valuation_ordering game node_total_ordering valu i
        else -1
    )

let evaluate_strategy game node_total_ordering strat =
	let n = Array.length game in
	let graph = paritygame_to_dynamic_paritygame_by_strategy game strat in
	let asc_rew = DynamicGraph.add_cmp (reward_total_ordering game node_total_ordering) graph in
	let rel_ord = relevance_total_ordering game node_total_ordering in
	let des_rel = DynamicGraph.add_cmp (flip rel_ord) graph in
	let valu = Array.make n (-1, empty_descending_relevance_ordered_set game node_total_ordering, 0) in

	DynamicGraph.iter_by (fun v ->
		let (w, _, _) = valu.(v) in
		if (w = -1) && (DynamicGraph.depth_find v (fun u -> rel_ord u v <= 0)
		                                          (fun u -> DynamicGraph.has_edge u v graph)
		                                          graph) then (
			let graph' = DynamicGraph.sub_graph_by_node_closure v (fun u f -> TreeSet.iter f (DynamicGraph.get_node_pred u graph)) graph in
			let del_other_edges x m =
				TreeSet.iter (fun y ->
					if not (TreeSet.mem y m) then DynamicGraph.del_edge x y graph'
				) (DynamicGraph.get_node_succ x graph')
			in
			DynamicGraph.iter (fun u _ -> valu.(u) <- (v, empty_descending_relevance_ordered_set game node_total_ordering, 0)) graph';
			DynamicGraph.iter_by (fun u ->
				if rel_ord u v > 0 then (
					if pg_get_pr game u mod 2 = 0 then (
						let m = GraphUtils.build_reachability_set (v, (fun x -> TreeSet.to_iterator (TreeSet.filter ((!=) u) (DynamicGraph.get_node_pred x graph')))) in
						DynamicGraph.iter (fun q _ ->
							if not (TreeSet.mem q m)
							then let (a, b, c) = valu.(q) in
								 valu.(q) <- (a, TreeSet.add u b, c)
						) graph';
						del_other_edges u m;
						TreeSet.iter (fun q -> del_other_edges q m) m
					)
					else (
						let m = GraphUtils.build_reachability_set (u, (fun x -> TreeSet.to_iterator (TreeSet.filter ((!=) v) (DynamicGraph.get_node_pred x graph')))) in
						TreeSet.iter (fun q ->
							let (a, b, c) = valu.(q) in
							valu.(q) <- (a, TreeSet.add u b, c)
						) m;
						TreeSet.iter (fun x ->
							if not (x = u) then del_other_edges x m
						) m
					)
				)
			) des_rel graph';
			let f n i = let (p0, p1, _) = valu.(n) in valu.(n) <- (p0, p1, i) in
			if pg_get_pr game v mod 2 = 0
            then GraphUtils.iterate_with_maximal_distance_single_loop (v, (fun x -> TreeSet.to_iterator (DynamicGraph.get_node_pred x graph')),
                                                                       (fun x -> TreeSet.to_iterator (DynamicGraph.get_node_succ x graph'))) f
            else GraphUtils.iterate_with_minimal_distance (v, (fun x -> TreeSet.to_iterator (DynamicGraph.get_node_pred x graph'))) f;
			DynamicGraph.iter (fun u _ -> DynamicGraph.del_node u graph) graph'
		)
	) asc_rew graph;
	valu

let compute_counter_strategy game strategy = 
	let valu = evaluate_strategy game node_total_ordering_by_position strategy in
	Array.init (Array.length valu) (fun i ->
		if pg_get_pl game i = 0
		then -1
		else best_decision_by_valuation_ordering game node_total_ordering_by_position valu i
	)

let less_valuation val_comp v u =
	let res = ref false in
	let fin = ref false in
	let n = Array.length v in
	let i = ref 0 in
	while (!i < n) && (not !fin) do
		let c = val_comp v.(!i) u.(!i) in
		if c > 0 then (
			res := false;
			fin := true
		)
		else if c < 0 then (
			res := true
		);
		incr i
	done;
	!res

(* TEST *)
(*
let format_game_state game valu =

    let lookup_node s =
        let check i =
            match (pg_get_desc game i) with
                None -> false
            |   Some t -> s=t
        in
        let i = ref 0 in
        let n = Array.length game in
        while (!i < n) && (not (check !i)) do
            incr i
        done;
        if !i < n then Some !i else None
    in

    let l = ref [] in
    let i = ref (-1) in
    let go = ref true in

   *)
let format_game_state game valu =

    let lookup_node s =
        let check i =
            match (pg_get_desc game i) with
                None -> false
            |   Some t -> s=t
        in
        let i = ref 0 in
        let n = Array.length game in
        while (!i < n) && (not (check !i)) do
            incr i
        done;
        if !i < n then Some !i else None
    in

    let l = ref [] in
    let i = ref (-1) in
    let go = ref true in

	if lookup_node "e0" = None then (
	    let u0 = lookup_node "k0" in
	    match u0 with
	    	Some d -> (
			    let (_, vald, _) = valu.(d) in
			    while !go do
			    	incr i;
			    	match (lookup_node ("k" ^ string_of_int !i)) with
			    		Some e -> (
			    			l := (if TreeSet.mem e vald then 1 else 0)::!l
			    		)
			    	|	None -> go := false
			    done
			   )
		|	_ -> ()
	)
	else (
	    while !go do
	    	incr i;
	    	match (lookup_node ("e" ^ string_of_int !i)) with
	    		Some e -> (
	    			match (lookup_node ("d(0,0,0)")) with
	    				Some _ -> (
		                let go2 = ref true in
		                let j = ref 0 in
		                let k = ref (-1) in
		                let pass = ref true in
		                while !go2 && !pass do
		                    incr k;
		                    match (lookup_node ("d(" ^ string_of_int !i ^ ","^string_of_int !j^","^string_of_int !k^ ")")) with
		                    	Some d -> (
		                    		let (_, vald, _) = valu.(d) in
		                    		pass := TreeSet.mem e vald;
		                    		go2 := !pass
		                    	)
		                    |	_ -> (
		                    	if !k = 0 then go2 := false else (k := -1; incr j)
		                    )
		                done;
		            	l := (if !pass then 1 else 0)::!l
	    				)
	    			|	None -> (
						match (lookup_node "d0") with
							Some _ -> (
								let pass = ref false in
								(
								match (lookup_node ("d" ^ string_of_int !i)) with
									Some d -> (
										let (_, vald, _) = valu.(d) in
										pass := TreeSet.mem e vald;
									)
								|	_ -> ();
								);
								l := (if !pass then 1 else 0)::!l
							)
						|	None -> (
		                let go2 = ref true in
		                let j = ref (-1) in
		                let pass = ref true in
		                while !go2 && !pass do
		                    incr j;
		                    match (lookup_node ("d(" ^ string_of_int !i ^ ","^string_of_int !j^ ")")) with
		                    	Some d -> (
		                    		let (_, vald, _) = valu.(d) in
		                    		pass := TreeSet.mem e vald;
		                    		go2 := !pass
		                    	)
		                    |	_ -> go2 := false
		                done;
		            	l := (if !pass then 1 else 0)::!l
		            ))
	    		)
	    	|	_ -> go := false
	    done
	);

(*
    let l' = ref [] in
    let i = ref (-1) in
    let go = ref true in

    while !go do
    	incr i;
    	match (lookup_node ("e" ^ string_of_int !i), lookup_node ("h" ^ string_of_int !i)) with
    		(Some e, Some h) -> (
    			let (_, vale, _) = valu.(e) in
	    		l' := (if TreeSet.mem h vale then 1 else 0)::!l'
    		)
    	|	_ -> go := false;
    done;

    let upl = ref [] in
    let i = ref (-1) in
    let go = ref true in

    while !go do
    	incr i;
    	match (lookup_node ("k" ^ string_of_int !i)) with
    		Some k -> (
    			let (_, valk, _) = valu.(k) in
    			let x = ref (0) in
    			let j = ref !i in
    			let go2 = ref true in
    			while !go2 do
    				incr j;
    				match (lookup_node ("f" ^ string_of_int !j)) with
    					Some f -> (
    						if TreeSet.mem f valk then (
    							x := !j;
    							go2 := false
    						)
    					)
    				| _ -> go2 := false
    			done;
    			upl := (!i, !x)::!upl
    		)
    	|	_ -> go := false
    done;
*)
     let v = ref 0 in

    let u = List.map (fun b ->
    	v := 2 * !v;
    	if b=1 then incr v;
    	if b=1 then "1" else "0"
    ) !l in
(*
    let w = List.map (fun (_, h) ->
    	string_of_int h
    ) !upl in

    let l = ref [] in
    let i = ref (-1) in
    let go = ref true in
    while !go do
    	incr i;
    	match (lookup_node ("f" ^ string_of_int !i)) with
    		Some f -> l := (!i, f)::!l
    	|	None -> go := false
    done;

    let cmp = (fun (_, f) (_, f') -> (node_valuation_ordering game (node_total_ordering_by_position) valu.(f') valu.(f))) in
    let a = Array.of_list !l in
    Array.sort cmp a;
    let s = ListUtils.format (fun (i, _) -> string_of_int i) (Array.to_list a) in

    let s' = ListUtils.format string_of_int !l' in

    (("[" ^ String.concat "," u ^ "]" ^ " / [" ^ String.concat "," w ^ "]" ^ " / " ^ s ^ " / " ^ s'), !v)
*)
    (("[" ^ String.concat "," u ^ "]"), !v)


let transform_game_state_list l =
	let t = ref [] in
	let l = ref l in
	let v = ref (-1) in
	while !l != [] do
		let (s, v') = List.hd !l in
		l := List.tl !l;
		if (!v < 0) || (v' < !v) then (
			v := v';
			t := (s, v')::!t
		)
	done;
	!t


(* TEST *)


let strategy_improvement (game: paritygame)
						 (init_strat: initial_strategy_fun)
						 (node_compare: node_total_ordering_fun)
						 (impr_policy: 'a improvement_policy_fun)
						 (impr_policy_init: 'a)
						 (check_policy: bool)
						 (ident: string) =

    let msg_tagged v = message_autotagged v (fun _ -> ident) in
    let msg_tagged_nl v = message_autotagged_newline v (fun _ -> ident) in
    let msg_plain = message in

    msg_tagged 2 (fun _ -> "Starting strategy improvement algorithm\n");
    if !verbosity > 2 then (
	    msg_tagged_nl 3 (fun _ -> "Considering game:\n" ^ game_to_string game ^ "\n")
	);

	let impr_data = ref impr_policy_init in
	let improve x y =
		let (strat, impr) = impr_policy game node_compare !impr_data x y in
		impr_data := impr;
		strat
	in
	let evaluate = evaluate_strategy game node_compare in
	let improvable = strategy_improvable game node_compare in

    let counter = ref 0 in

	let strat = ref (init_strat game) in
    let valu = ref (evaluate !strat) in

	let rec get_desc game o i =
		match pg_get_desc game i with Some s -> s | None -> ""
	in
    let show_eval _ =
        if !verbosity > 2 then (

            let g = subgame_by_edge_pred game (fun u v -> (!strat).(u) = -1 || (!strat).(u) = v) in
            for i = 0 to Array.length game - 1 do
                pg_set_desc g i (Some (string_of_int i ^ " : " ^ format_node_valuation (!valu).(i)))
            done;
(*            msg_tagged_nl 3 (fun _ -> "\nMade valuation:\n" ^ game_to_string g ^ "\n"); *)

			let myfmt game strat =
				let a =	Array.mapi (fun i j -> if (j >= 0) && (Array.length (pg_get_tr game i) > 1) then get_desc game i i ^ "->" ^ get_desc game j j else "") strat in
				let b = List.filter (fun s -> not (s = "" || s = "->")) (Array.to_list a) in
				ListUtils.format (fun s -> s) (List.sort compare b)
			in
            msg_tagged_nl 3 (fun _ -> "0-Strategy: " ^ myfmt game !strat ^ "\n");
			let strat' = Array.mapi (fun i j ->
				if j < 0 then best_decision_by_valuation_ordering game node_compare !valu i else -1
			) !strat in
            msg_tagged_nl 3 (fun _ -> "1-Strategy: " ^ myfmt game strat' ^ "\n");
			let impr = ref [] in
			Array.iteri (fun i j ->
				if j != -1 then Array.iter (fun k ->
					if node_valuation_ordering game node_compare !valu.(j) !valu.(k) < 0
					then impr := (i,k)::!impr
				) (pg_get_tr game i)
			) !strat;
			msg_tagged_nl 3 (fun _ -> "Improvement-Arena: " ^ ListUtils.format (fun (i,j) -> get_desc game i i ^ "->" ^ get_desc game j j) (List.sort compare !impr) ^ "\n")
        )
    in
	let show_changed oldstrat newstrat =
		if !verbosity > 2 then (
			let l = ref [] in
			Array.iteri (fun i j ->
				if newstrat.(i) != j then l := (i,j,newstrat.(i))::!l
			) oldstrat;
			msg_tagged_nl 3 (fun _ -> "Strategy-Change: " ^ ListUtils.format (fun (i,j,k) -> get_desc game i i ^ "->" ^ get_desc game j j ^ "/" ^ get_desc game k k) (List.sort compare !l) ^ "\n")
		)
	in

	(* TEST *)
	let old = ref (-1) in
	let t = ref "" in
	let l = ref [] in
	(* TEST *)

    while (improvable !strat !valu) do
    	show_eval ();
        incr counter;

        msg_tagged 2 (fun _ -> "Iteration: " ^ string_of_int !counter ^ "\r");
		
		(
		match !_strat_impr_callback with
			None -> ()
		|	Some c -> c !strat !counter
		);

        (* TEST *)
        if !enable_exp_bit_count then (
            let (s, v) = format_game_state game !valu in
            let s = string_of_int !counter ^ " : " ^ s in
			msg_tagged_nl 3 (fun _ -> "\nIteration: " ^ s ^ "\n");
            if (!old != v) || (!t <> s) then l := (s, v)::!l;
            t := s;
            old := v
        );
        (* TEST *)

		let old = !strat in
        strat := improve !strat !valu;
		show_changed old !strat;
        let valu' = evaluate !strat in
        if check_policy && (not (less_valuation (node_valuation_ordering game node_compare) !valu valu')) then (
	        valu := valu';
        	show_eval ();
        	failwith "valuation improvement failure"
        );
        valu := valu'
    done;

    incr counter;
	(
	match !_strat_impr_callback with
		None -> ()
	|	Some c -> c !strat !counter
	);
    
    (* TEST *)
    if !enable_exp_bit_count then (
        let (s, v) = format_game_state game !valu in
        let s = string_of_int !counter ^ " : " ^ s in
		msg_tagged_nl 3 (fun _ -> "\nIteration: " ^ s ^ "\n");
        if (!old != v) || (!t <> s) then l := (s, v)::!l;
        t := s;
        old := v
    );
    (* TEST *)

    show_eval ();
    last_iteration_count := !counter;

    msg_plain 2 (fun _ -> "\n");

    (*TEST*)
    if !enable_exp_bit_count then (

        (*
        List.iter (fun (s, v) -> msg_tagged 2 (fun _ -> s ^ " = " ^ string_of_int v ^ "\n")) (List.rev !l);
        *)

        let l = transform_game_state_list !l in

        (*
        List.iter (fun (s, v) -> msg_tagged 2 (fun _ -> s ^ " = " ^ string_of_int v ^ "\n")) l;
        *)

        last_exp_bit_count := List.length l;
        msg_tagged 2 (fun _ -> "Bit Rounds: " ^ string_of_int !last_exp_bit_count ^ "\n")
    );
    (*TEST*)

	(winning_regions game !valu, winning_strategies game node_compare !strat !valu)

let improvement_policy_optimize_all_locally game node_total_ordering old_strategy valu =
	Array.mapi (fun i j ->
		if j = -1 then -1
		else let k = best_decision_by_valuation_ordering game node_total_ordering valu i in
			 if node_valuation_ordering game node_total_ordering valu.(j) valu.(k) < 0
			 then k
			 else j
	) old_strategy

let evaluate_player1_strategy game node_compare strategy =
	let game' = Array.map (fun (pr, pl, tr, de) -> (1 + pr, 1 - pl, tr, de)) game in
	evaluate_strategy game' node_compare strategy

let improvement_policy_by_counterstrategy game node_compare old_strategy valu =
	let n = Array.length game in
	let tau = winning_strategies game node_compare (Array.make n (-1)) valu in
	let valutau = evaluate_player1_strategy game node_compare tau in
	let find i =
		let ordering_valu x y = node_valuation_total_ordering game node_compare valu x y >= 0 in
		let ordering_valutau x y = node_valuation_total_ordering game node_compare valutau x y >= 0 in
		let tr = pg_get_tr game i in
		let a = ArrayUtils.filter (fun j -> ordering_valu j old_strategy.(i)) tr in
		array_max a (fun x y -> ordering_valutau y x)
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


let improvement_policy_optimize_all_globally game' node_total_ordering old_strategy valu =
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

	let graph' = DynamicGraph.copy_graph graph in
	let check = ref (TreeSet.empty compare) in
	for v = 0 to n - 1 do
		let edges = DynamicGraph.get_node_succ v graph in
		if pg_get_pl game v = 1 then (
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
		let pl = pg_get_pl game v in
		let cmp x y = if pl = 0 then valu_ord valu2.(x) valu2.(y) else valu_ord valu2.(y) valu2.(x) in
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
					if pg_get_pl game v = 0 then strategy.(v) <- u
				)
				else if (pg_get_pl game v = 1) &&
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
            let temp = TreeSet.filter (fun v -> (pg_get_pl game v = 1) && (TreeSet.exists (fun u -> valued.(u)) (DynamicGraph.get_node_succ v graph))) !rest in
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


let list_upfront l i =
	let rec tile f t =
        let j = List.hd t in
        if j = i then (f, t) else tile (j::f) (List.tl t)
    in
    let (f, t) = tile [] l in
    t @ (List.rev f)
    
let default_snare_sel _ _ _ _ snares =
	match snares with
		[] -> None
	|	x::_ -> Some x
    
let rec improvement_policy_snare_based sub_policy snare_sel game node_total_ordering (snares, enforce) old_strategy valu =
	let cmp i j = node_valuation_ordering game node_total_ordering valu.(i) valu.(j) in
	let valx i = let (_, x, _) = valu.(i) in x in
	match enforce with
		Some (sna,str,esc) -> (
			let fnd = ref None in
			TreeSet.iter (fun (i,j) ->
				if cmp j old_strategy.(i) > 0 then fnd := Some (i,j)
			) str;
			match !fnd with
				Some (i,j) -> (
					let new_strat = Array.copy old_strategy in
					new_strat.(i) <- j;
					(new_strat, (snares, enforce))
				)
			|	None -> improvement_policy_snare_based sub_policy snare_sel game node_total_ordering (snares, None) old_strategy valu
		)
	|	None -> (
			(* Update set of snares *)
			let snares = ref snares in
			Array.iteri (fun i (_, pl, tr, _) ->
				if pl = 0 then (
					Array.iter (fun j ->
						if (cmp j old_strategy.(i) > 0) && (TreeSet.mem i (valx j)) then (
							let sna = ref TreeSet.empty_def in
							let todo = ref (TreeSet.singleton_def j) in
							while not (TreeSet.is_empty !todo) do
								let k = TreeSet.min_elt !todo in
								todo := TreeSet.remove k !todo;
								if (not (TreeSet.mem k !sna)) && (TreeSet.mem i (valx k)) then (
									sna := TreeSet.add k !sna;
									let (_, pl, tr, _) = game.(k) in
									if pl = 0 then todo := TreeSet.add old_strategy.(k) !todo
									else Array.iter (fun j -> todo := TreeSet.add j !todo) tr
								)
							done;
							let esc = ref TreeSet.empty_def in
							let str = ref TreeSet.empty_def in
							TreeSet.iter (fun k ->
								let (_, pl, tr, _) = game.(k) in
								if i = k then str := TreeSet.add (i, j) !str
								else if pl = 0 then str := TreeSet.add (k,old_strategy.(k)) !str
								else Array.iter (fun j -> if not (TreeSet.mem j !sna) then esc := TreeSet.add (k,j) !esc) tr
							) !sna;
							snares := (!sna,!str,!esc)::!snares
						)
					) tr
				)
			) game;
			(* Filter set of applicable snares *)
			let appsnares = List.filter (fun (_,_,esc) ->
				TreeSet.for_all (fun (i,j) ->
					not (TreeSet.equal (valx i) (TreeSet.add i (valx j)))
				) esc
			) !snares in
			match (snare_sel game node_total_ordering old_strategy valu appsnares) with
				Some snare -> (
					improvement_policy_snare_based sub_policy snare_sel game node_total_ordering (!snares, Some snare) old_strategy valu
				)
			|	None -> (sub_policy game node_total_ordering old_strategy valu, (!snares, None))
		)
	

	
	
let improvement_policy_learn_strategies game node_total_ordering strategy_set old_strategy valu =
	(* Step 1: Update strategy_set *)
	let strategy_set = ref strategy_set in
	let add_to_strategy_set strat =
		strategy_set := TreeSet.add strat !strategy_set
	in
	add_to_strategy_set old_strategy;
	Array.iteri (fun i (_, pl, tr, _) ->
		if pl = 0 then (
			Array.iter (fun j ->
				if node_valuation_ordering game node_total_ordering valu.(j) valu.(old_strategy.(i)) > 0 then (
					let s = Array.copy old_strategy in
					s.(i) <- j;
					add_to_strategy_set s
				)
			) tr;
		)
	) game;
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
		Array.iteri (fun node _ ->
			let current = ref (morph old_strategy strategy node) in
			while !current != None do
				match !current with
					Some cur -> (
						add_to_improvement_set cur;
						current := morph cur strategy node
					)
				|	None -> ()
			done;
		) game;
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
		Array.iteri (fun v _ ->
			if node_valuation_ordering game node_total_ordering va.(v) (snd improvement_set_array.(best_strategies.(v))).(v) > 0
			then best_strategies.(v) <- i;
		) game;
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
				
				Array.iteri (fun i (pr, pl, tr, _) ->
					if (pl = 1) && ArrayUtils.exists tr (fun _ j -> counter_strategy.(i) != j && (TreeSet.mem j !non_final_nodes || TreeSet.mem (i,j) !used_escape_edges))
					then pg_set_tr graph i [||]
				) game;
				

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

let improvement_policy_single_randomly game node_total_ordering old_strategy valu =
	let strategy = Array.copy old_strategy in
	let cmp i j =
		node_valuation_ordering game node_total_ordering valu.(i) valu.(j)
	in
	let edges = ref [] in
	Array.iteri (fun i j ->
		if j != -1 then
			Array.iter (fun k ->
				if cmp j k < 0 then edges := (i,k)::!edges
			) (pg_get_tr game i)
	) strategy;
	let edges_arr = Array.of_list !edges in
	let len = Array.length edges_arr in
	if len > 0 then (
		let (i, j) = edges_arr.(Random.int len) in
		strategy.(i) <- j
	);
	strategy

let improvement_policy_single_node_edge_randomly game node_total_ordering old_strategy valu =
	let strategy = Array.copy old_strategy in
	let cmp i j =
		node_valuation_ordering game node_total_ordering valu.(i) valu.(j)
	in
	let node_edges = ref [] in
	Array.iteri (fun i j ->	if j != -1 then (
        let edges = ref [] in
        Array.iter (fun k ->
            if cmp j k <= 0 then edges := k::!edges
        ) (pg_get_tr game i);
        node_edges := (i, !edges)::!node_edges
	)) strategy;
	let node_edges_arr = Array.of_list !node_edges in
	let len = Array.length node_edges_arr in
	if len > 0 then (
		let (i, edges) = node_edges_arr.(Random.int len) in
        let edges_arr = Array.of_list edges in
        let len = Array.length edges_arr in
        if len > 0 then (
            let j = edges_arr.(Random.int len) in
            strategy.(i) <- j
        )
	);
	strategy

let improvement_policy_all_randomly game node_total_ordering old_strategy valu =
	let n = pg_size game in
	let strategy = Array.copy old_strategy in
	let cmp i j =
		node_valuation_ordering game node_total_ordering valu.(i) valu.(j)
	in
	let improvable = ref true in
	let same = ref true in
	while (!improvable && !same) do
		improvable := false;
        for i = 0 to n - 1 do
            if strategy.(i) > -1 then (
                let a = ArrayUtils.filter (fun j -> cmp strategy.(i) j <= 0) (pg_get_tr game i) in
                improvable := !improvable || (Array.length a > 1);
                strategy.(i) <- a.(Random.int (Array.length a));
                same := !same && (strategy.(i) = old_strategy.(i))
            )
        done
    done;
	strategy


let improvement_policy_optimize_roundrobin game node_total_ordering (e, next) old_strategy valu =
	let strategy = Array.copy old_strategy in
	let e = ref e in
	let fnd = ref false in
	while (not !fnd) do
		let (v,w) = !e in
		if node_valuation_ordering game node_total_ordering valu.(old_strategy.(v)) valu.(w) < 0 then (
			strategy.(v) <- w;
			fnd := true
		)
		else
			e := next !e;
	done;
	(strategy, (!e, next))
	
let improvement_policy_optimize_roundrobin_default_first_edge game v =
	let n = pg_size game in
	let i = ref ((v+1) mod n) in
	while (pg_get_pl game !i = 1) do
		i := (!i + 1) mod n
	done;
	(!i, (pg_get_tr game !i).(0))

let improvement_policy_optimize_roundrobin_default_next game (v,w) =
	let tr = pg_get_tr game v in
	let idx = ref 0 in
	while (tr.(!idx) <> w) do
		incr idx
	done;
	if !idx + 1 < Array.length tr then (v,tr.(!idx+1))
	else improvement_policy_optimize_roundrobin_default_first_edge game v
	
let improvement_policy_optimize_roundrobin_ordering_next game (ordering, backtransform) (v,w) =
	backtransform.(((TreeMap.find (v,w) ordering)+1) mod (Array.length backtransform))
		
let improvement_policy_optimize_roundrobin_ordering_first game (ordering, backtransform) =
	backtransform.(0)

let improvement_policy_optimize_roundrobin_ordering_build game cmp =
	let l = ref [] in
	Array.iteri (fun v (_, pl, tr, _) ->
		if (pl = 0) then
			Array.iter (fun w ->
				l := (v,w)::!l
			) tr
	) game;
	let a = Array.of_list !l in
	Array.sort cmp a;
	let m = ref (TreeMap.empty compare) in
	Array.iteri (fun i e ->
		m := TreeMap.add e i !m
	) a;
	(!m, a)

let lower_bound_construction_round_robin_compare game (v,w) (v',w') =
	let f i =
		match pg_get_desc game i with
			None -> ('!', 0, 0)
		|	Some s -> 
				if String.length s = 1
				then (String.get s 0, -1, -1)
				else if String.get s 1 = '('
				then let t = StringUtils.explode (StringUtils.rest_string s 2) ',' in
 				     let x = int_of_string (List.hd t) in
					 let y = int_of_string (List.hd (StringUtils.explode (List.hd (List.tl t)) ')')) in
					 (String.get s 0, x, y)
				else (String.get s 0, int_of_string (StringUtils.rest_string s 1), -1)
	in
	let g = function
		('d','e') -> 0
	|	('d','d') -> 0
	|	('u',_) -> 1
	|	('d','p') -> 2
	|	('d','u') -> 2
	|	('w',_) -> 3
	|	_ -> -1
	in
		let (vv,vi,vij) = f v in
		let (ww,_,_) = f w in
		let (vv',vi',vij') = f v' in
		let (ww',_,_) = f w' in
		let vx = g (vv,ww) in
		let vx' = g (vv',ww') in
		let (c,ci,cij) = (compare vx vx', compare vi vi', compare vij vij') in
		if c != 0 then c
		else match vx with
				0 -> if ci = 0 then cij else ci
			|	1 -> -ci
			|	2 -> -cij
			|	3 -> -ci
			|	_ -> 0

let lower_bound_construction_round_robin_compare_exp game (v,w) (v',w') =
	let f i =
		match pg_get_desc game i with
			None -> ('!', 0, 0)
		|	Some s -> 
				if String.length s = 1
				then (String.get s 0, -1, -1)
				else if String.get s 1 = '('
				then let t = StringUtils.explode (StringUtils.rest_string s 2) ',' in
 				     let x = int_of_string (List.hd t) in
					 let y = int_of_string (List.hd (StringUtils.explode (List.hd (List.tl t)) ')')) in
					 (String.get s 0, x, y)
				else (String.get s 0, int_of_string (StringUtils.rest_string s 1), -1)
	in
	let g = function		
		('e','x') -> 1   (* complex ordering ... *)
	|	('b', _ ) -> 1	 (* complex ordering ... *)
	|	('d','b') -> 1   (* complex ordering ... *)
	|	('c', _ ) -> 2   (* ordering irrelevant *)
	|	('e','p') -> 3   (* ordering irrelevant *)
	|	('d','x') -> 4   (* ordering irrelevant *)
	|	('a', _ ) -> 5   (* top-down ordering *)
	|	_ -> -1
	in
	let h = function
		('e',_) -> 2
	|	('b',_) -> 0
	|	('d',_) -> 1
	|	_ -> -1
	in
		let (vv,vi,vij) = f v in
		let (ww,_,_) = f w in
		let (vv',vi',vij') = f v' in
		let (ww',_,_) = f w' in
		let vx = g (vv,ww) in
		let vx' = g (vv',ww') in
		let (c,ci,cij) = (compare vx vx', compare vi vi', compare vij vij') in
		let h_cmp = compare (h (vv,ww)) (h (vv',ww')) in
		if c != 0 then c
		else match vx with
				5 -> -ci
			|	1 -> if ci != 0
				     then ci
				     else h_cmp
			|	_ -> 0

			
let improvement_policy_optimize_fair_default_tie_break game node_total_ordering _ _ valu =
	ListUtils.min_elt (fun (_, _, k) (_, _, k') ->
		node_valuation_ordering game node_total_ordering valu.(k') valu.(k)
	)
	
let improvement_policy_optimize_fair tie_break
                                     game node_total_ordering occ old_strategy valu =
    let msg_tagged_nl v = message_autotagged_newline v (fun _ -> "STRIMPR_FAIR") in
	let desc i = match (pg_get_desc game i) with Some s -> s | None -> string_of_int i in

    msg_tagged_nl 3 (fun _ ->
    	"Occ: " ^ 
    	ArrayUtils.formati (fun i a -> desc i ^ ":" ^
    		let tr = pg_get_tr game i in
    		ArrayUtils.formati (fun j k ->
    			desc tr.(j) ^ ":" ^ string_of_int k
    		) a
    	) occ
    	^ "\n"
    );
	
    let strategy = Array.copy old_strategy in
	let l = ref [] in
	let minvalue = ref (-1) in
	Array.iteri (fun i (_, pl, tr, _) ->
		if pl = 0 then
			Array.iteri (fun j k ->		
				if node_valuation_ordering game node_total_ordering valu.(strategy.(i)) valu.(k) < 0 then (
					if !minvalue = -1 then minvalue := occ.(i).(j);
					if !minvalue = occ.(i).(j) then l := (i,j,k)::!l
					else if !minvalue > occ.(i).(j) then (
						l := [(i,j,k)];
						minvalue := occ.(i).(j)
					)
				)
			) tr
	) game;
	msg_tagged_nl 3 (fun _ -> "Occurrence-Arena: " ^ ListUtils.format (fun (i,_,k) -> desc i ^ "->" ^ desc k) (List.rev !l) ^ "\n");
	if !l != [] then (
		let (i,j,k) = tie_break game node_total_ordering occ old_strategy valu !l in 
		strategy.(i) <- k;
		occ.(i).(j) <- occ.(i).(j) + 1
	);
	(strategy, occ)	

	
let my_count_sub_exp = ref 0
let last_check_sub_exp = ref ""
		
let improvement_policy_optimize_fair_sub_exp_tie_break game _ occ old_strategy valu l =
    let msg_tagged_nl v = message_autotagged_newline v (fun _ -> "STRIMPR_FAIR") in

	let desc i = OptionUtils.get_some (pg_get_desc game i) in
	let find s =
		let i = ref 0 in
		while (!i < pg_size game) && (desc !i <> s) do
			incr i
		done;
		if !i < pg_size game then Some !i else None
	in
	let leadsto i j =
		let (_, path, _) = valu.(OptionUtils.get_some i) in
		TreeSet.mem (OptionUtils.get_some j) path
	in
	
	let n = ref 0 in
	while (find ("E" ^ string_of_int !n) != None) do incr n done;
	let n = !n in
	let s = ref "" in
	
	let state = Array.make n (0,0) in
	
	for i = 0 to n - 1 do
		let fndfst = ref 0 in
		if (leadsto (find ("a" ^ string_of_int i)) (find ("E" ^ string_of_int i))) &&
		   (leadsto (find ("b" ^ string_of_int i)) (find ("E" ^ string_of_int i))) then
		(
		   fndfst := 1;
           s := "1" ^ !s;
		)
        else s := "0" ^ !s;
		let fndsnd = ref 0 in
		if (i < n-1) && (leadsto (find ("v" ^ string_of_int i)) (find ("X" ^ string_of_int i))) &&
		   (leadsto (find ("w" ^ string_of_int i)) (find ("X" ^ string_of_int i))) then
		(
		   fndsnd := 1;
           s := "1" ^ !s;
		)
        else s := "0" ^ !s;
		s := "|" ^ !s;
		state.(i) <- (!fndsnd,!fndfst)
    done;
	
	let r = ref "" in
	let state' = Array.make (n+1) 0 in
	state'.(n) <- 0;
	for i = n - 1 downto 0 do
		let (s,f) = state.(i) in
		state'.(i) <- state'.(i+1)*s + (1-state'.(i+1)) * f;
		r := !r ^ string_of_int state'.(i)
	done;
	let idxmap = Array.make n 0 in
	let zeros = ref 0 in
	for i = 0 to n - 1 do
		if state'.(i) = 0 then incr zeros;
	done;
	let zeroidx = ref 0 in
	let oneidx = ref !zeros in
	for i = 0 to n - 1 do
		if state'.(i) = 1 then (
			idxmap.(i) <- !oneidx;
			incr oneidx
		)
		else (
			idxmap.(i) <- !zeroidx;
			incr zeroidx
		)
	done;
	
	if !last_check_sub_exp <> !r then (
		last_check_sub_exp := !r;
		incr my_count_sub_exp;
	);
    
    msg_tagged_nl 3 (fun _ -> "\n\rState: " ^ !s ^ " / " (*^ !t ^ " / "*) ^ !r ^ " = " ^ string_of_int !my_count_sub_exp ^ "        \n");

	let compare_nodes k (soulab0, souidx0) (tarlab0, taridx0) (oldlab0, oldidx0) (soulab1, souidx1) (tarlab1, taridx1) (oldlab1, oldidx1)
					  state idxmap strat =
		let nn = 2 * k in
		let f i = 2 * idxmap.(i) + state.(i+1) in
		let g i = 2 * idxmap.(i) + 1 - state.(i+1) in
		let h c d i = if fst (strat (c ^ string_of_int i)) = d then 1 else 0 in
		let mp = function
		|	(('m',_),_,_)                       		    			-> -3
		|	(('p',_),_,_)                       		    			-> -2
		|	(('q',_),_,_)                       		    			-> -2
		|	(('d',i),_,_)                       		    			-> -1
		|	(('a',_),_,('E',_)) (* CycleNodeX1 *) 		    			-> 0
		|	(('w',_),_,('X',_)) (* CycleNodeX2 *)  		 				-> 0
		|	(('b',_),_,('E',_)) (* CycleNodeY1 *)  		    			-> 0
		|	(('v',_),_,('X',_)) (* CycleNodeY2 *)  		    			-> 0
		|	(('a',i),('E',_),_) (* CycleNodeX1 *) -> 2 + (h "b" 'E' i) * nn + f i
		|	(('w',i),('X',_),_) (* CycleNodeX2 *) -> 2 + (h "v" 'X' i) * nn + g i
		|	(('b',i),('E',_),_) (* CycleNodeY1 *) -> 2 + (h "a" 'E' i) * nn + f i
		|	(('v',i),('X',_),_) (* CycleNodeY2 *) -> 2 + (h "w" 'X' i) * nn + g i
		|	(('a',_),_,_) (* CycleNodeX1 *) 		 				    -> 1
		|	(('w',_),_,_) (* CycleNodeX2 *)  		 					-> 1
		|	(('b',_),_,_) (* CycleNodeY1 *)  		    				-> 1
		|	(('v',_),_,_) (* CycleNodeY2 *)  		    				-> 1
		|	(('l',_),_,_) (* Helper *)		    						-> 4 + 2 * nn
		|	(('g',_),_,_) (* UpDown1 *)		    						-> 4 + 2 * nn
		|	(('s',_),_,_) (* UpDown2 *)		    						-> 4 + 2 * nn
		|	(('f',_),_,_) (* DownSelector *) 							-> 3 + 2 * nn
		|	(('c',_),_,_) (* UpperSelector1 *) 							-> 4 + 2 * nn
		|	(('u',_),_,_) (* UpperSelector2 *) 							-> 4 + 2 * nn
		|	_ -> failwith "imp"
		in	
		compare (mp ((soulab0, souidx0), (tarlab0, taridx0), (oldlab0, oldidx0))) (mp ((soulab1, souidx1), (tarlab1, taridx1), (oldlab1, oldidx1)))
	in
	let f i =
		match pg_get_desc game i with
			None -> ('!', 0)
		|	Some s -> 
				if String.length s = 1
				then (String.get s 0, 0)
				else if String.get s 1 = '('
				then (String.get s 0, 0)
				else (String.get s 0, int_of_string (StringUtils.rest_string s 1))
	in
	ListUtils.min_elt (fun (i0,j0,k0) (i1,j1,k1) ->
		compare_nodes (pg_size game) (f i0) (f k0) (f old_strategy.(i0)) (f i1) (f k1) (f old_strategy.(i1)) state' idxmap
		   (fun s -> f old_strategy.(OptionUtils.get_some (find s)))
	) l

		   

let improvement_policy_optimize_least_basic_iterations tie_break game node_total_ordering occ old_strategy valu =
	Array.iteri (fun i j ->
		let (_, pl, tr, _) = game.(i) in
		if pl = 0 then Array.iteri (fun k l ->
			if l = j then occ.(i).(k) <- occ.(i).(k) + 1
		) tr
	) old_strategy;
    let strategy = Array.copy old_strategy in
	let l = ref [] in
	let minvalue = ref (-1) in
	Array.iteri (fun i (_, pl, tr, _) ->
		if pl = 0 then
			Array.iteri (fun j k ->		
				if node_valuation_ordering game node_total_ordering valu.(strategy.(i)) valu.(k) < 0 then (
					if !minvalue = -1 then minvalue := occ.(i).(j);
					if !minvalue = occ.(i).(j) then l := (i,j,k)::!l
					else if !minvalue > occ.(i).(j) then (
						l := [(i,j,k)];
						minvalue := occ.(i).(j)
					)
				)
			) tr
	) game;
	if !l != [] then (
		let (i,j,k) = tie_break game node_total_ordering occ old_strategy valu !l in 
		strategy.(i) <- k
	);
	(strategy, occ)	
	
(* Select the improving edge that left the strategy least-recently. *)		
let improvement_policy_optimize_least_recently_basic tie_break game node_total_ordering occ old_strategy valu =
	Array.iteri (fun i ->
		Array.iteri (fun j k ->
			occ.(i).(j) <- if old_strategy.(i) = k then 0 else occ.(i).(j) + 1
		)
	) occ;
    let strategy = Array.copy old_strategy in
	let l = ref [] in
	let minvalue = ref (-1) in
	Array.iteri (fun i (_, pl, tr, _) ->
		if pl = 0 then
			Array.iteri (fun j k ->		
				if node_valuation_ordering game node_total_ordering valu.(strategy.(i)) valu.(k) < 0 then (
					if !minvalue = -1 then minvalue := occ.(i).(j);
					if !minvalue = occ.(i).(j) then l := (i,j,k)::!l
					else if !minvalue < occ.(i).(j) then (
						l := [(i,j,k)];
						minvalue := occ.(i).(j)
					)
				)
			) tr
	) game;
	if !l != [] then (
		let (i,j,k) = tie_break game node_total_ordering occ old_strategy valu !l in 
		strategy.(i) <- k
	);
	(strategy, occ)	
		
(* Select the improving edge that entered the strategy least-recently thus far.  *)	
let improvement_policy_optimize_least_recently_entered tie_break game node_total_ordering occ old_strategy valu =
    let strategy = Array.copy old_strategy in
	let l = ref [] in
	let minvalue = ref (-1) in
	let maxvalue = ref (-1) in
	Array.iteri (fun i (_, pl, tr, _) ->
		if pl = 0 then
			Array.iteri (fun j k ->		
				if node_valuation_ordering game node_total_ordering valu.(strategy.(i)) valu.(k) < 0 then (
					if !minvalue = -1 then minvalue := occ.(i).(j);
					maxvalue := max !maxvalue occ.(i).(j);
					if !minvalue = occ.(i).(j) then l := (i,j,k)::!l
					else if !minvalue > occ.(i).(j) then (
						l := [(i,j,k)];
						minvalue := occ.(i).(j)
					)
				)
			) tr
	) game;
	if !l != [] then (
		let (i,j,k) = tie_break game node_total_ordering occ old_strategy valu !l in 
		strategy.(i) <- k;
		occ.(i).(j) <- !maxvalue + 1
	);
	(strategy, occ)	
		
	
let improvement_policy_vorobyov2_init_edges game =
	let ed = ref (TreeSet.empty compare) in
	Array.iteri (fun i (pr, pl, tr, _) ->
		if (pr >= 0) && (pl = 0) then Array.iter (fun j ->
			ed := TreeSet.add (i,j) !ed
		) tr		
	) game;
	[(pg_copy game, !ed, TreeSet.empty compare, 0)]

let improvement_policy_vorobyov2 game node_compare
                                 stack
                                 strategy valu =
	let rec iterate = function
		[] -> (strategy, [])
	|	(ga, ed, av, nu)::stack -> (
		if strategy_improvable ga node_compare strategy valu then (
			let strate = ref (TreeSet.empty compare) in
			let ga' = Array.mapi (fun i (pr, pl, tr, de) ->
				if (pr >= 0) && (pl = 0) then strate := TreeSet.add (i,strategy.(i)) !strate;
				let tr' = if (pr >= 0) && (pl = 0) then [|strategy.(i)|] else tr in
				(pr, pl, tr', de)
			) ga in
			let entry = (ga', !strate, ed, max (TreeSet.cardinal !strate) (TreeSet.cardinal ed / 2)) in
			iterate (entry::(ga, ed, av, nu)::stack)
		)
		else if nu > 0 then (
			let impr_edges = Array.of_list (List.filter (fun (i,j) ->
				node_valuation_ordering ga node_compare valu.(j) valu.(strategy.(i)) > 0
			) (TreeSet.elements av)) in
			if Array.length impr_edges = 0 then iterate stack
			else (
				let (i,j) = impr_edges.(Random.int (Array.length impr_edges)) in
				let strategy' = Array.copy strategy in
				strategy'.(i) <- j;
				pg_set_tr ga i (Array.of_list (j::(Array.to_list (pg_get_tr ga i))));
				(strategy', (ga, TreeSet.add (i,j) ed, TreeSet.remove (i,j) av, nu - 1)::stack)
			)
		)
		else iterate stack
	)
	in	
	iterate stack                                 

                                

let improvement_policy_vorobyovordered game node_total_ordering
                                (edgearr, counter, stack, edgeord)
                                old_strategy valu =

	let strategy = Array.copy old_strategy in

	while !counter > 0 do
		let j = ref 0 in
		decr counter;
		let working = ref true in
		while !working do
			let (x,y) = edgeord.(!j) in
			let (i, tr) = edgearr.(x) in
			let k = ArrayUtils.index_of tr y in
			if k < !i then (
				incr j
			) else (
				working := false;
				let tmp = tr.(!i) in
				tr.(!i) <- tr.(k);
				tr.(k) <- tmp;
				incr i;
				stack := x::!stack
			)
		done;
	done;

	let unaltered = ref true in

	while (!stack != []) && !unaltered do
		let e = List.hd !stack in
		stack := List.tl !stack;
		incr counter;
		let (i, tr) = edgearr.(e) in
		decr i;
		let se = tr.(0) in
		let ee = tr.(!i) in
		if (node_valuation_ordering game node_total_ordering valu.(se) valu.(ee) < 0) then (
			strategy.(e) <- ee;
			tr.(0) <- ee;
			tr.(!i) <- se;
			unaltered := false
		)
	done;

	(strategy, (edgearr, counter, stack, edgeord));;


let improvement_policy_vorobyovordered_init_edges game init_strat =
	Random.self_init ();
	let counter = ref 0 in
	let ord = ref [] in
	let edgearr = Array.mapi (fun i (pr, pl, tr, _) ->
		if pl = 0 && pr >= 0 then (
			Array.iter (fun j -> ord := (i,j)::!ord) tr;
            let j = ArrayUtils.index_of tr init_strat.(i) in
            let tr' = Array.copy tr in
            tr'.(0) <- tr.(j);
            tr'.(j) <- tr.(0);
            counter := !counter + Array.length tr' - 1;
            (ref 1, tr')
        )
        else (ref 0, [||])
	) game in
	(edgearr, counter, ref [], (ArrayUtils.shuffle (Array.of_list !ord)))
	

let improvement_policy_randomizedbland game node_total_ordering ordering old_strategy valu =
	let strategy = Array.copy old_strategy in
	let cmp i j =
		node_valuation_ordering game node_total_ordering valu.(i) valu.(j)
	in
	let idx = ArrayUtils.find (fun (x,y) -> cmp y strategy.(x) > 0) ordering in
	let (x,y) = ordering.(idx) in
	strategy.(x) <- y;
	(strategy, ordering)

	
let improvement_policy_randomizedbland_init_edges game init_strat =
	Random.self_init ();
	let ord = ref [] in
	Array.iteri (fun i (pr, pl, tr, _) ->
		if pl = 0 && pr >= 0
		then Array.iter (fun j -> ord := (i,j)::!ord) tr;
	) game;
	(ArrayUtils.shuffle (Array.of_list !ord))
	
let improvement_policy_vorobyov game node_total_ordering
                                (edgearr, counter, stack)
                                old_strategy valu =

	let strategy = Array.copy old_strategy in

	while !counter > 0 do
		let j = ref (Random.int !counter) in
		decr counter;
		let k = ref 0 in
		let working = ref true in
		while !working do
			let (i, tr) = edgearr.(!k) in
			let c = Array.length tr - !i in
			if c > !j then (
				working := false;
				let h = !i + !j in
				let tmp = tr.(!i) in
				tr.(!i) <- tr.(h);
				tr.(h) <- tmp;
				incr i;
				stack := !k::!stack;
			)
			else (
				j := !j - c;
				incr k
			)
		done;
	done;

	let unaltered = ref true in

	while (!stack != []) && !unaltered do
		let e = List.hd !stack in
		stack := List.tl !stack;
		incr counter;
		let (i, tr) = edgearr.(e) in
		decr i;
		let se = tr.(0) in
		let ee = tr.(!i) in
		if (node_valuation_ordering game node_total_ordering valu.(se) valu.(ee) < 0) then (
			strategy.(e) <- ee;
			tr.(0) <- ee;
			tr.(!i) <- se;
			unaltered := false
		)
	done;

	(strategy, (edgearr, counter, stack));;


let improvement_policy_vorobyov_init_edges game init_strat =
	Random.self_init ();
	let counter = ref 0 in
	let edgearr = Array.mapi (fun i (pr, pl, tr, _) ->
		if pl = 0 && pr >= 0 then (
            let j = ArrayUtils.index_of tr init_strat.(i) in
            let tr' = Array.copy tr in
            tr'.(0) <- tr.(j);
            tr'.(j) <- tr.(0);
            counter := !counter + Array.length tr' - 1;
            (ref 1, tr')
        )
        else (ref 0, [||])
	) game in
	(edgearr, counter, ref [])


let vorobyov_map_multiplicity arr =
	let c = ref 0 in
	Array.iter (fun (_, m) -> c := !c + m) arr;
	let a = Array.make !c (-1) in
	let j = ref 0 in
	Array.iter (fun (e, m) ->
		for i = 1 to m do
			a.(!j) <- e;
			incr j
		done
	) arr;
	a

let policy_vorobyov_multiple_edges_init_edges game init_strat multiplicities =
	let comb a b = Array.mapi (fun i x -> (x, b.(i))) a in
	Random.self_init ();
	let counter = ref 0 in
	let edgearr = Array.mapi (fun i (pr, pl, tr, _) ->
		if pl = 0 && pr >= 0 then (
			let tr = vorobyov_map_multiplicity (comb tr multiplicities.(i)) in
            let j = ArrayUtils.index_of tr init_strat.(i) in
            let tr' = Array.copy tr in
            tr'.(0) <- tr.(j);
            tr'.(j) <- tr.(0);
            counter := !counter + Array.length tr' - 1;
            (ref 1, tr')
        )
        else (ref 0, [||])
	) game in
	(edgearr, counter, ref [])



let strategy_improvement' (game: paritygame)
						  (init_strat: initial_strategy_fun)
						  (node_compare: node_total_ordering_fun)
						  (impr_policy: 'a improvement_policy_fun)
						  (impr_policy_init: 'a)
						  (check_policy: bool)
						  (ident: string) =
	let game' = alternating_transformation game true in
	let game'' = cheap_escape_cycles_transformation game' false in
	let (sol'', strat'') = strategy_improvement game'' init_strat node_compare impr_policy impr_policy_init check_policy ident in
	let sol = Array.init (Array.length game') (fun i -> sol''.(i)) in
	let strat = Array.init (Array.length game') (fun i -> strat''.(i)) in
	let (sol', strat') = alternating_revertive_restriction game game' sol strat in
	for i = 0 to Array.length game - 1 do
		let (_, pl, _, _) = game.(i) in
		if sol'.(i) != pl then strat'.(i) <- -1
	done;
	(sol', strat');;

let strategy_improvement'' (game: paritygame)
						  (init_strat: initial_strategy_fun)
						  (node_compare: node_total_ordering_fun)
						  (impr_policy: 'a improvement_policy_fun)
						  (impr_policy_init: (paritygame -> 'a))
						  (check_policy: bool)
						  (ident: string) =
	let game' = cheap_escape_cycles_transformation game false in
	let (sol', strat') = strategy_improvement game' init_strat node_compare impr_policy (impr_policy_init game') check_policy ident in
	let sol = Array.init (Array.length game) (fun i -> sol'.(i)) in
	let strat = Array.init (Array.length game) (fun i -> strat'.(i)) in
	(sol, strat);;

let improvement_policy_no_user_data imprpolicy = fun g o u s v -> (imprpolicy g o s v, u);;





let strategy_improvement_by_policy game impr_policy impr_policy_init check_policy ident =
	strategy_improvement' game initial_strategy_by_last_edge node_total_ordering_by_position impr_policy impr_policy_init check_policy ident

let strategy_improvement_optimize_all_locally_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_no_user_data improvement_policy_optimize_all_locally) () false "STRIMPR_LOCOPT";;

let strategy_improvement_cycle_avoid game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_no_user_data improvement_policy_cycle_avoid) () false "STRIMPR_INTONE";;

let strategy_improvement_cycle_enforce game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position improvement_policy_cycle_enforce ((TreeSet.empty cycle_enforce_cycles_compare), 0) false "STRIMPR_INTTWO";;

let strategy_improvement_by_counterstrategy_policy game =
	strategy_improvement_by_policy game (improvement_policy_no_user_data improvement_policy_by_counterstrategy) () false "STRIMPR_BYCOU";;

let strategy_improvement_single_randomly_policy game =
	Random.self_init ();
	strategy_improvement game initial_strategy_by_last_edge node_total_ordering_by_position (improvement_policy_no_user_data improvement_policy_single_randomly) () false "STRIMPR_SIRAND";;

let strategy_improvement_single_node_edge_randomly_policy game =
	Random.self_init ();
	strategy_improvement_by_policy game (improvement_policy_no_user_data improvement_policy_single_node_edge_randomly) () false "STRIMPR_SINOED";;

let strategy_improvement_all_randomly_policy game =
	Random.self_init ();
	strategy_improvement_by_policy game (improvement_policy_no_user_data improvement_policy_all_randomly) () false "STRIMPR_RANDOM";;

let strategy_improvement_optimize_all_globally_policy game =
	strategy_improvement_by_policy game (improvement_policy_no_user_data improvement_policy_optimize_all_globally) () false "STRIMPR_GLOOPT";;

let strategy_improvement_optimize_best_locally_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_no_user_data improvement_policy_optimize_best_locally) () false "STRIMPR_LOCOPTBEST";;

let strategy_improvement_optimize_worst_locally_policy game =
	strategy_improvement_by_policy game (improvement_policy_no_user_data improvement_policy_optimize_worst_locally) () false "STRIMPR_LOCOPTWORST";;

let strategy_improvement_learn_strategies game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position improvement_policy_learn_strategies (TreeSet.empty (fun x y -> compare (Array.to_list x) (Array.to_list y))) true "STRIMPR_STRLEA";;

let strategy_improvement_smart_policy game =
	(*strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_learn_cycles improvement_policy_smart) (TreeSet.empty compare, TreeSet.empty compare) true "STRIMPR_SMART";; *)
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_level) (0) true "STRIMPR_SMART";;

let strategy_improvement_justlearn_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_learn_cycles (fun a b c d e f -> (improvement_policy_optimize_all_locally a b d e, c))) (TreeSet.empty compare, TreeSet.empty compare) true "STRIMPR_JULE";;

let strategy_improvement_snare_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position (improvement_policy_snare_based improvement_policy_optimize_all_locally default_snare_sel) ([], None) true "STRIMPR_SNARE";;


let strategy_improvement_optimize_roundrobin_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	improvement_policy_optimize_roundrobin
	(improvement_policy_optimize_roundrobin_default_first_edge game (-1),
	 improvement_policy_optimize_roundrobin_default_next game) false "STRIMPR_ROUNDROBIN";;

	 
let strategy_improvement_optimize_roundrobin_policy_lower_bound game =
	let lookup_node s =
        let check i =
            match (pg_get_desc game i) with
                None -> false
            |   Some t -> s=t
        in
        let i = ref 0 in
        let n = Array.length game in
        while (!i < n) && (not (check !i)) do
            incr i
        done;
        if !i < n then Some !i else None
    in
    let is_exp = lookup_node "e0" != None in
	let cmp = if is_exp
	          then lower_bound_construction_round_robin_compare_exp game
	          else lower_bound_construction_round_robin_compare game
	in
	let struc = improvement_policy_optimize_roundrobin_ordering_build game cmp in
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	improvement_policy_optimize_roundrobin
	(improvement_policy_optimize_roundrobin_ordering_first game struc,
	 improvement_policy_optimize_roundrobin_ordering_next game struc) false "STRIMPR_ROUNDROBIN";;

	
	 
let strategy_improvement_optimize_fair_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	                     (improvement_policy_optimize_fair improvement_policy_optimize_fair_default_tie_break) (
		Array.map (fun (_, pl, tr, _) ->
			if pl = 1 then [||]
			else Array.make (Array.length tr) 0
		) game
	) false "STRIMPR_FAIR";;

let strategy_improvement_optimize_fair_sub_exp_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position 
                         (improvement_policy_optimize_fair improvement_policy_optimize_fair_sub_exp_tie_break) (
		Array.map (fun (_, pl, tr, _) ->
			if pl = 1 then [||]
			else Array.make (Array.length tr) 0
		) game
	) false "STRIMPR_FAIRSE";;

let strategy_improvement_optimize_least_basic_iterations_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	                     (improvement_policy_optimize_least_basic_iterations improvement_policy_optimize_fair_default_tie_break) (
		Array.map (fun (_, pl, tr, _) ->
			if pl = 1 then [||]
			else Array.make (Array.length tr) 0
		) game
	) false "STRIMPR_LBI";;

let strategy_improvement_optimize_least_recently_basic_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	                     (improvement_policy_optimize_least_recently_basic improvement_policy_optimize_fair_default_tie_break) (
		Array.map (fun (_, pl, tr, _) ->
			if pl = 1 then [||]
			else Array.make (Array.length tr) 0
		) game
	) false "STRIMPR_LRB";;

let strategy_improvement_optimize_least_recently_entered_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	                     (improvement_policy_optimize_least_recently_entered improvement_policy_optimize_fair_default_tie_break) (
		Array.map (fun (_, pl, tr, _) ->
			if pl = 1 then [||]
			else Array.make (Array.length tr) 0
		) game
	) false "STRIMPR_LRE";;

let strategy_improvement_vorobyov_policy game =
	let init = initial_strategy_by_best_reward game in
	strategy_improvement game (fun _ -> init) node_total_ordering_by_position improvement_policy_vorobyov (improvement_policy_vorobyov_init_edges game init) false "VOROBYOV";;

let strategy_improvement_vorobyovordered_policy game =
	let init = initial_strategy_by_best_reward game in
	strategy_improvement game (fun _ -> init) node_total_ordering_by_position improvement_policy_vorobyovordered (improvement_policy_vorobyovordered_init_edges game init) false "VOROBYOVORDERED";;

let strategy_improvement_randomizedbland_policy game =
	let init = initial_strategy_by_best_reward game in
	strategy_improvement game (fun _ -> init) node_total_ordering_by_position improvement_policy_randomizedbland (improvement_policy_randomizedbland_init_edges game init) false "RANDOMIZEDBLAND";;

let strategy_improvement_vorobyov2_policy game =
	strategy_improvement'' game initial_strategy_by_best_reward node_total_ordering_by_position improvement_policy_vorobyov2 (fun g -> improvement_policy_vorobyov2_init_edges g) false "VOROBYOV2";;





let subsolvermap = ref TreeMap.empty_def;;

let register_sub_solver solver_func identifier abbreviation description =
	if TreeMap.mem identifier !subsolvermap
	then failwith ("Solver `" ^ identifier ^ "' already registered!\n")
	else subsolvermap := TreeMap.add identifier (solver_func, abbreviation, description) !subsolvermap;;

let mem_sub_solver identifier = TreeMap.mem identifier !subsolvermap;;

let find_sub_solver identifier = TreeMap.find identifier !subsolvermap;;

let enum_sub_solvers it = TreeMap.iter (fun i (f, a, d) -> it f i a d) !subsolvermap;;

let fold_sub_solvers fo b = TreeMap.fold (fun i (f, a, d) x -> fo f i a d x) !subsolvermap b;;







register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_vorobyov_policy g)
	"randomfacet" "rf" "random facet policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_vorobyovordered_policy g)
	"randomfacetord" "rfo" "random facet ordered policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_vorobyov2_policy g)
	"dualrandomfacet" "drf" "dual random facet policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_randomizedbland_policy g)
	"randombland" "rb" "randomized blands rule policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_all_locally_policy g)
	"switchall" "sa" "standard switch all policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_all_globally_policy g)
	"switchbest" "sb" "best combination of switches policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_all_randomly_policy g)
	"switchhalf" "sh" "switch every edge with probabiliby 0.5 policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_single_randomly_policy g)
	"randomedge" "re" "random edge strategy iteration";;
(*
register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_single_node_edge_randomly_policy g)
	"strimprsinera" "sisn" "use strategy improvement with single node/edge randomized i.p.";;
*)
(*
let improvement_vorobyov_policy_multiple_edges game =
	multiple_edge_solve game (fun game mult ->
		let init = initial_strategy_by_best_reward game in
		strategy_improvement game (fun _ -> init) node_total_ordering_by_position improvement_policy_vorobyov (policy_vorobyov_multiple_edges_init_edges game init mult) false "VOROBYOVMULT"
	);;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) improvement_vorobyov_policy_multiple_edges g)
	"randomfacetmult" "rfm" "random facet policy iteration with multiple edge transf.";;
	*)

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_smart_policy g)
	"smartstratimpr" "ssi" "use smart strategy improvement";;


register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_learn_strategies g)
	"learnstratimpr" "lsi" "use strategy-learning strategy improvement";;

	
register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_least_basic_iterations_policy g)
	"switchlbi" "slbi" "Least basic iterations policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_least_recently_basic_policy g)
	"switchlrb" "slrb" "Least recently basic policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_least_recently_entered_policy g)
	"switchlre" "slre" "Least recently entered policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_fair_policy g)
	"switchfair" "sf" "Zadeh's fair policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_fair_sub_exp_policy g)
	"switchfairse" "sfse" "Zadeh's fair policy iteration with lower bound breaking ties";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_snare_policy g)
	"snarememo" "sm" "Snare memorization policy iteration";;

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
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_roundrobin_policy g)
	"switchroundrob" "sirr" "Round Robin policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_roundrobin_policy_lower_bound g)
	"switchroundrobse" "sirrse" "Round Robin policy iteration with lower bound ordering";;
	
	
(*
register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_best_locally_policy g)
	"strimprlocbest" "sibe" "use strategy improvement w. single best local optimization";;
*)
(*
register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_worst_locally_policy g)
	"strimprlocwrst" "siwo" "use strategy improvement w. single worst local optimization";;
*)

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_cycle_avoid g)
	"switchint" "swint" "switch internal #1";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_cycle_enforce g)
	"switchintx" "swintx" "switch internal #2";;


module CommandLine = struct
  let solve = ref (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_all_locally_policy g)
  
  let speclist = fold_sub_solvers (fun solve' ident abbrev desc t ->
                          (["--" ^ ident; "-" ^ abbrev], Unit (fun _ -> solve := solve'), "\n     " ^ desc)::t) []
  
  let parse s = 
  	SimpleArgs.parsearr s speclist (fun _ -> ()) "Policy Iteration Algorithm\n" SimpleArgs.argprint_help SimpleArgs.argprint_bad;
  	!solve

end ;;


open CommandLine ;;



let _ = register_solver_factory (fun s -> parse s) "policyiter" "pi" "use policy iteration (see additional args)";;

