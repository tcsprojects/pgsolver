open Tcsset;;
open Tcsarray;;
open Tcslist;;
open Tcsbasedata;;
open Paritygame;;
open Univsolve;;
open Transformations;;
open Basics;;

module LocalSolver = struct
	
	type 'a doubled = 'a * 'a
	
	type node_head =
		(node *                                                (* index *)
		 priority *												  (* priority *)
		 player)                                                 (* owning player *)

	type 'a node_body =
		Winning of 				player *                         (* player *)
		           				node                           (* strategy *)
	|	Default of              (bool ref) doubled *          (* expanded w.r.t. players *)
								(node TreeSet.t) ref *                (* all edges minus winning nodes *)
	                            (node TreeSet.t) ref *                (* known edges for owning player *)
	                            (node TreeSet.t) ref *                (* unknown edges for owning player *)
	                            ((node TreeSet.t) ref) doubled *      (* back edges *)
	                            'a doubled                    (* data *)
	
	type 'a nodex =
		node_head *
		'a node_body
	                            
	type 'a state =
		partial_paritygame *								  (* the game *)
		((node, 'a nodex) TreeMap.t) ref *							  (* graph *)
		((node TreeSet.t) ref) doubled *	     	        		  (* expansion sets *)
		((node TreeSet.t) ref *                                       (* added nodes *)
		 (node TreeSet.t) ref *                                       (* removed nodes *)
		 (node TreeSet.t) ref) doubled *                              (* border nodes *)
		(player -> node -> 'a)                                    (* init node map: pl -> v -> 'a *)
		
	let _by_player player (x,y) =
		if player = plr_Even then x else y
		
	let format_state ((_,gr,exp,change,_): 'a state) (f: 'a -> string) =
		let (exp0, exp1) = exp in
		let ((add0,rem0,bor0),(add1,rem1,bor1)) = change in
		let win0 = ref [] in
		let win1 = ref [] in
		let nod0 = ref [] in
		let nod1 = ref [] in
		TreeMap.iter (fun _ x ->
			match x with
				((idx,_,_), Winning (pl, str)) -> (
					let s = if pl = plr_Even then win0 else win1 in
					s := (if str < 0 then string_of_int idx else string_of_int idx ^ "->" ^ string_of_int str)::!s
				)
			|	((idx,_,_), Default (exp,_,_,_,_,(data0,data1))) -> (
					if !(fst exp) then nod0 := (string_of_int idx ^ " with " ^ f data0)::!nod0;
					if !(snd exp) then nod1 := (string_of_int idx ^ " with " ^ f data1)::!nod1
				)
		) !gr;
		"Expansion   0 : " ^ TreeSet.format string_of_int !exp0 ^ "\n" ^
		"Expansion   1 : " ^ TreeSet.format string_of_int !exp1 ^ "\n" ^
		"Added       0 : " ^ TreeSet.format string_of_int !add0 ^ "\n" ^ 
		"Added       1 : " ^ TreeSet.format string_of_int !add1 ^ "\n" ^ 
		"Removed     0 : " ^ TreeSet.format string_of_int !rem0 ^ "\n" ^ 
		"Removed     1 : " ^ TreeSet.format string_of_int !rem1 ^ "\n" ^ 
		"Border      0 : " ^ TreeSet.format string_of_int !bor0 ^ "\n" ^ 
		"Border      1 : " ^ TreeSet.format string_of_int !bor1 ^ "\n" ^
		"Winning     0 : " ^ ListUtils.format (fun s -> s) !win0 ^ "\n" ^ 
		"Winning     1 : " ^ ListUtils.format (fun s -> s) !win1 ^ "\n" ^
		"Graph       0 : " ^ ListUtils.format (fun s -> s) !nod0 ^ "\n" ^ 
		"Graph       1 : " ^ ListUtils.format (fun s -> s) !nod1 ^ "\n"
				
	let _touch (((_,_,data,_), gr, _, _, init_node): 'a state) v =
		if not (TreeMap.mem v !gr)
		then let (pr, pl) = data v in
		     let node = ((v, pr, pl), Default ((ref false, ref false), ref TreeSet.empty_def,
		                  ref TreeSet.empty_def, ref TreeSet.empty_def, (ref TreeSet.empty_def, ref TreeSet.empty_def),
		                  (init_node plr_Even v, init_node plr_Odd v))) in
		     gr := TreeMap.add v node !gr
		
	let init_state ((start, _, _, _) as pg) init_node =
		let state = (pg, ref TreeMap.empty_def, (ref (TreeSet.singleton_def start), ref (TreeSet.singleton_def start)),
		             ((ref TreeSet.empty_def, ref TreeSet.empty_def, ref TreeSet.empty_def),
		              (ref TreeSet.empty_def, ref TreeSet.empty_def, ref TreeSet.empty_def)),
		             init_node) in
		_touch state start;
		state
	
	let _get_node (_, gr, _, _, _) v =
		TreeMap.find v !gr
		
	let get_node_info st v =
		fst (_get_node st v)
		
	let _get_node_content st v =
		snd (_get_node st v)
		
	let is_solved state v =
		try
			match _get_node_content state v with
				Winning _ -> true
			|	_ -> false
		with
			Not_found -> false
	
	let get_game (pg,_,_,_,_) =
		pg
	
	let get_solved state v =
		match _get_node_content state v with
			Winning (x,y) -> (x,y)
		|	_ -> raise Not_found

	let _get_def_node st v =
		match _get_node_content st v with
			Default (a,b,c,d,e,f) -> (a,b,c,d,e,f)
		|	_ -> raise (Failure "not default node")
		
	let _get_def_node_with_info st v =
		match _get_node st v with
			(info, Default (a,b,c,d,e,f)) -> (info, (a,b,c,d,e,f))
		|	_ -> raise (Failure "not default node")

	let get_data (st: 'a state) pl v =
		let (_,_,_,_,_,data) = _get_def_node st v in
		_by_player pl data
		
	let get_expansion_set (_,_,exp,_,_) pl =
		!(_by_player pl exp)
		
	let get_edges st pl v =
		let ((_,_,pl'),(_,all,known,_,_,_)) = _get_def_node_with_info st v in
		if pl = pl' then !known else !all

	let get_back_edges st pl v =
		let (_,_,_,_,back,_) = _get_def_node st v in
		!(_by_player pl back)
	
	let pop_added_nodes (_,_,_,change,_) pl =
		let (add,_,_) = _by_player pl change in
		let s = !add in
		add := TreeSet.empty_def;
		s

	let pop_removed_nodes (_,_,_,change,_) pl =
		let (_,rem,_) = _by_player pl change in
		let s = !rem in
		rem := TreeSet.empty_def;
		s
		
	let pop_border_nodes (_,_,_,change,_) pl =
		let (_,_,bor) = _by_player pl change in
		let s = !bor in
		bor := TreeSet.empty_def;
		s

	let _dbg_msg_tagged v = message_autotagged v (fun _ -> "LOCALSOLVER")
	let _dbg_msg_plain = message

	let winning' (((_,gr,expset,changesets,_) as st): 'a state) player nodes_with_strat forbidden =
		let update_global_sets_remove_node v pl expanded =
			if expanded then (
				let (added,removed,border) = _by_player pl changesets in
				if TreeSet.mem v !added then added := TreeSet.remove v !added else removed := TreeSet.add v !removed;
				border := TreeSet.remove v !border
			)
			else (
				let s = _by_player pl expset in
				s := TreeSet.remove v !s
			)
		in
		let processed = ref nodes_with_strat in
		let todo = ref TreeSet.empty_def in
		TreeMap.iter (fun v _ -> todo := TreeSet.add v !todo) nodes_with_strat;
		if not (TreeSet.is_empty !todo) then (
			_dbg_msg_tagged 3 (fun _ -> "Winning " ^ TreeSet.format string_of_int !todo ^ " for player " ^ string_of_int (if player = plr_Even then 0 else 1) ^ "\n");
			while not (TreeSet.is_empty !todo) do
				let v = TreeSet.min_elt !todo in
				todo := TreeSet.remove v !todo;
				let (exp,all,known,unknown,back,_) = _get_def_node st v in
				update_global_sets_remove_node v plr_Even !(fst exp);
				update_global_sets_remove_node v plr_Odd !(snd exp);
				TreeSet.iter (fun w ->
					let (_,_,_,_,(backw0,backw1),_) = _get_def_node st w in
					backw0 := TreeSet.remove v !backw0;
					backw1 := TreeSet.remove v !backw1
				) (TreeSet.union !unknown (TreeSet.union !all !known));
				_dbg_msg_tagged 3 (fun _ -> "Check predecessors of " ^ string_of_int v ^ "...\n");
				TreeSet.iter (fun w ->
					if not (TreeMap.mem w !processed) then (
						let ((_,_,pl),(exp,all,known,unknown,_,_)) = _get_def_node_with_info st w in
						let attractor_predicate =
							(pl = player) ||
							(!(_by_player (plr_opponent pl) exp) && TreeSet.for_all (fun u -> TreeMap.mem u !processed) !all) ||
							(!(_by_player pl exp) && TreeSet.for_all (fun u -> TreeMap.mem u !processed) !known && TreeSet.is_empty !unknown)
						in
						if attractor_predicate && not (TreeMap.mem w forbidden) then (
							todo := TreeSet.add w !todo;
							processed := TreeMap.add w (if pl = player then v else -1) !processed;
							_dbg_msg_tagged 3 (fun _ -> "Node " ^ string_of_int w ^ " owned by player " ^ string_of_int (if pl = plr_Even then 0 else 1) ^ " lies in the attractor.\n")
						)
						else (
							if !(fst exp) then (
								let (_,_,border) = _by_player plr_Even changesets in
							    border := TreeSet.add w !border
							);
							if !(snd exp) then (
								let (_,_,border) = _by_player plr_Odd changesets in
							    border := TreeSet.add w !border
							);
							all := TreeSet.remove v !all;
							known := TreeSet.remove v !known;
							unknown := TreeSet.remove v !unknown
						)
					)
				) (TreeSet.union !(fst back) !(snd back));
			done;
			_dbg_msg_tagged 3 (fun _ -> "Attractor finished. Converting to winning nodes.\n");
			TreeMap.iter (fun v w ->
				let head = get_node_info st v in
				gr := TreeMap.add v (head, Winning (player, w)) !gr;
			) !processed
		)
	
	let winning st player nodes_with_strat =
		winning' st player nodes_with_strat TreeMap.empty_def

	let expand ((pg,_,expset,changesets,_) as st) player nodes =
		let expset = _by_player player expset in
		let (addedset,removedset,borderset) = _by_player player changesets in
		let (_,delta,_,_) = pg in
		_dbg_msg_tagged 3 (fun _ -> "Expand " ^ TreeSet.format string_of_int nodes ^ " for player " ^ string_of_int (if player = plr_Even then 0 else 1) ^ "\n");
		let winning_maps = (ref TreeMap.empty_def, ref TreeMap.empty_def) in
		let nodes = ref nodes in
		while not (TreeSet.is_empty !nodes) do
			let v = TreeSet.min_elt !nodes in
			nodes := TreeSet.remove v !nodes;
			_dbg_msg_tagged 3 (fun _ -> "Subexpand " ^ string_of_int v ^ " for player " ^ string_of_int (if player = plr_Even then 0 else 1) ^ "\n");
			let ((_,_,pl),(exp,all,known,unknown,back,_)) = _get_def_node_with_info st v in
			let exp = _by_player player exp in
			if !exp then failwith "already expanded";
			exp := true;
			expset := TreeSet.remove v !expset;
			addedset := TreeSet.add v !addedset;
			TreeSet.iter (fun w ->
				let ((_,_,pl'),(_,_,known',unknown',_,_)) = _get_def_node_with_info st w in
				if pl' = player then (
					known' := TreeSet.add v !known';
					unknown' := TreeSet.remove v !unknown';
					borderset := TreeSet.add w !borderset
				)
			) !(_by_player player back);
			let winning_node = (ref (-1), ref (-1)) in
			List.iter (fun w ->
				_touch st w;
				match _get_node_content st w with
					Winning (winner, _) ->
						let win_node = _by_player winner winning_node in
						win_node := w
				|	Default (expw,_,_,_,backw,_) -> (
						let backw = _by_player player backw in
						let expw = _by_player player expw in
						backw := TreeSet.add v !backw;
						if !expw then (
							let succ = if player = pl then known else all in
							succ := TreeSet.add w !succ
						)
						else if pl = player then (
							unknown := TreeSet.add w !unknown;
							expset := TreeSet.add w !expset
						)
						else (
							all := TreeSet.add w !all;
							expset := TreeSet.add w !expset;
							nodes := TreeSet.add w !nodes
						)
					)
			) (Enumerators.to_list (delta v));
			let w = !(_by_player pl winning_node) in
			let w' = !(_by_player (plr_opponent pl) winning_node) in
			if w != -1 then (
				let win_map = _by_player pl winning_maps in
			    win_map := TreeMap.add v w !win_map;
			    _dbg_msg_tagged 3 (fun _ -> "Node " ^ string_of_int v ^ " is won by player " ^ string_of_int (if pl = plr_Even then 0 else 1) ^ " by moving to " ^ string_of_int w ^ "\n")
			)
			else if (w' != -1) &&
			        ((pl = player && TreeSet.is_empty !known && TreeSet.is_empty !unknown) ||
				     (pl != player && TreeSet.is_empty !all)) then (
				let win_map = _by_player (plr_opponent pl) winning_maps in
			    win_map := TreeMap.add v (-1) !win_map;
			    _dbg_msg_tagged 3 (fun _ -> "Node " ^ string_of_int v ^ " is won by player " ^ string_of_int (if pl = plr_Odd then 0 else 1) ^ " because all successors are won by the same player.\n")
			)
		done;
		if not (TreeMap.is_empty !(fst winning_maps)) then (
			_dbg_msg_tagged 3 (fun _ -> "Performing winning-by-expansion for player 0.\n");
			winning' st plr_Even !(fst winning_maps) !(snd winning_maps)
		);
		if not (TreeMap.is_empty !(snd winning_maps)) then (
			_dbg_msg_tagged 3 (fun _ -> "Performing winning-by-expansion for player 1.\n");
			winning st plr_Odd !(snd winning_maps)
		)
		
	let iterate_nodes (_,gr,_,_,_) pl f =
		TreeMap.iter (fun v (_, body) ->
			match body with
				Default (exp, _, _, _, _, _) ->
					if !(_by_player pl exp) then f v
			|	_ -> ()
		) !gr
		
end;;


module StrategyImprovement = struct

	type 'a doubled = 'a * 'a

	type valuation =
		Top
	|	Bottom of (int TreeSet.t) * (int * int) TreeSet.t

	type node_data =
		node ref *          						(* Strategy *)
		valuation ref      						(* Valuation *)

	type state =
		node_data LocalSolver.state *			(* Local Solver State *)
		((node TreeSet.t) ref) doubled *				(* Nodes to be improved *)
		((node TreeSet.t) ref) doubled                  (* Nodes to be evaluated *)
		
	let _by_player player (x,y) =
		if player = plr_Even then x else y
		
	let _dbg_msg_tagged v = message_autotagged v (fun _ -> "LOCALSTRATIMPR")
	let _dbg_msg_plain = message
	
	let _format_valuation = function
		Top -> "TOP"
	|	Bottom (s, _) -> TreeSet.format string_of_int s
	
	let _format_node_data (strat, valu) =
		"(" ^ string_of_int !strat ^ ", " ^ _format_valuation !valu ^ ")"

	let format_state (state,imp,eva) =
		let (imp0, imp1) = imp in
		let (eva0, eva1) = eva in
		LocalSolver.format_state state _format_node_data ^		
		"Improveable 0 : " ^ TreeSet.format string_of_int !imp0 ^ "\n" ^
		"Improveable 1 : " ^ TreeSet.format string_of_int !imp1 ^ "\n" ^
		"Evaluatable 0 : " ^ TreeSet.format string_of_int !eva0 ^ "\n" ^ 
		"Evaluatable 1 : " ^ TreeSet.format string_of_int !eva1 ^ "\n"
		
	let _compare_node (_,_,data,_) v w =
		compare (fst (data v), v) (fst (data w), w)
	
	let get_valuation (state, _, _) pl v =
		if v = -1 then Bottom (TreeSet.empty_def, TreeSet.empty_def)
		else let (_, valu) = LocalSolver.get_data state pl v in !valu
		
	let compute_valuation (state, _, _) pl v = function
		Top -> Top
	|	Bottom (s, t) ->
			if TreeSet.mem v s then Top
			else (
				let (_, pr, _) = LocalSolver.get_node_info state v in
				let (without_pr, with_pr) = TreeSet.partition (fun (pr', _) -> pr' != pr) t in
				let t' = if TreeSet.is_empty with_pr
				         then TreeSet.add (pr, 1) without_pr
						 else TreeSet.add (pr, 1 + snd (TreeSet.min_elt with_pr)) without_pr
				in
				Bottom (TreeSet.add v s, t')
			)

	let compare_valuation (state, _, _) pl valu valu' =
		match (valu, valu') with
			(Top, _) ->
				if valu' = Top then 0 else 1
		|	(_, Top) ->
				-1
		|	(Bottom (_, t), Bottom (_, t')) -> (
				if TreeSet.equal t t' then 0
				else let ((pr, _) as e) = TreeSet.max_elt (TreeSet.sym_diff t t') in
				     if TreeSet.mem e t
				     then if plr_benefits pr = pl then 1 else -1
				     else if plr_benefits pr = pl then -1 else 1
			)
	
	let _equal_valuation valu valu' =
		match (valu, valu') with
			(Top, Top) -> true
		|	(Bottom (s, t), Bottom (s', t')) -> TreeSet.equal s s' && TreeSet.equal t t'
		|	_ -> false	

	(* Updates the to-be-evaluated set and the improved set s.t.
	   - the t-b-e set contain all new nodes, all border nodes and no removed nodes
	   - the imp set contain no removed nodes *)
	let process_change_set ((state, imp, eva) as st) pl =
		_dbg_msg_tagged 3 (fun _ -> "Processing Change Sets for player " ^ string_of_int (if pl = plr_Even then 0 else 1) ^ ".\n");
		let imp = _by_player pl imp in
		let eva = _by_player pl eva in
		let rem = LocalSolver.pop_removed_nodes state pl in
		let add = LocalSolver.pop_added_nodes state pl in
		let bor = LocalSolver.pop_border_nodes state pl in
		imp := TreeSet.diff !imp rem;
		eva := TreeSet.union (TreeSet.union add bor) (TreeSet.diff !eva rem);
		TreeSet.iter (fun v ->
			let (strat, valu) = LocalSolver.get_data state pl v in
			if TreeSet.mem !strat rem then (
				strat := -1;
				valu := get_valuation st pl (-1)
			)
		) bor
	
	let process_change_sets state =
		process_change_set state plr_Even;
		process_change_set state plr_Odd

	(* Sets the improvement edge, removes the node from the set of improvable nodes,
	   adds the node to the to-be-evaluated set and updates the respective strategy *)
	let improve (state, imp, eva) pl edges =
		_dbg_msg_tagged 3 (fun _ -> "Improve edges " ^ TreeSet.format (fun (x,y) -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")") edges ^ " for player " ^ string_of_int (if pl = plr_Even then 0 else 1) ^ ".\n");
		let imp = _by_player pl imp in
		let eva = _by_player pl eva in
		TreeSet.iter (fun (i,j) ->
			imp := TreeSet.remove i !imp;
			eva := TreeSet.add i !eva;
			let (strat, _) = LocalSolver.get_data state pl i in
			strat := j
		) edges

	(* Evaluates the t-b-e-set, updates the improved set and returns winning dominion
	   for player pl *)
	let evaluate (((state, imp, eva) as st): state) pl =
		let imp = _by_player pl imp in
		let eva = _by_player pl eva in
		let todo = ref !eva in
		eva := TreeSet.empty_def;
		if not (TreeSet.is_empty !todo) then (
			_dbg_msg_tagged 3 (fun _ -> "Evalute nodes " ^ TreeSet.format string_of_int !todo ^ " for player " ^ string_of_int (if pl = plr_Even then 0 else 1) ^ "\n");
			let changedval = ref TreeSet.empty_def in
			while not (TreeSet.is_empty !todo) do
				let v = TreeSet.min_elt !todo in
				_dbg_msg_tagged 3 (fun _ -> "Checking node " ^ string_of_int v ^ "... ");
				todo := TreeSet.remove v !todo;
				let (_,_,pl') = LocalSolver.get_node_info state v in
				let (strat, valu) = LocalSolver.get_data state pl v in
				let (valu', strat') =
					if pl' = pl then (compute_valuation st pl v (get_valuation st pl !strat), !strat)
					else let edges = LocalSolver.get_edges state pl v in
					     let strat' = ref (-1) in
					     let valu' = ref Top in
					     TreeSet.iter (fun w ->
					     	let valu'' = compute_valuation st pl v (get_valuation st pl w) in
					     	if (!strat' = -1) || (compare_valuation st pl valu'' !valu' < 0) then (
					     		strat' := w;
					     		valu' := valu''
					     	)
					     ) edges;
					     (!valu', !strat')
				in
				if !strat = strat' && _equal_valuation valu' !valu then (
					_dbg_msg_plain 3 (fun _ -> "keeping valuation " ^ _format_valuation !valu ^ " following node " ^ string_of_int !strat ^ "\n")
				)
				else (
					_dbg_msg_plain 3 (fun _ -> "updating valuation " ^ _format_valuation !valu ^ " to " ^ _format_valuation valu' ^ " following node " ^ string_of_int strat' ^ "\n");
					valu := valu';
					strat := strat';
					changedval := TreeSet.add v !changedval;
					todo := TreeSet.union !todo (LocalSolver.get_back_edges state pl v)
				)
			done;
			_dbg_msg_tagged 3 (fun _ -> "Extract winning nodes and update improvable set.\n");
			let check = ref !changedval in
			TreeSet.iter (fun v -> 
				TreeSet.iter (fun w -> check := TreeSet.add w !check) (LocalSolver.get_back_edges state pl v);
			) !changedval;
			let winningmap = ref TreeMap.empty_def in
			TreeSet.iter (fun v ->
				let (_,_,pl') = LocalSolver.get_node_info state v in
				let (strat, valu) = LocalSolver.get_data state pl v in
				if !valu = Top then (
					imp := TreeSet.remove v !imp;
					winningmap := TreeMap.add v (if pl' = pl then !strat else -1) !winningmap;
					_dbg_msg_tagged 3 (fun _ -> "Node " ^ string_of_int v ^ " is won.\n");					
				)
				else if pl' = pl then (
					let edges = LocalSolver.get_edges state pl v in
					let valu = get_valuation st pl !strat in
					let improvable = ref false in
				    TreeSet.iter (fun w ->
				    	if not (!improvable || w = !strat) then (
					    	let valu' = get_valuation st pl w in
					    	improvable := compare_valuation st pl valu' valu > 0;
				     	)
				    ) edges;
				    if !improvable then (
						_dbg_msg_tagged 3 (fun _ -> "Node " ^ string_of_int v ^ " is improvable.\n");
				    	imp := TreeSet.add v !imp
				    )
				    else imp := TreeSet.remove v !imp
				)
			) !check;
			!winningmap
		)
		else TreeMap.empty_def
		
	let get_local_solver_state (state, _, _) =
		state
	
	let get_improvement_set (_, imp, _) pl =
		!(_by_player pl imp)
		
	let init_state pg =
		let init_node _ _ = (ref (-1), ref (Bottom (TreeSet.empty_def, TreeSet.empty_def))) in
		let state = LocalSolver.init_state pg init_node in
		(state, (ref TreeSet.empty_def, ref TreeSet.empty_def), (ref TreeSet.empty_def, ref TreeSet.empty_def))
		
	let stable_win (state, _, _) pl =
		let mp = ref TreeMap.empty_def in
		LocalSolver.iterate_nodes state pl (fun v ->
			let (_, _, pl') = LocalSolver.get_node_info state v in
			let strat =
				if pl != pl' then !(fst (LocalSolver.get_data state pl v))
				else -1
			in
			mp := TreeMap.add v strat !mp
		);
		LocalSolver.winning state (plr_opponent pl) !mp
	
end;;

module StrategyImprovementMain = struct

	type switch_players_policy = StrategyImprovement.state -> player -> player (* data -> current player -> next player *)
	
	type improvement_policy = StrategyImprovement.state -> player -> (node * node) TreeSet.t (* data -> player -> (node, succ) set *)
	
	type expansion_policy = StrategyImprovement.state -> player -> (node TreeSet.t) (* data -> player -> new nodes *)
	
	let _dbg_msg_tagged v = message_autotagged v (fun _ -> "LOCALSTRATIMPRMAIN")
	let _dbg_msg_tagged_nl v = message_autotagged_newline v (fun _ -> "LOCALSTRATIMPRMAIN")
	let _dbg_msg_plain = message

	let custom_solve (pg: partial_paritygame)
	                 (switch_pol: switch_players_policy)
	                 (expand_pol: expansion_policy)
	                 (improvement_pol: improvement_policy) =
	                 
	    let state = StrategyImprovement.init_state pg in
	    let state' = StrategyImprovement.get_local_solver_state state in
	    let current_player = ref (switch_pol state plr_Odd) in
	    let iteration = ref 0 in
		let (start,_,_,_) = pg in
	    
		while not (LocalSolver.is_solved state' start) do
			incr iteration;
			_dbg_msg_tagged 3 (fun _ -> "Iteration " ^ string_of_int !iteration ^ " with current player " ^ string_of_int (if !current_player = plr_Even then 0 else 1) ^ "\n");
			_dbg_msg_tagged_nl 3 (fun _ -> StrategyImprovement.format_state state);
			let imp = StrategyImprovement.get_improvement_set state !current_player in
			let exp = LocalSolver.get_expansion_set state' !current_player in
			if (TreeSet.is_empty imp) && (TreeSet.is_empty exp) then (
				_dbg_msg_tagged 3 (fun _ -> "Strategy is stable, other player wins the known graph.\n");
				StrategyImprovement.stable_win state !current_player
			)
			else (
				if TreeSet.is_empty imp then (
					_dbg_msg_tagged 3 (fun _ -> "Improvement set is empty, expand.\n");
					LocalSolver.expand state' !current_player (expand_pol state !current_player);
					StrategyImprovement.process_change_sets state;
				)
				else (
					_dbg_msg_tagged 3 (fun _ -> "Improving.\n");
					StrategyImprovement.improve state !current_player (improvement_pol state !current_player);
					current_player := switch_pol state !current_player
				);
				_dbg_msg_tagged 3 (fun _ -> "Updating.\n");
				let changed0 = ref true in
				let changed1 = ref true in
				while !changed0 || !changed1 do
					changed0 := false;
					let win0 = StrategyImprovement.evaluate state plr_Even in
					if not (TreeMap.is_empty win0) then (
						changed0 := true;
						LocalSolver.winning state' plr_Even win0;
						StrategyImprovement.process_change_sets state
					);
					changed1 := false;
					let win1 = StrategyImprovement.evaluate state plr_Odd in
					if not (TreeMap.is_empty win1) then (
						changed1 := true;
						LocalSolver.winning state' plr_Odd win1;
						StrategyImprovement.process_change_sets state
					);
				done;
			)
		done;
		_dbg_msg_tagged_nl 3 (fun _ -> StrategyImprovement.format_state state);
	
	let sol i =
		try
	 		let (win, strat) = LocalSolver.get_solved state' i in
			if strat = -1 then (win, None) else (win, Some strat)
	 	with
	 		Not_found -> (plr_Even, None)
	in
	
	sol


	let default_switch_players _ = plr_opponent
	
	let default_expansion_policy st pl =
		let st' = StrategyImprovement.get_local_solver_state st in
		let s = TreeSet.elements (LocalSolver.get_expansion_set st' pl) in
		let a = Array.of_list s in
		TreeSet.singleton_def (a.(Random.int (Array.length a)))
		(*
		TreeSet.singleton_def (ListUtils.max_elt (fun i j ->
			let (_, pri, _) = LocalSolver.get_node_info st' i in
			let (_, prj, _) = LocalSolver.get_node_info st' j in
			let rewi = if pri mod 2 = pl then pri else -pri in
			let rewj = if prj mod 2 = pl then prj else -prj in
			compare rewi rewj
		) s)
		*)
		
	let default_improvement_policy st pl =
		let st' = StrategyImprovement.get_local_solver_state st in
		let s = TreeSet.elements (StrategyImprovement.get_improvement_set st pl) in
		let s' = List.map (fun i ->
			let edges = LocalSolver.get_edges st' pl i in
			let j = ref (-1) in
			TreeSet.iter (fun k ->
				let valuk = StrategyImprovement.get_valuation st pl k in
				let valuj = StrategyImprovement.get_valuation st pl !j in
				if StrategyImprovement.compare_valuation st pl valuk valuj > 0 then j := k
			) edges;
			(i, !j)
		) s in
		TreeSet.of_list compare s'
		(*
		let i = TreeSet.min_elt (StrategyImprovement.get_improvement_set st pl) in
		let edges = LocalSolver.get_edges st' pl i in
		let j = ref (-1) in
		TreeSet.iter (fun k ->
			let valuk = StrategyImprovement.get_valuation st pl k in
			let valuj = StrategyImprovement.get_valuation st pl !j in
			if StrategyImprovement.compare_valuation st pl valuk valuj > 0 then j := k
		) edges;
		TreeSet.singleton (i, !j)
		*)
		
	let default_solve (pg: partial_paritygame) =
		Random.self_init ();
		custom_solve pg default_switch_players default_expansion_policy default_improvement_policy
	
end;;
	
let partially_solve pg =
	partialpg_alternating_revertive_restriction (StrategyImprovementMain.default_solve (partialpg_alternating_transformation pg));;
	


let solve' game =
	partially_solve_dominion game 0 partially_solve;;	
	
let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;

let register _ =
    Solverregistry.register_partial_solver partially_solve "stratimprlocal" "sl" "use the local strategy improvement method [experimental]";
    Solverregistry.register_solver solve "stratimprlocal" "sl" "use the local strategy improvement method [experimental]";;
