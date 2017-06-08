open Basics ;;
open Paritygame ;;
open Univsolve;;
open Tcslist;;
open Tcsarray;;
open Tcsset;;
open Tcsbasedata;;



let partially_solve (game: partial_paritygame) =

    let msg_tagged v = message_autotagged v (fun _ -> "MODELCHECKER") in
    (*let msg_plain = message in*)

    let (start, delta, data, fmtnode') = game in
    let fmtnode n = match fmtnode' n with None -> "[None]" | Some s -> s in
    let defIndex = TreeSet.empty_def in
    
    let counter = ref 0 in
    let invalidSegments = (ref [], ref []) in
    
    let nodes = Hashtbl.create 10 in
    
    let getNodeInfo node =
      if Hashtbl.mem nodes node
      then Hashtbl.find nodes node
      else (([], []), [], None)
    in
    
    let setNodeInfo node info =
      if Hashtbl.mem nodes node
      then Hashtbl.replace nodes node info
      else Hashtbl.add nodes node info
    in
    
    let other = plr_opponent in
    let prioWinner = plr_benefits in

	let pairMember first (fir, sec) =
		if first = plr_Even then fir else sec
	in

	let addKnowledge ((node, index), b, m) =
		let ((abDec, elDec), jus, occ) = getNodeInfo node in
		let newBDecInfo = (!counter, index) in
		let newDecInfo = if b = plr_Odd then (abDec, newBDecInfo::elDec) else (newBDecInfo::abDec, elDec) in
			match m with
			  None -> setNodeInfo node (newDecInfo, jus, occ)
			| Some next -> setNodeInfo node (newDecInfo, next :: jus, occ)
	in

	let compatindex newidx oldidx (b: player) =
		if TreeSet.equal newidx oldidx then true
		else let ((pr, _) as e) = TreeSet.max_elt (TreeSet.sym_diff newidx oldidx) in
		     if TreeSet.mem e newidx
			 then b = prioWinner pr
			 else not (b = prioWinner pr)
	in

    let updateIndex prio index =
		let index = ref index in
		let stay = ref true in
		while !stay do
			if TreeSet.is_empty !index then (
				stay := false;
				index := TreeSet.add (prio, 1) !index
			)
			else let (pr, nu) = TreeSet.min_elt !index in
			     if pr <= prio then (
					index := TreeSet.remove (pr, nu) !index;
					if pr = prio then (
						index := TreeSet.add (pr, nu + 1) !index;
						stay := false
					)
				 )
				 else (
					stay := false;
					index := TreeSet.add (prio, 1) !index
				 )
		done;
		!index
    in

(*	
	let winsOn from towhere =
		let frommax = Dynarray.length from - 1 in
		let towheremax = Dynarray.length towhere - 1 in
		let rec g k =
			let from_k = if k <= frommax then Dynarray.get from k else 0 in
			let towhere_k = if k <= towheremax then Dynarray.get towhere k else 0 in
			if towhere_k > from_k
			then prioWinner k
			else g (k - 1)
		in
			g (max frommax towheremax)
	in
*)
	
	let winsOn from towhere =
		let (pr, _) = TreeSet.max_elt (TreeSet.diff towhere from) in
		prioWinner pr
	in

	let format_index index =
		let l = ref [] in
		TreeSet.iter (fun (p,o) ->
			l := (string_of_int p ^ (if o > 1 then "^" ^ string_of_int o else ""))::!l
		) index;
		ListUtils.format (fun s -> s) !l
	in
	
    let isValid (player: player) no =
      if no = 0
      then true
      else
        let segments = !(pairMember player invalidSegments) in
        let rec f = function
          [] -> true
        | (high,low)::t -> (high < no) || (low > no && f t)
        in f segments
    in

	let popNextMove node =
		let (dec, jus, occ) = getNodeInfo node in
			setNodeInfo node (dec, List.tl jus, occ)
	in

	let rec lookAt ((node, index) as position) (p: player) w = function
		[] -> (false, [])
	  | (((j,i)::t) as d) ->
	  		if isValid p j
	  		then (compatindex index i p, d)
	  		else (
	  			if w = p then popNextMove(node) else ();
	  			lookAt position p w t
	  		)
	in

	let setDecs node dec =
		let (_, jus, occ) = getNodeInfo node in
			setNodeInfo node (dec, jus, occ)
	in

	let getKnowledge ((node, index) as position) =
		let (_, chooser) = data node in
		let ((abDec, elDec), _, _) = getNodeInfo node in
		let (canUseAb, newAbDec) = lookAt position plr_Even (chooser: player) abDec in
		let (newDecs, result) =
			if canUseAb
			then ((newAbDec, elDec), Some(plr_Even))
			else let (canUseEl, newElDec) = lookAt position plr_Odd chooser elDec in
				 ((newAbDec, newElDec), if canUseEl then Some(plr_Odd) else None)
		in (
			setDecs node newDecs;
			result
		)
    in

	let setOccurrence (node, info) =
		let (dec, jus, _) = getNodeInfo node in
			setNodeInfo node (dec, jus, info)
	in

	let getOccurrence node =
		let (_, _, occ) = getNodeInfo node in
			occ
	in

    let invalidate (leastInvalidNo, player) =
      let top = !counter in
      let segments = pairMember player invalidSegments in
      let rec addTo = function
          [] ->  [(top, leastInvalidNo)]
        | (high, low)::t ->
          if low > leastInvalidNo
          then addTo t
          else if high < leastInvalidNo
          then (top, leastInvalidNo)::(high, low)::t
          else (top, low)::t
      in
      (segments := addTo (!segments);
       counter := !counter + 1)
    in

	let backtrackingThrough ((node, _), winner) =
		let loser = other winner in
			match (getOccurrence node) with
			  None -> failwith "impossible"
			| Some (_, c, pairUse) -> (
				setOccurrence (node, None);
				if !(pairMember loser pairUse)
				then invalidate (c, loser)
				else ()
			  )
	in

	let getWinsByRepeat (node, index) =
		let f (i, _, pairUse) =
			let winner = winsOn i index
			in ((pairMember winner pairUse) := true;
				winner)
		in
			match (getOccurrence node) with
			  None -> None
			| Some x -> Some (f x)
	in
	
	let format_decisions pl =
		let process p l = List.filter (fun (c, _) -> isValid p c) l in
		let format l =
			ListUtils.format (fun (c, i) -> "(" ^ format_index i ^ "," ^ string_of_int c ^ ")") l
		in
		let decs = ref [] in
		Hashtbl.iter (fun node ((abDec, elDec), _, _) ->
			let dec = process pl (if pl = plr_Even then abDec else elDec) in
			if (dec != []) then (
				decs := (fmtnode node ^ "->" ^ format dec)::!decs
			)
		) nodes;
		ListUtils.format (fun s -> s) !decs
	in
	
	let format_position (node, index) =
		"(" ^ fmtnode node ^ "," ^ format_index index ^ ")"
	in
	
	let format_game_list glist =
		ListUtils.format (fun (pos, t) ->
			let (time, ass) = match (getOccurrence (fst pos)) with
			  None -> failwith "impossible"
			| Some (_, c, (p0, p1)) ->
				(c, (if !p0 then (if !p1 then "{0,1}" else "{0}") else if !p1 then "{1}" else "{}"))
			in
			"(" ^ format_position pos ^ "," ^ ListUtils.format fmtnode (Enumerators.to_list t) ^ "," ^ string_of_int time ^ "," ^ ass ^ ")"
		) glist
	in

	let rec explore ((node, index) as position) glist =
		if !verbosity <= 2 then	msg_tagged 2 (fun _ -> "Counter #" ^ string_of_int !counter ^ "\r");
        msg_tagged 3 (fun _ -> "Explore " ^ format_position position ^ " at " ^ string_of_int !counter ^ "\n");
        msg_tagged 3 (fun _ -> "  Gamelist = " ^ format_game_list glist ^ "\n");
        msg_tagged 3 (fun _ -> "  Decisions0 = " ^ format_decisions plr_Even ^ "\n");
        msg_tagged 3 (fun _ -> "  Decisions1 = " ^ format_decisions plr_Odd ^ "\n\n");
		(*
		msg_plain 3 (fun _ -> "\n");
		msg_tagged 3 (fun _ -> "Exploring " ^ string_of_int node ^ ": ");
		*)
        counter := !counter + 1;
        match (getKnowledge position) with
          Some(winner) -> (
          	(*
			msg_plain 3 (fun _ -> "Applying decision for player " ^ string_of_int winner ^ "\n");
			*)
          	backtrack winner glist node
		)
        | None ->
        	match (getWinsByRepeat position) with
        	  Some(winner) -> (
        	  	(*
				msg_plain 3 (fun _ -> "Repeat win for player " ^ string_of_int winner ^ "\n");
				*)
        	  	backtrack winner glist node
			)
        	| None ->
				let enum = delta node in
				let (rest, node') = Enumerators.next enum in
(*				msg_plain 3 (fun _ -> "Exploring transition.\n"); *)
				setOccurrence (node, Some (index, !counter, (ref false, ref false)));
				explore (node', updateIndex (fst (data node')) index)
						((position, rest)::glist)

    and backtrack winner gameList whereWeCameFrom =
    	let rec keepBacktracking ((node, index) as position, maybeMove, glist) = (
    		backtrackingThrough ((node, index), winner);
    		addKnowledge (position, winner, maybeMove);
    		(*
			msg_tagged 3 (fun _ -> "Advance backtracking to " ^ string_of_int node ^ ": ");
			*)
    		chk (glist, node))
    	and chk (gameList, whereWeCameFrom) =
	    	msg_tagged 3 (fun _ -> "Backtracking " ^ fmtnode whereWeCameFrom ^ " w.r.t. player " ^ string_of_int (if winner = plr_Even then 0 else 1) ^ " at " ^ string_of_int !counter^ "\n");
	    	msg_tagged 3 (fun _ -> "  Gamelist = " ^ format_game_list gameList ^ "\n");
	        msg_tagged 3 (fun _ -> "  Decisions0 = " ^ format_decisions plr_Even ^ "\n");
	        msg_tagged 3 (fun _ -> "  Decisions1 = " ^ format_decisions plr_Odd ^ "\n\n");
	    	match (gameList, whereWeCameFrom) with
    	    ([], whence) -> (
    	    	(*
				msg_plain 3 (fun _ -> "Playlist is empty.\n");
				*)
				winner
			)
    	  | ((((node, index) as position, unexplored)::glist), whence) ->
				let (pr, chooser) = data node in
    	  	  	if winner = chooser then (
    	  	  		(*
					msg_plain 3 (fun _ -> "Winner owns current node.\n");
					*)
					keepBacktracking (position, Some whence, glist)
				)
    	  	  	else
					try
						let (rest,node') = Enumerators.next unexplored in
						(*
						msg_plain 3 (fun _ -> "Trying next transition for other player.\n");
						*)
						explore (node', updateIndex (fst (data node')) index)
								((position, rest)::glist)
					with
						Not_found -> (
    	  	  		   		(*
							msg_plain 3 (fun _ -> "No more transitions for other player.\n");
							*)
							keepBacktracking (position, None, glist)
					   )
    	in
			chk (gameList, whereWeCameFrom)

	in

	msg_tagged 2 (fun _ -> "Exploring graph, starting with " ^ string_of_int start ^ "...\n");

	let winner = explore (start, updateIndex (fst (data start)) defIndex) [] in

	msg_tagged 2 (fun _ -> "\n");

	let rec get_strategy_decision p d j =
		if isValid p (fst (List.hd d))
		then List.hd j
		else get_strategy_decision p (List.tl d) (List.tl j)
	in

		fun node ->
		 (winner,
	      if snd (data node) = winner
		  then Some (
		    let (decs, jus, _) = getNodeInfo node in
		    	get_strategy_decision winner (pairMember winner decs) jus)
		  else None);;



let solve' game =
	partially_solve_dominion game 0 partially_solve;;

let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;

let register _ =
    Solverregistry.register_partial_solver partially_solve "modelchecker" "mc" "use the model checking algorithm due to Stevens / Stirling";
    Solverregistry.register_solver solve "modelchecker" "mc" "use the model checking algorithm due to Stevens / Stirling";;

