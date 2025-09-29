open Basics ;;
open Paritygame ;;
open Univsolve;;
open Tcsarray;;
open Tcsqueue;;

let ns_minarg arr f less = ns_fold (fun m i -> if less (f i) (f m) then i else m) (ns_some arr) arr;;
let ns_maxarg arr f less = ns_fold (fun m i -> if less (f m) (f i) then i else m) (ns_some arr) arr;;

let arr_minarg arr f less = Array.fold_left (fun m i -> if less (f i) (f m) then i else m) (arr.(0)) arr;;
let arr_maxarg arr f less = Array.fold_left (fun m i -> if less (f m) (f i) then i else m) (arr.(0)) arr;;

let solve_scc_reach game player spmidx updspm =

    let n = pg_size game in
    let maxprio = pg_max_prio_for game (plr_opponent player) in

    let pr2spm pr = (pr / 2) in

    let maxprspm = pr2spm maxprio in


    let format_spm spm =
        let s = ref "(" in
        for i = 0 to (Array.length spm) - 1 do
            s := !s ^ string_of_int (fst spm.(i)) ^ "/" ^ string_of_int (snd spm.(i)) ^
                 (if i < (Array.length spm) - 1 then ", " else ")")
        done;
        !s
    in

	let format_spmidx spmidx =
		let s = ref "[" in
		for i = 0 to (Array.length spmidx) - 1 do
			s := !s ^ string_of_int i ^ ":" ^ format_spm spmidx.(i) ^ (if i < (Array.length spmidx) - 1 then ", " else "]")
		done;
		!s
	in


    message 3 (fun _ -> "\nNow solving the following scc: \n" ^ format_game game);
    message 3 (fun _ -> "\nInit small progress measure: \n" ^ format_spmidx spmidx ^ "\n");



    let isTop spm = Array.fold_left (fun r (v, m) -> r && (v = m)) true spm in

    let prog pr spmx spmy =
        let spmz = Array.copy spmx in
        if isTop spmy then (
            for i = 0 to maxprspm do
                spmz.(i) <- (snd spmz.(i), snd spmz.(i))
            done
        ) else (
            for i = 0 to maxprio do
                if (i < pr) then (
                    if (plr_benefits i != player) then spmz.(pr2spm i) <- (0, snd spmz.(pr2spm i))
                ) else if (i = pr) then (
                    if (plr_benefits i != player)
                    then spmz.(pr2spm i) <- (1 + fst spmy.(pr2spm i), snd spmz.(pr2spm i))
                ) else if (i > pr) && (plr_benefits i != player)
                       then spmz.(pr2spm i) <- (fst spmy.(pr2spm i), snd spmz.(pr2spm i))
            done;
            updspm spmz pr
        );
        spmz
    in

    let less pr spmx spmy =
        let x i = fst spmx.(i) in
        let y i = fst spmy.(i) in
        let lesscmp i = (x i < y i) || ((x i < snd spmx.(i)) && (y i = snd spmy.(i))) in
        let rec less' pr' =
            if pr' > pr then (
                if (plr_benefits pr' = player) || (x (pr2spm pr') = y (pr2spm pr'))
                then less' (pr' - 1)
                else lesscmp (pr2spm pr')
            ) else (plr_benefits pr' = player) || (lesscmp (pr2spm pr'))
        in (less' maxprio) && (not (isTop spmx))
    in

    let update queue i =
        if not (isTop spmidx.(i)) then (
          let pr = pg_get_priority game i in
	  let pl = pg_get_owner game i in
	  let delta = pg_get_successors game i in
            let j = (if pl = player then ns_minarg else ns_maxarg)
                     delta (fun q -> prog pr spmidx.(i) spmidx.(q)) (less 0) in
            let y = prog pr spmidx.(i) spmidx.(j) in
            let x = spmidx.(i) in
            if (less 0 x y) && not (x = y) then (
                message 3 (fun _ -> "Enqueuing predecessors of " ^ string_of_int i ^ " for " ^ plr_show player ^ "\n");
                ns_iter (fun j -> SingleOccQueue.add j queue) (pg_get_predecessors game i);
                spmidx.(i) <- y;
                message 3 (fun _ -> "Updating small progress measure for " ^ plr_show player ^ ": \n" ^ format_spmidx spmidx ^ "\n");
            )
        )
    in

    let work queue =
        if not (SingleOccQueue.is_empty queue) then (
            let i = SingleOccQueue.take queue in
            message 3 (fun _ -> "Dequeuing " ^ string_of_int i ^ " for " ^ plr_show player ^ "\n");
            update queue i
        )
    in



    let queue = SingleOccQueue.create () in
    for i = 0 to n - 1 do
        SingleOccQueue.add i queue;
    done;

    while not (SingleOccQueue.is_empty queue) do
        work queue;
    done;

    let sol = sol_create game in
    let strat = Array.make n (-1) in

    pg_iterate (fun i -> fun (_,ow,succs,_,_) -> message 3 (fun _ -> "Checking " ^ string_of_int i ^ "\n");
						 sol.(i) <- if isTop spmidx.(i) then plr_opponent player else player;
						 if (ow = player) && (player = sol.(i))
						 then strat.(i) <- ns_minarg succs (fun q -> spmidx.(q)) (less 0)) game;

    (sol, strat);;


(*
let solve_scc game player =
    let spmidx = Array.map (Array.map (fun m -> (0, m))) (compute_priority_reach_array game player) in

	let spmupd spmz _ =
		let l = Array.length spmz in
        for i = 0 to l - 1 do
            if (fst spmz.(i) > snd spmz.(i)) then (
                spmz.(i) <- (0, snd spmz.(i));
                if (i < l - 1)
                then spmz.(i + 1) <- (1 + fst spmz.(i + 1), snd spmz.(i + 1))
                else for j = 0 to l - 1 do
                        spmz.(j) <- (snd spmz.(j), snd spmz.(j))
                     done
            )
        done
    in

	solve_scc_reach game player spmidx spmupd;;


let solve game = universal_solve_by_player_solver (universal_solve_init_options_verbose !universal_solve_global_options) solve_scc game;;
*)

let solve' game =

	let msg_tagged v = message_autotagged v (fun _ -> "SMALLPROGRESS") in
	(* let msg_plain = message in *)

	let n = pg_size game in
    let maxprio = pg_max_prio game in
	
	let max_values = Array.make (maxprio + 1) (n + 1) in
	let spmidx = Array.init n (fun _ -> Array.make (maxprio + 1) 0) in

    let format_spm spm =
        let s = ref "(" in
        for i = 0 to (Array.length spm) - 1 do
            s := !s ^ string_of_int spm.(i) ^
                 (if i < (Array.length spm) - 1 then ", " else ")")
        done;
        !s
    in

	let format_spmidx spmidx =
		let s = ref "[" in
		for i = 0 to (Array.length spmidx) - 1 do
			let x = match pg_get_desc game i with None -> "" | Some t -> "(" ^ t ^ ")" in
			s := !s ^ string_of_int i ^ x ^ ":" ^ format_spm spmidx.(i) ^ (if i < (Array.length spmidx) - 1 then ", " else "]")
		done;
		!s
	in

    msg_tagged 3 (fun _ -> "\nNow solving the following scc: \n" ^ format_game game);
    msg_tagged 3 (fun _ -> "\nInit small progress measure: \n" ^ format_spmidx spmidx ^ "\n");

	(* Returns true iff the spm equals top w.r.t. player *)
	let is_top spm player =
		let res = ref true in
		let i = ref 0 in
		while !res && (!i <= maxprio) do
			if plr_benefits !i != player then res := spm.(!i) = max_values.(!i);
			incr i
		done;
		!res
	in
	
	let make_top spm player =
		for i = 0 to maxprio do
			if plr_benefits i != player then spm.(i) <- max_values.(i)
		done
	in

	(* Recalculates the value of spm w.r.t. player inplace
	   meaning that entries exceeding the maximum value are
	   set to zero while the next entry is incremented.
	   If the last entry w.r.t. player exceeds its maximum
	   value, the spm is set to top w.r.t. player *)
	let updspm spm player =
        for i = 0 to maxprio do
            if (plr_benefits i != player) && (spm.(i) > max_values.(i)) then (
                spm.(i) <- 0;
                if (i + 2 <= maxprio)
                then spm.(i + 2) <- 2 + spm.(i + 2)
                else for j = 0 to maxprio do
						if plr_benefits j != player then spm.(j) <- max_values.(j)
                     done
            )
        done
    in

	(* Returns the least progress measure spmz w.r.t. player s.t.
       spmz >=_pr spmy. The spm of 1 - player is taken from spmx. *)
    let prog player pr spmx spmy =
        let spmz = Array.copy spmx in
        if is_top spmy player then make_top spmz player
        else (
            for i = 0 to maxprio do
                if (i < pr) then (
                    if (plr_benefits i != player) then spmz.(i) <- 0
                ) else if (i = pr) then (
                    if (plr_benefits i != player)
                    then spmz.(i) <- 1 + spmy.(i)
                ) else if (i > pr) && (plr_benefits i != player)
                       then spmz.(i) <- spmy.(i)
            done;
            updspm spmz player
        );
        spmz
    in

    (* Returns true iff x <_pr y w.r.t. player *)
    let less player pr x y =
        let rec less' pr' =
            if pr' > pr then (
                if (plr_benefits pr' = player) || (x.(pr') = y.(pr'))
                then less' (pr' - 1)
                else x.(pr') < y.(pr')
            ) else (plr_benefits pr' != player) && (x.(pr') < y.(pr'))
        in (less' maxprio) && (not (is_top x player))
    in
	
	(* Calculates the progress measure update for both players at node vi *)
	let calc_prog_for i =
          let pr = pg_get_priority game i in
	  let pl = pg_get_owner game i in
	  let tr = pg_get_successors game i in
		
		let tr_calc = Array.map (fun j ->
			let prog0 = prog plr_Even pr spmidx.(i) spmidx.(j) in
			let prog1 = prog plr_Odd pr spmidx.(i) spmidx.(j) in
			Array.init (maxprio + 1) (fun k -> if k mod 2 = 0 then prog1.(k) else prog0.(k))
		) (Array.of_list (ns_nodes tr)) in
		
		let get_succ player = (if pl = player then arr_minarg else arr_maxarg) tr_calc (fun q -> q) (less player 0) in
		
		let succ0 = get_succ plr_Even in
		let succ1 = get_succ plr_Odd in
		
		Array.init (maxprio + 1) (fun k -> if k mod 2 = 0 then succ1.(k) else succ0.(k))
	in
	
    let queue = SingleOccQueue.create () in
	let counter = ref 0 in
	
	let update_valid_for player =
		
		let temp = Array.make n true in
		let temp_queue = SingleOccQueue.create () in
		
		for i = 0 to n - 1 do
			SingleOccQueue.add i temp_queue;
		done;
		
		while not (SingleOccQueue.is_empty temp_queue) do

			let i = SingleOccQueue.take temp_queue in
			if temp.(i) then (
				if is_top spmidx.(i) player then temp.(i) <- false
				else (let pr = pg_get_priority game i in
				      let pl = pg_get_owner game i in
				      let tr = pg_get_successors game i in
					if pl = player then (
						let tr = Array.of_list (List.filter (fun j -> temp.(j)) (ns_nodes tr)) in
						let tr_calc = Array.map (fun j -> prog player pr spmidx.(i) spmidx.(j)) tr in
						if (Array.length tr_calc = 0) ||
						   (less player 0 spmidx.(i) (arr_minarg tr_calc (fun q -> q) (less player 0))) then temp.(i) <- false
					)
					else if ns_exists (fun j -> not temp.(j)) tr then temp.(i) <- false
					else let tr_calc = Array.map (fun j -> prog player pr spmidx.(i) spmidx.(j)) (Array.of_list (ns_nodes tr)) in
						 let upd = arr_maxarg tr_calc (fun q -> q) (less player 0) in
						 if (less player 0 spmidx.(i) upd) then temp.(i) <- false
				);
				if not temp.(i) then ns_iter (fun j -> if temp.(j) then SingleOccQueue.add j temp_queue) (pg_get_predecessors game i)
			)
		done;
		
		for i = 0 to n - 1 do
		  let op = plr_opponent player in
			if temp.(i) && (not (is_top spmidx.(i) op)) then (
				make_top spmidx.(i) op;
				ns_iter (fun j -> SingleOccQueue.add j queue) (pg_get_predecessors game i)
			)
		done

	in
	
    for i = 0 to n - 1 do
        SingleOccQueue.add i queue;
    done;

	while not (SingleOccQueue.is_empty queue) do
	
		let i = SingleOccQueue.take queue in
		
		msg_tagged 3 (fun _ -> "Dequeuing " ^ string_of_int i ^ "\n");

		(* Now check whether a real update is necessary. This is only true
		   iff the current spm of i is stricly less than its calculated spm
		   w.r.t. to both (!) players. *)

		let upd = calc_prog_for i in
		let less0 = less plr_Even 0 spmidx.(i) upd in
		let less1 = less plr_Odd 0 spmidx.(i) upd in
		if less0 || less1 then (
			msg_tagged 3 (fun _ -> "Enqueuing predecessors of " ^ string_of_int i ^ "\n");
			ns_iter (fun j -> SingleOccQueue.add j queue) (pg_get_predecessors game i);
			spmidx.(i) <- upd;
			msg_tagged 3 (fun _ -> "Updating small progress measure: \n" ^ format_spmidx spmidx ^ "\n");
		);
		incr counter;
		
		if !counter mod n = 0 then (
			update_valid_for plr_Even;
			update_valid_for plr_Odd
		)
	done;

    let sol = sol_create game in
    let strat = Array.make n (-1) in

    for i = 0 to n - 1 do
        msg_tagged 3 (fun _ -> "Checking " ^ string_of_int i ^ "\n");
        sol.(i) <- if is_top spmidx.(i) plr_Even then plr_Odd
		           else if is_top spmidx.(i) plr_Odd then plr_Even
				   else failwith "[Smallprogress.solve'] impossible: no top value";
    done;
    for i = 0 to n - 1 do
      let pl = pg_get_owner game i in
      let delta = pg_get_successors game i in

      if (pl = sol.(i))
      then strat.(i) <- ns_minarg delta (fun q -> spmidx.(q)) (less pl 0)
    done;

    (sol, strat);;

let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;


let register _ =
    Solverregistry.register_solver solve "smallprog" "sp" "use the small progress measure algorithm due to Jurdzinski";;
