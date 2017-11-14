(* The Big-Step Algorithm
 *
 * from:
 * Sven Schewe. Solving Parity Games in Big Steps. FSTTCS 2007: 449-460
 *)

open Basics ;;
open Paritygame ;;
open Univsolve;;
open Tcsarray;;
open Smallprogress;;
open Pgplayer;;
open Pgnode;;
open Pgnodeset;;


let compute_priority_reach_array mygame player =
    let maxprspm = (mygame#get_max_prio_for (plr_opponent player)) / 2 in
    (* Dumb version (!)  *)
    let rec calc_iter game  maxvalues =
        let badPrio = game#get_max_prio_for (plr_opponent player) in
        let goodPrio = game#get_max_prio_for player in
        if badPrio >= 0 then (
            let tmp_nodes = ref ns_empty in
            if goodPrio > badPrio then
                game#iterate (fun i (pr, _, _, _, _) ->
                    if pr > badPrio then tmp_nodes := ns_add i !tmp_nodes
                )
            else (
                let (sccs, sccindex, topology, roots) = game#strongly_connected_components in
                let sccs: nodeset array = sccs in
                let sccentry = Array.make (Array.length sccs) (-1) in
                let rec count_nodes r =
                	if sccentry.(r) = -1 then (
                        List.iter count_nodes topology.(r);
                        sccentry.(r) <- List.fold_left (fun a i -> a + sccentry.(i)) 0 topology.(r);
                        ns_iter (fun v ->
                        	if game#get_priority v = badPrio then sccentry.(r) <- 1 + sccentry.(r)
                        ) sccs.(r)
					)
                in
                List.iter count_nodes roots;
                game#iterate (fun i (pr, _, _, _, _) ->
                    if pr >= 0 then (maxvalues.(i)).(badPrio / 2) <- 1 + sccentry.(sccindex i);
                    if pr = badPrio then tmp_nodes := ns_add i !tmp_nodes
                );
            );
            game#remove_nodes !tmp_nodes;
            calc_iter game maxvalues
        )
    in
    let game = mygame#copy in
    let maxvalues = Array.make_matrix (game#size) (1 + maxprspm) 1 in
    calc_iter game maxvalues;
    maxvalues


let solve_scc_restr game player u =
    let spmidx = Array.map (Array.map (fun m -> (0, min m u))) (compute_priority_reach_array game player) in
    let spmupd spmz _ =
      let l = Array.length spmz in
      let c = ref (Array.fold_left (fun r (v, _) -> r + v) 0 spmz) in
      for i = 0 to l - 1 do
        if (fst spmz.(i) > snd spmz.(i)) || (!c > u) then (
          c := !c - (fst spmz.(i));
          spmz.(i) <- (0, snd spmz.(i));
          if (i < l - 1) then (
            spmz.(i + 1) <- (1 + fst spmz.(i + 1), snd spmz.(i + 1));
            incr c
          )
          else for j = 0 to l - 1 do
                 spmz.(j) <- (snd spmz.(j), snd spmz.(j))
               done
        )
      done
    in
    
    solve_scc_reach game player spmidx spmupd;;


let solve_scc game =

	let n = game#size in
	let m = game#get_max_prio in
	let sqrt3 x = x ** (1.0 /. 3.0) in
	let u = int_of_float (ceil (sqrt3 (float (n * n * m)))) in

	let correct (sol, strat) pl =
		for i = 0 to n - 1 do
			if sol#get i != pl then (
				sol#set i plr_undef;
				strat#set i nd_undef;
			)
		done
	in
	let (sol, strat) = solve_scc_restr game plr_Even u in
	correct (sol, strat) plr_Even;

	if sol#number_solved > 0
	then (sol, strat)
	else (
        let (sol, strat) = solve_scc_restr game plr_Odd u in
        correct (sol, strat) plr_Odd;
        (sol, strat)
	);;


let solve game = Recursive.fallback_solve game solve_scc (universal_solve_init_options_verbose !universal_solve_global_options);;

let register _ =
    Solverregistry.register_solver solve "bigstep" "bs" "use the big step procedure due to Schewe";;
