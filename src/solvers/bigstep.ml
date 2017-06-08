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

	let n = pg_size game in
	let m = pg_max_prio game in
	let sqrt3 x = x ** (1.0 /. 3.0) in
	let u = int_of_float (ceil (sqrt3 (float (n * n * m)))) in

	let correct (sol, strat) pl =
		for i = 0 to n - 1 do
			if sol.(i) != pl then (
				sol.(i) <- plr_undef;
				strat.(i) <- -1;
			)
		done
	in

	let (sol, strat) = solve_scc_restr game plr_Even u in
	correct (sol, strat) plr_Even;

	if ArrayUtils.exists sol (fun _ pl -> pl != plr_undef)
	then (sol, strat)
	else (
        let (sol, strat) = solve_scc_restr game plr_Odd u in
        correct (sol, strat) plr_Odd;
        (sol, strat)
	);;


let solve game = Recursive.fallback_solve game solve_scc (universal_solve_init_options_verbose !universal_solve_global_options);;

let register _ =
    Solverregistry.register_solver solve "bigstep" "bs" "use the big step procedure due to Schewe";;
