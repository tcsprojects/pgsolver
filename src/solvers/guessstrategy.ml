open Basics ;;
open Paritygame ;;
open Univsolve;;
open Solvers;;

let solve game =
    let solve' game =

        let msg_tagged v = message_autotagged v (fun _ -> "GUESSSTRATEGY") in
        let msg_plain = message in

        let generate_strat game pl =
            let n = Array.length game in
            let s = Array.make n (-1) in
            for i = 0 to n - 1 do
                let (_, pl', d, _) = game.(i) in
                    if (pl = pl') then s.(i) <- d.(Random.int (Array.length d))
            done;
            s
        in

	let heuristic_solve game =
		let n = Array.length game in
		let s0 = generate_strat game 0 in
		let s1 = generate_strat game 1 in
		let (sol0, _) = universal_solve_trivial verbosity_level_default (subgame_by_strat game s0) in
		let (sol1, _) = universal_solve_trivial verbosity_level_default (subgame_by_strat game s1) in
		let sol = Array.init n (fun i -> if sol0.(i) = 0 then 0 else if sol1.(i) = 1 then 1 else -1) in
		let strat = Array.init n (fun i -> if sol0.(i) = 0 then s0.(i) else if sol1.(i) = 1 then s1.(i) else -1) in
		(sol, strat)
	in

	let counter = ref 0 in

        let rec iterate_strat game =
		incr counter;
		msg_tagged 2 (fun _ -> "Guessing strategies #" ^ string_of_int !counter ^ "\r");
		let (sol, strat) = heuristic_solve game in
		let c = Array.fold_left (fun c e -> if e = -1 then c else c + 1) 0 sol in
		if c = 0 then iterate_strat game else (
			msg_plain 2 (fun _ -> "\n");
			msg_tagged 2 (fun _ -> "Returning " ^ string_of_int c ^ " solved nodes.\n");
			(sol, strat)
		)
        in

    	iterate_strat game
    in
	universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;


register_solver solve "guessstrategy" "gs" "use the strategy guessing heuristic";;
