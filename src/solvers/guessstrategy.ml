(* The strategy guessing heuristic
 * 
 * Why waste time computing winning strategies when all you have to do is pick them up from the floor?
 * Here is the simplest heuristc of them all: just guess a strategy and see what happens.
 *
 * from:
 * ... well, you guessed right (it's as if you just guessed a winning strategy, ha-ha-ha!). This has not
 * been published anywhere. We are still considering submitting it to LICS. Or POPL. Or STOC, FOCS, SODA, 
 * any of them will do.
 *)


open Basics ;;
open Paritygame ;;
open Univsolve;;
open Pgnodeset;;
open Pgplayer;;
open Pgpriority;;
open Pgstrategy;;
open Pgnode;;


let solve game =
    let solve' game =

        let msg_tagged v = message_autotagged v (fun _ -> "GUESSSTRATEGY") in
        let msg_plain = message in

        let generate_strat game pl =
            let n = game#size in
            let s = new array_strategy n in
	        game#iterate (fun v -> fun (_,pl',d,_,_) -> if (pl = pl') then s#set v (ns_some d));
            s
        in

	let heuristic_solve game =
		let n = game#size in
		let s0 = generate_strat game plr_Even in
		let s1 = generate_strat game plr_Odd in
		let (sol0, _) = universal_solve_trivial verbosity_level_default (game#subgame_by_strat s0) in
		let (sol1, _) = universal_solve_trivial verbosity_level_default (game#subgame_by_strat s1) in
		let sol = sol_init game (fun i -> if sol0#get i = plr_Even then plr_Even else if sol1#get i = plr_Odd then plr_Odd else plr_undef) in
		let strat = str_init game (fun i -> if sol0#get i = plr_Even then s0#get i else if sol1#get i = plr_Odd then s1#get i else nd_undef) in
		(sol, strat)
	in

	let counter = ref 0 in

        let rec iterate_strat game =
		incr counter;
		msg_tagged 2 (fun _ -> "Guessing strategy #" ^ string_of_int !counter ^ "\r");
		let (sol, strat) = heuristic_solve game in
		let c = sol#number_solved in
		if c = 0 then iterate_strat game else (
			msg_plain 2 (fun _ -> "\n");
			msg_tagged 2 (fun _ -> "Returning " ^ string_of_int c ^ " solved nodes.\n");
			(sol, strat)
		)
        in

    	iterate_strat game
    in
	universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;


let register _ =
    Solverregistry.register_solver solve "guessstrategy" "gs" "use the strategy guessing heuristic";;
