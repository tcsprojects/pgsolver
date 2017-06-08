(* The Dominion Decomposition Algorithm 
 *
 * from:
 * Marcin Jurdzinski, Mike Paterson, Uri Zwick.
 * A deterministic subexponential algorithm for solving parity games. SODA 2006: 117-123
 *)

open Basics ;;
open Tcsset;;
open Tcsbasedata.OptionUtils;;
open Paritygame ;;
open Univsolve;;


let newwin options game =

	let n = pg_size game in
	let l = int_of_float (ceil (sqrt (float (2 * n)))) in
	let sol = sol_create game in
	let strat = Array.make n (-1) in
	let options' = (universal_options_alter_verb options verbosity_level_default) in

	let is_dominion nodeset pl =
		let recsolve = fun g -> Recursive.mcnaughton_zielonka g options' in
		match (pg_set_dominion recsolve game nodeset pl) with
			None -> false
		|   Some strat' -> (
            ns_iter (fun q ->
                sol.(q) <- pl;
                if strat'.(q) != -1
                then strat.(q) <- strat'.(q)
            ) nodeset;
            true
        	)

	in

	let iter_subsets size =
		let n = pg_size game in

		let rec iter_subsets' size idx nodeset =
			if size > n - idx
			then None
			else if size > 0 then (
				let found = ref None in
				let i = ref idx in
				while (is_none !found) && (!i <= n - size) do
					if (pg_get_priority game !i >= 0) then (
                        nodeset := ns_add !i !nodeset;
                        found := iter_subsets' (size - 1) (!i + 1) nodeset;
                        if (is_none !found)
                        then nodeset := ns_del !i !nodeset
                    );
					i := !i + 1
				done;
				!found
            )
            else if is_dominion !nodeset plr_Even
            then Some(0, !nodeset)
            else if is_dominion !nodeset plr_Odd
            then Some(1, !nodeset)
            else None
        in

		let i = ref 1 in
		let nodeset = ref None in
		while (is_none !nodeset) && (!i <= size) do
			nodeset := iter_subsets' !i 0 (ref ns_empty);
			i := !i + 1
		done;
		!nodeset
	in

		match (iter_subsets l) with
            Some (pl, dominion) ->  (sol, strat)
        |   None -> (sol, strat);;


let solve game =
	let opt = (universal_solve_init_options_verbose !universal_solve_global_options) in
	Recursive.fallback_solve game (newwin opt) opt;;

let register _ =
    Solverregistry.register_solver solve "dominiondec" "dd" "use the dominion decomposition alg. due to Jurdzinski / Paterson / Zwick";;

(*
let rec newwin oldwin game =

	let n = Array.length game in
	let l = int_of_float (ceil (sqrt (float (2 * n)))) in
	let sol = Array.make n (-1) in
	let strat = Array.make n (-1) in

	let is_dominion nodeset pl =
		match (pg_set_dominion Recursive.solve game nodeset pl) with
			None -> false
		|   Some strat' -> (
            IntSet.iter (fun q ->
                sol.(q) <- pl;
                if strat'.(q) != -1
                then strat.(q) <- strat'.(q)
            ) nodeset;
            true
        	)
	in

	let iter_subsets size =
		let n = Array.length game in

		let rec iter_subsets' size idx nodeset =
			if size > n - idx
			then None
			else if size > 0 then (
				let found = ref None in
				let i = ref idx in
				while (is_none !found) && (!i <= n - size) do
					let (pr, _, _, _) = game.(!i) in
					if (pr >= 0) then (
                        nodeset := IntSet.add !i !nodeset;
                        found := iter_subsets' (size - 1) (!i + 1) nodeset;
                        if (is_none !found)
                        then nodeset := IntSet.remove !i !nodeset
                    );
					i := !i + 1
				done;
				!found
            )
            else if is_dominion !nodeset 0
            then Some(0, !nodeset)
            else if is_dominion !nodeset 1
            then Some(1, !nodeset)
            else None
        in

		let i = ref 1 in
		let nodeset = ref None in
		while (is_none !nodeset) && (!i <= size) do
			nodeset := iter_subsets' !i 0 (ref IntSet.empty);
			i := !i + 1
		done;
		!nodeset
	in

	if n = 0 then (sol, strat)
	else match (iter_subsets l) with
        Some (pl, dominion) -> (
        	let dom' = attr_closure_inplace game strat pl (IntSet.elements dominion) in
        	List.iter (fun q -> sol.(q) <- pl) dom';
        	let game' = pg_copy game in
        	pg_remove_nodes game' dom';
        	let (sol', strat') = newwin oldwin game' in
            for v=0 to n-1 do
              let (p',pl',_,_) = game'.(v) in
              if p' > -1 then (sol.(v) <- sol'.(v);
                               if pl' = sol.(v) then strat.(v) <- strat'.(v))
            done;
        	(sol, strat)
        )
    |   None -> !oldwin game;;


let solve game =
	let f = ref (fun _ -> ([||], [||])) in
	f := newwin (ref (universal_solve verbosity_level_default (Recursive.solve2 f)));
	universal_solve verbosity_level_verbose !f game;;

register_solver solve "dominiondec" "dd" "use the dominion decomposition alg. due to Jurdzinski / Paterson / Zwick";;

*)
