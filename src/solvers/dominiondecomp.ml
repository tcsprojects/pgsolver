open Basics ;;
open Tcsset;;
open Tcsbasedata.OptionUtils;;
open Paritygame ;;
open Univsolve;;
open Solvers;;

(*
let get_force_reach_table game player refl =
	let n = pg_size game in
	let tr = game_to_transposed_graph game in
	let init_entry i =
		let (pr, pl, delta, _) = game.(i) in
        if (pl = player) || (Array.length delta < 2)
        then Array.fold_left (fun retset j -> IntSet.add j retset) IntSet.empty delta
        else IntSet.empty
	in
	let r = Array.init n (fun i -> (0, init_entry i)) in
	let q = int_queue_new () in
	for i = 0 to n - 1 do
		int_queue_add i q
	done;
	let time = ref 0 in
	while not (int_queue_empty q) do
		let i = int_queue_take q in
		let (_, pl, delta, _) = game.(i) in
		if Array.length delta > 0 then (
            let (cur_time, cur_set) = r.(i) in
            let after_set = ref (snd r.(delta.(0))) in
            for j = 1 to (Array.length delta - 1) do
            	let (adj_time, adj_set) = r.(j) in
            	if pl != player
            	then after_set := IntSet.inter !after_set adj_set
            	else if adj_time > cur_time
            	then after_set := IntSet.union !after_set adj_set
            done;
            after_set := IntSet.union !after_set cur_set;
            if IntSet.cardinal !after_set > IntSet.cardinal cur_set then (
                incr time;
                r.(i) <- (!time, !after_set);
                List.iter (fun j -> int_queue_add j q) tr.(i)
            )
        )
    done;
    Array.init n (fun i -> if refl then IntSet.add i (snd r.(i)) else snd r.(i));;


module IntSet_for_set =
struct
  type t = IntSet.t
  let compare = IntSet.compare
end ;;
module IntSetSet = Set.Make(IntSet_for_set) ;;


let get_force_reach_topology game player =
	let tbl_dup = get_force_reach_table game player true in
	let tbl_dom = Array.fold_left (fun retset s -> IntSetSet.add s retset) IntSetSet.empty tbl_dup in
	let n = IntSetSet.cardinal tbl_dom in
	let tbl = Array.create n IntSet.empty in
	let _ = IntSetSet.fold (fun s i -> tbl.(i) <- s; i + 1) tbl_dom 0 in
	let roots = ref [] in
	let top = Array.make n [] in
	let find_parents l i = List.filter (fun p -> (i != p) && (IntSet.subset tbl.(i) tbl.(p))) l in
	let rec descend parent i =
		if (top.(parent) = []) || (List.hd top.(parent) != i) then
            match find_parents top.(parent) i with
                [] -> top.(parent) <- i::top.(parent)
            |   parents -> List.iter (fun p -> descend p i) parents
    in
	for i = 0 to n - 1 do
		match find_parents !roots i with
			[] ->  roots := i::!roots
		|	parents -> List.iter (fun p -> descend p i) parents
	done;
	(!roots, tbl, top);;


let iter_closed_subsets game (roots, tbl, top) exact_size cb =
	let finished = ref false in
	let rec descend current roots call =
		let n = IntSet.cardinal current in
		if (not !finished) && (n <= exact_size) then (
			if (call && n = exact_size) then finished := cb current;
			if (not !finished) && (not (roots = [])) then (
				let h = List.hd roots in
				let t = List.tl roots in
                let next = IntSet.union current tbl.(h) in
                let bigger = IntSet.cardinal next > n in
                descend next t bigger;
                if (not !finished) && bigger
                then descend current (top.(h) @ t) false
            )
		)
	in
	descend (IntSet.empty) roots false;;


let newwin game =
	let n = Array.length game in
	let l = int_of_float (ceil (sqrt (float (2 * n)))) in
	let sol = Array.make n (-1) in
	let strat = Array.make n (-1) in
	let found = ref false in

	let cb pl nodeset =
		let recsolve = fun g -> Recursive.mcnaughton_zielonka g verbosity_level_default in
		match (pg_set_dominion recsolve game nodeset pl) with
			None -> false
		|   Some strat' -> (
            IntSet.iter (fun q ->
                sol.(q) <- pl;
                if strat'.(q) != -1
                then strat.(q) <- strat'.(q)
            ) nodeset;
            found := true;
            true
        	)
	in

	let pl0 = get_force_reach_topology game 0 in
	let pl1 = get_force_reach_topology game 1 in
	let i = ref 1 in

	while (!i <= l) && (not !found) do
		iter_closed_subsets game pl1 !i (cb 0);
     	if not !found then iter_closed_subsets game pl0 !i (cb 1);
		incr i
	done;

	(sol, strat);;
*)



let newwin options game =

	let n = Array.length game in
	let l = int_of_float (ceil (sqrt (float (2 * n)))) in
	let sol = Array.make n (-1) in
	let strat = Array.make n (-1) in
	let options' = (universal_options_alter_verb options verbosity_level_default) in

	let is_dominion nodeset pl =
		let recsolve = fun g -> Recursive.mcnaughton_zielonka g options' in
		match (pg_set_dominion recsolve game nodeset pl) with
			None -> false
		|   Some strat' -> (
            TreeSet.iter (fun q ->
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
                        nodeset := TreeSet.add !i !nodeset;
                        found := iter_subsets' (size - 1) (!i + 1) nodeset;
                        if (is_none !found)
                        then nodeset := TreeSet.remove !i !nodeset
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
			nodeset := iter_subsets' !i 0 (ref TreeSet.empty_def);
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

register_solver solve "dominiondec" "dd" "use the dominion decomposition alg. due to Jurdzinski / Paterson / Zwick";;

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
