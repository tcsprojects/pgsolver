open Arg ;;
open Tcsargs;;
open Basics ;;
open Paritygame ;;
open Tcsstrings ;;
open Tcsarray;;
open Tcsset;;
open Univsolve;;
open Solvers ;;
open Str ;;
open Stratimpralgs ;;
open Pgnodeset;;
open Pgplayer;;
open Stratimprlowerboundanalyze;;

(*
bin/stratimprgen -pg randomedgeexptest 3 | bin/analyzepolicy.native -pi -re
*)

module CommandLine =
struct
  let solver = ref None

	let random_seed = ref None

  let speclist = [
		(["--randomseed"; "-rs"], Int(fun i -> random_seed := Some i),
	"\n     set random seed[int] (default: disabled)")
  ]

		      @

  fold_solvers (fun solve ident abbrev desc arr ->
  					(["--" ^ ident; "-" ^ abbrev], String(fun solveargs -> let solve = solve (Array.of_list (Tcsstrings.StringUtils.explode solveargs ' ')) in solver := Some solve),
  					 "\n     Use solver: " ^ desc)::arr
  ) []

  let header = Info.get_title "Strategy Improvement Analyzer Tool"
end ;;

open CommandLine ;;

let out s =
	print_string s;
	flush stdout

let _ =
  SimpleArgs.parsedef speclist (fun _ -> ()) (header ^ "\nOptions are");

	(match !random_seed with
		Some rs -> Random.init rs
	| None -> Random.self_init ());

(Univsolve.universal_solve_global_options := fun gen_stat verb -> {
	generate_statistics = gen_stat ;
	verb_level = verb ;
	global_optimization = false ;
	decompose_sccs = false ;
	solve_special_games = false ;
	local_optimization = false ;
	globalopt_remove_useless_self_cycles = false ;
	globalopt_solve_useful_self_cycles = false ;
	solvespec_single_parity = false ;
	solvespec_single_player = false ;
	localopt_priority_propagation = false ;
	localopt_compact_priorities = false ;
  });

  let game = Parsers.parse_parity_game stdin in

	let mapper (s, _) (t, _) = match s with
	  'c' -> t = 'A'
	| 'q' -> t = 'H'
	| 'a' -> t != 'K'
	| 'r' -> t = 'd'
	| 'd' -> t != 'M'
	| 'b' -> t != 'L'
	| 'A' -> t = 'H'
	| 'e' -> t = 'C'
	| 'F' -> t = 'W'
	| 'M' -> t = 'P'
	| 'W' -> t = 'd'
	| 'P' -> t = 'd'
	in

	let binary_strategy_to_state bs =
		let bs = desc_map_s bs 'a' (Arr (Array.map (fun b ->
			Ele (Array.for_all mda_as_ele (mda_as_arr b))
		) (mda_as_arr (desc_map_g bs 'a')))) in
		let bs = desc_map_s bs 'b' (Arr (Array.map (fun b ->
			Ele (Array.for_all mda_as_ele (mda_as_arr b))
		) (mda_as_arr (desc_map_g bs 'b')))) in
		let bs = desc_map_s bs 'd' (Arr (Array.map (fun b ->
			Ele (Array.for_all mda_as_ele (mda_as_arr b))
		) (mda_as_arr (desc_map_g bs 'd')))) in
		bs
	in

	let state_to_simplify bs =
	  let a = mda_as_arr (desc_map_g bs 'a') in
		let b = mda_as_arr (desc_map_g bs 'b') in
		let d = mda_as_arr (desc_map_g bs 'd') in
		let r = mda_as_arr (desc_map_g bs 'r') in
		let c = mda_as_arr (desc_map_g bs 'c') in
		let x = Array.init (Array.length a) (fun i ->
			(mda_as_ele a.(i)) && (mda_as_ele b.(i)) && (mda_as_ele d.(i)) && (mda_as_ele r.(i))
		) in
		let y = Array.init (Array.length c) (fun i -> mda_as_ele c.(i)) in
		(x, y)
	in

	let state_to_bits bs =
		let a = mda_as_arr (desc_map_g bs 'a') in
		let b = mda_as_arr (desc_map_g bs 'b') in
		let d = mda_as_arr (desc_map_g bs 'd') in
		let r = mda_as_arr (desc_map_g bs 'r') in
		let c = mda_as_arr (desc_map_g bs 'c') in
		let q = mda_as_arr (desc_map_g bs 'q') in
		let n = Array.length a in
		let z = Array.make n false in
		let continue = ref true in
		for i = n - 1 downto 0 do
			if !continue then (
				z.(i) <- mda_as_ele d.(i) && mda_as_ele r.(i) && mda_as_ele b.(i);
				continue := z.(i) = mda_as_ele q.(i) && z.(i) = mda_as_ele c.(i) && z.(i) = mda_as_ele a.(i)
			)
		done;
		z
	in

	let state_of_interest ps bs bits =
		let q = mda_as_arr (desc_map_g bs 'q') in
		let c = mda_as_arr (desc_map_g bs 'c') in
		let a = mda_as_arr (desc_map_g bs 'a') in
		let d = mda_as_arr (desc_map_g bs 'd') in


		let good = ref true in
(*
		let d = mda_as_arr (desc_map_get ps ('d', [0])) in
		let r = mda_as_ele (desc_map_get ps ('r', [0])) in
		if (not (Array.for_all mda_as_ele d)) && r then good := false;
*)
		for i = 0 to Array.length bits - 1 do
		  good := !good && bits.(i) = mda_as_ele q.(i) && bits.(i) = mda_as_ele c.(i);
	      if bits.(i) then good := !good && bits.(i) = mda_as_ele a.(i)
	      (*
			if not bits.(i) then good := !good && not (mda_as_ele d.(i))
			*)
		done;

		!good
	in

	let mu bits =
	  let n = Array.length bits in
		let r = ref n in
		for i = n-1 downto 0 do
		  if not bits.(i) then r := i
		done;
		!r
	in
(*
	let assumption_checker counter bits =
	  let f = mda_as_arr (desc_map_g counter 'F') in
		let mm = mda_as_arr (desc_map_g counter 'M') in
		let m = mu bits in
		for i = 0 to Array.length bits - 1 do
			if not bits.(i) then (
				if m < i then (
					if not (mda_as_ele f.(i)) then failwith "F";
				  let ma = mda_as_arr mm.(i) in
					for j = 0 to Array.length ma - 1 do
						if not (mda_as_ele ma.(j)) then failwith "M"
					done;
				);
			);
		done;
	in
*)
	let posmod j k = (j mod k) + if j >= 0 then 0 else k in

	let guess_counter bs state n =
		let k = Array.length (mda_as_arr (desc_map_get bs ('d', [0]))) in
		(*
		let rg = Array.init n (fun i ->
			if mda_as_ele (desc_map_get state ('d', [i])) then k
			else if mda_as_ele (desc_map_get state ('r', [i])) then 0
			else let don = ref false in
			     let j = ref (k-1) in
			     while not !don do
					 		if not (mda_as_ele (desc_map_get bs ('d', [i;!j])))
							then done := true
							else decr j
					 done;
					 !j
		) in
		let sg = Array.make n rg.(0) in
		for i = 1 to n - 1 do
			sg.(i) <- min sg.(i-1) rg.(i)
		done;
		*)
		let guess_counter = ref TreeMap.empty_def in
		let last_w_reaches = ref None in
		for i = 0 to n - 1 do
			let underbits = ref [] in
			for i' = 0 to i-1 do
				if not (mda_as_ele (desc_map_get state ('d', [i']))) || not (mda_as_ele (desc_map_get state ('r', [i'])))
				then underbits := i'::!underbits
			done;
			let underbits = !underbits in
			for j = 0 to k - 1 do
				let value = ref false in
				if List.length underbits > 1 then value := true;
				if List.length underbits = 1 then (
					let i' = List.hd underbits in
					if not (mda_as_ele (desc_map_get state ('d', [i']))) && mda_as_ele (desc_map_get state ('r', [i']))
					then value := true;
					for j' = 0 to j do
						if not (mda_as_ele (desc_map_get bs ('d', [i';j'])))
						then value := true
					done
				);
				guess_counter := desc_map_set !guess_counter ('M', [i;j]) !value;
			done;
			guess_counter := desc_map_set !guess_counter ('F', [i]) (List.length underbits > 0);
			(*
			if i < n-1 then (
				s.(i-1) vs g.(i)
				if g.(i) >=

				let value = ref false in
				if !last_w_reaches != None then value := false ( * this is almsot right - but we can go down via M's so now the q is can we go down via M's to a G? * )
				else if mda_as_ele (desc_map_get state ('d', [i])) then value := i = 0
				else (value := true; last_w_reaches := Some i);
				guess_counter := desc_map_set !guess_counter ('W', [i]) !value
			);
			*)
			(* not yet *)
			(*
			if i < n-1 then (
				let value = ref false in
				if i = 0 then value := true
				else if mda_as_ele (desc_map_get state ('d', [i])) && mda_as_ele (desc_map_get state ('r', [i])) then value := false
				else if List.length underbits = 0 then value := true
				else if mda_as_ele (desc_map_get state ('d', [i])) then value := List.length underbits > 0
				else (
					let don = ref false in
					let i' = ref i in
					let j = ref (k-1) in
					while (!j >= 0 && not !don && !i' >= 0) do
					  if (mda_as_ele (desc_map_get state ('d', [!i'])) && mda_as_ele (desc_map_get state ('r', [!i']))) then decr i' else (
							if not (mda_as_ele (desc_map_get bs ('d', [!i';!j]))) then (
								if mda_as_ele (desc_map_get !guess_counter ('M', [!i';!j])) then (
									if i = 0 then value := true else (incr j; decr i')
								) else (
									don := true
								)
							);
							decr j
						)
					done;
					if (!i' < 0) then value := true
			  );
				guess_counter := desc_map_set !guess_counter ('W', [i]) !value
			);
			*)
		done;
		!guess_counter
	in

	let guess_improving bs state bits =
		let guess = ref TreeMap.empty_def in
		let k = Array.length (mda_as_arr (desc_map_get bs ('d', [0]))) in
		let m = mu bits in

    let read bs i j =
			let j = posmod j (k + 1) in
			mda_as_ele (desc_map_get bs (if j = k then ('r', [i]) else ('d', [i;j])))
		in

		let write bs i j v =
			let j = posmod j (k + 1) in
			desc_map_set bs (if j = k then ('r', [i]) else ('d', [i;j])) v
		in
(*
		guess_counter := desc_map_set !guess_counter ('F', [0]) false;
		guess_counter := desc_map_set !guess_counter ('W', [0]) true;
		for j = 0 to k - 1 do
			guess_counter := desc_map_set !guess_counter ('M', [0;j]) false;
			guess_counter := desc_map_set !guess_counter ('P', [0;j]) true;
		done;
		let mu0 = ref k in
		for j=k-1 downto 0 do
			if not (mda_as_ele (desc_map_get bs ('d', [0;j]))) then mu0 := j
		done;
		let mu0 = !mu0 in
		let mu1 = ref k in
		for j=k-1 downto 0 do
			if not (mda_as_ele (desc_map_get bs ('d', [1;j]))) then mu1 := j
		done;
		let mu1 = !mu1 in

		guess_counter := desc_map_set !guess_counter ('F', [1]) (m < 1);
		for j = 0 to k - 1 do
			guess_counter := desc_map_set !guess_counter ('M', [1;j])
				(not bits.(0) && (mda_as_ele (desc_map_get bs ('r', [0])) || mu0 <= j));
				*)
				(*
				guess_counter := desc_map_set !guess_counter ('P', [1;j])
				( (not (mda_as_ele (desc_map_get bs ('d', [1;j]))) || (j = 0 && (not (mda_as_ele (desc_map_get bs ('r', [1])))))) ||
					(bits.(0) && not bits.(1)) ||
					 (not bits.(0) && not bits.(1) && (mda_as_ele (desc_map_get bs ('r', [1])) || mu1 <= j))
				 )
		done;
		guess_counter := desc_map_set !guess_counter ('W', [1]) false;

		guess_counter := desc_map_set !guess_counter ('F', [2]) (m < 2);
		*)

		let base0 = ref 0 in
		let base1 = ref 0 in
		let base2 = ref 0 in
		let base3 = ref 0 in
		let good = ref true in
		for j = 0 to k do
			let cur = read bs 0 (!base0 + j) in
			guess := write !guess 0 (!base0 + j) (cur != !good);
			if (!good && not cur)
			then base1 := j mod k;
			good := !good && cur
		done;
		good := true;

		for j = 0 to k do
			let cur = read bs 1 (!base1 + j) in
			guess := write !guess 1 (!base1 + j) (cur != !good);
			if (!good && not cur)
			then base2 := j mod k;
			good := !good && cur
		done;
		good := true;

		if read bs 1 k then base2 := 0;

		if bits.(1) then base2 := !base1;

(*
		base2 := min !base1 !base2;

    not sure this is really correct

		looking at bit 4 when bit states are 0 0 1 0
		not clear why same issue does not happen with 3 bits and bit 3 in state 0 0 0

		consider case where r = 0 0 . 0 and d_1 = 1 1 . .
		then first we go via d_1,2->r_2->d_1,-1....

		understand why we have r

*)
(*
		for j = 0 to k do
			let cur = read bs 2 (!base2 + j) in
			guess := write !guess 2 (!base2 + j) (cur != !good);
			if (!good && not cur)
			then base3 := j mod k;
			good := !good && cur
		done;

		good := true;

		if read bs 2 k then base3 := 0;

		if bits.(2) then base3 := !base2;

		for j = 0 to k do
			let cur = read bs 3 (!base3 + j) in
			guess := write !guess 3 (!base3 + j) (cur != !good);
			good := !good && cur
		done;
*)

		!guess
	in

	let reachabilityValues bs state bits =
	    let n = Array.length bits in
		let k = Array.length (mda_as_arr (desc_map_get bs ('d', [0]))) in

        let read bs i j =
			let j = posmod j (k + 1) in
			mda_as_ele (desc_map_get bs (if j = k then ('r', [i]) else ('d', [i;j])))
		in

		let bsa = Array.init n (fun i -> Array.init (k+1) (fun j -> if read bs i j then 1 else 0)) in

		let rv = Array.init n (fun i -> Array.init (k+1) (fun j -> [|-1;-1|] ) ) in
        for i = 0 to n-1 do
            (* Compute down ladder by minimum over all. *)
            for j = 0 to k do
                let cur = ref (if j != k then i else n) in
                let jj = min j (k-1) in
                if i > 0 && (rv.(i-1).(jj).(0) < i-1 || rv.(i-1).(jj).(1) < i)
                then cur := min !cur (min rv.(i-1).(jj).(0) rv.(i-1).(jj).(1));
                rv.(i).(j).(0) <- !cur;
                (* If going down this equals computation. *)
                if bsa.(i).(j) == 0
                then rv.(i).(j).(1) <- !cur;
            done;
            (* If cycle is closed take it out of the game. *)
            if Array.for_all (fun cell -> cell == 1) bsa.(i) then (
                for j = 0 to k do
                    rv.(i).(j).(1) <- n+1
                done;
            ) else (
                (* Find going down base. *)
                let base = ref 0 in
                while bsa.(i).(!base) == 1 do
                    incr base
                done;
                (* Work way backwards *)
                for j = 0 to k do
                    let jj = (j + !base) mod (k + 1) in
                    if bsa.(i).(jj) == 1 then rv.(i).(jj).(1) <- rv.(i).((jj-1+k+1)mod(k+1)).(1)
                done
            )
        done;
        rv

    in

    let guess_improving bs state bits =
        let guess = ref TreeMap.empty_def in
	    let n = Array.length bits in
		let k = Array.length (mda_as_arr (desc_map_get bs ('d', [0]))) in

        let read bs i j =
			let j = posmod j (k + 1) in
			mda_as_ele (desc_map_get bs (if j = k then ('r', [i]) else ('d', [i;j])))
		in

		let write bs i j v =
			let j = posmod j (k + 1) in
			desc_map_set bs (if j = k then ('r', [i]) else ('d', [i;j])) v
		in

		let bsa = Array.init n (fun i -> Array.init (k+1) (fun j -> if read bs i j then 1 else 0)) in

		let rv = reachabilityValues bs state bits in

		let is = Array.mapi (fun i row ->
            let bb = ref true in
            Array.mapi (fun j cell ->
                if cell == 1
                then (* Currently going into the cycle *)
                     let delta = rv.(i).(j).(0) - rv.(i).(j).(1) in
                     (* Improving to leave cycle if going down is better (duh)
                        or if going down is equal BUT we are already going down via a successor with same valuation *)
                     let ret = delta > 0 || (delta == 0 && not !bb) in
                     ret
                else (* Currently leaving the cycle *)
                     let delta = rv.(i).((j-1+k+1)mod(k+1)).(1) - rv.(i).(j).(0) in
                     (* Improving to go into cycle if go in is better (duh)
                        or if going in is equal BUT we're not going down via a successor *)
                     let ret = delta > 0 || (delta == 0 && !bb) in
                     bb := false;
                     ret
            ) row
		) bsa in

		Array.iteri (fun i row -> Array.iteri (fun j cell -> guess := write !guess i j cell) row) is;

		!guess

    in


	let ic = ref 0 in

	_strat_impr_callback := Some (fun strat counter ->
		incr ic;
		print_string ("\n\n\nIteration: " ^ string_of_int !ic ^ "\n");
		let node_compare = node_total_ordering_by_position in
		let valu = evaluate_strategy game node_compare strat in
		let less i j = node_valuation_ordering game node_compare valu.(i) valu.(j) < 0 in
		let counter_strat = counter_strategy_by_valu_and_ordering game valu node_compare in

		print_string "Strategy\n";
		let parsed = parse_binary_strategy game strat mapper in
		print_string (game_desc_binary_map_format parsed);
		print_string "Counter\n";
		let parsedCounter = parse_binary_strategy game counter_strat mapper in
		print_string (game_desc_binary_map_format parsedCounter);
		print_string "Improve\n";
		let improving = parse_binary_improving game node_compare strat valu in
		print_string (game_desc_binary_map_format improving);
		print_string "State\n";
		let state = binary_strategy_to_state (parse_binary_strategy game strat mapper) in
		print_string (game_desc_binary_map_format state);
		print_string "Bits\n";
		let bits = state_to_bits state in
		print_string ((ArrayUtils.format (function true -> "1" | _ -> "0") bits) ^ "\n");
		print_string "Interest\n";
		let interest = state_of_interest parsed state bits in
		print_string (if interest then "Yes\n" else "No\n");
		if (interest) then (
(*			assumption_checker parsedCounter bits; *)
			print_string "Guess\n";
			let guess = guess_improving parsed state bits in
			print_string (game_desc_binary_map_format guess);
			(*
			let guess_counter = guess_counter parsed state (Array.length bits) in
			print_string (game_desc_binary_map_format guess_counter);
			*)
			let sub = desc_map_is_subset_of guess improving in
			if (not sub) then failwith "not sub"			;
			(*
			let sub = desc_map_is_subset_of guess_counter parsedCounter in
			if (not sub) then failwith "not sub"			;
			*)
		)
	);

	match !solver with
		None -> ()
	|	Some solve -> let _ = solve game in ()
