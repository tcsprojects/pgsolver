open Arg ;;
open Tcsargs;;
open Tcsbasedata;;
open Basics ;;
open Paritygame ;;
open Tcsstrings ;;
open Str ;;
open Stratimpralgs;;
open Tcsset;;

let out s =
	print_string s;
	flush stdout

let _ =
	let header = Info.get_title "Parity Game Improvement Arena Tool" in
	  
	SimpleArgs.parsedef [] (fun _ -> ()) (header ^ "Usage: fullimprarena\n" ^
                                           "Provides some information about a parity game improvement arena.");

	let (in_channel,name) = (stdin,"STDIN") in

	let game = Parsers.parse_parity_game in_channel in
	
	let print_strat strategy =
		let valu = evaluate_strategy game node_total_ordering_by_position strategy in
		
		let compare_by_desc i j =
			compare (pg_get_desc game i) (pg_get_desc game j)
		in
		
		let ordered = ref (TreeSet.empty compare_by_desc) in
		let longest = ref 0 in
		
		pg_iterate (fun i -> fun (_,_,_,_,desc) -> longest := max !longest (String.length (OptionUtils.get_some desc));
							   ordered := TreeSet.add i !ordered) game;
		
		let getd i = (StringUtils.fillup (OptionUtils.get_some (pg_get_desc game i)) !longest ' ') in
		
		TreeSet.iter (fun i ->
			out (getd i);
			out " | ";
			let pl = pg_get_owner game i in
			let tr = pg_get_successors game i in 
			let j =
			  if pl = plr_Even then strategy.(i)
			  else best_decision_by_valuation_ordering game node_total_ordering_by_position valu i
			in
			out (getd j);
			out " | ";
			if pl = plr_Even then (
				ns_iter (fun j ->
					if node_valuation_ordering game node_total_ordering_by_position valu.(strategy.(i)) valu.(j) < 0
					then out (getd j ^ " ");
				) tr;
			);
			out "\n";
		) !ordered;
		out "\n\n";
	in
	
	let rec iterate_strat strategy i =
		if i >= pg_size game
		then print_strat strategy
		else (
		let pl = pg_get_owner game i in
		let tr = pg_get_successors game i in 
		if pl = plr_Odd
		then iterate_strat strategy (i + 1)
		else ns_iter (fun j ->
			      strategy.(i) <- j;
			      iterate_strat strategy (i + 1)
			     ) tr
		)
	in
		
	let strategy = Array.make (pg_size game) (-1) in
	
	iterate_strat strategy 0;

	out "\n";;
