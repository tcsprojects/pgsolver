open Arg ;;
open Tcsargs;;
open Tcsbasedata;;
open Basics ;;
open Paritygame ;;
open Tcsstrings ;;
open Str ;;
open Stratimpralgs;;
open Tcsset;;
open Pgnodeset;;
open Pgplayer;;
open Pgnode;;


let out s =
	print_string s;
	flush stdout

let _ =
	let header = Info.get_title "Parity Game Improvement Arena Tool" in
	  
	SimpleArgs.parsedef [] (fun _ -> ()) (header ^ "Usage: imprarena\n" ^
                                           "Provides some information about a parity game improvement arena.");

	let (in_channel,name) = (stdin,"STDIN") in

	let game = Parsers.parse_parity_game in_channel in
	
	let pre_strategy = Array.make (game#size ) "" in
	
	let rg = Str.regexp "\\(.*\\)\\[\\(.*\\)\\]" in
	
	game#iterate (fun i -> fun (_,ow,_,_,desc) -> if ow = plr_Even then (
						      let desc = OptionUtils.get_some desc in
						      if not (Str.string_match rg desc 0) then failwith "No strategy included!";
						      pre_strategy.(i) <- Str.matched_group 2 desc;
						      game#set_desc  i (Some (Str.matched_group 1 desc))
						    )
		   );
	
	let strategy = str_init game (fun i ->
		if pre_strategy.(i) = "" then nd_undef
		else game#find_desc  (Some pre_strategy.(i))
	) in
	
	let valu = evaluate_strategy game node_total_ordering_by_position strategy in
	
	let compare_by_desc i j =
		compare (game#get_desc  i) (game#get_desc  j)
	in
	
	let ordered = ref (TreeSet.empty compare_by_desc) in
	let longest = ref 0 in
	
	game#iterate (fun i -> fun (_,_,_,_,desc) -> longest := max !longest (String.length (OptionUtils.get_some desc));
						   ordered := TreeSet.add i !ordered) ;
	
	let getd i = (StringUtils.fillup (OptionUtils.get_some (game#get_desc  i)) !longest ' ') in
	
	TreeSet.iter (fun i ->
		out (getd i);
		out " | ";
		let pl = game#get_owner  i in
		let tr = game#get_successors  i in 
		let j =
			if pl = plr_Even then strategy#get i
			else best_decision_by_valuation_ordering game node_total_ordering_by_position valu i
		in
		out (getd j);
		out " | ";
		if pl = plr_Even then (
			ns_iter (fun j ->
				if node_valuation_ordering game node_total_ordering_by_position valu.(strategy#get i) valu.(j) < 0
				then out (getd j ^ " ");
			) tr;
		);
		out "\n";
	) !ordered;

	out "\n";;
