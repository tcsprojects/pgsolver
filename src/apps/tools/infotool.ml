open Arg ;;
open Tcsargs;;
open Basics ;;
open Paritygame ;;

let out s =
	print_string s;
	flush stdout

let _ =
	let input_file = ref "" in
	let speclist =  [] in
	
	let header = Info.get_title "Parity Game Info Tool" in
	  
	SimpleArgs.parsedef speclist (fun f -> input_file := f) (header ^ "Usage: infotool [infile]\n" ^
                                           "Provides some information about a parity game.");

	let (in_channel,name) = if !input_file = "" then (stdin,"STDIN") else (open_in !input_file,!input_file) in

	let game = Parsers.parse_parity_game in_channel in

	let number_of_nodes = ref 0 in
	let number_of_player0_nodes = ref 0 in
	let number_of_player1_nodes = ref 0 in
	let min_priority = ref (-1) in
	let max_priority = ref (-1) in
	let number_of_edges = ref 0 in
	let number_of_player0_edges = ref 0 in
	let number_of_player1_edges = ref 0 in
	
	pg_iterate (fun _ -> fun (pr, pl, tr, _, _) ->
		let trn = ns_size tr in
		incr number_of_nodes;
		number_of_edges := !number_of_edges + trn;
		if pl = plr_Even then (
			incr number_of_player0_nodes;
			number_of_player0_edges := !number_of_player0_edges + trn
		)
		else (
			incr number_of_player1_nodes;
			number_of_player1_edges := !number_of_player1_edges + trn
		);
		if (!min_priority < 0) || (pr < !min_priority) then min_priority := pr;
		if (!max_priority < 0) || (pr > !max_priority) then max_priority := pr;
	) game;
	
	out ("Number of Nodes    : " ^ string_of_int !number_of_nodes ^ "\n");
	out ("Number of P0 Nodes : " ^ string_of_int !number_of_player0_nodes ^ "\n");
	out ("Number of P1 Nodes : " ^ string_of_int !number_of_player1_nodes ^ "\n");
	out ("Minimum Priority   : " ^ string_of_int !min_priority ^ "\n");
	out ("Maximum Priority   : " ^ string_of_int !max_priority ^ "\n");
	out ("Number of Edges    : " ^ string_of_int !number_of_edges ^ "\n");
	out ("Number of P0 Edges : " ^ string_of_int !number_of_player0_edges ^ "\n");
	out ("Number of P1 Edges : " ^ string_of_int !number_of_player1_edges ^ "\n");
	out "\n";;
