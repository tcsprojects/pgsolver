open Arg ;;
open Tcsargs;;
open Tcsbasedata;;
open Basics ;;
open Paritygame ;;
open Tcsstrings ;;
open Tcslist;;
open Str ;;
open Stratimpralgs;;
open Tcsset;;

let out s =
	print_string s;
	flush stdout

let _ =
	let header = Info.get_title "Graph Complexity Decomposition Tool" in
	  
	SimpleArgs.parsedef [] (fun _ -> ()) (header ^ "Usage: game | ./complexdecomp\n" ^
                                           "Computes graph complexity decomposition.");

	let (in_channel,name) = (stdin,"STDIN") in

	let game = Parsers.parse_parity_game in_channel in
	
	let number_of_nodes = ref 0 in
	let number_of_player0_nodes = ref 0 in
	let number_of_player1_nodes = ref 0 in
	let min_priority = ref (-1) in
	let max_priority = ref (-1) in
	let number_of_edges = ref 0 in
	let number_of_player0_edges = ref 0 in
	let number_of_player1_edges = ref 0 in
	
	pg_iterate (fun i -> fun (pr,pl,tr,_,_) -> let trn = ns_size tr in
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
						   if (!max_priority < 0) || (pr > !max_priority) then max_priority := pr
		   ) game;
	
	out ("Number of Nodes    : " ^ string_of_int !number_of_nodes ^ "\n");
	out ("Number of P0 Nodes : " ^ string_of_int !number_of_player0_nodes ^ "\n");
	out ("Number of P1 Nodes : " ^ string_of_int !number_of_player1_nodes ^ "\n");
	out ("Minimum Priority   : " ^ string_of_int !min_priority ^ "\n");
	out ("Maximum Priority   : " ^ string_of_int !max_priority ^ "\n");
	out ("Number of Edges    : " ^ string_of_int !number_of_edges ^ "\n");
	out ("Number of P0 Edges : " ^ string_of_int !number_of_player0_edges ^ "\n");
	out ("Number of P1 Edges : " ^ string_of_int !number_of_player1_edges ^ "\n");
	out "\n";
	out ("Finding optimal strategies using strategy iteration... ");

	let sigma = ref [||] in
	_strat_impr_callback := Some (fun strat _ -> sigma := strat);
	let (_, _) = strategy_improvement_optimize_all_locally_policy game in
	_strat_impr_callback := None;
	let sigma = !sigma in
	
	let tau = compute_counter_strategy game sigma in
	
	out "done.\n";
	
	out ("Optimal player 0 strategy: " ^ format_strategy sigma ^ "\n");
	out ("Optimal player 1 strategy: " ^ format_strategy tau ^ "\n");
	
	out "\n";
	
	let game' = subgame_by_strat game sigma in
	
	let rec helper game indent sccfilter =
		let (sccs', _, topology', _) = strongly_connected_components game in
		let sccs = Array.map (fun scc -> if sccfilter scc then scc else ns_empty) sccs' in
		let topology = Array.mapi (fun i -> List.filter (fun j -> not (ns_isEmpty sccs.(j)))) topology' in
		
		Array.iteri (fun i nodes -> 
			if not (ns_isEmpty nodes) then (
				out (indent ^ "SCC #" ^ string_of_int i ^ " -> " ^ ListUtils.format string_of_int topology.(i) ^ " : " ^ ListUtils.format string_of_int (ns_nodes nodes) ^ "\n");
				if (ns_size nodes > 1) then (
					let sub = pg_copy game in
					let subnodes = nodes in
					ns_iter (fun v ->
						if (pg_get_owner game v = plr_Odd) && (not (ns_elem tau.(v) subnodes))
						then
						  begin
						    ns_iter (fun w -> pg_del_edge sub v w) (pg_get_successors sub v);
						    pg_add_edge sub v tau.(v)
						  end
					) nodes;
					helper sub (indent ^ "  ") (fun scc -> ns_elem (ns_first scc) subnodes);
				)
			)
		) sccs
	in
	
	helper game' "" (fun _ -> true);

	out "\n\n";;
	
