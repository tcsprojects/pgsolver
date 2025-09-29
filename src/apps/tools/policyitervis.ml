open Arg ;;
open Tcsargs;;
open Basics ;;
open Paritygame ;;
open Tcsstrings ;;
open Tcsarray;;
open Univsolve;;
open Solvers ;;
open Str ;;
open Stratimpralgs ;;

module CommandLine =
struct
  let solver = ref None
  let start_iteration = ref 1
  let end_iteration = ref None
  let add_iteration = ref 0

  let speclist = [
        (["--startiter"; "-si"], Int(fun i -> start_iteration := i),
		 "\n     start with iteration [int] (default: 1)");
		(["--enditer"; "-ei"], Int(fun i -> end_iteration := Some i),
		 "\n     end with iteration [int] (default: infinity)");
		(["--additer"; "-ai"], Int(fun i -> add_iteration := i),
		 "\n     add index to iteration [int] (default: 0)");
  ]

		      @

  fold_solvers (fun solve ident abbrev desc arr ->
  					(["--" ^ ident; "-" ^ abbrev], String(fun solveargs -> let solve = solve (Array.of_list (Tcsstrings.StringUtils.explode solveargs ' ')) in solver := Some solve),
  					 "\n     Use solver: " ^ desc)::arr
  ) []

  let header = Info.get_title "Strategy Improvement Visualizer Tool"
end ;;

open CommandLine ;;

let out s =
	print_string s;
	flush stdout

type kind = Even_player_strategy | Even_player_disabled | Even_player_improving | Odd_player_strategy | Odd_player_disabled

let _ =
  SimpleArgs.parsedef speclist (fun _ -> ()) (header ^ "\nOptions are");
  
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

	let before_iteration i =
		out ("beginfig(" ^ string_of_int (i + !add_iteration) ^ ");\n")
	in
	
	let after_iteration i =
		out ("  graph_draw;\n");
		out ("endfig;\n\n")
	in
	
	let set_edge_iteration (from_ident, from_index_stack) (to_ident, to_index_stack) kind = (
				let t = "edge_translate(\"" ^ Char.escaped from_ident ^ "\", \"" ^
				                             ArrayUtils.custom_format string_of_int "" "" ", " from_index_stack ^ "\", " ^
				                             "\"" ^ Char.escaped to_ident ^ "\", \"" ^ 
				                             ArrayUtils.custom_format string_of_int "" "" ", " to_index_stack ^ "\")" in
				let k =
					match kind with
						Even_player_strategy -> "edge_style_strategy"
					|	Even_player_disabled -> "edge_style_disabled"
					|	Even_player_improving -> "edge_style_improving"
					|	Odd_player_strategy -> "edge_style_counter"
					|	Odd_player_disabled -> "edge_style_discounter"
				in
				out ("  edge_set_style(" ^ t ^ ", " ^ k ^ ");\n")
			)
	in
	
	
	let get_ident i =
		match pg_get_desc game i with
			None -> ('!', [||])
		|	Some s -> 
				if String.length s = 1
				then (String.get s 0, [||])
				else if String.get s 1 = '('
				then (String.get s 0, Array.of_list (List.map int_of_string (StringUtils.explode (List.hd (StringUtils.explode (StringUtils.rest_string s 2) ')')) ',')))
				else (String.get s 0, [|int_of_string (StringUtils.rest_string s 1)|])
	in
	
	
	_strat_impr_callback := Some (fun strat counter ->
		let node_compare = node_total_ordering_by_position in
		let valu = evaluate_strategy game node_compare strat in
		let less i j = node_valuation_ordering game node_compare valu.(i) valu.(j) < 0 in
		let counter_strat =
			Array.init (Array.length valu) (fun i ->
				if pg_get_owner game i = plr_Odd
				then best_decision_by_valuation_ordering game node_compare valu i
				else -1
			)
		in

		if counter >= !start_iteration && (match !end_iteration with None -> true | Some end_idx -> counter <= end_idx) then (
			before_iteration counter;
			let n = pg_size game in
			for i = 0 to n - 1 do
			let pl = pg_get_owner game i in
			let tr = pg_get_successors game i in
				let from_node = get_ident i in
				ns_iter (fun j ->
					    let to_node = get_ident j in
					    let kind =
					      if pl = plr_Even then
						if strat.(i) = j
						then Even_player_strategy
						else if less j strat.(i)
						then Even_player_disabled
						else Even_player_improving
					      else
                        			if counter_strat.(i) = j
                        			then Odd_player_strategy
						else Odd_player_disabled
					    in
					    set_edge_iteration from_node to_node kind
					   ) tr
    			   done;		
		after_iteration counter;
		);
	);
	
	match !solver with
		None -> ()
	|	Some solve -> let _ = solve game in ()
	
	
