open Basics ;;
open Tcsargs;;
open Tcslist;;
open Arg ;;
open Tcstiming;;
open Paritygame ;;
open Verification ;;
open Univsolve ;;
open Solvers ;;
open Generators ;;
open Whoiswho ;;
(* open Pgprofiling;; *)

module CommandLine =
struct
  type solver = NoSolver
              | YesSolver of string * Solverregistry.global_solver_factory
	      | LocalSolver of string * Solverregistry.partial_solver_factory

  type generator = NoGenerator
                 | YesGenerator of string * (string array -> paritygame)

  type verifier = NoVerifier
  		| YesVerifier of (paritygame -> solution -> strategy -> (node list * string) option)

  let solver = ref NoSolver
  
  let generator = ref NoGenerator

  let solonly = ref false
  let set_verbosity i = if i >=0 && i <= 3 then verbosity := i
  let be_quiet _ = set_verbosity 0
  let be_solonly _ = set_verbosity 0; solonly := true
  let be_verbose _ = set_verbosity 2
  let be_very_verbose _ = set_verbosity 3

  let make_dotty_graph = ref false
  let dotty_file = ref ""
  let input_file = ref ""

  let parse_sol = ref ""

  let solving = ref true
  
  let initnode = ref 0 

  let verifier = ref NoVerifier

  let format_game = ref false
  let print_strategies = ref true

  let set_dotty_output f = dotty_file := f;
                           make_dotty_graph := true

  let set_input_file f = input_file := f

  let satsolv = Satsolvers.get_list ()
  
  let solveargs = ref ""

(*  let perform_sanity_check = ref false *)

  let univsolve_global_optimization = ref true
  let univsolve_decompose_sccs = ref true
  let univsolve_solve_special_games = ref true
  let univsolve_local_optimization = ref true
  let univsolve_globalopt_remove_useless_self_cycles = ref true
  let univsolve_globalopt_solve_useful_self_cycles = ref true
  let univsolve_solvespec_single_parity = ref true
  let univsolve_solvespec_single_player = ref true
  let univsolve_localopt_priority_propagation = ref false
  let univsolve_localopt_compact_priorities = ref true

  let genlist = fold_generators (fun _ ident _ t -> t ^ " " ^ ident) ""
  let genargs = ref ""
  let printsolverinfo = ref false

  let speclist =  [ (["-v"], Int(set_verbosity),
                      "<level>\n     sets the verbosity level, valid arguments are 0 (quiet), 1 (default), 2 (verbose) and 3 (very verbose)");
(*                   (["--verbose"], Unit(be_verbose), "\n     causes the program to be verbose, same as `-v 2'");
                   (["--quiet"], Unit(be_quiet), "\n     causes the program to be quiet, same as `-v 0'");
                   (["--debug"], Unit(be_very_verbose), "\n     causes the program to be very verbose, same as `-v 3'"); *)
                   (["--printsolonly"], Unit(be_solonly), "\n     causes the program to simply output a parsable solution");
                   (["-d"], String(set_dotty_output),
                      "<filename>\n     output the solution as a coloured graph in dotty format into <filename>");
                   (["--printsolvedgame"; "-pg"], Unit(fun _ -> format_game := true),
                      "\n     outputs the solved (!) game on STDOUT");
                   (["--parsesolution"; "-ps"], String(fun s -> parse_sol := s; solving := false),
                      "<filename>\n     parses the solution to the game from FILE");
                   (["--justheatCPU"; "-jh"], Unit(fun _ -> print_strategies := false),
                      "\n     suppress the printing of the strategies and the winning regions");
(*                   (["--sanitycheck"; "-sc"], Unit(fun _ -> perform_sanity_check := true),
                      "\n     checks whether the game is well-formed"); *)
                   (["--verify"; "-ve"], Unit(fun _ -> verifier := YesVerifier verify_solution_strategy_univ),
                      "\n     verify solution using the universal solver (fast)") ;
(*                   (["--verify2"], Unit(fun _ -> verifier := YesVerifier verify_solution_strategy_direct),
                      "\n     verify solution using an alternative method") ; *)
(*                   (["--verify3"], Unit(fun _ -> verifier := YesVerifier verify_solution_strategy_generic),
                      "\n     verify solution using another alternative method") ; *)
(*                   (["-nosolve"], Unit(fun _ -> solver := NoSolver),
                      "\n     suppress solving, can be used to simply draw a parity game in dotty format") ; *)
                   (["--disableglobalopt"; "-dgo"], Unit(fun _ -> univsolve_global_optimization := false),
                      "\n     disable global optimization");
(*                   (["--disableuselesscycles"; "-dul"], Unit(fun _ -> univsolve_globalopt_remove_useless_self_cycles := false),
                      "\n     disable removing useless cycles [global optimization]") ;
                   (["--disableusefulcycles"; "-duf"], Unit(fun _ -> univsolve_globalopt_solve_useful_self_cycles := false),
                      "\n     disable exploiting useful cycles [global optimization]") ; *)
                   (["--disablesccdecomposition"; "-dsd"], Unit(fun _ -> univsolve_decompose_sccs := false),
                      "\n     disable scc decomposition") ;
                   (["--disablelocalopt"; "-dlo"], Unit(fun _ -> univsolve_local_optimization := false),
                      "\n     disable local optimization") ;
(*                   (["--enableprioprop"; "-pp"], Unit(fun _ -> univsolve_localopt_priority_propagation := true),
                      "\n     enable priority propagation [local optimization]") ;
                   (["--disablepriocomp"; "-dcp"], Unit(fun _ -> univsolve_localopt_compact_priorities := false),
                      "\n     disable compactation of priorities [local optimization]") ; *)
                   (["--disablespecialgames"; "-dsg"], Unit(fun _ -> univsolve_solve_special_games := false),
                      "\n     disable optimized solving of special games") ;
(*                   (["--disablesingleparity"; "-dpa"], Unit(fun _ -> univsolve_solvespec_single_parity := false),
                      "\n     disable solving single parity [special game]") ;
                   (["--disablesingleplayer"; "-dpl"], Unit(fun _ -> univsolve_solvespec_single_player := false),
                      "\n     disable solving single player [special game]") ; *)
                   (["--solverinfo"], Unit (fun _ -> printsolverinfo := true),
                      "\n     output information about all available solvers");
                   (["--globallysolve"; "-global"], String (fun s -> let (solve, _, _) = find_solver s in solver := YesSolver (s, solve)),
                     "<solver>\n     solves globally, valid solvers are" ^
                     fold_solvers (fun _ ident _ _ t -> t ^ " " ^ ident) ""); 
                   (["--locallysolve"; "-local"], Tuple [String (fun s -> let (solve, _, _) = find_partial_solver s in solver := LocalSolver (s, solve));
                                                        Int (fun i -> initnode := i)],
                     "<solver> <node>\n     solves locally, valid solvers are" ^
                     fold_partial_solvers (fun _ ident _ _ t -> t ^ " " ^ ident) ""); 
                   (["--args"; "-x"], String(fun s -> solveargs := s),
                      "\n     pass args to the solver (write '-x \"--help\"' to learn about available args (if there are any))")]
                      @
                      (if List.length satsolv < 2 then [] else
		       [(["--changesat"; "-cs"], String (fun s -> Satsolvers.set_default s),
                      "\n     select sat solver; " ^ "default: " ^ ((Satsolvers.get_default ())#identifier) ^
	              "; available: " ^ ListUtils.format (fun f -> f#identifier) (Satsolvers.get_list ()))])
		      @
		           (if genlist = "" then [] else [
					(["--generator"; "-gen"], Tuple [String (fun s -> let (gen, ident) = find_generator s in generator := YesGenerator (ident, gen));
					                                 String (fun a -> genargs := a)],
                     "<generator> \"<args>\"\n     use generator, valid ones are" ^ genlist)
		           ])

  let header = Info.get_title "Parity Game Solver"
end ;;

open CommandLine ;;

let _ =
  Random.self_init ();
  init_message_timing ();
  SimpleArgs.parsedef speclist (fun f -> set_input_file f) (header ^ "Usage: pgsolver [options] [infile]\n" ^
                                              "Solves the parity game given in <infile>. If this argument is omitted it reads a game from STDIN.\n\nOptions are");

  (Univsolve.universal_solve_global_options := fun gen_stat verb -> {
	generate_statistics = gen_stat ;
	verb_level = verb ;
	global_optimization = !univsolve_global_optimization ;
	decompose_sccs = !univsolve_decompose_sccs;
	solve_special_games = !univsolve_solve_special_games ;
	local_optimization = !univsolve_local_optimization ;
	globalopt_remove_useless_self_cycles = !univsolve_globalopt_remove_useless_self_cycles ;
	globalopt_solve_useful_self_cycles = !univsolve_globalopt_solve_useful_self_cycles ;
	solvespec_single_parity = !univsolve_solvespec_single_parity ;
	solvespec_single_player = !univsolve_solvespec_single_player ;
	localopt_priority_propagation = !univsolve_localopt_priority_propagation ;
	localopt_compact_priorities = !univsolve_localopt_compact_priorities ;
  });

  message 1 (fun _ -> header);
  if !printsolverinfo then (
  	  message 1 (fun _ -> "Available global solver are\n");
  	  fold_solvers (fun _ ident _ desc _ -> message 1 (fun _ -> "    " ^ ident ^ " : " ^ desc ^ "\n")) ();
  	  message 1 (fun _ -> "\nAvailable local solver are\n");
  	  fold_partial_solvers (fun _ ident _ desc _ -> message 1 (fun _ -> "    " ^ ident ^ " : " ^ desc ^ "\n")) ();
  )

  else
  
  (
  let game = match !generator with

    NoGenerator -> (
	  message 2 (fun _ -> "Reading game from ..................... ");
	  let (in_channel,name) = if !input_file = "" then (stdin,"STDIN") else (open_in !input_file,!input_file) in
	  message 2 (fun _ -> name ^ "\n");

	  message 1 (fun _ -> "Parsing ............................... ");
	  let timobj = SimpleTiming.init true in
(*	  let game = Parserhelper.parse_from_channel in_channel !perform_sanity_check in *)
	  let game = (match !solver with
			LocalSolver _ -> let (init, g) = Parsers.parse_init_parity_game in_channel in
					 initnode := init;
					 g
		      | _             -> Parsers.parse_parity_game in_channel
		     )
	  in
	  SimpleTiming.stop timobj;
	  message 1 (fun _ -> (SimpleTiming.format timobj) ^ "\n");
	  game
	)
  | YesGenerator (ident, gen) -> (
	  message 1 (fun _ -> "Generating " ^ ident ^ " with arguments '" ^ !genargs ^ "'... ");
	  let timobj = SimpleTiming.init true in
	  let game = gen (Array.of_list (Tcsstrings.StringUtils.explode !genargs ' ')) in
	  SimpleTiming.stop timobj;
	  message 1 (fun _ -> (SimpleTiming.format timobj) ^ "\n");
	  game
    )
  in
  
  flush stdout;
  Gc.compact ();

  match !solver with
	LocalSolver (id, solve) -> (
		 message 1 (fun _ -> "Chosen local solver `" ^ id ^ "' " ^ String.make (16 - (String.length id)) '.' ^ " ");
		 let timobj = SimpleTiming.init true in
		 let (c, g) = induce_counting_partialparitygame game !initnode in
		 let result = (solve (Array.of_list (Tcsstrings.StringUtils.explode !solveargs ' '))) g in
		 SimpleTiming.stop timobj;
		 message 1 (fun _ -> (SimpleTiming.format timobj) ^ "\n");
		 if SimpleTiming.read timobj > 10.0
		 then message 1 (fun _ -> "\nOh, and by the way: " ^ someone () ^
								  " solved this game in " ^
								  Printf.sprintf "%.2f" (Random.float 10.0) ^ " sec.\n");
		message 1 (fun _ -> "Visited " ^ string_of_int !c ^ " nodes.\n");
		message 1 (fun _ -> "Winner of initial node is player " ^ string_of_int (if fst (result !initnode) = plr_Even then 0 else 1) ^ "\n\n")
	)
  | _ -> (
	  let (solution,strategy) =
		match !solver with
			  NoSolver            -> if !parse_sol = "" then (
										 message 1 (fun _ -> "Chosen not to solve at all.\n");
										 ([||],[||])
									 )
									 else (
										Parsers.parse_solution (open_in !parse_sol)
									 )
			| YesSolver(id,solve) -> message 1 (fun _ -> "Chosen solver `" ^ id ^ "' " ^
												String.make (22 - (String.length id)) '.' ^ " ");
									 let timobj = SimpleTiming.init true in
									 let result = solve (Array.of_list (Tcsstrings.StringUtils.explode !solveargs ' ')) game in
									 SimpleTiming.stop timobj;
									 message 1 (fun _ -> (SimpleTiming.format timobj) ^ "\n");
									 if SimpleTiming.read timobj > 10.0
									 then message 1 (fun _ -> "\nOh, and by the way: " ^ someone () ^
															  " solved this game in " ^
															  Printf.sprintf "%.2f" (Random.float 10.0) ^ " sec.\n");
									 result
			| _ -> failwith "impossible"
	  in

	  let win0 = ref [] in
	  let win1 = ref [] in
	  let str0 = ref [] in
	  let str1 = ref [] in
	  sol_iter (fun j -> fun pl -> let ow = pg_get_owner game j in
				       if pl = plr_Even then
					 (win0 := j :: !win0;
					  if ow=plr_Even then let k = str_get strategy j in
							      str0 := (nd_show j ^ "->" ^ nd_show k) :: !str0)
				       else if pl= plr_Odd then
					 (win1 := j :: !win1;
					  if ow=plr_Odd then let k = str_get strategy j in
							     str1 := (nd_show j ^ "->" ^ nd_show k) :: !str1)
		   ) solution;

	  if (!print_strategies) then (
		  let first = ref false in
		  message 1 (fun _ -> "\nPlayer 0 wins from nodes:\n  ");
		  message 1 (fun _ -> "{");
		  first := true;
		  List.iter (fun i -> message 1 (fun _ -> (if !first then "" else ", ") ^ nd_show i); first := false) !win0;
		  message 1 (fun _ -> "}\n");
		  message 1 (fun _ -> "with strategy\n  ");
		  message 1 (fun _ -> "[" ^ String.concat "," (List.rev !str0) ^ "]\n\n");
		  message 1 (fun _ -> "Player 1 wins from nodes:\n  ");
		  message 1 (fun _ -> "{");
		  first := true;
		  List.iter (fun i -> message 1 (fun _ -> (if !first then "" else ", ") ^ nd_show i); first := false) !win1;
		  message 1 (fun _ -> "}\n");
		  message 1 (fun _ -> "with strategy\n  ");
		  message 1 (fun _ -> "[" ^ String.concat "," (List.rev !str1) ^ "]\n");
	  );

	  if !make_dotty_graph then Paritygame.to_dotty_file game solution strategy !dotty_file;

	  if !format_game then Paritygame.print_game (Paritygame.subgame_by_strat game strategy);

	  if !solonly then Paritygame.print_solution_strategy_parsable solution strategy;

	  match !verifier with
		YesVerifier verify -> (
			let timobj = SimpleTiming.init true in
			message 1 (fun _ -> "\n\nVerifying... ");
			match (verify game solution strategy) with
				None -> (
					SimpleTiming.stop timobj;
					message 1 (fun _ -> (SimpleTiming.format timobj) ^ " - valid solution and strategy!\n\n")
				)
			|   Some (trace, err) -> (
					SimpleTiming.stop timobj;
					message 1 (fun _ -> (SimpleTiming.format timobj) ^ " - " ^
					"INVALID solution and/or strategy!\n" ^
					"Reason: " ^ err ^ "\n" ^
					"Trace:  " ^ (List.fold_left (fun s i -> s ^ "->" ^ nd_show i) "" trace) ^ "\n\n"
				);
				exit 1)
		)
		| NoVerifier -> ()
	)
  )
  (*  prof_print_results () *)
