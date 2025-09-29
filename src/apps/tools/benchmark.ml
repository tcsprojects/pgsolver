open Arg ;;
open Tcsargs;;
open Tcslist;;
open Basics ;;
open Univsolve;;
open Solvers ;;
open Paritygame ;;
open Tcstiming ;;
open Tcsstrings;;

module CommandLine =
struct
  let solvers = ref []
  let input_files = ref []
  let times = ref 10
  let silent = ref false
  let gnuplotformat = ref false
  let title = ref "Benchmark Statistics"
  let innertitle = ref ""
  let timeout = ref (-1.0)

  let satsolv = Satsolvers.get_list ()

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

  let to_global alg pg =
	let (c, g) = induce_counting_partialparitygame pg 0 in
	let _ = alg g in
	(([||],[||]), !c)

  let speclist =  [(*(["--all"; "-a"], Unit(fun _ -> enum_solvers (fun s i _ _ -> solvers := (i, s)::!solvers)),
                      "\n     use all solvers") ; *)
                    (["--silent"; "-s"], Unit(fun _ -> silent := true),
                      "\n     only output statistics");
                    (["--gnuplotformat"; "-gp"], Unit(fun _ -> gnuplotformat := true; silent := true),
                      "\n     output gnuplot line");
                    (["--timeout"; "-to"], Int(fun i -> timeout := (float i)),
                      "\n     set a timeout in seconds (default = infty)");
                    (["--name"; "-n"], String(fun s -> innertitle := s; title := !title ^ ": " ^ s),
                      "\n     title name of the statistics");
                   (["--disableglobalopt"; "-dgo"], Unit(fun _ -> univsolve_global_optimization := false),
                      "\n     disable global optimization");
                   (["--disableuselesscycles"; "-dul"], Unit(fun _ -> univsolve_globalopt_remove_useless_self_cycles := false),
                      "\n     disable removing useless cycles [global optimization]") ;
                   (["--disableusefulcycles"; "-duf"], Unit(fun _ -> univsolve_globalopt_solve_useful_self_cycles := false),
                      "\n     disable exploiting useful cycles [global optimization]") ;
                   (["--disablesccdecomposition"; "-dsd"], Unit(fun _ -> univsolve_decompose_sccs := false),
                      "\n     disable scc decomposition") ;
                   (["--disablelocalopt"; "-dlo"], Unit(fun _ -> univsolve_local_optimization := false),
                      "\n     disable local optimization") ;
                   (["--enableprioprop"; "-pp"], Unit(fun _ -> univsolve_localopt_priority_propagation := true),
                      "\n     enable priority propagation [local optimization]") ;
                   (["--disablepriocomp"; "-dcp"], Unit(fun _ -> univsolve_localopt_compact_priorities := false),
                      "\n     disable compactation of priorities [local optimization]") ;
                   (["--disablespecialgames"; "-dsg"], Unit(fun _ -> univsolve_solve_special_games := false),
                      "\n     disable optimized solving of special games") ;
                   (["--disablesingleparity"; "-dpa"], Unit(fun _ -> univsolve_solvespec_single_parity := false),
                      "\n     disable solving single parity [special game]") ;
                   (["--disablesingleplayer"; "-dpl"], Unit(fun _ -> univsolve_solvespec_single_player := false),
                      "\n     disable solving single player [special game]") ;
                    (["--times"; "-t"], Int(fun i -> times := i),
                      "\n     how many times the solving process is iterated (default is 10)")]
                      @


                      (if List.length satsolv < 2 then [] else
		       [(["--changesat"; "-cs"], String (fun s -> Satsolvers.set_default s),
                      "\n     select sat solver; " ^ "default is " ^ ((Satsolvers.get_default ())#identifier) ^
	              "; available: " ^ ListUtils.format (fun f -> f#identifier) (Satsolvers.get_list ()))])

		      @

  fold_solvers (fun solve ident abbrev desc arr ->
  					(["--" ^ ident; "-" ^ abbrev], String(fun solveargs -> let solve = solve (Array.of_list (Tcsstrings.StringUtils.explode solveargs ' ')) in solvers := (ident, (fun g -> (solve g, pg_size g)))::!solvers),
  					 "\n     Use solver: " ^ desc)::arr
  ) []

  @ [(["--localsolver"], String (fun s -> let (solve, _, _) = find_partial_solver s in solvers := (s, to_global (solve [||]))::!solvers),
     ("<algorithm>\n     use one of the following algorithms (experimental):" ^ fold_partial_solvers (fun _ id _ _ s -> s ^ " " ^ id) ""))]

  let header = Info.get_title "Benchmark Tool"
end ;;


open CommandLine ;;

let fmttime t =
	(Printf.sprintf "%.2f" t) ^ " sec";;

let message v f =
	if (v < 1) || (not !silent)
	then Basics.message 0 f;;


let _ =
  SimpleArgs.parsedef speclist (fun f -> input_files := f::!input_files) (header ^ "Usage: benchmark [options] [infile] [infile] ... [infile]\n" ^
                                              "Benchmarks all specified solvers with all parity games given in the <infile> parameters. If these arguments are omitted it reads a single game from STDIN.\n\nOptions are");

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

  let in_channels = if !input_files = [] then [stdin] else List.map open_in !input_files in

  let games = List.map (fun in_channel -> Parsers.parse_parity_game in_channel) in_channels in

  let game_count = ref 0 in
  let game_size = ref 0 in
  let game_edges = ref 0 in
  let game_index = ref 0 in
  let scc_count = ref 0 in
  let largest_scc = ref 0 in

  List.iter (fun g ->
	incr game_count;
	game_size := !game_size + pg_node_count g;
	game_edges := !game_edges + pg_edge_count g;
	game_index := !game_index + pg_get_index g;
	let (sccs, _, _, _) = Paritygame.strongly_connected_components g in
	let larg = ref 0 in
	Array.iter (fun s -> larg := max !larg (ns_size s)) sccs;
	largest_scc := !largest_scc + !larg;
	scc_count := !scc_count + Array.length sccs
  ) games;

  let avgsize = !game_size / !game_count in
  let avgedges = !game_edges / !game_count in
  let avgindex = !game_index / !game_count in
  let avgsccs = !scc_count / !game_count in
  let avgsccsize = !game_size / !scc_count in
  let avglargestscc = !largest_scc / !game_count in

  let stats = ref [] in
  let solvers' = ref !solvers in

  while (!solvers' != []) do
  	let (ident, solve) = List.hd !solvers' in
  	solvers' := List.tl !solvers';

    message 1 (fun _ -> "Benchmarking " ^ ident ^ "...\n");

    let i = ref 0 in
    let best = ref (-1.0) in
    let worst = ref (-1.0) in
	let visited_sum = ref 0 in
    let sum = ref 0.0 in
    let gi = ref 0 in
    let games = ref games in

    while !games != [] do
	let game = List.hd !games in
	games := List.tl !games;
	i := 0;
	while (!i < !times) && ((!timeout < 0.0) || (!best < !timeout)) do
		incr i;
		message 1 (fun _ -> "Game # " ^ string_of_int !gi ^ ", Iteration #" ^ string_of_int !i ^ "...");
		let tim = SimpleTiming.init true in
		let (_, v) = solve game in
		SimpleTiming.stop tim;
		visited_sum := !visited_sum + v;
		message 1 (fun _ -> SimpleTiming.format tim ^ "\n");
		let current = SimpleTiming.read tim in
		sum := !sum +. current;
		if (!best < 0.0) || (!best > current)
		then best := current;
		if (!worst < 0.0) || (!worst < current)
		then worst := current;
	done;
	incr gi;
    done;

    let mean = !sum /. (float (!i * !gi)) in
	let visi_mean = (float !visited_sum) /. (float (!i * !gi)) in

    message 1 (fun _ -> "Finished. Best: " ^ fmttime !best ^ "  Avg: " ^ fmttime mean ^ "  Worst: " ^ fmttime !worst ^ "  Visited-Avg: " ^ (Printf.sprintf "%.2f" visi_mean) ^ "\n\n");

    stats := (ident, !best, mean, !worst, visi_mean)::!stats;

  done;

  if !gnuplotformat then (
	message 0 (fun _ -> "# Title\tAvgSize\tAvgSCCs\tAvgSCCSize\tAvgLargestSCC" ^ (List.fold_left (fun s (ident, _, _, _, _) -> s ^
			"\t" ^ ident ^ "(time)\t" ^ ident ^ "(visited)") "" !stats) ^ "\n");
	message 0 (fun _ -> !innertitle ^ "\t" ^ string_of_int avgsize ^ "\t" ^ string_of_int avgsccs ^ "\t" ^ string_of_int avgsccsize ^ "\t" ^ string_of_int avglargestscc ^ "\t" ^ (List.fold_left (fun s (_, _, avg, _, avg_visi) -> s ^
			"\t" ^ (Printf.sprintf "%.2f" avg)^ "\t" ^ (Printf.sprintf "%.2f" avg_visi)) "" !stats) ^ "\n");
  )
  else (
	message 0 (fun _ ->
	"+-----------------------------------------------------------------------------+\n" ^
	"| " ^ StringUtils.fillup !title 75 ' ' ^ " |\n" ^
        "+-----------------------------------------------------------------------------+\n" ^
        "| " ^ StringUtils.fillup ("Average #nodes: "  ^ string_of_int avgsize ^ ", #edges: " ^ string_of_int avgedges ^ ", index: " ^ string_of_int avgindex) 75 ' ' ^ " |\n" ^
	"+-----------------------------------------------------------------------------+\n" ^
	"| Solver                   |           Best |        Average |          Worst |\n" ^
	"+-----------------------------------------------------------------------------+\n" ^
		(List.fold_left (fun s (ident, best, avg, worst, _) -> s ^
			"| " ^ StringUtils.fillup ident 25 ' ' ^ "|" ^ StringUtils.fillup_left (fmttime best) 15 ' ' ^ " |" ^ StringUtils.fillup_left (fmttime avg) 15 ' ' ^ " |" ^ StringUtils.fillup_left (fmttime worst) 15 ' ' ^ " |\n"
		) "" !stats) ^
	"+-----------------------------------------------------------------------------+\n" ^
	"\n")
  )
