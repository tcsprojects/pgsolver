open Arg ;;
open Tcsargs;;
open Basics ;;
open Tcsstrings ;;
open Univsolve;;
open Solvers ;;
open Stratimpralgs ;;

module CommandLine =
struct
  let solvers = ref []
  let input_files = ref []
  let timesx = ref 10
  let intelli_timing = ref true
  let silent = ref false
  let gnuplotformat = ref false
  let title = ref "Benchmark Statistics"
  let measure_function = ref get_last_iteration_count
  let innertitle = ref ""
  let timeout = ref (-1.0)

  let speclist =  [(["--silent"; "-s"], Unit(fun _ -> silent := true),
                      "\n     only output statistics");
                    (["--measureexpbits"; "-meb"], Unit(fun _ -> enable_exp_bit_count := true;
                                                                 measure_function := get_last_exp_bit_count),
                      "\n     measure exponential bits [internal use]");
                    (["--gnuplotformat"; "-gp"], Unit(fun _ -> gnuplotformat := true; silent := true),
                      "\n     output gnuplot line");
                    (["--name"; "-n"], String(fun s -> innertitle := s; title := !title ^ ": " ^ s),
                      "\n     title name of the statistics");
                    (["--times"; "-t"], Int(fun i -> timesx := i; intelli_timing := false),
                      "\n     how many times the solving process is iterated (default is intelligent timing)")]

		      @

  fold_solvers (fun solve ident abbrev desc arr ->
  					(["--" ^ ident; "-" ^ abbrev], String(fun solveargs -> let solve = solve (Array.of_list (Tcsstrings.StringUtils.explode solveargs ' ')) in solvers := (ident, solve)::!solvers),
  					 "\n     Use solver: " ^ desc)::arr
  ) []


  let header = Info.get_title "Strategy Improvement Benchmark Tool"
end ;;


open CommandLine ;;

let fmttime t =
	(Printf.sprintf "%.2f" t) ^ " sec";;

let message v f =
	if (v < 1) || (not !silent)
	then Basics.message 0 f;;


let _ =
  SimpleArgs.parsedef speclist (fun f -> input_files := f::!input_files) (header ^ "Usage: benchstratimpr [options] [infile] [infile] ... [infile]\n" ^
                                              "Benchmarks all specified solvers with all parity games given in the <infile> parameters. If these arguments are omitted it reads a single game from STDIN.\n\nOptions are");

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

  let in_channels = if !input_files = [] then [stdin] else List.map open_in !input_files in

  let games = List.map (fun in_channel -> Parsers.parse_parity_game in_channel) in_channels in

  let stats = ref [] in
  let solvers' = ref !solvers in

  while (!solvers' != []) do
  	let (ident, solve) = List.hd !solvers' in
  	solvers' := List.tl !solvers';

    message 1 (fun _ -> "Benchmarking " ^ ident ^ "...\n");

    let i = ref 0 in
    let best = ref (-1) in
    let worst = ref (-1) in
    let sum = ref 0 in
    let gi = ref 0 in
    let games = ref games in
    let times = ref 0 in

    while !games != [] do
	let game = List.hd !games in
	games := List.tl !games;
	i := 0;
	times := !timesx;
	while (!i < !times) do
		incr i;
		message 1 (fun _ -> "Game # " ^ string_of_int !gi ^ ", Iteration #" ^ string_of_int !i ^ "...");
		let _ = solve game in
		let itr = !measure_function () in
		sum := !sum + itr;
	    let mean = (float !sum) /. (float !i) in
		message 1 (fun _ -> string_of_int itr ^ " ~ " ^ Printf.sprintf "%.2f" mean ^ "\n");
		if (!best < 0) || (!best > itr)
		then best := itr;
		if (!worst < 0) || (!worst < itr)
		then worst := itr;
	done;
	incr gi;
    done;

    let mean = (float !sum) /. (float (!i * !gi)) in

    message 1 (fun _ -> "Finished. Best: " ^ string_of_int !best ^ "  Avg: " ^ Printf.sprintf "%.2f" mean ^ "  Worst: " ^ string_of_int !worst ^ "\n\n");

    stats := (ident, !best, mean, !worst)::!stats;

  done;

  if !gnuplotformat then (
	message 0 (fun _ -> "# Title" ^ (List.fold_left (fun s (ident, _, _, _) -> s ^
			"\t" ^ ident) "" !stats) ^ "\n");
	message 0 (fun _ -> !innertitle ^ "\t" ^ (List.fold_left (fun s (_, _, avg, _) -> s ^
			"\t" ^ Printf.sprintf "%.2f" avg) "" !stats) ^ "\n");
  )
  else (
	message 0 (fun _ ->
	"+-----------------------------------------------------------------------------+\n" ^
	"| " ^ StringUtils.fillup !title 75 ' ' ^" |\n" ^
	"+-----------------------------------------------------------------------------+\n" ^
	"| Solver                   |           Best |        Average |          Worst |\n" ^
	"+-----------------------------------------------------------------------------+\n" ^
		(List.fold_left (fun s (ident, best, avg, worst) -> s ^
			"| " ^ StringUtils.fillup ident 25 ' ' ^ "|" ^ StringUtils.fillup_left (string_of_int best) 15 ' ' ^ " |" ^ StringUtils.fillup_left (Printf.sprintf "%.2f" avg) 15 ' ' ^ " |" ^ StringUtils.fillup_left (string_of_int worst) 15 ' ' ^ " |\n"
		) "" !stats) ^
	"+-----------------------------------------------------------------------------+\n" ^
	"\n")
  )
