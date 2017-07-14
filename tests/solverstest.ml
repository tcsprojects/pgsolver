open OUnit;;
open Solvers;;
open Paritygame;;

let random_game (size, max_prio, outdegmin, outdegmax, self_cycles, seed) =
		Random.init seed;
    Paritygame.pg_init size (fun i -> (
			Random.int max_prio,
  	  (if Random.int 2 = 0 then plr_Even else plr_Odd),
	    Array.to_list (Array.map (fun j ->
				if j < i || self_cycles then j else j + 1
			)
	    (Tcsmaths.RandomUtils.get_pairwise_different_from_range
				(outdegmin + Random.int (outdegmax - outdegmin + 1))
				0
				(size-1 - (if self_cycles then 0 else 1))
			)),
	    Some (string_of_int i))
   );;

let game_profiles = Array.init 500 (fun i ->
	let k = i + 2 in
	(10 + k, 10 + k, 3, 3, true, k)
);;

open Univsolve
open Verification


let solvers = [|
	("recursive", [||], 150, false);
	("recursive", [||], 150, true);
	("bigstep", [||], 20, false); 
	("bigstep", [||], 20, true); 
	("dominiondec", [||], 40, false);
	("dominiondec", [||], 40, true);
	("fpiter", [||], 30, false);
	("fpiter", [||], 30, true);
	("genetic", [||], 40, false); 
	("genetic", [||], 40, true); 
	("guessstrategy", [||], 150, false);
	("guessstrategy", [||], 150, true);
	("modelchecker", [||], 150, false);
	("modelchecker", [||], 150, true);
	("smallprog", [||], 10, false);
	("smallprog", [||], 10, true);
	("optstratimprov", [||], 150, false);
	("optstratimprov", [||], 150, true);
	("stratimprdisc", [||], 25, false);
	("stratimprdisc", [||], 25, true);
	("stratimprloc2", [||], 150, false);
	("stratimprloc2", [||], 150, true);
	("stratimprlocal", [||], 150, false);
	("stratimprlocal", [||], 150, true);
	("stratimprove", [||], 150, false);
	("stratimprove", [||], 150, true);
	("policyiter", [|"-sirr"|], 50, false);
	("policyiter", [|"-sirr"|], 50, true);
	("policyiter", [|"-slre"|], 50, false);
	("policyiter", [|"-slre"|], 50, true);
	("policyiter", [|"-slrb"|], 50, false);
	("policyiter", [|"-slrb"|], 50, true);
	("policyiter", [|"-slbi"|], 50, false);
	("policyiter", [|"-slbi"|], 50, true);
	("policyiter", [|"-sh"|], 50, false);
	("policyiter", [|"-sh"|], 50, true);
	("policyiter", [|"-sf"|], 50, false);
	("policyiter", [|"-sf"|], 50, true);
	("policyiter", [|"-sb"|], 50, false);
	("policyiter", [|"-sb"|], 50, true);
	("policyiter", [|"-sa"|], 50, false);
	("policyiter", [|"-sa"|], 50, true);
	("policyiter", [|"-rfo"|], 50, false);
	("policyiter", [|"-rfo"|], 50, true);
	("policyiter", [|"-rf"|], 50, false);
	("policyiter", [|"-rf"|], 50, true);
	("policyiter", [|"-re"|], 50, false);
	("policyiter", [|"-re"|], 50, true);
	("policyiter", [|"-drf"|], 50, false);
	("policyiter", [|"-drf"|], 50, true);
	("succinctsmallprog", [||], 15, false);
	("succinctsmallprog", [||], 15, true);
|]


let verifiers = [|
  ("universal verifier", verify_solution_strategy_univ);
	("alternative verifier", verify_solution_strategy_direct);
	("generic verifier", verify_solution_strategy_generic)
|]


let test_fixture = "Solvers Test" >::: Array.to_list (Array.map (fun (solver_str, solver_opts, upto, univ) ->
	"Test: " ^ solver_str ^ " " ^ Tcsarray.ArrayUtils.format (fun s -> s) solver_opts >:: (fun () ->
(Univsolve.universal_solve_global_options := fun gen_stat verb -> {
	generate_statistics = gen_stat ;
	verb_level = verb ;
	global_optimization = univ ;
	decompose_sccs = univ ;
	solve_special_games = univ ;
	local_optimization = univ ;
	globalopt_remove_useless_self_cycles = univ ;
	globalopt_solve_useful_self_cycles = univ ;
	solvespec_single_parity = univ ;
	solvespec_single_player = univ ;
	localopt_priority_propagation = univ ;
	localopt_compact_priorities = univ ;
  });
			Array.iteri (fun i profile ->
				if (upto >= i) then (
					let game = random_game profile in
					let (solver, _, _) = Solvers.find_solver solver_str in
					let (solution, strategy) = (solver solver_opts) game in
					Array.iter (function (verifier_str, verifier) ->
						let verified = verifier game solution strategy in
						assert_equal ~msg:(solver_str ^ " : game " ^ string_of_int i ^ " verified with " ^ verifier_str) None verified
					) verifiers
				)
			) game_profiles
	)
) solvers)




let _ = run_test_tt ~verbose:true test_fixture