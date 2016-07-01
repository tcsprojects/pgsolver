open OUnit

let random_game (size, max_prio, outdegmin, outdegmax, self_cycles, seed) =
		Random.init seed;
    Array.init size (fun i -> (
			Random.int max_prio,
  	  Random.int 2,
	    Array.map (fun j ->
				if j < i || self_cycles then j else j + 1
			)
	    (Tcsmaths.RandomUtils.get_pairwise_different_from_range
				(outdegmin + Random.int (outdegmax - outdegmin + 1))
				0
				(size-1 - (if self_cycles then 0 else 1))
			),
	    Some (string_of_int i))
   );;

let game_profiles = Array.init 500 (fun i ->
	let k = i + 2 in
	(10 + k, 10 + k, 3, 3, true, k)
);;

open Univsolve


let solvers = [|
	("recursive", [||], 500);
	("bigstep", [||], 20); 
	("dominiondec", [||], 40);
	("fpiter", [||], 30);
	("genetic", [||], 40); 
	("guessstrategy", [||], 200);
	("modelchecker", [||], 150);
	("smallprog", [||], 10);
	("optstratimprov", [||], 150);
	("stratimprdisc", [||], 25);
	("stratimprloc2", [||], 150);
	("stratimprlocal", [||], 150);
	("stratimprove", [||], 150);
	("policyiter", [|"-sirr"|], 50);
	("policyiter", [|"-slre"|], 50);
	("policyiter", [|"-slrb"|], 50);
	("policyiter", [|"-slbi"|], 50);
	("policyiter", [|"-sh"|], 50);
	("policyiter", [|"-sf"|], 50);
	("policyiter", [|"-sb"|], 50);
	("policyiter", [|"-sa"|], 50);
	("policyiter", [|"-rfo"|], 50);
	("policyiter", [|"-rf"|], 50);
	("policyiter", [|"-re"|], 50);
	("policyiter", [|"-drf"|], 50);
|]



let test_fixture = "Solvers Test" >::: Array.to_list (Array.map (fun (solver_str, solver_opts, upto) ->
	"Test: " ^ solver_str ^ " " ^ Tcsarray.ArrayUtils.format (fun s -> s) solver_opts >:: (fun () ->
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
			Array.iteri (fun i profile ->
				if (upto >= i) then (
					let game = random_game profile in
					let (solver, _, _) = Solvers.find_solver solver_str in
					let (solution, strategy) = (solver solver_opts) game in
					let verified = Verification.verify_solution_strategy_univ game solution strategy in
					assert_equal ~msg:(solver_str ^ " : game " ^ string_of_int i) None verified
				)
			) game_profiles
	)
) solvers)




let _ = run_test_tt ~verbose:true test_fixture