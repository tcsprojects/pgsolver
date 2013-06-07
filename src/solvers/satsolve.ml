open Paritygame ;;
open Basics ;;
open Univsolve ;;
open Solvers;;
open Tcstiming;;
open Satwrapper;;


type vars =
 	Winning of int
|	Strategy of (int * int)
|	SubEdge of (int * int * int)
|	ReachLower of (int * int * int * int)


let msg_tagged v = message_autotagged v (fun _ -> "SATSOLVE");;
let msg_plain = message;;

let solve' game =
	let solver = new Satwrapper.satWrapper (Satsolvers.get_default ()) in

	msg_tagged 2 (fun _ -> "Using backend sat solver: " ^ (Satsolvers.get_default ())#identifier ^ "\n");
	msg_tagged 2 (fun _ -> "Building constraints...\n");

	let n = Array.length game in
	let prio_arr = Array.init n (fun i -> i) in
	Array.sort (fun i j -> compare (pg_get_pr game i) (pg_get_pr game j)) prio_arr;

	for i = 0 to n - 1 do
		let (_, pl, delta, _) = game.(i) in
		solver#add_helper_exactlyone 0 (Array.length delta - 1) [||] (fun j -> Po (Strategy (i, delta.(j))));
	done;

	for i = 0 to n - 1 do
		let (_, pl, delta, _) = game.(i) in
		Array.iter (fun j ->
			solver#add_clause_array [|Ne (SubEdge (0, i, j)); Ne (Winning i)|];
			solver#add_clause_array [|Ne (SubEdge (1, i, j)); Po (Winning i)|];
			solver#add_clause_array [|Ne (SubEdge (0, i, j)); Ne (Winning j)|];
			solver#add_clause_array [|Ne (SubEdge (1, i, j)); Po (Winning j)|];
			solver#add_clause_array [|Ne (SubEdge (pl, i, j)); Po (Strategy (i, j))|];
			solver#add_clause_array [|if pl = 1 then Po (Winning i) else Ne (Winning i); Po (SubEdge (1 - pl, i, j))|];
			solver#add_clause_array [|if pl = 1 then Ne (Winning i) else Po (Winning i); Ne (Strategy (i, j)); Po (SubEdge (pl, i, j))|]
        ) delta;
    done;

	for p = 0 to 1 do
        for i = 0 to n - 1 do
            for j = 0 to n - 1 do
            	if solver#mem_variable (SubEdge (p, i, j)) then (
                    solver#add_clause_array [|Ne (ReachLower (p, i, j, -1)); Po (SubEdge (p, i, j))|];
                    solver#add_clause_array [|Po (ReachLower (p, i, j, -1)); Ne (SubEdge (p, i, j))|]
                ) else (
                    solver#add_clause_array [|Ne (ReachLower (p, i, j, -1))|]
                );
                for t = 0 to n - 1 do
                	let s = prio_arr.(t) in
                	let s_1 = if t = 0 then -1 else prio_arr.(t-1) in
                    solver#add_clause_array [|Ne (ReachLower (p, i, j, s)); Po (ReachLower (p, i, j, s_1)); Po (ReachLower (p, i, s, s_1))|];
                    solver#add_clause_array [|Ne (ReachLower (p, i, j, s)); Po (ReachLower (p, i, j, s_1)); Po (ReachLower (p, s, j, s_1))|];
                    solver#add_clause_array [|Po (ReachLower (p, i, j, s)); Ne (ReachLower (p, i, j, s_1))|];
                    solver#add_clause_array [|Po (ReachLower (p, i, j, s)); Ne (ReachLower (p, i, s, s_1)); Ne (ReachLower (p, s, j, s_1))|];
                done
            done
        done
    done;

    for i = 0 to n - 1 do
    	solver#add_clause_array [|Ne (ReachLower (1 - pg_get_pr game i mod 2, i, i, i))|]
    done;

 	let v = solver#variable_count + solver#helper_variable_count in
  	let c = solver#clause_count + solver#helper_clause_count in
  	let l = solver#literal_count + solver#helper_literal_count in
    msg_tagged 2 (fun _ -> "Number of clauses: " ^ string_of_int c ^ " \n");
    msg_tagged 2 (fun _ -> "Number of variables: " ^ string_of_int v ^ " \n");
    msg_tagged 2 (fun _ -> "Number of literals: " ^ string_of_int l ^ " \n");

    let tim = SimpleTiming.init false in
    msg_tagged 2 (fun _ -> SimpleTiming.start tim; "SAT Solving ... ");

    solver#solve;

    msg_plain 2 (fun _ -> SimpleTiming.stop tim; "done: " ^ SimpleTiming.format tim ^ "\n");

    let satis = solver#get_solve_result = SolveSatisfiable in
    if not satis then failwith "impossible: unsatisfiable";

	let sol = Array.init n (fun i -> solver#get_variable (Winning i)) in
	let strat = Array.init n (fun i -> if sol.(i) = pg_get_pl game i
	                                   then let delta = pg_get_tr game i in
	                                        delta.(solver#get_variable_first (Array.map (fun j -> Strategy (i, j)) delta)) else -1) in

	solver#dispose;
	(sol, strat);;


let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;


let _ = register_solver solve "satsolve" "ss" "directly solve the game by an NP predicate";;

