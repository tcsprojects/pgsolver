open Paritygame ;;
open Basics ;;
open Univsolve ;;
open Tcstiming;;
open Satwrapper;;


type vars =
 	Winning of int
|	Strategy of (int * int)
|	SubEdge of (player * int * int)
|	ReachLower of (player * int * int * int)


let msg_tagged v = message_autotagged v (fun _ -> "SATSOLVE");;
let msg_plain = message;;

let solve' game =
	let solver = new Satwrapper.satWrapper (Satsolvers.get_default ()) None in

	msg_tagged 2 (fun _ -> "Using backend sat solver: " ^ (Satsolvers.get_default ())#identifier ^ "\n");
	msg_tagged 2 (fun _ -> "Building constraints...\n");

	let n = pg_size game in
	let prio_arr = Array.init n (fun i -> i) in
	Array.sort (fun i j -> compare (pg_get_priority game i) (pg_get_priority game j)) prio_arr;

	pg_iterate (fun i -> fun (_,_,d,_,_) -> let delta = Array.of_list (ns_nodes d) in
						solver#add_helper_exactlyone 0 (Array.length delta - 1) [||] (fun j -> Po (Strategy (i, delta.(j))))) game;

	pg_iterate (fun i -> fun (_,pl,delta,_,_) ->
			     ns_iter (fun j ->
				      solver#add_clause_array [|Ne (SubEdge (plr_Even, i, j)); Ne (Winning i)|];
				      solver#add_clause_array [|Ne (SubEdge (plr_Odd, i, j)); Po (Winning i)|];
				      solver#add_clause_array [|Ne (SubEdge (plr_Even, i, j)); Ne (Winning j)|];
				      solver#add_clause_array [|Ne (SubEdge (plr_Odd, i, j)); Po (Winning j)|];
				      solver#add_clause_array [|Ne (SubEdge (pl, i, j)); Po (Strategy (i, j))|];
				      solver#add_clause_array [|if pl = plr_Odd then Po (Winning i) else Ne (Winning i); Po (SubEdge (plr_opponent pl, i, j))|];
				      solver#add_clause_array [|if pl = plr_Odd then Ne (Winning i) else Po (Winning i); Ne (Strategy (i, j)); Po (SubEdge (pl, i, j))|]
				     ) delta) game;
							  
	plr_iterate (fun p -> 
	  pg_iterate (fun i -> fun _ ->
            pg_iterate (fun j -> fun _ ->
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
	      done) game
	    ) game);

	pg_iterate (fun i -> fun (pr,_,_,_,_) -> solver#add_clause_array [|Ne (ReachLower (plr_opponent (plr_benefits pr), i, i, i))|]) game;

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

	let sol = Array.init n (fun i -> if solver#get_variable (Winning i) = 0 then plr_Even else plr_Odd) in
	let strat = Array.init n (fun i -> if sol.(i) = pg_get_owner game i
	                                   then let delta = Array.of_list (ns_nodes (pg_get_successors game i)) in
	                                        delta.(solver#get_variable_first (Array.map (fun j -> Strategy (i, j)) delta)) else -1)
	in
	solver#dispose;
	(sol, strat);;


let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;


let register _ = Solverregistry.register_solver solve "satsolve" "ss" "directly solve the game by an NP predicate";;

