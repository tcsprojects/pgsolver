open Paritygame ;;
open Basics ;;
open Univsolve ;;
open Tcstiming;;
open Satwrapper;;
open Transformations;;


type vars =
|	Strategy of (int * int)
|	Winning of int
|	WorstPathSet of (int * int)
|	CompPathSet of (int * int * int)
|	LowerNode of (int * int)


let msg_tagged v = message_autotagged v (fun _ -> "STRATIMPRSAT");;
let msg_plain = message;;

let solve' game sink0 sink1 =
	let solver = new Satwrapper.satWrapper (Satsolvers.get_default ()) None in

	msg_tagged 2 (fun _ -> "Using backend sat solver: " ^ (Satsolvers.get_default ())#identifier ^ "\n");
	msg_tagged 2 (fun _ -> "Building constraints...\n");

	let n = pg_size game in
	let par a = (pg_get_priority game a) mod 2 in

	for i = 0 to n - 1 do
	  let pl = pg_get_owner game i in
	  let tr = Array.of_list (ns_nodes (pg_get_successors game i)) in
	  (* Strategy *)
	  solver#add_helper_exactlyone 0 (Array.length tr - 1) [||] (fun j -> Po (Strategy (i, tr.(j))));
	  (* Path Sets *)
	  solver#add_clause_array [|Po (WorstPathSet (i, i))|];
	  if not (i = sink0) then	solver#add_clause_array [|Ne (WorstPathSet (sink0, i))|];
	  if not (i = sink1) then solver#add_clause_array [|Ne (WorstPathSet (sink1, i))|];
	  if not (i = sink0 || i = sink1) then
            for j = 0 to n - 1 do
              if not (i = j) then (
                solver#add_clause_array [|Ne (WorstPathSet (i, j)); Ne (WorstPathSet (j, i))|];
                solver#add_formula (Equiv (Atom (WorstPathSet (i, j)), Or (
									   Array.map (fun u -> And [|Atom (Strategy (i, u)); Atom (WorstPathSet (u, j))|]
										     ) tr)));
              )
            done;
	  (* Winning Set *)
	  solver#add_clause_array [|Ne (Winning i); Po (WorstPathSet (i, sink0))|];
	  solver#add_clause_array [|Po (Winning i); Po (WorstPathSet (i, sink1))|]; 
          solver#add_clause_array [|Po (WorstPathSet (i, sink0)); Po (WorstPathSet (i, sink1))|];
	  (* Ordering *)
	  let lower_node_for a b =
	    if not (solver#mem_variable (LowerNode (a, b))) then (
              solver#add_clause_array [|Po (CompPathSet (a, b, n))|];
              for i = 0 to n - 1 do
                solver#add_clause_array [|Ne (CompPathSet (a, b, i)); Po (CompPathSet (a, b, i + 1))|];
                solver#add_clause_array [|Ne (CompPathSet (a, b, i)); Ne (WorstPathSet (a, i)); Po (WorstPathSet (b, i))|];
                solver#add_clause_array [|Ne (CompPathSet (a, b, i)); Ne (WorstPathSet (b, i)); Po (WorstPathSet (a, i))|];
                solver#add_clause_array [|Po (CompPathSet (a, b, i)); Ne (WorstPathSet (a, i)); Ne (WorstPathSet (b, i)); Ne (CompPathSet (a, b, i + 1))|];
                solver#add_clause_array [|Po (CompPathSet (a, b, i)); Po (WorstPathSet (a, i)); Po (WorstPathSet (b, i)); Ne (CompPathSet (a, b, i + 1))|];
              done;
              solver#add_clause_array [|Po (Winning a); Ne (Winning b); Ne (LowerNode (a, b))|];
              solver#add_clause_array [|Ne (Winning a); Po (Winning b); Po (LowerNode (a, b))|];
              for u = 0 to n - 1 do
                if par u = 1 then (
                  solver#add_clause_array [|Ne (Winning a); Ne (Winning b); Po (CompPathSet (a, b, u)); Ne (CompPathSet (a, b, u + 1)); Ne (WorstPathSet (b, u)); Ne (LowerNode (a, b))|];
                  solver#add_clause_array [|Ne (Winning a); Ne (Winning b); Po (CompPathSet (a, b, u)); Ne (CompPathSet (a, b, u + 1)); Po (WorstPathSet (b, u)); Po (LowerNode (a, b))|];
                  solver#add_clause_array [|Po (Winning a); Po (Winning b); Po (CompPathSet (a, b, u)); Ne (CompPathSet (a, b, u + 1)); Ne (WorstPathSet (b, u)); Ne (LowerNode (a, b))|];
                  solver#add_clause_array [|Po (Winning a); Po (Winning b); Po (CompPathSet (a, b, u)); Ne (CompPathSet (a, b, u + 1)); Po (WorstPathSet (b, u)); Po (LowerNode (a, b))|]
                )
                else (
                  solver#add_clause_array [|Ne (Winning a); Ne (Winning b); Po (CompPathSet (a, b, u)); Ne (CompPathSet (a, b, u + 1)); Ne (WorstPathSet (b, u)); Po (LowerNode (a, b))|];
                  solver#add_clause_array [|Ne (Winning a); Ne (Winning b); Po (CompPathSet (a, b, u)); Ne (CompPathSet (a, b, u + 1)); Po (WorstPathSet (b, u)); Ne (LowerNode (a, b))|];
                  solver#add_clause_array [|Po (Winning a); Po (Winning b); Po (CompPathSet (a, b, u)); Ne (CompPathSet (a, b, u + 1)); Ne (WorstPathSet (b, u)); Po (LowerNode (a, b))|];
                  solver#add_clause_array [|Po (Winning a); Po (Winning b); Po (CompPathSet (a, b, u)); Ne (CompPathSet (a, b, u + 1)); Po (WorstPathSet (b, u)); Ne (LowerNode (a, b))|]
                )
              done
            )
          in
	  (* Final Ordering *)
	  Array.iter (fun j ->
		      Array.iter (fun k ->
				  if not (j = k) then (
				    lower_node_for k j;
				    if pl = plr_Even
				    then solver#add_clause_array [|Ne (Strategy (i, j)); Po (LowerNode(k, j))|]
				    else solver#add_clause_array [|Ne (Strategy (i, k)); Po (LowerNode(k, j))|]
				  )
				 ) tr
		     ) tr
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
	
	let sol = Array.init n (fun i -> if solver#get_variable (Winning i) = 0 then plr_Even else plr_Odd) in
	let strat = Array.init n (fun i -> if sol.(i) = pg_get_owner game i
	                                   then let delta = Array.of_list (ns_nodes (pg_get_successors game i)) in
	                                        delta.(solver#get_variable_first (Array.map (fun j -> Strategy (i, j)) delta)) else -1) in
	
	solver#dispose;
	(sol, strat);;
  
  
let solve'' game =
  let n = pg_size game in
  let game_cheap = cheap_escape_cycles_transformation game false in
  let (game_cheap, a, b) = sort_game_by_prio game_cheap in
  let (sink0, sink1) = (b.(Array.length b - 4), b.(Array.length b - 3)) in
  let (sol, str) = solve' game_cheap sink0 sink1 in
  (Array.init n (fun i -> sol.(b.(i))),
   Array.init n (fun i -> let k = str.(b.(i)) in
	 		  if k < 0 then k else a.(k)));;
  
let solve''' game =
  let game' = alternating_transformation game true in
  let (sol, strat) = solve'' game' in
  let (sol', strat') = alternating_revertive_restriction game game' sol strat in
  for i = 0 to pg_size game - 1 do
    if sol'.(i) != pg_get_owner game i then strat'.(i) <- -1
  done;
  (sol', strat');;
  
  
let solve game =
  universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve''' game
		  
let register _ =
  Solverregistry.register_solver solve "stratimprsat" "is" "solve the game by a strategy improvement reduction to SAT";;
  
  
