open Arg ;;
open Tcsargs;;
open Basics ;;
open Paritygame ;;
open Parsers ;; 
open Tcstiming ;;
open Tcsset;;
open Tcslist;;
open Satwrapper ;;
open Pseudosatwrapper ;;
open Externalsat ;;

let split s i = (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1));;

let split_ints2 s =
	let (a, b) = split s (String.index s ',') in
	(int_of_string a, int_of_string b);;

let split_ints3 s =
	let (a, b) = split s (String.index s ',') in
	let (b, c) = split b (String.index b ',') in
	(int_of_string a, int_of_string b, int_of_string c);;

verbosity := 1;

(*****************************************************************************
 * Main                                                                      *
 *****************************************************************************)

module CommandLine =
struct

  let nodes = ref 0
  let outdegree = ref (-1)
  let indegree = ref (-1)
  let iterations = ref 0
  let playeronenodes = ref 0
  let option_scc = ref false
  let option_alternating = ref false
  let option_alternatingprios = ref false
  let option_cycleinvariant = ref false
  let option_centered = ref false
  let option_nosingleplayer = ref false
  let option_cyclenode = ref (-1)
  let option_forbid_cyclenode = ref []
  let option_setparity = ref []
  let option_setplayer = ref []
  let option_incrementaliterations = ref false
  let option_forbidpassthrough = ref []
  let option_assumegame = ref None
  let option_lastvaluchange = ref false
  let option_passthroughnodes = ref None
  let option_forbidgame = ref []
  let option_static_dominees = ref []
  let option_nonstatic_dominees = ref []
  let option_nonstatic_attractees = ref []
  let option_printonly = ref false
  let option_enfdominee_lower = ref []
  let option_enfdominee_upper = ref []
  let option_enfptnodes_lower = ref []
  let option_enfptnodes_upper = ref []
  let option_enf_worstset = ref []
  let option_parsesolution = ref ""
  let option_enfedges = ref []
  let option_enfedgecount = ref []
  let option_showiterations = ref false
  let option_preprocess = ref false
  let option_incrgames = ref 1
  let option_improvement_policy = ref "ALLMAX"
  let option_cycleparity = ref None
  let satsolv = Satsolvers.get_list ()

  let speclist =  [(["--nodes"; "-n"], Int (fun i -> nodes := i),
                      "\n     number of nodes in the game (mandatory)") ;
                   (["--iteration"; "-i"], Int (fun i -> iterations := i),
                      "\n     lower bound on the number of iterations (mandatory)") ;
                   (["--playeronenodes"; "-po"], Int (fun i -> playeronenodes := i),
                      "\n     lower bound on the number of nodes of player 1") ;
                   (["--outdegree"; "-od"], Int (fun i -> outdegree := i),
                      "\n     upper bound on the node outdegree") ;
                   (["--indegree"; "-id"], Int (fun i -> indegree := i),
                      "\n     upper bound on the node indegree") ;
                   (["--scc"; "-sc"], Unit(fun _ -> option_scc := true),
                      "\n     game needs to be an scc") ;
                   (["--alternating"; "-al"], Unit(fun _ -> option_alternating := true),
                      "\n     enforce alternating playing positions") ;
                   (["--alternatingprios"; "-ap"], Unit(fun _ -> option_alternatingprios := true),
                      "\n     enforce alternating priorities");
                   (["--cycleinvariant"; "-ci"], Unit(fun _ -> option_cycleinvariant := true),
                      "\n     enforce cycle invariant valuations") ;
                   (["--centered"; "-ce"], Unit(fun _ -> option_centered := true),
                      "\n     enforce centered valuations") ;
                   (["--nosingleplayer"; "-ns"], Unit(fun _ -> option_nosingleplayer := true),
                      "\n     enforce proper game") ;
                   (["--cyclenode"; "-cn"], Int(fun i -> option_cyclenode := i),
                      "\n     enforce cycle node") ;
                   (["--forbidcyclenode"; "-fcn"], Int(fun i -> option_forbid_cyclenode := i::!option_forbid_cyclenode),
                      "\n     forbid cycle node") ;
                   (["--cycleparity"; "-cp"], Int(fun i -> option_cycleparity := Some i),
                      "\n     enforce cycle parity") ;
                   (["--passthroughnodes"; "-pt"], Int(fun i -> option_passthroughnodes := Some i),
                      "\n     lower bound on the number of nodes with outdegree 1") ;
                   (["--setparity"; "-sp"], Int(fun i -> option_setparity := i::!option_setparity),
                      "\n     enforce parity at given node") ;
                   (["--enfplayer"; "-ep"], Int(fun i -> option_setplayer := i::!option_setplayer),
                      "\n     enforce player at given node") ;
                   (["--enfedge"; "-ee"], String(fun s -> option_enfedges := (split_ints2 s)::!option_enfedges),
                      "\n     enforce edge") ;
                   (["--enfedgecount"; "-eec"], String(fun s -> option_enfedgecount := (split_ints2 s)::!option_enfedgecount),
                      "\n     enforce upper limit on edge count w.r.t. a specified node") ;
                   (["--enfworstpathset"; "-ewp"], String(fun s -> option_enf_worstset := (split_ints3 s)::!option_enf_worstset),
                      "\n     enforce nonstatic attractee set for node starting with iteration") ;
                   (["--staticdominees"; "-sd"], String(fun s -> option_static_dominees := (split_ints2 s)::!option_static_dominees),
                      "\n     enforce static dominee set for node starting with iteration") ;
                   (["--nonstaticdominees"; "-nsd"], String(fun s -> option_nonstatic_dominees := (split_ints2 s)::!option_nonstatic_dominees),
                      "\n     enforce nonstatic dominee set for node starting with iteration") ;
                   (["--nonstaticattractees"; "-nsa"], String(fun s -> option_nonstatic_attractees := (split_ints2 s)::!option_nonstatic_attractees),
                      "\n     enforce nonstatic attractee set for node starting with iteration") ;
                   (["--enfdomineelowersize"; "-edl"], String(fun s -> option_enfdominee_lower := (split_ints3 s)::!option_enfdominee_lower),
                      "\n     enforce dominee lower size [iteration,node,size]") ;
                   (["--enfdomineeuppersize"; "-edu"], String(fun s -> option_enfdominee_upper := (split_ints3 s)::!option_enfdominee_upper),
                      "\n     enforce dominee upper size [iteration,node,size]") ;
                   (["--enfptnodeslowersize"; "-epl"], String(fun s -> option_enfptnodes_lower := (split_ints3 s)::!option_enfptnodes_lower),
                      "\n     enforce ptnodes lower size [iteration,node,size]") ;
                   (["--enfptnodesuppersize"; "-epu"], String(fun s -> option_enfptnodes_upper := (split_ints3 s)::!option_enfptnodes_upper),
                      "\n     enforce ptnodes upper size [iteration,node,size]") ;
                   (["--forbidpassthrough"; "-ft"], Int(fun i -> option_forbidpassthrough := i::!option_forbidpassthrough),
                      "\n     forbid pass through for specified node") ;
                   (["--forbidgame"; "-fg"], String(fun s -> option_forbidgame := s::!option_forbidgame),
                      "\n     forbid game") ;
                   (["--assumegame"; "-ag"], String(fun s -> option_assumegame := Some s),
                      "\n     assume given game") ;
		   (["--incrementalgames"; "-ig"], Int(fun i -> option_incrgames := i),
                      "\n     output k solutions") ;
		   (["--incrementaliterations"; "-ii"], Unit(fun _ -> option_incrementaliterations := true),
                      "\n     enable incremental iterations") ;
                   (["--lastvalchange"; "-lv"], Unit(fun _ -> option_lastvaluchange := true),
                      "\n     enforce change of last valuation") ;
                   (["--verbose"; "-ve"], Unit(fun _ -> verbosity := 2),
                      "\n     verbose output") ;
                   (["--showiterations"; "-si"], Unit(fun _ -> option_showiterations := true),
                      "\n     show iterations") ;
		   (["--printdimacs"; "-pd"], Unit(fun _ -> option_printonly := true; verbosity := 0),
                      "\n     print dimacs only") ;
		   (["--parsesolution"; "-ps"], String(fun s -> option_parsesolution := s),
                      "\n     parse external solution") ;
		   (["--preprocess"; "-pp"], Unit(fun _ -> option_preprocess := true),
                      "\n     preprocess sat formula") ;
		   (["--improvementpolicy"; "-ip"], String(fun s -> option_improvement_policy := s),
                      "\n     default is ALLMAX; available: ALLMAX, FREE, WORSTFIRST, BESTFIRST, SINGLEWORSTFIRST, SINGLEBESTFIRST") ;
		   (["--changesat"; "-cs"], String (fun s -> Satsolvers.set_default s),
                      "\n     select sat solver; " ^ if satsolv = [] then "no sat solvers included!" else (
			      "default is " ^ ((Satsolvers.get_default ())#identifier) ^
	              "\n     available: " ^ ListUtils.format (fun f -> f#identifier) (Satsolvers.get_list ())))]

  let header = Info.get_title ("Strategy Improvement Iteration Satisfier" ^ "\n\nNOTE: This experimental tool is no longer maintained.\n")

  let usage = (header ^ "Usage: itersat -n [nodes] -i [iterations]\n" ^
                        "Searches for a parity game with [nodes] nodes enforcing at least [iterations] iterations of the strategy improvement algorithm.\n\nOptions are")
end ;;

open CommandLine ;;

let _ =
  SimpleArgs.parsedef speclist (fun _ -> ()) usage;
  if (!nodes = 0) || (!iterations = 0) then (
  	SimpleArgs.argprint_help usage speclist;
  	exit 1
  );
  if (!option_incrementaliterations) && (!option_incrgames > 1) then (
	message 0 (fun _ -> "Incremental iterations and incremental games cannot be specified at the same time!\n");
  	SimpleArgs.argprint_help usage speclist;
  	exit 1
  );;



(*****************************************************************************
 * Propositions                                                              *
 *****************************************************************************)

type vars =
	Player of int
|	Edge of (int * int)
|	Parity of int
|	Reward of (int * int)
|	Strategy of (int * int * int)
|	CounterStrategy of (int * int * int)
|	StrategyChange of (int * int * int)
|	SubEdge of (int * int * int)
|	SubSubEdge of (int * int * int)
|	ReachLower of (int * int * int * int)
|	WorstCycle of (int * int * int)
|	CycleParity of (int * int)
|	WorstPath of (int * int * int * int)
|	WorstPathSet of (int * int * int)
|	WorstPathLen of (int * int * int)
|	SCCReach of (int * int * int)
|	LowerCycle of (int * int * int)
|	EqualCycle of (int * int * int)
|	CompPathSet of (int * int * int * int)
|	LowerPathSet of (int * int * int)
|	EqualPathSet of (int * int * int)
|	LowerPathLen of (int * int * int)
|	EqualPathLen of (int * int * int)
|	LowerEqualValuation of (int * int * int)
|	NoSinglePlayerStruct of (int * int * int)
|	PassthroughNodeStruct of int
|	DirectAttractor of (int * int * int)
|	PassthroughNode of (int * int * int)
|	PassthroughNodeHelper of (int * int * int * int)
|	ImprovableRelation of (int * int * int * int)
|	Improvable of int * int
|	ImprovementSet of int * int
|	ImprovementSetMatch of int * int



(*****************************************************************************
 * Solver Initialization                                                     *
 *****************************************************************************)

let factory = if !option_printonly then get_pseudo_factory
              else if not (!option_parsesolution = "") then new Externalsat.externalSolverFactory !option_parsesolution
              else Satsolvers.get_default ()
let factory = if !option_preprocess then new Preprocessor.preprocessorSolverFactory factory else factory
let solver = new Satwrapper.satWrapper (factory) None


(*****************************************************************************
 * Clauses                                                                   *
 *****************************************************************************)

let cons_every_node_has_an_edge n =
	for i = 0 to n - 1 do
		solver#add_clause_array (Array.init n (fun j -> Po (Edge (i, j))))
	done

let no_self_cycles n =
	for i = 0 to n - 1 do
		solver#add_clause_array [|Ne (Edge(i, i))|]
	done

let rewards n =
	for i = 0 to n - 1 do
		for j = 0 to n - 1 do
			if i < j then (
				solver#add_clause_array [|Ne (Reward (i, j)); Ne (Parity (j))|];
				solver#add_clause_array [|Po (Reward (i, j)); Po (Parity (j))|]
			) else if i > j then (
				solver#add_clause_array [|Ne (Reward (i, j)); Po (Parity (i))|];
				solver#add_clause_array [|Po (Reward (i, j)); Ne (Parity (i))|]
			) else solver#add_clause_array [|Ne (Reward (i, j))|]
		done
	done

let no_single_player n =
	for v = 0 to n - 1 do
		for u = 0 to n - 1 do
			for w = 0 to n - 1 do
				if w <= u
				then solver#
add_clause_array [|Ne (NoSinglePlayerStruct (v, u, w))|]
				else solver#add_helper_conjunction (Po (NoSinglePlayerStruct (v, u, w))) [|Po (Player v); Po (Edge (v, u)); Po (Edge (v, w))|]
			done
		done
	done;
	solver#add_clause_array (Array.init (n * n * n) (fun i -> Po (NoSinglePlayerStruct ((i mod n), ((i / n) mod n), ((i / n) / n)))))

let scc_reach n =
    for i = 0 to n - 1 do
        for j = 0 to n - 1 do
            solver#add_clause_array [|Ne (SCCReach (i, j, -1)); Po (Edge (i, j))|];
            solver#add_clause_array [|Po (SCCReach (i, j, -1)); Ne (Edge (i, j))|];
            for s = 0 to n - 1 do
                solver#add_clause_array [|Ne (SCCReach (i, j, s)); Po (SCCReach (i, j, s - 1)); Po (SCCReach (i, s, s - 1))|];
                solver#add_clause_array [|Ne (SCCReach (i, j, s)); Po (SCCReach (i, j, s - 1)); Po (SCCReach (s, j, s - 1))|];
                solver#add_clause_array [|Po (SCCReach (i, j, s)); Ne (SCCReach (i, j, s - 1))|];
                solver#add_clause_array [|Po (SCCReach (i, j, s)); Ne (SCCReach (i, s, s - 1)); Ne (SCCReach (s, j, s - 1))|];
            done
        done
    done;
    for i = 0 to n - 1 do
    	solver#add_clause_array [|Po (SCCReach (i, (i + 1) mod n, n - 1))|]
    done

let alternating n =
	for i = 0 to n - 1 do
		for j = 0 to n - 1 do
			solver#add_clause_array [|Ne (Edge (i, j)); Po (Player (i)); Po (Player (j))|];
			solver#add_clause_array [|Ne (Edge (i, j)); Ne (Player (j)); Ne (Player (i))|];
		done
	done

let alternating_parities n =
	for i = 0 to n - 2 do
		solver#add_clause_array [|Ne (Parity i); Ne (Parity (i + 1))|];
		solver#add_clause_array [|Po (Parity i); Po (Parity (i + 1))|]
	done

let playeronenodebound po n =
	solver#add_helper_atleastcount po 0 (n - 1) [||] (fun i -> Po (Player i))

let pass_through_nodes l n =
	for i = 0 to n - 1 do
		solver#add_helper_atmostone 0 (n - 1) [|Ne (PassthroughNodeStruct i)|] (fun j -> Po (Edge (i, j)))
	done;
	solver#add_helper_atleastcount l 0 (n - 1) [||] (fun i -> Po (PassthroughNodeStruct i))

let outdegreebound b n =
	for i = 0 to n - 1 do
		solver#add_helper_atmostcount b 0 (n - 1) [||] (fun j -> Po (Edge (i, j)))
	done

let indegreebound b n =
	for i = 0 to n - 1 do
		solver#add_helper_atmostcount b 0 (n - 1) [||] (fun j -> Po (Edge (j, i)))
	done

let enforce_edge_count l n =
	List.iter (fun (v, c) -> solver#add_helper_atmostcount c 0 (n - 1) [||] (fun j -> Po (Edge (v, j)))) l

let enforce_edges l n =
	List.iter (fun (i, j) -> solver#add_clause_array [|Po(Edge(i,j))|]) l

let set_parity l n =
	List.iter (fun i ->
		solver#add_clause_array [|if i >= 0 then Ne (Parity i) else Po (Parity (-i))|]
	) l

let set_player l n =
	List.iter (fun i ->
		solver#add_clause_array [|if i >= 0 then Po (Player i) else Ne (Player (-i))|]
	) l


let game_to_conjunction game =
 	let n = pg_size game in
	let pr = Array.init n (fun i ->
			if (pg_get_priority game i) mod 2 = 0
			then Ne (Parity i)
			else Po (Parity i)
		) in
	let pl = Array.init n (fun i ->
			if pg_get_owner game i = plr_Even
			then Ne (Player i)
			else Po (Player i)
		) in
	let edges = Array.init n (fun i ->
			Array.init n (fun j ->
				Ne (Edge (i, j))
			)
		) in
	for i = 0 to n - 1 do
		ns_iter (fun j ->
			edges.(i).(j) <- Po (Edge (i, j))
		) (pg_get_successors game i)
	done;
	Array.init (2 * n + n * n) (fun i ->
		if i < n then pr.(i)
		else if i < 2 * n then pl.(i - n)
		else edges.(i / n - 2).(i mod n)
	)

let assume_game game =
	Array.iter (fun l ->
		solver#add_clause_array [|l|]
	) (game_to_conjunction game)

let negate_lit = function Po v -> Ne v | Ne v -> Po v

let forbid_game game =
	solver#add_clause_array (Array.map negate_lit (game_to_conjunction game))

let strategy_to_conjunction strat l =
	Array.init (Array.length strat) (fun i ->
		if strat.(i) != -1
		then Po (Strategy (l, i, strat.(i)))
		else Ne (Strategy (l, i, i))
	)

let forbid_game_with_strategy game strat =
	solver#add_clause_array (Array.append (Array.map negate_lit (game_to_conjunction game)) (Array.map negate_lit (strategy_to_conjunction strat 0)))

let load_game_by_file f =
  let in_channel = open_in f in
  Parsers.parse_parity_game in_channel

let assume_game_list l n =
	List.iter assume_game (List.map load_game_by_file l)

let forbid_game_list l n =
	List.iter forbid_game (List.map load_game_by_file l)

let forbid_passthrough l n =
	List.iter (fun i ->
		solver#add_helper_atleastcount 2 0 (n - 1) [||] (fun j -> Po (Edge (i, j)))
	) l


let valid_strategies n r k =
	for l = r to k - 1 do
		for i = 0 to n - 1 do
			(* Init Exactly Structure *)
			solver#add_helper_exactlyone 0 (n - 1) [|Po (Player i)|] (fun j -> Po (Strategy (l, i, j)));
			(* No Player 1 Strategy *)
			for j = 0 to n - 1 do
				solver#add_clause_array [|Ne (Player i); Ne (Strategy (l, i, j))|];
			done;
		done
	done

let valid_counterstrategies n r k =
	for l = r to k - 1 do
		for i = 0 to n - 1 do
			(* Init Exactly Structure *)
			solver#add_helper_exactlyone 0 (n - 1) [|Ne (Player i)|] (fun j -> Po (CounterStrategy (l, i, j)));
			(* No Player 0 Strategy *)
			for j = 0 to n - 1 do
				solver#add_clause_array [|Po (Player i); Ne (CounterStrategy (l, i, j))|];
			done;
			for j = 0 to n - 1 do
				solver#add_clause_array [|Ne (CounterStrategy (l, i, j)); Po (Edge (i, j))|];
			done;
		done
	done

let cyclecomp_counterstrategies n r k =
	for l = r to k - 1 do
		for u = 0 to n - 1 do
			for v = 0 to n - 1 do
				for w = 0 to n - 1 do
					solver#add_clause_array [|Ne (CounterStrategy (l, u, v)); Ne (WorstCycle (l, u, w)); Po (WorstCycle (l, v, w))|];
				done;
			done;
		done
	done

let strategy_progress n r k =
	for l = max 0 (r - 1) to k - 2 do
		for i = 0 to n - 1 do
			for j = 0 to n - 1 do
(*
				solver#add_clause_array [|Ne (StrategyChange (l, i, j)); Ne (Strategy (l, i, j)); Ne (Strategy (l + 1, i, j))|];
				solver#add_clause_array [|Ne (StrategyChange (l, i, j)); Po (Strategy (l, i, j)); Po (Strategy (l + 1, i, j))|];
				*)
				solver#add_clause_array [|Ne (StrategyChange (l, i, j)); Ne (Strategy (l, i, j))|];
				solver#add_clause_array [|Ne (StrategyChange (l, i, j)); Po (Strategy (l + 1, i, j))|];

			done;
		done;
		solver#add_clause_array (Array.init (n * n) (fun a -> Po (StrategyChange (l, (a / n), (a mod n)))));
	done

let sub_edges n r k =
	for l = r to k - 1 do
		for i = 0 to n - 1 do
			for j = 0 to n - 1 do
				solver#add_clause_array [|Ne (SubEdge (l, i, j)); Po (Edge (i, j))|];
				solver#add_clause_array [|Ne (SubEdge (l, i, j)); Po (Player i); Po (Strategy (l, i, j))|];
				solver#add_clause_array [|Ne (Strategy (l, i, j)); Po (SubEdge (l, i, j))|];
				solver#add_clause_array [|Ne (Player i); Ne (Edge (i, j)); Po (SubEdge (l, i, j))|];
			done
		done
	done

let subsub_edges n r k =
	for l = r to k - 1 do
		for i = 0 to n - 1 do
			for j = 0 to n - 1 do
				solver#add_clause_array [|Ne (SubSubEdge (l, i, j)); Po (Strategy (l, i, j)); Po (CounterStrategy (l, i, j))|];
				solver#add_clause_array [|Ne (SubSubEdge (l, i, j)); Ne (WorstCycle (l, i, i))|];
				solver#add_clause_array [|Ne (Strategy (l, i, j)); Po (SubSubEdge (l, i, j)); Po (WorstCycle (l, i, i))|];
				solver#add_clause_array [|Ne (CounterStrategy (l, i, j)); Po (SubSubEdge (l, i, j)); Po (WorstCycle (l, i, i))|];
			done
		done
	done

let reach_lower n r k =
	for l = r to k - 1 do
		for i = 0 to n - 1 do
			for j = 0 to n - 1 do
				solver#add_clause_array [|Ne (ReachLower (l, i, j, -1)); Po (SubEdge (l, i, j))|];
				solver#add_clause_array [|Po (ReachLower (l, i, j, -1)); Ne (SubEdge (l, i, j))|];
				for s = 0 to n - 1 do
					solver#add_clause_array [|Ne (ReachLower (l, i, j, s)); Po (ReachLower (l, i, j, s - 1)); Po (ReachLower (l, i, s, s - 1))|];
					solver#add_clause_array [|Ne (ReachLower (l, i, j, s)); Po (ReachLower (l, i, j, s - 1)); Po (ReachLower (l, s, j, s - 1))|];
					solver#add_clause_array [|Po (ReachLower (l, i, j, s)); Ne (ReachLower (l, i, j, s - 1))|];
					solver#add_clause_array [|Po (ReachLower (l, i, j, s)); Ne (ReachLower (l, i, s, s - 1)); Ne (ReachLower (l, s, j, s - 1))|];
				done
			done
		done
	done

let worst_cycles n r k =
	for l = r to k - 1 do
		for i = 0 to n - 1 do
			for j = 0 to n - 1 do
				solver#add_clause_array [|Ne (WorstCycle (l, j, i)); Po (ReachLower(l, j, i, n - 1))|];
				solver#add_clause_array [|Ne (WorstCycle (l, j, i)); Po (ReachLower(l, i, i, i))|];
				solver#add_clause_array (Array.init n (fun a -> Po (WorstCycle (l, j, a))));
				for a = 0 to n - 1 do
					solver#add_clause_array [|Ne (WorstCycle (l, j, i)); Ne (Reward (a, i)); Ne (ReachLower (l, j, a, n - 1)); Ne (ReachLower (l, a, a, a))|]
				done
			done
		done
	done

let cycle_parities n r k =
	for l = r to k - 1 do
		for v = 0 to n - 1 do
			for w = 0 to n - 1 do
				solver#add_clause_array [|Ne (WorstCycle (l, v, w)); Ne (Parity w); Po (CycleParity (l, v))|];
				solver#add_clause_array [|Ne (WorstCycle (l, v, w)); Po (Parity w); Ne (CycleParity (l, v))|]
			done
		done
	done

let worst_paths2 n r k =
	for l = r to k - 1 do
		for v = 0 to n - 1 do
			solver#add_clause_array [|Po (WorstPath (l, v, v, 0))|];
			for w = 0 to n - 1 do
				if v != w then solver#add_clause_array [|Ne (WorstPath (l, v, w, 0))|];
				for i = 0 to n - 2 do
					for u = 0 to n - 1 do
						solver#add_clause_array [|Ne (SubSubEdge (l, u, w)); Ne (WorstPath (l, v, u, i)); Po (WorstPath (l, v, w, i + 1))|];
						solver#add_clause_array [|Po (SubSubEdge (l, u, w)); Ne (WorstPath (l, v, u, i)); Ne (WorstPath (l, v, w, i + 1))|]
					done;
					solver#add_clause_array (Array.append [|Ne (WorstPath (l, v, w, i + 1))|] (Array.init n (fun u -> Po (WorstPath (l, v, u, i)))))
				done;
			done
		done
	done

let worst_paths n r k =
	for l = r to k - 1 do
		for v = 0 to n - 1 do
			solver#add_clause_array [|Po (WorstPath (l, v, v, 0))|];
			for i = 0 to n - 1 do
				 solver#add_helper_atmostone 0 (n - 1) [||] (fun w -> Po (WorstPath(l, v, w, i)))
			done;
			for w = 0 to n - 1 do
				solver#add_clause_array (Array.append (Array.init n (fun i -> Po (WorstPath (l, v, w, i)))) [|Ne (WorstCycle (l, v, w))|]);
				for i = 1 to n - 1 do
					for j = 0 to i - 1 do
						solver#add_clause_array [|Ne (WorstPath (l, v, w, i)); Ne (WorstPath (l, v, w, j))|]
					done;
					solver#add_clause_array (Array.append (Array.init n (fun j -> Po (WorstPath (l, v, j, i - 1)))) [|Ne (WorstPath (l, v, w, i))|]);
					for m = 0 to n - 1 do
						solver#add_clause_array [|Ne (WorstPath (l, v, w, i)); Ne (WorstPath (l, v, m, i - 1)); Po (SubEdge (l, m, w))|];
						solver#add_clause_array [|Ne (WorstPath (l, v, w, i)); Ne (WorstPath (l, v, m, i - 1)); Ne (WorstCycle (l, v, m))|]
					done
				done
			done
		done
	done

let worst_path_set n r k =
	for l = r to k - 1 do
		for v = 0 to n - 1 do
			for w = 0 to n - 1 do
				solver#add_clause_array (Array.append (Array.init n (fun i -> Po (WorstPath (l, v, w, i)))) [|Ne (WorstPathSet (l, v, w))|]);
				for i = 0 to n - 1 do
					for u = 0 to w - 1 do
						solver#add_clause_array [|Ne (WorstPath (l, v, w, i)); Ne (WorstCycle (l, v, u)); Po (WorstPathSet (l, v, w))|]
					done
				done;
				for u = 0 to w do
					solver#add_clause_array [|Ne (WorstCycle (l, v, w)); Ne (WorstPathSet (l, v, u))|]
				done
			done
		done
	done

let worst_path_len n r k =
	for l = r to k - 1 do
		for v = 0 to n - 1 do
			for i = 0 to n - 1 do
				solver#add_helper_disjunction (Po (WorstPathLen (l, v, i))) (Array.init n (fun w -> (Po (WorstPath (l, v, w, i)))))
			done
		done
	done

let build_valuation n l a b =
	if not (solver#mem_variable (LowerEqualValuation (l, a, b))) then (

		(* Cycle Comparison *)
	(*
		solver#add_clause_array [|Ne (LowerCycle (l, a, b)); Ne (EqualCycle (l, a, b))|];
		for w = 0 to n - 1 do
			solver#add_clause_array [|Ne (WorstCycle (l, a, w)); Ne (WorstCycle (l, b, w)); Po (EqualCycle (l, a, b))|];
			solver#add_clause_array [|Ne (WorstCycle (l, a, w)); Po (WorstCycle (l, b, w)); Ne (EqualCycle (l, a, b))|];
			for u = 0 to n - 1 do
				if u != w then (
					solver#add_clause_array [|Ne (WorstCycle (l, a, w)); Ne (WorstCycle (l, b, u)); Ne (Reward (w, u)); Po (LowerCycle (l, a, b))|];
					solver#add_clause_array [|Ne (WorstCycle (l, a, w)); Ne (WorstCycle (l, b, u)); Po (Reward (w, u)); Ne (LowerCycle (l, a, b))|]
				)
			done
		done;
*)
		for w = 0 to n - 1 do
			for u = 0 to n - 1 do
				if u = w then (
					solver#add_clause_array [|Ne (WorstCycle (l, a, w)); Ne (WorstCycle (l, b, u)); Po (EqualCycle (l, a, b))|];
					solver#add_clause_array [|Ne (WorstCycle (l, a, w)); Ne (WorstCycle (l, b, u)); Ne (LowerCycle (l, a, b))|]
				)
				else (
					solver#add_clause_array [|Ne (WorstCycle (l, a, w)); Ne (WorstCycle (l, b, u)); Ne (EqualCycle (l, a, b))|];
					solver#add_clause_array [|Ne (WorstCycle (l, a, w)); Ne (WorstCycle (l, b, u)); Ne (Reward (w, u)); Po (LowerCycle (l, a, b))|];
					solver#add_clause_array [|Ne (WorstCycle (l, a, w)); Ne (WorstCycle (l, b, u)); Po (Reward (w, u)); Ne (LowerCycle (l, a, b))|]
				)
			done
		done;

		(* CompPathSet Comparison *)
		solver#add_clause_array [|Po (CompPathSet (l, a, b, n))|];
		for i = 0 to n - 1 do
			solver#add_clause_array [|Ne (CompPathSet (l, a, b, i)); Po (CompPathSet (l, a, b, i + 1))|];
			solver#add_clause_array [|Ne (CompPathSet (l, a, b, i)); Ne (WorstPathSet (l, a, i)); Po (WorstPathSet (l, b, i))|];
			solver#add_clause_array [|Ne (CompPathSet (l, a, b, i)); Ne (WorstPathSet (l, b, i)); Po (WorstPathSet (l, a, i))|];
			solver#add_clause_array [|Po (CompPathSet (l, a, b, i)); Ne (WorstPathSet (l, a, i)); Ne (WorstPathSet (l, b, i)); Ne (CompPathSet (l, a, b, i + 1))|];
			solver#add_clause_array [|Po (CompPathSet (l, a, b, i)); Po (WorstPathSet (l, a, i)); Po (WorstPathSet (l, b, i)); Ne (CompPathSet (l, a, b, i + 1))|];
		done;

		(* PathSet Equal Comparison *)
		solver#add_clause_array [|Ne (EqualPathSet (l, a, b)); Po (CompPathSet (l, a, b, 0))|];
		solver#add_clause_array [|Po (EqualPathSet (l, a, b)); Ne (CompPathSet (l, a, b, 0))|];

		(* PathSet Lower Comparison *)
		solver#add_clause_array [|Ne (EqualPathSet (l, a, b)); Ne (LowerPathSet (l, a, b))|];
		for u = 0 to n - 1 do
			solver#add_clause_array [|Po (CompPathSet (l, a, b, u)); Ne (CompPathSet (l, a, b, u + 1)); Ne (WorstPathSet (l, b, u)); Po (Parity u); Po (LowerPathSet (l, a, b))|];
			solver#add_clause_array [|Po (CompPathSet (l, a, b, u)); Ne (CompPathSet (l, a, b, u + 1)); Po (WorstPathSet (l, b, u)); Po (Parity u); Ne (LowerPathSet (l, a, b))|];
			solver#add_clause_array [|Po (CompPathSet (l, a, b, u)); Ne (CompPathSet (l, a, b, u + 1)); Ne (WorstPathSet (l, b, u)); Ne (Parity u); Ne (LowerPathSet (l, a, b))|];
			solver#add_clause_array [|Po (CompPathSet (l, a, b, u)); Ne (CompPathSet (l, a, b, u + 1)); Po (WorstPathSet (l, b, u)); Ne (Parity u); Po (LowerPathSet (l, a, b))|]
		done;

		(* PathLen Equal Comparison *)
		for i = 0 to n - 1 do
			solver#add_clause_array [|Ne (WorstPathLen (l, a, i)); Po (WorstPathLen (l, b, i)); Ne (EqualPathLen (l, a, b))|];
			solver#add_clause_array [|Po (WorstPathLen (l, a, i)); Ne (WorstPathLen (l, b, i)); Ne (EqualPathLen (l, a, b))|];
		done;
		(*
		solver#add_clause_array [|Po (WorstPathLen (l, a, 0)); Po (WorstPathLen (l, b, 0)); Po (EqualPathLen (l, a, b))|];
		*)
		solver#add_clause_array [|Ne (WorstPathLen (l, a, n - 1)); Ne (WorstPathLen (l, b, n - 1)); Po (EqualPathLen (l, a, b))|];
		for i = 0 to n - 2 do
			solver#add_clause_array [|Ne (WorstPathLen (l, a, i)); Ne (WorstPathLen (l, b, i)); Po (WorstPathLen (l, a, i + 1)); Po (WorstPathLen (l, b, i + 1)); Po (EqualPathLen (l, a, b))|];
		done;

		(* PathLen Lower Comparison *)
		for i = 0 to n - 1 do
			solver#add_clause_array [|Po (WorstPathLen (l, a, i)); Ne (WorstPathLen (l, b, i)); Ne (CycleParity (l, a)); Po (LowerPathLen (l, a, b))|];
			solver#add_clause_array [|Po (WorstPathLen (l, a, i)); Ne (WorstPathLen (l, b, i)); Po (CycleParity (l, a)); Ne (LowerPathLen (l, a, b))|];
			solver#add_clause_array [|Ne (WorstPathLen (l, a, i)); Po (WorstPathLen (l, b, i)); Ne (CycleParity (l, a)); Ne (LowerPathLen (l, a, b))|];
			solver#add_clause_array [|Ne (WorstPathLen (l, a, i)); Po (WorstPathLen (l, b, i)); Po (CycleParity (l, a)); Po (LowerPathLen (l, a, b))|]
		done;
		solver#add_clause_array [|Ne (EqualPathLen (l, a, b)); Ne (LowerPathLen (l, a, b))|];

		(* Lexicographic Comparison *)
		solver#add_clause_array [|Ne (LowerCycle (l, a, b)); Po (LowerEqualValuation (l, a, b))|];
		solver#add_clause_array [|Ne (EqualCycle (l, a, b)); Ne (LowerPathSet (l, a, b)); Po (LowerEqualValuation (l, a, b))|];
		solver#add_clause_array [|Ne (EqualCycle (l, a, b)); Ne (EqualPathSet (l, a, b)); Ne (LowerPathLen (l, a, b)); Po (LowerEqualValuation (l, a, b))|];
		solver#add_clause_array [|Ne (EqualCycle (l, a, b)); Ne (EqualPathSet (l, a, b)); Ne (EqualPathLen (l, a, b)); Po (LowerEqualValuation (l, a, b))|];
		solver#add_clause_array [|Ne (LowerEqualValuation (l, a, b)); Po (LowerCycle (l, a, b)); Po (EqualCycle (l, a, b))|];
		solver#add_clause_array [|Ne (LowerEqualValuation (l, a, b)); Ne (EqualCycle (l, a, b)); Po (LowerPathSet (l, a, b)); Po (EqualPathSet (l, a, b))|];
		solver#add_clause_array [|Ne (LowerEqualValuation (l, a, b)); Ne (EqualCycle (l, a, b)); Ne (EqualPathSet (l, a, b)); Po (LowerPathLen (l, a, b)); Po (EqualPathLen (l, a, b))|];

	)

let valuation_valid n r k =
	for l = r to k - 1 do
		for v = 0 to n - 1 do
			for w = 0 to n - 1 do
				for u = 0 to n - 1 do
					if w != u then (
						build_valuation n l w u;
						solver#add_clause_array [|Ne (Player v); Ne (WorstPath (l, v, w, 1)); Ne (Edge (v, u)); Po (LowerEqualValuation (l, w, u))|]
					)
				done
			done
		done
	done

let valuation_progress n r k =
	for l = r to k - 1 do
		for v = 0 to n - 1 do
			for w = 0 to n - 1 do
				for u = 0 to n - 1 do
					for i = 2 to n - 1 do
						solver#add_clause_array [|Ne (WorstPath (l, v, w, 1)); Ne (WorstPath (l, v, u, i)); Po (WorstPath (l, w, u, i - 1))|]
					done
				done
			done
		done
	done

let improvement_policy_all_max n r k =
	for l = (max r 1) to k - 1 do
		for v = 0 to n - 1 do
			for w = 0 to n - 1 do
				for u = 0 to n - 1 do
					if w != u then (
						build_valuation n (l - 1) u w;
						solver#add_clause_array [|Po (Player v); Ne (Strategy (l, v, w)); Ne (Edge (v, u)); Po (LowerEqualValuation (l - 1, u, w))|]
					)
				done
			done
		done
	done

let improvable_helper_required n r k =
	for l = (max r 1) to k - 1 do
		if not (solver#mem_variable (Improvable (l, 0))) then (
			for v = 0 to n - 1 do
				solver#add_helper_disjunction (Po (Improvable (l - 1, v))) (Array.init (n * n) (fun i -> Po (ImprovableRelation (l - 1, v, i / n, i mod n))));
				for u = 0 to n - 1 do
					for w = 0 to n - 1 do
						if u = w then solver#add_clause_array [|Ne (ImprovableRelation (l - 1, v, u, w))|]
						else solver#add_helper_conjunction (Po (ImprovableRelation (l - 1, v, u, w))) [|Po (Edge (v, u)); Po (Strategy (l - 1, v, w)); Ne (LowerEqualValuation (l - 1, u, w))|]
					done
				done
			done
		)
	done

let improvement_set_only n r k =
	for l = (max r 1) to k - 1 do
		solver#add_clause_array (Array.init n (fun v -> Po (ImprovementSetMatch (l - 1, v))));
		for v = 0 to n - 1 do
			solver#add_helper_conjunction (Po (ImprovementSetMatch (l - 1, v))) [|Po (Improvable (l - 1, v)); Po (ImprovementSet (l - 1, v))|];
			for w = 0 to n - 1 do
				for u = 0 to n - 1 do
					if w != u then (
						build_valuation n (l - 1) u w;
						solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Po (Player v); Ne (Strategy (l, v, w)); Ne (Edge (v, u)); Po (LowerEqualValuation (l - 1, u, w))|]
					)
					else solver#add_clause_array [|Po (ImprovementSet (l - 1, v)); Po (Player v); Po (Strategy (l, v, w)); Ne (Strategy (l - 1, v, u))|]
				done
			done
		done
	done

let improvement_policy_worst_first n r k =
	improvable_helper_required n r k;
	improvement_set_only n r k;
	for l = (max r 1) to k - 1 do
		for v = 0 to n - 1 do
			for w = 0 to n - 1 do
				solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Ne (ImprovementSet (l - 1, w)); Po (EqualCycle (l - 1, v, w))|];
				solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Ne (ImprovementSet (l - 1, w)); Po (EqualPathSet (l - 1, v, w))|];
				solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Po (ImprovementSet (l - 1, w)); Ne (EqualCycle (l - 1, v, w)); Ne (EqualPathSet (l - 1, v, w))|];
				solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Po (ImprovementSet (l - 1, w)); Ne (LowerCycle (l - 1, w, v)); Ne (Improvable (l - 1, w))|];
				solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Po (ImprovementSet (l - 1, w)); Ne (EqualCycle (l - 1, w, v)); Ne (LowerPathSet (l - 1, w, v)); Ne (Improvable (l - 1, w))|]
			done
		done
	done

let improvement_policy_best_first n r k =
	improvable_helper_required n r k;
	improvement_set_only n r k;
	for l = (max r 1) to k - 1 do
		for v = 0 to n - 1 do
			for w = 0 to n - 1 do
				solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Ne (ImprovementSet (l - 1, w)); Po (EqualCycle (l - 1, v, w))|];
				solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Ne (ImprovementSet (l - 1, w)); Po (EqualPathSet (l - 1, v, w))|];
				solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Po (ImprovementSet (l - 1, w)); Ne (EqualCycle (l - 1, v, w)); Ne (EqualPathSet (l - 1, v, w))|];
				solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Po (ImprovementSet (l - 1, w)); Ne (LowerCycle (l - 1, v, w)); Ne (Improvable (l - 1, w))|];
				solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Po (ImprovementSet (l - 1, w)); Ne (EqualCycle (l - 1, w, v)); Ne (LowerPathSet (l - 1, v, w)); Ne (Improvable (l - 1, w))|]
			done
		done
	done

let improvement_policy_single_worst_first n r k =
	improvable_helper_required n r k;
	improvement_set_only n r k;
	for l = (max r 1) to k - 1 do
		for v = 0 to n - 1 do
			for w = 0 to n - 1 do
				if w != v then solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Ne (ImprovementSet (l - 1, w))|];
				solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Po (ImprovementSet (l - 1, w)); Po (LowerEqualValuation (l - 1, v, w)); Ne (Improvable (l - 1, w))|];
			done
		done
	done

let improvement_policy_single_best_first n r k =
	improvable_helper_required n r k;
	improvement_set_only n r k;
	for l = (max r 1) to k - 1 do
		for v = 0 to n - 1 do
			for w = 0 to n - 1 do
				if w != v then solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Ne (ImprovementSet (l - 1, w))|];
				solver#add_clause_array [|Ne (ImprovementSet (l - 1, v)); Po (ImprovementSet (l - 1, w)); Po (LowerEqualValuation (l - 1, w, v)); Ne (Improvable (l - 1, w))|];
			done
		done
	done

let strategy_really_improved n r k =
	for l = (max r 1) to k - 1 do
		for v = 0 to n - 1 do
			for w = 0 to n - 1 do
				for u = 0 to n - 1 do
					if u != w then (
						build_valuation n (l - 1) u w;
						solver#add_clause_array [|Ne (Strategy (l - 1, v, w)); Ne (Strategy (l, v, u)); Ne (LowerEqualValuation (l - 1, u, w))|]
					)
				done
			done
		done
	done

let cycle_invariant n r k =
	for l = (max (r - 1) 0) to k - 2 do
		for i = 0 to n - 1 do
			for j = 0 to n - 1 do
				solver#add_helper_equivalent (Po (WorstCycle (l, i, j))) (Po (WorstCycle (l + 1, i, j)))
			done
		done
	done

let centered n r k =
	for l = r to k - 1 do
		for i = 0 to n - 1 do
			for j = 0 to n - 1 do
				solver#add_helper_equivalent (Po (WorstCycle (l, i, j))) (Po (WorstCycle (l, (i + 1) mod n, j)))
			done
		done
	done

let cycle_node q n r k =
	for l = r to k - 1 do
		for i = 0 to n - 1 do
			solver#add_clause_array [|Po (WorstCycle (l, i, q))|]
		done
	done;
	if not (!option_alternatingprios) then (
		for i = 0 to q - 1 do
			solver#add_clause_array [|Po (Parity i)|]
		done
	)

let forbid_cycle_node l n r k =
	List.iter (fun q ->
		for l = r to k - 1 do
			for i = 0 to n - 1 do
				solver#add_clause_array [|Ne (WorstCycle (l, i, q))|]
			done
		done
	) l

let cycle_parity p n r k =
	for l = r to k - 1 do
		for i = 0 to n - 1 do
			for j = 0 to n - 1 do
				solver#add_clause_array [|Ne (WorstCycle (l, i, j)); (if p = 1 then Po (Parity j) else Ne (Parity j))|]
			done
		done
	done

let last_valu_change n _ k =
	let pairs = Array.init (n * 3 * n) (fun i ->
		let v = i / (3 * n) in
		let p = i mod (3 * n) in
		if p < n then (Po (WorstCycle (k - 2, v, p)), Po (WorstCycle (k - 1, v, p)))
		else if p < 2 * n then (Po (WorstPathSet (k - 2, v, p - n)), Po (WorstPathSet (k - 1, v, p - n)))
		else (Po (WorstPathLen (k - 2, v, p - 2 * n)), Po (WorstPathLen (k - 1, v, p - 2 * n)))
	)
	in
		solver#add_helper_not_equal_pairs pairs

let build_direct_attractors n l =
	if not (solver#mem_variable (DirectAttractor (l, 0, 0))) then (
		for v = 0 to n - 1 do
			solver#add_helper_exactlyone 0 (n - 1) [||] (fun w -> Po (DirectAttractor(l, v, w)));
			solver#add_clause_array [|Ne (WorstPathSet (l, v, v)); Po (DirectAttractor (l, v, v))|];
			solver#add_clause_array [|Ne (WorstCycle (l, v, v)); Po (DirectAttractor (l, v, v))|];
			for u = 0 to n - 1 do
				for w = 0 to n - 1 do
					solver#add_clause_array [|Po (WorstPathSet (l, v, v)); Ne (WorstPath (l, v, u, 1)); Ne (DirectAttractor (l, u, w)); Po (DirectAttractor (l, v, w))|];
				done
			done
		done
	)

let build_passthrough_nodes n l =
	if not (solver#mem_variable (PassthroughNode (l, 0, 0))) then (
		build_direct_attractors n l;
		for v = 0 to n - 1 do
			for u = 0 to n - 1 do
				if u = v then solver#add_clause_array [|Ne (PassthroughNode (l, v, u))|];
				solver#add_helper_disjunction (Po (PassthroughNode (l, v, u))) (Array.init n (fun w -> Po (PassthroughNodeHelper (l, v, u, w))));
				for w = 0 to n - 1 do
					solver#add_helper_conjunction (Po (PassthroughNodeHelper (l, v, w, u))) [|Po (Edge (v, u)); Ne (DirectAttractor (l, u, w)); Ne (Player v); Po (DirectAttractor (l, v, w))|]
				done
			done
		done
	)

let enforce_worstset l n =
	List.iter (fun (i, v, s) ->
		solver#add_clause_array [|Po (WorstPathSet (i, v, s))|]
	) l

let enforce_ptnodes_lower_size l n =
	let helper (it, no, si) =
		build_passthrough_nodes n it;
		solver#add_helper_atleastcount si 0 (n - 1) [||] (fun i -> Po (PassthroughNode (it, i, no)))
	in
		List.iter helper l

let enforce_ptnodes_upper_size l n =
	let helper (it, no, si) =
		build_passthrough_nodes n it;
		solver#add_helper_atmostcount si 0 (n - 1) [||] (fun i -> Po (PassthroughNode (it, i, no)))
	in
		List.iter helper l


let enforce_dominee_lower_size l n =
	let helper (it, no, si) =
		solver#add_helper_atleastcount si 0 (n - 1) [||] (fun i -> Po (WorstPathSet (it, i, no)))
	in
		List.iter helper l

let enforce_dominee_upper_size l n =
	let helper (it, no, si) =
		solver#add_helper_atmostcount si 0 (n - 1) [||] (fun i -> Po (WorstPathSet (it, i, no)))
	in
		List.iter helper l

let enforce_static_dominees l n r k =
	let helper (no, it) =
		for i = max r it to k - 2 do
			for j = 0 to n - 1 do
				solver#add_helper_equivalent (Po (WorstPathSet (i, j, no))) (Po (WorstPathSet (i + 1, j, no)))
			done
		done
	in
		List.iter helper l

let enforce_nonstatic_dominees l k n =
	let helper (no, it) =
		solver#add_helper_not_equal_pairs (Array.init (n * (k - it - 1)) (fun i ->
			let (v, r) = (i mod n, it + i / n) in
			(Po (WorstPathSet (r, v, no)), Po (WorstPathSet (r + 1, v, no)))
		))
	in
		List.iter helper l

let enforce_nonstatic_attractees l k n =
	let helper (no, it) =
		for i = it to k - 1 do
			build_direct_attractors n i
		done;
		solver#add_helper_not_equal_pairs (Array.init (n * (k - it - 1)) (fun i ->
			let (v, r) = (i mod n, it + i / n) in
			(Po (DirectAttractor (r, v, no)), Po (DirectAttractor (r + 1, v, no)))
		))
	in
		List.iter helper l




(*****************************************************************************
 * Extraction routines                                                       *
 *****************************************************************************)

let get_strategy n u =
	Array.init n (fun i ->
		solver#get_variable_first (Array.init n (fun a -> Strategy (u, i, a)))
	)

let get_worst_cycles n u =
	Array.init n (fun i ->
		solver#get_variable_first (Array.init n (fun a -> WorstCycle (u, i, a)))
	)

let get_valuation n u v =
	(solver#get_variable_first (Array.init n (fun a -> WorstCycle (u, v, a))),
	 TreeSet.of_list compare (List.filter (fun i -> solver#get_variable_bool (WorstPathSet(u, v, i))) (Array.to_list (Array.init n (fun i -> i)))),
	 solver#get_variable_count (Array.init n (fun i -> WorstPathLen (u, v, i))) - 1)


let get_paritygame n =
	let s = get_strategy n 0 in
	pg_init n (fun i ->
		let l = ref [] in
		for j = 0 to n - 1 do
			if (s.(i) != j) && (solver#get_variable_bool (Edge (i, j))) then l := j::!l
		done;
		((i * 2 + solver#get_variable (Parity i)),
		 (if solver#get_variable (Player i) = 0 then plr_Even else plr_Odd),
		 (if s.(i) != -1 then !l @ [s.(i)] else !l),
		 (Some (string_of_int i)))
	)



let get_improvement_policy = function
	"ALLMAX" -> improvement_policy_all_max
|	"FREE"	 -> (fun _ _ _ -> ())
|	"WORSTFIRST" -> improvement_policy_worst_first
|	"BESTFIRST" -> improvement_policy_best_first
|	"SINGLEWORSTFIRST" -> improvement_policy_single_worst_first
|	"SINGLEBESTFIRST" -> improvement_policy_single_best_first
|	_ -> failwith "Unknown improvement policy"


let _ =
  let queue_general =
  	[("Every node has an edge", cons_every_node_has_an_edge);
  	 ("No self cycles", no_self_cycles);
  	 ("Reward structure", rewards)]
	@ (if !option_scc then [("Enforce SCC", scc_reach)] else [])
	@ (if !option_nosingleplayer then [("No single player", no_single_player)] else [])
	@ (if !option_alternating then [("Alternating nodes", alternating)] else [])
	@ (if !option_alternatingprios then [("Alternating priorities", alternating_parities)] else [])
	@ (if !playeronenodes > 0 then [("Player one node bound", playeronenodebound !playeronenodes)] else [])
	@ (if !outdegree > -1 then [("Outdegree bound", outdegreebound !outdegree)] else [])
	@ (if !indegree > -1 then [("Indegree bound", indegreebound !indegree)] else [])
	@ (match !option_passthroughnodes with None -> [] | Some i -> [("Pass through nodes", pass_through_nodes i)])
	@ (if (!option_setparity != []) then [("Set parity", set_parity !option_setparity)] else [])
	@ (if (!option_enfedges != []) then [("Enforce edges", enforce_edges !option_enfedges)] else [])
	@ (if (!option_enfedgecount != []) then [("Enforce edge count", enforce_edge_count !option_enfedgecount)] else [])
	@ (if (!option_setplayer != []) then [("Set player", set_player !option_setplayer)] else [])
	@ (if (!option_forbidgame != []) then [("Forbid game", forbid_game_list !option_forbidgame)] else [])
	@ (if (!option_enf_worstset != []) then [("Enforce worst set", enforce_worstset !option_enf_worstset)] else [])
	@ (if (!option_nonstatic_dominees != []) then [("Enforce nonstatic dominee set", enforce_nonstatic_dominees !option_nonstatic_dominees !iterations)] else [])
	@ (if (!option_nonstatic_attractees != []) then [("Enforce nonstatic attractee set", enforce_nonstatic_attractees !option_nonstatic_attractees !iterations)] else [])
	@ (if (!option_enfdominee_lower != []) then [("Enforce dominee lower size", enforce_dominee_lower_size !option_enfdominee_lower)] else [])
	@ (if (!option_enfdominee_upper != []) then [("Enforce dominee upper size", enforce_dominee_upper_size !option_enfdominee_upper)] else [])
	@ (if (!option_enfptnodes_lower != []) then [("Enforce ptnodes lower size", enforce_ptnodes_lower_size !option_enfptnodes_lower)] else [])
	@ (if (!option_enfptnodes_upper != []) then [("Enforce ptnodes upper size", enforce_ptnodes_upper_size !option_enfptnodes_upper)] else [])
	@ (if (!option_forbidpassthrough != []) then [("Forbid passthrough", forbid_passthrough !option_forbidpassthrough)] else [])
	@ (match !option_assumegame with None -> [] | Some s -> [("Assume game", assume_game_list [s])])
   in

  let queue_iterations =
	[("Valid strategies", valid_strategies);
  	 ("Strategy progress", strategy_progress);
  	 ("Sub edge structure", sub_edges);
  	 ("Reach values", reach_lower);
  	 ("Worst cycles", worst_cycles);
(*
		("Valid counter strategies", valid_counterstrategies);
		("Cycle compatible counter strategy", cyclecomp_counterstrategies);
	  	("Subsub Edges", subsub_edges);
	  	("Worst paths2", worst_paths2);
*)
  	 ("Cycle parities", cycle_parities);
  	 ("Worst paths", worst_paths);
  	 ("Worst path set", worst_path_set);
  	 ("Worst path len", worst_path_len);
  	 ("Valuation valid", valuation_valid);
  	 ("Valuation progress", valuation_progress);
  	 ("Improvement policy", get_improvement_policy !option_improvement_policy);
  	 ("Strategy really improved", strategy_really_improved)]
	@ (if !option_cycleinvariant then [("Cycle invariant", cycle_invariant)] else [])
	@ (if !option_centered then [("Centered", centered)] else [])
	@ (if (!option_static_dominees != []) then [("Enforce static dominee set", enforce_static_dominees !option_static_dominees)] else [])
	@ (if (!option_cyclenode >= 0) then [("Cycle node", cycle_node !option_cyclenode)] else [])
	@ (if (!option_forbid_cyclenode != []) then [("Forbid cycle node", forbid_cycle_node !option_forbid_cyclenode)] else [])
	@ (match !option_cycleparity with None -> [] | Some i -> [("Cycle parity", cycle_parity i)])
	@ (if !option_lastvaluchange then [("Last valu change", last_valu_change)] else [])
  in

  let process_queue queue e =
	List.iter (fun (s, f) ->
		let v = solver#variable_count + solver#helper_variable_count in
		let c = solver#clause_count + solver#helper_clause_count in
		let l = solver#literal_count + solver#helper_literal_count in
		message 2 (fun _ -> s ^ "... ");
		e f;
		let v = solver#variable_count + solver#helper_variable_count - v in
		let c = solver#clause_count + solver#helper_clause_count - c in
		let l = solver#literal_count + solver#helper_literal_count - l in
		message 2 (fun _ -> string_of_int v ^ " variables, " ^ string_of_int c ^ " clauses and " ^ string_of_int l ^ " literals\n")
	) queue
  in

  message 1 (fun _ -> "Building constraints...\n");

  let n = !nodes in
  let k = ref !iterations in
  let games = ref 0 in

  process_queue queue_general (fun f -> f n);
  process_queue queue_iterations (fun f -> f n 0 !k);

  message 1 (fun _ -> "Finished building constraints!\n");
  let v = solver#variable_count + solver#helper_variable_count in
  let c = solver#clause_count + solver#helper_clause_count in
  let l = solver#literal_count + solver#helper_literal_count in
  message 1 (fun _ -> "    " ^ string_of_int v ^ " variables\n    " ^ string_of_int c ^ " clauses\n    " ^ string_of_int l ^ " literals\n");

  let globaltime = SimpleTiming.init true in

  if !option_printonly
  then (solver#get_solver)#print_dimacs stdout
  else while (!option_incrgames > !games) || (!option_incrgames = 1 && !option_incrementaliterations) do
  	incr games;
        message 1 (fun _ -> "\nSAT solving game #" ^ string_of_int !games ^ ", iterations #" ^ string_of_int !k ^ "... ");
	let timobj = SimpleTiming.init true in

	solver#solve;

	SimpleTiming.stop timobj;
	message 1 (fun _ -> "finished: " ^ (SimpleTiming.format timobj) ^ ", total: " ^(SimpleTiming.format globaltime) ^ "!\n\n");

	match solver#get_solve_result with
	SolveSatisfiable -> (
		message 1 (fun _ -> "A game satisfying the given conditions runs as follows:\n");

		let pg = get_paritygame n in
		print_game pg;
		message 1 (fun _ -> "\n");

		if !option_showiterations then (
			message 1 (fun _ -> "\n");
			for j = 0 to !k - 1 do
				message 1 (fun _ -> "\n");
				message 1 (fun _ -> format_strategy (get_strategy n j) ^ "\n");
				message 1 (fun _ -> "\n");
				if j < !k then
					for i = 0 to n - 1 do
						let (u, a, e) = get_valuation n j i in
						message 1 (fun _ -> string_of_int i ^ " --> " ^ string_of_int u ^ " " ^ TreeSet.format string_of_int a ^ " " ^ string_of_int e ^ "\n");
					done
				done;
		);
		if !option_incrgames > 1 then (
			solver#incremental_reset;
			forbid_game pg;
		);
		if !option_incrementaliterations then (
			solver#incremental_reset;
			incr k;
			process_queue queue_iterations (fun f -> f n (!k - 1) !k)
		)
	)
	| SolveUnsatisfiable -> (message 1 (fun _ -> "There is no such game.\n"); option_incrgames := 0)
	| SolveFailure s -> (message 1 (fun _ -> s ^ "\n"); option_incrgames := 0);

	message 1 (fun _ -> "\n")
  done;
  solver#dispose
