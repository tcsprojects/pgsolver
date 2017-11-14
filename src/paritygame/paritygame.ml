(** Paritygame module.
    For detailed information about each component see interface "paritygame.mli" 
*)
open Basics;;
open Tcsbasedata;;
open Tcsarray;;
open Tcsset;;
open Tcslist;;
open Tcsgraph;;
open Pgnode;;
open Pgnodeset;;
open Pgplayer;;
open Pgpriority;;
open Pgsolution;;
open Pgstrategy;;
(*open Pgprofiling*)
  


                                 
                              

(**************************************************************
 *                        NODE ORDERING                       *
 **************************************************************)
type pg_ordering      = node * priority * player * nodeset -> node * priority * player * nodeset -> int

(* type pg_ordering      = int * int * int * int array -> int * int * int * int array -> int *)

let ord_rew_for pl (_, pr, _, _) (_, pr', _, _) =
	if (pr != prio_undef) && (pr' != prio_undef)
	then compare (reward pl pr) (reward pl pr')
	else compare pr pr';;

let ord_prio (_, pr, _, _) (_, pr', _, _) = compare pr pr';;

let ord_total_by ordering (i, pri, pli, tri) (j, prj, plj, trj) =
	let o = ordering (i, pri, pli, tri) (j, prj, plj, trj) in
	if o = 0 then compare i j else o;;


(***************************************************************
 *                SOLUTION/STRATEGY FUNCTIONS                  *
 ***************************************************************)

let print_solution_strategy_parsable sol strat =
	(*let n = Array.length sol in
	print_string ("paritysol " ^ string_of_int (n-1) ^ ";\n");*)
	print_string ("paritysol;\n");
	sol#iter (fun i pl ->
        print_string (nd_show i);
        print_char ' ';
        print_string (plr_show pl);
        if strat#get i != nd_undef then (
            print_char ' ';
            print_string (nd_show (strat#get i))
        );
        print_char ';';
        print_newline ()
    );;

(**************************************************************
 *                   PARTIAL PARITYGAME                       *
 **************************************************************)
type partial_paritygame = node * (node -> node Enumerators.enumerator) * (node -> priority * player) * (node -> string option)
type partial_solution = node -> player * node option
type partial_solver = partial_paritygame -> partial_solution


(**************************************************************
 *                   DYNAMIC PARITYGAME                       *
 **************************************************************)
type dynamic_paritygame = (priority * player * string option) DynamicGraph.dynamic_graph

                                            
(***************************************************************
 *                  DECOMPOSITION FUNCTIONS                    *
 ***************************************************************)
type scc = int

let sccs_compute_leaves roots topology =
	let leafs = ref TreeSet.empty_def in
	let rec process r =
		if topology.(r) = []
		then leafs := TreeSet.add r !leafs
		else List.iter process topology.(r)
	in
	List.iter process roots;
	TreeSet.elements !leafs;;


let sccs_compute_transposed_topology topology =
	let n = Array.length topology in
	let transp = Array.make n [] in
	Array.iteri (fun r -> List.iter (fun ch -> transp.(ch) <- r::transp.(ch))) topology;
	transp;;

let show_sccs sccs topology roots =
  let s = ref "}" in
  let l = Array.length sccs in

  s := " {" ^ String.concat "," (List.map string_of_int roots) ^ !s;

  for i=1 to l-1 do
    s := "," ^ string_of_int (l-i) ^ "->{" ^ String.concat "," (List.map string_of_int (Array.get topology (l-i))) ^ "}" ^ !s
  done;
  if l > 0 then s := "0:{" ^ String.concat "," (List.map string_of_int (Array.get topology 0)) ^ "}" ^ !s;

  s := " {" ^ !s;

  for i=1 to l-1 do
    s := "," ^ string_of_int (l-i) ^ ":{" ^ String.concat "," (List.map nd_show (ns_nodes (Array.get sccs (l-i)))) ^ "}" ^ !s
  done;
  if l > 0 then s := "0:{" ^ String.concat "," (List.map nd_show (ns_nodes (Array.get sccs 0))) ^ "}" ^ !s;
  "{" ^ !s;;

                                              
(**************************************************************
 *                 (VIRTUAL) PARITYGAME                       *
 **************************************************************)
class virtual paritygame = object (self : 'self)

(******************** VIRTUAL METHODS ********************)
        
(********** GENERAL **********)
method virtual size : int
method virtual copy : 'self

method virtual iterate : (node -> (priority * player * nodeset * nodeset * string option) -> unit) -> unit

method virtual edge_iterate : (node -> node -> unit) -> unit
method virtual map : (node -> (priority * player * nodeset * nodeset * string option) ->  (priority * player * nodeset * nodeset * string option)) -> 'self
method virtual map2 : 'a. (node -> (priority * player * nodeset * nodeset * string option) -> 'a) -> 'a array

(********** GETTERS **********)
method virtual get_node : int -> (priority * player * nodeset * nodeset * string option)
method virtual get_priority : node -> priority
method virtual get_owner : node -> player
method virtual get_successors : node -> nodeset
method virtual get_predecessors : node -> nodeset
method virtual get_desc : node -> string option
method virtual get_desc' : node -> string
method virtual find_desc : string option -> node
method virtual is_defined : node -> bool
method virtual format_game : string

                               
(********** SETTERS **********)
method virtual set_node' : int ->(priority * player * nodeset * nodeset * string option) -> unit
method virtual set_node : int -> priority -> player -> nodeset -> nodeset -> string option -> unit
method virtual set_priority : node -> priority -> unit
method virtual set_owner : node -> player -> unit
method virtual set_desc : node -> string option -> unit
method virtual set_desc' : node -> string -> unit
method virtual add_edge : node -> node -> unit
method virtual del_edge : node -> node -> unit
method virtual remove_nodes : nodeset -> unit
method virtual remove_edges : (node * node) list -> unit


(********** SUBGAME **********)
method virtual subgame_by_edge_pred : (node -> node -> bool) -> 'self
method virtual subgame_by_node_pred : (node -> bool) -> 'self
method virtual subgame_by_list : nodeset -> 'self * (node -> node) * (node -> node)
method virtual subgame_by_node_filter : (node -> bool) -> 'self * (node -> node) * (node -> node)

                                                                                     
(******************** NON-VIRTUAL METHODS ********************)

(********** GENERAL **********)
method print =
	let n = self#size in
	print_string ("parity " ^ string_of_int n ^ ";\n");
	for i = 0 to n - 1 do
	  let (pr, pl, succs, _, desc) = self#get_node i in
	  if pr >= 0 && pl != plr_undef then (
            print_int i;
            print_char ' ';
            print_int pr;
            print_char ' ';
            print_string (plr_show pl);
            print_char ' ';
            print_string (String.concat "," (List.map nd_show (ns_nodes succs)));
            (
             match desc with
               None -> () (* print_string (" \"" ^ string_of_int i ^ "\"") *)
             | Some s -> if s <> "" then print_string (" \"" ^ s ^ "\"")
            );
            print_char ';';
            print_newline ()
           )
        done
          
method to_dotty (solution: solution) (strategy: strategy) h =
  let encode i = "N" ^ (nd_show i) in

  output_string h "digraph G {\n";

  self#iterate (fun i (p,pl,succs,_,ann) ->

    if p >= 0 && pl != plr_undef
    then (let name = encode i in
          let label = (match ann with None -> ""
                                    | Some s -> s ^ ": ") ^ string_of_int p
          in
          let shape = if pl=plr_Even then "diamond" else "box" in
          let color =
                        match solution#get i with
                              PlayerEven -> "green"
                            | PlayerOdd -> "red"
			                | _ -> "black"
          in
          output_string h (name ^ " [ shape=\"" ^ shape ^ "\", label=\"" ^ label ^ "\", color=\"" ^ color ^ "\" ];\n");

	  ns_iter (fun w -> let color2 = try
				             if (plr_opponent pl = solution#get i) || (w = strategy#get i) then color else "black"
				           with _ -> "black"
			      in
			      output_string h (name ^ " -> " ^ encode w ^ " [ color=\"" ^ color2 ^ "\" ];\n" )) succs
	 )
  );
  output_string h "}\n"

method to_dotty_file solution strategy filename =
  let h = open_out filename in
  self#to_dotty solution strategy h;
  close_out h

            
(********** GETTERS **********)
method node_count =
  let count = ref 0 in
  self#iterate (fun i _ ->
    if self#is_defined i then incr count
  );
  !count

method edge_count =
  let count = ref 0 in
  self#iterate (fun i _ ->
    if self#is_defined i then count := !count + (ns_size (self#get_successors i))
  );
  !count

method get_max o =
	let m = ref nd_undef in
	let vm = ref None in
	self#iterate (fun (i: node) (pri,pli,succi,_,_) ->
	  let vi = (i,pri,pli,succi) in
	  match !vm with
	    Some vm' -> if o vm' vi < 0 then (m := i; vm := Some vi)
	  | _ -> (m := i; vm := Some vi)
	);
	!m

method get_min o = self#get_max (fun x y -> - (o x y))
                                
method get_max_prio_node = self#get_max ord_prio
                                        
method get_max_rew_node_for pl = self#get_max (ord_rew_for pl)
                                              
method get_max_prio = self#get_priority (self#get_max_prio_node)
                                        
method get_min_prio = self#get_priority (self#get_min ord_prio)
                                        
method get_max_prio_for player =
    let nd = self#get_max_rew_node_for player in
    if nd != nd_undef then (
        let pr = self#get_priority nd in
        if plr_benefits pr = player then pr else -1
    ) else -1

method get_index = self#get_max_prio - self#get_min_prio + 1
                                                             
method get_prio_nodes p =
  let l = ref ns_empty in
  self#iterate (fun i (pr, _, _, _, _) ->
    if pr = p then l := ns_add i !l
  );
  !l

method get_selected_priorities pred =
  let prios = ref TreeSet.empty_def in
  self#iterate (fun v -> fun (pr,_,_,_,_) -> if pred pr then prios := TreeSet.add pr !prios);
  TreeSet.elements !prios

method get_priorities = self#get_selected_priorities (fun _ -> true)

method to_string =
	let n = self#size in
	let s = ref "" in
	self#iterate (fun i  (pr, pl, succs, _ , desc) ->
	  if pr >= 0 && pl != plr_undef then
	    begin
	      s := !s ^ nd_show i ^ " " ^ string_of_int pr ^ " " ^ plr_show pl ^ " " ^
	           (String.concat "," (List.map nd_show (ns_nodes succs)) ^
		     (match desc with
			None   -> ""
		      | Some a -> if a <> "" then " \"" ^ a ^ "\"" else "")
		   ) ^ ";\n"
           end
	);
	"parity " ^ string_of_int (n-1) ^ ";\n" ^ !s
                                                                                                     
                                                 
(********** NODE COLLECTION  **********)
method collect_nodes pred =
    let l = ref ns_empty in
    self#iterate (fun i nd ->
    	if self#is_defined i && (pred i nd) then l := ns_add i !l
    );
    !l
     
method collect_nodes_by_prio pred =
	self#collect_nodes (fun _ (pr, _, _, _, _) -> pred pr)

method collect_nodes_by_owner pred =
  let ltrue = ref ns_empty in
  let lfalse = ref ns_empty in
  self#iterate (fun i (_, pl, _, _, _) ->
    if self#is_defined i then
      begin
	if pred pl then
	  ltrue := ns_add i !ltrue
	else
	  lfalse := ns_add i !lfalse
      end
  );
  (!ltrue, !lfalse)

method collect_max_prio_nodes =
  let m = self#get_max_prio in
	self#collect_nodes_by_prio (fun pr -> pr = m)

method collect_max_parity_nodes =
	let (p0, p1) = (self#get_max_prio_for plr_Even, self#get_max_prio_for plr_Odd) in
    let p = if p0 < 0 then p1 else if p1 < 0 then p0
            else if p0 < p1 then p0 + 1 else p1 + 1 in
    self#collect_nodes_by_prio (fun pr -> pr >= p)

                               
(********** SUBGAME **********)
method subgame_by_strat (strat: strategy) = self#subgame_by_edge_pred (fun x y -> strat#get x = nd_undef || strat#get x = y)
                                                          
method subgame_by_strat_pl (strat: strategy) pl =
   self#subgame_by_edge_pred (fun i j ->
		let pl' = self#get_owner i in
		pl != pl' || strat#get i = j
     )
                             
                             
(********** DOMINION **********)
method set_closed nodeset pl =
    ns_forall (fun q ->
		     let pl' = self#get_owner q in
		     let delta = self#get_successors q in
		     if pl = pl'
		     then ns_fold (fun r i -> r || ns_elem i nodeset) false delta
		     else ns_fold (fun r i -> r && ns_elem i nodeset) true delta
    ) nodeset

method set_dominion (solver:  ('self -> solution * strategy)) nodeset pl =
	if self#set_closed nodeset pl then (
        let (sub_game, old_to_new, new_to_old) = self#subgame_by_list nodeset in
        let (sol, strat') = solver sub_game in
        if sol#for_all (fun _ pl' -> pl' = pl)
        then (
        	let strat = new array_strategy self#size in
            ns_iter (fun q ->
                let i = old_to_new q in
                if strat'#get i != nd_undef
                then strat#set q (new_to_old (strat'#get i))
            ) nodeset;
            Some strat
        )
        else None
    )
        else None

               
(********** DECOMPOSITION **********)
method strongly_connected_components (*tgraph*) =
  let l = self#size in
  let dfsnum = ref TreeMap.empty_def in
  let index = Array.make l nd_undef in

  let todo = ref [] in
  self#iterate (fun (i: node) _ ->
    if self#is_defined i then todo := i :: !todo
  );

  let n = ref 0 in
  let visited = ref TreeSet.empty_def in

  let dfs v =
  	let st = Stack.create () in
  	Stack.push v st;
  	while not (Stack.is_empty st) do
  		let u = Stack.pop st in
  		let pushed = ref false in
  		if not (TreeSet.mem u !visited) then (
  		    visited := TreeSet.add u !visited;
  			ns_iter (fun w ->
  				if not (TreeSet.mem w !visited) then (
  					if not !pushed then (
  						Stack.push u st;
  						pushed := true
  					);
  					Stack.push w st
  				)
  			) (self#get_successors u)
  		);
  		if (not !pushed) && (not (TreeMap.mem u !dfsnum)) then (
  			dfsnum := TreeMap.add u !n !dfsnum;
  			index.(!n) <- u;
  			incr n
  		)
  	done
  in

  self#iterate(fun i _ ->
    if not (TreeSet.mem i !visited) && self#is_defined i then dfs i
  );

  decr n;

  let visited = ref TreeSet.empty_def in

  let sccs = DynArray.create ns_empty in
  let topology = DynArray.create TreeSet.empty_def in
  let scc_index = ref TreeMap.empty_def in
  let next_index = ref 0 in
  let roots = ref TreeSet.empty_def in
  let is_root = ref true in

  while !n >= 0 do
    DynArray.insert topology !next_index TreeSet.empty_def;
    is_root := true;
    todo := [index.(!n)];
    let scc = ref ns_empty in

    while !todo <> [] do
      let v = List.hd !todo in
      todo := List.tl !todo;

      if not (TreeSet.mem v !visited) && TreeMap.mem v !dfsnum
      then (visited := TreeSet.add v !visited;
            scc := ns_add v !scc;
            let succs = List.sort (fun x -> fun y -> compare (TreeMap.find x !dfsnum) (TreeMap.find y !dfsnum)) (ns_nodes (self#get_predecessors v)) in
            todo := succs @ !todo;
            List.iter (fun w ->
                try
                    let c = TreeMap.find w !scc_index in
                    DynArray.set topology c (TreeSet.add !next_index (DynArray.get topology c));
                    is_root := false
                with Not_found ->()
            ) succs
       )
    done;
    DynArray.insert sccs !next_index !scc;
    if !is_root then roots := TreeSet.add !next_index !roots;
    ns_iter (fun v ->
        scc_index := TreeMap.add v !next_index !scc_index
    ) !scc;
    incr next_index;

    while !n >= 0 && (TreeSet.mem index.(!n) !visited) do
      decr n
    done
  done;
  (DynArray.to_array sccs,
   (fun x -> TreeMap.find x !scc_index),
   DynArray.to_array (DynArray.map [] (fun s -> TreeSet.fold (fun x -> fun l -> x::l) s []) topology),
   TreeSet.fold (fun x -> fun l -> x::l) !roots [])

 method sccs_compute_connectors (sccs, sccindex, topology, roots) =
	let s = Array.length sccs in
	let conn = Hashtbl.create 10 in
	let computed = Array.make s false in

	let rec subcompute r =
		if (topology.(r) != []) && (not computed.(r)) then (
			computed.(r) <- true;
			let temp = Array.make s [] in
			List.iter subcompute topology.(r);
			ns_iter (fun v -> ns_iter (fun w ->
							if sccindex w != r
							then temp.(sccindex w) <- (v, w)::temp.(sccindex w)
						    )
						    (self#get_successors v)
				  ) sccs.(r);
			List.iter (fun c ->	Hashtbl.add conn (r, c) temp.(c)) topology.(r)
		)
	in

	List.iter subcompute roots;
	conn


(********** ATTRACTOR CLOSURE **********)    
method attr_closure_inplace' (strategy: strategy) player region include_region includeNode overwrite_strat =
  let message _ _ = () in
  let attr = ref ns_empty in
  let todo = Queue.create () in

  let schedule_predecessors v = ns_iter (fun w -> if includeNode w then (
                                                    message 3 (fun _ -> "    Scheduling node " ^ nd_show w ^
                                                                        " for attractor check\n");
                                                    Queue.add w todo)
                                                    )
                                          (self#get_predecessors v)
  in

  let inattr v = ns_elem v !attr || ((not include_region) && ns_elem v region) in

  ns_iter (fun v -> if include_region then attr := ns_add v !attr;
			 schedule_predecessors v) region;

  while not (Queue.is_empty todo) do
    let v = Queue.take todo in
    if not (ns_elem v !attr)
    then let pl' = self#get_owner v in
	 let ws = self#get_successors v in
         if pl'=player
         then let w = ns_fold (fun b -> fun w -> if (not (includeNode w)) || (b != nd_undef || not (inattr w)) then b else w) nd_undef ws in
              if w != nd_undef then (message 3 (fun _ -> "    Node " ^ nd_show v ^ " is in the attractor because of " ^
                                         nd_show v ^ "->" ^ nd_show w ^ "\n");
                              attr := ns_add v !attr;
                              if overwrite_strat || strategy#get v = nd_undef
                              then strategy#set v w;
                              schedule_predecessors v)
              else message 3 (fun _ -> "    Node " ^ nd_show v ^ " is not (yet) found to be in the attractor\n")
         else if ns_fold (fun b -> fun w -> b && (inattr w)) true ws
              then (message 3 (fun _ -> "    Node " ^ nd_show v ^ " is in the attractor because all successors are so");
                    attr := ns_add v !attr;
                    schedule_predecessors v)
              else message 3 (fun _ -> "    Node " ^ nd_show v ^ " is not (yet) found to be in the attractor\n")
  done;
  !attr

method attr_closure_inplace strategy player region =
	self#attr_closure_inplace' strategy player region true (fun _ -> true) true


method attractor_closure_inplace_sol_strat (deltafilter : (node -> bool)) (sol: solution) strat pl0 pl1 =
	let sol0 = self#attr_closure_inplace' strat plr_Even pl0 true (fun v -> not (ns_elem v pl1)) true in
	let sol1 = self#attr_closure_inplace' strat plr_Odd pl1 true (fun v -> not (ns_elem v pl0)) true in
	ns_iter (fun q -> sol#set q plr_Even) sol0;
	ns_iter (fun q -> sol#set q plr_Odd) sol1;
        (sol0, sol1)

(********** PARTIAL PARITYGAME **********)
method induce_partialparitygame start =
	let delta i = Enumerators.of_list (ns_nodes (self#get_successors i)) in
	let data i = (self#get_priority i, self#get_owner i) in
	let desc i = self#get_desc i in
	((start, delta, data, desc): partial_paritygame)

method induce_counting_partialparitygame start =
	let counter = ref 0 in
	let access = ref TreeSet.empty_def in
	let delta i =
		if not (TreeSet.mem i !access) then (
			access := TreeSet.add i !access;
			incr counter
		);
		Enumerators.of_list (ns_nodes (self#get_successors i))
	in
	let data i =
		if not (TreeSet.mem i !access) then (
            access := TreeSet.add i !access;
			incr counter
		);
		(self#get_priority i, self#get_owner i)
	in
	let desc i =
		if not (TreeSet.mem i !access) then (
            access := TreeSet.add i !access;
			incr counter
		);
		self#get_desc i
	in
		(counter, ((start, delta, data, desc): partial_paritygame))

method partially_solve_dominion (start: node) (partially_solve: partial_solver) =
	let n = self#size in
	let (_, delta, data, desc) = self#induce_partialparitygame start in
	let solution = new array_solution n in
	let strategy = new array_strategy n in

	let rec expand i f =
		if solution#get i != plr_undef then ()
		else let (winner, strat) = f i in
			 	solution#set i winner;
			 	match strat with
			 		Some j -> (
			 			strategy#set i j;
			 			expand j f
			 		)
			 	|   None -> ns_iter (fun x -> expand x f) (self#get_successors i)
	in

    expand start (partially_solve (start, delta, data, desc));

	(solution, strategy)

method partially_solve_game partially_solve =
	let n = self#size in
	let nodeIdx = ref nd_undef in
	self#iterate (fun i _ ->
	    if (!nodeIdx = nd_undef && self#is_defined i)
	    then nodeIdx := i
	);
	let (_, delta, data, desc) = self#induce_partialparitygame !nodeIdx in
	let solution = new array_solution n in
	let strategy = new array_strategy n in
	let data' node =
		if solution#get node = plr_undef
		then data node
		else (fst (data node), solution#get node)
	in
	let delta' node =
		if solution#get node = plr_undef
		then delta node
		else Enumerators.singleton node
	in

	let rec expand i f =
		if solution#get i != plr_undef then ()
		else let (winner, strat) = f i in
			 	solution#set i winner;
			 	match strat with
			 		Some j -> (
			 			strategy#set i j;
			 			expand j f
			 		)
			 	|   None -> ns_iter (fun x -> expand x f) (self#get_successors i)
	in

    self#iterate (fun i _ ->
        if (solution#get i != plr_undef) || (self#get_owner i = plr_undef) then ()
        else expand i (partially_solve (i, delta', data', desc))
    );

    (solution, strategy)

      
(********** GAME INFORMATION **********)
method get_player_decision_info =
  let hasPl0 = ref false in
  let hasPl1 = ref false in
  self#iterate (fun _ (_, pl, succs, _, _) ->
    if (ns_size succs > 1)
    then (if pl = plr_Even then hasPl0 else hasPl1) := true;
  );
  (!hasPl0, !hasPl1)

method is_single_parity_game =
    let hasPar0 = ref false in
    let hasPar1 = ref false in
    self#iterate (fun _ (pr, _, _, _, _) ->
    	if pr >= 0
    	then (if pr mod 2 = 0 then hasPar0 else hasPar1) := true;
    );
    if !hasPar0 && !hasPar1
    then None
    else Some (if !hasPar0 then 0 else 1)

method number_of_strategies pl m =
  let n = ref 1 in
  self#iterate (fun v -> fun (_,p,vs,_,_) -> if !n < m && p=pl then n := !n * (ns_size vs));
  min !n m

(********** DYNAMIC PARITYGAME **********)
method to_dynamic_paritygame =
	let graph = DynamicGraph.make () in
	self#iterate (fun i (pr, pl, _, _, desc) ->
		DynamicGraph.add_node i (pr, pl, desc) graph
	);
	self#iterate (fun i (_, _, tr, _, _) ->
		ns_iter (fun j -> DynamicGraph.add_edge i j graph) tr
	);
	graph

method to_dynamic_paritygame_by_strategy (strat: strategy) =
	let graph = DynamicGraph.make () in
	self#iterate (fun i (pr, pl, _, _, desc) ->
		DynamicGraph.add_node i (pr, pl, desc) graph
	);
	self#iterate (fun i (_, _, tr, _, _) ->
		if strat#get i = nd_undef
		then ns_iter (fun j -> DynamicGraph.add_edge i j graph) tr
		else DynamicGraph.add_edge i (strat#get i) graph
	);
	graph


(********** MODAL LOGIC **********)
method get_diamonds t =
  ns_fold (fun s -> fun v ->
                 ns_fold (fun s' -> fun u ->
                                   if self#is_defined u then
                                     ns_add u s'
                                   else s')
                                 s (self#get_predecessors v))
               ns_empty t

method get_boxes t =
  let c = self#get_diamonds t in
  ns_filter (fun v -> if self#is_defined v then
                             ns_fold (fun b -> fun w -> b && ns_elem w t) true (self#get_successors v)
                           else
                             false
                 ) c
end;;

  
(**************************************************************
 *                        SOLUTION PART 2                     *
 **************************************************************)
let sol_init game f =
    let sol = new array_solution game#size in
    game#iterate (fun i _ ->
        sol#set i (f i)
    );
    sol

let str_init game f =
    let str = new array_strategy game#size in
    game#iterate (fun i _ ->
        str#set i (f i)
    );
    str



(**************************************************************
 *                       GLOBAL_SOLVER                        *
 **************************************************************)
type global_solver = (paritygame -> solution * strategy)

                     


