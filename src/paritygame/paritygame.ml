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
(*open Pgprofiling*)
  


                                 
                              
(**************************************************************
 *                       PLAYER/PRIORITY                      *
 **************************************************************)
type player = int
type priority = int

(********** PLAYER FUNCTIONS **********)                  
let plr_Even = 0
let plr_Odd = 1
let plr_Zero = plr_Even
let plr_One = plr_Odd
let plr_undef = -1

let plr_random _ = Random.int 2
let plr_opponent pl = 1 - pl
let plr_benefits pr = pr mod 2
let plr_show = string_of_int

let plr_iterate f =
  f plr_Even; f plr_Odd


(********** PRIORITY FUNCTION **********)
let prio_good_for_player pr pl = if pl = plr_Even then pr mod 2 = 0 else pr mod 2 = 1
let odd pr = pr mod 2 = 1
let even pr = pr mod 2 = 0

                           
(**************************************************************
 *                        NODE ORDERING                       *
 **************************************************************)
type pg_ordering      = node * priority * player * nodeset -> node * priority * player * nodeset -> int

(* type pg_ordering      = int * int * int * int array -> int * int * int * int array -> int *)

let reward player prio =
	if (not (prio mod 2 = player)) && (prio >= 0) then -prio else prio;;

let ord_rew_for pl (_, pr, _, _) (_, pr', _, _) =
	if (pr != -1) && (pr' != -1)
	then compare (reward pl pr) (reward pl pr')
	else compare pr pr';;

let ord_prio (_, pr, _, _) (_, pr', _, _) = compare pr pr';;

let ord_total_by ordering (i, pri, pli, tri) (j, prj, plj, trj) =
	let o = ordering (i, pri, pli, tri) (j, prj, plj, trj) in
	if o = 0 then compare i j else o;;
  
(**************************************************************
 *                        SOLUTION                            *
 **************************************************************)
type solution = player array

(* sol_create and sol_init below paritygame class (because of dependency) *)
let sol_make n = Array.make n plr_undef
let sol_number_solved sol =
  Array.fold_left (fun c e -> if e = plr_undef then c else c + 1) 0 sol

let sol_get sol v = sol.(v)
let sol_set sol v pl = sol.(v) <- pl
let sol_iter = Array.iteri

let format_solution sol =
  let show i = match i with -1 -> "_" | _ -> string_of_int i in
  "[" ^ String.concat "," (Array.to_list (Array.mapi (fun i -> fun w -> string_of_int i ^ ":" ^ show w) sol)) ^ "]"

                                                                                                                  
(***************************************************************
 *                        STRATEGY                             *
 ***************************************************************)
type strategy = node array

(* str_create and str_init below paritygame class (because of dependency) *)                 
let str_make n = Array.make n nd_undef
let str_get str v = str.(v)
let str_set str v u = str.(v) <- u
let str_iter = Array.iteri

let format_strategy st =
  let show i = match i with -1 -> "_" | _ -> string_of_int i in
  "[" ^ String.concat "," (Array.to_list (Array.mapi (fun i -> fun w -> string_of_int i ^ "->" ^ show w) st)) ^ "]"

                                                                                                                  
(***************************************************************
 *                SOLUTION/STRATEGY FUNCTIONS                  *
 ***************************************************************)
exception Unmergable

let merge_strategies_inplace st1 st2 =
  let l = Array.length st1 in
  for i=0 to l-1 do
   st1.(i) <-  match (st1.(i),st2.(i)) with
                     (-1,x) -> x
                   | (x,-1) -> x
	 	   | _      -> raise Unmergable
  done

let merge_solutions_inplace sol1 sol2 =
  let l = Array.length sol1 in
  for i=0 to l-1 do
   sol1.(i) <-  match (sol1.(i),sol2.(i)) with
                      (-1,x) -> x
                    | (x,-1) -> x
	  	    | _      -> raise Unmergable
  done

let print_solution_strategy_parsable sol strat =
	let n = Array.length sol in
	print_string ("paritysol " ^ string_of_int (n-1) ^ ";\n");
	for i = 0 to n - 1 do
		if sol.(i) >= 0 then (
            print_int i;
            print_char ' ';
            print_int sol.(i);
            if strat.(i) >= 0 then (
                print_char ' ';
                print_int strat.(i)
            );
            print_char ';';
            print_newline ()
        )
        done;;
  
(**************************************************************
 *                   PARTIAL PARITYGAME                       *
 **************************************************************)
type partial_paritygame = int * (int -> int Enumerators.enumerator) * (int -> int * int) * (int -> string option)
type partial_solution = int -> int * int option
type partial_solver = partial_paritygame -> partial_solution


(**************************************************************
 *                   DYNAMIC PARITYGAME                       *
 **************************************************************)
type dynamic_paritygame = (priority * player * string option) DynamicGraph.dynamic_graph

let dynamic_subgame_by_strategy graph strat =
	DynamicGraph.sub_graph_by_edge_pred (fun v w ->
		strat.(v) = -1 || strat.(v) = w
	  ) graph

                                            
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
    s := "," ^ string_of_int (l-i) ^ ":{" ^ String.concat "," (List.map string_of_int (ns_nodes (Array.get sccs (l-i)))) ^ "}" ^ !s
  done;
  if l > 0 then s := "0:{" ^ String.concat "," (List.map string_of_int (ns_nodes (Array.get sccs 0))) ^ "}" ^ !s;
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
method virtual subgame_by_list : nodeset -> 'self
method virtual subgame_by_node_filter : (node -> bool) -> 'self * (node -> node) * (node -> node)

                                                                                     
(******************** NON-VIRTUAL METHODS ********************)

(********** GENERAL **********)
method print =
	let n = self#size in
	print_string ("parity " ^ string_of_int n ^ ";\n");
	for i = 0 to n - 1 do
	  let (pr, pl, succs, _, desc) = self#get_node i in
	  if pr >= 0 && pl >= 0 && pl <= 1 then (
            print_int i;
            print_char ' ';
            print_int pr;
            print_char ' ';
            print_int pl;
            print_char ' ';
            print_string (String.concat "," (List.map string_of_int (ns_nodes succs)));
            (
             match desc with
               None -> () (* print_string (" \"" ^ string_of_int i ^ "\"") *)
             | Some s -> if s <> "" then print_string (" \"" ^ s ^ "\"")
            );
            print_char ';';
            print_newline ()
           )
        done
          
method to_dotty solution strategy h =
  let encode i = "N" ^ (string_of_int i) in

  output_string h "digraph G {\n";

  for i = 0 to (self#size)-1 do
    let (p,pl,succs,_,ann) = self#get_node i in

    if p >= 0 && pl >= 0 && pl <= 1
    then (let name = encode i in
          let label = (match ann with None -> ""
                                    | Some s -> s ^ ": ") ^ string_of_int p
          in
          let shape = if pl=0 then "diamond" else "box" in
          let color = try
                        match solution.(i) with
                              0 -> "green"
                            | 1 -> "red"
			    | _ -> "black"
                      with _ -> "black"
          in
          output_string h (name ^ " [ shape=\"" ^ shape ^ "\", label=\"" ^ label ^ "\", color=\"" ^ color ^ "\" ];\n");

	  ns_iter (fun w -> let color2 = try
				             if pl = 1 - solution.(i) || w = strategy.(i) then color else "black"
				           with _ -> "black"
			      in
			      output_string h (name ^ " -> " ^ encode w ^ " [ color=\"" ^ color2 ^ "\" ];\n" )) succs
	 )
  done;
  output_string h "}\n"

method to_dotty_file solution strategy filename =
  let h = open_out filename in
  self#to_dotty solution strategy h;
  close_out h

            
(********** GETTERS **********)
method node_count =
  let count = ref 0 in
  let n = self#size in
  for i = 0 to n-1 do
    if self#is_defined i then incr count
  done;
  !count

method edge_count =
  let count = ref 0 in
  let n = self#size in
  for i=0 to n-1 do
    if self#is_defined i then count := !count + (ns_size (self#get_successors i))
  done;
  !count

method get_max o =
	let m = ref 0 in
	let n = self#size in
	let (prm,plm,succm,_,_) = self#get_node !m in
	let vm = ref (!m,prm,plm,succm) in
	for i = 1 to n - 1 do
	  let (pri,pli,succi,_,_) = self#get_node i in
	  let vi = (i,pri,pli,succi) in
	  if o !vm vi < 0
	  then (m := i; vm := vi)
	done;
	!m

method get_min o = self#get_max (fun x y -> - (o x y))
                                
method get_max_prio_node = self#get_max ord_prio
                                        
method get_max_rew_node_for pl = self#get_max (ord_rew_for pl)
                                              
method get_max_prio = self#get_priority (self#get_max_prio_node)
                                        
method get_min_prio = self#get_priority (self#get_min ord_prio)
                                        
method get_max_prio_for player =
	let pr = self#get_priority (self#get_max_rew_node_for player) in
	if pr mod 2 = player then pr else -1

method get_index = self#get_max_prio - self#get_min_prio + 1
                                                             
method get_prio_nodes p =
  let l = ref ns_empty in
  self#iterate (fun i (pr, _, _, _, _) ->
    if pr = p then l := ns_add i !l
  );
  !l

method get_selected_priorities pred =
  let prios = ref ns_empty in
  self#iterate (fun v -> fun (pr,_,_,_,_) -> if pred pr then prios := ns_add pr !prios);
  ns_nodes !prios

method get_priorities = self#get_selected_priorities (fun _ -> true)

method to_string =
	let n = self#size in
	let s = ref "" in
	for i = n-1 downto 0 do
	  let (pr, pl, succs, _ , desc) = self#get_node i in
	  if pr >= 0 && pl >= 0 && pl <= 1 then
	    begin
	      s := string_of_int i ^ " " ^ string_of_int pr ^ " " ^ string_of_int pl ^ " " ^
	           (String.concat "," (List.map string_of_int (ns_nodes succs)) ^
		     (match desc with
			None   -> ""
		      | Some a -> if a <> "" then " \"" ^ a ^ "\"" else "")
		   ) ^ ";\n" ^ !s
           end
	done;
	"parity " ^ string_of_int (n-1) ^ ";\n" ^ !s
                                                                                                     
                                                 
(********** NODE COLLECTION  **********)
method collect_nodes pred =
    let l = ref ns_empty in
    for i = (self#size) - 1 downto 0 do
    	if self#is_defined i && (pred i (self#get_node i)) then l := ns_add i !l
    done;
    !l
     
method collect_nodes_by_prio pred =
	self#collect_nodes (fun _ (pr, _, _, _, _) -> pred pr)

method collect_nodes_by_owner pred =
  let ltrue = ref ns_empty in
  let lfalse = ref ns_empty in
  for i = self#size - 1 downto 0 do
    if self#is_defined i then
      begin
	if pred (self#get_owner i) then
	  ltrue := ns_add i !ltrue
	else
	  lfalse := ns_add i !lfalse
      end
  done;
  (!ltrue, !lfalse)

method collect_max_prio_nodes =
  let m = self#get_max_prio in
	self#collect_nodes_by_prio (fun pr -> pr = m)

method collect_max_parity_nodes =
	let (p0, p1) = (self#get_max_prio_for 0, self#get_max_prio_for 1) in
    let p = if p0 < 0 then p1 else if p1 < 0 then p0
            else if p0 < p1 then p0 + 1 else p1 + 1 in
    self#collect_nodes_by_prio (fun pr -> pr >= p)

                               
(********** SUBGAME **********)
method subgame_by_strat strat = self#subgame_by_edge_pred (fun x y -> strat.(x) < 0 || strat.(x) = y)
                                                          
method subgame_by_strat_pl strat pl =
   self#subgame_by_edge_pred (fun i j ->
		let pl' = self#get_owner i in
		pl != pl' || strat.(i) = j
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

method set_dominion solver nodeset pl =
	if self#set_closed nodeset pl then (
        let l = ns_nodes nodeset in
        let a = Array.of_list l in
        let (sol, strat') = solver (self#subgame_by_list nodeset) in
        if ArrayUtils.forall sol (fun _ pl' -> pl' = pl)
        then (
        	let strat = Array.make (self#size) (-1) in
            let i = ref 0 in
            List.iter (fun q ->
                if strat'.(!i) != -1
                then strat.(q) <- a.(strat'.(!i));
                i := !i + 1
            ) l;
            Some strat
        )
        else None
    )
        else None

               
(********** DECOMPOSITION **********)
method strongly_connected_components (*tgraph*) =
  let l = self#size in
  let dfsnum = Array.make l (-1) in
  let index = Array.make l (-1) in

  let todo = ref [] in
  for i=l-1 downto 0 do
    if self#is_defined i then todo := i :: !todo
  done;

  let n = ref 0 in
  let visited = Array.make l false in

  let dfs v =
  	let st = Stack.create () in
  	Stack.push v st;
  	while not (Stack.is_empty st) do
  		let u = Stack.pop st in
  		let pushed = ref false in
  		if not visited.(u) then (
  			visited.(u) <- true;
  			ns_iter (fun w ->
  				if not visited.(w) then (
  					if not !pushed then (
  						Stack.push u st;
  						pushed := true
  					);
  					Stack.push w st
  				)
  			) (self#get_successors u)
  		);
  		if (not !pushed) && (dfsnum.(u) < 0) then (
  			dfsnum.(u) <- !n;
  			index.(!n) <- u;
  			incr n
  		)
  	done
  in

  for i=0 to l-1 do
    if not visited.(i) && self#is_defined i then dfs i
  done;

  decr n;

  for i=0 to l-1 do
    visited.(i) <- false
  done;

  let sccs = DynArray.create ns_empty in
  let topology = DynArray.create TreeSet.empty_def in
  let scc_index = Array.make l (-1) in
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

      if not visited.(v) && dfsnum.(v) >= 0
      then (visited.(v) <- true;
            scc := ns_add v !scc;
            let succs = List.sort (fun x -> fun y -> compare dfsnum.(y) dfsnum.(x)) (ns_nodes (self#get_predecessors v)) in
            todo := succs @ !todo;
            List.iter (fun w -> let c = scc_index.(w) in
                                if c > -1
                                then (DynArray.set topology c (TreeSet.add !next_index (DynArray.get topology c));
                                      is_root := false))
                       succs)
    done;
    DynArray.insert sccs !next_index !scc;
    if !is_root then roots := TreeSet.add !next_index !roots;
    ns_iter (fun v -> scc_index.(v) <- !next_index) !scc;
    incr next_index;

    while !n >= 0 && visited.(index.(!n)) do
      decr n
    done
  done;
  (DynArray.to_array sccs,
   scc_index,
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
							if sccindex.(w) != r
							then temp.(sccindex.(w)) <- (v, w)::temp.(sccindex.(w))
						    )
						    (self#get_successors v)
				  ) sccs.(r);
			List.iter (fun c ->	Hashtbl.add conn (r, c) temp.(c)) topology.(r)
		)
	in

	List.iter subcompute roots;
	conn


(********** ATTRACTOR CLOSURE **********)    
method attr_closure_inplace' strategy player region include_region includeNode overwrite_strat =
  let message _ _ = () in
  let attr = ref ns_empty in
  let todo = Queue.create () in

  let schedule_predecessors v = ns_iter (fun w -> if includeNode w then (
                                                    message 3 (fun _ -> "    Scheduling node " ^ string_of_int w ^
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
         then let w = ns_fold (fun b -> fun w -> if (not (includeNode w)) || (b > -1 || not (inattr w)) then b else w) (-1) ws in
              if w > -1 then (message 3 (fun _ -> "    Node " ^ string_of_int v ^ " is in the attractor because of " ^
                                         string_of_int v ^ "->" ^ string_of_int w ^ "\n");
                              attr := ns_add v !attr;
                              if overwrite_strat || strategy.(v) < 0
                              then strategy.(v) <- w;
                              schedule_predecessors v)
              else message 3 (fun _ -> "    Node " ^ string_of_int v ^ " is not (yet) found to be in the attractor\n")
         else if ns_fold (fun b -> fun w -> b && (inattr w)) true ws
              then (message 3 (fun _ -> "    Node " ^ string_of_int v ^ " is in the attractor because all successors are so");
                    attr := ns_add v !attr;
                    schedule_predecessors v)
              else message 3 (fun _ -> "    Node " ^ string_of_int v ^ " is not (yet) found to be in the attractor\n")
  done;
  !attr

method attr_closure_inplace strategy player region =
	self#attr_closure_inplace' strategy player region true (fun _ -> true) true


method attractor_closure_inplace_sol_strat (deltafilter : (node -> bool)) sol strat pl0 pl1 =
	let sol0 = self#attr_closure_inplace' strat 0 pl0 true (fun v -> not (ns_elem v pl1)) true in
	let sol1 = self#attr_closure_inplace' strat 1 pl1 true (fun v -> not (ns_elem v pl0)) true in
	ns_iter (fun q -> sol.(q) <- 0) sol0;
	ns_iter (fun q -> sol.(q) <- 1) sol1;
        (sol0, sol1)

(********** PARTIAL PARITYGAME **********)
method induce_partialparitygame start =
	let delta i = Enumerators.of_list (ns_nodes (self#get_successors i)) in
	let data i = (self#get_priority i, self#get_owner i) in
	let desc i = self#get_desc i in
	((start, delta, data, desc): partial_paritygame)

method induce_counting_partialparitygame start =
	let counter = ref 0 in
	let access = Array.make (self#size) false in
	let delta i =
		if not access.(i) then (
			access.(i) <- true;
			incr counter
		);
		Enumerators.of_list (ns_nodes (self#get_successors i))
	in
	let data i =
		if not access.(i) then (
			access.(i) <- true;
			incr counter
		);
		(self#get_priority i, self#get_owner i)
	in
	let desc i =
		if not access.(i) then (
			access.(i) <- true;
			incr counter
		);
		self#get_desc i
	in
		(counter, ((start, delta, data, desc): partial_paritygame))

method partially_solve_dominion (start: int) (partially_solve: partial_solver) =
	let n = self#size in
	let (_, delta, data, desc) = self#induce_partialparitygame start in
	let solution = Array.make n (-1) in
	let strategy = Array.make n (-1) in

	let rec expand i f =
		if solution.(i) > -1 then ()
		else let (winner, strat) = f i in
			 	solution.(i) <- winner;
			 	match strat with
			 		Some j -> (
			 			strategy.(i) <- j;
			 			expand j f
			 		)
			 	|   None -> ns_iter (fun x -> expand x f) (self#get_successors i)
	in

    expand start (partially_solve (start, delta, data, desc));

	(solution, strategy)

method partially_solve_game partially_solve =
	let n = self#size in
	let (_, delta, data, desc) = self#induce_partialparitygame 0 in
	let solution = Array.make n (-1) in
	let strategy = Array.make n (-1) in
	let data' node =
		if solution.(node) = -1
		then data node
		else (solution.(node), snd (data node))
	in
	let delta' node =
		if solution.(node) = -1
		then delta node
		else Enumerators.singleton node
	in

	let rec expand i f =
		if solution.(i) > -1 then ()
		else let (winner, strat) = f i in
			 	solution.(i) <- winner;
			 	match strat with
			 		Some j -> (
			 			strategy.(i) <- j;
			 			expand j f
			 		)
			 	|   None -> ns_iter (fun x -> expand x f) (self#get_successors i)
	in

    for i = 0 to n - 1 do
        if (solution.(i) > -1) || (self#get_owner i < 0) then ()
        else expand i (partially_solve (i, delta', data', desc))
    done;

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

method compute_priority_reach_array player =
    let maxprspm = (self#get_max_prio_for (1 - player)) / 2 in
    (* Dumb version (!)  *)
    let rec calc_iter game  maxvalues =
        let badPrio = game#get_max_prio_for (1 - player) in
        let goodPrio = game#get_max_prio_for player in
        if badPrio >= 0 then (
            let tmp_nodes = ref ns_empty in
            if goodPrio > badPrio then
                game#iterate (fun i (pr, _, _, _, _) ->
                    if pr > badPrio then tmp_nodes := ns_add i !tmp_nodes
                )
            else (
                let (sccs, sccindex, topology, roots): nodeset array * scc array * scc list array * scc list = game#strongly_connected_components in
                let sccs: nodeset array = sccs in
                let sccentry = Array.make (Array.length sccs) (-1) in
                let rec count_nodes r =
                	if sccentry.(r) = -1 then (
                        List.iter count_nodes topology.(r);
                        sccentry.(r) <- List.fold_left (fun a i -> a + sccentry.(i)) 0 topology.(r);
                        ns_iter (fun v ->
                        	if game#get_priority v = badPrio then sccentry.(r) <- 1 + sccentry.(r)
                        ) sccs.(r)
					)
                in
                List.iter count_nodes roots;
                game#iterate (fun i (pr, _, _, _, _) ->
                    if pr >= 0 then (maxvalues.(i)).(badPrio / 2) <- 1 + sccentry.(sccindex.(i));
                    if pr = badPrio then tmp_nodes := ns_add i !tmp_nodes
                )
            );
            game#remove_nodes !tmp_nodes;
            calc_iter game maxvalues
        )
    in
    let game = self#copy in
    let maxvalues = Array.make_matrix (game#size) (1 + maxprspm) 1 in
    calc_iter game maxvalues;
    maxvalues

      
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

method to_dynamic_paritygame_by_strategy strat =
	let graph = DynamicGraph.make () in
	self#iterate (fun i (pr, pl, _, _, desc) ->
		DynamicGraph.add_node i (pr, pl, desc) graph
	);
	self#iterate (fun i (_, _, tr, _, _) ->
		if strat.(i) = -1
		then ns_iter (fun j -> DynamicGraph.add_edge i j graph) tr
		else DynamicGraph.add_edge i strat.(i) graph
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
let sol_create game  = Array.make (game#size) plr_undef                               
let sol_init game f = Array.init (game#size) f


(***************************************************************
 *                        STRATEGY PART 2                      *
 ***************************************************************)
let str_create game = Array.make (game#size) nd_undef                               
let str_init game f = Array.init (game#size) f

                                 
(**************************************************************
 *                       GLOBAL_SOLVER                        *
 **************************************************************)
type global_solver = (paritygame -> solution * strategy)

                     


