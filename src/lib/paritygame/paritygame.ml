open Basics;;
open Tcsbasedata;;
open Tcsarray;;
open Tcsset;;
open Tcslist;;
open Tcsgraph;;
(* open Pgprofiling;; *)

(**************************************************************
 * nodes in a parity game                                     *
 **************************************************************)		    

type node = int

let nd_undef = -1
let nd_make v = v
let nd_reveal v = v		  

let nd_show = string_of_int
		
(**************************************************************
 * access functions for nodes in set-like data structures for *
 * successors and predecessors in a game                      *
 *                                                            *
 * here: sorted lists                                         *
 **************************************************************)


let ns_nodeCompare = compare



type nodeset = node TreeSet.t

let ns_isEmpty = TreeSet.is_empty
let ns_compare = TreeSet.compare
let ns_empty = TreeSet.empty ns_nodeCompare
let ns_elem = TreeSet.mem
let ns_fold f acc ns = TreeSet.fold (fun x y -> f y x) ns acc
let ns_iter = TreeSet.iter
let ns_filter = TreeSet.filter
let ns_map = TreeSet.map
let ns_size = TreeSet.cardinal
let ns_exists = TreeSet.exists
let ns_forall = TreeSet.for_all
let ns_first = TreeSet.min_elt
let ns_last = TreeSet.max_elt
let ns_some = TreeSet.choose
let ns_add = TreeSet.add
let ns_del = TreeSet.remove
let ns_union = TreeSet.union
let ns_make = TreeSet.of_list ns_nodeCompare
let ns_nodes = TreeSet.elements





(*
let ns_nodeCompare = compare

type nodeset = node list
   
let ns_compare = ListUtils.compare_lists ns_nodeCompare

let ns_isEmpty ws = ws = []
let ns_empty = []
let ns_elem = List.mem
let ns_fold = List.fold_left
let ns_iter = List.iter
let ns_filter = List.filter
let ns_map f = ns_fold (fun ns v -> let u = f v in if not (ns_elem u ns) then u::ns else ns) [] 
let ns_size = List.length
let ns_exists = List.exists
let ns_forall = List.for_all
let ns_first = List.hd
let rec ns_last = function []    -> failwith "Paritygame.ns_last: cannot extract node from empty node set"
                        | [u]   -> u
                        | _::us -> ns_last us

let ns_add v vs =
  let rec add = function []    -> [v]
                      | w::ws -> (match ns_nodeCompare v w with
                                    -1 -> v::w::ws
                                  | 0  -> w::ws
                                  | 1  -> w::(add ws)
                                  | _  -> failwith "Paritygame.ns_add: unexpected return value of function `compare´")
  in
  add vs

let ns_del v vs =
  let rec del = function []    -> []
                      | w::ws -> (match ns_nodeCompare v w with
                                    -1 -> w::ws
                                  | 0  -> ws
                                  | 1  -> w::(del ws)
                                  | _  -> failwith "Paritygame.ns_del: unexpected return value of function `compare´")
  in
  del vs

let ns_make = List.sort compare
let ns_nodes ws = ws

let ns_union a b = TreeSet.elements (TreeSet.union (TreeSet.of_list_def a) (TreeSet.of_list_def b))

*)


let ns_find f ns =
    OptionUtils.get_some (ns_fold (fun a v -> if a = None && f v then Some v else a) None ns)

let ns_some ws =
  let n = ns_size ws in
  let i = ref (Random.int n) in
  ns_find (fun v ->
    decr i;
    !i = -1
  ) ws

let ns_max ns lessf = ns_fold (fun v -> fun w -> if lessf v w then w else v) (ns_some ns) ns



(**************************************************************
 * players                                                    *
 **************************************************************)

type player = int

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
		
(**************************************************************
 * priorities                                                 *
 **************************************************************)

type priority = int

let prio_good_for_player pr pl = if pl = plr_Even then pr mod 2 = 0 else pr mod 2 = 1

let odd pr = pr mod 2 = 1 
let even pr = pr mod 2 = 0 
									   
(**************************************************************
 * Parity Game Definitions                                    *
 **************************************************************)

type paritygame = (priority * player * nodeset * nodeset * string option) array
		    
type solution = player array
type strategy = node array

type global_solver = (paritygame -> solution * strategy)

(**************************************************************
 * Access Functions                                           *
 *                                                            *
 * these depend on the type paritygame                        *
 * independent functions should be listed below               *
 **************************************************************)

let pg_create n = Array.make n (-1, -1, ns_empty, ns_empty, None)

let pg_sort = Array.sort

let pg_size = Array.length

let pg_isDefined game v =
  (* prof_access v prof_definedcheck; *)
  let (p,_,_,_,_) = game.(v) in p >= 0
  
let pg_get_node pg i = pg.(i)
let pg_set_node' pg i node = pg.(i) <- node

let pg_iterate f game =
  for i=0 to (pg_size game) - 1 do
    if pg_isDefined game i then f i (pg_get_node game i)
  done
    
let pg_map = Array.mapi
let pg_map2 = Array.mapi

let pg_edge_iterate f pg =
  pg_iterate (fun v -> fun (_,_,succs,_,_) -> ns_iter (fun w -> f v w) succs) pg
		       
let pg_find_desc pg desc = ArrayUtils.find (fun (_,_,_,_,desc') -> desc = desc') pg

let pg_add_edge gm v u =
  (* prof_access v prof_successors; *)
  (* prof_access u prof_predecessors; *)
  let (pr,pl,succs,preds,desc) = pg_get_node gm v in
  pg_set_node' gm v (pr, pl, ns_add u succs, preds, desc);
  let (pr,pl,succs,preds,desc) = pg_get_node gm u in
  pg_set_node' gm u (pr, pl, succs, ns_add v preds, desc)
	      	    
let pg_del_edge gm v u =
  (* prof_access v prof_successors; *)
  (* prof_access u prof_predecessors; *)
  let (pr,pl,succs,preds,desc) = pg_get_node gm v in
  pg_set_node' gm v (pr, pl, ns_del u succs, preds, desc);
  let (pr,pl,succs,preds,desc) = pg_get_node gm u in
  pg_set_node' gm u (pr, pl, succs, ns_del v preds, desc)



(**********************************************************
 * access functions for parity games                      *
 **********************************************************)

(* for internal use only! *)
let pg_set_node pg i pr pl succs preds desc = pg_set_node' pg i (pr, pl, succs, preds, desc);;

let pg_get_priority pg i =
  (* prof_access i prof_priority; *)
  let (pr, _, _, _, _) = pg_get_node pg i in pr

let pg_set_priority pg i pr =
  (* prof_access i prof_priority; *)
  let (_, pl, succs, preds, desc) = pg_get_node pg i in
  pg_set_node pg i pr pl succs preds desc

let pg_get_owner pg i =
  (* prof_access i prof_owner; *)
  let (_, pl, _, _, _) = pg_get_node pg i in pl

let pg_set_owner pg i pl =
  (* prof_access i prof_owner; *)
  let (pr, _, succs, preds, desc) = pg_get_node pg i in
  pg_set_node pg i pr pl succs preds desc

let pg_get_desc pg i = let (_, _, _, _, desc) = pg_get_node pg i in desc

let pg_set_desc pg i desc = let (pr, pl, succs, preds, _) = pg_get_node pg i in
			    pg_set_node pg i pr pl succs preds desc

let pg_get_desc' pg i = match pg_get_desc pg i with None -> "" | Some s -> s

let pg_set_desc' pg i desc = pg_set_desc pg i (if desc = "" then None else Some desc)

let pg_get_successors pg i =
  (* prof_access i prof_successors; *)
    let (_,_,succs,_,_) = pg_get_node pg i in succs
						
let pg_get_predecessors pg i =
  (* prof_access i prof_predecessors; *)
  let (_,_,_,preds,_) = pg_get_node pg i in preds


let pg_node_count game =
	let count = ref 0 in
	let n = pg_size game in
	for i = 0 to n - 1 do
		if pg_isDefined game i then incr count
	done;
	!count;;

let pg_edge_count game =
	let count = ref 0 in
	let n = pg_size game in
	for i = 0 to n - 1 do
		if pg_isDefined game i then count := !count + (ns_size (pg_get_successors game i))
	done;
	!count;;

let pg_copy pg =
	let pg' = pg_create (pg_size pg) in
	pg_iterate (fun i -> fun (pr, pl, succs, preds, desc) -> pg_set_node pg' i pr pl succs preds desc) pg;
	pg';;

let pg_init n f =
  let game = pg_create n in
  for i=0 to n-1 do
    let (pr,pl,succs,name) = f i in
    pg_set_priority game i pr;
    pg_set_owner game i pl;
    pg_set_desc game i name;
    List.iter (fun w -> pg_add_edge game i w) succs
  done;
  game;;

  
let pg_remove_nodes game nodes =
  ns_iter (fun v -> let succs = pg_get_successors game v in
		      ns_iter (fun u -> pg_del_edge game v u) succs;
		      let preds = pg_get_predecessors game v in
		      ns_iter (fun u -> pg_del_edge game u v) preds;
		      pg_set_priority game v (-1);
		      pg_set_owner game v (-1);
		      pg_set_desc game v None
	    ) nodes

let pg_remove_edges game edges =
    List.iter (fun (v, w) -> pg_del_edge game v w) edges;;

  

 
(**************************************************************
 * Solutions                                                  *
 **************************************************************)

let sol_create game = Array.make (pg_size game) plr_undef
let sol_make n = Array.make n plr_undef
let sol_init game f = Array.init (pg_size game) f			    

let sol_number_solved sol =
  Array.fold_left (fun c e -> if e = plr_undef then c else c + 1) 0 sol

let sol_get sol v = sol.(v) 
let sol_set sol v pl = sol.(v) <- pl
let sol_iter = Array.iteri  

(***************************************************************
 * Strategies                                                  *
 ***************************************************************)
							   
let str_create game = Array.make (pg_size game) nd_undef
let str_make n = Array.make n nd_undef 
let str_init game f = Array.init (pg_size game) f 

let str_get str v = str.(v)
let str_set str v u = str.(v) <- u 
let str_iter = Array.iteri 





(**************************************************************
 * Formatting Functions                                       *
 **************************************************************)

let game_to_string game =
	let n = pg_size game in
	let s = ref "" in
	for i = n-1 downto 0 do
	  let (pr, pl, succs, _ , desc) = pg_get_node game i in
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
	"parity " ^ string_of_int (n-1) ^ ";\n" ^ !s;;

let output_int c_out i = output_string c_out (string_of_int i);;
let output_newline c_out = output_char c_out '\n'; flush c_out;;
let output_game c_out game =
	let n = pg_size game in
	output_string c_out ("parity " ^ string_of_int (n-1) ^ ";\n");
	for i = 0 to n - 1 do
	  let (pr, pl, succs, _, desc) = pg_get_node game i in
	  if pr >= 0 && pl >= 0 && pl <= 1 then (
            output_int c_out i;
            output_char c_out ' ';
            output_int c_out pr;
            output_char c_out ' ';
            output_int c_out pl;
            output_char c_out ' ';
            output_string c_out (String.concat "," (List.map string_of_int (ns_nodes succs)));
            (
             match desc with
               None -> () (* print_string (" \"" ^ string_of_int i ^ "\"") *)
             | Some s -> if s <> "" then output_string c_out (" \"" ^ s ^ "\"")
            );
            output_char c_out ';';
            output_newline c_out
           )
        done;;
let print_game = output_game stdout;;

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
        
let to_dotty game solution strategy h =
  let encode i = "N" ^ (string_of_int i) in

  output_string h "digraph G {\n";

  for i = 0 to (pg_size game)-1 do
    let (p,pl,succs,_,ann) = pg_get_node game i in

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
  output_string h "}\n";;


let to_dotty_file game solution strategy filename =
  let h = open_out filename in
  to_dotty game solution strategy h;
  close_out h;;

let format_strategy st =
  let show i = match i with -1 -> "_" | _ -> string_of_int i in
  "[" ^ String.concat "," (Array.to_list (Array.mapi (fun i -> fun w -> string_of_int i ^ "->" ^ show w) st)) ^ "]"

let format_solution sol =
  let show i = match i with -1 -> "_" | _ -> string_of_int i in
  "[" ^ String.concat "," (Array.to_list (Array.mapi (fun i -> fun w -> string_of_int i ^ ":" ^ show w) sol)) ^ "]"

let format_game gm =
  "[" ^
  String.concat ";"
                (List.filter (fun s -> s <> "")
                             (Array.to_list (pg_map (fun i -> fun (p,pl,ws,_,_) ->
                                              if p <> -1 then string_of_int i ^ ":" ^ string_of_int p ^ "," ^
                                                              string_of_int pl ^ ",{" ^
                                                              String.concat "," (List.map string_of_int (ns_nodes ws))
                                                              ^ "}"
                                                         else "") gm)))
  ^ "]"



(**************************************************************
 * Node Orderings                                             *
 **************************************************************)

(* type pg_ordering      = int * int * int * int array -> int * int * int * int array -> int *)
type pg_ordering      = node * priority * player * nodeset -> node * priority * player * nodeset -> int

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

let pg_max pg o =
	let m = ref 0 in
	let n = pg_size pg in
	let (prm,plm,succm,_,_) = pg_get_node pg !m in
	let vm = ref (!m,prm,plm,succm) in
	for i = 1 to n - 1 do
	  let (pri,pli,succi,_,_) = pg_get_node pg i in
	  let vi = (i,pri,pli,succi) in
	  if o !vm vi < 0
	  then (m := i; vm := vi)
	done;
	!m;;

let pg_min pg o = pg_max pg (fun x y -> - (o x y));;

let pg_max_prio_node pg = pg_max pg ord_prio;;

let pg_max_rew_node_for pg pl = pg_max pg (ord_rew_for pl);;

let pg_max_prio pg = pg_get_priority pg (pg_max_prio_node pg);;

let pg_min_prio pg = pg_get_priority pg (pg_min pg ord_prio);;

let pg_max_prio_for pg player =
	let pr = pg_get_priority pg (pg_max_rew_node_for pg player) in
	if pr mod 2 = player then pr else -1;;

let pg_get_index pg = pg_max_prio pg - pg_min_prio pg + 1;;

let pg_prio_nodes pg p =
  let l = ref ns_empty in
  for i = (pg_size pg)-1 downto 0 do
    if pg_get_priority pg i = p then l := ns_add i !l
  done;
  !l

let pg_get_selected_priorities game pred =
  let prios = ref ns_empty in
  pg_iterate (fun v -> fun (pr,_,_,_,_) -> if pred pr then prios := ns_add pr !prios) game;
  ns_nodes !prios

let pg_get_priorities game = pg_get_selected_priorities game (fun _ -> true)

(**************************************************************
 * Node Collect Functions                                     *
 **************************************************************)

let collect_nodes game pred =
    let l = ref ns_empty in
    for i = (pg_size game) - 1 downto 0 do
    	if pg_isDefined game i && (pred i (pg_get_node game i)) then l := ns_add i !l
    done;
    !l;;

let collect_nodes_by_prio game pred =
	collect_nodes game (fun _ (pr, _, _, _, _) -> pred pr);;

let collect_nodes_by_owner game pred =
  let ltrue = ref ns_empty in
  let lfalse = ref ns_empty in
  for i = pg_size game - 1 downto 0 do
    if pg_isDefined game i then
      begin
	if pred (pg_get_owner game i) then
	  ltrue := ns_add i !ltrue
	else
	  lfalse := ns_add i !lfalse
      end
  done;
  (!ltrue, !lfalse)

let collect_max_prio_nodes game =
	let m = pg_max_prio game in
	collect_nodes_by_prio game (fun pr -> pr = m);;

let collect_max_parity_nodes game =
	let (p0, p1) = (pg_max_prio_for game 0, pg_max_prio_for game 1) in
    let p = if p0 < 0 then p1 else if p1 < 0 then p0
            else if p0 < p1 then p0 + 1 else p1 + 1 in
    collect_nodes_by_prio game (fun pr -> pr >= p);;



(**************************************************************
 * Sub Game Creation                                          *
 **************************************************************)

let subgame_by_node_filter (game: paritygame) pred =
    let map_to_sub = ref TreeMap.empty_def in
    let map_to_game = ref TreeMap.empty_def in
    pg_iterate (fun i _ ->
        if pred i then (
            map_to_sub := TreeMap.add i (TreeMap.cardinal !map_to_game) !map_to_sub;
            map_to_game := TreeMap.add (TreeMap.cardinal !map_to_game) i !map_to_game
        )
    ) game;
    let sub = pg_init (TreeMap.cardinal !map_to_game) (fun i ->
        let j = TreeMap.find i !map_to_game in
        let li = ref [] in
        ns_iter (fun k ->
            if (TreeMap.mem k !map_to_sub)
            then li := (TreeMap.find k !map_to_sub) :: !li
        ) (pg_get_successors game j);
        (pg_get_priority game j,
         pg_get_owner game j,
         !li,
         pg_get_desc game j)
    ) in
    (sub, (fun i -> TreeMap.find i !map_to_sub), (fun i -> TreeMap.find i !map_to_game));;


let subgame_by_edge_pred (game: paritygame) pred =
	let n = pg_size game in
	let g = pg_create n in
	for i = 0 to n - 1 do
		pg_set_priority g i (pg_get_priority game i);
		pg_set_owner g i (pg_get_owner game i);
		pg_set_desc g i (pg_get_desc game i);
		ns_iter (fun j ->
			if pred i j then pg_add_edge g i j
		) (pg_get_successors game i)
	done;
	g;;

let subgame_by_node_pred game pred =
	let n = pg_size game in
	let g = pg_create n in
	for i = 0 to n - 1 do
		if (pred i) then (
			pg_set_priority g i (pg_get_priority game i);
			pg_set_owner g i (pg_get_owner game i);
			pg_set_desc g i (pg_get_desc game i);
			ns_iter (fun j ->
				pg_add_edge g i j
			) (pg_get_successors game i)
		)
	done;
	g;;

let subgame_by_strat game strat = subgame_by_edge_pred game (fun x y -> strat.(x) < 0 || strat.(x) = y);;

let subgame_by_strat_pl game strat pl =
	subgame_by_edge_pred game (fun i j ->
		let pl' = pg_get_owner game i in
		pl != pl' || strat.(i) = j
	);;

let subgame_by_list game li =
  (* Very dirty solution: original game is being destroyed temporarily and restored in the end.
     Maybe better to use separate data structures to store information about renaming and which nodes have been visited.
     I am also not sure that it is correct anymore. Does pg_add_edge know the right new names in the subgame to store predecessor information? - ML *) 
    let n = ns_size li in
    let g = pg_create n in
    let i = ref 0 in
    ns_iter (fun arri ->
               let (pr, pl, succs, preds, desc) = pg_get_node game arri in
               pg_set_priority game arri (-2);
	       pg_set_owner game arri !i; (* dirty code: player int values are used to remember the re-mapping of node names *) 
               pg_set_priority g !i pr;
	       pg_set_owner g !i pl;
	       pg_set_desc g !i desc;
               incr i
    ) li;
    i := 0;
    ns_iter (fun arri ->
               (* let pr = pg_get_priority g !i in   (* seemingly unused code *)
	       let pl = pg_get_owner g !i in *)
               let l = ref [] in
               ns_iter (fun w -> let h = pg_get_priority game w in (* dirty code: priority int values are used to remember visitation status of a node *)
				 let k = pg_get_owner game w in (* dirty code: player int value is actually referring to old node name *) 
				 if h = -2 then l := k::!l
		       ) (pg_get_successors game arri);
	       List.iter (fun w -> pg_add_edge g !i w) !l;
               incr i
    ) li;
    i := 0;
    ns_iter (fun arri ->
	       pg_set_priority game arri (pg_get_priority g !i);
	       pg_set_owner game arri (pg_get_owner g !i);
               incr i 
    ) li;
    g;;

(* DEPRECATED: use subgame_by_list instead; it has the graph information built in now
let subgame_and_subgraph_by_list game tgraph li =
    let n = List.length li in
    let g = pg_create n in
	let t = Array.make n [] in
    let i = ref 0 in
    List.iter (fun arri ->
        let (pr, pl, delta, desc) = game.(arri) in
        game.(arri) <- (-2, !i, delta, desc);
        g.(!i) <- (pr, pl, [||], desc);
        i := !i + 1
    ) li;
    let i = ref 0 in
    List.iter (fun arri ->
        let (pr, pl, _, desc) = g.(!i) in
        let (_, _, delta, _) = game.(arri) in
        let l = ref [] in
		for j = 0 to (Array.length delta) - 1 do
			let (h, k, _, _) = game.(delta.(j)) in
				if h = -2 then l := k::!l
		done;
        g.(!i) <- (pr, pl, Array.of_list !l, desc);
        let l = ref [] in
		List.iter (fun j ->
			let (h, k, _, _) = game.(j) in
				if h = -2 then l := k::!l
		) tgraph.(arri);
		t.(!i) <- !l;
        i := !i + 1
    ) li;
    let i = ref 0 in
    List.iter (fun arri ->
        let (_, _, delta, desc) = game.(arri) in
        let (pr, pl, _, _) = g.(!i) in
        game.(arri) <- (pr, pl, delta, desc);
        i := !i + 1
    ) li;
    (g,t);;
 *)


(**************************************************************
 * Solution / Strategy Update Functions                       *
 **************************************************************)

(* result := sol o perm *)
(*
let permute_solution perm sol =
	let n = Array.length sol in
	let sol' = Array.make n (-1) in

    for i = 0 to n - 1 do
        sol'.(i) <- sol.(perm.(i))
    done;
    sol'
*)

(* result := perm^-1 o strat o perm *)
(*
let permute_strategy perm perm' strat =
	let n = Array.length strat in
	let strat' = Array.make n (-1) in

    for i = 0 to n - 1 do
    	let j = strat.(perm.(i)) in
	        strat'.(i) <- if j = -1 then -1 else perm'.(j)
    done;
    strat'
*)

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



(**************************************************************
 * Decomposition Functions                                    *
 **************************************************************)

type scc = int
	     
let strongly_connected_components (game: paritygame) (*tgraph*) =
  let l = pg_size game in
  let dfsnum = Array.make l (-1) in
  let index = Array.make l (-1) in

  let todo = ref [] in
  for i=l-1 downto 0 do
    if pg_isDefined game i then todo := i :: !todo
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
  			) (pg_get_successors game u)
  		);
  		if (not !pushed) && (dfsnum.(u) < 0) then (
  			dfsnum.(u) <- !n;
  			index.(!n) <- u;
  			incr n
  		)
  	done
  in

  for i=0 to l-1 do
    if not visited.(i) && pg_isDefined game i then dfs i
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
            let succs = List.sort (fun x -> fun y -> compare dfsnum.(y) dfsnum.(x)) (ns_nodes (pg_get_predecessors game v)) in
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
   TreeSet.fold (fun x -> fun l -> x::l) !roots []);;

  
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


let sccs_compute_connectors game (sccs, sccindex, topology, roots) =
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
						    (pg_get_successors game v)
				  ) sccs.(r);
			List.iter (fun c ->	Hashtbl.add conn (r, c) temp.(c)) topology.(r)
		)
	in

	List.iter subcompute roots;
	conn;;


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
 * Attractor Closure                                          *
 **************************************************************)

let attr_closure_inplace' (game: paritygame) strategy player region include_region includeNode overwrite_strat =
  let message _ _ = () in
  let attr = ref ns_empty in
  let todo = Queue.create () in

  let schedule_predecessors v = ns_iter (fun w -> if includeNode w then (
                                                    message 3 (fun _ -> "    Scheduling node " ^ string_of_int w ^
                                                                        " for attractor check\n");
                                                    Queue.add w todo)
                                                    )
                                          (pg_get_predecessors game v)
  in

  let inattr v = ns_elem v !attr || ((not include_region) && ns_elem v region) in

  ns_iter (fun v -> if include_region then attr := ns_add v !attr;
			 schedule_predecessors v) region;

  while not (Queue.is_empty todo) do
    let v = Queue.take todo in
    if not (ns_elem v !attr)
    then let pl' = pg_get_owner game v in
	 let ws = pg_get_successors game v in
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
  !attr;;


let attr_closure_inplace game strategy player region =
	attr_closure_inplace' game strategy player region true (fun _ -> true) true;;


let attractor_closure_inplace_sol_strat game deltafilter sol strat pl0 pl1 =
	let sol0 = attr_closure_inplace' game strat 0 pl0 true (fun v -> not (ns_elem v pl1)) true in
	let sol1 = attr_closure_inplace' game strat 1 pl1 true (fun v -> not (ns_elem v pl0)) true in
	ns_iter (fun q -> sol.(q) <- 0) sol0;
	ns_iter (fun q -> sol.(q) <- 1) sol1;
	(sol0, sol1);;



(**************************************************************
 * Dominion Functions                                         *
 **************************************************************)

let pg_set_closed pg nodeset pl =
    ns_forall (fun q ->
		     let pl' = pg_get_owner pg q in
		     let delta = pg_get_successors pg q in                              
		     if pl = pl'
		     then ns_fold (fun r i -> r || ns_elem i nodeset) false delta
		     else ns_fold (fun r i -> r && ns_elem i nodeset) true delta
    ) nodeset;;

let pg_set_dominion solver pg nodeset pl =
	if pg_set_closed pg nodeset pl then (
        let l = ns_nodes nodeset in
        let a = Array.of_list l in
        let (sol, strat') = solver (subgame_by_list pg nodeset) in
        if ArrayUtils.forall sol (fun _ pl' -> pl' = pl)
        then (
        	let strat = Array.make (pg_size pg) (-1) in
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
    else None;;



(**************************************************************
 * Partial Parity Game                                        *
 **************************************************************)

type partial_paritygame = int * (int -> int Enumerators.enumerator) * (int -> int * int) * (int -> string option)
type partial_solution = int -> int * int option
type partial_solver = partial_paritygame -> partial_solution

(* Canonically maps a paritygame to its associated paritygame2 *)
let induce_partialparitygame (pg: paritygame) start =
	let delta i = Enumerators.of_list (ns_nodes (pg_get_successors pg i)) in
	let data i = (pg_get_priority pg i, pg_get_owner pg i) in
	let desc i = pg_get_desc pg i in
	((start, delta, data, desc): partial_paritygame);;

let induce_counting_partialparitygame (pg: paritygame) start =
	let counter = ref 0 in
	let access = Array.make (pg_size pg) false in
	let delta i =
		if not access.(i) then (
			access.(i) <- true;
			incr counter
		);
		Enumerators.of_list (ns_nodes (pg_get_successors pg i))
	in
	let data i =
		if not access.(i) then (
			access.(i) <- true;
			incr counter
		);
		(pg_get_priority pg i, pg_get_owner pg i)
	in
	let desc i =
		if not access.(i) then (
			access.(i) <- true;
			incr counter
		);
		pg_get_desc pg i
	in
		(counter, ((start, delta, data, desc): partial_paritygame));;

		
let partially_solve_dominion (pg: paritygame) (start: int) (partially_solve: partial_solver) =
	let n = pg_size pg in
	let (_, delta, data, desc) = induce_partialparitygame pg start in
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
			 	|   None -> ns_iter (fun x -> expand x f) (pg_get_successors pg i)
	in

    expand start (partially_solve (start, delta, data, desc));

	(solution, strategy);;


(*
	Takes a paritygame pg
	      and a partially solving function s
	      	taking a paritygame2
	      	       a starting node
	      	returning a map sol: int -> winner * strategy_decision option
	returning solution x strategy on the whole game
*)
let partially_solve_game (pg: paritygame) partially_solve =
	let n = pg_size pg in
	let (_, delta, data, desc) = induce_partialparitygame pg 0 in
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
			 	|   None -> ns_iter (fun x -> expand x f) (pg_get_successors pg i)
	in

    for i = 0 to n - 1 do
        if (solution.(i) > -1) || (pg_get_owner pg i < 0) then ()
        else expand i (partially_solve (i, delta', data', desc))
    done;

	(solution, strategy);;



(**************************************************************
 * Game Information                                           *
 **************************************************************)

let get_player_decision_info game =
    let hasPl0 = ref false in
    let hasPl1 = ref false in
    let n = pg_size game in
    let i = ref 0 in
    while (!i < n) && (not (!hasPl0 && !hasPl1)) do
        if (ns_size (pg_get_successors game !i) > 1) (* && (ArrayUtils.exists delta (fun _ el -> delta.(0) != el)) *)
        then (if pg_get_owner game !i = 0 then hasPl0 else hasPl1) := true;
        incr i
    done;
    (!hasPl0, !hasPl1);;


let is_single_parity_game game =
    let hasPar0 = ref false in
    let hasPar1 = ref false in
    let n = pg_size game in
    let i = ref 0 in
    while (!i < n) && (not (!hasPar0 && !hasPar1)) do
    	let pr = pg_get_priority game !i in
    	if pr >= 0
    	then (if pr mod 2 = 0 then hasPar0 else hasPar1) := true;
    	incr i
    done;
    if !hasPar0 && !hasPar1
    then None
    else Some (if !hasPar0 then 0 else 1);;


let number_of_strategies game pl m =
  let n = ref 1 in
  pg_iterate (fun v -> fun (_,p,vs,_,_) -> if !n < m && p=pl then n := !n * (ns_size vs)) game;
  min !n m


let compute_priority_reach_array game player =
    let maxprspm = (pg_max_prio_for game (1 - player)) / 2 in
    (* Dumb version (!)  *)
    let rec calc_iter (game': paritygame) maxvalues =
        let badPrio = pg_max_prio_for game' (1 - player) in
        let goodPrio = pg_max_prio_for game' player in
        if badPrio >= 0 then (
            let nodes = ref ns_empty in
            if goodPrio > badPrio then
                pg_iterate (fun i (pr, _, _, _, _) ->
                    if pr > badPrio then nodes := ns_add i !nodes
                ) game'
            else (
                let (sccs, sccindex, topology, roots): nodeset array * scc array * scc list array * scc list = strongly_connected_components game' in
                let sccs: nodeset array = sccs in
                let sccentry = Array.make (Array.length sccs) (-1) in
                let rec count_nodes r =
                	if sccentry.(r) = -1 then (
                        List.iter count_nodes topology.(r);
                        sccentry.(r) <- List.fold_left (fun a i -> a + sccentry.(i)) 0 topology.(r);
                        ns_iter (fun v ->
                        	if pg_get_priority game' v = badPrio then sccentry.(r) <- 1 + sccentry.(r)
                        ) sccs.(r)
					)
                in
                List.iter count_nodes roots;
                pg_iterate (fun i (pr, _, _, _, _) ->
                    if pr >= 0 then (maxvalues.(i)).(badPrio / 2) <- 1 + sccentry.(sccindex.(i));
                    if pr = badPrio then nodes := ns_add i !nodes
                ) game'
            );
            pg_remove_nodes game' !nodes;
            calc_iter game' maxvalues
        )
    in
    let game' = pg_copy game in
    let maxvalues = Array.make_matrix (Array.length game') (1 + maxprspm) 1 in
    calc_iter game' maxvalues;
    maxvalues;;




(**************************************************************
 * Dynamic Parity Game                                        *
 **************************************************************)

type dynamic_paritygame = (priority * player * string option) DynamicGraph.dynamic_graph

let paritygame_to_dynamic_paritygame game =
	let graph = DynamicGraph.make () in
	pg_iterate (fun i (pr, pl, _, _, desc) ->
		DynamicGraph.add_node i (pr, pl, desc) graph
	) game;
	pg_iterate (fun i (_, _, tr, _, _) ->
		ns_iter (fun j -> DynamicGraph.add_edge i j graph) tr
	) game;
	graph

let dynamic_subgame_by_strategy graph strat =
	DynamicGraph.sub_graph_by_edge_pred (fun v w ->
		strat.(v) = -1 || strat.(v) = w
	) graph

let paritygame_to_dynamic_paritygame_by_strategy game strat =
	let graph = DynamicGraph.make () in
	pg_iterate (fun i (pr, pl, _, _, desc) ->
		DynamicGraph.add_node i (pr, pl, desc) graph
	) game;
	pg_iterate (fun i (_, _, tr, _, _) ->
		if strat.(i) = -1
		then ns_iter (fun j -> DynamicGraph.add_edge i j graph) tr
		else DynamicGraph.add_edge i strat.(i) graph
	) game;
	graph


(********************************************************
 * a type and data structure for sets of game nodes     *
 ********************************************************)

module NodeSet = Set.Make(
struct
  type t = int
  let compare = compare
end);;

module NodePairSet = Set.Make(
struct
  type t = int * int
  let compare = compare
end);;


(********************************************************
 * Modal logic operations on sets of game nodes.        *
 * takes a set of nodes, a parity game and its          *
 *  transposed graph                                    *
 ********************************************************)

(* return the set of all nodes which have a successors in t *)

  
let diamond game t =
  NodeSet.fold (fun v -> fun s -> 
                 ns_fold (fun s' -> fun u ->
                                   if pg_isDefined game u then 
                                     NodeSet.add u s'
                                   else s')  
                                 s (pg_get_predecessors game v)) 
               t NodeSet.empty 
  
  
(* return the set of all nodes for which all successors are in t *)

let box game t =
  let c = diamond game t in      
  NodeSet.filter (fun v -> if pg_isDefined game v then
                             ns_fold (fun b -> fun w -> b && NodeSet.mem w t) true (pg_get_successors game v)
                           else
                             false
                 ) c 
  


(********************************************************
 * Building Parity Games                                *
 ********************************************************)

module type PGDescription = 
  sig
    type gamenode

    val compare    : gamenode -> gamenode -> int

    val owner      : gamenode -> player
    val priority   : gamenode -> priority
    val successors : gamenode -> gamenode list
    val show_node  : gamenode -> string option

    val initnodes  : unit -> gamenode list
  end;;

module type PGBuilder = 
  sig
    type gamenode
	   
    val build            : unit -> paritygame
    val build_from_node  : gamenode -> paritygame
    val build_from_nodes : gamenode list -> paritygame
  end

module Build(T: PGDescription) : (PGBuilder with type gamenode = T.gamenode ) =
  struct

    type gamenode = T.gamenode  
	   
    module Encoding = Map.Make(
      struct 
        type t = T.gamenode 
        let compare = compare 
      end);;

    let codes = ref Encoding.empty

    let next_code = ref 0


    let encode v = try
                     Encoding.find v !codes
                   with Not_found -> begin
                                       codes := Encoding.add v !next_code !codes;
                                       incr next_code;
                                       !next_code - 1
                                     end

    let build_from_nodes vlist = 
      let rec iterate acc visited = 
        function []          -> acc
               | ((v,c)::vs) -> begin
                                if NodeSet.mem c visited then
                                  iterate acc visited vs
                                else
                                  let ws = T.successors v in
                                  let ds = List.map encode ws in
                                  iterate ((c, T.owner v, T.priority v, ds, T.show_node v) :: acc) (NodeSet.add c visited) ((List.combine ws ds) @ vs)
                              end
      in
      let nodes = iterate [] NodeSet.empty (List.map (fun v -> (v,encode v)) vlist) in
      let game = pg_create (List.length nodes) in
      let rec transform = 
        function []                  -> ()
	       | ((v,o,p,ws,nm)::ns) -> pg_set_priority game v p;
					pg_set_owner game v o;
					pg_set_desc game v nm;
					List.iter (fun w -> pg_add_edge game v w) ws;
                                        transform ns
      in
      transform nodes;
      game
			
    let build_from_node v = build_from_nodes [v]

    let build _ = build_from_nodes (T.initnodes ())
  end;;


