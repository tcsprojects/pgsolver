open Basics;;
open Tcsbasedata;;
open Tcsarray;;
open Tcsset;;
open Tcsgraph;;



(**************************************************************
 * Parity Game Definitions                                    *
 **************************************************************)

type paritygame = (int * int * int array * string option) array
type solution = int array
type strategy = int array

type global_solver = (paritygame -> solution * strategy)


(**************************************************************
 * Access Functions                                           *
 **************************************************************)

let pg_create n = Array.make n (-1, -1, [||], None);;

let pg_size = Array.length;;

let pg_node_count game =
	let count = ref 0 in
	let n = pg_size game in
	for i = 0 to n - 1 do
		let (pr, _, _, _) = game.(i) in
		if pr >= 0 then incr count
	done;
	!count;;

let pg_edge_count game =
	let count = ref 0 in
	let n = pg_size game in
	for i = 0 to n - 1 do
		let (pr, _, tr, _) = game.(i) in
		if pr >= 0 then count := !count + Array.length tr
	done;
	!count;;

let pg_copy pg =
	let pg' = pg_create (pg_size pg) in
	Array.iteri (fun i -> fun (pr, pl, delta, desc) ->
		pg'.(i) <- (pr, pl, Array.copy delta, desc)
	) pg;
	pg';;

let pg_get_pr pg i = let (pr, _, _, _) = pg.(i) in pr;;

let pg_set_pr pg i pr = let (_, pl, delta, desc) = pg.(i) in pg.(i) <- (pr, pl, delta, desc);;

let pg_get_pl pg i = let (_, pl, _, _) = pg.(i) in pl;;

let pg_set_pl pg i pl = let (pr, _, delta, desc) = pg.(i) in pg.(i) <- (pr, pl, delta, desc);;

let pg_get_tr pg i = let (_, _, tr, _) = pg.(i) in tr;;

let pg_set_tr pg i tr = let (pr, pl, _, desc) = pg.(i) in pg.(i) <- (pr, pl, tr, desc);;

let pg_get_desc pg i = let (_, _, _, desc) = pg.(i) in desc;;

let pg_set_desc pg i desc = let (pr, pl, delta, _) = pg.(i) in pg.(i) <- (pr, pl, delta, desc);;

let pg_get_desc' pg i = match pg_get_desc pg i with None -> "" | Some s -> s;;

let pg_set_desc' pg i desc = pg_set_desc pg i (if desc = "" then None else Some desc);;

let pg_get_node pg i = pg.(i);;

let pg_set_node pg i pr pl delta desc = pg.(i) <- (pr, pl, delta, desc);;

let pg_find_desc pg desc = ArrayUtils.find (fun (_,_,_,desc') -> desc = desc') pg



(**************************************************************
 * Formatting Functions                                       *
 **************************************************************)

let game_to_string game =
	let n = Array.length game in
	let s = ref ("parity " ^ string_of_int (n-1) ^ ";\n") in
	for i = 0 to n - 1 do
	  let (pr, pl, delta, desc) = game.(i) in
	  if pr >= 0 && pl >= 0 && pl <= 1 then (
	  		s := !s ^ string_of_int i ^ " " ^ string_of_int pr ^ " " ^ string_of_int pl ^ " ";
            for j = 0 to (Array.length delta) - 2 do
              s := !s ^ string_of_int delta.(j) ^ ","
            done;
            if (Array.length delta) > 0 then s := !s ^ string_of_int delta.(Array.length delta - 1);
            (
             match desc with
               None -> ()
             |   Some a -> if a <> "" then s := !s ^ " \"" ^ a ^ "\""
            );
            s := !s ^ ";\n"
           )
    done;
    !s;;

let print_game game =
	let n = Array.length game in
	print_string ("parity " ^ string_of_int (n-1) ^ ";\n");
	for i = 0 to n - 1 do
	  let (pr, pl, delta, desc) = game.(i) in
	  if pr >= 0 && pl >= 0 && pl <= 1 then (
            print_int i;
            print_char ' ';
            print_int pr;
            print_char ' ';
            print_int pl;
            print_char ' ';
            for j = 0 to (Array.length delta) - 2 do
              print_string ((string_of_int delta.(j)) ^ ",")
            done;
            if (Array.length delta) > 0 then print_int delta.((Array.length delta) - 1) else ();
            (
             match desc with
               None -> () (* print_string (" \"" ^ string_of_int i ^ "\"") *)
             |   Some s -> if s <> "" then print_string (" \"" ^ s ^ "\"")
            );
            print_char ';';
            print_newline ()
           )
        done;;

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

  for i = 0 to (Array.length game)-1 do
    let (p,pl,succs,ann) = game.(i) in

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

          for j=0 to (Array.length succs)-1 do
            let color2 = try
                           if pl = 1 - solution.(i) || succs.(j) = strategy.(i) then color else "black"
                         with _ -> "black"
            in
            output_string h (name ^ " -> " ^ encode succs.(j) ^ " [ color=\"" ^ color2 ^ "\" ];\n" )
          done)
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
                             (Array.to_list (Array.mapi (fun i -> fun (p,pl,ws,_) ->
                                              if p <> -1 then string_of_int i ^ ":" ^ string_of_int p ^ "," ^
                                                              string_of_int pl ^ ",{" ^
                                                              String.concat "," (List.map string_of_int
                                                                                          (Array.to_list ws))
                                                              ^ "}"
                                                         else "") gm)))
  ^ "]"



(**************************************************************
 * Parsing Functions                                          *
 **************************************************************)
 
let parse_parity_game ch =
	Array.map (fun (a,b,c,d) -> (a,b,c,(if d = "" then None else Some d))) (Tcsgameparser.parse_explicit_pg ch)

 
 
 
(**************************************************************
 * Node Orderings                                             *
 **************************************************************)

type pg_ordering      = int * int * int * int array -> int * int * int * int array -> int

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
	for i = 1 to n - 1 do
		let (prm, plm, trm, _) = pg.(!m) in
		let (pri, pli, tri, _) = pg.(i) in
		if o (!m, prm, plm, trm) (i, pri, pli, tri) < 0
		then m := i
	done;
	!m;;

let pg_min pg o = pg_max pg (fun x y -> - (o x y));;

let pg_max_prio_node pg = pg_max pg ord_prio;;

let pg_max_rew_node_for pg pl = pg_max pg (ord_rew_for pl);;

let pg_max_prio pg = pg_get_pr pg (pg_max_prio_node pg);;

let pg_min_prio pg = pg_get_pr pg (pg_min pg ord_prio);;

let pg_max_prio_for pg player =
	let pr = pg_get_pr pg (pg_max_rew_node_for pg player) in
	if pr mod 2 = player then pr else -1;;

let pg_get_index pg = pg_max_prio pg - pg_min_prio pg + 1;;

let pg_prio_nodes pg p =
	let l = ref [] in
	Array.iteri (fun i (pr, _, _, _) ->
		if pr = p then l := i::!l
	) pg;
	!l


(**************************************************************
 * Inplace Modifications                                      *
 **************************************************************)

let pg_add_successor game v w =
      let (p,pl,succs,ann) = game.(v) in
      game.(v) <- (p,pl,Array.append [|w|] succs,ann);;

let pg_add_successors game v l =
       let (p,pl,succs,ann) = game.(v) in
       game.(v) <- (p,pl,Array.append l succs,ann);;

let pg_remove_nodes game nodes =
  List.iter (fun v -> game.(v) <- (-1,-1,[||],None)) nodes;
  for v=0 to (Array.length game)-1 do
    let (p,pl,ws,ann) = game.(v) in
    game.(v) <- (p,pl,Array.of_list (List.filter (fun w -> let (p,_,_,_) = game.(w) in
                                                           p <> -1)
                                                 (Array.to_list ws)),ann);
  done

let pg_remove_edges game edges =
    List.iter (fun (v, w) ->
        let tr = pg_get_tr game v in
        let n = Array.length tr in
        try
        	let i = ArrayUtils.index_of tr w in
            let tr' = Array.sub tr 0 (n - 1) in
            if i < n - 1
            then tr'.(i) <- tr.(n - 1);
            pg_set_tr game v tr'
        with
        	Not_found -> ()
    ) edges;;



(**************************************************************
 * Node Collect Functions                                     *
 **************************************************************)

let collect_nodes game pred =
    let l = ref [] in
    for i = 0 to Array.length game - 1 do
    	let (pr, _, _, _) = game.(i) in
    	if (pr >= 0) && (pred i game.(i)) then l := i::!l
    done;
    !l;;

let collect_nodes_by_prio game pred =
	collect_nodes game (fun _ (pr, _, _, _) -> pred pr);;

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

let subgame_by_edge_pred game pred =
	let n = Array.length game in
	let g = Array.make n (-1, -1, [||], None) in
	for i = 0 to n - 1 do
		let (pr, pl, dl, desc) = game.(i) in
		g.(i) <- (pr, pl, Array.of_list (List.filter (pred i) (Array.to_list dl)), desc)
	done;
	g;;

let subgame_by_node_pred game pred =
	let n = Array.length game in
	let g = Array.make n (-1, -1, [||], None) in
	for i = 0 to n - 1 do
		let (pr, pl, dl, desc) = game.(i) in
		if (pred i)
		then g.(i) <- (pr, pl, Array.of_list (List.filter pred (Array.to_list dl)), desc)
	done;
	g;;

let subgame_by_strat game strat = subgame_by_edge_pred game (fun x y -> strat.(x) < 0 || strat.(x) = y);;

let subgame_by_strat_pl game strat pl =
	let n = Array.length game in
	let g = Array.make n (-1, -1, [||], None) in
	for i = 0 to n - 1 do
		let (pr, pl', dl, desc) = game.(i) in
		if pl = pl' then g.(i) <- (pr, pl', [|strat.(i)|], desc)
		else g.(i) <- game.(i)
	done;
	g;;

let subgame_by_list game li =
    let n = List.length li in
    let g = Array.make n (-1, -1, [||], None) in
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
        i := !i + 1
    ) li;
    let i = ref 0 in
    List.iter (fun arri ->
        let (_, _, delta, desc) = game.(arri) in
        let (pr, pl, _, _) = g.(!i) in
        game.(arri) <- (pr, pl, delta, desc);
        i := !i + 1
    ) li;
    g;;

let subgame_and_subgraph_by_list game tgraph li =
    let n = List.length li in
    let g = Array.make n (-1, -1, [||], None) in
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



(**************************************************************
 * Solution / Strategy Update Functions                       *
 **************************************************************)

(* result := sol o perm *)
let permute_solution perm sol =
	let n = Array.length sol in
	let sol' = Array.make n (-1) in

    for i = 0 to n - 1 do
        sol'.(i) <- sol.(perm.(i))
    done;
    sol'


(* result := perm^-1 o strat o perm *)
let permute_strategy perm perm' strat =
	let n = Array.length strat in
	let strat' = Array.make n (-1) in

    for i = 0 to n - 1 do
    	let j = strat.(perm.(i)) in
	        strat'.(i) <- if j = -1 then -1 else perm'.(j)
    done;
    strat'


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
 * Graph Transformations                                      *
 **************************************************************)

let game_to_transposed_graph game =
  let l = Array.length game in
  let g = Array.make l [] in
  Array.iteri (fun i -> fun (_,_,ws,_) -> Array.iter (fun w -> g.(w) <- i::g.(w)) ws) game;
  g


let transposed_graph_remove_nodes game graph nodes =
	let affected = ref TreeSet.empty_def in
	List.iter (fun v ->
		graph.(v) <- [-1];
		Array.iter (fun w -> affected := TreeSet.add w !affected) (pg_get_tr game v)
	) nodes;
	TreeSet.iter (fun v ->
		if not (graph.(v) = [-1]) then
		graph.(v) <- List.filter (fun w -> not (graph.(w) = [-1])) graph.(v)
	) !affected;
	List.iter (fun v -> graph.(v) <- []) nodes;;

	
let pg_with_graph_remove_nodes game graph nodes =
	let check_succ = ref TreeSet.empty_def in
	let check_pred = ref TreeSet.empty_def in
	List.iter (fun v ->
		let (_, _, tr, _) = game.(v) in
		Array.iter (fun w -> check_pred := TreeSet.add w !check_pred) tr;
		game.(v) <- (-1, -1, [||], None);
		List.iter (fun w -> check_succ := TreeSet.add w !check_succ) graph.(v);
		graph.(v) <- [];
	) nodes;
	TreeSet.iter (fun v ->
		if pg_get_pr game v >= 0
		then graph.(v) <- List.filter (fun w -> pg_get_pr game w >= 0) graph.(v)
	) !check_pred;
	TreeSet.iter (fun v ->
		if pg_get_pr game v >= 0
		then pg_set_tr game v (ArrayUtils.filter (fun w -> pg_get_pr game w >= 0) (pg_get_tr game v))
	) !check_succ;;
	

let game_to_graph game =
  let l = Array.length game in
  let g = Array.make l [] in
  Array.iteri (fun i -> fun (_,_,ws,_) -> Array.iter (fun w -> g.(i) <- w::g.(i)) ws) game;
  g

let transposed_graph_remove_edges graph edges =
    List.iter (fun (v, w) ->
        graph.(w) <- List.filter (fun u -> not (u = v)) graph.(w)
    ) edges;;



(**************************************************************
 * Decomposition Functions                                    *
 **************************************************************)

let strongly_connected_components' game tgraph =
  let l = Array.length game in
  let dfsnum = Array.make l (-1) in
  let index = Array.make l (-1) in

  let todo = ref [] in
  for i=1 to l do
    let (p,_,_,_) = game.(l-i) in
    if p > (-1) then todo := (l-i) :: !todo
  done;

  let n = ref 0 in
  let visited = Array.make l false in

  (* Reason for stack overflow in large sccs...*)
  (*
  let rec dfs v =
    if not visited.(v)
    then (visited.(v) <- true;
          let (_,_,ws,_) = game.(v) in
          Array.iter (fun w -> dfs w) ws;
          dfsnum.(v) <- !n;
          index.(!n) <- v;
          incr n)
  in
  *)

  let dfs v =
  	let st = Stack.create () in
  	Stack.push v st;
  	while not (Stack.is_empty st) do
  		let u = Stack.pop st in
  		let pushed = ref false in
  		if not visited.(u) then (
  			visited.(u) <- true;
  			Array.iter (fun w ->
  				if not visited.(w) then (
  					if not !pushed then (
  						Stack.push u st;
  						pushed := true
  					);
  					Stack.push w st
  				)
  			) (pg_get_tr game u)
  		);
  		if (not !pushed) && (dfsnum.(u) < 0) then (
  			dfsnum.(u) <- !n;
  			index.(!n) <- u;
  			incr n
  		)
  	done
  in


  for i=0 to l-1 do
    let (p,_,_,_) = game.(i) in
    if not visited.(i) && p >= 0 then dfs i;
  done;

  decr n;

  for i=0 to l-1 do
    visited.(i) <- false
  done;

  let sccs = DynArray.create [] in
  let topology = DynArray.create TreeSet.empty_def in
  let scc_index = Array.make l (-1) in
  let next_index = ref 0 in
  let roots = ref TreeSet.empty_def in
  let is_root = ref true in

  while !n >= 0 do
    DynArray.insert topology !next_index TreeSet.empty_def;
    is_root := true;
    todo := [index.(!n)];
    let scc = ref [] in

    while !todo <> [] do
      let v = List.hd !todo in
      todo := List.tl !todo;

      if not visited.(v) && dfsnum.(v) >= 0
      then (visited.(v) <- true;
            scc := v :: !scc;
            let succs = List.sort (fun x -> fun y -> (-1) * (compare dfsnum.(x) dfsnum.(y))) tgraph.(v) in
            todo := succs @ !todo;
            List.iter (fun w -> let c = scc_index.(w) in
                                if c > -1
                                then (DynArray.set topology c (TreeSet.add !next_index (DynArray.get topology c));
                                      is_root := false))
                       succs)
    done;
    DynArray.insert sccs !next_index !scc;
    if !is_root then roots := TreeSet.add !next_index !roots;
    List.iter (fun v -> scc_index.(v) <- !next_index) !scc;
    incr next_index;

    while !n >= 0 && visited.(index.(!n)) do
      decr n
    done
  done;
  (DynArray.to_array sccs,
   scc_index,
   DynArray.to_array (DynArray.map [] (fun s -> TreeSet.fold (fun x -> fun l -> x::l) s []) topology),
   TreeSet.fold (fun x -> fun l -> x::l) !roots []);;

let strongly_connected_components game =
	strongly_connected_components' game (game_to_transposed_graph game)

let sccs_compute_leafs roots topology =
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
			List.iter (fun v -> let (_, _, delta, _) = game.(v) in
				Array.iter (fun w ->
					if sccindex.(w) != r
					then temp.(sccindex.(w)) <- (v, w)::temp.(sccindex.(w))
				) delta
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
    s := "," ^ string_of_int (l-i) ^ ":{" ^ String.concat "," (List.map string_of_int (Array.get sccs (l-i))) ^ "}" ^ !s
  done;
  if l > 0 then s := "0:{" ^ String.concat "," (List.map string_of_int (Array.get sccs 0)) ^ "}" ^ !s;
  "{" ^ !s;;



(**************************************************************
 * Attractor Closure                                          *
 **************************************************************)

let attr_closure_inplace' game strategy player region include_region tgraph deltafilter overwrite_strat =
  let message _ _ = () in
  let l = Array.length game in
  let attr = Array.make l false in
  let todo = Queue.create () in

  let schedule_predecessors v = List.iter (fun w -> if deltafilter w then (
                                                    message 3 (fun _ -> "    Scheduling node " ^ string_of_int w ^
                                                                        " for attractor check\n");
                                                    Queue.add w todo)
                                                    )
                                          tgraph.(v)
  in

  let inattr v = attr.(v) || ((not include_region) && TreeSet.mem v region) in

  TreeSet.iter (fun v -> if include_region then attr.(v) <- true;
                      schedule_predecessors v) region;

  while not (Queue.is_empty todo) do
    let v = Queue.take todo in
    if not (attr.(v))
    then let (_,pl',ws,_) = game.(v) in
         if pl'=player
         then let w = Array.fold_left (fun b -> fun w -> if (not (deltafilter w)) || (b > -1 || not (inattr w)) then b else w) (-1) ws in
              if w > -1 then (message 3 (fun _ -> "    Node " ^ string_of_int v ^ " is in the attractor because of " ^
                                         string_of_int v ^ "->" ^ string_of_int w ^ "\n");
                              attr.(v) <- true;
                              if overwrite_strat || strategy.(v) < 0
                              then strategy.(v) <- w;
                              schedule_predecessors v)
              else message 3 (fun _ -> "    Node " ^ string_of_int v ^ " is not (yet) found to be in the attractor\n")
         else if Array.fold_left (fun b -> fun w -> b && (inattr w)) true ws
              then (message 3 (fun _ -> "    Node " ^ string_of_int v ^ " is in the attractor because all successors are so");
                    attr.(v) <- true;
                    schedule_predecessors v)
              else message 3 (fun _ -> "    Node " ^ string_of_int v ^ " is not (yet) found to be in the attractor\n")
  done;
  let w = ref [] in
  for i=1 to l do
    if attr.(l-i) then w := (l-i) :: !w
  done;
  !w;;


let attr_closure_inplace game strategy player region =
	attr_closure_inplace' game strategy player (TreeSet.of_list_def region) true
	                     (game_to_transposed_graph game) (fun _ -> true) true;;


let attractor_closure_inplace_sol_strat game transp deltafilter sol strat pl0 pl1 =
	let sol0 = attr_closure_inplace' game strat 0 pl0 true transp (fun v -> not (TreeSet.mem v pl1)) true in
	let sol1 = attr_closure_inplace' game strat 1 pl1 true transp (fun v -> not (TreeSet.mem v pl0)) true in
	List.iter (fun q -> sol.(q) <- 0) sol0;
	List.iter (fun q -> sol.(q) <- 1) sol1;
	(sol0, sol1);;



(**************************************************************
 * Dominion Functions                                         *
 **************************************************************)

let pg_set_closed pg nodeset pl =
    TreeSet.for_all (fun q -> let (_, pl', delta, _) = pg.(q) in
        if pl = pl'
        then Array.fold_left (fun r i -> r || TreeSet.mem i nodeset) false delta
        else Array.fold_left (fun r i -> r && TreeSet.mem i nodeset) true delta
    ) nodeset;;

let pg_set_dominion solver pg nodeset pl =
	if pg_set_closed pg nodeset pl then (
        let l = TreeSet.elements nodeset in
        let a = Array.of_list l in
        let (sol, strat') = solver (subgame_by_list pg l) in
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
	let delta i = let (_, _, delta, _) = pg.(i) in
		Enumerators.of_array delta
	in
	let data i = let (pr, pl, _, _) = pg.(i) in (pr, pl)
	in
	let desc i = let (_, _, _, desc) = pg.(i) in desc
	in
		((start, delta, data, desc): partial_paritygame);;

let induce_counting_partialparitygame (pg: paritygame) start =
	let counter = ref 0 in
	let access = Array.make (Array.length pg) false in
	let delta i =
		if not access.(i) then (
			access.(i) <- true;
			incr counter
		);
		let (_, _, delta, _) = pg.(i) in
		Enumerators.of_array delta
	in
	let data i =
		if not access.(i) then (
			access.(i) <- true;
			incr counter
		);
		let (pr, pl, _, _) = pg.(i) in
		(pr, pl)
	in
	let desc i =
		if not access.(i) then (
			access.(i) <- true;
			incr counter
		);
		let (_, _, _, desc) = pg.(i) in
		desc
	in
		(counter, ((start, delta, data, desc): partial_paritygame));;

		
let partially_solve_dominion (pg: paritygame) (start: int) (partially_solve: partial_solver) =
	let n = Array.length pg in
	let (_, delta, data, desc) = induce_partialparitygame pg start in
	let solution = Array.make n (-1) in
	let strategy = Array.make n (-1) in

	let rec expand i f =
		if solution.(i) > -1 then ()
		else let (winner, strat) = f i in
			 let (pl, _, delta, desc) = pg.(i) in
			 	solution.(i) <- winner;
			 	match strat with
			 		Some j -> (
			 			strategy.(i) <- j;
			 			expand j f
			 		)
			 	|   None -> (
			 			for k = 0 to Array.length delta - 1 do
							expand delta.(k) f
			     		done
			     	)
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
	let n = Array.length pg in
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
			 let (pl, _, delta, desc) = pg.(i) in
			 	solution.(i) <- winner;
			 	match strat with
			 		Some j -> (
			 			strategy.(i) <- j;
			 			expand j f
			 		)
			 	|   None -> (
			 			for k = 0 to Array.length delta - 1 do
							expand delta.(k) f
			     		done
			     	)
	in

    for i = 0 to n - 1 do
        let (_, pl, _, _) = pg.(i) in
            if (solution.(i) > -1) || (pl < 0) then ()
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
        let (_, pl, delta, _) = game.(!i) in
        if (Array.length delta > 1) (* && (ArrayUtils.exists delta (fun _ el -> delta.(0) != el)) *)
        then (if pl = 0 then hasPl0 else hasPl1) := true;
        incr i
    done;
    (!hasPl0, !hasPl1);;


let is_single_parity_game game =
    let hasPar0 = ref false in
    let hasPar1 = ref false in
    let n = pg_size game in
    let i = ref 0 in
    while (!i < n) && (not (!hasPar0 && !hasPar1)) do
    	let pr = pg_get_pr game !i in
    	if pr >= 0
    	then (if pr mod 2 = 0 then hasPar0 else hasPar1) := true;
    	incr i
    done;
    if !hasPar0 && !hasPar1
    then None
    else Some (if !hasPar0 then 0 else 1);;


let number_of_strategies game pl m =
  let n = ref 1 in
  let i = ref 0 in
  let l = Array.length game in

  while !i < l && !n < m do
    let (_,pl',ws,_) = game.(!i) in
    if pl=pl' then n := !n * (Array.length ws);
    incr i
  done;
  min !n m



let compute_priority_reach_array game player =
    let maxprspm = (pg_max_prio_for game (1 - player)) / 2 in
    (* Dumb version (!)  *)
    let rec calc_iter game' maxvalues =
        let badPrio = pg_max_prio_for game' (1 - player) in
        let goodPrio = pg_max_prio_for game' player in
        if badPrio >= 0 then (
            let nodes = ref [] in
            if goodPrio > badPrio then
                Array.iteri (fun i (pr, _, _, _) ->
                    if pr > badPrio then nodes := i::!nodes
                ) game'
            else (
                let (sccs, sccindex, topology, roots) = strongly_connected_components game' in
                let sccentry = Array.make (Array.length sccs) (-1) in
                let rec count_nodes r =
                	if sccentry.(r) = -1 then (
                        List.iter count_nodes topology.(r);
                        sccentry.(r) <- List.fold_left (fun a i -> a + sccentry.(i)) 0 topology.(r);
                        List.iter (fun v ->
                        	if pg_get_pr game' v = badPrio then sccentry.(r) <- 1 + sccentry.(r)
                        ) sccs.(r)
					)
                in
                List.iter count_nodes roots;
                Array.iteri (fun i (pr, _ ,_, _) ->
                    if pr >= 0 then (maxvalues.(i)).(badPrio / 2) <- 1 + sccentry.(sccindex.(i));
                    if pr = badPrio then nodes := i::!nodes
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

(*
module Set = Intervaltree.IntSet ;;
module Set = IntSet ;;

let compute_priority_reach_array' game =
  let m = Array.length game in
  let reaches = Array.make m Set.empty in

  let todo = Queue.create () in

  for i=0 to m-1 do
    reaches.(i) <- Set.singleton i;
    Queue.add i todo
  done;

  let tgame = game_to_transposed_graph game in

  while not (Queue.is_empty todo) do
    let v = Queue.take todo in
    let (pv,_,_,_) = game.(v) in

    List.iter (fun w -> Set.reset_notice ();
                        reaches.(w) <- Set.union
                                       reaches.(w)
                                       (Set.filter (fun u -> let (pu,_,_,_) = game.(u) in
                                                             pu >= pv) reaches.(v));
                        if !Set.insert_notice then (
                           Queue.add w todo
                        )
              )
              tgame.(v);
  done;

  let mp = pg_max_prio game in
  let reach = Array.make_matrix m (1 + mp) 0 in
  for v = 0 to m-1 do
    Set.iter (fun w -> let p = pg_get_pr game w in
                       reach.(v).(p) <- reach.(v).(p) + 1)
             reaches.(v)
  done;
  reach


*)



(**************************************************************
 * Symbolic Parity Game                                       *
 **************************************************************)

type dynamic_paritygame = (int * int * string option) DynamicGraph.dynamic_graph

module SymbolicParityGame = struct

	open Tcsset

	type 'a symbolic_paritygame = ('a, int) Hashtbl.t * dynamic_paritygame

	let create_new x = (Hashtbl.create 10, DynamicGraph.make ())

	let to_paritygame (ht, gr) =
		let pg = pg_create (Hashtbl.length ht) in
		Hashtbl.iter (fun _ ind ->
			let (pr, pl, desc) = DynamicGraph.get_node_data ind gr in
			let succs = TreeSet.elements (DynamicGraph.get_node_succ ind gr) in
			pg.(ind) <- (pr, pl, Array.of_list succs, desc)
		) ht;
		pg

	let internal_add (ht, gr) symb pr pl desc override =
		if Hashtbl.mem ht symb
		then let ind = Hashtbl.find ht symb in
			 if override then DynamicGraph.set_node_data ind (pr, pl, desc) gr;
			 ind
		else let ind = Hashtbl.length ht in
			 Hashtbl.add ht symb ind;
			 DynamicGraph.add_node ind (pr, pl, desc) gr;
			 ind
   
    let touch_node (ht, gr) symb =
   		let _ = internal_add (ht, gr) symb (-1) (-1) None false in ()

	let add_node (ht, gr) symb pr pl tr desc =
		let ind = internal_add (ht, gr) symb pr pl desc true in
		Array.iter (fun symb' ->
			let ind' = internal_add (ht, gr) symb' (-1) (-1) None false in
			DynamicGraph.add_edge ind ind' gr
		) tr

end;;



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

let diamond_with_transposed_graph t game tg =
  NodeSet.fold (fun v -> fun s -> 
                 List.fold_left (fun s' -> fun u -> 
                                   let (pr,_,_,_) = game.(u) in
                                   if (pr >=0) then 
                                     NodeSet.add u s'
                                   else s')  
                                 s tg.(v)) 
               t NodeSet.empty 


(* return the set of all nodes for which all successors are in t *)

let box_with_transposed_graph t game tg =
  let c = diamond_with_transposed_graph t game tg in      
  NodeSet.filter (fun v -> let (pr, _, successors, _) = game.(v) in
                          if pr >= 0 then
                            Array.fold_left (fun b -> fun w -> b && NodeSet.mem w t) true successors
                          else
                            false
                 ) c 



(********************************************************
 * Building Parity Games                                *
 ********************************************************)

module type GameNode = 
  sig
    type node

    val compare    : node -> node -> int

    val owner      : node -> int
    val priority   : node -> int
    val successors : node -> node list
    val name       : node -> string option
  end;;

module Build = functor (T: GameNode) ->
  struct

    module Encoding = Map.Make(
      struct 
        type t = T.node 
        let compare = T.compare 
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

    let build_from_node v = 
      let rec iterate acc visited = 
        function []          -> acc
               | ((v,c)::vs) -> begin
                                  if NodeSet.mem c visited then
                                    iterate acc visited vs
                                  else
                                    let ws = T.successors v in
                                    let ds = List.map encode ws in
                                    iterate ((c, T.owner v, T.priority v, ds, T.name v) :: acc) (NodeSet.add c visited) ((List.combine ws ds) @ vs)
                                  end
      in
      let nodes = iterate [] NodeSet.empty [(v,encode v)] in
      let game = pg_create (List.length nodes) in
      let rec transform = 
        function []                  -> ()
	       | ((v,o,p,ws,nm)::ns) -> pg_set_node game v p o (Array.of_list ws) nm;
                                        transform ns
      in
      transform nodes;
      game
  end;;
