open Basics;;
open Tcslist;;
open Tcsset;;
open Tcsarray;;
open Tcsqueue;;
open Tcsbasedata;;
open Paritygame;;
open Arrayparitygame;;



(**************************************************************
 * Global Preprocessing                                       *
 **************************************************************)

let remove_useless_self_cycles_inplace game =
  let l = ref [] in
  game#iterate (fun v -> fun (pr,pl,succs,_,_) -> if not (prio_good_for_player pr pl) && ns_size succs > 1 then
						  if ns_elem v succs then
						    begin
						      game#del_edge v v;
	                                              l := v :: !l
						    end
	     );
  !l
	     



(**************************************************************
 * Local Preprocessing                                        *
 **************************************************************)

(* Compacts the priorities inplace;
   returns a map newprio -> oldprio *)
let compact_prio_inplace (pg: paritygame) real_alternation =
  let n = pg#size in
  let getpr i = pg#get_priority i in
  let prios = Array.init n (fun i -> i) in
  Array.sort (fun i j -> (getpr i) - (getpr j)) prios;
  let rec count index i last =
    if i >= n
    then index
    else let pr = getpr prios.(i) in
	 if (pr < 0) || (pr = last)
	 then count index (i + 1) last
	 else if last < 0
	 then count (pr mod 2) (i + 1) pr
	 else if real_alternation && (pr mod 2 = last mod 2)
	 then count index (i + 1) pr
	 else count (index + 1 + (1 + pr - last) mod 2) (i + 1) pr
  in
  let index = 1 + count 0 0 (-1) in
  let compact = Array.make index (-1) in
  let rec buildmap newprio i last =
    if i < n then (
      let (j, pr) = (prios.(i), getpr prios.(i)) in
      if (pr < 0)
      then buildmap newprio (i + 1) last
      else if (pr = last)
      then (
	pg#set_priority j newprio;
        buildmap newprio (i + 1) last
      )
      else let pr' = if last < 0
                     then pr mod 2
                     else if real_alternation && (pr mod 2 = last mod 2)
                     then newprio
                     else newprio + 1 + (1 + pr - last) mod 2 in
           (
             compact.(pr') <- pr;
	     pg#set_priority j pr';
             buildmap pr' (i + 1) pr
           )
    )
  in
  buildmap 0 0 (-1);
  compact;;
  

let priority_propagation_inplace pg =
	let cmp x y = compare (pg#get_priority x) (pg#get_priority y) in
	let n = pg#size in
	let wasminpred = Array.make n (-1, -1) in
	let wasminsucc = Array.make n (-1, -1) in
	let updint interval entry pr =
		let (lo, hi) = interval.(entry) in
		if lo = -1
		then interval.(entry) <- (pr, pr)
		else if pr < lo
		then interval.(entry) <- (pr, hi)
		else if pr > hi
		then interval.(entry) <- (lo, pr)
	in
	let qu = SingleOccQueue.create () in
	pg#iterate (fun i -> fun (pr,_,_,_,_) -> if pr >= 0 then SingleOccQueue.add i qu);
	while (not (SingleOccQueue.is_empty qu)) do
		let i = SingleOccQueue.take qu in
		let pr = pg#get_priority i in
		let succs = pg#get_successors i in
		let preds = pg#get_predecessors i in
		let minpredpr = if ns_isEmpty preds then -1
		                else pg#get_priority (ListUtils.min_elt cmp (ns_nodes preds)) in
		let minsuccpr = if ns_isEmpty succs then -1
		                else pg#get_priority (ListUtils.min_elt cmp (ns_nodes succs)) in
		let maxpr = max minpredpr minsuccpr in
		let newpr = max maxpr pr in
		ns_iter (fun j -> if pg#get_priority j <= newpr then updint wasminpred j (newpr + 1)) preds;
		ns_iter (fun j -> if pg#get_priority j <= newpr then updint wasminsucc j (newpr + 1)) succs;
		if pr < maxpr then (
			pg#set_priority i maxpr;
			let (lopred, hipred) = wasminpred.(i) in
			let (losucc, hisucc) = wasminsucc.(i) in
			if (lopred >= 0) && (lopred <= maxpr)
			then ns_iter (fun j -> if pg#get_priority j < maxpr then SingleOccQueue.add j qu) succs;
			if (losucc >= 0) && (losucc <= maxpr)
			then ns_iter (fun j -> if pg#get_priority j < maxpr then SingleOccQueue.add j qu) preds;
			if (maxpr >= hipred)
			then wasminpred.(i) <- (-1, -1)
			else if (maxpr >= lopred) && (maxpr < hipred)
			then wasminpred.(i) <- (maxpr + 1, hipred);
			if (maxpr >= hisucc)
			then wasminsucc.(i) <- (-1, -1)
			else if (maxpr >= losucc) && (maxpr < hisucc)
			then wasminsucc.(i) <- (maxpr + 1, hisucc)
		)
	done;;


let anti_propagation_inplace pg =
	let getpr = pg#get_priority in

	let rec reach iterfunc v pr doneset = function [] -> false |
		u::r -> let l = ref r in
			let d = ref doneset in
			let found = ref false in
			iterfunc (fun w ->
				if w = v then found := true
				else if (getpr w < pr) && (not (TreeSet.mem w !d)) then (
					d := TreeSet.add w !d;
					l := w::!l
				)
			) u;
			!found || (reach iterfunc v pr !d !l)
	in

	pg#iterate (fun v -> fun (pr,_,_,_,_) -> 
			     if pr > 0 &&
				  not ((reach (fun f u -> ns_iter f (pg#get_successors u)) v (getpr v) TreeSet.empty_def [v]) ||
					 (reach (fun f u -> ns_iter f (pg#get_predecessors u)) v (getpr v) TreeSet.empty_def [v]))
			     then pg#set_priority v 0
		   );;



(**************************************************************
 * Game Transformations                                       *
 **************************************************************)

let single_scc_transformation pg =
	let (sccs, sccindex, topology, roots) = strongly_connected_components pg in
	if Array.length sccs <= 1 then pg else (
		let leaves = sccs_compute_leaves roots topology in
		let (has0, has1) = List.fold_left (fun (h0, h1) i -> if pg#get_owner (ns_first sccs.(i)) = plr_Even then (true, h1) else (h0, true))
						  (false, false)
						  leaves
		in
		let n = pg#size in
		let pg' = new array_pg (n + (if has0 && has1 then 4 else 2)) in
		let maxpr = ref 0 in
		pg#iterate (fun i -> fun (pr,ow,succs,_,desc) -> 
				     pg'#set_priority i pr;
				     pg'#set_owner i ow;
				     pg'#set_desc i desc;
				     ns_iter (fun w -> pg'#add_edge i w) succs;
				     if pr > !maxpr then maxpr := pr
			   );
		let prio0 = !maxpr + 2 - !maxpr mod 2 in
		let prio1 = !maxpr + 1 + !maxpr mod 2 in
		let ptr0 = ref n in
		let ptr1 = ref (if has0 then n + 2 else n) in
		let roots = List.filter (fun v -> pg#get_priority v >= 0) (List.map (fun i -> ns_first sccs.(i)) roots) in
		if has0 then (
		  let v = !ptr0 in
		  let v' = v + 1 in
		  pg'#set_priority v 0;
		  pg'#set_owner v plr_Odd;
		  pg'#set_desc v None;
		  List.iter (fun r -> pg'#add_edge v r) roots;
		  pg'#add_edge v v';

		  pg'#set_priority v' prio1;
		  pg'#set_owner v' plr_Even;
		  pg'#add_edge v' v;
		  pg'#set_desc v' None
		);
		if has1 then (
		  let v = !ptr1 in
		  let v' = v + 1 in
		  pg'#set_priority v 0;
		  pg'#set_owner v plr_Even;
		  pg'#set_desc v None;
		  List.iter (fun r -> pg'#add_edge v r) roots;
		  pg'#add_edge v v';

		  pg'#set_priority v' prio0;
		  pg'#set_owner v' plr_Odd;
		  pg'#add_edge v' v;
		  pg'#set_desc v' None
		);
		List.iter (fun i ->
			let v = ns_first sccs.(i) in
			let pl = pg'#get_owner v in
			pg'#add_edge v (if pl = plr_Even then !ptr0 else !ptr1) 
		) leaves;
		pg'
	)



let anti_priority_compactation_transformation pg =
	let n = pg#size in
	let v = pg#get_max_prio_node in
	let maxprio = pg#get_priority v in
	let a = Array.make (maxprio + 1) false in
	let unused = ref (maxprio + 1) in
	pg#iterate (fun i -> fun (pr,_,_,_,_) -> if (pr > -1) && (not a.(pr)) then (
						   a.(pr) <- true;
						   decr unused
						 )
		   );
	if !unused = 0 then pg else (
          let m = n + !unused in
          let pg' = new array_pg m in
          pg#iterate (fun i -> fun (pr,pl,succs,_,desc) -> pg'#set_priority i pr;
							   pg'#set_owner i pl;
							   pg'#set_desc i desc;
							   if i=v then
							     pg'#add_edge i n
							   else
							     ns_iter (fun w -> pg'#add_edge i w) succs
		     );
          let pl = ref (plr_opponent (pg#get_owner v)) in
          let i = ref 0 in
          let j = ref n in
          while !j < m do
            if not a.(!i) then (
              if !j = m - 1
              then
		begin
	          pg'#set_priority !j !i;
	          pg'#set_owner !j (pg#get_owner v);
		  pg'#set_desc !j None;
		  ns_iter (fun w -> pg'#add_edge !j w) (pg#get_successors v)
		end
              else
		begin
		  pg'#set_priority !j !i;
	          pg'#set_owner !j !pl;
		  pg'#set_desc !j None;
		  pg'#add_edge !j (!j+1);
		  
        	  incr j;
        	  pl := plr_opponent !pl
		end
            );
            incr i
          done;
          pg'
	)

let cheap_escape_cycles_transformation pg keep_scc =
	let n = pg#size in
	let pg' = new array_pg (n + (if keep_scc then 5 else 4)) in
	pg#iterate (fun i -> fun (pr,pl,succs,_,desc) -> if pr >= 0 then
							   begin
							     pg'#set_priority i (pr+2);
							     pg'#set_owner i pl;
							     pg'#set_desc i desc;
							     pg'#add_edge i (if pl = plr_Even then n else n + 1);
							     ns_iter (fun w -> pg'#add_edge i w) succs
							   end
		   );
	let r = pg#get_max_prio in
	let pl = pg#get_owner 0 in
	let m = if prio_good_for_player r pl then r + 1 else r + 2 in
        let n1 = n+1 in
	let n2 = n+2 in
	let n3 = n+3 in
	let n4 = n+4 in
	
	pg'#set_priority n 1;
	pg'#set_owner n plr_Odd;
	pg'#set_desc n None;
	pg'#add_edge n n2;
	if keep_scc then pg'#add_edge n (if pl = plr_Odd then n4 else n1);
	
	pg'#set_priority n1 0;
	pg'#set_owner n1 plr_Even;
	pg'#set_desc n1 None;
	pg'#add_edge n1 n3;
	if keep_scc then pg'#add_edge n (if pl = plr_Even then n4 else n);
	
	pg'#set_priority n2 0;
	pg'#set_owner n2 plr_Even;
	pg'#set_desc n2 None;
	pg'#add_edge n2 n;

	pg'#set_priority n3 0;
	pg'#set_owner n3 plr_Odd;
	pg'#set_desc n3 None;
	pg'#add_edge n3 n1;

	if keep_scc then
	  begin
	    pg'#set_priority n4 m;
	    pg'#set_owner n4 (plr_opponent pl);
	    pg'#set_desc n4 None;
	    pg'#add_edge n4 0
	  end;
	pg'


(* Builds totality closure of a parity game inplace *)
let total_transformation_inplace (pg: paritygame) =
    pg#iterate (fun i -> fun (_,pl,succs,_,_) -> 			
			 if ns_isEmpty succs
			 then (pg#set_priority i (if pl=plr_Even then 1 else 0);
			       pg#add_edge i i)
	       )


(* Restricts the strategy of the total closure to the original game *)
let total_revertive_restriction_inplace (oldpg: paritygame) (strat: strategy) =
	oldpg#iterate (fun i -> fun (_,_,succs,_,_) -> if ns_isEmpty succs then strat.(i) <- -1)


(* Transforms a parity game into an equivalent (modulo dummy nodes) alternating parity game *)
let alternating_transformation (pg: paritygame) (total: bool) =
	let n = pg#size  in
	let counter = ref n in
	let ptr = ref (-1) in
	let altmap = Array.make n (-1, -1) in

	pg#iterate (fun i -> fun (_,pl,delta,_,_) -> if ns_isEmpty delta && total
						     then (altmap.(i) <- (!counter, !ptr);
        						   counter := !counter + 1;
        						   ptr := i)
						     else
						       ns_iter (fun j -> let pl' = pg#get_owner j in
									 if (pl = pl') && (altmap.(j) = (-1, -1))
									 then (altmap.(j) <- (!counter, !ptr);
									       counter := !counter + 1;
									       ptr := j)
							       )
							       delta
		   );
	
	let newpg = new array_pg !counter in

	pg#iterate (fun i -> fun (pr,pl,delta,_,desc) -> if ns_isEmpty delta && total
							 then
							   begin
							     newpg#set_priority i (if pl=plr_Even then 1 else 0);
							     newpg#set_owner i pl;
							     newpg#set_desc i desc;
							     newpg#add_edge i (fst altmap.(i))
							   end
							 else
							   let delta' = ns_map 
									  (fun j ->
									   let pl' = pg#get_owner j in
									   if pl = pl' then fst altmap.(j) else j)
									  delta
							   in
							   newpg#set_priority i pr;
							   newpg#set_owner i pl;
							   newpg#set_desc i desc;
							   ns_iter (fun w -> newpg#add_edge i w ) delta'
		   );
	
	let descmap = function None -> None | Some s -> Some (s ^ "'") in

	while (!ptr != -1) do
		let (ind, prev) = altmap.(!ptr) in
		let pl = pg#get_owner !ptr in
		newpg#set_priority ind 0;
		newpg#set_owner ind (plr_opponent pl);
		newpg#set_desc ind (descmap (pg#get_desc !ptr));
		newpg#add_edge ind !ptr;
		ptr := prev
	done;

	newpg;;


(* Restricts strategy and solution to the original game *)
let alternating_revertive_restriction (oldpg: paritygame)
				      (altpg: paritygame)
				      (sol: solution)
				      (strat: strategy) =
	let n = oldpg#size in
	let sol' = sol_create oldpg in
	let strat' = Array.make n (-1) in

	for i = 0 to n - 1 do
	  sol'.(i) <- sol.(i);
	  if strat.(i) < n
	  then strat'.(i) <- strat.(i)
	  else let delta = altpg#get_successors strat.(i) in
	       strat'.(i) <- ns_some delta
	done;

	(sol', strat');;

		
let partialpg_alternating_transformation (start, delta, data, format) =
	let start' =
		2 * start
	in
	let delta' v =
	  if v mod 2 = 1 then Enumerators.singleton (v - 1)
	  else let pl = snd (data (v / 2)) in
	       Enumerators.map (fun u ->
				let pl' = snd (data u) in
				if pl = pl' then 2 * u + 1 else 2 * u
			       ) (delta (v / 2))
	in
	let data' v =
		if v mod 2 = 0 then data (v / 2)
		else (0, plr_opponent (snd (data (v / 2))))
	in
	let format' v =
		if v mod 2 = 0 then format (v / 2)
		else OptionUtils.map_some (format (v / 2)) (fun s -> s ^ "'")
	in
	(start', delta', data', format')

let partialpg_alternating_revertive_restriction sol =
	(fun v ->
		let (win, strat) = sol (2 * v) in
		let strat' = OptionUtils.map_some strat (fun u -> if u < 0 then u else u / 2) in
		(win, strat')
	)

let increase_priority_occurrence game =
	let v = game#get_max_prio_node in
	let pr = game#get_priority v in
	let pl = game#get_owner v in
	let tr = game#get_successors v in
	let de = game#get_desc v in
	let pl' = ref (plr_opponent pl) in
	let pr' = if prio_good_for_player pr pl then pr + 1 else pr in
	let n = game#size in
	let game' = new array_pg (n + pr' + 1) in
	game#iterate (fun i -> fun (pr',pl',succs',_,desc') -> if i = v then
							       begin
								 game'#set_priority i pr;
								 game'#set_owner i pl;
								 game'#set_desc i de;
								 game'#add_edge i n;
								 ns_iter (fun w -> game'#add_edge i w) tr
							       end
							     else
							       begin
								 game'#set_priority i pr';
								 game'#set_owner i pl';
								 game'#set_desc i desc';
								 ns_iter (fun w -> game'#add_edge i w) succs'
							       end
		   );
	for i = 0 to pr' do
	  let i' = n+i in
	  game'#set_priority i' i;
	  game'#set_owner i' !pl';
	  game'#set_desc i' None;
	  game'#add_edge i' (if i = pr' then v else n + i + 1);
	  pl' := plr_opponent !pl'
	done;
	game'



(* prio[i] mod 2 == player[i] *)
let prio_alignment_transformation (game: paritygame) =
	let n = game#size in
	let mp = Array.init n (fun i -> i) in
	let m = ref 0 in
	let l = ref [] in
	game#iterate (fun i -> fun (pr,pl,_,_,_) -> 
			     if (pr >= 0) && (not (prio_good_for_player pr pl)) then (
			       mp.(i) <- n + !m;
			       incr m;
			       l := i::!l
			     )
		   );
	let game' = new array_pg (n + !m) in
	game#iterate (fun i -> fun (pr,pl,tr,_,desc) -> game'#set_priority i pr;
						      game'#set_desc i desc;
						      if mp.(i) = i
						      then
							begin
							  game'#set_owner i pl;
							  ns_iter (fun w -> game'#add_edge i w) tr
							end
						      else
							begin
							  game'#set_owner i (plr_opponent pl);
							  game'#add_edge i mp.(i)
							end
		   );
	let i = ref (!m + n - 1) in
	while (!l != []) do
		let j = List.hd !l in
		l := List.tl !l;
		game'#set_priority !i 0;
		game'#set_owner !i (game#get_owner j);
		game'#set_desc !i None;
		ns_iter (fun w -> game'#add_edge !i w) (game#get_successors j);
		decr i
	done;
	game'




let shift_game game k =
	let n = game#size in
	let game' = new array_pg (n + k) in
	game#iterate (fun i -> fun (pr,pl,tr,_,de) -> let ki = k+i in 
						    game'#set_priority ki pr;
						    game'#set_owner ki pl;
						    game'#set_desc ki de;
						    ns_iter (fun w -> game'#add_edge ki (k+w)) tr
		   );
	game'



let bouncing_node_transformation game =
	let l = ref [] in
	let m = ref 0 in
	let n = game#size in
	game#iterate (fun i -> fun (_,_,succs,_,_) -> if ns_elem i succs then
						      begin
							l := (i, n + !m)::!l;
							incr m
						      end
		   );
	let g = new array_pg (n + !m) in
	game#iterate (fun i -> fun (pr,pl,tr,_,de) -> g#set_priority i pr;
						    g#set_owner i pl;
						    g#set_desc i de;
						    ns_iter (fun w -> g#add_edge i w) tr
		   );
	List.iter (fun (i, k) ->
		   let pr = g#get_priority i in
		   let pl = g#get_owner i in
		   g#set_priority k pr;
		   g#set_owner k (plr_opponent pl);
		   g#set_desc k None;
		   g#del_edge k k;
		   g#add_edge k i;
	) !l;
	g


let compress_nodes game =
	let n = game#size in
	let m = ref 0 in
	for i = 0 to n - 1 do
	  if game#isDefined i then incr m
	done;
	let game' = new array_pg !m in
	let newToOld = Array.make !m (-1) in
	let oldToNew = Array.make n (-1) in
	let j = ref 0 in
	for i = 0 to !m - 1 do
		while not (game#is_defined !j) do
		  incr j
		done;
		newToOld.(i) <- !j;
		oldToNew.(!j) <- i;
		incr j
	done;
	for i = 0 to !m - 1 do
		game'#set_priority i (game#get_priority newToOld.(i));
		game'#set_owner i (game#get_owner newToOld.(i));
		game'#set_desc i (game#get_desc newToOld.(i));
		ns_iter (fun w -> game'#add_edge i oldToNew.(w)) (game#get_successors newToOld.(i))
	done;
	(game', newToOld, oldToNew);;


let sort_game_by_prio game =
  let n = game#size in
  let new_names = let a = Array.init n (fun i -> (i, game#get_priority i)) in
		  Array.sort (fun (_,p) (_,p') -> p - p') a;
		  Array.map (fun (i,_) -> i) a
  in
  let old_names = Array.make n (-1) in
  Array.iteri (fun i -> fun j -> old_names.(j) <- i) new_names;

  let game' = new array_pg n ~initFunc:(fun i -> (game#get_priority old_names.(i),
		       game#get_owner old_names.(i),
		       List.map (fun w -> new_names.(w)) (ns_nodes (game#get_successors old_names.(i))),
		       game#get_desc old_names.(i)))
  in
  (game', new_names, old_names)
  
		    
let normal_form_translation pg =
  let n = pg#size in
  let a = ref 0 in
  pg#iterate (fun _ -> fun (_,_,succs,_,_) -> let l = ns_size succs in
					      if l > 2 then a := !a + l - 2
	     );
  let game = new array_pg (n + !a) ~initFunc:(fun i ->
			       if i >= n then
				 (0, plr_Even, [], None)
			       else
				 (pg#get_priority i, pg#get_owner i, ns_nodes (pg#get_successors i), pg#get_desc i)
			      ) in
  let j = ref n in
  game#iterate (fun i -> fun (_,pl,tr,_,_) -> let l = ns_size tr in
					    if l > 2 then (
					      let c = ref i in
					      let t = ref (ns_nodes tr) in
					      for p = 0 to l - 3 do
						game#add_edge !c (List.hd !t);
						game#add_edge !c !j;
						game#set_owner !j pl;
						t := List.tl !t;
						c := !j;
						incr j;
					      done;
					      game#add_edge (!j-1) (List.hd !t);
					      t := List.tl !t;
					      game#add_edge (!j-1) (List.hd !t)
					    )
	     );
  game
    
let normal_form_revertive_translation old_game sol strat =
  let n = old_game#size in
  let rec lookup v = if v < n then v else lookup strat.(v) in
  (Array.init n (fun i -> sol.(i)),
   Array.init n (fun i -> lookup strat.(i)))
    
let uniquize_sorted_prios_inplace game =
  let pr = ref 0 in
  game#iterate (fun i -> fun (pr',_,_,_,_) -> pr := !pr + if !pr mod 2 = pr' mod 2 then 2 else 1;
					    game#set_priority i !pr)
    
let uniquize_prios_inplace game =
  let (game',_, perm) = sort_game_by_prio game in
  uniquize_sorted_prios_inplace game';
  Array.iteri (fun i j -> game#set_priority i (game'#get_priority j)) perm
	      
	      
(* turns a min-parity into a max-parity game and vice versa *)
let min_max_swap_transformation g =
  let m = g#max_prio in
  let m = m + (m mod 2) in
  let g' = g#copy in
  g#iterate (fun i -> fun (pr,_,_,_,_) -> g'#set_priority i (m - (g#get_priority i)));
  g'
	  
	  
(* Every node is accessed through one additional dummy node *)
let dummy_transformation game =
  let n = game#size in
  let m = 2*n in
  let g = new array_pg m in
  game#iterate (fun i -> fun (pr,pl,tr,_,desc) ->
		       g#set_priority i pr;
		       g#set_owner i pl;
		       g#set_desc i desc;
		       ns_iter (fun w -> g#add_edge i (w+n)) tr
	     );
  for i=n to m-1 do
    g#set_priority i 0;
    g#set_owner i (plr_opponent (game#get_owner (i-n)));
    g#set_desc i None;
    g#add_edge i (i-n)
  done;
  g
	  
let combine_games l =
  let size = List.fold_left (fun s g -> s + g#size) 0 l in
  let game = new array_pg size in
  let _ = List.fold_left (fun k g ->
			  let n = g#size in
			  g#iterate (fun i -> fun (pr,pl,tr,_,de) -> let ki = k+i in 
								      game#set_priority ki pr;
								      game#set_owner ki pl;
								      game#set_desc ki de;
								      ns_iter (fun w -> game#add_edge ki (k+w)) tr
				     );
			  k + n
			 ) 0 l
  in
  game
    
