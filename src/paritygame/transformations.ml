open Basics;;
open Tcslist;;
open Tcsset;;
open Tcsarray;;
open Tcsqueue;;
open Tcsbasedata;;
open Paritygame;;



(**************************************************************
 * Global Preprocessing                                       *
 **************************************************************)

let remove_useless_self_cycles_inplace game =
  let l = ref [] in
  pg_iterate (fun v -> fun (pr,pl,succs,_,_) -> if pr mod 2 != pl && ns_size succs > 1 then
						  if ns_elem v succs then
						    begin
						      pg_del_edge game v v;
	                                              l := v :: !l
						    end
	     ) game;
  !l
	     
(*
	let n = pg_size game in
	let l = ref [] in
	for i = 0 to n - 1 do
		if pg_get_priority game i mod 2 != pg_get_owner game i then (
		  let delta = pg_get_successors game i  in
		  let j = ns_fold (fun k -> fun w -> if w=i then k+1 else k) 0 delta in
		  if j < ns_size delta then (
                    let a = Array.make !j (-1) in
                    j := 0;
                    l := i::!l;
                    for k = 0 to Array.length delta - 1 do
                      if delta.(k) != i then (
                        a.(!j) <- delta.(k);
                        j := !j + 1
                      )
                    done;
                    pg_set_tr game i a
		  )
		)
	done;
	!l
 *)



(**************************************************************
 * Local Preprocessing                                        *
 **************************************************************)

(* Compacts the priorities inplace;
   returns a map newprio -> oldprio *)
let compact_prio_inplace (pg: paritygame) real_alternation =
  let n = pg_size pg in
  let getpr i = pg_get_priority pg i in
  let prios = Array.make n (-1) in
  for i = 0 to n - 1 do
    prios.(i) <- i
  done;
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
	pg_set_priority pg j newprio;
        buildmap newprio (i + 1) last
      )
      else let pr' = if last < 0
                     then pr mod 2
                     else if real_alternation && (pr mod 2 = last mod 2)
                     then newprio
                     else newprio + 1 + (1 + pr - last) mod 2 in
           (
             compact.(pr') <- pr;
	     pg_set_priority pg j pr';
             buildmap pr' (i + 1) pr
           )
    )
  in
  buildmap 0 0 (-1);
  compact;;
  

let priority_propagation_inplace pg =
	let cmp x y = compare (pg_get_priority pg x) (pg_get_priority pg y) in
	let n = pg_size pg in
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
	for i = 0 to n - 1 do
		let pr = pg_get_priority pg i in
		if pr >= 0 then SingleOccQueue.add i qu;
	done;
	while (not (SingleOccQueue.is_empty qu)) do
		let i = SingleOccQueue.take qu in
		let pr = pg_get_priority pg i in
		let succs = pg_get_successors pg i in
		let preds = pg_get_predecessors pg i in
		let minpredpr = if ns_isEmpty preds then -1
		                else pg_get_priority pg (ListUtils.min_elt cmp (ns_nodes preds)) in
		let minsuccpr = if ns_isEmpty succs then -1
		                else pg_get_priority pg (ListUtils.min_elt cmp (ns_nodes succs)) in
		let maxpr = max minpredpr minsuccpr in
		let newpr = max maxpr pr in
		ns_iter (fun j -> if pg_get_priority pg j <= newpr then updint wasminpred j (newpr + 1)) preds;
		ns_iter (fun j -> if pg_get_priority pg j <= newpr then updint wasminsucc j (newpr + 1)) succs;
		if pr < maxpr then (
			pg_set_priority pg i maxpr;
			let (lopred, hipred) = wasminpred.(i) in
			let (losucc, hisucc) = wasminsucc.(i) in
			if (lopred >= 0) && (lopred <= maxpr)
			then ns_iter (fun j -> if pg_get_priority pg j < maxpr then SingleOccQueue.add j qu) succs;
			if (losucc >= 0) && (losucc <= maxpr)
			then ns_iter (fun j -> if pg_get_priority pg j < maxpr then SingleOccQueue.add j qu) preds;
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
	let getpr = pg_get_priority pg in
	let n = pg_size pg in

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

	for v = 0 to n - 1 do
		if ((getpr v > 0) &&
		   (not ((reach (fun f u -> ns_iter f (pg_get_successors pg u)) v (getpr v) TreeSet.empty_def [v]) ||
                         (reach (fun f u -> ns_iter f (pg_get_predecessors pg u)) v (getpr v) TreeSet.empty_def [v]))))
		then pg_set_priority pg v 0
	done;;



(**************************************************************
 * Game Transformations                                       *
 **************************************************************)

let single_scc_transformation pg =
	let (sccs, sccindex, topology, roots) = strongly_connected_components pg in
	if Array.length sccs <= 1 then pg else (
		let leaves = sccs_compute_leaves roots topology in
		let (has0, has1) = List.fold_left (fun (h0, h1) i -> if pg_get_owner pg (List.hd sccs.(i)) = 0 then (true, h1) else (h0, true) ) (false, false) leaves in
		let n = pg_size pg in
		let pg' = pg_create (n + (if has0 && has1 then 4 else 2)) in
		let maxpr = ref 0 in
		for i = 0 to n - 1 do
		  let pr = pg_get_priority pg i in
		  pg_set_priority pg' i pr;
		  pg_set_owner pg' i (pg_get_owner pg i);
		  pg_set_desc pg' i (pg_get_desc pg i);
		  ns_iter (fun w -> pg_add_edge pg' i w) (pg_get_successors pg i);
		  if pr > !maxpr then maxpr := pr
		done;
		let prio0 = !maxpr + 2 - !maxpr mod 2 in
		let prio1 = !maxpr + 1 + !maxpr mod 2 in
		let ptr0 = ref n in
		let ptr1 = ref (if has0 then n + 2 else n) in
		let roots = List.filter (fun v -> pg_get_priority pg v >= 0) (List.map (fun i -> List.hd sccs.(i)) roots) in
		if has0 then (
		  let v = !ptr0 in
		  let v' = v+1 in
		  pg_set_priority pg' v 0;
		  pg_set_owner pg' v 1;
		  pg_set_desc pg' v None;
		  List.iter (fun r -> pg_add_edge pg' v r) roots;
		  pg_add_edge pg' v v';

		  pg_set_priority pg' v' prio1;
		  pg_set_owner pg' v' 0;
		  pg_add_edge pg' v' v;
		  pg_set_desc pg' v' None
		);
		if has1 then (
		  let v = !ptr1 in
		  let v' = v+1 in
		  pg_set_priority pg' v 0;
		  pg_set_owner pg' v 0;
		  pg_set_desc pg' v None;
		  List.iter (fun r -> pg_add_edge pg' v r) roots;
		  pg_add_edge pg' v v';

		  pg_set_priority pg' v' prio0;
		  pg_set_owner pg' v' 1;
		  pg_add_edge pg' v' v;
		  pg_set_desc pg' v' None
		);
		List.iter (fun i ->
			let v = List.hd sccs.(i) in
			let pl = pg_get_owner pg' v in
			pg_add_edge pg' v (if pl = 0 then !ptr0 else !ptr1) 
		) leaves;
		pg'
	)



let anti_priority_compactation_transformation pg =
	let n = pg_size pg in
	let v = pg_max_prio_node pg in
	let maxprio = pg_get_priority pg v in
	let a = Array.make (maxprio + 1) false in
	let unused = ref (maxprio + 1) in
	for i = 0 to n - 1 do
		let pr = pg_get_priority pg i in
		if (pr > -1) && (not a.(pr)) then (
			a.(pr) <- true;
			decr unused
		)
	done;
	if !unused = 0 then pg else (
          let m = n + !unused in
          let pg' = pg_create m in
          for i = 0 to n - 1 do 
	    pg_set_priority pg' i (pg_get_priority pg i);
	    pg_set_owner pg' i (pg_get_owner pg i);
	    pg_set_desc pg' i (pg_get_desc pg i);

	    if i=v then
	      pg_add_edge pg' i n
	    else
	      ns_iter (fun w -> pg_add_edge pg' i w) (pg_get_successors pg i)
          done;
          let pl = ref (1 - pg_get_owner pg v) in
          let i = ref 0 in
          let j = ref n in
          while !j < m do
            if not a.(!i) then (
              if !j = m - 1
              then
		begin
	          pg_set_priority pg' !j !i;
	          pg_set_owner pg' !j (pg_get_owner pg v);
		  pg_set_desc pg' !j None;
		  ns_iter (fun w -> pg_add_edge pg' !j w) (pg_get_successors pg v)
		end
              else
		begin
		  pg_set_priority pg' !j !i;
	          pg_set_owner pg' !j !pl;
		  pg_set_desc pg' !j None;
		  pg_add_edge pg' !j (!j+1);
		  
        	  incr j;
        	  pl := 1 - !pl
		end
            );
            incr i
          done;
          pg'
	)
(*
	if !unused = 0 then pg else (
        let m = n + !unused in
        let pg' = Array.make m (-1, -1, [||], None) in
        for i = 0 to n - 1 do
        	let (pr, pl, tr, de) = pg.(i) in
        	let tr' = Array.map (fun w -> if w = v then n else w) tr in
        	pg'.(i) <- (pr, pl, tr', de)
        done;
        let i = ref 0 in
        let j = ref n in
        while !j < m do
        	if not a.(!i) then (
        		let w = if !j < m - 1 then !j + 1 else v in
        		pg'.(!j) <- (!i, 0, [|w|], None);
        		incr j
        	);
        	incr i
        done;
        pg'
    )
*)

let cheap_escape_cycles_transformation pg keep_scc =
	let n = pg_size pg in
	let pg' = pg_create (n + (if keep_scc then 5 else 4)) in
	for i = 0 to n - 1 do
		let pl = pg_get_owner pg i in
		let pr = pg_get_priority pg i in
		if pr >= 0
		then pg_set_priority pg' i (pr+2);
		     pg_set_owner pg' i pl;
		     pg_set_desc pg' i (pg_get_desc pg i);
		     pg_add_edge pg' i (if pl = 0 then n else n + 1);
		     ns_iter (fun w -> pg_add_edge pg' i w) (pg_get_successors pg i)
				 
	done;
	let r = pg_max_prio pg in
	let pl = pg_get_owner pg 0 in
	let m = if pl mod 2 = r mod 2 then r + 1 else r + 2 in
        let n1 = n+1 in
	let n2 = n+2 in
	let n3 = n+3 in
	let n4 = n+4 in
	
	pg_set_priority pg' n 1;
	pg_set_owner pg' n 1;
	pg_set_desc pg' n None;
	pg_add_edge pg' n n2;
	if keep_scc then pg_add_edge pg' n (if pl = 1 then n4 else n1);
	
	pg_set_priority pg' n1 0;
	pg_set_owner pg' n1 0;
	pg_set_desc pg' n1 None;
	pg_add_edge pg' n1 n3;
	if keep_scc then pg_add_edge pg' n (if pl = 0 then n4 else n);
	
	pg_set_priority pg' n2 0;
	pg_set_owner pg' n2 0;
	pg_set_desc pg' n2 None;
	pg_add_edge pg' n2 n;

	pg_set_priority pg' n3 0;
	pg_set_owner pg' n3 1;
	pg_set_desc pg' n3 None;
	pg_add_edge pg' n3 n1;

	if keep_scc then
	  begin
	    pg_set_priority pg' n4 m;
	    pg_set_owner pg' n4 (1-pl);
	    pg_set_desc pg' n4 None;
	    pg_add_edge pg' n4 0
	  end;
	pg'


(* Builds totality closure of a parity game inplace *)
let total_transformation_inplace (pg: paritygame) =
    for i = 0 to (pg_size pg) - 1 do			
			if ns_isEmpty (pg_get_successors pg i) && pg_isDefined pg i
			then let pl = pg_get_owner pg i in
			     pg_set_priority pg i (1-pl);
			     pg_set_owner pg i pl;
			     pg_add_edge pg i i
    done


(* Restricts the strategy of the total closure to the original game *)
let total_revertive_restriction_inplace (oldpg: paritygame) (strat: strategy) =
	for i = 0 to (pg_size oldpg) - 1 do
		if ns_isEmpty (pg_get_successors oldpg i)
		then strat.(i) <- -1
	done


(* Transforms a parity game into an equivalent (modulo dummy nodes) alternating parity game *)
let alternating_transformation (pg: paritygame) (total: bool) =
	let n = pg_size pg in
	let counter = ref n in
	let ptr = ref (-1) in
	let altmap = Array.make n (-1, -1) in

	for i = 0 to n - 1 do
  	  let pl = pg_get_owner pg i in
	  let delta = pg_get_successors pg i in
          if ns_isEmpty delta && total
          then (altmap.(i) <- (!counter, !ptr);
        	counter := !counter + 1;
        	ptr := i)
          else
            ns_iter (fun j -> let pl' = pg_get_owner pg j in
			      if (pl = pl') && (altmap.(j) = (-1, -1))
			      then (altmap.(j) <- (!counter, !ptr);
				    counter := !counter + 1;
				    ptr := j)
			      else ())
		    delta
	done;
	
	let newpg = pg_create !counter in

	for i = 0 to n - 1 do
	  let (pr, pl, delta, _, desc) = pg_get_node pg i in
	  if ns_isEmpty delta && total
	  then
	    begin
	      pg_set_priority newpg i (1-pl);
	      pg_set_owner newpg i pl;
	      pg_set_desc newpg i desc;
	      pg_add_edge newpg i (fst altmap.(i))
	    end
	  else
            let delta' = ns_map 
                           (fun j ->
                            let pl' = pg_get_owner pg j in
                            if pl = pl' then fst altmap.(j) else j)
                           delta
	    in
	    pg_set_priority newpg i pr;
	    pg_set_owner newpg i pl;
	    pg_set_desc newpg i desc;
	    ns_iter (fun w -> pg_add_edge newpg i w ) delta'
	done;
	
	let descmap = function None -> None | Some s -> Some (s ^ "'") in

	while (!ptr != -1) do
		let (ind, prev) = altmap.(!ptr) in
		let pl = pg_get_owner pg !ptr in
		pg_set_priority newpg ind 0;
		pg_set_owner newpg ind (1-pl);
		pg_set_desc newpg ind (descmap (pg_get_desc pg !ptr));
		pg_add_edge newpg ind !ptr;
		ptr := prev
	done;

	newpg;;


(* Restricts strategy and solution to the original game *)
let alternating_revertive_restriction (oldpg: paritygame)
				      (altpg: paritygame)
				      (sol: solution)
				      (strat: strategy) =
	let n = pg_size oldpg in
	let sol' = Array.make n (-1) in
	let strat' = Array.make n (-1) in

		for i = 0 to n - 1 do
			sol'.(i) <- sol.(i);
			if strat.(i) < n
			then strat'.(i) <- strat.(i)
			else let delta = pg_get_successors altpg strat.(i) in
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
		else (0, 1 - snd (data (v / 2)))
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
	let v = pg_max_prio_node game in
	let (pr, pl, tr, _, de) = pg_get_node game v in
	let pl' = ref (1 - pl) in
	let pr' = if pr mod 2 = pl then pr + 1 else pr in
	let n = pg_size game in
	let game' = pg_create (n + pr' + 1) in
	for i = 0 to n - 1 do
		if i = v
		then
		  begin
		    pg_set_priority game' i pr;
		    pg_set_owner game' i pl;
		    pg_set_desc game' i de;
		    pg_add_edge game' i n;
		    ns_iter (fun w -> pg_add_edge game' i w) tr
		  end
		else
		  begin
		    pg_set_priority game' i (pg_get_priority game i);
		    pg_set_owner game' i (pg_get_owner game i);
		    pg_set_desc game' i (pg_get_desc game i);
		    ns_iter (fun w -> pg_add_edge game' i w) (pg_get_successors game i)
		  end
	done;
	for i = 0 to pr' do
	  let i' = n+i in
	  pg_set_priority game' i' i;
	  pg_set_owner game' i' !pl';
	  pg_set_desc game' i' None;
	  pg_add_edge game' i' (if i = pr' then v else n + i + 1);
	  pl' := 1 - !pl'
	done;
	game'



(* prio[i] mod 2 == player[i] *)
let prio_alignment_transformation (game: paritygame) =
	let n = pg_size game in
	let mp = Array.init n (fun i -> i) in
	let m = ref 0 in
	let l = ref [] in
	for i = 0 to n - 1 do
		let pr = pg_get_priority game i in
		let pl = pg_get_owner game i in
		if (pr >= 0) && (not (pr mod 2 = pl)) then (
			mp.(i) <- n + !m;
			incr m;
			l := i::!l
		)
	done;
	let game' = pg_create (n + !m) in
	for i = 0 to n - 1 do
		let (pr, pl, tr, _, desc) = pg_get_node game i in
		if mp.(i) = i
		then
		  begin
		    pg_set_priority game' i pr;
		    pg_set_owner game' i pl;
		    pg_set_desc game' i desc;
		    ns_iter (fun w -> pg_add_edge game' i w) tr
		  end
		else
		  begin
		    pg_set_priority game' i pr;
		    pg_set_owner game' i (1-pl);
		    pg_set_desc game' i desc;
		    pg_add_edge game' i mp.(i)
		  end
	done;
	let i = ref (!m + n - 1) in
	while (!l != []) do
		let j = List.hd !l in
		l := List.tl !l;
		pg_set_priority game' !i 0;
		pg_set_owner game' !i (pg_get_owner game j);
		pg_set_desc game' !i None;
		ns_iter (fun w -> pg_add_edge game' !i w) (pg_get_successors game j);
		decr i
	done;
	game'




let shift_game game k =
	let n = pg_size game in
	let game' = pg_create (n + k) in
	for i = 0 to n - 1 do
	  let (pr, pl, tr, _, de) = pg_get_node game i in
	  let ki = k+i in 
	  pg_set_priority game' ki pr;
	  pg_set_owner game' ki pl;
	  pg_set_desc game' ki de;
	  ns_iter (fun w -> pg_add_edge game' ki (k+w)) tr
	done;
	game'



let bouncing_node_transformation game =
	let l = ref [] in
	let m = ref 0 in
	let n = pg_size game in
	for i = 0 to n - 1 do
	  if ns_elem i (pg_get_successors game i) then
	    begin
	      l := (i, n + !m)::!l;
	      incr m
	    end
	done;
	let g = pg_create (n + !m) in
	for i = 0 to n - 1 do
		let (pr, pl, tr, _, de) = pg_get_node game i in
		pg_set_priority g i pr;
		pg_set_owner g i pl;
		pg_set_desc g i de;
		ns_iter (fun w -> pg_add_edge g i w) tr
	done;
	List.iter (fun (i, k) ->
		let (pr, pl, tr, _, _) = pg_get_node g i in
		pg_set_priority g k pr;
		pg_set_owner g k (1-pl);
		pg_set_desc g k None;
		pg_del_edge g k k;
		pg_add_edge g k i;
	) !l;
	g


let compress_nodes game =
	let n = pg_size game in
	let m = ref 0 in
	for i = 0 to n - 1 do
	  if pg_isDefined game i then incr m
	done;
	let game' = pg_create !m in
	let newToOld = Array.make !m (-1) in
	let oldToNew = Array.make n (-1) in
	let j = ref 0 in
	for i = 0 to !m - 1 do
		while not (pg_isDefined game !j) do
		  incr j
		done;
		newToOld.(i) <- !j;
		oldToNew.(!j) <- i;
		incr j
	done;
	for i = 0 to !m - 1 do
		pg_set_priority game' i (pg_get_priority game newToOld.(i));
		pg_set_owner game' i (pg_get_owner game newToOld.(i));
		pg_set_desc game' i (pg_get_desc game newToOld.(i));
		ns_iter (fun w -> pg_add_edge game' i oldToNew.(w)) (pg_get_successors game newToOld.(i))
	done;
	(game', newToOld, oldToNew);;


let sort_game_inplace pg cmp =

  let n = pg_size pg in
  
  let encode pl i =
    if pl = 0 then i else pl * (n + i)
  in
  
  let decode k =
    let a = abs k in
    if a < n then (0, k) else (k / a, a - n)
  in
  
  for i = 0 to n - 1 do
    pg_set_owner pg i (encode (pg_get_owner pg i) i)
  done;
  
  let b (pr, pl, d, _, s) = (pr, fst (decode pl), d, s) in
  
  pg_sort (fun x y -> cmp (b x) (b y)) pg;
  
  let perm = Array.make n (-1) in
  let perm' = Array.make n (-1) in
  
  for i = 0 to n - 1 do
    let (pl', j) = decode (pg_get_owner pg i) in
    perm.(i) <- j;
    perm'.(j) <- i;
    pg_set_pl pg i pl'
  done;
  
  for i = 0 to n - 1 do
    let delta = pg_get_successors pg i in 
    ns_iter (fun el -> pg_del_edge pg i el;
		       pg_add_edge pg i perm'.(el)) delta  (* TODO: not sure that this works. What if perm'.(el) equals some other el in later iterations?
                                                                    Maybe needs to be reimplemented making proper use of edge addition and deletion functions. *)
  done;
  
  (perm, perm');;
  
  
let sort_game_by_prio_inplace pg =
  sort_game_inplace pg (fun (pr1, _, _, _) (pr2, _, _, _) -> pr1 - pr2)
		    
let normal_form_translation pg =
  let n = pg_size pg in
  let a = ref 0 in
  for i = 0 to n - 1 do
    let l = ns_size (pg_get_successors pg i) in
    if l > 2 then a := !a + l - 2
  done;
  let game = pg_init (n + !a) (fun i ->
			       if i >= n then (0, 0, ns_empty, ns_empty, None) else pg_get_node pg i
			      ) in
  let j = ref n in
  for i = 0 to n - 1 do
    let tr = pg_get_successors game i in
    let pl = pg_get_owner game i in
    let l = ns_size tr in
    if l > 2 then (
      let c = ref i in
      let t = ref (ns_nodes tr) in
      for p = 0 to l - 3 do
	pg_add_edge game !c (List.hd !t);
	pg_add_edge game !c !j;
	pg_set_owner game !j pl;
	t := List.tl !t;
	c := !j;
	incr j;
      done;
      pg_add_edge game (!j-1) (List.hd !t);
      t := List.tl !t;
      pg_add_edge game (!j-1) (List.hd !t)
    )
  done;
  game
    
let normal_form_revertive_translation old_game sol strat =
  let n = pg_size old_game in
  let rec lookup v = if v < n then v else lookup strat.(v) in
  (Array.init n (fun i -> sol.(i)),
   Array.init n (fun i -> lookup strat.(i)))
    
let uniquize_sorted_prios_inplace game =
  let pr = ref 0 in
  let n = pg_size game in
  for i = 0 to n - 1 do
    let pr' = pg_get_priority game i in
    pr := !pr + if !pr mod 2 = pr' mod 2 then 2 else 1;
    pg_set_priority game i !pr
  done
    
let uniquize_prios_inplace game =
  let game' = pg_copy game in
  let (_, perm) = sort_game_by_prio_inplace game' in
  uniquize_sorted_prios_inplace game';
  Array.iteri (fun i j -> pg_set_priority game i (pg_get_priority game' j)) perm
	      
	      
(* turns a min-parity into a max-parity game and vice versa *)
let min_max_swap_transformation g =
  let n = pg_size g in
  let m = pg_max_prio g in
  let m = m + (m mod 2) in
  let g' = pg_copy g in
  for i=0 to n-1 do
    pg_set_priority g' i (m - (pg_get_priority g i))
  done;
  g'
	  
	  
(* Every node is accessed through one additional dummy node *)
let dummy_transformation game =
  let n = pg_size game in
  let m = 2*n in
  let g = pg_create m in
  for i=0 to n-1 do
    let (pr, pl, tr, _, desc) = pg_get_node game i in
    pg_set_priority g i pr;
    pg_set_owner g i pl;
    pg_set_desc g i desc;
    ns_iter (fun w -> pg_add_edge g i (w+n)) tr
  done;
  for i=n to m-1 do
    pg_set_priority g i 0;
    pg_set_owner g i (1-(pg_get_owner game (i-n)));
    pg_set_desc g i None;
    pg_add_edge g i (i-n)
  done;
  g
	  
let combine_games l =
  let size = List.fold_left (fun s g -> s + pg_size g) 0 l in
  let game = pg_create size in
  let _ = List.fold_left (fun k g ->
			  let n = pg_size g in
			  for i = 0 to n - 1 do
			    let (pr, pl, tr, _, de) = pg_get_node g i in
			    let ki = k+i in 
			    pg_set_priority game ki pr;
			    pg_set_owner game ki pl;
			    pg_set_desc game ki de;
			    ns_iter (fun w -> pg_add_edge game ki (k+w)) tr
			  done;
			  k + n
			 ) 0 l in
  game
    
