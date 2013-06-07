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
	let n = pg_size game in
	let l = ref [] in
	for i = 0 to n - 1 do
		let (pr, pl, delta, desc) = game.(i) in
		if pr mod 2 != pl then (
            let j = ref 0 in
            for k = 0 to Array.length delta - 1 do
                if not (delta.(k) = i) then j := !j + 1
            done;
            if !j < Array.length delta then (
                let a = Array.make !j (-1) in
                j := 0;
                l := i::!l;
                for k = 0 to Array.length delta - 1 do
                    if delta.(k) != i then (
                        a.(!j) <- delta.(k);
                        j := !j + 1
                    )
                done;
                game.(i) <- (pr, pl, a, desc)
            )
        )
	done;
	!l




(**************************************************************
 * Local Preprocessing                                        *
 **************************************************************)

(* Compacts the priorities inplace;
   returns a map newprio -> oldprio *)
let compact_prio_inplace pg real_alternation =
	let n = Array.length pg in
	let getpr i = let (pr, _, _, _) = pg.(i) in pr in
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
                let (_, pl, delta, s) = pg.(j) in
                pg.(j) <- (newprio, pl, delta, s);
                buildmap newprio (i + 1) last
            )
            else let pr' = if last < 0
                           then pr mod 2
                           else if real_alternation && (pr mod 2 = last mod 2)
                           then newprio
                           else newprio + 1 + (1 + pr - last) mod 2 in
            (
                let (_, pl, delta, s) = pg.(j) in
                compact.(pr') <- pr;
                pg.(j) <- (pr', pl, delta, s);
                buildmap pr' (i + 1) pr
            )
        )
	in
		buildmap 0 0 (-1);
		compact;;


let priority_propagation_inplace pg =
	let cmp x y = compare (pg_get_pr pg x) (pg_get_pr pg y) in
	let gr = game_to_transposed_graph pg in
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
	Array.iteri (fun i (pr, _, _, _) -> if pr >= 0 then SingleOccQueue.add i qu) pg;
	while (not (SingleOccQueue.is_empty qu)) do
		let i = SingleOccQueue.take qu in
		let (pr, _, succs, _) = pg.(i) in
		let preds = gr.(i) in
		let minpredpr = if preds = [] then -1
		                else pg_get_pr pg (ListUtils.min_elt cmp preds) in
		let minsuccpr = if Array.length succs = 0 then -1
		                else pg_get_pr pg (ArrayUtils.min_elt cmp succs) in
		let maxpr = max minpredpr minsuccpr in
		let newpr = max maxpr pr in
		List.iter (fun j -> if pg_get_pr pg j <= newpr then updint wasminpred j (newpr + 1)) preds;
		Array.iter (fun j -> if pg_get_pr pg j <= newpr then updint wasminsucc j (newpr + 1)) succs;
		if pr < maxpr then (
			pg_set_pr pg i maxpr;
			let (lopred, hipred) = wasminpred.(i) in
			let (losucc, hisucc) = wasminsucc.(i) in
			if (lopred >= 0) && (lopred <= maxpr)
			then Array.iter (fun j -> if pg_get_pr pg j < maxpr then SingleOccQueue.add j qu) succs;
			if (losucc >= 0) && (losucc <= maxpr)
			then List.iter (fun j -> if pg_get_pr pg j < maxpr then SingleOccQueue.add j qu) preds;
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
	let getpr = pg_get_pr pg in
	let gr = game_to_transposed_graph pg in
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
		   (not ((reach (fun f u -> Array.iter f (pg_get_tr pg u)) v (getpr v) TreeSet.empty_def [v]) ||
                         (reach (fun f u -> List.iter f gr.(u)) v (getpr v) TreeSet.empty_def [v]))))
		then pg_set_pr pg v 0
	done;;



(**************************************************************
 * Game Transformations                                       *
 **************************************************************)

let single_scc_transformation pg =
	let (sccs, sccindex, topology, roots) = strongly_connected_components pg in
	if Array.length sccs <= 1 then pg else (
		let leafs = sccs_compute_leafs roots topology in
		let (has0, has1) = List.fold_left (fun (h0, h1) i -> if pg_get_pl pg (List.hd sccs.(i)) = 0 then (true, h1) else (h0, true) ) (false, false) leafs in
		let n = pg_size pg in
		let pg' = pg_create (n + (if has0 && has1 then 4 else 2)) in
		let maxpr = ref 0 in
		for i = 0 to n - 1 do
			let (pr, pl, tr, de) = pg.(i) in
			pg'.(i) <- (pr, pl, tr, de);
			if pr > !maxpr then maxpr := pr
		done;
		let prio0 = !maxpr + 2 - !maxpr mod 2 in
		let prio1 = !maxpr + 1 + !maxpr mod 2 in
		let ptr0 = ref n in
		let ptr1 = ref (if has0 then n + 2 else n) in
		let roots = List.filter (fun v -> pg_get_pr pg v >= 0) (List.map (fun i -> List.hd sccs.(i)) roots) in
		if has0 then (
			pg'.(!ptr0) <- (0, 1, Array.of_list ((!ptr0 + 1)::roots), None);
			pg'.(!ptr0 + 1) <- (prio1, 0, [|!ptr0|], None)
		);
		if has1 then (
			pg'.(!ptr1) <- (0, 0, Array.of_list ((!ptr1 + 1)::roots), None);
			pg'.(!ptr1 + 1) <- (prio0, 1, [|!ptr1|], None)
		);
		List.iter (fun i ->
			let v = List.hd sccs.(i) in
			let (pr, pl, tr, de) = pg'.(v) in
			pg'.(v) <- (pr, pl, Array.append [|if pl = 0 then !ptr0 else !ptr1|] tr, de)
		) leafs;
		pg'
	)



let anti_priority_compactation_transformation pg =
	let n = pg_size pg in
	let v = pg_max_prio_node pg in
	let maxprio = pg_get_pr pg v in
	let a = Array.make (maxprio + 1) false in
	let unused = ref (maxprio + 1) in
	for i = 0 to n - 1 do
		let pr = pg_get_pr pg i in
		if (pr > -1) && (not a.(pr)) then (
			a.(pr) <- true;
			decr unused
		)
	done;
	if !unused = 0 then pg else (
        let m = n + !unused in
        let pg' = Array.make m (-1, -1, [||], None) in
        for i = 0 to n - 1 do
        	let (pr, pl, tr, de) = pg.(i) in
        	pg'.(i) <- (pr, pl, (if i = v then [|n|] else tr), de)
        done;
        let pl = ref (1 - pg_get_pl pg v) in
        let i = ref 0 in
        let j = ref n in
        while !j < m do
        	if not a.(!i) then (
        		if !j = m - 1
        		then pg'.(!j) <- (!i, pg_get_pl pg v, pg_get_tr pg v, None)
        		else pg'.(!j) <- (!i, !pl, [|!j + 1|], None);
        		incr j;
        		pl := 1 - !pl
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
		let (pr, pl, tr, de) = pg.(i) in
		if pr >= 0 then pg'.(i) <- (pr + 2, pl, Array.of_list ((Array.to_list tr) @ [if pl = 0 then n else n + 1]), de)
	done;
	let r = pg_max_prio pg in
	let pl = pg_get_pl pg 0 in
	let m = if pl mod 2 = r mod 2 then r + 1 else r + 2 in
	pg'.(n)     <- (1, 1, (if keep_scc then [|(if pl = 1 then n + 4 else n + 1); n + 2|]
	                                   else [|n + 2|]),                                        None);
	pg'.(n + 1) <- (0, 0, (if keep_scc then [|(if pl = 0 then n + 4 else n    ); n + 3|]
	                        	       else [|n + 3|]),                                        None);
	pg'.(n + 2) <- (0, 0,                   [|n    |],                                         None);
	pg'.(n + 3) <- (0, 1,                   [|n + 1|],                                         None);
	if keep_scc then pg'.(n + 4) <- (m, 1 - pl, [|0|],  None);
	pg'


(* Builds total closure of a parity game inplace *)
let total_transformation_inplace (pg: paritygame) =
    for i = 0 to (Array.length pg) - 1 do
        let (pr, pl, delta, desc) = pg.(i) in
            if (Array.length delta = 0) && (pr >= 0)
            then pg.(i) <- (1 - pl, pl, Array.make 1 i, desc)
            else ()
    done


(* Restricts the strategy of the total closure to the original game *)
let total_revertive_restriction_inplace (oldpg: paritygame)
								  	    (strat: strategy) =
	for i = 0 to (Array.length oldpg) - 1 do
		let (_, _, delta, _) = oldpg.(i) in
			if Array.length delta = 0
			then strat.(i) <- -1
			else ()
	done


(* Transforms a parity game into an equivalent (modulo dummy nodes) alternating parity game *)
let alternating_transformation (pg: paritygame) (total: bool) =
	let n = Array.length pg in
	let counter = ref n in
	let ptr = ref (-1) in
	let altmap = Array.make n (-1, -1) in

    for i = 0 to n - 1 do
        let (_, pl, delta, _) = pg.(i) in
        	if (Array.length delta = 0) && total
        	then (altmap.(i) <- (!counter, !ptr);
        	      counter := !counter + 1;
        	      ptr := i)
        	else
                Array.iter
                    (fun j ->
                        let (_, pl', _, _) = pg.(j) in
                            if (pl = pl') && (altmap.(j) = (-1, -1))
                            then (altmap.(j) <- (!counter, !ptr);
                                  counter := !counter + 1;
                                  ptr := j)
                            else ())
                    delta
    done;

	let newpg = Array.make (!counter) (-1, -1, [||], None) in

	for i = 0 to n - 1 do
		let (pr, pl, delta, desc) = pg.(i) in
		if (Array.length delta = 0) && total
		then newpg.(i) <- (1 - pl, pl, Array.make 1 (fst altmap.(i)), desc)
		else
            let delta' = Array.map
                            (fun j ->
                                let (_, pl', _, _) = pg.(j) in
                                    if pl = pl' then fst altmap.(j) else j)
                            delta in
            newpg.(i) <- (pr, pl, delta', desc)
	done;

	let descmap = function None -> None | Some s -> Some (s ^ "'") in

	while (!ptr != -1) do
		let (ind, prev) = altmap.(!ptr) in
			let (_, pl, _, _) = pg.(!ptr) in
				newpg.(ind) <- (0, 1 - pl, Array.make 1 (!ptr), descmap (pg_get_desc pg !ptr));
				ptr := prev
	done;

	newpg;;


(* Restricts strategy and solution to the original game *)
let alternating_revertive_restriction (oldpg: paritygame)
									  (altpg: paritygame)
									  (sol: solution)
									  (strat: strategy) =
	let n = Array.length oldpg in
	let sol' = Array.make n (-1) in
	let strat' = Array.make n (-1) in

		for i = 0 to n - 1 do
			sol'.(i) <- sol.(i);
			if strat.(i) < n
			then strat'.(i) <- strat.(i)
			else let (_, _, delta, _) = altpg.(strat.(i)) in
				strat'.(i) <- delta.(0)
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
	let (pr, pl, tr, de) = game.(v) in
	let pl' = ref (1 - pl) in
	let pr' = if pr mod 2 = pl then pr + 1 else pr in
	let n = pg_size game in
	let game' = pg_create (n + pr' + 1) in
	for i = 0 to n - 1 do
		if i = v
		then game'.(i) <- (pr, pl, Array.append tr [|n|], de)
		else game'.(i) <- game.(i)
	done;
	for i = 0 to pr' do
		game'.(n + i) <- (i, !pl', [|if i = pr' then v else n + i + 1|], None);
		pl' := 1 - !pl'
	done;
	game'



(* prio[i] mod 2 == player[i] *)
let prio_alignment_transformation game =
	let n = Array.length game in
	let mp = Array.init n (fun i -> i) in
	let m = ref 0 in
	let l = ref [] in
	for i = 0 to n - 1 do
		let (pr, pl, _ ,_) = game.(i) in
		if (pr >= 0) && (not (pr mod 2 = pl)) then (
			mp.(i) <- n + !m;
			incr m;
			l := i::!l
		)
	done;
	let game' = Array.make (n + !m) (-1, -1, [||], None) in
	for i = 0 to n - 1 do
		let (pr, pl, tr, desc) = game.(i) in
		if mp.(i) = i
		then game'.(i) <- (pr, pl, tr, desc)
		else game'.(i) <- (pr, 1 - pl, [|mp.(i)|], desc)
	done;
	let i = ref (!m + n - 1) in
	while (!l != []) do
		let j = List.hd !l in
		l := List.tl !l;
		let (_, pl, tr, _) = game.(j) in
		game'.(!i) <- (0, pl, tr, None);
		decr i
	done;
	game'


(* Every node is accessed through one additional dummy node *)
let dummy_transformation game =
	let n = pg_size game in
	Array.init (2 * n) (fun i ->
		let (pr, pl, tr, desc) = game.(i mod n) in
		if i < n
		then (pr, pl, Array.map (fun j -> j + n) tr, desc)
		else (0, 1 - pl, [|i - n|], None)
	)


let shift_game game k =
	let n = pg_size game in
	let game' = pg_create (n + k) in
	for i = 0 to n - 1 do
		let (pr, pl, tr, de) = game.(i) in
		game'.(k + i) <- (pr, pl, Array.map (fun j -> k + j) tr, de)
	done;
	game'

let combine_games l =
	let size = List.fold_left (fun s g -> s + pg_size g) 0 l in
	let game = pg_create size in
	let _ = List.fold_left (fun k g ->
		Array.iteri (fun i (pr, pl, tr, de) ->
			game.(k + i) <- (pr, pl, Array.map (fun j -> k + j) tr, de)
		) g;
		k + (pg_size g)
	) 0 l in
	game


let bouncing_node_transformation game =
	let l = ref [] in
	let m = ref 0 in
	let n = pg_size game in
	for i = 0 to n - 1 do
		try
			let j = ArrayUtils.index_of (pg_get_tr game i) i in
			l := (i, j, n + !m)::!l;
			incr m
		with
			Not_found -> ()
	done;
	let g = pg_create (n + !m) in
	for i = 0 to n - 1 do
		let (pr, pl, tr, de) = game.(i) in
		g.(i) <- (pr, pl, Array.copy tr, de)
	done;
	List.iter (fun (i, j, k) ->
		let (pr, pl, tr, _) = g.(i) in
		tr.(j) <- k;
		g.(k) <- (pr, 1 - pl, [|i|], None)
	) !l;
	g


let compress_nodes game =
	let n = pg_size game in
	let m = ref 0 in
	for i = 0 to n - 1 do
		if pg_get_pr game i >= 0 then incr m
	done;
	let game' = pg_create !m in
	let newToOld = Array.make !m (-1) in
	let oldToNew = Array.make n (-1) in
	let j = ref 0 in
	for i = 0 to !m - 1 do
		while (pg_get_pr game !j < 0) do
			incr j
		done;
		game'.(i) <- game.(!j);
		newToOld.(i) <- !j;
		oldToNew.(!j) <- i;
		incr j
	done;
	for i = 0 to !m - 1 do
		pg_set_tr game' i (Array.map (fun k -> oldToNew.(k)) (pg_get_tr game' i))
	done;
	(game', newToOld, oldToNew);;


let sort_game_inplace pg cmp =

	let n = Array.length pg in

	let encode pl i =
		if pl = 0 then i else pl * (n + i)
	in

	let decode k =
		let a = abs k in
			if a < n then (0, k) else (k / a, a - n)
	in

    for i = 0 to n - 1 do
        let (pr, pl, delta, desc) = pg.(i) in
            pg.(i) <- (pr, encode pl i, delta, desc)
    done;

	let b (pr, pl, d, s) = (pr, fst (decode pl), d, s) in

    Array.sort (fun x y -> cmp (b x) (b y)) pg;

    let perm = Array.make n (-1) in
    let perm' = Array.make n (-1) in

    for i = 0 to n - 1 do
    	let (pr, pl, delta, desc) = pg.(i) in
    		let (pl', j) = decode pl in
    		perm.(i) <- j;
    		perm'.(j) <- i;
    		pg.(i) <- (pr, pl', delta, desc)
    done;

    for i = 0 to n - 1 do (
    	let (pr, pl, delta, desc) = pg.(i) in
    		Array.iteri (fun j el -> delta.(j) <- perm'.(el)) delta)
    done;

    (perm, perm');;


let sort_game_by_prio_inplace pg =
	sort_game_inplace pg (fun (pr1, _, _, _) (pr2, _, _, _) -> pr1 - pr2)

let normal_form_translation pg =
	let n = Array.length pg in
	let a = ref 0 in
	for i = 0 to n - 1 do
		let l = Array.length (pg_get_tr pg i) in
		if l > 2 then a := !a + l - 2
	done;
	let game = Array.init (n + !a) (fun i ->
		if i >= n then (0, 0, [||], None) else pg.(i)
	) in
	let j = ref n in
	for i = 0 to n - 1 do
		let tr = pg_get_tr game i in
		let pl = pg_get_pl game i in
		let l = Array.length tr in
		if l > 2 then (
			let c = ref i in
			for p = 0 to l - 3 do
				pg_set_tr game !c [|tr.(p); !j|];
				pg_set_pl game !j pl;
				c := !j;
				incr j;
			done;
			pg_set_tr game (!j - 1) [|tr.(l - 2); tr.(l - 1)|]
		)
	done;
	game

let normal_form_revertive_translation old_game sol strat =
	let n = Array.length old_game in
	let rec lookup v = if v < n then v else lookup strat.(v) in
	(Array.init n (fun i -> sol.(i)),
	 Array.init n (fun i -> lookup strat.(i)))

let uniquize_sorted_prios_inplace game =
	let pr = ref 0 in
	let n = Array.length game in
	for i = 0 to n - 1 do
		let pr' = pg_get_pr game i in
		pr := !pr + if !pr mod 2 = pr' mod 2 then 2 else 1;
		pg_set_pr game i !pr
	done

let uniquize_prios_inplace game =
	let game' = pg_copy game in
	let (_, perm) = sort_game_by_prio_inplace game' in
	uniquize_sorted_prios_inplace game';
	Array.iteri (fun i j -> pg_set_pr game i (pg_get_pr game' j)) perm


(* turns a min-parity into a max-parity game and vice versa *)
let min_max_swap_transformation g =
  let m = pg_max_prio g in
  let m = m + (m mod 2) in
  Array.map (fun (p,o,ss,n) -> (m-p,o,ss,n)) g 
