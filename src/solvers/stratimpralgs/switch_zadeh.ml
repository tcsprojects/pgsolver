open Basics;;
open Stratimpralgs;;
open Paritygame;;
open Tcsset;;
open Tcsbasedata;;
open Univsolve;;
open Tcsarray;;
open Tcslist;;
open Tcsstrings;;



(************************ ANALYZE **************************)

let mdplike = false;;

module BitScheme = struct
	let models_scheme scheme bits = TreeSet.for_all (fun (i,j) -> (if i < Array.length bits then bits.(i) else 0) = j) scheme
	let leq_bits bits = TreeSet.of_array_def (Array.init (Bits.to_int bits + 1) Bits.of_int)
	let match_set bits scheme = TreeSet.filter (models_scheme scheme) (leq_bits bits)
	let flip_set bits i scheme = match_set bits (TreeSet.union (TreeSet.add (i,1) scheme) (TreeSet.of_array_def (Array.init i (fun j -> (j, 0)))))
	let unflip_set bits i scheme = match_set bits (TreeSet.union (TreeSet.add (i,0) scheme) (TreeSet.of_array_def (Array.init i (fun j -> (j, 0)))))
	let bit_flips bits i scheme = TreeSet.cardinal (flip_set bits i scheme)
	let bit_unflips bits i scheme = TreeSet.cardinal (unflip_set bits i scheme)
	let max_flip_number bits i scheme = TreeSet.max_elt (TreeSet.add 0 (TreeSet.map2_def Bits.to_int (flip_set bits i scheme)))
	let max_unflip_number bits i scheme = TreeSet.max_elt (TreeSet.add 0 (TreeSet.map2_def Bits.to_int (unflip_set bits i scheme)))
end;;


module StrategyHelper = struct
	
	let length game =
		let n = ref 0 in 
		while (
			try
				let _ =  pg_find_desc game (Some ("m" ^ string_of_int !n)) in
				true
			with Not_found -> false
		) do
			incr n 
		done;
		!n
	
	let is game strategy v w =
		try
			strategy.(pg_find_desc game (Some v)) = pg_find_desc game (Some w)
		with Not_found -> false
	
	let leads_to game valu v w =
		try
			let i = pg_find_desc game (Some v) in
			let j = pg_find_desc game (Some w) in
			let (_, path, _) = valu.(i) in
			TreeSet.mem j path
		with Not_found -> false

	let improvable game strategy valu v =
		let i = pg_find_desc game (Some v) in
		best_decision_by_valuation_ordering game node_total_ordering_by_position valu i != strategy.(i)
		
end;;


	
	
	
	
type strategy_info = {
	len: int;
	b: int array;
	sx: int array array;
	s: int array;
  g: int array;
	e: int array;
	ex: int array array;
  ddd: int array array array;	
	eg: int array;
	eb: int array;
	e0g: int array;
	e1g: int array;
	e0b: int array;
	e1b: int array;
	mu: int;
	mug: int;
	mus: int;
	least_one: int;
	greatest_one: int;
	eee: int array array array;
	eeb: int array array;
	eeg: int array array;
};;



let strategy_info game strategy = 
	let len = StrategyHelper.length game in
	let strat a i b j = if StrategyHelper.is game strategy (a ^ string_of_int i) (b ^ string_of_int j) then 1 else 0 in
    let ddd = [|[|
    Array.init len (fun i -> strat "a" i "E" i);
    Array.init len (fun i -> strat "b" i "E" i)
    |];[|
    Array.init len (fun i -> strat "v" i "X" i);
    Array.init len (fun i -> strat "w" i "X" i)
    |]|] in
	let b = Array.init len (fun i -> strat "m" i "d" i) in
	let s0 = Array.init len (fun i -> 1 - strat "g" i "m" 0) in
	let s1 = Array.init len (fun i -> if i < len - 1 then 1 - strat "s" i "m" 0 else 0) in
	let g = Array.init len (fun i -> strat "d" i "X" i) in
	let ex = Array.init 2 (fun j -> Array.init len (fun i -> ddd.(j).(0).(i) * ddd.(j).(1).(i))) in
  let sx = [|s0; s1|] in
	let e = Array.init len (fun i -> ex.(g.(i)).(i)) in
	let s = Array.init len (fun i -> sx.(g.(i)).(i)) in
	
	let last = ref len in
	let searching = ref true in
	for i = len - 1 downto 0 do
		if (!searching && b.(i) = 1) then (
			if (g.(i) != (if i + 1 = len then 0 else b.(i+1)))
			then searching := false
			else last := i
		);
	done;
	let mu = if !searching then Bits.least_zero b else !last in
	
	
	let eee = [|[|
	Array.init len (fun i -> strat "o" i "m" 1);
	Array.init len (fun i -> strat "p" i "m" 1)
	|];[|
	Array.init len (fun i -> strat "r" i "m" 1);
	Array.init len (fun i -> strat "q" i "m" 1)
	|]|] in

	let e0g = Array.init len (fun i -> max ((1-eee.(0).(0).(i)) * (1-ddd.(0).(0).(i))) ((1-eee.(0).(1).(i)) * (1-ddd.(0).(1).(i)))) in
	let e0b = Array.init len (fun i -> max (eee.(0).(0).(i) * (1-ddd.(0).(0).(i))) (eee.(0).(1).(i) * (1-ddd.(0).(1).(i)))) in
	let e1g = Array.init len (fun i -> if i < len - 1 then max ((1-eee.(1).(1).(i)) * (1-ddd.(1).(1).(i))) ((1-eee.(1).(0).(i)) * (1-ddd.(1).(0).(i))) else 0) in
	let e1b = Array.init len (fun i -> if i < len - 1 then max (eee.(1).(1).(i) * (1-ddd.(1).(1).(i))) (eee.(1).(0).(i) * (1-ddd.(1).(0).(i))) else 0) in
	let eeg = [|e0g;e1g|] in
	let eeb = [|e0b;e1b|] in
	let eg = Array.init len (fun i -> if g.(i) = 0 then e0g.(i) else e1g.(i)) in
	let eb = Array.init len (fun i -> if g.(i) = 0 then e0b.(i) else e1b.(i)) in
	{
		len = len;
		b = b;
		ex = ex;
		e1b = e1b;
		eeb = eeb;
		e1g = e1g;
		eeg = eeg;
		e0b = e0b;
		e0g = e0g;
		e = e;
		s = s;
		sx = sx;
		g = g;
		mu = mu;
		eg = eg;
		eb = eb;
		eee = eee;
		ddd = ddd;
		least_one = Bits.least_one b;
		greatest_one = Bits.greatest_one b;
		mus = Bits.least_zero s;
		mug = Bits.least_zero g;
	};;

	
	
type val_info = {
	bv: int TreeSet.t array;
	gv: int TreeSet.t array;
	sv: int TreeSet.t array;
	xv: int TreeSet.t array;
	fv: int TreeSet.t array array;
	ll: int TreeSet.t array;
	rr: int TreeSet.t array;
};;	

let valuation_info game strategy compare =
	let info = strategy_info game strategy in
	let n = info.len in
	let find a i = pg_find_desc game (Some (a ^ string_of_int i)) in 
	let y = TreeSet.singleton compare (pg_find_desc game (Some "Y")) in
	let w = Array.init info.len (fun i -> TreeSet.add (find (if info.g.(i) = 0 then "c" else "z") i) (TreeSet.add (find "d" i) y)) in
	let ll = Array.make (info.len + 1) y in
	for i = info.len - 1 downto 0 do
		ll.(i) <- if info.b.(i) = 1 then TreeSet.union ll.(i+1) w.(i) else ll.(i+1);
	done;
	let rr = Array.make (info.len+1) y in
	for i = info.len - 1 downto 0 do
		rr.(i) <- if i < info.mu then TreeSet.union rr.(i+1) w.(i) else ll.(i+1);
	done;
	let sgn i = if i > 0 then 1 else 0 in
	let b = Array.init (n+2) (fun i -> if i >= n then y else if i >= info.mu || info.b.(i) = 0 then ll.(i) else rr.(i)) in
	let g = Array.init n (fun i -> if info.sx.(0).(i) = 0 then b.(0) else TreeSet.add (find "c" i) b.(i+2)) in
	let s = Array.make (n-1) y in
	for i = n - 2 downto 0 do
			s.(i) <- if info.sx.(1).(i) = 0
							 then b.(0)
							 else if info.b.(i+1) = 1
							 then TreeSet.add (find "z" i) b.(i+1)
							 else if info.mu > 0
							 then TreeSet.add (find "z" i) rr.(i+1)
							 else if info.g.(i+1) = 1 && info.s.(i+1) = 1
							 then TreeSet.add (find "z" i) (TreeSet.union w.(i+1) s.(i+1))  
							 else if info.s.(i+1) = 0 
							 then TreeSet.add (find "z" i) (TreeSet.add (find "d" (i+1)) b.(0))
							 else TreeSet.add (find "z" i) (TreeSet.union w.(i+1) b.(i+3))					
	done;
	let sss = [|g;s|] in
	let sg = Array.init n (fun i -> sss.(info.g.(i)).(i)) in
	let fv = Array.init 2 (fun k -> Array.init n (fun i ->
		if k = 1 && i = n - 1 then y
		else if info.ex.(k).(i) = 1 || (info.g.(i) = k && info.s.(i) = 1 && i < n-1 && info.b.(i+1) != info.g.(i)) then sss.(k).(i)
    else if info.eeg.(k).(i) = 1 && info.b.(0) = 0 && info.mu = 0 && info.e.(0) = 0 then TreeSet.add (find "d" 0) sg.(0)
    else b.(if ((info.eeg.(k).(i) = 0 || info.mu > 0) && info.b.(0) = 0) || ((info.eeb.(k).(i) = 0 || info.mu = 0) && info.b.(0) = 1) then 0 else if info.mu > 1 then info.b.(1) else 1 - info.b.(1) * sgn info.mu)
	)) in
  let x = Array.init n (fun i -> TreeSet.add (find "d" i) (fv.(info.g.(i)).(i))) in
	{
		bv = b;
		gv = g;
		sv = s;
		xv = x;
		fv = fv;
		ll = ll;
		rr = rr;
	};;

type mdpval_info = {
	mbv: (int, float) TreeMap.t array;
	mll: (int, float) TreeMap.t array;
	mrr: (int, float) TreeMap.t array;
	mgg: (int, float) TreeMap.t array;
};;	

let mdpvaluation_info game strategy compare =
	let info = strategy_info game strategy in
	let valinfo = valuation_info game strategy compare in
	let find a i = pg_find_desc game (Some (a ^ string_of_int i)) in
	let mbv = Array.map (fun s -> TreeMap.by_set s (fun _ -> 1.0)) valinfo.bv in
	if (info.b.(0) = 1 && info.mu > 0) then (
		let found = ref None in
		for i = info.mu-1 downto 0 do
			if (info.e.(i) = 0)
			then found := Some i;
		done;
		if (!found != None) then (
			let j = OptionUtils.get_some !found in
			mbv.(0) <- TreeMap.add (find "d" j) 1.0 mbv.(1);
			for i = j-1 downto 0 do
				mbv.(0) <- TreeMap.add (find "d" i) 1.0 (TreeMap.add (find "z" i) 1.0 mbv.(0));
			done; 
		);
	);
		let mrr = Array.map (fun s -> TreeMap.by_set s (fun _ -> 1.0)) valinfo.rr in
		let mgg = Array.init info.len (fun i -> mrr.(i)) in
		for j = (if info.b.(1) = 1 then 0 else info.mu - 1) downto 0 do
				if (info.e.(j) = 0)
				then mgg.(j) <- TreeMap.add (find "d" j) 1.0 mbv.(1)
				else mgg.(j) <- TreeMap.add (find "d" j) 1.0 (TreeMap.add (find (if j = info.mu-1 then "c" else "z") j) 1.0 mgg.(j+1));
		done;
	{
		mbv = mbv;
		mll = Array.map (fun s -> TreeMap.by_set s (fun _ -> 1.0)) valinfo.ll;
		mrr = mrr;
		mgg = mgg;
	};;


type improvement_info_struct = {
	imp_b: int array;
	imp_g: int array;
	imp_s0: int array;
	imp_s1: int array;
	imp_d00: int array;
	imp_d01: int array;
	imp_d10: int array;
	imp_d11: int array;
	imp_e00: int array;
	imp_e01: int array;
	imp_e10: int array;
	imp_e11: int array;
};;

						
let improvement_info game strategy valu =
	let info = strategy_info game strategy in
	let improvable s = if StrategyHelper.improvable game strategy valu s then 1 else 0
	in 
	{
		imp_b = Array.init info.len (fun i -> improvable ("m" ^ string_of_int i));
		imp_g = Array.init info.len (fun i -> improvable ("d" ^ string_of_int i));
		imp_s0 = Array.init info.len (fun i -> improvable ("g" ^ string_of_int i));
		imp_s1 = Array.init (info.len-1) (fun i -> improvable ("s" ^ string_of_int i));
		imp_d00 = Array.init info.len (fun i -> improvable ("a" ^ string_of_int i));
		imp_d01 = Array.init info.len (fun i -> improvable ("b" ^ string_of_int i));
		imp_d10 = Array.init (info.len-1) (fun i -> improvable ("w" ^ string_of_int i));
		imp_d11 = Array.init (info.len-1) (fun i -> improvable ("v" ^ string_of_int i));
		imp_e00 = Array.init info.len (fun i -> improvable ("o" ^ string_of_int i));
		imp_e01 = Array.init info.len (fun i -> improvable ("p" ^ string_of_int i));
		imp_e10 = Array.init (info.len-1) (fun i -> improvable ("q" ^ string_of_int i));
		imp_e11 = Array.init (info.len-1) (fun i -> improvable ("r" ^ string_of_int i));
	};;

type occ_info_struct = {
	occ_b0: int array;
	occ_b1: int array;
	occ_g0: int array;
	occ_g1: int array;
	occ_s0_0: int array;
	occ_s0_1: int array;
	occ_s1_0: int array;
	occ_s1_1: int array;
	occ_d0_0_0: int array;
	occ_d0_0_1: int array;
	occ_d0_1_0: int array;
	occ_d0_1_1: int array;
	occ_d1_0_0: int array;
	occ_d1_0_1: int array;
	occ_d1_1_0: int array;
	occ_d1_1_1: int array;
	occ_e0_0_0: int array;
	occ_e0_0_1: int array;
	occ_e0_1_0: int array;
	occ_e0_1_1: int array;
	occ_e1_0_0: int array;
	occ_e1_0_1: int array;
	occ_e1_1_0: int array;
	occ_e1_1_1: int array;
};;

let occ_info game occ n =
	let occrec from tto =
		let i = pg_find_desc game (Some from) in
		let j = pg_find_desc game (Some tto) in
		let o = ref 0 in
		Array.iteri (fun ind k ->
			if k = j then o := occ.(i).(ind);
		) (Array.of_list (ns_nodes (pg_get_successors game i)));
		!o
	in 
	let occrecnot from tto =
		let i = pg_find_desc game (Some from) in
		let j = pg_find_desc game (Some tto) in
		let o = ref 0 in
		Array.iteri (fun ind k ->
			if k != j then o := occ.(i).(ind);
		) (Array.of_list (ns_nodes (pg_get_successors game i)));
		!o
	in 
	{
		occ_b0 = Array.init n (fun i -> occrecnot ("m" ^ string_of_int i) ("d" ^ string_of_int i));
		occ_b1 = Array.init n (fun i -> occrec ("m" ^ string_of_int i) ("d" ^ string_of_int i));
		occ_g0 = Array.init n (fun i -> occrec ("d" ^ string_of_int i) ("E" ^ string_of_int i));
		occ_g1 = Array.init n (fun i -> occrecnot ("d" ^ string_of_int i) ("E" ^ string_of_int i));
		occ_s0_0 = Array.init n (fun i -> occrec ("g" ^ string_of_int i) ("m" ^ string_of_int 0));
		occ_s0_1 = Array.init n (fun i -> occrecnot ("g" ^ string_of_int i) ("m" ^ string_of_int 0));
		occ_s1_0 = Array.init (n-1) (fun i -> occrec ("s" ^ string_of_int i) ("m" ^ string_of_int 0));
		occ_s1_1 = Array.init (n-1) (fun i -> occrecnot ("s" ^ string_of_int i) ("m" ^ string_of_int 0));
		occ_d0_0_0 = Array.init n (fun i -> occrecnot ("a" ^ string_of_int i) ("E" ^ string_of_int i));
		occ_d0_0_1 = Array.init n (fun i -> occrec ("a" ^ string_of_int i) ("E" ^ string_of_int i));
		occ_d0_1_0 = Array.init n (fun i -> occrecnot ("b" ^ string_of_int i) ("E" ^ string_of_int i));
		occ_d0_1_1 = Array.init n (fun i -> occrec ("b" ^ string_of_int i) ("E" ^ string_of_int i));
		occ_d1_0_0 = Array.init (n-1) (fun i -> occrecnot ("v" ^ string_of_int i) ("X" ^ string_of_int i));
		occ_d1_0_1 = Array.init (n-1) (fun i -> occrec ("v" ^ string_of_int i) ("X" ^ string_of_int i));
		occ_d1_1_0 = Array.init (n-1) (fun i -> occrecnot ("w" ^ string_of_int i) ("X" ^ string_of_int i));
		occ_d1_1_1 = Array.init (n-1) (fun i -> occrec ("w" ^ string_of_int i) ("X" ^ string_of_int i));
		occ_e0_0_0 = Array.init n (fun i -> occrecnot ("o" ^ string_of_int i) ("m" ^ string_of_int 1));
		occ_e0_0_1 = Array.init n (fun i -> occrec ("o" ^ string_of_int i) ("m" ^ string_of_int 1));
		occ_e0_1_0 = Array.init n (fun i -> occrecnot ("p" ^ string_of_int i) ("m" ^ string_of_int 1));
		occ_e0_1_1 = Array.init n (fun i -> occrec ("p" ^ string_of_int i) ("m" ^ string_of_int 1));
		occ_e1_0_0 = Array.init (n-1) (fun i -> occrecnot ("q" ^ string_of_int i) ("m" ^ string_of_int 1));
		occ_e1_0_1 = Array.init (n-1) (fun i -> occrec ("q" ^ string_of_int i) ("m" ^ string_of_int 1));
		occ_e1_1_0 = Array.init (n-1) (fun i -> occrecnot ("r" ^ string_of_int i) ("m" ^ string_of_int 1));
		occ_e1_1_1 = Array.init (n-1) (fun i -> occrec ("r" ^ string_of_int i) ("m" ^ string_of_int 1));
	};;
																								
let show_info game occ strategy valu n =
	let string_of_int2 i = if i < 10 then "0" ^ string_of_int i else string_of_int i in
	let info = strategy_info game strategy in
	let iinfo = improvement_info game strategy valu in
	let oinfo = occ_info game occ info.len in
	let s = ref ("\n|" ^ (string_of_int info.mu) ^ "|bgssddddeeee|bgssddddeeee|b0b1g0g1s0s1s0s1d0d1d0d1d0d1d0d1e0e1e0e1e0e1e0e1|\n" ^
	                                             "+-+------------+------------+------------------------------------------------+\n") in
	for i = n-1 downto 0 do
		s := !s ^ "|" ^ (string_of_int (i+1)) ^ "|" ^ ListUtils.custom_format string_of_int "" "" "" [
			info.b.(i); info.g.(i); info.sx.(0).(i); (if i < n-1 then info.sx.(1).(i) else 0);
			info.ddd.(0).(0).(i); info.ddd.(0).(1).(i); (if i < n-1 then info.ddd.(1).(0).(i) else 0); (if i < n-1 then info.ddd.(1).(1).(i) else 0);
			info.eee.(0).(0).(i); info.eee.(0).(1).(i); (if i < n-1 then info.eee.(1).(0).(i) else 0); (if i < n-1 then info.eee.(1).(1).(i) else 0);
		] ^ "|" ^ ListUtils.custom_format string_of_int "" "" "" [
			iinfo.imp_b.(i); iinfo.imp_g.(i); iinfo.imp_s0.(i); (if i < n-1 then iinfo.imp_s1.(i) else 0);
			iinfo.imp_d00.(i); iinfo.imp_d01.(i); (if i < n-1 then iinfo.imp_d10.(i) else 0); (if i < n-1 then iinfo.imp_d11.(i) else 0);
			iinfo.imp_e00.(i); iinfo.imp_e01.(i); (if i < n-1 then iinfo.imp_e10.(i) else 0); (if i < n-1 then iinfo.imp_e11.(i) else 0);
		] ^ "|" ^ ListUtils.custom_format string_of_int2 "" "" "" [
			oinfo.occ_b0.(i); oinfo.occ_b1.(i); oinfo.occ_g0.(i); oinfo.occ_g1.(i);
			oinfo.occ_s0_0.(i); oinfo.occ_s0_1.(i); (if i < n-1 then oinfo.occ_s1_0.(i) else 0); (if i < n-1 then oinfo.occ_s1_1.(i) else 0);
			oinfo.occ_d0_0_0.(i); oinfo.occ_d0_0_1.(i); oinfo.occ_d0_1_0.(i); oinfo.occ_d0_1_1.(i);
			(if i < n-1 then oinfo.occ_d1_0_0.(i) else 0); (if i < n-1 then oinfo.occ_d1_0_1.(i) else 0); (if i < n-1 then oinfo.occ_d1_1_0.(i) else 0); (if i < n-1 then oinfo.occ_d1_1_1.(i) else 0);
			oinfo.occ_e0_0_0.(i); oinfo.occ_e0_0_1.(i); oinfo.occ_e0_1_0.(i); oinfo.occ_e0_1_1.(i);
			(if i < n-1 then oinfo.occ_e1_0_0.(i) else 0); (if i < n-1 then oinfo.occ_e1_0_1.(i) else 0); (if i < n-1 then oinfo.occ_e1_1_0.(i) else 0); (if i < n-1 then oinfo.occ_e1_1_1.(i) else 0);
		] ^ "|\n";
  done;
	!s;;				
	

let counter_strategy_lookup game strategy len = 
	let counter = compute_counter_strategy game strategy in
	let find a i = pg_find_desc game (Some (a ^ string_of_int i)) in
	([| Array.init len (fun i -> if counter.(find "E" i) = find "g" i then 1 else 0);
	   Array.init (len-1) (fun i -> if counter.(find "X" i) = find "s" i then 1 else 0) |],
	 [| Array.init len (fun i -> if strategy.(strategy.(counter.(find "E" i))) = find "d" 0 then 0 else 1);
	   Array.init (len-1) (fun i -> if strategy.(strategy.(counter.(find "X" i))) = find "d" 0 then 0 else 1) |]);;

let test_assumptions game strategy valu =
  let assert_iff ident leftformula rightformula left right =
	  if (left != right)
		then failwith (ident ^ " : " ^ leftformula ^ " <==> " ^ rightformula)
	in
  let assert_ifthen ident leftformula rightformula left right =
	  if (left && not right)
		then failwith (ident ^ " : " ^ leftformula ^ " ==> " ^ rightformula)
	in
	let info = strategy_info game strategy in
	let (counter, counterx) = counter_strategy_lookup game strategy info.len in  
		for i = 0 to info.len - 1 do
		  assert_iff "mu assumption" "mu > 0" "info.b.(0)" (info.mu > 0) (info.b.(0) = 1);
			assert_ifthen "(As1)" "i >= mu && sigma(b_i)" "sigma(s_i)" (i >= info.mu && info.b.(i) = 1) (info.s.(i) = 1); 
		  assert_ifthen "(As2)" "i < mu && ((sigma(b_2) && i > 1) || sigma(d_i) || !sigma(b_1))" "sigma(s_i)" (i < info.mu && ((info.b.(1) = 1 && i > 0) || info.e.(i) = 1 || info.b.(0) = 0)) (info.s.(i) = 1);
			if i < info.len - 1 then assert_ifthen "(Ab)" "i < mu - 1 && !sigma(b_i)" "!sigma(b_i+1)" (i < info.mu - 1 && info.b.(i) = 0) (info.b.(i+1) = 0);
			if info.mu != 0 then assert_ifthen "(Ab2)" "mu != 1 && !sigma(b_mu-1)" "sigma(b_mu)" (info.mu != 0 && info.b.(info.mu - 1) = 0) (info.b.(info.mu) = 1);
		  if i < info.len - 2 then assert_ifthen "(Ab3)" "sigma(s_i,1) && !sigma(b_i+1)" "sigma(b_i+2) != sigma(g_i+1)" (info.sx.(1).(i) = 1 && info.b.(i+1) = 0) (info.b.(i+2) != info.g.(i+1));
			if i < info.mu then assert_ifthen "(Ag)" "i < mu" "sigma(g_i) <==> i != mu - 1" (i < info.mu) ((info.g.(i) = 1) = (i != info.mu - 1));
			assert_ifthen "(Ae)" "sigma(b_i) && (i > 1 || mu = 1 || (sigma(b_2) <==> mu > 2)" "sigma(d_i)" (info.b.(i) = 1 && (i > 0 || info.mu = 0 || ((info.b.(1) = 1) = (info.mu > 1)))) (info.e.(i) = 1);
		  assert_ifthen "(Ae')" "sigma(b_2) && (2 <= i < mu)" "sigma(d_i)" (info.b.(1) = 1 && 1 <= i && i < info.mu) (info.e.(i) = 1);
			assert_ifthen "(Aeb1)" "mu = 1 && !sigma(b_1) && i < nub <= mus,mug" "!sigma(eb_i)" (info.mu = 0 && info.b.(0) = 0 && i < info.least_one && info.least_one <= info.mus && info.least_one <= info.mug) (info.eb.(i) = 0);
			assert_ifthen "(Aeb2)" "mu = 1 && !sigma(b_1) && i < mug <= mus,nub && (PG ==> !sigma(b_mug+1))" "!sigma(eb_i)" (info.mu = 0 && info.b.(0) = 0 && i < info.mug && info.mug <= info.least_one && info.mug <= info.mus && (mdplike || info.b.(info.mug + 1) = 0)) (info.eb.(i) = 0);
			assert_ifthen "(Aeb3)" "mu = 1 && !sigma(b_1) && i < mus <= mus,nub && MDP" "!sigma(eb_i)" (info.mu = 0 && info.b.(0) = 0 && i < info.mus && info.mus <= info.least_one && info.mus <= info.mug && mdplike) (info.eb.(i) = 0);
			assert_ifthen "(Age1)" "sigma(eg_i,j) && !sigma(eb_i,j) && info.mu = 1" "!sigma(s_i,j)" (info.e0g.(i) = 1 && info.e0b.(i) = 0 && info.mu = 0) (info.sx.(0).(i) = 0);
			if i < info.len - 2 then assert_ifthen "(Age1)" "sigma(eg_i,j) && !sigma(eb_i,j) && info.mu = 1" "!sigma(s_i,j)" (info.e1g.(i) = 1 && info.e1b.(i) = 0 && info.mu = 0) (info.sx.(1).(i) = 0);
			assert_ifthen "(Age2)" "sigma(eg_i,j) && !sigma(eb_i,j) && info.mu = 1" "sigma(d_1)" (info.e0g.(i) = 1 && info.e0b.(i) = 0 && info.mu = 0) (info.e.(0) = 1);
			if i < info.len - 2 then assert_ifthen "(Age2)" "sigma(eg_i,j) && !sigma(eb_i,j) && info.mu = 1" "sigma(d_1)" (info.e1g.(i) = 1 && info.e1b.(i) = 0 && info.mu = 0) (info.e.(0) = 1);
			assert_ifthen "(Age3)" "sigma(eg_i,j) && !sigma(eb_i,j) &&" "sigma(s_1)" (info.e0g.(i) = 1 && info.e0b.(i) = 0 && not mdplike) (info.s.(0) = 1);
			if i < info.len - 2 then assert_ifthen "(Age3)" "sigma(eg_i,j) && !sigma(eb_i,j) && PG" "sigma(s_1)" (info.e1g.(i) = 1 && info.e1b.(i) = 0 && not mdplike) (info.s.(0) = 1);
			assert_ifthen "(Age4)" "sigma(eg_i,j) && !sigma(eb_i,j) && info.mu = 1" "sigma(g_1) = sigma(b_2)" (info.e0g.(i) = 1 && info.e0b.(i) = 0 && info.mu = 0) (info.g.(0) = info.b.(1));
			if i < info.len - 2 then assert_ifthen "(Age4)" "sigma(eg_i,j) && !sigma(eb_i,j) && info.mu = 1" "sigma(g_1) = sigma(b_2)" (info.e1g.(i) = 1 && info.e1b.(i) = 0 && info.mu = 0) (info.g.(0) = info.b.(1));
			if i < info.len - 1 then assert_ifthen "(Age5)" "sigma(eg_i,j) && !sigma(eb_i,j) && info.mu != 1 && sigma(s_i,j)" "sigma(b_i+1) = j" (info.e0g.(i) = 1 && info.e0b.(i) = 0 && info.mu != 0 && info.sx.(0).(i) = 1) (info.b.(i+1) = 0);
			if i < info.len - 1 then assert_ifthen "(Age5)" "sigma(eg_i,j) && !sigma(eb_i,j) && info.mu != 1 && sigma(s_i,j)" "sigma(b_i+1) = j" (info.e1g.(i) = 1 && info.e1b.(i) = 0 && info.mu != 0 && info.sx.(1).(i) = 1) (info.b.(i+1) = 1);
		  if i < info.len - 1 then assert_ifthen "(Aeg1)" "sigma(eb_i,j) && !sigma(eg_i,j) && sigma(b_1)" "sigma(b_i+1) != j" (info.e0b.(i) = 1 && info.e0g.(i) = 0 && info.b.(0) = 1) (info.b.(i+1) = 1);
		  if i < info.len - 1 then assert_ifthen "(Aeg1)" "sigma(eb_i,j) && !sigma(eg_i,j) && sigma(b_1)" "sigma(b_i+1) != j" (info.e1b.(i) = 1 && info.e1g.(i) = 0 && info.b.(0) = 1) (info.b.(i+1) = 0);
		  assert_ifthen "(Aeg2)" "sigma(eb_i,0) && !sigma(eg_i,0) && sigma(b_1) && sigma(s_i,0)" "mu = i+1" (info.e0b.(i) = 1 && info.e0g.(i) = 0 && info.b.(0) = 1 && info.sx.(0).(i) = 1) (info.mu = i + 1);
		  assert_ifthen "(Aeg3)" "sigma(eb_i,j) && !sigma(eg_i,j) && sigma(b_1) && sigma(s_i,j) && i > 1" "!sigma(b_2)" (info.e0b.(i) = 1 && info.e0g.(i) = 0 && info.b.(0) = 1 && info.sx.(0).(i) = 1 && i > 0) (info.b.(1) = 0);
		  assert_ifthen "(Aeg3)" "sigma(eb_i,j) && !sigma(eg_i,j) && sigma(b_1) && sigma(s_i,j) && i > 1" "!sigma(b_2)" (info.e1b.(i) = 1 && info.e1g.(i) = 0 && info.b.(0) = 1 && info.sx.(1).(i) = 1 && i > 0) (info.b.(1) = 0);
		  assert_ifthen "(Aeg4)" "sigma(eb_i,1) && !sigma(eg_i,1) && sigma(b_1) && sigma(s_i,1)" "mu > i+1" (info.e1b.(i) = 1 && info.e1g.(i) = 0 && info.b.(0) = 1 && info.sx.(1).(i) = 1) (info.mu > i + 1);
		  assert_ifthen "(Aeg5)" "sigma(eb_i,j) && !sigma(eg_i,j) && sigma(b_1)" "sigma(b_mu)" (info.e0b.(i) = 1 && info.e0g.(i) = 0 && info.b.(0) = 1) (info.b.(info.mu) = 1);
		  assert_ifthen "(Aeg5)" "sigma(eb_i,j) && !sigma(eg_i,j) && sigma(b_1)" "sigma(b_mu)" (info.e1b.(i) = 1 && info.e1g.(i) = 0 && info.b.(0) = 1) (info.b.(info.mu) = 1);
		  assert_ifthen "(Aeg6)" "sigma(eb_i,j) && !sigma(eg_i,j) && sigma(b_1) && mu > 2" "!sigma(b_2)" (info.e0b.(i) = 1 && info.e0g.(i) = 0 && info.b.(0) = 1 && info.mu > 1) (info.b.(1) = 0);
		  assert_ifthen "(Aeg6)" "sigma(eb_i,j) && !sigma(eg_i,j) && sigma(b_1) && mu > 2" "!sigma(b_2)" (info.e1b.(i) = 1 && info.e1g.(i) = 0 && info.b.(0) = 1 && info.mu > 1) (info.b.(1) = 0);
(*		  assert_ifthen "(Aeg7)" "sigma(b_i+1) != j && sigma(s_i,j) && !sigma(d_i,j)" "sigma(eb_i,j) && !sigma(eg_i,j)" (i < info.len - 1 && info.b.(i+1) = 1 && info.sx.(0).(i) = 1 && info.ex.(0).(i) = 0) (info.e0b.(i) = 1 && info.e0g.(i) = 0);
		  assert_ifthen "(Aeg7)" "sigma(b_i+1) != j && sigma(s_i,j) && !sigma(d_i,j)" "sigma(eb_i,j) && !sigma(eg_i,j)" (i < info.len - 1 && info.b.(i+1) = 0 && info.sx.(1).(i) = 1 && info.ex.(1).(i) = 0) (info.e1b.(i) = 1 && info.e1g.(i) = 0);*)
		  if i < info.len - 1 then assert_ifthen "(Abg1)" "sigma(eb_i,j) && sigma(eg_i,j) && sigma(s_i,j)" "sigma(b_i+1)=j" (info.e0b.(i) = 1 && info.e0g.(i) = 1 && info.sx.(0).(i) = 1) (info.b.(i+1) = 0);
		  if i < info.len - 1 then assert_ifthen "(Abg1)" "sigma(eb_i,j) && sigma(eg_i,j) && sigma(s_i,j)" "sigma(b_i+1)=j" (info.e1b.(i) = 1 && info.e1g.(i) = 1 && info.sx.(1).(i) = 1) (info.b.(i+1) = 1);
		  assert_ifthen "(Abg2)" "sigma(eb_i,j) && sigma(eg_i,j) && sigma(g_1) = sigma(b_2)" "sigma(s_1)" (info.e0b.(i) = 1 && info.e0g.(i) = 1 && info.g.(0) = info.b.(1)) (info.s.(0) = 1);
		  assert_ifthen "(Abg2)" "sigma(eb_i,j) && sigma(eg_i,j) && sigma(g_1) = sigma(b_2)" "sigma(s_1)" (info.e1b.(i) = 1 && info.e1g.(i) = 1 && info.g.(0) = info.b.(1)) (info.s.(0) = 1);
		  assert_ifthen "(Abg3)" "sigma(eb_i,j) && sigma(eg_i,j) && sigma(g_1) = sigma(b_2)" "sigma(d_1)" (info.e0b.(i) = 1 && info.e0g.(i) = 1 && info.g.(0) = info.b.(1)) (info.e.(0) = 1);
		  assert_ifthen "(Abg3)" "sigma(eb_i,j) && sigma(eg_i,j) && sigma(g_1) = sigma(b_2)" "sigma(d_1)" (info.e1b.(i) = 1 && info.e1g.(i) = 1 && info.g.(0) = info.b.(1)) (info.e.(0) = 1);
		  assert_ifthen "(Abg4)" "sigma(eb_i,j) && sigma(eg_i,j) && !sigma(g_1) && sigma(b_2)" "mu <= 2" (info.e0b.(i) = 1 && info.e0g.(i) = 1 && info.g.(0) = 0 && info.b.(1) = 1) (info.mu <= 1);
		  assert_ifthen "(Abg4)" "sigma(eb_i,j) && sigma(eg_i,j) && !sigma(g_1) && sigma(b_2)" "mu <= 2" (info.e1b.(i) = 1 && info.e1g.(i) = 1 && info.g.(0) = 0 && info.b.(1) = 1) (info.mu <= 1);
		  assert_ifthen "(Abg5)" "sigma(eb_i,j) && sigma(eg_i,j) && sigma(g_1) && !sigma(b_2)" "mu != 2" (info.e0b.(i) = 1 && info.e0g.(i) = 1 && info.g.(0) = 1 && info.b.(1) = 0) (info.mu != 1);
		  assert_ifthen "(Abg5)" "sigma(eb_i,j) && sigma(eg_i,j) && sigma(g_1) && !sigma(b_2)" "mu != 2" (info.e1b.(i) = 1 && info.e1g.(i) = 1 && info.g.(0) = 1 && info.b.(1) = 0) (info.mu != 1);

		  assert_ifthen "test" "test" "test" (info.e.(i) = 0 && i < info.mu && i > 0) (info.b.(1) = 0);

		  if (not mdplike) then (
					assert_ifthen "1" "sigma(d_i,j)" "F_i,j->s_i,j" (info.ex.(0).(i) = 1) (counter.(0).(i) = 1);
					if i < info.len - 1 then assert_ifthen "1" "sigma(d_i,j)" "F_i,j->s_i,j" (info.ex.(1).(i) = 1) (counter.(1).(i) = 1);
					assert_ifthen "2" "sigma(eg_i,j) && !sigma(eb_i,j) && mu = 1" "F_i,j->s_i,j" (info.e0g.(i) = 1 && info.e0b.(i) = 0 && info.mu = 0) (counter.(0).(i) = 1);
					if i < info.len - 1 then assert_ifthen "2" "sigma(eg_i,j) && !sigma(eb_i,j) && mu = 1" "F_i,j->s_i,j" (info.e1g.(i) = 1 && info.e1b.(i) = 0 && info.mu = 0) (counter.(1).(i) = 1);
					assert_ifthen "3" "sigma(eg_i,j) && !sigma(eb_i,j) && mu != 1" "F_i,j->g_1" (info.e0g.(i) = 1 && info.e0b.(i) = 0 && info.mu != 0) (counter.(0).(i) = 0 && counterx.(0).(i) = 0);
					if i < info.len - 1 then assert_ifthen "3" "sigma(eg_i,j) && !sigma(eb_i,j) && mu != 1" "F_i,j->g_1" (info.e1g.(i) = 1 && info.e1b.(i) = 0 && info.mu != 0) (counter.(1).(i) = 0 && counterx.(1).(i) = 0);
					if i < info.len - 1 then assert_ifthen "4" "sigma(eb_i,j) && !sigma(eg_i,j) && !sigma(b_1) && (!sigma(s_i,j) || sigma(b_i+1) = j)" "F_i,j->b_2" (info.e0g.(i) = 0 && info.e0b.(i) = 1 && info.b.(0) = 0 && (info.sx.(0).(i) = 0 || info.b.(i+1) = 0)) (counter.(0).(i) = 0 && counterx.(0).(i) = 1);
					if i < info.len - 1 then assert_ifthen "4" "sigma(eb_i,j) && !sigma(eg_i,j) && !sigma(b_1) && (!sigma(s_i,j) || sigma(b_i+1) = j)" "F_i,j->b_2" (info.e1g.(i) = 0 && info.e1b.(i) = 1 && info.b.(0) = 0 && (info.sx.(1).(i) = 0 || info.b.(i+1) = 1)) (counter.(1).(i) = 0 && counterx.(1).(i) = 1);
					if i < info.len - 1 then assert_ifthen "5" "sigma(eb_i,j) && !sigma(eg_i,j) && (sigma(b_1) || (sigma(s_i,j) && sigma(b_i+1) != j))" "F_i,j->s_i,j" (info.e0g.(i) = 0 && info.e0b.(i) = 1 && (info.b.(0) = 1 || (info.sx.(0).(i) = 1 && info.b.(i+1) != 0))) (counter.(0).(i) = 1);
					if i < info.len - 1 then assert_ifthen "5" "sigma(eb_i,j) && !sigma(eg_i,j) && (sigma(b_1) || (sigma(s_i,j) && sigma(b_i+1) != j))" "F_i,j->s_i,j" (info.e1g.(i) = 0 && info.e1b.(i) = 1 && (info.b.(0) = 1 || (info.sx.(1).(i) = 1 && info.b.(i+1) != 1))) (counter.(1).(i) = 1);
					assert_ifthen "6" "sigma(eb_i,j) && sigma(eg_i,j) && sigma(g_1) != sigma(b_2)" "F_i,j->g_1" (info.e0g.(i) = 1 && info.e0b.(i) = 1 && info.g.(0) != info.b.(1)) (counter.(0).(i) = 0 && counterx.(0).(i) = 0);
					if i < info.len - 1 then assert_ifthen "6" "sigma(eb_i,j) && sigma(eg_i,j) && sigma(g_1) != sigma(b_2)" "F_i,j->g_1" (info.e1g.(i) = 1 && info.e1b.(i) = 1 && info.g.(0) != info.b.(1)) (counter.(1).(i) = 0 && counterx.(1).(i) = 0);
					assert_ifthen "7" "sigma(eb_i,j) && sigma(eg_i,j) && sigma(g_1) = sigma(b_2)" "F_i,j->b_2" (info.e0g.(i) = 1 && info.e0b.(i) = 1 && info.g.(0) = info.b.(1)) (counter.(0).(i) = 0 && counterx.(0).(i) = 1);
					if i < info.len - 1 then assert_ifthen "7" "sigma(eb_i,j) && sigma(eg_i,j) && sigma(g_1) = sigma(b_2)" "F_i,j->b_2" (info.e1g.(i) = 1 && info.e1b.(i) = 1 && info.g.(0) = info.b.(1)) (counter.(1).(i) = 0 && counterx.(1).(i) = 1);
			);
		
		
		if ((not mdplike) && info.s.(i) = 1 && info.e.(i) = 0) then (
			let x = counter.(info.g.(i)).(i) = 1 in
			let y = i < info.len - 1 && info.b.(i+1) != info.g.(i) in
			if x != y 
			then failwith "yikes";
		);
		
		
		
		(*
		  let sxb = if i = info.len-1 then 0 else 1-info.b.(i+1) in
			let mui = if info.mu <= i + 1 then 1 else 0 in
			let lei = if info.least_one > i + 1 then 1 else 0 in
			
			if (info.mu = 0 && info.b.(0) = 1) then failwith "impossible";

			if info.sx.(0).(i) != sxb && info.sx.(0).(i) != mui
			then print_string ("\nWrong" ^ string_of_int i ^ "/" ^ string_of_int info.least_one ^ "\n");
			if info.sx.(1).(i) = sxb && info.sx.(1).(i) = mui && (mdplike || info.sx.(1).(i) = 0 || info.b.(0) = 1)
			then print_string ("\nXrong" ^ string_of_int i ^ "/" ^ string_of_int info.sx.(1).(i) ^ "\n");
			if info.sx.(1).(i) = sxb && info.sx.(1).(i) = 1 && info.b.(0) = 0 && info.sx.(1).(i) != lei && not mdplike
			then print_string ("\nXrong" ^ string_of_int i ^ "/" ^ string_of_int info.least_one ^ "\n");
			
			
			if (info.mu = 0 && i > 0 && info.b.(i) = 0 && info.s.(i) = 1 && info.e.(i) = 1) then failwith "arg";
			
			if (info.mu != 0 && info.b.(info.mu -1) = 0 && info.b.(info.mu) = 0) then print_string("noway\n");
			
			if (info.e0g.(i) = 0 && info.e0b.(i) = 1 && info.b.(0) = 1 && i < info.len - 1 && info.b.(i+1) = 0) then print_string("S0\n");
			if (info.e0g.(i) = 0 && info.e0b.(i) = 1 && info.b.(0) = 1 && info.sx.(0).(i) = 0 && (info.mu = 0 || info.b.(info.mu) = 0)) then print_string("S0\n");

			if (info.e0g.(i) = 1 && info.e0b.(i) = 0 && info.mu = 0 && info.sx.(0).(i) = 1) then print_string("foobarina\n");
			if (info.e1g.(i) = 1 && info.e1b.(i) = 0 && info.mu = 0 && info.sx.(1).(i) = 1) then print_string("foobarina\n");
			if (info.e0g.(i) = 1 && info.e0b.(i) = 0 && info.mu = 0 && info.e.(0) = 0) then print_string("foobarina\n");
			if (info.e1g.(i) = 1 && info.e1b.(i) = 0 && info.mu = 0 && info.e.(0) = 0) then print_string("foobarina\n");
			if (info.e0g.(i) = 1 && info.e0b.(i) = 0 && info.s.(0) = 0) then print_string("foobarinaX\n");
			if (info.e1g.(i) = 1 && info.e1b.(i) = 0 && info.s.(0) = 0) then print_string("foobarinaY\n");
			if (info.e0g.(i) = 1 && info.e0b.(i) = 0 && info.mu = 0 && info.g.(0) != info.b.(1)) then print_string("foobarinaX\n");
			if (info.e1g.(i) = 1 && info.e1b.(i) = 0 && info.mu = 0 && info.g.(0) != info.b.(1)) then print_string("foobarinaY\n");

			if ( info.mu > 0 && 0= info.b.(0)) then print_string("foobarinaY\n");
			
			if (info.e0g.(i) = 1 && info.e0b.(i) = 0 && info.mu > 0 && i < info.len - 1 && info.b.(i+1) = 1 && info.sx.(0).(i) = 1) then print_string("foobarinaXX\n");
			if (info.e1g.(i) = 1 && info.e1b.(i) = 0 && info.mu > 0 && i < info.len - 1 && info.b.(i+1) = 0 && info.sx.(1).(i) = 1) then print_string("foobarinaZZ\n");

			if (info.e0g.(i) = 1 && info.e0b.(i) = 0 && info.mu > 0 && i < info.len - 1 && info.b.(i+1) = 0 && info.sx.(0).(i) = 1 && i = info.mu - 1) then print_string("foobarina!!\n");
			if (info.e1g.(i) = 1 && info.e1b.(i) = 0 && info.mu > 0 && i < info.len - 1 && info.b.(i+1) = 1 && info.sx.(1).(i) = 1 && i = info.mu - 2) then print_string("foobarinaZZ\n");

									
			if (info.e0g.(i) = 1 && info.e0b.(i) = 0) then (
				if (info.mu = 0) then (
						if (info.sx.(0).(i) = 1) then print_string("\nasdfasdf\n");
						if (info.b.(0) = 1) then print_string("\nasdfasdf\n");
					);
					let x = if (info.mu = 0) then 0 else 1 in
					if (counter.(0).(i) = x) then print_string ("\nLoops " ^ string_of_int i ^ " / " ^ string_of_int counter.(0).(i) ^ "\n");
			);
			if (info.e0g.(i) = 0 && info.e0b.(i) = 1 && info.b.(0) = 1) then (
					if (i < info.len - 1 && info.b.(i+1) = 0) then failwith "asdf";
			);
			if (info.e1g.(i) = 0 && info.e1b.(i) = 1 && info.b.(0) = 1) then (
					if (i < info.len - 1 && info.b.(i+1) = 1) then failwith "asdf";
			);
			if (info.e0g.(i) = 0 && info.e0b.(i) = 1) then (
					let x = if info.b.(0) = 0 && (info.sx.(0).(i) = 0 || i = info.len - 1 || info.b.(i+1) = 0) then 1 else 0 in
					
					if (x = 0 &&  i < info.len - 1 && info.b.(i+1) = 0) then failwith "asdf";
					
					if (info.sx.(0).(i) = 1 && info.b.(0) = 1 && info.mu != i+1) then failwith "fdsay";
					if (info.sx.(0).(i) = 1 && info.b.(0) = 1 && i > 0 && info.b.(1) = 1) then failwith "rrer";
					if (info.sx.(0).(i) = 1 && info.b.(0) = 1 && info.b.(i+1) = 0) then failwith "fdsay";
					if (info.sx.(0).(i) = 1 && info.b.(0) = 1 && i >= 1 && info.b.(1) = 1) then failwith "fdsay";
					if (info.sx.(0).(i) = 0 && info.b.(0) = 1 && (info.b.(1) = 1 && info.mu != 1)) then failwith "fdsay!";
					if (info.b.(0) = 1 && info.mu > 1 && info.b.(1) = 1) then failwith "fdsfdfdfdfax";

										if (info.b.(0) = 1 && info.b.(info.mu) = 0) then failwith "fdsazzzx";

										
					if (counter.(0).(i) = x) then print_string ("\nLoops " ^ string_of_int i ^ " / " ^ string_of_int x ^ "\n");
			);
			
				  if (i + 1 < info.len && info.b.(i+1) = 1 && info.sx.(0).(i) = 1 && info.g.(i) = 1) then failwith "asdfasf";
				  if (i + 1 < info.len && info.b.(i+1) = 0 && info.sx.(1).(i) = 1 && info.g.(i) = 0) then failwith "asdfasf";
			
			if (info.e0g.(i) = 1 && info.e0b.(i) = 1) then (
					if (counter.(0).(i) = 1) then print_string ("\nLoops " ^ string_of_int i ^ " / " ^ string_of_int counter.(0).(i) ^ "\n");
					let x = if info.e.(0) = 0 || info.g.(0) != info.b.(1) then 1 else 0 in
					
				  if (i + 1 < info.len && info.b.(i+1) = 1 && info.sx.(0).(i) = 1) then failwith "asdfasf";
					if (info.s.(0) = 0 && info.g.(0) = info.b.(1) && info.e.(0) = 1) then failwith "w00t";
					
					if (info.e.(0) = 0 &&  info.g.(0) = info.b.(1)) then failwith "nope";
					
					if (info.g.(0) = 0 && info.b.(1) = 1 && info.mu > 1) then failwith "aha";
					if (info.g.(0) = 1 && info.b.(1) = 0 && info.mu = 1) then failwith "sjs";

					if (counterx.(0).(i) = x) then print_string ("\nLoops " ^ string_of_int i ^ " / " ^ string_of_int x ^ "\n");
			);
			
			if (info.e1g.(i) = 1 && info.e1b.(i) = 0) then (
				if (info.mu = 0) then (
						if (info.sx.(1).(i) = 1) then print_string("\nasdfasdf\n");
						if (info.b.(0) = 1) then print_string("\nasdfasdf\n");
					);
					let x = if (info.mu = 0) then 0 else 1 in
					if (counter.(1).(i) = x) then print_string ("\nLoops " ^ string_of_int i ^ " / " ^ string_of_int counter.(0).(i) ^ "\n");
			);
			if (info.e1g.(i) = 0 && info.e1b.(i) = 1) then (
					let x = if info.b.(0) = 0 && (info.sx.(1).(i) = 0 || i = info.len - 1 || info.b.(i+1) = 1) then 1 else 0 in

					if (x = 0 &&  i < info.len - 1 && info.b.(i+1) = 1) then failwith "asdf";

					

					if (info.sx.(1).(i) = 1 && info.b.(0) = 1 && info.mu <= i+1) then failwith "fdsax";
					if (info.sx.(1).(i) = 1 && info.b.(0) = 1 && i > 0 && info.b.(1) = 1) then failwith "fdsax";
					if (info.b.(0) = 1 && info.mu > 1 && info.b.(1) = 1) then failwith "fdsfdfdfdfax";
					if (info.b.(0) = 1 && info.b.(info.mu) = 0) then failwith "fdsazzzx";
					if (info.sx.(1).(i) = 0 && info.b.(0) = 1 && (info.b.(1) = 1 && info.mu != 1)) then failwith "fdsay?";

					if (info.g.(0) = 0 && info.b.(1) = 1 && info.mu > 1) then failwith "aha";
					if (info.g.(0) = 1 && info.b.(1) = 0 && info.mu = 1) then failwith "sjs";

					if (counter.(1).(i) = x) then print_string ("\nLoops " ^ string_of_int i ^ " / " ^ string_of_int x ^ "\n");
					
					
					if (info.sx.(1).(i) = 1 && info.b.(i+1) = 0) then (
						if (info.eb.(i) = 0) then (
							print_string "rats";
						);
					);
					
			);
			if (info.e1g.(i) = 1 && info.e1b.(i) = 1) then (
					if (counter.(1).(i) = 1) then print_string ("\nLoops " ^ string_of_int i ^ " / " ^ string_of_int counter.(1).(i) ^ "\n");
					let x = if info.e.(0) = 0 || info.g.(0) != info.b.(1) then 1 else 0 in

				  if (i + 1 < info.len && info.b.(i+1) = 0 && info.sx.(1).(i) = 1) then failwith "asdfasf";
					if (info.s.(0) = 0 && info.g.(0) = info.b.(1) && info.e.(0) = 1) then failwith "w11t";
					if (info.e.(0) = 0 && info.g.(0) = info.b.(1)) then failwith "nope";

					if (counterx.(1).(i) = x) then print_string ("\nLoops " ^ string_of_int i ^ " / " ^ string_of_int x ^ "\n");
			);
															
																																													
		if (i >= info.mu && info.b.(i) = 1) || (i < info.mu && i > 0 && info.b.(1) = 1) || (i < info.mu && info.e.(i) = 1) || (i < info.mu && info.b.(0) = 0) then (
			if (info.s.(i) != 1) then print_string ("\n\nAs" ^ string_of_int i ^ "/" ^ string_of_int info.mu ^ "\n\n");
		);
		if (i < info.len-1 && info.mu > 0 && i >= info.mu) then (
			 if (info.sx.(1 - info.b.(i+1)).(i) != 0) then print_string ("\n\nAs!\n\n");
		);
		if (i < info.mu - 1 && info.b.(i+1) = 1) then (
				if (info.b.(i) != 1) then print_string ("\n\nAb\n\n");
		);
		if (info.b.(i) = 1 && i > 0) ||
		   (info.b.(i) = 1 && i = 0 && info.mu = 0) ||
			 (info.b.(1) = 1 && i > 0 && i < info.mu) ||
			 (info.b.(i) = 1 && i = 0 && info.b.(1) = (if info.mu = 1 then 0 else 1)) then (
				if (info.e.(i) != 1) then print_string ("\n\nAF\n\n");
		);

		if (i < info.mu) then (
				if (info.g.(i) != (if i != info.mu - 1 then 1 else 0)) then print_string ("\n\nAg\n\n");
		);
		if info.mu = 0 && info.b.(0) = 0 && Bits.least_one info.b <= Bits.least_zero info.s && Bits.least_one info.b <= Bits.least_zero info.g && info.eb.(i) = 1 && i < Bits.least_one info.b then print_string  ("\n\nEbbb\n\n");
		if info.mu = 0 && info.b.(0) = 0 && Bits.least_zero info.g <= Bits.least_one info.b && Bits.least_zero info.g <= Bits.least_zero info.s && info.eb.(i) = 1 && i < Bits.least_zero info.g && (mdplike || info.b.(Bits.least_zero info.g + 1) = 0) then print_string  ("\n\nAbbb\n\n");
		if info.mu = 0 && info.b.(0) = 0 && Bits.least_zero info.s <= Bits.least_one info.b && Bits.least_zero info.s <= Bits.least_zero info.g && info.eb.(i) = 1 && i < Bits.least_zero info.s && mdplike then print_string  ("\n\nCbbb\n\n");
		*)
	done;
	(* b_i never meets g_0, b_0, b_1 *)	
	for i = 1 to info.len - 1 do
		if (StrategyHelper.leads_to game valu ("m" ^ string_of_int i) "d0") then print_string ("\n\nb_" ^ string_of_int i ^ " to g_0\n\n");
		if (StrategyHelper.leads_to game valu ("m" ^ string_of_int i) "m0") then print_string ("\n\nb_" ^ string_of_int i ^ " to b_0\n\n");
	done;
	for i = 2 to info.len - 1 do
		if (StrategyHelper.leads_to game valu ("m" ^ string_of_int i) "m1") then print_string ("\n\nb_" ^ string_of_int i ^ " to b_1\n\n");
	done;
	for i = 0 to info.len - 2 do
		if (info.sx.(1).(i) = 1 && StrategyHelper.leads_to game valu ("s" ^ string_of_int i) "d0") then print_string ("\n\ns1_" ^ string_of_int i ^ " to g_0\n\n");
		if (info.sx.(1).(i) = 1 && StrategyHelper.leads_to game valu ("s" ^ string_of_int i) "m0") then print_string ("\n\ns1_" ^ string_of_int i ^ " to b_0\n\n");
  done;
	(* g_0 never sees b_0 *)
	if (StrategyHelper.leads_to game valu "d0" "m0") then print_string ("\n\ng_0 to b_0\n\n");;



let test_valuation_assumptions game strategy valu n =
	let find a i = pg_find_desc game (Some (a ^ string_of_int i)) in	
	let info = strategy_info game strategy in
	let vinfo = valuation_info game strategy (TreeSet.get_compare (let (_, v_valu, _) = valu.(0) in v_valu)) in
	let check_valu desc s i assrt =
		let desc = desc ^ " " ^ string_of_int i ^ " (mu=" ^ string_of_int info.mu ^ ") " in
		let v = pg_find_desc game (Some (s ^ string_of_int i)) in
		let (_, v_valu, _) = valu.(v) in
		let va = TreeSet.filter (fun u -> pg_get_priority game u >= 11) v_valu in
		let ff = TreeSet.format (fun i -> OptionUtils.get_some (pg_get_desc game i)) in
		let diff = TreeSet.sym_diff va assrt in
		if not (TreeSet.is_empty diff)
		then print_string ("\n\n" ^ desc ^ " " ^ " " ^ ArrayUtils.format string_of_int info.b ^ " " ^ ff diff ^ " | " ^ ff va ^ " | " ^ ff assrt ^ "\n\n");
	in
let check_valu_range desc s i assrt_low assrt_high =
    let desc = desc ^ " " ^ string_of_int i ^ " (mu=" ^ string_of_int info.mu ^ ") " in
    let v = pg_find_desc game (Some (s ^ string_of_int i)) in
    let (_, v_valu, _) = valu.(v) in
    let va = TreeSet.filter (fun u -> pg_get_priority game u >= 11) v_valu in
    let diff_low = TreeSet.sym_diff va assrt_low in
		let diff_high = TreeSet.sym_diff va assrt_high in
    if not (TreeSet.is_empty diff_low) && not (TreeSet.is_empty diff_high)
    then print_string ("\n\n" ^ desc ^ " " ^ " " ^ ArrayUtils.format string_of_int info.b ^ "\n\n");
in
  if (info.mu = 0 && info.b.(0) = 0) then (
		let k = ref (n-1) in
		for i = n - 1 downto 0 do
			if info.g.(i) = 0 || info.s.(i) = 0 || info.b.(i) = 1 then k := i;
		done;
		let k = !k in
		let set = if (info.b.(k) = 1) then vinfo.bv.(k)
			        else if info.g.(k) = 0 && info.s.(k) = 1 && (info.eb.(k) = 0 || info.b.(k+1) = 1) then TreeSet.add (find "d" k) (TreeSet.add (find "c" k) vinfo.bv.(k+2))
			        else TreeSet.add (find "d" k) vinfo.bv.(0) in
    let set = ref set in
		for i = k-1 downto 0 do
			(*
			if (info.b.(k) = 1 && info.eb.(i) = 1) then failwith "akjsdhfa";
			if (info.b.(k) = 0 && info.s.(k) = 1 && info.b.(k+1) = 0 && info.eb.(i) = 1) then failwith "foobar";
			*)
			set := TreeSet.add (find "d" i) (TreeSet.add (find "z" i) !set)
		done;
		let set = !set in
		check_valu "bisel" "d" 0 set
	);
	for i = 0 to n - 1 do
			check_valu "ladder" "m" i vinfo.bv.(i);
			check_valu "left_up" "g" i vinfo.gv.(i);
			if (i < n-1) then check_valu "right_up" "s" i vinfo.sv.(i);
			check_valu "bisel" "d" i vinfo.xv.(i);
			check_valu "leftcyc" "E" i vinfo.fv.(0).(i);			
			if (i < n-1) then check_valu "ritecyc" "X" i vinfo.fv.(1).(i);
			
			if (i < info.mu && info.b.(1) = 1) then check_valu "bisel-l3" "d" i vinfo.rr.(i);
			if (i < info.mu) then check_valu_range "bisel-l4" "d" i vinfo.rr.(i) (TreeSet.add (find "d" i) vinfo.bv.(1));
			(*
			if (info.mu = 0 && i = 0 && info.g.(0) = 0 && info.b.(0) = 0 && info.b.(1) = 0) then check_valu_range "test" "d" i (TreeSet.add (find "d" i) vinfo.bv.(2)) (TreeSet.add (find "c" i) (TreeSet.add (find "d" i) vinfo.bv.(1)));
      if (info.mu = 0 && i = 0 && info.g.(0) = 0 && info.b.(0) = 0 && info.b.(1) = 1) then check_valu_range "test" "d" i (TreeSet.add (find "c" i) (TreeSet.add (find "d" i) vinfo.bv.(2))) (TreeSet.add (find "d" i) vinfo.bv.(1));
      if (info.mu = 0 && i = 0 && info.g.(0) = 1 && info.b.(0) = 0 && info.b.(1) = 1) then check_valu_range "test" "d" i (TreeSet.add (find "d" i) vinfo.bv.(1)) (TreeSet.add (find "z" i) (TreeSet.add (find "d" i) vinfo.bv.(1)));
			*)
 	done;;
	
	
	
let test_mdpvaluation_assumptions game strategy valu mdpvalu n =
	let find a i = pg_find_desc game (Some (a ^ string_of_int i)) in
	let info = strategy_info game strategy in
	let vinfo = mdpvaluation_info game strategy (TreeSet.get_compare (let (_, v_valu, _) = valu.(0) in v_valu)) in
	let check_valu desc s i assrt =
		let desc = desc ^ " " ^ string_of_int i ^ " (mu=" ^ string_of_int info.mu ^ ") " in
		let v = pg_find_desc game (Some (s ^ string_of_int i)) in
		let ff = TreeMap.format (fun (i, v) -> OptionUtils.get_some (pg_get_desc game i) ^ ":" ^ string_of_float v) in
		let va = fst mdpvalu.(v) in
		let va = TreeMap.filter (fun u v -> pg_get_priority game u >= 11) va in
		if not (TreeMap.equal (fun v1 v2 -> not (v1 > v2) && not (v1 < v2)) va assrt) 
		then print_string ("\n\n" ^ desc ^ " " ^ " " ^ ArrayUtils.format string_of_int info.b ^ " " ^ " | " ^ ff va ^ " | " ^ ff assrt ^ "\n\n");
	in
if (info.mu = 0 && info.b.(0) = 0) then (
		let k = ref (n-1) in
		for i = n - 1 downto 0 do
			if info.g.(i) = 0 || info.s.(i) = 0 || info.b.(i) = 1 then k := i;
		done;
		let k = !k in
		let set = if info.b.(k) = 1 then vinfo.mbv.(0)
							else if info.g.(k) = 0 && info.s.(k) = 1 && (info.eb.(k) = 0 (*|| info.b.(k+1) = 1*)) then TreeMap.add (find "d" k) 1.0 (TreeMap.add (find "c" k) 1.0 vinfo.mbv.(k+2))
					    else TreeMap.add (find "d" k) 1.0 vinfo.mbv.(0) in
    let set = ref set in
		for i = k-1 downto 0 do
			set := TreeMap.add (find "d" i) 1.0 (TreeMap.add (find "z" i) 1.0 !set)
		done;
		let set = !set in
		check_valu "bisel" "d" 0 set
	);
		for i = 0 to n - 1 do
			check_valu "ladder" "m" i vinfo.mbv.(i);
			if (i < info.mu) then check_valu "bisel" "d" i vinfo.mgg.(i);
 	done;;
		
	
	
	
	
let test_improving_switches game strategy valu n =
	let best_decision v =
		let cmp =
			if not mdplike
			then fun i j -> node_valuation_ordering game node_total_ordering_by_position valu.(i) valu.(j)
			else let mdplike_valu = mdplike_valuation game 7 strategy in
	  	     compare_mdplike_valuation game mdplike_valu
		in
			best_decision_by_ordering game cmp v
	in
	let info = strategy_info game strategy in
	(*let strat a i b j = if StrategyHelper.is game strategy (a ^ string_of_int i) (b ^ string_of_int j) then 1 else 0 in*)
	let impr = Array.init (pg_size game) (fun i ->
    (pg_get_owner game i = plr_Even) && (strategy.(i) != best_decision i)
  ) in
	let check_impr desc s i assrt =
		let desc = desc ^ " " ^ string_of_int i ^ " (mu=" ^ string_of_int info.mu ^ ") " in
		let v = pg_find_desc game (Some (s ^ string_of_int i)) in
		if (impr.(v) != assrt) then (
			print_string ("\n\n" ^ desc ^ " " ^ s ^ string_of_int i ^ " (false " ^ (if assrt then "+ + + + + +" else "- - - - - -") ^ ") -- " ^ "\n\n");
		)
  in
  	let rec ex i j f = i <= j && ((f i) || (ex (i+1) j f)) in 
	(*let sgn i = if i > 0 then 1 else 0 in*)
	let impr_b = Array.init n (fun i ->
		   (i = info.mu && info.b.(i) = 0 && info.e.(i) = 1 && info.g.(i) = (if i = n-1 then 0 else info.b.(i+1))) ||
			 (i = info.mu - 1 && info.b.(i) = 1 && info.b.(i+1) = 1) ||
		   (i < info.mu - 1 && info.b.(i) = 1 && info.b.(i+1) = 0)
	) in
	let impr_s_0 = Array.init n (fun i ->
		     (i = info.mu - 2 && info.b.(i+2) = 1 && info.sx.(0).(i) = 0) ||
			 (i < info.mu - 2 && info.b.(i+2) = 0 && info.sx.(0).(i) = 0) ||  
			 (i > info.mu - 2 && i < n-1 && info.sx.(0).(i) = 0 && info.b.(i+1) = 0) ||
			 (i > info.mu - 2 && i < n-1 && info.sx.(0).(i) = 1 && info.b.(i+1) = 1 && info.b.(0) = 0) ||
			 (mdplike && i = info.mu - 1 && info.sx.(0).(i) = 1 && info.b.(info.mu) = 1 && info.b.(0) = 1 && (info.b.(1) = 0 || i = 0) && ex 0 i (fun j -> info.eb.(j) = 1))			 
  ) in
	(*let impr_s_1 = Array.init (n-1) (fun i ->
		   (i = info.mu - 1 && info.e.(i+1) = 1 && info.g.(i+1) = (if i < n-2 then info.b.(i+2) else 0) && info.sx.(1).(i) = 0) ||
			 (i < info.mu - 1 && info.b.(0) = 0 && info.sx.(1).(i) = 1) ||  
			 (i > info.mu - 1 && i < n-1 && info.sx.(1).(i) != info.b.(i+1) && info.sx.(1).(i) * info.b.(0) = 0)
  ) in
	let impr_e i value =
		(info.mu >= 1 && info.b.(1) = (if info.mu > 1 then value else 1 - value)) ||
		(info.mu = 0 && value = if info.e.(0) = 1 && info.g.(0) = info.b.(1) then 1 else 0)
	in
	let impr_g = Array.init n (fun i ->
		(
			info.ex.(1).(i) = 1 &&
			(
			  (
					info.ex.(0).(i) = 1 &&
					i = info.mu &&
					info.g.(i) != info.b.(i+1)
				) ||
		    (
					info.ex.(0).(i) = 0 &&
					i >= info.mu &&
					info.g.(i) = 0 &&
					info.b.(i) = 0 &&
					info.sx.(0).(i) != info.sx.(1).(i)
				)
			)
		) ||
		(
			info.ex.(1).(i) = 0 &&
			info.ex.(0).(i) = 0 &&
			(
				(
					info.g.(i) = 1 &&
					info.e0b.(i) <= info.eb.(i) &&
					info.e0g.(i) <= info.eg.(i)
				) ||
				(
					info.b.(i) = 0 &&
					info.g.(i) = 0 && 
					info.eb.(i) = 1 && 
					info.eg.(i) >= info.e1g.(i) &&
					(
						(
							i >= info.mu && 
							info.e1b.(i) = 0 && 
							info.e1g.(i) = 1
						) ||
						(
							info.mu = 0 && 
							info.sx.(0).(i) = 1 && 
							info.e1b.(i) = 1 && 
							(
          			(
									info.sx.(1).(i) = 0 &&
									info.eg.(i) = 1 &&
									info.e1g.(i) = 0
								) ||
								(
									i = 0 && 
									info.b.(1) = 1
								)
							)
						)
					)
				)
			)
		)
  ) in
	let impr_d i value other_value exit other_exit side =
		(
			value = 1 &&
			(
		  	(
					(
						info.b.(i) = 0 ||
						info.g.(i) != side
					) &&
					(
						(
							info.mu = 0 &&
							info.e.(0) = 1 &&
							info.g.(0) = info.b.(1) &&
							exit = 0 &&
							info.sx.(side).(i) >= info.b.(0) &&
							i > 0
						) ||
						(
							info.mu > 0 &&
							exit = 1 &&
							info.b.(1) = 1 - sgn (info.mu - 1)
						)
					)
				) ||
        (
					side = 1 &&
					exit = 1 &&
					info.mu > 1 &&
					i < info.mu &&
					info.b.(1) = 0 &&
					info.ex.(side).(i) >= i
				) ||
				(
					i = 0 &&
					info.mu = exit && 
					info.b.(1) = 1 &&
					exit = 1
				) ||
				(
					i = 0 &&
					info.mu = exit &&
					info.g.(0) = info.b.(1) &&
					exit = 0 &&
					side != info.g.(0)
				)
			)
		) ||
		(
			value = 0 &&
			other_value = 1 &&
			(
				(
					info.sx.(side).(i) = 1 &&
					side = (if i >= n-1 then 0 else info.b.(i+1))
				) ||
				(
					info.sx.(side).(i) = 0 &&
					info.mu > 0 &&
					exit = 0
				) ||
				(
					info.mu = 0 &&
					info.b.(0) = 0 &&
					info.sx.(side).(i) = 0 &&
					exit = 1
				)
			)
		) ||
		(
			value = 0 &&
			other_value = 0 &&
			(
				let better_exit = (info.mu >= 1 && info.b.(1) = (if info.mu > 1 then exit else 1 - exit)) || (info.mu = 0 && exit = if info.e.(0) = 1 && info.g.(0) = info.b.(1) then 1 else 0) in
				(
					info.sx.(side).(i) = 1 &&
					(
						exit = other_exit ||
						better_exit
					) &&
					(
						i >= n-1 ||
						(
							side = 1 &&
							info.b.(i+1) = 1
						) || (
							side = 0 &&
							info.b.(i) >= info.b.(i+1) &&
							(
								i > 0 ||
								info.mu != 1
							)						
						)
					)
				) ||
			  (
					info.sx.(side).(i) = 0 &&
					(
						(
							exit != other_exit &&
							better_exit
						) ||
						exit = other_exit && exit != info.b.(0) && info.b.(0) = sgn info.mu
					)
				)
			)
		)
	in*)
	for i = 0 to n - 1 do
		check_impr "b" "m" i impr_b.(i);		
		check_impr "s0" "g" i impr_s_0.(i);
		(*	
		if i < n-1 then check_impr "s1" "s" i impr_s_1.(i);
		check_impr "g" "d" i impr_g.(i);
		check_impr "e" "o" i (impr_e i (strat "o" i "m" 1));
		check_impr "e" "p" i (impr_e i (strat "p" i "m" 1));
		if i < n-1 then check_impr "e" "q" i (impr_e i (strat "q" i "m" 1));
		if i < n-1 then check_impr "e" "r" i (impr_e i (strat "r" i "m" 1));
		check_impr "d" "a" i (impr_d i (strat "a" i "E" i) (strat "b" i "E" i) (strat "o" i "m" 1) (strat "p" i "m" 1) 0);
		check_impr "d" "b" i (impr_d i (strat "b" i "E" i) (strat "a" i "E" i) (strat "p" i "m" 1) (strat "o" i "m" 1) 0);
 		if i < n-1 then
		check_impr "d" "w" i (impr_d i (strat "w" i "X" i) (strat "v" i "X" i) (strat "q" i "m" 1) (strat "r" i "m" 1) 1);
		if i < n-1 then
		check_impr "d" "v" i (impr_d i (strat "v" i "X" i) (strat "w" i "X" i) (strat "r" i "m" 1) (strat "q" i "m" 1) 1);
		*) 
	done;;


let initial_strategy_check game strategy n =
	let info = strategy_info game strategy in
	let result = ref true in
	for i = 0 to n - 1 do
		if (info.b.(i) = 1) then (
			if not (info.g.(i) = (if i = n-1 then 0 else info.b.(i+1)) && info.e.(i) = 1 && info.s.(i) = 1)
			then result := false; 
		);
		if (i < n - 1) then (
			if (info.sx.(0).(i) = info.b.(i+1) || info.sx.(1).(i) != info.b.(i+1))
			then result := false;
		);
		if info.eee.(0).(0).(i) = info.b.(0)
		then result := false;
		if info.eee.(0).(1).(i) = info.b.(0)
		then result := false;
		if (i < n - 1) then (
			if info.eee.(1).(0).(i) = info.b.(0)
			then result := false;
			if info.eee.(1).(1).(i) = info.b.(0)
			then result := false;
		);
		if (info.b.(i) = 0 || info.g.(i) = 1) then (
			if (info.ddd.(0).(0).(i) = 0 && info.ddd.(0).(1).(i) = 0)
			then result := false;
		);
		if (info.b.(i) = 0 || info.g.(i) = 0) then (
			if (i < n - 1 && info.ddd.(1).(0).(i) = 0 && info.ddd.(1).(1).(i) = 0)
			then result := false;
		);
	done;
	!result;;



let oldoccrec = ref [||];;
let oldinfo = ref None;;

let test_occrec_delta_assumptions game strategy valu occrec n =
	let info = strategy_info game strategy in
	if (Array.length !oldoccrec = 0) then (
		oldoccrec := Array.init (Array.length occrec) (fun i -> Array.init (Array.length occrec.(i)) (fun j -> occrec.(i).(j)));
		oldinfo := Some info;
	) else if (info.b = (OptionUtils.get_some !oldinfo).b || not (initial_strategy_check game strategy n)) then () else (
		print_string "Check\n";
		let oi = OptionUtils.get_some !oldinfo in
		let check desc s i assrt =
			let v = pg_find_desc game (Some (s ^ string_of_int i)) in
			let delta = ref 0 in
			for j = 0 to Array.length occrec.(v) - 1 do
			  delta := !delta + occrec.(v).(j) - !oldoccrec.(v).(j);
			done;
			if (!delta != assrt)
			then print_string ("\n\n" ^ desc ^ " " ^ s ^ string_of_int i ^ " : " ^ string_of_int !delta ^ " vs " ^ string_of_int assrt ^ " " ^ ArrayUtils.format string_of_int oi.b ^ " --> " ^ ArrayUtils.format string_of_int info.b ^ "\n\n");
		in	
		
		let delta_b = Array.init n (fun i ->
			if (i <= oi.mu) then 1 else 0 
		) in
		let delta_s_0 = Array.init n (fun i ->
			if (i < oi.mu) then 1 else 0 
		) in
		let delta_s_1 = Array.init n (fun i ->
			if (i < oi.mu) then 1 else 0 
		) in
		let delta_e = Array.init n (fun i ->
			1
		) in

		let delta_d = Array.init 2 (fun k -> Array.init 2 (fun l -> Array.init n (fun i ->
			if (i > oi.mu) then (
				if oi.b.(i) = 1 && (i = n-1 || oi.b.(i+1) = k) then 0				
				else if oi.b.(i) = 1 then 1
				else if l = 1 && Bits.greatest_one oi.b = n then 2
				else if i < n-1 && oi.b.(i+1) = 1-k && oi.b.(0) = 0 && (ArrayUtils.forall oi.b (fun j x -> j < 1 || j >= i || x = 1)) then 1
				else if oi.b.(0) >= l && i <= n-1-k && (ArrayUtils.forall oi.b (fun j x -> j = 0 || x = 0)) && i > oi.b.(0) then 1
				else if i > 0 && (i = n-1 || oi.b.(i+1) = k) && ((ArrayUtils.exists oi.b (fun j x -> j < i && x = 0))) then 1
				else if k = 1 && (ArrayUtils.forall oi.b (fun j x -> j <= i || x = 0)) then 1
				else if l = 0 then 2
				else if i < n-1 && oi.b.(i+1) = 1-k && i > 2 && (ArrayUtils.exists oi.b (fun j x -> j >= 2 && j < i && x = 0)) then 2
				else if oi.b.(0) = 0 && oi.b.(1) = 0 && oi.b.(i) = 0 && (i = n-1 || oi.b.(i+1) = 1-k) then 1
				else 2
			) else if (i = oi.mu) then (
				if k = 1 && Bits.greatest_one oi.b < i then 1
				else if l = 1 && Bits.greatest_one oi.b = n then 1 + k
				else if i < n-1 && oi.b.(i+1) = 1-k && i > 0 then 2
				else if (i = n-1 || oi.b.(i+1) = k) && i > 0 then l
				else if i = 0 && oi.b.(1) = 1-k then 1
				else 1-l
			) else (				
				if k = 1 && l = 1 && oi.b.(i+1) = 1 && i = 1 then 1
				else if k = 1 && oi.b.(i+1) = 1 && i > 0 then 2
				else if i = 0 && oi.b.(0) = 1 && l = 1 && oi.b.(1) = k then 2					
			  else if oi.b.(0) = 1 && oi.b.(1) = 1 && i = 1 && oi.mu = 2 && l = 1 then 1
				else if k = 1 && oi.b.(0) = 1 && oi.b.(1) = 1 && oi.b.(i) = 1 then 1
				else if i = oi.mu - 1 && i > 0 then 2
				else 1
			)			
		)))	in

		let delta_g = Array.init n (fun i ->
			if i = 2 && (oi.mu > 3 || (oi.mu = 1 && oi.b.(2) = 0 && oi.b.(3) = 0 && Bits.greatest_one oi.b > 2)) then 2
			else if i < n-1 && oi.b.(0) = 1 && oi.b.(i+1) = 0 && i = oi.mu && Bits.greatest_one oi.b >= i then 2
			else if oi.b.(0) = 0 && Bits.greatest_one oi.b < i && i < n-1 then 2
			else if i < n-1 && i > 0 && oi.b.(i+1) = 0 && oi.b.(i) = 0 && (ArrayUtils.forall oi.b (fun j x -> j < 1 + 2 * oi.b.(0) || j >= i || x = 1)) &&
			        Bits.least_one oi.b >= 0 && Bits.greatest_one oi.b > i && (oi.b.(0) = 0 || oi.b.(1) + oi.b.(2) = 1) then 2

			else if i < 2 && i + 1 < oi.mu then 1
			else if i = oi.mu && (oi.b.(1) = 1 || i > 0) && Bits.greatest_one oi.b >= i then 1
			else if i < n-1 && oi.b.(0) = 0 && oi.b.(1) = 0 && oi.b.(i+1) = 0 && Bits.greatest_one oi.b > i && Bits.least_one oi.b >= 0 && i > 1 &&
			        (ArrayUtils.forall oi.b (fun j x -> j < 2 || j >= i || x = 1)) && oi.b.(i) = 0 then 1
			
			else 0
		) in
		
		for i = 0 to n - 1 do
			check "b" "m" i delta_b.(i);
			if i < n - 1 then
			check "s0" "g" i delta_s_0.(i);
			if i < n - 1 then
			check "s1" "s" i delta_s_1.(i);
			
			
			check "d00" "a" i delta_d.(0).(0).(i);
			check "d01" "b" i delta_d.(0).(1).(i);
			if i < n - 1 then
			check "d10" "w" i delta_d.(1).(0).(i);
			if i < n - 1 then
			check "d11" "v" i delta_d.(1).(1).(i);

			check "g" "d" i delta_g.(i);

			check "e*" "o" i delta_e.(i);
			check "e*" "p" i delta_e.(i);
			if i < n - 1 then
			check "e*" "q" i delta_e.(i);
			if i < n - 1 then
			check "e*" "r" i delta_e.(i);
		done;
		
		oldoccrec := Array.init (Array.length occrec) (fun i -> Array.init (Array.length occrec.(i)) (fun j -> occrec.(i).(j)));
		oldinfo := Some info;
	);;



let oldoccrecx = ref [||];;
let oldinfox = ref None;;

let test_occrec_assumptions game strategy valu occrec n =
	let info = strategy_info game strategy in
	if (Array.length !oldoccrecx = 0) then (
		oldoccrecx := Array.init (Array.length occrec) (fun i -> Array.init (Array.length occrec.(i)) (fun j -> occrec.(i).(j)));
		oldinfox := Some info;
	) else if (info.b = (OptionUtils.get_some !oldinfox).b || not (initial_strategy_check game strategy n)) then () else (
		print_string "CheckGlobal\n";
		let oi = OptionUtils.get_some !oldinfox in
		let check desc s i assrt =
			let v = pg_find_desc game (Some (s ^ string_of_int i)) in
			let acc = ref 0 in
			for j = 0 to Array.length occrec.(v) - 1 do
			  acc := !acc + occrec.(v).(j);
			done;
			if (!acc != assrt)
			then print_string ("\n\n" ^ desc ^ " " ^ s ^ string_of_int i ^ " : " ^ string_of_int (!acc-assrt) ^ " " ^ ArrayUtils.format string_of_int oi.b ^ " --> " ^ ArrayUtils.format string_of_int info.b ^ "\n\n");
		in
		
		let assrt_b = Array.init n (fun i ->
			BitScheme.bit_flips info.b i TreeSet.empty_def + BitScheme.bit_unflips info.b i TreeSet.empty_def - 1
		) in
		let assrt_s_0 = Array.init n (fun i ->
			BitScheme.bit_flips info.b (i+1) TreeSet.empty_def + BitScheme.bit_unflips info.b (i+1) TreeSet.empty_def - 1
		) in
		let assrt_s_1 = Array.init n (fun i ->
			BitScheme.bit_flips info.b (i+1) TreeSet.empty_def + BitScheme.bit_unflips info.b (i+1) TreeSet.empty_def - 1
		) in
		let assrt_e = Array.init n (fun i ->
			Bits.to_int info.b
		) in
		
		let sgn i = if i = 0 then 0 else if i > 0 then 1 else -1 in
		
		let assrt_d = Array.init 2 (fun k -> Array.init 2 (fun l -> Array.init n (fun i ->
			if (info.b.(i) = 1 && (i = n-1 || info.b.(i+1) = k))
			then BitScheme.max_flip_number info.b i (TreeSet.singleton_def (i+1, k)) + (2 * l - 1) * sgn i
			else if (info.b.(i) = 0 && (i = n-1 || info.b.(i+1) = k))
			then 2 * BitScheme.bit_flips info.b 0 TreeSet.empty_def - info.b.(0) * sgn i + l 
			else if (info.b.(i) = 1)
			then 2 * BitScheme.bit_flips info.b 0 TreeSet.empty_def - info.b.(0) + l
			else if i = 0
		  then BitScheme.max_flip_number info.b i (TreeSet.singleton_def (i+1, k)) + 2 * Bits.to_int info.b + 1 + l				 
				   - 2 * ((if k = 0 then BitScheme.max_flip_number else BitScheme.max_unflip_number) info.b (i+1) TreeSet.empty_def)
			else if i = 1
			then BitScheme.max_flip_number info.b i (TreeSet.singleton_def (i+1, k))
				 + (2 - k) * Bits.to_int info.b + l + (1-k) * (1-info.b.(0))
				 - 2 * ((if k = 0 then BitScheme.max_flip_number else BitScheme.max_unflip_number) info.b (i+1) TreeSet.empty_def)
				 + k * (if Bits.to_int info.b < 2 then 0 else 1) * (Bits.to_int info.b + 1 - info.b.(0))
			else if i = 2
			then BitScheme.max_flip_number info.b i (TreeSet.singleton_def (i+1, k))
				 + (2 - k) * Bits.to_int info.b + l + (if Bits.least_zero info.b = i then 0 else 1-k)
				 - 2 * ((if k = 0 then BitScheme.max_flip_number else BitScheme.max_unflip_number) info.b (i+1) TreeSet.empty_def)
				 + k * (if Bits.greatest_one info.b < i then 0 else Bits.to_int info.b + (if Bits.least_zero info.b = i then 0 else 1))
				 + l * (1-info.b.(0)) * (1-info.b.(1)) * (1 - k + k *(1-info.b.(2)) * (1-info.b.(3)))
			else if i = n-1
			then 2 * Bits.to_int info.b + (if Bits.least_zero info.b = i then 0 else 1)
			else BitScheme.max_flip_number info.b i (TreeSet.singleton_def (i+1, k))
				 + (2 - k) * Bits.to_int info.b + l + (if Bits.least_zero info.b = i then 0 else 1-k)
				 - 2 * ((if k = 0 then BitScheme.max_flip_number else BitScheme.max_unflip_number) info.b (i+1) TreeSet.empty_def)				 
				 + k * (if Bits.greatest_one info.b < i then 0 else Bits.to_int info.b + (if Bits.least_zero info.b = i then 0 else 1))
				 + (1-k) * l * (if (Bits.least_zero info.b < i) then (if info.b.(0) + info.b.(1) = 1 && ArrayUtils.forall info.b (fun j x -> j < 2 || j >= i || x = 1) then 0 else 1) else 0)
				 + k * l * (if info.b.(i+1) = 0 && Bits.greatest_one info.b > i && (info.b.(i-1) = 0 || Bits.numb_zero_below info.b i > 1 || (i > 3 && info.b.(0) + info.b.(1) != 1) || (i > 3 && info.b.(i+2) = 1)) then 1 else 0)
				 + k * l * (if i > 3 && i < n-2 && info.b.(i+1) = 0 && (if Bits.numb_zero_below info.b i > 0 then info.b.(i+2) = 1 else Bits.greatest_one info.b > i) && info.b.(i-1) = 1 && Bits.numb_zero_below info.b i < 2 && Bits.numb_zero_below info.b i = Bits.numb_zero_below info.b 2 then -1 else 0)
		))) in
				
		let assrt_g = Array.init n (fun i ->
			if i = n-1 then 0
			else if (Bits.greatest_one info.b < i)
			then Bits.to_int info.b - 2 + Bits.to_int info.b mod 2
			else if i = 0 then info.b.(i) * info.b.(i+1) + Bits.to_int (Bits.shri info.b (i+2)) * 2
			else if i = 1 then info.b.(i) * info.b.(i+1) + Bits.to_int (Bits.shri info.b (i+2)) * 6 + (1-info.b.(i)) * (1-info.b.(i+1)) * (2 * info.b.(0) - 4)
			else info.b.(i) * info.b.(i+1) + Bits.to_int (Bits.shri info.b (i+2)) * 10 +
			     Bits.to_int (Bits.onei n i) - 2 + (1-info.b.(i)) * (1-info.b.(i+1)) * (
				if (ArrayUtils.forall info.b (fun j x -> j < 2 || j >= i || x = 1)) && info.b.(i+1) = 0 then info.b.(1) * 3 + info.b.(0) + info.b.(0) * info.b.(1) - 7 else -9
			)
		) in
				
		for i = 0 to n - 1 do
			check "b" "m" i assrt_b.(i);
			if i < n - 1 then
			check "s0" "g" i assrt_s_0.(i);
			if i < n - 1 then
			check "s1" "s" i assrt_s_1.(i);
			
			check "d00" "a" i assrt_d.(0).(0).(i);
			check "d01" "b" i assrt_d.(0).(1).(i);
			if i < n - 1 then 
			check "d10" "w" i assrt_d.(1).(0).(i);
			if i < n - 1 then
			check "d11" "v" i assrt_d.(1).(1).(i);

			check "g" "d" i assrt_g.(i);

			
			check "e*" "o" i assrt_e.(i);
			check "e*" "p" i assrt_e.(i);
			if i < n - 1 then
			check "e*" "q" i assrt_e.(i);
			if i < n - 1 then
			check "e*" "r" i assrt_e.(i);
		done;
		
		oldoccrecx := Array.init (Array.length occrec) (fun i -> Array.init (Array.length occrec.(i)) (fun j -> occrec.(i).(j)));
		oldinfox := Some info;
	);;




let check_fair_exp_occ game strategy bits occ =
		let find x i = pg_find_desc game (Some (x ^ string_of_int i)) in
		let get x i y j = let (a,b) = (find x i,find y j) in occ.(a).(ArrayUtils.index_of (Array.of_list (ns_nodes (pg_get_successors game a))) b) in
		let n = Array.length bits - 1 in
		let valid = ref true in
		let active = Bits.shr bits in
		let assrt x i y j k s f =
			let g = get x i y j in
			if f g then true else (
				print_string ("\n\n" ^ x ^ string_of_int i ^ " " ^ y ^ string_of_int j ^ " : " ^ string_of_int g ^ s ^ string_of_int k ^ "\n\n");
				false
			) in
		let eqs x i y j k = assrt x i y j k "=" (fun a -> a = k) in
		let bounded x i y j k l = assrt x i y j k ">=" (fun a -> a >= k) && assrt x i y j l "<=" (fun a -> a <= l) in
		let bits_as_int = Bits.to_int bits in
		for i = 0 to n - 1 do
			let ibfl = BitScheme.bit_flips bits i TreeSet.empty_def in
			let ibflh = ibfl / 2 in
			let zbfl = BitScheme.bit_flips bits 0 TreeSet.empty_def in
 			let ipbfl = if i < n-1 then BitScheme.bit_flips bits (i+1) TreeSet.empty_def else 0 in
			let ibfla0 = BitScheme.max_flip_number bits i (TreeSet.singleton_def (i+1, 0)) in
			let ibfla1 = BitScheme.max_flip_number bits i (TreeSet.singleton_def (i+1, 1)) in
			let ipbmfl = if i < n-1 then BitScheme.max_flip_number bits (i+1) TreeSet.empty_def else 0 in
			let ipbmflu = if i < n-1 then BitScheme.max_unflip_number bits (i+1) TreeSet.empty_def else 0 in
			
			valid := eqs "m" i "d" i ibfl && !valid;

			if (bits.(i) = 1 && active.(i) = 0) then (
				valid := eqs "a" i "E" i (ibfla0/2 + ibfla0 mod 2) && !valid;
				valid := bounded "a" i "o" i (ibfla0/2 - 1) (ibfla0/2) && !valid;
        valid := bounded "b" i "E" i (ibfla0/2) (ibfla0/2 + 1) && !valid;
        valid := bounded "b" i "p" i (ibfla0/2 - 1) (ibfla0/2) && !valid;
			  valid := eqs "o" i "d" 0 zbfl && !valid;
			  valid := eqs "p" i "d" 0 zbfl && !valid;
			  valid := bounded "o" i "m" 1 (zbfl-1) zbfl && !valid;
			  valid := bounded "p" i "m" 1 (zbfl-1) zbfl && !valid;
			) else (
				let limit = min ((ibfla0/2) + bits_as_int - ipbmfl) zbfl in
				valid := bounded "a" i "E" i (limit - 1) (limit + 1) && !valid;
				valid := bounded "b" i "E" i (limit - 1) (limit + 1) && !valid;
				valid := bounded "a" i "o" i (limit - 1) (limit + 1) && !valid;
				valid := bounded "b" i "p" i (limit - 1) (limit + 1) && !valid;
				valid := bounded "o" i "d" 0 (zbfl-1) zbfl && !valid;
				valid := bounded "p" i "d" 0 (zbfl-1) zbfl && !valid;
				valid := bounded "o" i "m" 1 (zbfl-1) zbfl && !valid;
				valid := bounded "p" i "m" 1 (zbfl-1) zbfl && !valid;
	  	);

			if (i < n-1) then (
				
				valid := eqs "m" i "m" (i+1) (ibfl - bits.(i)) && !valid;
				valid := eqs "g" i "c" i (ipbfl - 1 * bits.(i+1)) && !valid;
				valid := bounded "s" i "u" i (ipbfl - 0 * bits.(i+1) - 1) (ipbfl - 0 * bits.(i+1)) && !valid;
				valid := bounded "g" i "m" 0 (ipbfl - 0 * bits.(i+1) - 1) (ipbfl - 0 * bits.(i+1)) && !valid;
				valid := bounded "s" i "m" 0 (ipbfl - 1 * bits.(i+1) - 1) (ipbfl - 1 * bits.(i+1)) && !valid;
				
				if (bits.(i) = 1 && active.(i) = 1) then (
          valid := bounded "v" i "X" i (ibfla1/2 + ibfla1 mod 2) (ibfla1/2 + ibfla1 mod 2 + 1) && !valid;
          valid := bounded "w" i "X" i (ibfla1/2) (ibfla1/2 + 1) && !valid;
	        valid := bounded "v" i "r" i (ibfla1/2 - 1) (ibfla1/2) && !valid;
	        valid := bounded "w" i "q" i (ibfla1/2 - 1) (ibfla1/2) && !valid;
					valid := eqs "q" i "d" 0 zbfl && !valid;
				  valid := eqs "r" i "d" 0 zbfl && !valid;
					valid := bounded "q" i "m" 1 (zbfl-1) zbfl && !valid;
				  valid := bounded "r" i "m" 1 (zbfl-1) zbfl && !valid;
				) else (
				  let limit = min ((ibfla1/2) + bits_as_int - ipbmflu) zbfl in
					valid := bounded "v" i "X" i (limit - 1) (limit + 1) && !valid;
					valid := bounded "w" i "X" i (limit - 1) (limit + 1) && !valid; 
					valid := bounded "v" i "r" i (limit - 1) (limit + 1) && !valid;
					valid := bounded "w" i "q" i (limit - 1) (limit + 1) && !valid; 
					valid := bounded "q" i "d" 0 (zbfl-1) zbfl && !valid;
					valid := bounded "r" i "d" 0 (zbfl-1) zbfl && !valid;
					valid := bounded "q" i "m" 1 (zbfl-1) zbfl && !valid;
					valid := bounded "r" i "m" 1 (zbfl-1) zbfl && !valid;
				);

				valid := bounded "d" i "E" i (ibflh - 1) (get "a" i "E" i) && !valid;
				valid := bounded "d" i "X" i (ibflh - 1) (get "v" i "X" i) && !valid;

			);
		done;
		!valid;;


let curbits = ref [||] ;;
let old_phase = ref "";;
let iteration = ref 0;;
let last_active_phases = ref TreeSet.empty_def;;
let count_bits = ref 1;;

let switch_zadeh_exp_tie_break_callback n game old_strategy valu occ v w r s =
	
	  let is_initial_strategy bits = Array.init (Array.length bits - 1) (fun i -> bits.(i)) = (strategy_info game old_strategy).b in

		let msg_tagged_nl v = message_autotagged_newline v (fun _ -> "ANALYZE") in

		if (Array.length !curbits = 0)
		then curbits := Bits.zero (n+1);

	  if (not (is_initial_strategy !curbits) && (is_initial_strategy (Bits.inc !curbits)) && initial_strategy_check game old_strategy n) 
		then (
			curbits := Bits.inc !curbits;
			incr count_bits;
			msg_tagged_nl 1 (fun _ -> "\n-----" ^  "------" ^ ArrayUtils.format string_of_int !curbits ^ "--------" ^ string_of_int !count_bits ^ " of " ^ string_of_int (1 lsl n) ^ "\n");
			(*let check = check_fair_exp_occ game old_strategy !curbits occ in
			msg_tagged_nl 1 (fun _ -> "\nOcc - " ^ (if check then "yes" else "no") ^ "\n"); *)
			last_active_phases := TreeSet.empty_def;
		);

		test_assumptions game old_strategy valu;

	  if mdplike then (
	    let mdpvalu = mdplike_valuation game 7 old_strategy in
			test_mdpvaluation_assumptions game old_strategy valu mdpvalu n;
		) else (
  		test_valuation_assumptions game old_strategy valu n;
		);

(*

  	test_occrec_assumptions game old_strategy valu occ n;
		test_occrec_delta_assumptions game old_strategy valu occ n;*)
		test_improving_switches game old_strategy valu n;
		
		();;
		
	
(***************************************** ANALYZE ********************************************)		



let improvement_policy_optimize_fair_default_tie_break game node_total_ordering _ _ valu =
	ListUtils.min_elt (fun (_, _, k) (_, _, k') ->
		node_valuation_ordering game node_total_ordering valu.(k') valu.(k)
	)


  
let improvement_policy_optimize_fair tie_break
                                     game node_total_ordering occ old_strategy valu =
    let msg_tagged_nl v = message_autotagged_newline v (fun _ -> "STRIMPR_FAIR") in
	let desc i = match (pg_get_desc game i) with Some s -> s | None -> string_of_int i in
  
	let cmp =
		if not mdplike
		then fun i j -> node_valuation_ordering game node_total_ordering valu.(i) valu.(j)
		else let mdplike_valu = mdplike_valuation game 7 old_strategy in
  	     compare_mdplike_valuation game mdplike_valu
	in
	

	
			
    msg_tagged_nl 4 (fun _ ->
    	"Occ: " ^ 
    	ArrayUtils.formati (fun i a -> desc i ^ ":" ^
    		let tr = Array.of_list (ns_nodes (pg_get_successors game i)) in
    		ArrayUtils.formati (fun j k ->
    			desc tr.(j) ^ ":" ^ string_of_int k
    		) a
    	) occ
    	^ "\n"
    );
	
    let strategy = Array.copy old_strategy in
	let l = ref [] in
	let minvalue = ref (-1) in
	pg_iterate (fun i (_, pl, tr, _, de) ->
		if pl = plr_Even then
			Array.iteri (fun j k ->		
				if cmp strategy.(i) k < 0 then (
					if !minvalue = -1 then minvalue := occ.(i).(j);
					if !minvalue = occ.(i).(j) then l := (i,j,k)::!l
					else if !minvalue > occ.(i).(j) then (
						l := [(i,j,k)];
						minvalue := occ.(i).(j)
					)
				)
			) (Array.of_list (ns_nodes tr))
	) game;
	l := List.rev !l;
	msg_tagged_nl 4 (fun _ -> "Occurrence-Arena: " ^ ListUtils.format (fun (i,_,k) -> desc i ^ "->" ^ desc k) (List.rev !l) ^ "\n");
	if !l != [] then (
		let (i,j,k) = tie_break game node_total_ordering occ old_strategy valu !l in 
		strategy.(i) <- k;
		occ.(i).(j) <- occ.(i).(j) + 1
	);
	(strategy, occ)	

				
				
				
				
let improvement_policy_optimize_fair_sub_exp_tie_break game _ occ old_strategy valu l =
	let desc i = OptionUtils.get_some (pg_get_desc game i) in
	let find s =
		let i = ref 0 in
		while (!i < pg_size game) && (desc !i <> s) do
			incr i
		done;
		if !i < pg_size game then Some !i else None
	in
	let leadsto i j =
		let (_, path, _) = valu.(OptionUtils.get_some i) in
		TreeSet.mem (OptionUtils.get_some j) path
	in
	
	let n = ref 0 in
	while (find ("E" ^ string_of_int !n) != None) do incr n done;
	let n = !n in
	let s = ref "" in
	
	let state = Array.make n (0,0) in
	
	for i = 0 to n - 1 do
		let fndfst = ref 0 in
		if (leadsto (find ("a" ^ string_of_int i)) (find ("E" ^ string_of_int i))) &&
		   (leadsto (find ("b" ^ string_of_int i)) (find ("E" ^ string_of_int i))) then
		(
		   fndfst := 1;
           s := "1" ^ !s;
		)
        else s := "0" ^ !s;
		let fndsnd = ref 0 in
		if (i < n-1) && (leadsto (find ("v" ^ string_of_int i)) (find ("X" ^ string_of_int i))) &&
		   (leadsto (find ("w" ^ string_of_int i)) (find ("X" ^ string_of_int i))) then
		(
		   fndsnd := 1;
           s := "1" ^ !s;
		)
        else s := "0" ^ !s;
		s := "|" ^ !s;
		state.(i) <- (!fndsnd,!fndfst)
    done;
	
	let r = ref "" in
	let state' = Array.make (n+1) 0 in
	state'.(n) <- 0;
	for i = n - 1 downto 0 do
		let (s,f) = state.(i) in
		state'.(i) <- state'.(i+1)*s + (1-state'.(i+1)) * f;
		r := !r ^ string_of_int state'.(i)
	done;
	let idxmap = Array.make n 0 in
	let zeros = ref 0 in
	for i = 0 to n - 1 do
		if state'.(i) = 0 then incr zeros;
	done;
	let zeroidx = ref 0 in
	let oneidx = ref !zeros in
	for i = 0 to n - 1 do
		if state'.(i) = 1 then (
			idxmap.(i) <- !oneidx;
			incr oneidx
		)
		else (
			idxmap.(i) <- !zeroidx;
			incr zeroidx
		)
	done;
	let msg_tagged_nl v = message_autotagged_newline v (fun _ -> "ANALYZE") in
		msg_tagged_nl 2 (fun _ -> show_info game occ old_strategy valu n);

	
	let compare_nodes k (soulab0, souidx0) (tarlab0, taridx0) (oldlab0, oldidx0) (soulab1, souidx1) (tarlab1, taridx1) (oldlab1, oldidx1)
					  state idxmap strat =
		let nn = 2 * k in
		let f i = 2 * idxmap.(i) + state.(i+1) in
		let g i = 2 * idxmap.(i) + 1 - state.(i+1) in
		let h c d i = if fst (strat (c ^ string_of_int i)) = d then 1 else 0 in
		let mp = function
		|	(('m',_),_,_)       (* c *)            		    			-> -2 * nn
(*		|	(('p',_),_,_)                       		    			-> -1
		|	(('q',_),_,_)                       		    			-> -1 *)
		|	(('d',i),_,_)       (* g *)           		    			-> (5 + 2 * nn) * nn
  	|	(('a',_),_,('E',_)) (* e *) 		    			        -> -3 * nn
		|	(('w',_),_,('X',_)) (* e *)  		 				        -> -3 * nn
		|	(('b',_),_,('E',_)) (* d *)  		    			        -> -3 * nn
		|	(('v',_),_,('X',_)) (* d *)  		    			        -> -3 * nn
  	|	(('o',i),_,(_,_)) (* e *) 		    			        -> (if (h "a" 'E' i) = 1 then -4 else 1) * nn + (nn-i)
		|	(('p',i),_,(_,_)) (* e *)  		 				        -> (if (h "b" 'E' i) = 1 then -4 else 1) * nn + (nn-i)
		|	(('q',i),_,(_,_)) (* d *)  		    			        -> (if (h "w" 'X' i) = 1 then -4 else 1) * nn + (nn-i)
		|	(('r',i),_,(_,_)) (* d *)  		    			        -> (if (h "v" 'X' i) = 1 then -4 else 1) * nn + (nn-i)
		|	(('a',i),('E',_),_) (* e *)                                 -> (2 + (h "b" 'E' i) * nn + f i) * nn
		|	(('w',i),('X',_),_) (* e *)                                 -> (2 + (h "v" 'X' i) * nn + g i) * nn
		|	(('b',i),('E',_),_) (* d *)                                 -> (2 + (h "a" 'E' i) * nn + f i) * nn
		|	(('v',i),('X',_),_) (* d *)                                 -> (2 + (h "w" 'X' i) * nn + g i) * nn
		|	(('a',_),_,('Y',_)) (* e *) 		    			        -> -3 * nn
		|	(('w',_),_,('Y',_)) (* e *)  		 				        -> -3 * nn
		|	(('b',_),_,('Y',_)) (* d *)  		    			        -> -3 * nn
		|	(('v',_),_,('Y',_)) (* d *)  		    			        -> -3 * nn
  	|	(('a',_),_,_)       (* e *) 		 		      		    -> 1 * nn
		|	(('w',_),_,_)       (* e *)  		 		      			-> 1 * nn
		|	(('b',_),_,_)       (* d *)  		        				-> 1 * nn
		|	(('v',_),_,_)       (* d *)  		    				    -> 1 * nn
		|	(('l',_),_,_)           		    						-> (4 + 2 * nn) * nn
		|	(('g',_),_,_)       (* s *)		    					    -> (4 + 2 * nn) * nn
		|	(('s',_),_,_)       (* s *)		    					    -> (4 + 2 * nn) * nn
		|	(('f',_),_,_)       (* b *) 					            -> (3 + 2 * nn) * nn
		|	(('c',_),_,_)       (* h *) 					            -> (4 + 2 * nn) * nn
		|	(('u',_),_,_)       (* h *) 					            -> (4 + 2 * nn) * nn
		|	((x,_),_,_) -> failwith ("imp:" ^ Char.escaped x)
		in	
		compare (mp ((soulab0, souidx0), (tarlab0, taridx0), (oldlab0, oldidx0))) (mp ((soulab1, souidx1), (tarlab1, taridx1), (oldlab1, oldidx1)))
	in
	let f i =
		match pg_get_desc game i with
			None -> ('!', 0)
		|	Some s -> 
				if String.length s = 1
				then (String.get s 0, 0)
				else if String.get s 1 = '('
				then (String.get s 0, 0)
				else (String.get s 0, int_of_string (StringUtils.rest_string s 1))
	in
	let (i,j,k) = ListUtils.min_elt (fun (i0,j0,k0) (i1,j1,k1) ->
		compare_nodes (pg_size game) (f i0) (f k0) (f old_strategy.(i0)) (f i1) (f k1) (f old_strategy.(i1)) state' idxmap
		   (fun s -> f old_strategy.(OptionUtils.get_some (find s)))
	) l in
	switch_zadeh_exp_tie_break_callback n game old_strategy valu occ i k !r !s;
	(i,j,k)


		   

let strategy_improvement_optimize_fair_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	                     (improvement_policy_optimize_fair improvement_policy_optimize_fair_default_tie_break) (
		pg_map2 (fun _ (_, pl, tr, _, _) ->
			if pl = plr_Odd then [||]
			else Array.make (ns_size tr) 0
		) game
	) false "STRIMPR_FAIR";;


let strategy_improvement_optimize_fair_sub_exp_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position 
                         (improvement_policy_optimize_fair improvement_policy_optimize_fair_sub_exp_tie_break) (
		pg_map2 (fun _ (_, pl, tr, _, _) ->
			if pl = plr_Odd then [||]
			else Array.make (ns_size tr) 0
		) game
	) false "STRIMPR_FAIRSE";;



let initial_strategy_for_exp_game game =
	let n = ref 0 in
	let find s =
		let i = ref 0 in
		while (!i < pg_size game) && (pg_get_desc game !i <> Some s) do
			incr i
		done;
		if !i < pg_size game then !i else -1
	in
	while (find ("m" ^ string_of_int !n) != -1) do incr n done;
	let n = !n in
	let parse de =
		let s = OptionUtils.get_some de in
		(String.get s 0, int_of_string (StringUtils.rest_string s 1))
	in		
	let strategy = initial_strategy_by_best_reward game in
	pg_iterate (fun i (pr, pl, tr, _, de) ->
		if (pl = plr_Even) && (ns_size tr >= 2) then (
			let (c, j) = parse de in
			match c with
			| 'a' -> strategy.(i) <- find ("o" ^ string_of_int j)
			| 'b' -> strategy.(i) <- find ("p" ^ string_of_int j)
			| 'v' -> strategy.(i) <- find ("r" ^ string_of_int j)
			| 'w' -> strategy.(i) <- find ("q" ^ string_of_int j)
			| 'o' -> strategy.(i) <- find ("m" ^ string_of_int 1)
			| 'p' -> strategy.(i) <- find ("m" ^ string_of_int 1)
			| 'q' -> strategy.(i) <- find ("m" ^ string_of_int 1)
			| 'r' -> strategy.(i) <- find ("m" ^ string_of_int 1)
			| 'd' -> strategy.(i) <- find ("E" ^ string_of_int j)
			| 'g' -> strategy.(i) <- find ("c" ^ string_of_int j)
			| 's' -> strategy.(i) <- find ("m" ^ string_of_int 0)
			| 'm' -> strategy.(i) <- find (if j < n -1 then ("m" ^ string_of_int (j+1)) else "Y")
			| _ -> ()
		)
  ) game;
	strategy;;

let strategy_improvement_optimize_fair_exp_policy game =
	strategy_improvement game initial_strategy_for_exp_game node_total_ordering_by_position 
                         (improvement_policy_optimize_fair improvement_policy_optimize_fair_sub_exp_tie_break) (
		pg_map2 (fun _ (_, pl, tr, _, _) ->
			if pl = plr_Odd then [||]
			else Array.make (ns_size tr) 0
		) game
	) false "STRIMPR_FAIRSE";;



let strategy_improvement_optimize_fair_worstcase_policy game =
	let find s =
		let i = ref 0 in
		while (!i < pg_size game) && (pg_get_desc game !i <> Some s) do
			incr i
		done;
		if !i < pg_size game then Some !i else None
	in
	if (find "p0" != None) then strategy_improvement_optimize_fair_exp_policy game else strategy_improvement_optimize_fair_sub_exp_policy game;;


let register _ =
    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_fair_policy g)
        "switchfair" "sf" "Zadeh's fair policy iteration";

    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_fair_worstcase_policy g)
        "switchfairse" "sfse" "Zadeh's fair policy iteration with lower bound breaking ties";;

