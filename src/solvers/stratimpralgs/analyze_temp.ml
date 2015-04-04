open Basics;;
open Stratimpralgs;;
open Paritygame;;
open Tcsset;;
open Tcsbasedata;;
open Univsolve;;
open Tcsarray;;
open Tcslist;;
open Tcsstrings;;


module BitScheme = struct
	let models_scheme scheme bits = TreeSet.for_all (fun (i,j) -> (if i < Array.length bits then bits.(i) else 0) = j) scheme
	let leq_bits bits = TreeSet.of_array_def (Array.init (Bits.to_int bits + 1) Bits.of_int)
	let match_set bits scheme = TreeSet.filter (models_scheme scheme) (leq_bits bits)
	let flip_set bits i scheme = match_set bits (TreeSet.union (TreeSet.add (i,1) scheme) (TreeSet.of_array_def (Array.init i (fun j -> (j, 0)))))
	let unflip_set bits i scheme = match_set bits (TreeSet.union (TreeSet.add (i,0) scheme) (TreeSet.of_array_def (Array.init i (fun j -> (j, 0)))))
	let bit_flips bits i scheme = TreeSet.cardinal (flip_set bits i scheme)
	let max_flip_number bits i scheme = TreeSet.max_elt (TreeSet.add 0 (TreeSet.map2_def Bits.to_int (flip_set bits i scheme)))
	let max_unflip_number bits i scheme = TreeSet.max_elt (TreeSet.add 0 (TreeSet.map2_def Bits.to_int (unflip_set bits i scheme)))
end;;



let strategy_length game =
	let n = ref 0 in 
	while (
		try
			let _ =  pg_find_desc game (Some ("m" ^ string_of_int !n)) in
			true
		with Not_found -> false
	) do
		incr n 
	done;
	!n;;

let strategy_is game strategy v w =
	try
		strategy.(pg_find_desc game (Some v)) = pg_find_desc game (Some w)
	with Not_found -> false;;
	
	
	
	
type strategy_info = {
	len: int;
	b: int array;
	s0: int array;
	s1: int array;
	e0: int array;
	e1: int array;
	e0g: int array;
	e1g: int array;
	e0b: int array;
	e1b: int array;
	g: int array;
	good: int array;
	mu: int;
	r: int;
};;

let strategy_info game strategy = 
	let len = strategy_length game in
	let strat a i b j = if strategy_is game strategy (a ^ string_of_int i) (b ^ string_of_int j) then 1 else 0 in
	let b = Array.init len (fun i -> strat "m" i "d" i) in
	let s0 = Array.init len (fun i -> 1 - strat "g" i "m" 0) in
	let s1 = Array.init len (fun i -> if i < len - 1 then 1 - strat "s" i "m" 0 else 0) in
	let e0 = Array.init len (fun i -> strat "a" i "E" i * strat "b" i "E" i) in
	let e1 = Array.init len (fun i -> if i < len - 1 then strat "v" i "X" i * strat "w" i "X" i else 0) in
	let g = Array.init len (fun i -> strat "d" i "X" i) in
	let good = Array.make len 1 in
	for i = len - 2 downto 0 do
		good.(i) <- if b.(i) = 0 || (g.(i) = 0 && e0.(i) = 1 && b.(i+1) = 0) || (g.(i) = 1 && e1.(i) = 1 && b.(i+1) = 1 && good.(i+1) = 1) then 1 else 0
	done;
	let mu = if Bits.least_zero good = len then Bits.least_one (Bits.mult good (Bits.not b)) else Bits.least_one (Bits.mult good b) in
	let r = Bits.least_zero (Array.init (len-2) (fun i -> if b.(i) = 0 && (g.(i) = 0 || s1.(i) = 0 || good.(i+1) = 0) then 0 else 1)) in
	
	let e0g = Array.init len (fun i -> max (strat "o" i "d" 0 * strat "a" i "o" i) (strat "p" i "d" 0 * strat "b" i "p" i)) in
	let e0b = Array.init len (fun i -> max (strat "o" i "m" 1 * strat "a" i "o" i) (strat "p" i "m" 1 * strat "b" i "p" i)) in
	let e1g = Array.init len (fun i -> max (strat "q" i "d" 0 * strat "w" i "q" i) (strat "r" i "d" 0 * strat "v" i "r" i)) in
	let e1b = Array.init len (fun i -> max (strat "q" i "m" 1 * strat "w" i "q" i) (strat "r" i "m" 1 * strat "v" i "r" i)) in
	{
		len = len;
		b = b;
		s0 = s0;
		s1 = s1;
		e0 = e0;
		e1 = e1;
		g = g;
		good = good;
		mu = mu;
		r = r;
		e0g = e0g;
		e0b = e0b;
		e1g = e1g;
		e1b = e1b;
	};;
	
	
	
type valuation_info = {
	y: int TreeSet.t;
	w: int TreeSet.t array;
	b: int TreeSet.t array;
	g: int TreeSet.t array;
	d: int TreeSet.t array;
	s: int TreeSet.t array;
	x: int TreeSet.t array;
};;	

let valuation_info game strategy compare =
	let info = strategy_info game strategy in
	let n = info.len in
	let find a i = pg_find_desc game (Some (a ^ string_of_int i)) in
	let y = TreeSet.singleton compare (pg_find_desc game (Some "Y")) in
	let w = Array.init info.len (fun i -> TreeSet.add (find (if info.g.(i) = 0 then "c" else "z") i) (TreeSet.add (find "d" i) y)) in
	let lr_b = Array.make (info.len+1) (y, y) in
	for i = info.len - 1 downto 0 do
		lr_b.(i) <- (
			(if info.b.(i) = 1                                    then TreeSet.union (fst lr_b.(i+1)) w.(i) else fst lr_b.(i+1)),
			(if (i != info.mu && (info.b.(i) = 1 || i < info.mu)) then TreeSet.union (snd lr_b.(i+1)) w.(i) else snd lr_b.(i+1))
		)
	done;
	let b = Array.init (n+2) (fun i -> if i >= n then y else if i >= n || info.b.(i) = 0 || i = info.mu then fst lr_b.(i) else snd lr_b.(i)) in
	let g = Array.init n (fun i -> if info.s0.(i) = 0 then b.(0) else TreeSet.add (find "c" i) b.(i+2)) in
	let d = Array.init (info.r + 1) (fun i ->
		let result = ref (TreeSet.add (find "d" info.r) (
			if info.mu = 0 && ((info.g.(info.r) = 0 && info.s0.(info.r) = 0) || (info.g.(info.r) = 1 && info.s1.(info.r) = 0))
			then b.(0)
			else TreeSet.add (find "c" info.r) (if info.r + 2 >= n then y else b.(info.r+2))
		)) in
		for j = i to info.r - 1 do
			result := TreeSet.add (find "z" j) (TreeSet.add (find "d" j) !result)
		done;
		!result
	) in
	let s = Array.init (n-1) (fun i ->
			if info.s1.(i) = 0
			then b.(0)
			else TreeSet.add (find "z" i) (if info.b.(i+1) = 1 || i >= info.r then b.(i+1) else d.(i+1))
	) in
	let x0 = TreeSet.add (find "d" 0) (
		if info.g.(0) = 0 && (info.e0.(0) = 1 || (info.s0.(0) = 1 && info.b.(1) = 1))
		then g.(0)
		else if info.g.(0) = 1 && (info.e1.(0) = 1 || (info.s1.(0) = 1 && info.b.(1) = 0))
		then s.(0)
		else if info.b.(0) = 1
		then b.(0)
	  else b.(1)
	) in
	let x = Array.init n (fun i -> TreeSet.add (find "d" i) (
		if i = 0
		then x0
		else if info.g.(i) = 0 && (info.e0.(i) = 1 || (info.s0.(i) = 1 && i < n-1 && info.b.(i+1) = 1))
		then g.(i)
		else if i < n-1 && info.g.(i) = 1 && (info.e1.(i) = 1 || (info.s1.(i) = 1 && (i = n-1 || info.b.(i+1) = 0)))
		then s.(i)
		else if info.b.(i) = 1 || (i > info.r && info.s1.(i-1) = 1)
		then b.(i)
		else if info.s1.(i-1) = 1
		then d.(i)
	  else if (info.b.(0) = 0 && (info.g.(i) = 1 || info.e0g.(i) = 0) && (i = n-1 || info.g.(i) = 0 || info.e1g.(i) = 0)) ||
		        ((info.b.(0) = 0 || (info.good.(0) = 1 && ((info.g.(i) = 0 && info.e0b.(i) = 1) || (i < n-1 && info.g.(i) = 1 && info.e1b.(i) = 1))))
					 &&(info.b.(0) = 1 || info.mu > 0 || (info.g.(0) = 0 && info.e0.(0) = 1) || (info.g.(0) = 1 && info.e1.(0) = 1)))
		then b.(1)
		else x0
	)) in
	{
		y = y;
		w = w;
		b = b;
		g = g;
		d = d;
		s = s;
		x = x;
	};;



						
let strategy_leads_to game valu v w =
	try
		let i = pg_find_desc game (Some v) in
		let j = pg_find_desc game (Some w) in
		let (_, path, _) = valu.(i) in
		TreeSet.mem j path
	with Not_found -> false;;
	

let test_assumptions game strategy valu =
	let info = strategy_info game strategy in
	(* b_i never meets g_0, b_0, b_1 *)
	for i = 1 to info.len - 1 do
		if (strategy_leads_to game valu ("m" ^ string_of_int i) "d0") then print_string ("\n\nb_" ^ string_of_int i ^ " to g_0\n\n");
		if (strategy_leads_to game valu ("m" ^ string_of_int i) "m0") then print_string ("\n\nb_" ^ string_of_int i ^ " to b_0\n\n");
	done;
	for i = 2 to info.len - 1 do
		if (strategy_leads_to game valu ("m" ^ string_of_int i) "m1") then print_string ("\n\nb_" ^ string_of_int i ^ " to b_1\n\n");
	done;
	for i = 0 to info.len - 2 do
		if (info.s1.(i) = 1 && strategy_leads_to game valu ("s" ^ string_of_int i) "d0") then print_string ("\n\ns1_" ^ string_of_int i ^ " to g_0\n\n");
		if (info.s1.(i) = 1 && strategy_leads_to game valu ("s" ^ string_of_int i) "m0") then print_string ("\n\ns1_" ^ string_of_int i ^ " to b_0\n\n");
  done;
	(* g_0 never sees b_0 *)
	if (strategy_leads_to game valu "d0" "m0") then print_string ("\n\ng_0 to b_0\n\n");;



let test_valuation_assumptions game strategy valu n =	
	let info = strategy_info game strategy in
	let vinfo = valuation_info game strategy (TreeSet.get_compare (let (_, v_valu, _) = valu.(0) in v_valu)) in
	let check_valu desc s i assrt =
		let desc = desc ^ " " ^ string_of_int i ^ " (mu=" ^ string_of_int info.mu ^ ") " in
		let v = pg_find_desc game (Some (s ^ string_of_int i)) in
		let (_, v_valu, _) = valu.(v) in
		let va = TreeSet.filter (fun u -> pg_get_pr game u >= 11) v_valu in
		let ff = TreeSet.format (fun i -> OptionUtils.get_some (pg_get_desc game i)) in
		let diff = TreeSet.sym_diff va assrt in
		if not (TreeSet.is_empty diff)
		then print_string ("\n\n" ^ desc ^ " " ^ " " ^ ArrayUtils.format string_of_int info.b ^ " " ^ ff diff ^ " | " ^ ff va ^ " | " ^ ff assrt ^ "\n\n");
	in
	for i = 0 to n - 1 do
			check_valu "ladder" "m" i vinfo.b.(i);
			check_valu "left_up" "g" i vinfo.g.(i);
			if (i < n-1) then check_valu "right_up" "s" i vinfo.s.(i);
			check_valu "bisel" "d" i vinfo.x.(i);
 	done;;
	
	
	



let check_fair_exp_occ game strategy bits occ =
		let find x i = pg_find_desc game (Some (x ^ string_of_int i)) in
		let get x i y j = let (a,b) = (find x i,find y j) in occ.(a).(pg_get_tr_index_of game a b) in
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
				valid := eqs "a" i "o" i (ibfla0/2) && !valid;
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
				valid := eqs "s" i "u" i (ipbfl - 0 * bits.(i+1)) && !valid;
				valid := eqs "g" i "m" 0 (ipbfl - 0 * bits.(i+1)) && !valid;
				valid := eqs "s" i "m" 0 (ipbfl - 1 * bits.(i+1)) && !valid;
				if (bits.(i) = 1 && active.(i) = 1) then (
          valid := eqs "v" i "X" i (ibfla1/2 + ibfla1 mod 2) && !valid;
          valid := eqs "w" i "X" i (ibfla1/2) && !valid;
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

	  if (not (is_initial_strategy !curbits) && (is_initial_strategy (Bits.inc !curbits))) 
		then (
			curbits := Bits.inc !curbits;
			incr count_bits;
			msg_tagged_nl 1 (fun _ -> "-----" ^  "------" ^ ArrayUtils.format string_of_int !curbits ^ "--------" ^ string_of_int !count_bits ^ " of " ^ string_of_int (1 lsl n) ^ "\n");
			(*
			let check = check_fair_exp_occ game old_strategy !curbits occ in
			msg_tagged_nl 1 (fun _ -> "Occ - " ^ (if check then "yes" else "no") ^ "\n");
			*)
			last_active_phases := TreeSet.empty_def;
		);
		
		test_assumptions game old_strategy valu;
		test_valuation_assumptions game old_strategy valu n;
		