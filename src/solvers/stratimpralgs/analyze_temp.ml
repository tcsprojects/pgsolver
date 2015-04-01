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
	

		
						
let is_initial_strategy game strategy bits = Array.init (Array.length bits - 1) (fun i -> bits.(i)) = (strategy_info game strategy).b;;

	
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
	let find x = try pg_find_desc game (Some x) with Not_found -> failwith ("Not found: " ^ x ^ "\n") in
	let node_valu u =
		let (_, v_valu, _) = valu.(u) in v_valu
  in
	let empty_valu =
		TreeSet.empty (TreeSet.get_compare (node_valu 0))
	in  
	let filter_valu va p =
		TreeSet.filter (fun u ->pg_get_pr game u >= p) va
	in
	let diff_valu va1 va2 p =
		TreeSet.sym_diff (filter_valu va1 p) (filter_valu va2 p)
	in
	let check_valu s va1 va2 p =
		let ff = TreeSet.format (fun i -> OptionUtils.get_some (pg_get_desc game i)) in
		let va1 = filter_valu va1 p in
		let va2 = filter_valu va2 p in
		let diff = diff_valu va1 va2 p in
		if (not (TreeSet.is_empty diff)) then (
			print_string ("\n\n" ^ s ^ " " ^ " " ^ ArrayUtils.format string_of_int info.b ^ " " ^ ff diff ^ " | " ^ ff va1 ^ " | " ^ ff va2 ^ "\n\n")
		)
	in
	let k = info.mu in
		
	let add_walkthrough nodes j =
		let temp = ref nodes in
			temp := TreeSet.add (find ("d" ^ string_of_int j) ) !temp;
			if info.g.(j) = 0 then
				temp := TreeSet.add (find ("c" ^ string_of_int j) ) !temp
			else
				temp := TreeSet.add (find ("z" ^ string_of_int j) ) !temp;
		!temp
	in
		
	let left_b_valus = Array.make (n+1) (TreeSet.add (find "Y") empty_valu) in
	let right_b_valus = Array.make (n+1) (TreeSet.add (find "Y") empty_valu) in
	for i = n - 1 downto 0 do
		left_b_valus.(i) <- left_b_valus.(i+1);
		if (info.b.(i) = 1)
		then left_b_valus.(i) <- add_walkthrough left_b_valus.(i) i;
		right_b_valus.(i) <- right_b_valus.(i+1);
		if (i != k && (info.b.(i) = 1 || i < k))
		then right_b_valus.(i) <- add_walkthrough right_b_valus.(i) i
	done;
		
	let bvalus = Array.init (n+2) (fun i -> if i >= n then (TreeSet.add (find "Y") empty_valu) else if i >= n || info.b.(i) = 0 || i = k then left_b_valus.(i) else right_b_valus.(i)) in

	let r = info.r in
	let t_test = (info.g.(r) = 0 && info.s0.(r) = 0) || (info.g.(r) = 1 && info.s1.(r) = 0)  in
	let dvalusy = Array.init (r+1) (fun i ->
		let temp = ref (if r+2 >= n then TreeSet.add (find "Y") empty_valu else bvalus.(r+2)) in
		temp := TreeSet.add (find ("d" ^ string_of_int r) ) !temp;
		temp := TreeSet.add (find ("c" ^ string_of_int r) ) !temp;
		for j = i to r - 1 do
			temp := TreeSet.add (find ("d" ^ string_of_int j) ) !temp;
			temp := TreeSet.add (find ("z" ^ string_of_int j) ) !temp;
		done;
		!temp
	) in
	let dvalusx = Array.init (r+1) (fun i ->
		let temp = ref (bvalus.(0)) in
		temp := TreeSet.add (find ("d" ^ string_of_int r) ) !temp;
		for j = i to r - 1 do
			temp := TreeSet.add (find ("d" ^ string_of_int j) ) !temp;
			temp := TreeSet.add (find ("z" ^ string_of_int j) ) !temp;
		done;
		!temp
	) in
	let svalus = Array.init (n-1) (fun i ->
			if info.s1.(i) = 0
			then bvalus.(0)
			else if info.b.(i+1) = 1 || i >= r
			then TreeSet.add (find ("z" ^ string_of_int i)) bvalus.(i+1)
			else if k != 0 || not t_test
			then TreeSet.add (find ("z" ^ string_of_int i) ) dvalusy.(i+1)
			else TreeSet.add (find ("z" ^ string_of_int i) ) dvalusx.(i+1)
	) in
	let gvalus = Array.init n (fun i -> if info.s0.(i) = 0 then bvalus.(0) else TreeSet.add (find ("c" ^ string_of_int i)) bvalus.(i+2)) in
	let d_zero = ref (TreeSet.add (find "d0") (TreeSet.add (find "Y") empty_valu)) in
	let d_comp i =
		if info.g.(i) = 0 && info.e0.(i) = 1
		then TreeSet.add (find ("d" ^ string_of_int i)) gvalus.(i)
		else if i < n-1 && info.g.(i) = 1 && info.e1.(i) = 1
		then TreeSet.add (find ("d" ^ string_of_int i)) svalus.(i)
		else
		if info.b.(i) = 1 || (i > r && info.s1.(i-1) = 1)
		then bvalus.(i)
		else if (i > 0) && info.s1.(i-1) = 1 then (
			if k != 0 || not t_test
			then dvalusy.(i)
			else dvalusx.(i)
		)
		else if i < n-1 && info.g.(i) = 1 && info.s1.(i) = 1 && (i >= n-1 || info.b.(i+1) = 0)
		then TreeSet.add (find ("d" ^ string_of_int i)) svalus.(i)
		else if info.g.(i) = 0 && info.s0.(i) = 1 && (i < n-1 && info.b.(i+1) = 1)
		then TreeSet.add (find ("d" ^ string_of_int i)) gvalus.(i)
	  else if i = 0
		then TreeSet.add (find ("d" ^ string_of_int i)) bvalus.(1)
		else if info.b.(0) = 0 && not ((info.g.(i) = 0 && info.e0g.(i) = 1) || (i < n-1 && info.g.(i) = 1 && info.e1g.(i) = 1))
		then TreeSet.add (find ("d" ^ string_of_int i)) bvalus.(1)
		else if info.b.(0) = 1 && not ((info.g.(i) = 0 && info.e0b.(i) = 1) || (i < n-1 && info.g.(i) = 1 && info.e1b.(i) = 1))
		then TreeSet.add (find ("d" ^ string_of_int i)) !d_zero
		else if info.b.(0) = 1 && info.good.(0) = 0
		then TreeSet.add (find ("d" ^ string_of_int i)) !d_zero
		else if info.b.(0) = 0 && k = 0 && (info.g.(0) = 1 || info.e0.(0) = 0) && (info.g.(0) = 0 || info.e1.(0) = 0)
		then TreeSet.add (find ("d" ^ string_of_int i)) !d_zero
		else TreeSet.add (find ("d" ^ string_of_int i)) bvalus.(1)
	in
	d_zero := d_comp 0;
	let dvalus = Array.init n (fun i ->
		d_comp i
	) in
	for i = 0 to n - 1 do
				check_valu ("ladder " ^ string_of_int i ^ " (mu=" ^ string_of_int k ^ ") ") (node_valu (find ("m" ^ string_of_int i))) bvalus.(i) 11;

				check_valu ("left_up " ^ string_of_int i ^ " (mu=" ^ string_of_int k ^ ") ") (node_valu (find ("g" ^ string_of_int i))) gvalus.(i) 11;

				if (i < n-1)
				then check_valu ("right_up " ^ string_of_int i ^ " (mu=" ^ string_of_int k ^ ") ") (node_valu (find ("s" ^ string_of_int i))) svalus.(i) 11;
				
				check_valu ("bisel " ^ string_of_int i ^ " (mu=" ^ string_of_int k ^ ") ") (node_valu (find ("d" ^ string_of_int i))) dvalus.(i) 11;
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

		let msg_tagged_nl v = message_autotagged_newline v (fun _ -> "ANALYZE") in

		if (Array.length !curbits = 0)
		then curbits := Bits.zero (n+1);

	  if (not (is_initial_strategy game old_strategy !curbits) && (is_initial_strategy game old_strategy (Bits.inc !curbits))) 
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
		