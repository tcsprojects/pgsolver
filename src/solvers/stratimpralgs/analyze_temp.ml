open Basics;;
open Stratimpralgs;;
open Paritygame;;
open Analyze_helper;;
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



		
						
let is_initial_strategy game strategy bits =
		let find x i = pg_find_desc game (Some (x ^ string_of_int i)) in
		let n = Array.length bits - 1 in
		let valid = ref "" in
		let v x y asrt =
			let r = if strategy.(x) = y then 1 else 0 in 
			if (r != asrt) then valid := !valid ^ (match pg_get_desc game x with None -> "" | Some y -> y) ^ ", ";
			()
		in
	  for i = 0 to n - 1 do
			v (find "m" i) (find "d" i) bits.(i);
			v (find "g" i) (find "c" i) (1 - bits.(i+1));
			if (i < n-1) then (
    			v (find "s" i) (find "u" i) bits.(i+1);
			);
			if (bits.(i) = 1) then (
   			v (find "d" i) (find "E" i) (1 - bits.(i+1));
				if (bits.(i+1) = 0) then (
     			v (find "a" i) (find "E" i) 1;
     			v (find "b" i) (find "E" i) 1;
				) else (
     			v (find "v" i) (find "X" i) 1;
     			v (find "w" i) (find "X" i) 1;
				)
			);
	  done;
		!valid = "";;
	

	
let test_assumptions game strategy valu =
	let findf x = pg_find_desc game (Some x) in
	let find x = try pg_find_desc game (Some x) with Not_found -> failwith ("Not found: " ^ x ^ "\n") in
 	let leadsto i j =
		let (_, path, _) = valu.(i) in
		TreeSet.mem (j) path
	in
	let sigma_is s t = strategy.(find s) = find t in
	let n = ref 0 in
	while (try let _ = findf ("m" ^ string_of_int !n) in true with Not_found -> false) do incr n done;
	let n = !n in
	(* b_i never meets g_0, b_0, b_1 *)
	for i = 0 to n - 1 do
		let bi = find ("m" ^ string_of_int i) in
		let g0 = find ("d0") in
		let b0 = find ("m0") in
		let b1 = find ("m1") in
		if (i > 0 && leadsto bi g0) then print_string ("\n\nb_i to g_0\n\n");
		if (i > 0 && leadsto bi b0) then print_string ("\n\nb_i to b_0\n\n");
		if (i > 1 && leadsto bi b1) then print_string ("\n\nb_i to b_1\n\n");
		if (i < n-1) then (
			let si = find ("s" ^ string_of_int i) in
			if (strategy.(si) != b0) then (
				if (leadsto si g0) then print_string ("\n\ns_i to g_0\n\n");
				if (leadsto si g0) then print_string ("\n\ns_i to b_0\n\n");
			);
		);
	done;
	(* g_0 never sees b_0 *)
	let g0 = find ("d0") in
	let b0 = find ("m0") in
	let s0 = find ("s0") in
	if (leadsto g0 b0) then print_string ("\n\ng_0 to b_0\n\n");
	if (not (sigma_is "s0" "m0") && (leadsto s0 g0)) then print_string("\ncheck\n");;

	
let bitstrategyinfo game strategy valu =
	let findf x = pg_find_desc game (Some x) in
	let find x = try pg_find_desc game (Some x) with Not_found -> failwith ("Not found: " ^ x ^ "\n") in
	let sigma_is s t = strategy.(find s) = find t in
 	let leadsto i j =
		let (_, path, _) = valu.(i) in
		TreeSet.mem (j) path
	in
	let n = ref 0 in
	while (try let _ = findf ("m" ^ string_of_int !n) in true with Not_found -> false) do incr n done;
	let n = !n in
	let bitinfo = Array.make n (false, false) in (* good, set *)
	for i = n - 1 downto 0 do
		if (sigma_is ("m" ^ string_of_int i) ("d" ^ string_of_int i)) then (
			if (i = n-1) then (
				bitinfo.(i) <- (true, true)
			) else (
				if (leadsto (find ("m" ^ string_of_int i)) (find ("c" ^ string_of_int i)))
				then let (is_good, is_set) = bitinfo.(i+1) in bitinfo.(i) <- (not is_set, true)
				else if (leadsto (find ("m" ^ string_of_int i)) (find ("d" ^ string_of_int (i+1))))
				then let (is_good, is_set) = bitinfo.(i+1) in bitinfo.(i) <- (is_set && is_good, true)
				else bitinfo.(i) <- (false, true)
			)
		) else (
			bitinfo.(i) <- (true, false)
		)
	done;
	bitinfo;;
	
let strategy_mu game strategy valu =
	let bitinfo =	bitstrategyinfo game strategy valu in
	let all_good = ref true in
	let n = Array.length bitinfo in
	let least_good_one = ref n in
	let least_good_zero = ref n in
	for i = n - 1 downto 0 do
		let (is_good, is_set) = bitinfo.(i) in
		all_good := !all_good && is_good;
		if (is_good && is_set) then least_good_one := i;
		if (is_good && not is_set) then least_good_zero := i;
	done;
	if (!all_good) then !least_good_zero else !least_good_one;;


let test_valuation_assumptions game strategy valu n =	
	let bitinfo = bitstrategyinfo game strategy valu in
	let find x = try pg_find_desc game (Some x) with Not_found -> failwith ("Not found: " ^ x ^ "\n") in
	let sigma_is s t = strategy.(find s) = find t in
	let sigmis s i t j = sigma_is (s ^ string_of_int i) (if j >= 0 then t ^ string_of_int j else t) in
	let mbits = Array.init (n+1) (fun i -> if i < n && (sigma_is ("m" ^ string_of_int i) ("d" ^ string_of_int i)) then 1 else 0) in
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
			print_string ("\n\n" ^ s ^ " " ^ " " ^ ArrayUtils.format string_of_int mbits ^ " " ^ ff diff ^ " | " ^ ff va1 ^ " | " ^ ff va2 ^ "\n\n")
		)
	in
	let k = strategy_mu game strategy valu in
		
	let add_walkthrough nodes j =
		let temp = ref nodes in
			temp := TreeSet.add (find ("d" ^ string_of_int j) ) !temp;
			if sigma_is ("d" ^ string_of_int j) ("E" ^ string_of_int j)  then
				temp := TreeSet.add (find ("c" ^ string_of_int j) ) !temp
			else
				temp := TreeSet.add (find ("z" ^ string_of_int j) ) !temp;
		!temp
	in
		
	let left_b_valus = Array.make (n+1) (TreeSet.add (find "Y") empty_valu) in
	let right_b_valus = Array.make (n+1) (TreeSet.add (find "Y") empty_valu) in
	for i = n - 1 downto 0 do
		left_b_valus.(i) <- left_b_valus.(i+1);
		if (mbits.(i) = 1)
		then left_b_valus.(i) <- add_walkthrough left_b_valus.(i) i;
		right_b_valus.(i) <- right_b_valus.(i+1);
		if (i != k && (mbits.(i) = 1 || i < k))
		then right_b_valus.(i) <- add_walkthrough right_b_valus.(i) i
	done;
		
	let bvalus = Array.init (n+2) (fun i -> if i >= n then (TreeSet.add (find "Y") empty_valu) else if mbits.(i) = 0 || i = k then left_b_valus.(i) else right_b_valus.(i)) in
	let s = ref n in
	for i = n - 1 downto 0 do
	  if (mbits.(i) = 0)
		then s := i
  done;
	let s = !s in

	let r = ref (n-2) in
	for i = n - 2 downto 0 do
	  if mbits.(i) = 0 && (
		    sigma_is ("d" ^ string_of_int i) ("E" ^ string_of_int i) ||
				sigma_is ("s" ^ string_of_int i) ("m0") ||
				not (fst bitinfo.(i+1)) 		   
		)
		then r := i
  done;
	let r = !r in
	let t_test = (sigma_is ("d" ^ string_of_int r) ("E" ^ string_of_int r) && sigma_is ("g" ^ string_of_int r) ("m0")) ||
		(sigma_is ("d" ^ string_of_int r) ("X" ^ string_of_int r) && sigma_is ("s" ^ string_of_int r) ("m0"))  in
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
			if sigma_is ("s" ^ string_of_int i) ("m0")
			then bvalus.(0)
			else if mbits.(i+1) = 1 || i >= r
			then TreeSet.add (find ("z" ^ string_of_int i)) bvalus.(i+1)
			else if s != 0 || not t_test
			then TreeSet.add (find ("z" ^ string_of_int i) ) dvalusy.(i+1)
			else TreeSet.add (find ("z" ^ string_of_int i) ) dvalusx.(i+1)
	) in
	let gvalus = Array.init n (fun i -> if sigma_is ("g" ^ string_of_int i) ("m0") then bvalus.(0) else TreeSet.add (find ("c" ^ string_of_int i)) bvalus.(i+2)) in
	let d_zero = ref (TreeSet.add (find "d0") (TreeSet.add (find "Y") empty_valu)) in
	let d_comp i =
		if (sigma_is ("d" ^ string_of_int i) ("E" ^ string_of_int i)) && (sigma_is ("a" ^ string_of_int i) ("E" ^ string_of_int i)) && (sigma_is ("b" ^ string_of_int i) ("E" ^ string_of_int i))
		then TreeSet.add (find ("d" ^ string_of_int i)) gvalus.(i)
		else if i < n-1 && (sigma_is ("d" ^ string_of_int i) ("X" ^ string_of_int i)) && (sigma_is ("w" ^ string_of_int i) ("X" ^ string_of_int i)) && (sigma_is ("v" ^ string_of_int i) ("X" ^ string_of_int i))
		then TreeSet.add (find ("d" ^ string_of_int i)) svalus.(i)
		else
		if mbits.(i) = 1 || (i > r && not (sigma_is ("s" ^ string_of_int (i-1)) ("m0")))
		then bvalus.(i)
		else if (i > 0) && not (sigma_is ("s" ^ string_of_int (i-1)) ("m0")) then (
			if s != 0 || not t_test
			then dvalusy.(i)
			else dvalusx.(i)
		)
		else if i < n-1 && (sigma_is ("d" ^ string_of_int i) ("X" ^ string_of_int i)) && (not (sigma_is ("s" ^ string_of_int i) ("m0"))) && mbits.(i+1) = 0
		then TreeSet.add (find ("d" ^ string_of_int i)) svalus.(i)
		else if (sigma_is ("d" ^ string_of_int i) ("E" ^ string_of_int i)) && not (sigma_is ("g" ^ string_of_int i) ("m0")) && mbits.(i+1) = 1
		then TreeSet.add (find ("d" ^ string_of_int i)) gvalus.(i)
	  else if i = 0
		then TreeSet.add (find ("d" ^ string_of_int i)) bvalus.(1)

		else (
			 let reachable_d0 = ((sigmis "d" i "E" i) && (((sigmis "o" i "d" 0) && (sigmis "a" i "o" i || sigmis "b" i "o" i)) || ((sigmis "p" i "d" 0) && (sigmis "a" i "p" i || sigmis "b" i "p" i)))) ||
              (i < n-1 && ((sigmis "d" i "X" i) && (((sigmis "q" i "d" 0) && (sigmis "v" i "q" i || sigmis "w" i "q" i)) || ((sigmis "r" i "d" 0) && (sigmis "v" i "r" i || sigmis "w" i "r" i))))) in
			 let reachable_m1 = ((sigmis "d" i "E" i) && (((sigmis "o" i "m" 1) && (sigmis "a" i "o" i || sigmis "b" i "o" i)) || ((sigmis "p" i "m" 1) && (sigmis "a" i "p" i || sigmis "b" i "p" i)))) ||
              (i < n-1 && ((sigmis "d" i "X" i) && (((sigmis "q" i "m" 1) && (sigmis "v" i "q" i || sigmis "w" i "q" i)) || ((sigmis "r" i "m" 1) && (sigmis "v" i "r" i || sigmis "w" i "r" i))))) in
			 let reachable_y = ((sigmis "d" i "E" i) && ((sigmis "a" i "Y" (-1)) || (sigmis "b" i "Y" (-1)) || ((sigmis "o" i "Y" (-1)) && (sigmis "a" i "o" i || sigmis "b" i "o" i)) || ((sigmis "p" i "Y" (-1)) && (sigmis "a" i "p" i || sigmis "b" i "p" i)))) ||
			       (i < n-1 && ((sigmis "d" i "X" i) && ((sigmis "v" i "Y" (-1)) || (sigmis "w" i "Y" (-1)) || ((sigmis "q" i "Y" (-1)) && (sigmis "v" i "q" i || sigmis "w" i "q" i)) || ((sigmis "r" i "Y" (-1)) && (sigmis "v" i "r" i || sigmis "w" i "r" i))))) in

	  if (reachable_y)
		then TreeSet.add (find ("d" ^ string_of_int i))(TreeSet.add (find "Y") empty_valu)
		
		else if mbits.(0) = 0 && not reachable_d0
		then TreeSet.add (find ("d" ^ string_of_int i)) bvalus.(1)
		
		else if mbits.(0) = 1 && not reachable_m1
		then TreeSet.add (find ("d" ^ string_of_int i)) !d_zero
		
		else if mbits.(0) = 1 && not (fst bitinfo.(0))
		then TreeSet.add (find ("d" ^ string_of_int i)) !d_zero
		
		else if mbits.(0) = 0 && k = 0 && not ((sigmis "d" 0 "E" 0 && sigmis "a" 0 "E" 0 && sigmis "b" 0 "E" 0) || (sigmis "d" 0 "X" 0 && sigmis "v" 0 "X" 0 && sigmis "w" 0 "X" 0))
		then TreeSet.add (find ("d" ^ string_of_int i)) !d_zero

		else TreeSet.add (find ("d" ^ string_of_int i)) bvalus.(1)
		)
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
		