open Basics ;;
open Paritygame ;;
open Univsolve;;
open Tcsmaths.MathField;;
open Tcsarray;;
open Tcslist;;


let array_max a less = ArrayUtils.max_elt (fun x y -> if less x y then -1 else 1) a
let list_max a less = ListUtils.max_elt (fun x y -> if less x y then -1 else 1) a


type 'a payoffgame = ('a * player * node array * string option) array

let paritygame_to_payoffgame (game: paritygame) (fld: 'a field) =
	let n = pg_size game in
	let (_, _, _, neg, _, _, _, _, pow, _, int_to) = fld in
	Array.init n (fun i ->
		      let pr = pg_get_priority game i in
		      let pl = pg_get_owner game i in
		      let tr = pg_get_successors game i in
		      let de = pg_get_desc game i in
		      (pow (neg (int_to n 1)) pr, pl, Array.of_list (ns_nodes tr), de)
	)
	
let get_paritygame_discount_factor (game: paritygame) (fld: 'a field) = 
	let (_, one, _, _, sub, mul, inv, _, pow, _, int_to) = fld in
	let n = int_to (pg_size game) 1 in
	let pr = pg_max_prio game in
	let num = mul (mul (int_to 4 1) (pow n 3)) (pow n pr) in
	let v = sub one (inv num) in
	v;;

let dpg_compute_valuation_for_strategy game discount strategy fld = 
	let (zero, one, sum, _, sub, mul, _, div, pow, _, _) = fld in
	let n = Array.length game in
	let valuation = Array.make n zero in
	let valid = Array.make n false in
	let walk = Array.make n false in
	let playlist = ref [] in
	for i = 0 to n - 1 do
		if not valid.(i) then (
			let j = ref i in
			while not (walk.(!j) || valid.(!j)) do
				walk.(!j) <- true;
				playlist := !j::!playlist;
				j := strategy.(!j)
			done;
			if not valid.(!j) then (
				let start = !j in
				let cyclevalue = ref zero in
				let len = ref 0 in
				j := (-1);
				while not (!j = start) do
					incr len;
					j := List.hd !playlist;
					playlist := List.tl !playlist;
					valid.(!j) <- false;
					walk.(!j) <- false;
					let (va, _, _, _) = game.(!j) in
					cyclevalue := sum va (mul discount !cyclevalue)
				done;
				valuation.(start) <- div !cyclevalue (sub one (pow discount !len));
				valid.(start) <- true
			);
			while not (!playlist = []) do
				j := List.hd !playlist;
				playlist := List.tl !playlist;
				valid.(!j) <- true;
				walk.(!j) <- false;
				let (va, _, _, _) = game.(!j) in
				valuation.(!j) <- sum va (mul discount valuation.(strategy.(!j)))
			done
		)
	done;
	valuation;;

	
let dpg_compute_valuation game discount strategy fld =
	let (_, _, _, _, _, _, _, _, _, cmp, _) = fld in
	let strat = Array.copy strategy in
	Array.iteri (fun i (_, pl, tr, _) ->
		if pl = plr_Odd then strat.(i) <- tr.(0)
	) game;

	let valuation = ref [||] in
	let changed = ref true in
	
	while !changed do
		valuation := dpg_compute_valuation_for_strategy game discount strat fld;
		changed := false;
		Array.iteri (fun i (_, pl, tr, _) ->
			if pl = plr_Odd then (
				let j = array_max tr (fun x y -> cmp !valuation.(x) !valuation.(y) > 0) in
				if cmp !valuation.(strat.(i)) !valuation.(j) > 0 then (
					strat.(i) <- j;
					changed := true
				)
			)
		) game;
	done;
	!valuation;;
	
	
let solve_discountedpayoffgame (game: 'a payoffgame) (discount: 'a) (fld: 'a field) =
	
	let msg_tagged v = message_autotagged v (fun _ -> "STRATIMPRDISC") in
	let msg_plain = message in

	let (zero, _, _, _, _, _, _, _, _, cmp, _) = fld in
	let n = Array.length game in
	
	let strategy = Array.init n (fun i ->
		let (_, pl, tr, _) = game.(i) in
		if pl = plr_Even then array_max tr (fun x y -> let (v, _, _, _) = game.(x) in
		                                               let (u, _, _, _) = game.(y) in
							       cmp v u < 0) 
		          else -1
	) in
	
	let valuation = ref [||] in
	
	let changed = ref true in
	let iterations = ref 0 in
	
	while !changed do

		incr iterations;
		msg_tagged 2 (fun _ -> "Iteration: " ^ string_of_int !iterations ^ "\r");
		
		valuation := dpg_compute_valuation game discount strategy fld;
		msg_tagged 3 (fun _ -> format_strategy strategy ^ "\n");
		changed := false;
		for i = 0 to n - 1 do
			let (_, pl, tr, _) = game.(i) in
			let j = if pl = plr_Even
				then array_max tr (fun x y -> cmp !valuation.(x) !valuation.(y) < 0) 
			        else array_max tr (fun x y -> cmp !valuation.(x) !valuation.(y) > 0)
			in
			if pl = plr_Odd then strategy.(i) <- j
			else if cmp !valuation.(strategy.(i)) !valuation.(j) < 0 then (
				strategy.(i) <- j;
				changed := true
			)
		done;
	done;

	msg_plain 2 (fun _ -> "\n");
	
	let solution = sol_make n in
	for i=0 to n-1 do
	  let wpl = if cmp !valuation.(i) zero < 0 then plr_Odd else plr_Even in
	  solution.(i) <- wpl;
	  let (_, pl, _, _) = game.(i) in
	  if wpl != pl then strategy.(i) <- -1
	done;
	
	(solution, strategy);;


let solve' game =
	let fld = rational_field in
	let payoffgame = paritygame_to_payoffgame game fld in
	let discountfactor = get_paritygame_discount_factor game fld in
	solve_discountedpayoffgame payoffgame discountfactor fld;;
		
let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;

let register _ =
    Solverregistry.register_solver solve "stratimprdisc" "sid" "solve parity game by reduction to discounted payoff games";;
