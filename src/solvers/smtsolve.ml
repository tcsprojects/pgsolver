open Paritygame ;;
open Basics ;;
open Univsolve ;;
open Solvers;;
open Tcstiming;;
open Z3;;
open Tcslist;;
open Tcsarray;;

let msg_tagged v = message_autotagged v (fun _ -> "SMTSOLVE");;
let msg_plain = message;;

let new_symbol ctx s l =
	Z3.mk_string_symbol ctx (s ^ IntListUtils.format l)

let new_int_symbol ctx int_sort s l =
	Z3.mk_const ctx (new_symbol ctx s l) int_sort

let new_bool_symbol ctx bool_sort s l =
	Z3.mk_const ctx (new_symbol ctx s l) bool_sort
	
let get_bool_assignm_failundef ctx model ast =
	match Z3.ast_to_string ctx (snd (Z3.eval ctx model ast)) with
		"true" -> true
	|	"false" -> false
	|	_ -> failwith "undefined variable!"

let get_bool_assignm ctx model ast =
	match Z3.ast_to_string ctx (snd (Z3.eval ctx model ast)) with
		"true" -> true
	|	_ -> false

let solve' game =
    let tim = SimpleTiming.init false in
    msg_tagged 2 (fun _ -> SimpleTiming.start tim; "Generating constraints ... ");
    
	let ctx       = Z3.mk_context_x [| ("MODEL_PARTIAL", "false") |] in
	
	let assrt c =
		msg_tagged 3 (fun _ -> "Adding: " ^ Z3.ast_to_string ctx c ^ "\n");
		Z3.assert_cnstr ctx c
	in
	
	let int_sort  = Z3.mk_int_sort ctx in
	let bool_sort = Z3.mk_bool_sort ctx in

	let n = pg_size game in
	let max_prio = pg_max_prio game in
	
	let var_win = Array.init n (fun i -> new_bool_symbol ctx bool_sort "win" [i]) in
	let var_desc_edge = Array.init n (fun i -> Array.map (fun j -> new_bool_symbol ctx bool_sort "des" [i;j]) (pg_get_successors game i)) in
	let var_pm = Array.init n (fun i -> Array.init (max_prio + 1) (fun p -> new_int_symbol ctx int_sort "pro" [i;p])) in
	
	pg_iterate (fun v -> fun (_,ow,tr,_,_) -> let plwin w = if ow = plr_Even then var_win.(w) else Z3.mk_not ctx var_win.(w) in
						  let plwin' w = if ow = plr_Odd then var_win.(w) else Z3.mk_not ctx var_win.(w) in

						  let pre = plwin v in
						  let post = Z3.mk_or ctx (Array.mapi (fun i w -> Z3.mk_and ctx [|plwin w; var_desc_edge.(v).(i)|]) tr) in
						  assrt (Z3.mk_implies ctx pre post);
		
						  let pre' = plwin' v in
						  let post' = Z3.mk_and ctx (Array.mapi (fun i w -> Z3.mk_and ctx [|plwin' w; var_desc_edge.(v).(i)|]) tr) in
						  assrt (Z3.mk_implies ctx pre' post')
		   ) game;
	
	plr_iterate (fun pl -> 
	  pg_iterate (fun v -> fun (pr,_,tr,_) ->
			       Array.iteri (fun i w ->
					    let pre = Z3.mk_and ctx [|(if pl = 0 then var_win.(v) else Z3.mk_not ctx var_win.(v)); var_desc_edge.(v).(i)|] in
					    for pr' = pr + 1 to max_prio do
					      if not (plr_benefits pr' pl) then (
						let post = Z3.mk_ge ctx var_pm.(v).(pr') var_pm.(w).(pr') in
						assrt (Z3.mk_implies ctx pre post);
					      )
					    done;
					    if not (plr_benefits pr pl) then (
	  				      let post2 = Z3.mk_gt ctx var_pm.(v).(pr) var_pm.(w).(pr) in
					      assrt (Z3.mk_implies ctx pre post2)
					    )
					   ) (Array.of_list (ns_nodes tr))
		     ) game);
	
	msg_plain 2 (fun _ -> SimpleTiming.stop tim; "done: " ^ SimpleTiming.format tim ^ "\n");

	let tim = SimpleTiming.init false in
	msg_tagged 2 (fun _ -> SimpleTiming.start tim; "SMT Solving ... ");

	let (result, model) = Z3.check_and_get_model ctx in

	msg_plain 2 (fun _ -> SimpleTiming.stop tim; "done: " ^ SimpleTiming.format tim ^ "\n");
    
	msg_plain 3 (fun _ -> Z3.model_to_string ctx model ^ "\n");

	(match result with
	    Z3.L_UNDEF -> failwith "impossible: undefined!"
	  | Z3.L_FALSE -> failwith "impossible: unsat!"
	  | _ -> ());

	let sol = Array.init n (fun i -> if get_bool_assignm_failundef ctx model var_win.(i) then 0 else 1) in
	let strat = Array.init n (fun i -> if sol.(i) = pg_get_owner game i
	                                   then let delta = pg_get_successors game i in
	                                   	let j = ref 0 in
	                                   	let fnd = ref false in
	                                   	while (not !fnd) && (!j < Array.length delta) do
	                                   	  fnd := get_bool_assignm ctx model var_desc_edge.(i).(!j);
	                                   	  if not !fnd then incr j
	                                   	done;
	                                   	if not !fnd then failwith "impossible: no strategy found!" else delta.(!j)
	                                   else -1) in

	Z3.del_model ctx model;
	Z3.del_context ctx;
	(sol, strat);;


let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;


let _ = register_solver solve "smtsolve" "smts" "directly solve the game by a difference logic predicate using an SMT solver";;

