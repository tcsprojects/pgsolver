open Paritygame;;
open Bytes;;
  
let parse_init_parity_game in_channel = 
	let game = ref (pg_create 0) in
	let add v pr pl succs desc =
	  pg_set_priority !game v pr;
	  pg_set_owner !game v (if pl = 0 then plr_Even else plr_Odd);
	  pg_set_desc !game v (if desc = "" then None else Some desc);
	  List.iter (fun w -> pg_add_edge !game v w) succs
	in
	let queue = ref [] in
	let max_node = ref (-1) in
	let adder = ref (fun v pr pl succs desc ->
		queue := (v, pr, pl, succs, desc)::!queue;
		max_node := max !max_node v 
	) in
	let init_value = ref 0 in
	Tcsgameparser.parse_parity_game (fun n ->
		game := pg_create n;
		adder := add
	) (fun i -> init_value := i) !adder (fun _ -> ()) in_channel;
	if !queue != [] then (
		game := pg_create (!max_node + 1);
		List.iter (fun (v, pr, pl, succs, desc) -> add v pr pl succs desc) !queue
	);
	(!init_value, !game)

let parse_parity_game in_channel = snd (parse_init_parity_game in_channel)
     
let parse_solution in_channel =
	let (sol, str) = Tcsgameparser.parse_explicit_parity_solution in_channel in
	(Array.map (fun pl -> if pl = 0 then plr_Even else plr_Odd) sol, str)
