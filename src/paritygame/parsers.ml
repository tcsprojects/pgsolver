open Paritygame;;
open Arrayparitygame;;
open Bytes;;


let parse_init_parity_game in_channel = 
	let game = ref (new array_pg 0) in
	let add v pr pl succs desc =
	  !game#set_priority v pr;
	  !game#set_owner v (if pl = 0 then plr_Even else plr_Odd);
	  !game#set_desc v (if desc = "" then None else Some desc);
	  List.iter (fun w -> !game#add_edge v w) succs
	in
	let queue = ref [] in
	let max_node = ref (-1) in
	let adder = ref (fun v pr pl succs desc ->
		queue := (v, pr, pl, succs, desc)::!queue;
		max_node := max !max_node v 
	) in
	let init_value = ref 0 in
	Tcsgameparser.parse_parity_game (fun n ->
		game := new array_pg n;
		adder := add
	) (fun i -> init_value := i) !adder (fun _ -> ()) in_channel;
	if !queue != [] then (
		game := new array_pg (!max_node + 1);
		List.iter (fun (v, pr, pl, succs, desc) -> add v pr pl succs desc) !queue
	);
	(!init_value, !game)



(*
let parse_init_parity_game in_channel =
	let game = new map_pg in
	let init_value = ref 0 in
	let edges = ref [] in
	Tcsgameparser.parse_parity_game (fun _ -> ()) (fun i -> init_value := i) (fun v pr pl succs desc ->
	    let pl = if pl = 0 then plr_Even else plr_Odd in
	    let desc = if desc = "" then None else Some desc in
	    game#set_node v pr pl ns_empty ns_empty desc;
	    edges := (v, succs)::!edges
	) (fun _ -> ()) in_channel;
    List.iter (fun (v, succs) -> List.iter (game#add_edge v) succs) !edges;
	(!init_value, game)
*)

let parse_parity_game in_channel = snd (parse_init_parity_game in_channel)
     
let parse_solution in_channel =
	let (sol, str) = Tcsgameparser.parse_explicit_parity_solution in_channel in
	(Array.map (fun pl -> if pl = 0 then plr_Even else plr_Odd) sol, str)
