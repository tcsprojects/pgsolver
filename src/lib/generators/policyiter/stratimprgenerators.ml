open Tcsset
open Paritygame
open Mdp



module SymbolicParityGame = struct

	open Tcsset
	open Tcsgraph

	type 'a symbolic_paritygame = ('a, int) Hashtbl.t * dynamic_paritygame

	let create_new x = (Hashtbl.create 10, DynamicGraph.make ())

	let to_paritygame (ht, gr) =
		let pg = pg_create (Hashtbl.length ht) in
		Hashtbl.iter (fun _ ind -> 
			let (pr, pl, desc) = DynamicGraph.get_node_data ind gr in
			pg_set_priority pg ind pr;
			pg_set_owner pg ind pl;
			pg_set_desc pg ind desc;
			TreeSet.iter (fun w -> pg_add_edge pg ind w) (DynamicGraph.get_node_succ ind gr)
		) ht;
		pg

	let internal_add (ht, gr) symb pr pl desc override =
		if Hashtbl.mem ht symb
		then let ind = Hashtbl.find ht symb in
			 if override then DynamicGraph.set_node_data ind (pr, pl, desc) gr;
			 ind
		else let ind = Hashtbl.length ht in
			 Hashtbl.add ht symb ind;
			 DynamicGraph.add_node ind (pr, pl, desc) gr;
			 ind
   
    let touch_node (ht, gr) symb =
   		let _ = internal_add (ht, gr) symb (-1) plr_Even None false in ()

	let add_node (ht, gr) symb pr pl tr desc =
		let ind = internal_add (ht, gr) symb pr pl desc true in
		Array.iter (fun symb' ->
			let ind' = internal_add (ht, gr) symb' (-1) plr_Even None false in
			DynamicGraph.add_edge ind ind' gr
		) tr

end;;


type strat_impr_gen_args = string array

type strat_impr_gen = {
	ident: string;
	description: string;
	parity_game: (strat_impr_gen_args -> paritygame) option;
	generalized_mdp: (strat_impr_gen_args -> generalized_mdp) option;
}

let strat_impr_gen_map = ref TreeMap.empty_def

let register_strat_impr_gen gen =
	if TreeMap.mem gen.ident !strat_impr_gen_map
	then failwith ("generator `" ^ gen.ident ^ "' already registered!\n")
	else strat_impr_gen_map := TreeMap.add gen.ident gen !strat_impr_gen_map

let mem_strat_impr_gen ident = TreeMap.mem ident !strat_impr_gen_map

let find_strat_impr_gen ident = TreeMap.find ident !strat_impr_gen_map

let enum_strat_impr_gen f = TreeMap.iter (fun _ -> f) !strat_impr_gen_map

let fold_strat_impr_gen f = TreeMap.fold (fun _ -> f) !strat_impr_gen_map
