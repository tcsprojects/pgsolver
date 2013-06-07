open Tcsset
open Paritygame
open Mdp

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
