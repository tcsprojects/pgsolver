open Paritygame
open Mdp

type strat_impr_gen_args = string array

type strat_impr_gen = {
	ident: string;
	description: string;
	parity_game: (strat_impr_gen_args -> paritygame) option;
	generalized_mdp: (strat_impr_gen_args -> generalized_mdp) option;
}

val register_strat_impr_gen: strat_impr_gen -> unit

val mem_strat_impr_gen: string -> bool

val find_strat_impr_gen: string -> strat_impr_gen

val enum_strat_impr_gen: (strat_impr_gen -> unit) -> unit

val fold_strat_impr_gen: (strat_impr_gen -> 'a -> 'a) -> 'a -> 'a
