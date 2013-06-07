open Basics;;
open Paritygame;;



(**************************************************************
 * Global Preprocessing                                       *
 **************************************************************)

val remove_useless_self_cycles_inplace: paritygame -> int list



(**************************************************************
 * Local Preprocessing                                        *
 **************************************************************)

val compact_prio_inplace: paritygame -> bool -> int array

val priority_propagation_inplace: paritygame -> unit

val anti_propagation_inplace: paritygame -> unit



(**************************************************************
 * Game Transformations                                       *
 **************************************************************)

val single_scc_transformation: paritygame -> paritygame

val anti_priority_compactation_transformation: paritygame -> paritygame

val cheap_escape_cycles_transformation: paritygame -> bool -> paritygame

val total_transformation_inplace: paritygame -> unit

val total_revertive_restriction_inplace: paritygame -> strategy -> unit

val alternating_transformation: paritygame -> bool -> paritygame

val alternating_revertive_restriction: paritygame -> paritygame -> solution -> strategy -> solution * strategy

val partialpg_alternating_transformation: partial_paritygame ->  partial_paritygame

val partialpg_alternating_revertive_restriction: partial_solution -> partial_solution

val increase_priority_occurrence: paritygame -> paritygame

val prio_alignment_transformation: paritygame -> paritygame

val dummy_transformation: paritygame -> paritygame

val shift_game: paritygame -> int -> paritygame

val combine_games: paritygame list -> paritygame

val bouncing_node_transformation: paritygame -> paritygame

val compress_nodes: paritygame -> paritygame * int array * int array

val sort_game_inplace : paritygame -> ((int * int * int array * string option) -> (int * int * int array * string option) -> int) -> (int array * int array)

val sort_game_by_prio_inplace : paritygame -> (int array * int array)

val normal_form_translation: paritygame -> paritygame

val normal_form_revertive_translation: paritygame -> solution -> strategy -> solution * strategy

val uniquize_sorted_prios_inplace: paritygame -> unit

val uniquize_prios_inplace: paritygame -> unit

val min_max_swap_transformation: paritygame -> paritygame
