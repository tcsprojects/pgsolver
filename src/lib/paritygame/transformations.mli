open Basics;;
open Paritygame;;



(**************************************************************
 * Global Preprocessing                                       *
 **************************************************************)

val remove_useless_self_cycles_inplace: paritygame -> node list



(**************************************************************
 * Local Preprocessing                                        *
 **************************************************************)

val compact_prio_inplace: paritygame -> bool -> priority array

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

val compress_nodes: paritygame -> paritygame * node array * node array

val normal_form_translation: paritygame -> paritygame

val normal_form_revertive_translation: paritygame -> solution -> strategy -> solution * strategy

val uniquize_sorted_prios_inplace: paritygame -> unit

val uniquize_prios_inplace: paritygame -> unit

val min_max_swap_transformation: paritygame -> paritygame

						 
(* broken; unfixed; replaced by non-inplace version, specialised to sorting by priority comparison
val sort_game_inplace         : paritygame -> ((priority * player * nodeset * string option) ->
					       (priority * player * nodeset * string option) -> int) ->
				(player array * node array)
 *)
						 
val sort_game_by_prio : paritygame -> (paritygame * node array * node array)
