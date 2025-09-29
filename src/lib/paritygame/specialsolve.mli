open Basics;;
open Paritygame;;

type compact_sol_strat = (node * player * node) list
					   
val compact_sol_strat_to_sol_strat: paritygame -> compact_sol_strat -> solution * strategy

val find_useful_self_cycles: paritygame -> compact_sol_strat

val solve_cycle_scc: paritygame -> solution * strategy

val solve_single_player_scc: paritygame -> player -> solution * strategy

val solve_single_parity_scc: paritygame -> player -> solution * strategy

(* Instead use the functions in univsolve. The only reason why you would want to use the following two function is to avoid the use of the universal solving process. *)
val compute_winning_nodes_for_direct: paritygame -> player -> solution * strategy

val compute_winning_nodes_direct: paritygame -> strategy -> player -> node list
