open Paritygame
open Tcsset

val enable_exp_bit_count: bool ref

val get_last_exp_bit_count: unit -> int

val get_last_iteration_count: unit -> int

val _strat_impr_callback: (strategy -> int -> unit) option ref

type node_total_ordering_fun = Paritygame.paritygame -> int -> int -> int

val node_total_ordering_by_position : node_total_ordering_fun

type node_valuation = int * int TreeSet.t * int
type game_valuation = node_valuation array

val evaluate_strategy :
  paritygame -> node_total_ordering_fun -> strategy -> game_valuation

val strategy_improvable :
  paritygame -> node_total_ordering_fun -> strategy -> game_valuation -> bool

val initial_strategy_by_best_reward : paritygame -> strategy

val improvement_policy_optimize_all_locally :
  paritygame -> node_total_ordering_fun -> strategy -> game_valuation -> strategy
 
val strategy_improvement_optimize_all_locally_policy: paritygame -> solution * strategy
 
val best_decision_by_valuation_ordering :
  paritygame -> node_total_ordering_fun -> game_valuation -> int -> int

val node_valuation_ordering :
  paritygame -> node_total_ordering_fun -> node_valuation -> node_valuation -> int

val compute_counter_strategy:
  paritygame -> strategy -> strategy