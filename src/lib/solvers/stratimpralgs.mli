open Paritygame
open Tcsset

val register: unit -> unit

val enable_exp_bit_count: bool ref

val get_last_exp_bit_count: unit -> int

val get_last_iteration_count: unit -> int

val _strat_impr_callback: (strategy -> int -> unit) option ref

type node_total_ordering_fun = Paritygame.paritygame -> node -> node -> int

val node_total_ordering_by_position : node_total_ordering_fun

type node_valuation = node * node TreeSet.t * int
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
  paritygame -> node_total_ordering_fun -> game_valuation -> node -> node

val node_valuation_ordering :
  paritygame -> node_total_ordering_fun -> node_valuation -> node_valuation -> int

val compute_counter_strategy:
  paritygame -> strategy -> strategy

	
	
val register_sub_solver: (paritygame -> solution * strategy) -> string -> string -> string -> unit

				
type initial_strategy_fun = paritygame -> strategy

type 'a improvement_policy_fun = paritygame -> node_total_ordering_fun -> 'a -> strategy -> game_valuation -> strategy * 'a

val strategy_improvement: paritygame -> initial_strategy_fun -> node_total_ordering_fun -> 'a improvement_policy_fun -> 'a -> bool -> string -> solution * strategy

val strategy_improvement'': paritygame -> initial_strategy_fun -> node_total_ordering_fun -> 'a improvement_policy_fun -> (paritygame -> 'a) -> bool -> string -> solution * strategy

val strategy_improvement_by_policy: paritygame -> 'a improvement_policy_fun -> 'a -> bool -> string -> solution * strategy


val improvement_policy_no_user_data: ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'f -> 'c -> 'd -> 'e * 'f

val node_valuation_total_ordering: paritygame -> node_total_ordering_fun -> game_valuation -> node -> node -> int

val best_decision_by_ordering: paritygame -> (node -> node -> int) -> node -> int

val empty_descending_relevance_ordered_set: paritygame -> node_total_ordering_fun -> node TreeSet.t

val initial_strategy_by_last_edge: paritygame -> strategy

val winning_strategies: paritygame -> node_total_ordering_fun -> strategy -> game_valuation -> strategy



type mdplike_valuation = (((node, float) TreeMap.t) * bool option) array

val mdplike_valuation: paritygame -> int -> strategy -> mdplike_valuation

val compare_mdplike_valuation: paritygame -> mdplike_valuation -> node -> node -> int

