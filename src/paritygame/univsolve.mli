open Basics;;
open Paritygame;;
open Tcstiming.SimpleTiming;;



(******************************************************************************
 *                                                                            *
 * Universal Solver Options                                                   *
 *                                                                            *
 ******************************************************************************)

type universal_solve_options = {
	(* General configuration *)
	generate_statistics: bool;
	verb_level: verbosity_level;

	decompose_sccs: bool; (* default: true *)

	(* Optimization phases *)
	global_optimization: bool; (* default: true *)
	solve_special_games: bool; (* default: true *)
	local_optimization: bool; (* default: true *)

	(* Global optimization *)
	globalopt_remove_useless_self_cycles: bool; (* default: true *)
	globalopt_solve_useful_self_cycles: bool; (* default: true *)

	(* Solving special games *)
	solvespec_single_parity: bool; (* default: true *)
	solvespec_single_player: bool; (* default: true *)

	(* Local optimization *)
	localopt_priority_propagation: bool; (* default: false *)
	localopt_compact_priorities: bool; (* default: true *)
}

val universal_solve_def_options: bool -> int -> universal_solve_options

val universal_solve_global_options: (bool -> int -> universal_solve_options) ref

val universal_solve_init_options_verbose: (bool -> int -> universal_solve_options) -> universal_solve_options

val universal_options_alter_verb: universal_solve_options -> verbosity_level -> universal_solve_options


(******************************************************************************
 *                                                                            *
 * Universal Solver Statistics                                                *
 *                                                                            *
 ******************************************************************************)

type universal_solve_statistics = {
	(* General statistics *)
	overall_timing: timing_object;
	universal_timing: timing_object;
	backend_timing: timing_object;
	logistics_timing: timing_object;
	stats_timing: timing_object;
	overall_solved_nodes: int ref; (* size of game *)
	universal_solved_nodes: int ref;
	backend_solved_nodes: int ref;
	backend_investigated_nodes: int ref;
	index_sum: int ref;
	index_reduced_sum: int ref;
	index_count: int ref;

	(* Optimization phase timing *)
	global_timing: timing_object;
	global_timing_without_attractor: timing_object;
	global_nodes: int ref;
	global_nodes_without_attractor: int ref;
	special_timing: timing_object;
	special_nodes: int ref;
	local_timing: timing_object;

	(* Recursion and decomposition *)
	decomposition_timing: timing_object;
	max_recursion_depth: int ref;
	recursive_calls: int ref;
	toplevel_sccs: int ref;
	largest_toplevel_scc: int ref;
	total_sccs: int ref;

	(* Attractor closure *)
	attractor_timing: timing_object;
	attractor_investigated_nodes: int ref;
	attractor_solved_nodes: int ref;

	(* Global optimization *)
	globalopt_remove_useless_self_cycles_nodes: int ref;
	globalopt_remove_useless_self_cycles_timing: timing_object;
	globalopt_solve_useful_self_cycles_nodes: int ref;
	globalopt_solve_useful_self_cycles_timing: timing_object;

	(* Special games *)
	solvespec_single_parity_nodes: int ref;
	solvespec_single_parity_timing: timing_object;
	solvespec_single_player_nodes: int ref;
	solvespec_single_player_timing: timing_object;

	(* Local optimization *)
	localopt_priority_propagation_timing: timing_object;
	localopt_compact_priorities_timing: timing_object;
}

val universal_solve_format_stats: universal_solve_statistics -> string

val universal_solve_init_statistics: unit -> universal_solve_statistics




(******************************************************************************
 *                                                                            *
 * Universal Solver Main                                                      *
 *                                                                            *
 ******************************************************************************)

val universal_solve_run: universal_solve_options -> universal_solve_statistics -> (paritygame -> solution * strategy) -> paritygame -> solution * strategy




(******************************************************************************
 *                                                                            *
 * User Functions                                                             *
 *                                                                            *
 ******************************************************************************)

(* Calling universal_solve options solver game starts the universal solving process using solver as a backend. It returns the solved game as a pair of solution and strategy. *)
val universal_solve: universal_solve_options ->
                     (paritygame -> solution * strategy) ->
                     paritygame -> solution * strategy

(* Calling universal_solve options solver fallback game starts the universal solving process using solver as a backend. If solver doesn't solve an SCC at all, the fallback solver is called. It returns the solved game as a pair of solution and strategy. *)
val universal_solve_fallback: universal_solve_options ->
                              (paritygame -> solution * strategy) ->
                              (paritygame -> solution * strategy) ->
                              paritygame -> solution * strategy

(* Calling universal_solve_by_player_solver options player_solver game starts the universal solving process using player_solver as a backend. Instead of a solver backend that solves arbitrary sccs player_solver is supposed to solve a SCC w.r.t. a given player, for instance if player_solver scc 0 is called, the backend is supposed to determine whether a node is won by player 0 (return 0 for this node) or not (return -1 for this node). The strategy that is returned by player_solver is only considered w.r.t. the given player. Calling universal_solve_by_player_solver returns the solved game as a pair of solution and strategy. *)
val universal_solve_by_player_solver: universal_solve_options ->
                                      (paritygame -> player ->
                                       solution * strategy) ->
                                      paritygame -> solution * strategy

(* Calling universal_solve_trivial verbosity game starts the universal solving process with a trivial i.e. not-solving backend. This function is legal to be called when game can be completely solved by the universal solving process without requiring the backend. Calling universal_solve_trivial returns the solved game as a pair of solution and strategy.*)
val universal_solve_trivial: verbosity_level -> paritygame -> solution * strategy

(* Calling compute_winning_nodes verbosity game strategy player considers the subgame of game w.r.t. the strategy decisions for player; the strategy is assumed to be total w.r.t. player. It returns the list of nodes player wins on the game following strategy. *)
val compute_winning_nodes: verbosity_level -> paritygame -> strategy -> player -> node list
