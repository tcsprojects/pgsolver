open Tcsbasedata
open Tcsset


(**************************************************************
 * Parity Game Definitions                                    *
 **************************************************************)

type paritygame = (int * int * int array * string option) array
type solution = int array
type strategy = int array

type global_solver = (paritygame -> solution * strategy)


(**************************************************************
 * Access Functions                                           *
 **************************************************************)

val pg_create     : int -> paritygame
val pg_size		  : paritygame -> int
val pg_node_count : paritygame -> int
val pg_edge_count : paritygame -> int
val pg_copy       : paritygame -> paritygame

val pg_get_pr     : paritygame -> int -> int
val pg_set_pr     : paritygame -> int -> int -> unit
val pg_get_pl     : paritygame -> int -> int
val pg_set_pl     : paritygame -> int -> int -> unit
val pg_get_tr     : paritygame -> int -> int array
val pg_set_tr     : paritygame -> int -> int array -> unit
val pg_get_desc   : paritygame -> int -> string option
val pg_set_desc   : paritygame -> int -> string option -> unit
val pg_get_desc'  : paritygame -> int -> string
val pg_set_desc'  : paritygame -> int -> string -> unit


val pg_get_node   : paritygame -> int -> (int * int * int array * string option)
val pg_set_node   : paritygame -> int -> int -> int -> int array -> string option -> unit


val pg_find_desc  : paritygame -> string option -> int



(**************************************************************
 * Formatting Functions                                       *
 **************************************************************)

val game_to_string : paritygame -> string

(* Calling print_game game prints game on STDOUT s.t. it could be parsed again. *)
val print_game : paritygame -> unit

val print_solution_strategy_parsable : solution -> strategy -> unit

val to_dotty : paritygame -> solution -> strategy -> out_channel -> unit
val to_dotty_file : paritygame -> solution -> strategy -> string -> unit

val format_strategy : strategy -> string
val format_solution : solution -> string
val format_game : paritygame -> string




(**************************************************************
 * Parsing Functions                                          *
 **************************************************************)
 
 val parse_parity_game: in_channel -> paritygame
 

 
 
 (**************************************************************
 * Node Orderings                                             *
 **************************************************************)

type pg_ordering      = int * int * int * int array -> int * int * int * int array -> int

val reward            : int -> int -> int

val ord_rew_for       : int -> pg_ordering
val ord_prio          : pg_ordering
val ord_total_by      : pg_ordering -> pg_ordering

val pg_max            : paritygame -> pg_ordering -> int
val pg_min            : paritygame -> pg_ordering -> int

val pg_max_prio_node  : paritygame -> int
val pg_max_rew_node_for : paritygame -> int -> int


(* Calling pg_max_prio game returns the greatest priority occurring in the game *)
val pg_max_prio       : paritygame -> int

(* Calling pg_min_prio game returns the least priority occurring in the game *)
val pg_min_prio       : paritygame -> int

(* Calling pg_max_prio_for game player returns the greatest reward for player occurring in the game *)
val pg_max_prio_for   : paritygame -> int -> int

(* Calling pg_get_index game returns the index of the game *)
val pg_get_index      : paritygame -> int

val pg_prio_nodes: paritygame -> int -> int list



(**************************************************************
 * Inplace Modifications                                      *
 **************************************************************)

val pg_add_successor  : paritygame -> int -> int -> unit
val pg_add_successors : paritygame -> int -> int array -> unit

(* Calling pg_remove_nodes game node_list removes all nodes specified in node_list *)
val pg_remove_nodes   : paritygame -> int list -> unit

(* Calling pg_remove_edges game edge_list removes all edges specified in edge_list *)
val pg_remove_edges   : paritygame -> (int * int) list -> unit



(**************************************************************
 * Node Collect Functions                                     *
 **************************************************************)

val collect_nodes: paritygame -> (int -> int * int * int array * string option -> bool) -> int list
val collect_nodes_by_prio: paritygame -> (int -> bool) -> int list

(* Calling collect_max_prio_nodes game returns all nodes with greatest priority *)
val collect_max_prio_nodes: paritygame -> int list

val collect_max_parity_nodes: paritygame -> int list



(**************************************************************
 * Sub Game Creation                                          *
 **************************************************************)

val subgame_by_edge_pred: paritygame -> (int -> int -> bool) -> paritygame
val subgame_by_strat: paritygame -> strategy -> paritygame
val subgame_by_strat_pl: paritygame -> strategy -> int -> paritygame

(* Calling subgame_by_list game nodes returns a compressed sub game induced and ordered by the nodes-list *)
val subgame_by_list: paritygame -> int list -> paritygame

val subgame_and_subgraph_by_list: paritygame -> int list array -> int list -> paritygame * int list array


(**************************************************************
 * Solution / Strategy Update Functions                       *
 **************************************************************)

val permute_solution: int array -> solution -> solution
val permute_strategy: int array -> int array -> solution -> solution

exception Unmergable

(* Calling merge_strategies_inplace strat1 strat2 adds all strategy decisions from strat2 to strat1. Throws an Unmergable-Exception if the domain of both strategies is not empty. *)
val merge_strategies_inplace : strategy -> strategy -> unit

(* Calling merge_solutions_inplace sol1 sol2 adds all solution informations from sol2 to sol1. Throws an Unmergable-Exception if the domain of both solutions is not empty. *)
val merge_solutions_inplace : solution -> solution -> unit


(**************************************************************
 * Graph Transformations                                      *
 **************************************************************)

(* Calling game_to_transposed_graph game returns the transposed graph associated with the game. *)
val game_to_transposed_graph : paritygame -> int list array

val transposed_graph_remove_nodes : paritygame -> int list array -> int list -> unit

val pg_with_graph_remove_nodes: paritygame -> int list array -> int list -> unit

(* Calling game_to_graph game returns the graph associated with the game. *)
val game_to_graph : paritygame -> int list array

val transposed_graph_remove_edges : int list array -> (int * int) list -> unit



(**************************************************************
 * Decomposition Functions                                    *
 **************************************************************)

val strongly_connected_components' : paritygame -> (int list array) -> int list array * int array * int list array * int list

(* Calling strongly_connected_components game decomposes the game into its SCCs. It returns a tuple (sccs, sccindex, topology, roots) where sccs is an array mapping each SCC to its list of nodes, sccindex is an array mapping each node to its SCC, topology is an array mapping each SCC to the list of its immediate successing SCCs and roots is the list of SCC having no predecessing SCC. *)
val strongly_connected_components : paritygame -> int list array * int array * int list array * int list

val sccs_compute_leafs: int list -> int list array -> int list

val sccs_compute_transposed_topology: int list array -> int list array


val sccs_compute_connectors : paritygame -> int list array * int array * int list array * int list -> (int * int, (int * int) list) Hashtbl.t

val show_sccs : int list array -> int list array -> int list -> string



(**************************************************************
 * Attractor Closure                                          *
 **************************************************************)

(* game strategy player region include_region tgraph deltafilter overwrite_strat *)
val attr_closure_inplace': paritygame -> strategy -> int -> int TreeSet.t -> bool -> int list array -> (int -> bool) -> bool -> int list

(* Calling attr_closure_inplace game strategy player region returns the attractor for the given player and region. Additionally all necessary strategy decisions for player leading into the region are added to strategy. *)
val attr_closure_inplace : paritygame -> strategy -> int -> int list -> int list

val attractor_closure_inplace_sol_strat: paritygame -> int list array -> (int -> bool) -> solution -> strategy -> int TreeSet.t -> int TreeSet.t -> (int list * int list)



(**************************************************************
 * Dominion Functions                                         *
 **************************************************************)

val pg_set_closed: paritygame -> int TreeSet.t -> int -> bool

val pg_set_dominion: (paritygame -> solution * strategy) -> paritygame -> int TreeSet.t -> int -> strategy option



(**************************************************************
 * Partial Parity Game                                        *
 **************************************************************)

type partial_paritygame = int * (int -> int Enumerators.enumerator) * (int -> int * int) * (int -> string option)
type partial_solution = int -> int * int option
type partial_solver = partial_paritygame -> partial_solution

val induce_partialparitygame: paritygame -> int -> partial_paritygame

val induce_counting_partialparitygame: paritygame -> int -> int ref * partial_paritygame

val partially_solve_dominion: paritygame -> int -> partial_solver -> solution * strategy

val partially_solve_game: paritygame -> partial_solver -> solution * strategy




(**************************************************************
 * Game Information                                           *
 **************************************************************)

val get_player_decision_info: paritygame -> bool * bool
val is_single_parity_game: paritygame -> int option

val number_of_strategies : paritygame -> int -> int -> int

val compute_priority_reach_array : paritygame -> int -> int array array

(*
val compute_priority_reach_array': paritygame -> int array array
*)




(**************************************************************
 * Symbolic Parity Game                                       *
 **************************************************************)

type dynamic_paritygame = (int * int * string option) Tcsgraph.DynamicGraph.dynamic_graph

module SymbolicParityGame : sig

	type 'a symbolic_paritygame

	val create_new: 'a -> 'a symbolic_paritygame

	val to_paritygame: 'a symbolic_paritygame -> paritygame

    val touch_node: 'a symbolic_paritygame -> 'a -> unit
   
	val add_node: 'a symbolic_paritygame -> 'a -> int -> int -> 'a array -> string option -> unit

end



(********************************************************
 * a type and data structure for sets of game nodes     *
 ********************************************************)

module NodeSet : sig
    type elt = int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
end


(********************************************************
 * Modal logic operations on sets of game nodes.        *
 * takes a set of nodes, a parity game and its          *
 *  transposed graph                                    *
 ********************************************************)

val diamond_with_transposed_graph: NodeSet.t -> paritygame -> int list array -> NodeSet.t
val box_with_transposed_graph    : NodeSet.t -> paritygame -> int list array -> NodeSet.t



(**************************************************************
 * Building Parity Games                                      *
 **************************************************************)

module type GameNode =
  sig
    type node

    val compare    : node -> node -> int

    val owner      : node -> int
    val priority   : node -> int
    val successors : node -> node list
    val name       : node -> string option
  end

module Build: functor (T: GameNode) -> 
  sig
    val build_from_node : T.node -> paritygame
  end
