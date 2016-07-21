open Tcsbasedata
open Tcsset


(***************************************************************
 * Functions for representing sets of nodes,                   *
 * particularly for successors and predecessors of given nodes *
 *                                                             *
 * Warning: these types may become abstract in the future      *
 ***************************************************************)

type node = int
type nodeset

val ns_isEmpty : nodeset -> bool
val ns_empty   : nodeset
val ns_elem    : node -> nodeset -> bool
val ns_fold    : ('a -> node -> 'a) -> 'a -> nodeset -> 'a
val ns_iter    : (node -> unit) -> nodeset -> unit
val ns_map     : (node -> node) -> nodeset -> nodeset
val ns_size    : nodeset -> int
val ns_exists  : (node -> bool) -> nodeset -> bool
val ns_some    : nodeset -> node
val ns_add     : node -> nodeset -> nodeset
val ns_del     : node -> nodeset -> nodeset
val ns_make    : node list -> nodeset
val ns_nodes   : nodeset -> node list
			
(**************************************************************
 * Parity Game Definitions                                    *
 *                                                            *
 * Warning: the types may become abstract in the future       *
 **************************************************************)

type priority = int
type player = int
type paritygame
type solution = player array
type strategy = node array

type global_solver = (paritygame -> solution * strategy)


(**************************************************************
 * Access Functions                                           *
 **************************************************************)

val pg_create     : int -> paritygame
val pg_init       : int -> (int -> (priority * player * nodeset * nodeset * string option)) -> paritygame (* TODO: dangerous! Can be used to violate succ-/predecessor invariants. Can we get rid of it? *)
val pg_sort       : ((priority * player * nodeset * nodeset * string option) -> (priority * player * nodeset * nodeset * string option) -> int) -> paritygame -> unit
val pg_size	  : paritygame -> int
val pg_node_count : paritygame -> int
val pg_edge_count : paritygame -> int
val pg_copy       : paritygame -> paritygame

(**************************************************************
 * possibly DEPRECATED functions                              *
 * use the ones with the more verbose names instead           *
 **************************************************************)
val pg_get_pr     : paritygame -> node -> priority
val pg_set_pr     : paritygame -> node -> priority -> unit
val pg_get_pl     : paritygame -> node -> player
val pg_set_pl     : paritygame -> node -> player -> unit
val pg_get_tr     : paritygame -> node -> nodeset

val pg_set_tr     : paritygame -> int -> int array -> unit (* DEPRECATED *)

(**************************************************************
 * node access and modification functions                     *
 *                                                            *
 * modifications are inplace                                  *
 **************************************************************)
val pg_get_priority     : paritygame -> node -> priority
val pg_get_owner        : paritygame -> node -> player
val pg_get_successors   : paritygame -> node -> nodeset
val pg_get_predecessors : paritygame -> node -> nodeset

val pg_set_priority     : paritygame -> node -> priority -> unit
val pg_set_owner        : paritygame -> node -> player -> unit
val pg_add_edge         : paritygame -> node -> node -> unit
val pg_del_edge         : paritygame -> node -> node -> unit

val pg_get_desc   : paritygame -> node -> string option
val pg_set_desc   : paritygame -> node -> string option -> unit
val pg_get_desc'  : paritygame -> node -> string
val pg_set_desc'  : paritygame -> node -> string -> unit

val pg_isDefined : paritygame -> node -> bool
					   
val pg_iterate : (node -> (priority * player * nodeset * nodeset * string option) -> unit) -> paritygame -> unit 
val pg_map     : (node -> (priority * player * nodeset * nodeset * string option) -> (priority * player * nodeset * nodeset * string option)) -> paritygame -> paritygame 
val pg_map2    : (node -> (priority * player * nodeset * nodeset * string option) -> 'a) -> paritygame -> 'a array 

val pg_get_node   : paritygame -> node -> (priority * player * nodeset * nodeset * string option)
val pg_set_node   : paritygame -> node -> priority -> player -> nodeset -> nodeset -> string option -> unit (* DEPRECATED *)
val pg_set_node'  : paritygame -> int -> (priority * player * nodeset * nodeset * string option) -> unit (* DEPRECATED *)

val pg_find_desc  : paritygame -> string option -> node

val pg_get_tr_index_of: paritygame -> int -> int -> int (* DEPRECATED *)

(* `pg_remove_nodes <game> <node_list>' removes all nodes from <game> that are specified in <node_list> *)
val pg_remove_nodes   : paritygame -> node list -> unit

(* `pg_remove_edges <game> <edge_list>' removes all edges from <game> that are specified in <edge_list> *)
val pg_remove_edges   : paritygame -> (node * node) list -> unit


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

type pg_ordering      = node * priority * player * nodeset -> node * priority * player * nodeset -> int

(* `reward <player> <priority>' returns the reward that <priority> has for player <player> *)
val reward            : player -> priority -> priority

val ord_rew_for       : player -> pg_ordering
val ord_prio          : pg_ordering
val ord_total_by      : pg_ordering -> pg_ordering

val pg_max            : paritygame -> pg_ordering -> node
val pg_min            : paritygame -> pg_ordering -> node

val pg_max_prio_node  : paritygame -> node
val pg_max_rew_node_for : paritygame -> player -> node


(* Calling pg_max_prio game returns the greatest priority occurring in the game *)
val pg_max_prio       : paritygame -> priority

(* Calling pg_min_prio game returns the least priority occurring in the game *)
val pg_min_prio       : paritygame -> priority

(* Calling pg_max_prio_for game player returns the greatest reward for player occurring in the game *)
val pg_max_prio_for   : paritygame -> player -> priority

(* Calling pg_get_index game returns the index of the game *)
val pg_get_index      : paritygame -> int

(* `pg_prio_nodes <game> <prio>' returns a list of all nodes having priority <prio> *)
val pg_prio_nodes: paritygame -> priority -> node list



(**************************************************************
 * Inplace Modifications                                      *
 **************************************************************)

(* 
val pg_add_successor  : paritygame -> int -> int -> unit  (* DEPRECATED *)
val pg_add_successors : paritygame -> int -> int array -> unit (* DEPRECATED *)
*)
						  



(**************************************************************
 * Node Collect Functions                                     *
 **************************************************************)

val collect_nodes: paritygame -> (node -> priority * player * nodeset * nodeset * string option -> bool) -> node list
val collect_nodes_by_prio: paritygame -> (priority -> bool) -> node list

(* `collect_max_prio_nodes <game>' returns all nodes with greatest priority *)
val collect_max_prio_nodes: paritygame -> node list

val collect_max_parity_nodes: paritygame -> node list



(**************************************************************
 * Sub Game Creation                                          *
 **************************************************************)

val subgame_by_edge_pred: paritygame -> (int -> int -> bool) -> paritygame
val subgame_by_strat: paritygame -> strategy -> paritygame
val subgame_by_strat_pl: paritygame -> strategy -> int -> paritygame

(* Calling subgame_by_list game nodes returns a compressed sub game induced and ordered by the nodes-list *)
val subgame_by_list: paritygame -> int list -> paritygame

(*
val subgame_and_subgraph_by_list: paritygame -> int list array -> int list -> paritygame * int list array (* DEPRECATED *)
*)

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
 * Decomposition Functions                                    *
 **************************************************************)

type scc = int
(*
val strongly_connected_components' : paritygame -> (node list array) -> node list array * scc array * scc list array * scc list (* DEPRECATED *)
 *)
	     
(* `strongly_connected_components <game>' decomposes the game into its SCCs. 
   It returns a tuple (<sccs>, <sccindex>, <topology>, <roots>) where 
    - <sccs> is an array mapping each SCC to its list of nodes, 
    - <sccindex> is an array mapping each node to its SCC, 
    - <topology> is an array mapping each SCC to the list of its immediate successing SCCs and 
    - <roots> is the list of SCCs having no predecessing SCC. 
*)
val strongly_connected_components : paritygame -> node list array * scc array * scc list array * scc list

(* `sccs_compute_leaves <scc_list> <topology>' returns the leaf SCCs reachable from some SCC in <scc_list> via <topology> *)
val sccs_compute_leaves: scc list -> scc list array -> scc list

val sccs_compute_transposed_topology: scc list array -> scc list array


val sccs_compute_connectors : paritygame -> node list array * scc array * scc list array * scc list -> (scc * scc, (scc * scc) list) Hashtbl.t

val show_sccs : node list array -> scc list array -> scc list -> string



(**************************************************************
 * Attractor Closure                                          *
 **************************************************************)

(* game strategy player region include_region tgraph deltafilter overwrite_strat *)
val attr_closure_inplace': paritygame -> strategy -> player -> node TreeSet.t -> bool -> (node -> bool) -> bool -> node list

(* `attr_closure_inplace <game> <strategy> <player> <region>' returns the attractor for the given player and region. 
   Additionally all necessary strategy decisions for player leading into the region are added to <strategy>. *)
val attr_closure_inplace : paritygame -> strategy -> player -> node list -> node list

val attractor_closure_inplace_sol_strat: paritygame -> (node -> bool) -> solution -> strategy -> node TreeSet.t -> node TreeSet.t -> (node list * node list)



(**************************************************************
 * Dominion Functions                                         *
 **************************************************************)

val pg_set_closed: paritygame -> int TreeSet.t -> int -> bool

val pg_set_dominion: (paritygame -> solution * strategy) -> paritygame -> node TreeSet.t -> player -> strategy option



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
val is_single_parity_game: paritygame -> priority option

val number_of_strategies : paritygame -> player -> int -> int

val compute_priority_reach_array : paritygame -> int -> int array array

(*
val compute_priority_reach_array': paritygame -> int array array
*)




(**************************************************************
 * Dynamic Parity Game                                        *
 **************************************************************)

type dynamic_paritygame = (int * int * string option) Tcsgraph.DynamicGraph.dynamic_graph

val paritygame_to_dynamic_paritygame: paritygame -> dynamic_paritygame

val dynamic_subgame_by_strategy: dynamic_paritygame -> strategy -> dynamic_paritygame

val paritygame_to_dynamic_paritygame_by_strategy: paritygame -> strategy -> dynamic_paritygame



(**************************************************************
 * Symbolic Parity Game                                       *
 **************************************************************)


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

val diamond : paritygame -> NodeSet.t -> NodeSet.t
val box     : paritygame -> NodeSet.t -> NodeSet.t


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
