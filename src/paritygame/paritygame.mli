open Tcsbasedata
open Tcsset


(***************************************************************
 * Functions for representing sets of nodes,                   *
 * particularly for successors and predecessors of given nodes *
 *                                                             *
 * Warning: the type `node' may become abstract in the future  *
 ***************************************************************)

type node = int
val nd_undef  : node
(*
val nd_make   : int -> node   (* turns an int into a node; mostly to be used with paritygame generators that like to build a game using ints for nodes *) 
val nd_reveal : node -> int   (* provides the inverse function to nd_make *)
 *)
		  
val nd_show : node -> string
			
type nodeset

(* check for emptiness and membership *)
val ns_isEmpty : nodeset -> bool
val ns_elem    : node -> nodeset -> bool

val ns_nodeCompare : node -> node -> int
val ns_compare : nodeset -> nodeset -> int

(* constructor functions for node sets *)
val ns_empty   : nodeset
val ns_make    : node list -> nodeset

(* returns the number of elements in a node set *)
val ns_size    : nodeset -> int

(* iterator functions over node sets *)
val ns_fold    : ('a -> node -> 'a) -> 'a -> nodeset -> 'a
val ns_iter    : (node -> unit) -> nodeset -> unit
val ns_map     : (node -> node) -> nodeset -> nodeset
val ns_filter  : (node -> bool) -> nodeset -> nodeset
						
(* finding elements in a node set *)
val ns_exists  : (node -> bool) -> nodeset -> bool			
val ns_forall  : (node -> bool) -> nodeset -> bool			
val ns_find    : (node -> bool) -> nodeset -> node
val ns_max     : nodeset -> (node -> node -> bool) -> node
val ns_some    : nodeset -> node (* returns a randomly chosen element from a node set *)
val ns_first   : nodeset -> node (* return the smallest (by name) node in a node set *)
val ns_last    : nodeset -> node (* return the greatest (by name) node in a node set *)

(* add a node to, resp. delete a node from a nodeset *) 
val ns_add     : node -> nodeset -> nodeset
val ns_del     : node -> nodeset -> nodeset

val ns_union   : nodeset -> nodeset -> nodeset

(* extract a list of nodes from a node set *)
val ns_nodes   : nodeset -> node list


(**************************************************************
 * Players and priorities                                     *
 *                                                            *
 * Warning: the types may become abstract in the future       *
 **************************************************************)

type player 
type priority = int

val plr_Even  : player
val plr_Odd   : player
val plr_undef : player

val plr_random : unit -> player
			   
val plr_opponent : player -> player
val plr_benefits : priority -> player
val plr_show : player -> string

(* applies a function to both players *)
val plr_iterate : (player -> unit) -> unit
					
val prio_good_for_player : priority -> player -> bool

val odd: priority -> bool
val even: priority -> bool
			
						 
(**************************************************************
 * Parity Game Definitions                                    *
 **************************************************************)

type paritygame

		       
(**************************************************************
 * Access Functions                                           *
 **************************************************************)

val pg_create     : int -> paritygame
val pg_init       : int -> (node -> (priority * player * node list * string option)) -> paritygame
val pg_size	  : paritygame -> int
val pg_node_count : paritygame -> int
val pg_edge_count : paritygame -> int
val pg_copy       : paritygame -> paritygame


				  
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
					   
val pg_iterate      : (node -> (priority * player * nodeset * nodeset * string option) -> unit) -> paritygame -> unit
val pg_edge_iterate : (node -> node -> unit) -> paritygame -> unit
val pg_map          : (node -> (priority * player * nodeset * nodeset * string option) -> (priority * player * nodeset * nodeset * string option)) -> paritygame -> paritygame 
val pg_map2         : (node -> (priority * player * nodeset * nodeset * string option) -> 'a) -> paritygame -> 'a array 
					    
val pg_find_desc  : paritygame -> string option -> node

(* `pg_get_index <game>' returns the index of the game <game>*)
val pg_get_index      : paritygame -> int

(* `pg_prio_nodes <game> <prio>' returns a list of all nodes having priority <prio> *)
val pg_prio_nodes: paritygame -> priority -> nodeset

(* returns a list of all the priorities occurring in the game *)
val pg_get_selected_priorities : paritygame -> (priority -> bool) -> priority list
val pg_get_priorities : paritygame -> priority list
						     
(* `pg_remove_nodes <game> <node_list>' removes all nodes from <game> that are specified in <node_list> *)
val pg_remove_nodes   : paritygame -> nodeset -> unit

(* `pg_remove_edges <game> <edge_list>' removes all edges from <game> that are specified in <edge_list> *)
val pg_remove_edges   : paritygame -> (node * node) list -> unit


(* sorting *)
val pg_sort                   : ((priority * player * nodeset * nodeset * string option) ->
				 (priority * player * nodeset * nodeset * string option) -> int) -> paritygame -> unit


(**************************************************************
 * Solutions, strategies and solvers                          *
 *                                                            *
 * Warning: the types of solution and strategy may become     *
 * abstract in the future                                     *
 **************************************************************)

type solution = player array
type strategy = node array

(* create solution spaces for a parity game *)
val sol_create : paritygame -> solution                       (* initially, every node is won by player plr_undef *)
val sol_make   : int -> solution                              (* same as sol_create but only gets to know the size of the game *)
val sol_init   : paritygame -> (node -> player) -> solution   (* create solution space initially filled with values *)

val sol_get    : solution -> node -> player                   (* get the winner of a node according to a solution *)
val sol_set    : solution -> node -> player -> unit           (* set the winner of a node in a solution *)
val sol_iter   : (node -> player -> unit) -> solution -> unit (* iterate over all nodes with their winners in a solution space *) 
						 
val sol_number_solved : solution -> int                       (* test solutions *)

(* create positional strategies for a parity game 
 *
 * A value of type strategy is essentially a map of type node -> node that represents positional strategies for both players.
 * The player for whom a decision v -> u is included in the strategy is implicitly given by the owner of node v in the underlying parity game.
 * Warning: a strategy does not remember its underlying parity game. Hence, a strategy that was created for one game can be used for another game,
 * but this can not only obviously lead to wrong computations but also to runtime errors.
 *)
val str_create : paritygame -> strategy                       (* initially, every node maps to a special undefined node *)
val str_make   : int -> strategy                              (* same as str_create but only gets to know the size of the game *)
val str_init   : paritygame -> (node -> node) -> strategy     (* create strategy initially filled with decisions according to its second argument *)

val str_get    : strategy -> node -> node                     (* get the strategy decision at a node *)
val str_set    : strategy -> node -> node -> unit             (* `str_set <str> <v> <u>´ records the strategy decision <v> -> <u> in <str> *)
val str_iter   : (node -> node -> unit) -> strategy -> unit   (* iterate over all nodes and their corresponding successors in a strategy *) 


(**************************************************************
 * A type for algorithms that solve a paritygame              *
 **************************************************************)
type global_solver = paritygame -> solution * strategy

							 
(**************************************************************
 * Formatting Functions                                       *
 **************************************************************)

val game_to_string : paritygame -> string

(* `print_game <game>' prints <game> on STDOUT s.t. it could be parsed again. *)
val print_game : paritygame -> unit
val output_game : out_channel -> paritygame -> unit

val print_solution_strategy_parsable : solution -> strategy -> unit

val to_dotty : paritygame -> solution -> strategy -> out_channel -> unit
val to_dotty_file : paritygame -> solution -> strategy -> string -> unit

val format_strategy : strategy -> string
val format_solution : solution -> string
val format_game : paritygame -> string



 
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




(**************************************************************
 * Node Collect Functions                                     *
 **************************************************************)

val collect_nodes: paritygame -> (node -> priority * player * nodeset * nodeset * string option -> bool) -> nodeset
val collect_nodes_by_prio: paritygame -> (priority -> bool) -> nodeset

(* `collect_nodes_by_owner <game> <f>' returns two lists: the first one contains all nodes v for which f v is true, the other all those for which it is false *)
val collect_nodes_by_owner: paritygame -> (player -> bool) -> nodeset * nodeset
								  
(* `collect_max_prio_nodes <game>' returns all nodes with greatest priority *)
val collect_max_prio_nodes: paritygame -> nodeset

val collect_max_parity_nodes: paritygame -> nodeset



(**************************************************************
 * Sub Game Creation                                          *
 **************************************************************)

val subgame_by_edge_pred: paritygame -> (node -> node -> bool) -> paritygame
val subgame_by_strat: paritygame -> strategy -> paritygame
val subgame_by_strat_pl: paritygame -> strategy -> player -> paritygame

(* Calling subgame_by_list game nodes returns a compressed sub game induced and ordered by the nodes-list *)
val subgame_by_list: paritygame -> nodeset -> paritygame

val subgame_by_node_filter: paritygame -> (node -> bool) -> paritygame * (node -> node) * (node -> node)

(*
val subgame_and_subgraph_by_list: paritygame -> int list array -> int list -> paritygame * int list array (* DEPRECATED *)
*)

(**************************************************************
 * Solution / Strategy Update Functions                       *
 **************************************************************)

(*
val permute_solution: int array -> solution -> solution
val permute_strategy: int array -> int array -> solution -> solution
*)

exception Unmergable

(* Calling merge_strategies_inplace strat1 strat2 adds all strategy decisions from strat2 to strat1. Throws an Unmergable-Exception if the domain of both strategies is not empty. *)
val merge_strategies_inplace : strategy -> strategy -> unit

(* Calling merge_solutions_inplace sol1 sol2 adds all solution informations from sol2 to sol1. Throws an Unmergable-Exception if the domain of both solutions is not empty. *)
val merge_solutions_inplace : solution -> solution -> unit



(**************************************************************
 * Decomposition Functions                                    *
 **************************************************************)

type scc = int
	     
(* `strongly_connected_components <game>' decomposes the game into its SCCs. 
   It returns a tuple (<sccs>, <sccindex>, <topology>, <roots>) where 
    - <sccs> is an array mapping each SCC to its list of nodes, 
    - <sccindex> is an array mapping each node to its SCC, 
    - <topology> is an array mapping each SCC to the list of its immediate successing SCCs and 
    - <roots> is the list of SCCs having no predecessing SCC. 
*)
val strongly_connected_components : paritygame -> nodeset array * scc array * scc list array * scc list

(* `sccs_compute_leaves <scc_list> <topology>' returns the leaf SCCs reachable from some SCC in <scc_list> via <topology> *)
val sccs_compute_leaves: scc list -> scc list array -> scc list

val sccs_compute_transposed_topology: scc list array -> scc list array


val sccs_compute_connectors : paritygame -> nodeset array * scc array * scc list array * scc list -> (scc * scc, (scc * scc) list) Hashtbl.t

val show_sccs : nodeset array -> scc list array -> scc list -> string



(**************************************************************
 * Attractor Closure                                          *
 **************************************************************)

(* game strategy player region include_region tgraph deltafilter overwrite_strat *)
val attr_closure_inplace': paritygame -> strategy -> player -> nodeset -> bool -> (node -> bool) -> bool -> nodeset

(* `attr_closure_inplace <game> <strategy> <player> <region>' returns the attractor for the given player and region. 
   Additionally all necessary strategy decisions for player leading into the region are added to <strategy>. *)
val attr_closure_inplace : paritygame -> strategy -> player -> nodeset -> nodeset

val attractor_closure_inplace_sol_strat: paritygame -> (node -> bool) -> solution -> strategy -> nodeset -> nodeset -> (nodeset * nodeset)



(**************************************************************
 * Dominion Functions                                         *
 **************************************************************)

val pg_set_closed: paritygame -> nodeset -> player -> bool

val pg_set_dominion: (paritygame -> solution * strategy) -> paritygame -> nodeset -> player -> strategy option



(**************************************************************
 * Partial Parity Game                                        *
 **************************************************************)

type partial_paritygame = node * (node -> node Enumerators.enumerator) * (node -> priority * player) * (node -> string option)
type partial_solution = node -> player * node option
type partial_solver = partial_paritygame -> partial_solution

val induce_partialparitygame: paritygame -> node -> partial_paritygame

val induce_counting_partialparitygame: paritygame -> node -> int ref * partial_paritygame

val partially_solve_dominion: paritygame -> node -> partial_solver -> solution * strategy

val partially_solve_game: paritygame -> partial_solver -> solution * strategy




(**************************************************************
 * Game Information                                           *
 **************************************************************)

val get_player_decision_info: paritygame -> bool * bool
val is_single_parity_game: paritygame -> priority option

(* computes the number of strategies for a player in a game; the third parameter is an upper bound on the returned value *)
val number_of_strategies : paritygame -> player -> int -> int

val compute_priority_reach_array : paritygame -> player -> priority array array  

(*
val compute_priority_reach_array': paritygame -> int array array
*)




(**************************************************************
 * Dynamic Parity Game                                        *
 **************************************************************)

type dynamic_paritygame = (priority * player * string option) Tcsgraph.DynamicGraph.dynamic_graph

val paritygame_to_dynamic_paritygame: paritygame -> dynamic_paritygame

val dynamic_subgame_by_strategy: dynamic_paritygame -> strategy -> dynamic_paritygame

val paritygame_to_dynamic_paritygame_by_strategy: paritygame -> strategy -> dynamic_paritygame




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
 ********************************************************)

(* `diamond <pg> <ns>' returns the set of all nodes in <pg> that have a successor in <ns>. 
    Likewise, `box' does so for nodes whose successors are all in <ns>. *)
val diamond : paritygame -> NodeSet.t -> NodeSet.t
val box     : paritygame -> NodeSet.t -> NodeSet.t


(**************************************************************
 * Building Parity Games                                      *
 **************************************************************)

(* This can be used to build parity games starting from a particular node in an on-the-fly fashion.
   It is particularly useful when the resulting size is not (easily) known in advance. For an example
   of its use, see src/generators/langincl.ml . 

   To use it, define a module of the type PGDescription using some type gamenode to represent nodes and
   giving functions that read off the priority, owner successors, a possible string representation of a 
   game node, and a list of particular initial nodes.
   The module obtained by applying the functor Build then gives you a module with a function that
   builds a parity game containing all the game nodes that are reachable these initial ones. Additionally,
   you get functions that take nodes as arguments from which to build the parity game. 
*)
					   
module type PGDescription =
  sig
    type gamenode

    val compare    : gamenode -> gamenode -> int

    val owner      : gamenode -> player
    val priority   : gamenode -> priority
    val successors : gamenode -> gamenode list   (* should always return a non-empty list *)
    val show_node  : gamenode -> string option

    val initnodes  : unit -> gamenode list 
  end

module type PGBuilder = 
  sig
    type gamenode
	   
    val build            : unit -> paritygame
    val build_from_node  : gamenode -> paritygame
    val build_from_nodes : gamenode list -> paritygame
  end

module Build(T: PGDescription) : PGBuilder with type gamenode = T.gamenode 

