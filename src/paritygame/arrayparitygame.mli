(** Interface for array paritygame. 
    For further information on each class method see paritygame.mli 
 *)


(**************************************************************
 *                          TOOLS                             *
 **************************************************************)
(** Method for adding an edge between two nodes in an (priority * player * successors * predecessors * description) array.
    
    @param array array to set edge (v,u) in
    @param predecessor v
    @param successor u
*)
val add_edge_in_node_array :
  ('a * 'b * Paritygame.nodeset * Paritygame.nodeset * 'c) array ->
  Paritygame.node -> Paritygame.node -> unit




(**************************************************************
 *                       ARRAY PARITYGAME                     *
 **************************************************************)
(** Arrayparitygame class. 
    This class inherits from the general paritygame class and replaces the former paritygame type.
    The structure behind this class is an (priority * player * nodeset * nodeset * string option) array. 

    @param initfunction initializer function parameter. is not required. if should be used has to be tagged with ~initFunc:(fun....)
    @param int size of arrayparitygame. is required.
*)                                          
class array_pg : ?initFunc:(Paritygame.node ->Paritygame.priority * Paritygame.player * Paritygame.node list * string option) -> int ->
  object ('self)
           
    val mutable nodes : (Paritygame.priority * Paritygame.player * Paritygame.nodeset * Paritygame.nodeset * string option) array

                                                                                                                            
    (********** GENERAL **********)  
    method private init : (Paritygame.node -> Paritygame.priority * Paritygame.player * Paritygame.node list * string option) -> unit

    method size : int
                    
    method copy : 'self
                    
    method sort : (Paritygame.priority * Paritygame.player * Paritygame.nodeset * Paritygame.nodeset * string option ->
                   Paritygame.priority * Paritygame.player * Paritygame.nodeset * Paritygame.nodeset * string option -> int) -> unit
                                                                                                                                  
    method iterate : (Paritygame.node -> Paritygame.priority * Paritygame.player * Paritygame.nodeset * Paritygame.nodeset * string option -> unit) -> unit
                                                                                                                                                         
    method edge_iterate : (Paritygame.node -> Paritygame.node -> unit) -> unit

    method map : (Paritygame.node -> Paritygame.priority * Paritygame.player * Paritygame.nodeset * Paritygame.nodeset * string option ->
                  Paritygame.priority * Paritygame.player * Paritygame.nodeset * Paritygame.nodeset * string option) -> 'self
                                                                                                                          
    method map2 : 'a. (Paritygame.node -> (Paritygame.priority * Paritygame.player * Paritygame.nodeset * Paritygame.nodeset * string option) -> 'a) -> 'a array
                                                                                                                                                     

    (********** GETTERS **********)                                                                                                                                                 
    method get_node : int -> Paritygame.priority * Paritygame.player * Paritygame.nodeset * Paritygame.nodeset * string option

    method get_priority : Paritygame.node -> Paritygame.priority

    method get_owner : Paritygame.node -> Paritygame.player

    method get_successors : Paritygame.node -> Paritygame.nodeset

    method get_predecessors : Paritygame.node -> Paritygame.nodeset

    method get_desc : Paritygame.node -> string option
                                                
    method get_desc' : Paritygame.node -> string

    method find_desc : string option -> Paritygame.node

    method is_defined : Paritygame.node -> bool

    method format_game : string

    method get_boxes : Paritygame.NodeSet.t -> Paritygame.NodeSet.t

    method get_diamonds : Paritygame.NodeSet.t -> Paritygame.NodeSet.t

    method get_index : int

    method get_max : Paritygame.pg_ordering -> Paritygame.node

    method get_max_prio : Paritygame.priority

    method get_max_prio_for : Paritygame.player -> Paritygame.priority

    method get_max_prio_node : Paritygame.node

    method get_max_rew_node_for : Paritygame.player -> Paritygame.node

    method get_min : Paritygame.pg_ordering -> Paritygame.node

    method get_min_prio : Paritygame.priority

    method get_prio_nodes : Paritygame.priority -> Paritygame.nodeset

    method get_priorities : Paritygame.priority list

    method get_selected_priorities : (Paritygame.priority -> bool) -> Paritygame.priority list

    method edge_count : int

    method node_count : int

    method print : unit
  
    method to_dotty : Paritygame.solution -> Paritygame.strategy -> out_channel -> unit

    method to_dotty_file : Paritygame.solution -> Paritygame.strategy -> string -> unit

    method to_string : string

                         
    (********** SETTERS **********)
    method set_node' : int -> Paritygame.priority * Paritygame.player * Paritygame.nodeset * Paritygame.nodeset * string option -> unit

    method set_node : int -> Paritygame.priority -> Paritygame.player -> Paritygame.nodeset -> Paritygame.nodeset -> string option -> unit
                                                
    method set_priority : Paritygame.node -> Paritygame.priority -> unit

    method set_owner : Paritygame.node -> Paritygame.player -> unit

    method set_desc : Paritygame.node -> string option -> unit

    method set_desc' : Paritygame.node -> string -> unit

    method add_edge : Paritygame.node -> Paritygame.node -> unit

    method del_edge : Paritygame.node -> Paritygame.node -> unit
                                                              
    method remove_nodes : Paritygame.nodeset -> unit

    method remove_edges : (Paritygame.node * Paritygame.node) list -> unit


    (********** NODE COLLECTION  **********)
    method collect_nodes : (Paritygame.node -> Paritygame.priority * Paritygame.player * Paritygame.nodeset * Paritygame.nodeset * string option -> bool) -> Paritygame.nodeset

    method collect_nodes_by_prio : (Paritygame.priority -> bool) -> Paritygame.nodeset

    method collect_nodes_by_owner : (Paritygame.player -> bool) -> Paritygame.nodeset * Paritygame.nodeset

    method collect_max_prio_nodes : Paritygame.nodeset

    method collect_max_parity_nodes : Paritygame.nodeset


    (********** SUBGAME **********)
    method subgame_by_edge_pred : (Paritygame.node -> Paritygame.node -> bool) -> 'self

    method subgame_by_node_pred : (Paritygame.node -> bool) -> 'self

    method subgame_by_list : Paritygame.nodeset -> 'self

    method subgame_by_node_filter : (Paritygame.node -> bool) -> 'self * (Paritygame.node -> Paritygame.node) * (Paritygame.node -> Paritygame.node)

    method subgame_by_strat : Paritygame.strategy -> 'self

    method subgame_by_strat_pl : Paritygame.strategy -> Paritygame.player -> 'self


    (********** DOMINION **********)
    method set_closed : Paritygame.nodeset -> Paritygame.player -> bool

    method set_dominion : ('self -> Paritygame.solution * Paritygame.strategy) -> Paritygame.nodeset -> Paritygame.player -> Paritygame.strategy option                                                                 

    (********** DECOMPOSITION **********)                                                                                                                                             
    method strongly_connected_components : Paritygame.nodeset array * Paritygame.scc array * Paritygame.scc list array * Paritygame.scc list

    method sccs_compute_connectors : Paritygame.nodeset array * Paritygame.scc array * Paritygame.scc list array * Paritygame.scc list ->
                                     (Paritygame.scc * Paritygame.scc, (Paritygame.scc * Paritygame.scc) list) Hashtbl.t


    (********** ATTRACTOR CLOSURE **********) 
    method attr_closure_inplace' : Paritygame.strategy -> Paritygame.player -> Paritygame.nodeset -> bool -> (Paritygame.node -> bool) -> bool -> Paritygame.nodeset

    method attr_closure_inplace : Paritygame.strategy -> Paritygame.player -> Paritygame.nodeset -> Paritygame.nodeset
                                                                                                                                                    
    method attractor_closure_inplace_sol_strat : (Paritygame.node -> bool) -> Paritygame.solution -> Paritygame.strategy -> Paritygame.nodeset ->
                                                  Paritygame.nodeset -> Paritygame.nodeset * Paritygame.nodeset


    (********** PARTIAL PARITYGAME **********)
    method induce_partialparitygame : Paritygame.node -> Paritygame.partial_paritygame

    method induce_counting_partialparitygame : Paritygame.node -> int ref * Paritygame.partial_paritygame

    method partially_solve_dominion : Paritygame.node -> Paritygame.partial_solver -> Paritygame.solution * Paritygame.strategy

    method partially_solve_game : Paritygame.partial_solver -> Paritygame.solution * Paritygame.strategy



    (********** GAME INFORMATION **********)
    method get_player_decision_info : bool * bool

    method is_single_parity_game : Paritygame.priority option

    method number_of_strategies : Paritygame.player -> int -> int

    method compute_priority_reach_array : Paritygame.player -> Paritygame.priority array array


    (********** DYNAMIC PARITYGAME **********)
    method to_dynamic_paritygame : Paritygame.dynamic_paritygame

    method to_dynamic_paritygame_by_strategy : Paritygame.strategy -> Paritygame.dynamic_paritygame                                           
  end



(********************************************************
 *                 PARITYGAME BUILDER                   *
 ********************************************************)
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
    val compare : gamenode -> gamenode -> int
    val owner : gamenode -> Paritygame.player
    val priority : gamenode -> Paritygame.priority
    val successors : gamenode -> gamenode list
    val show_node : gamenode -> string option
    val initnodes : unit -> gamenode list
  end

module type PGBuilder =
  sig
    type gamenode
    val build : unit -> array_pg
    val build_from_node : gamenode -> array_pg
    val build_from_nodes : gamenode list -> array_pg
  end

module Build :
  functor (T : PGDescription) ->
    sig
      type gamenode = T.gamenode
      val build : unit -> array_pg
      val build_from_node : gamenode -> array_pg
      val build_from_nodes : gamenode list -> array_pg
    end
