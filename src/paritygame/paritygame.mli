(** Module containing everything related to paritygame.  TODO
    This includes:
      - nodes
      - nodeset
      - nodeset structure
      - player/priority
      - node ordering
      - solution
      - strategy
      - partial paritygames
      - dynamic paritygames
      - decomposition
      - paritygame "template" (virtual class)
      - global solver

    In general, from all of the above, functions that should belong to the paritygame class
    (e.g to_dynamic_paritygame) are part of the class. They can be found in the according segment of the
    class definition.  
    Furthermore solution and strategy depend on paritygame, thus some parts are at the end of the file.

    PGBuilder can be found in arrayparitygame.ml
*)

open Tcsbasedata
open Tcsset
open Pgnode
open Pgnodeset
open Pgplayer
open Pgpriority
open Pgsolution
open Pgstrategy




(**************************************************************
 *                        NODE ORDERING                       *
 **************************************************************)
(** Type of a paritygame ordering. 
    This is a function, which specifies a comparison for two nodes
    to create an ordering.
*)
type pg_ordering  = node * priority * player * nodeset -> node * priority * player * nodeset -> int
                                                                                                  

(** Returns pg_ordering by reward for given player.

    @param player player to get reward-pg_ordering for
    @return reward-pg_ordering for given player
 *)
val ord_rew_for       : player -> pg_ordering

(** Returns pg_ordering by priorities.

    @return pg_ordering by priorities.
 *)
val ord_prio          : pg_ordering

(** Makes given pg_ordering total. Uses compare of Tcsset for former equal nodes.

    @param pg_ordering pg_ordering to make total
    @return total order based on pg_ordering and compare
 *)
val ord_total_by      : pg_ordering -> pg_ordering





                                    
(***************************************************************
 *                SOLUTION/STRATEGY FUNCTIONS                  *
 ***************************************************************)

(** Print solution and strategy.

    @param solution solution to print
    @parm strategy strategy to print
 *)
val print_solution_strategy_parsable : solution -> strategy -> unit



                                                                 
(**************************************************************
 *                   PARTIAL PARITYGAME                       *
 **************************************************************)
(** Type for partial_paritygame.
 *)
type partial_paritygame = node * (node -> node Enumerators.enumerator) * (node -> priority * player) * (node -> string option)
                                                                                                         
(** Type for partial solution.
 *)
type partial_solution = node -> player * node option
                                              
(** Type for partial solver.
 *)
type partial_solver = partial_paritygame -> partial_solution



                                              
(**************************************************************
 *                   DYNAMIC PARITYGAME                       *
 **************************************************************)
(** Type for dynamic paritygame.
 *)
type dynamic_paritygame = (priority * player * string option) Tcsgraph.DynamicGraph.dynamic_graph




                                                                     
(***************************************************************
 *                  DECOMPOSITION FUNCTIONS                    *
 ***************************************************************)
(** Type for a strongly-connected-component.
 *)
type scc = int

(** Returns the leaf SCCs reachable from some SCC in scc_list via topology (scc list array).

    @param scc list scc_list to get leaf SCCs from
    @param scc list array topology
    @return leaf SCCs
*)
val sccs_compute_leaves: scc list -> scc list array -> scc list

(** Returns transposed topology.

    @param scc list array topology to transpose
    @return transposed topology
 *)
val sccs_compute_transposed_topology: scc list array -> scc list array

(** Returns string representation of SCCs.

    @param nodeset array SCCs
    @param scc list array topology
    @param scc list roots
    @return string representation
 *)
val show_sccs : nodeset array -> scc list array -> scc list -> string


                                                                 
                                                                     
(**************************************************************
 *                 (VIRTUAL) PARITYGAME                       *
 **************************************************************)
(** Virtual class representing a paritygame. 
    This class can't be constructed as is and  needs to be inherited 
    In this regard the virtual methods need to be overwritten.
*)
class virtual paritygame : object('self)

  (******************** VIRTUAL METHODS ********************)
        
  (********** GENERAL **********)
  (** Returns size of this paritygame. This does not equals the number of nodes.
      Far more it is the size of the node container of the actual implementation
      of this virtual class.

      @return size of paritygame
  *)
  method virtual size : int

  (** Returns a copy of this object. This is a completely new object.

      @return new paritygame object with same specifications
  *)
  method virtual copy : 'self

  (** Iterates over the whole paritygame and applies the given function to each node.

      @param f function which will be applied to each node. expects node and node properties as arguments
  *)
  method virtual iterate : (node -> (priority * player * nodeset * nodeset * string option) -> unit) -> unit

  (** Iterate over all edges of the paritygame and applies the given function to each edge.

      @param f function to be applied to each edge 
  *)
  method virtual edge_iterate : (node -> node -> unit) -> unit
                                                            
  (** Mapping function. Maps this paritygame with the given function and returns the result as a new game.

      @param f function to map on the current paritygame
      @return new paritygame from mapping f on this
  *)
  method virtual map : (node -> (priority * player * nodeset * nodeset * string option) ->  (priority * player * nodeset * nodeset * string option)) -> 'self

  (** Mapping function. Maps this paritygame with the given function and returns the result as an a'array.

      @param f function to map on the current paritygame
      @return array with mapped results.
  *)                                                                                                                                                        
  method virtual map2 : (node -> (priority * player * nodeset * nodeset * string option) -> 'a) -> 'a array
                                                                                                      

  (********** GETTERS **********)                                                        
  (** Gets node at the given position in container.

      @param int position of wanted node
      @return node if it exists
  *)
  method virtual get_node : node -> (priority * player * nodeset * nodeset * string option)

  (** Gets priority of given node.

      @param node node which priority is wanted
      @return priority of given node 
  *)
  method get_priority : node -> priority
                                          
  (** Gets owner of given node. Owner means the  player which is allowed to choose on this node.

      @param node node which owner is wanted
      @return owner of this node
  *)
  method get_owner : node -> player

  (** Gets sucessors of given node.

      @param node node which sucessors are wanted
      @return set of sucessors of given node
  *)
  method get_successors : node -> nodeset
                                            
  (** Gets predecessors of given node.

      @param node node which predecessors are wanted
      @return set of predecessors of given node
  *)
  method get_predecessors : node -> nodeset
                                              
  (** Gets description of given node as string option.

      @param node node which descr is wanted
      @return description of given node as string option
  *)
  method get_desc : node -> string option
                                           
  (** Gets description of given node as string.

      @param node node which desc is wanted
      @return description of given node as string
  *)
  method get_desc' : node -> string

  (** Gets node from given description as string option.

      @param stringopt description to look for
      @return node with looked for descr
  *)
  method find_desc : string option -> node
                                 
  (** Checks if the given node is defined. Means if it exists in this paritygame.

      @param node node to be checked
      @return if node is defined
  *)
  method virtual is_defined : node -> bool

  (** Formats game. This means it creates a string representation of this game.

      @return string representation of this game
  *)
  method format_game : string
                                 

  (********** SETTERS **********)
  (** Sets given node at the given position in container of paritygame.

      @param int position where node should be positioned
      @param node node to be set
  *)
  method virtual set_node' : node -> (priority * player * nodeset * nodeset * string option) -> unit

  (** Sets node at given position with given properties.
      Parameters are typical node properties.
  *)
  method set_node : node -> priority -> player -> nodeset -> nodeset -> string option -> unit
                                                                                                  
  (** Sets priority for given node.

      @param node node which priority should be set
      @param priorirty priority to be set to given node
  *)
  method set_priority : node -> priority -> unit

  (** Sets owner for given node. Owener means the player which is allowed to choose on this node.

      @param node node which owner should be set
      @param owner player which should be owner of this node
  *)
  method set_owner : node -> player -> unit

  (** Sets description for given node from string option.

      @param node node which desc should be set
      @param stringopt descr for given node
  *)
  method set_desc : node -> string option -> unit

  (** Sets description for given node from string.

      @param node node which descr should be set
      @param string description for given node
  *)
  method set_desc' : node -> string -> unit

  (** Adds edge between two nodes.

      @param node predecessor node
      @param node sucessor node
  *)
  method virtual add_edge : node -> node -> unit

  (** Deletes existing edge between two nodes.

      @param node predecessor node
      @param node sucessor node
  *)
  method virtual del_edge : node -> node -> unit

  (** Removes all nodes from this game that are specified in the given list.

      @param nodeset set of nodes which should be removed.
  *)
  method virtual remove_nodes : nodeset -> unit

  (** Removes all edges of this game that are specified in the given list. Edges are represented as (predecessor * sucessor) in the list.

      @param list list of edges to be removed.
  *)
  method virtual remove_edges : (node * node) list -> unit
                                                        

  (********** SUBGAME **********)                                               
  (** Creates a subgame from specified edges. This means the subgame includes all nodes connected to the
      edges specified by the given function.

      @param f function to specify edges
      @return subgame created by specified edges
  *)
  method subgame_by_edge_pred : paritygame -> (node -> node -> bool) -> paritygame

  (** Creates a subgame from specified nodes. This means the subgame includes all nodes specified by the given function.

      @param f function to specify nodes
      @return subgame created by specified nodes
  *)
  method virtual subgame_by_node_pred : (node -> bool) -> 'self

  (** Create subgame induced and ordered by the nodes list.

      @param nodeset list to create subgame by
      @return subgame created by nodelist
  *)
  method virtual subgame_by_list : nodeset -> 'self * (node -> node) * (node -> node)

  (** Creates subgame by node filter.

      @param f node filter
  *)
  method virtual subgame_by_node_filter : (node -> bool) -> 'self * (node -> node) * (node -> node)


  (******************** NON-VIRTUAL METHODS ********************)

  (********** GENERAL **********)
  (** Prints game on STDOUT s.t. It could be parsed again. 
   *)
  method print : unit

  (** Prints dotty representation of game with solution and strategy
      into out channel.
  
      @param solution 
      @param strategy 
  *) 
  method to_dotty : solution -> strategy -> out_channel -> unit

  (** Creates dotty file from game with solution and strategy.

      @param solution
      @param strategy
      @param string filename
  *)
  method to_dotty_file : solution -> strategy -> string -> unit 


  (********** GETTERS **********)
  (** Returns count of actual existing nodes in this paritygame.

      @return amount of nodes
  *)                                    
  method node_count : int

  (** Returns count of actual existing edges in this paritygame.

      @return amount of edges
  *)
  method edge_count : int

  (** Gets the maximum node for the given pg_ordering.

      @param pg_ordering pg_ordering which determines the max node
      @return max node for given pg_ordering
  *)
  method get_max : pg_ordering -> node

  (** Gets the minimum node for the given pg_ordering.

      @param pg_ordering pg_ordering which determines the min node
      @return min node for given pg_ordering
  *)
  method get_min : pg_ordering -> node

  (** Gets node with maximum priority in this game.

      @return node with maximum priority
  *)
  method get_max_prio_node  : node

  (** Gets the node with the maximum reward for the given player.

      @param player player to get maximum reward node for
      @return maximum reward node for the given player.
  *)
  method get_max_rew_node_for : player -> node

  (** Gets maximum priority occurring in this game.

      @return maximum priority of this game
  *)
  method get_max_prio : priority

  (** Gets the minimum priority occurring in this game.

      @return minimum priority of this game
  *)
  method get_min_prio : priority

  (** Gets the maximum priority for the given player. Means the node with maximum priority that the player benefits from.

      @param player player to get maximum benefit node for
      @return priority of maximum benifit
  *)
  method get_max_prio_for : player -> priority

  (** Gets index of the game. This means the range of priorities.

      @return index of this game
  *)
  method get_index : int

  (** Gets a list of all nodes with same priority as the given one.

      @param priority priority to find nodes with same priority
      @return list of nodes with priority same as the given one
  *)
  method get_prio_nodes : priority -> nodeset

  (** Gets a list of the priorities occurring in this game from the selected ones.

      @param f function which determines which priorities are selected
      @return list of priorities which are selected and occur in this game
  *)
  method get_selected_priorities : (priority -> bool) -> priority list

  (** Gets a list of all priorities occurring in this game.

      @return list of all priorities
  *)
  method  get_priorities : priority list

  (** Returns string representation of game.
   *)
  method to_string : string

  
  (********** NODE COLLECTION  **********)
  (** Returns set of nodes determined by (node -> priority * player * nodeset * nodeset * string option -> bool).

      @param (node -> priority * player * nodeset * nodeset * string option -> bool) function to determine nodes
      @return set of collected nodes
   *)
  method collect_nodes : (node -> priority * player * nodeset * nodeset * string option -> bool) -> nodeset

  (** Returns set of nodes by priority.

      @param (priority -> bool) function to determine which priority should be returned
      @return set of nodes with specified priority
   *)
  method collect_nodes_by_prio : (priority -> bool) -> nodeset

  (** Returns two lists: The first one contains all nodes v for which f v is true. 
      The other one all those for which it is false. 

      @param (player -> bool) function to determine which nodes belong to this player
      @return pair of sets. see above 
  *)
  method collect_nodes_by_owner : (player -> bool) -> nodeset * nodeset

  (** Returns all nodes with greatest priority 

      @return set of nodes with max prio
  *)
  method collect_max_prio_nodes : nodeset

  (** Collects all nodes with maximum parity.

      @return set of nodes with maximum parity.
   *)
  method collect_max_parity_nodes : nodeset


  (********** SUBGAME **********)
  (** Creates a subgame from a given strategy. This means the subgame includes all nodes specified by the given strategy.

      @param strategy strategy to specify nodes
      @return subgame specified by given strategy
  *)
    method subgame_by_strat : paritygame -> Pgstrategy.strategy -> paritygame

  (** Creates a subgame from strategy + player. This means the subgame includes all nodes specified by the given strategy
      or owned by the opponent player of the given player.

      @param strategy strategy to specify nodes
      @param player player which opponents nodes will be included
      @return subgame specified by given strategy and player
  *)

    method subgame_by_strat_pl : paritygame -> Pgstrategy.strategy -> Pgplayer.player -> paritygame
                                                                                                                                 

  (********** DOMINION **********)
  method set_closed: nodeset -> player -> bool
  method set_dominion: ('self -> solution * strategy) -> nodeset -> player -> strategy option

                                                                                       
  (********** DECOMPOSITION **********)
  (** Decomposes the game into its SCCs.
      It returns a tuple (<sccs>, <sccindex>, <topology>, <roots>) where
        
        @param nodeset array  is an array mapping each SCC to its list of nodes,
        @param scc array  is an array mapping each node to its SCC,
        @param scc list array is an array mapping each SCC to the list of its immediate successing SCCs and
        @param scc list is the list of SCCs having no predecessing SCC.
  *)
  method strongly_connected_components : nodeset array * (node -> scc) * scc list array * scc list

  (** Computes connectors for SCCs of game.

      @param nodeset array SCCs
      @param scc array SCC index
      @param scc list array topology
      @param scc list roots
      @return hashtable of connectors
   *)
  method sccs_compute_connectors : nodeset array * (node -> scc) * scc list array * scc list -> (scc * scc, (node * node) list) Hashtbl.t


  (********** ATTRACTOR CLOSURE **********) 
  (** 
      @param strategy strategy 
      @param player player 
      @param nodeset region 
      @param bool include_region 
      @param (node -> bool) tgraph 
      @param bool deltafilter 
      @param nodeset overwrite_strat 
  *)
  method attr_closure_inplace' : strategy -> player -> nodeset -> bool -> (node -> bool) -> bool -> nodeset

  (** Returns the attractor for the given player and region. 
      Additionally all necessary strategy decisions for player leading 
      into the region are added to strategy

      @param strategy strategy
      @param player player
      @param nodeset region
      @return attractor
  *)
  method attr_closure_inplace : strategy -> player -> nodeset -> nodeset
  method attractor_closure_inplace_sol_strat : (node -> bool) -> solution -> strategy -> nodeset -> nodeset -> (nodeset * nodeset)

                                                                                                                 
  (********** PARTIAL PARITYGAME **********)
  (** Induces partial paritygame by startnode.

      @param node start node
      @return partial paritygame
  *)
  method induce_partialparitygame : node -> partial_paritygame

  (** Induces partial paritygame with counter by startnode.

      @param node startnode
      @return pair of counter and partial paritygame
   *)
  method induce_counting_partialparitygame : node -> int ref * partial_paritygame

  method partially_solve_dominion : node -> partial_solver -> solution * strategy
  method partially_solve_game : partial_solver -> solution * strategy

                                                               
  (********** GAME INFORMATION **********)
  method get_player_decision_info : bool * bool
  method is_single_parity_game : priority option

  (** Computes the number of strategies for a player in a game.
      

      @param player player to get amount of strats for
      @param int upper bound on the returned value 
      @return amount of strategies for player (bounded by m)
  *)
  method number_of_strategies : player -> int -> int


  (********** DYNAMIC PARITYGAME **********)
  (** Create dynamic paritygame out of this.

      @return dynamic paritygame corresponding to this
   *)
  method to_dynamic_paritygame : dynamic_paritygame

  (** Create dynamic paritygame out of this determined by strategy.

      @param strategy determines how dynamic paritygame looks
      @return dynamic paritygame determined by strategy
   *)
  method to_dynamic_paritygame_by_strategy : strategy -> dynamic_paritygame
 

  (********** MODAL LOGIC **********)
  (** Returns the set of all nodes in this pg that have at least one successor in nodeset.

      @param nodeset nodeset to look for successor
      @return set of all nodes with one or more successors in nodeset
  *)
  method get_diamonds : nodeset -> nodeset

  (** Returns set of all nodes in this pg that have all successors in nodeset.

      @param nodeset nodeset to look for all successors
      @return set of nodes with all successors in nodeset
   *)
  method get_boxes     : nodeset -> nodeset
end



                             
(**************************************************************
 *                     SOLUTION PART 2                        *
 **************************************************************)

(** Create solution for game,
    Initially filled with values determined by (node -> player).

    @param game game to create solution for
    @param (node -> player) function to determine values for solution
    @return initialized solution
 *)
val  sol_init   : paritygame -> (node -> player) -> solution

val  str_init   : paritygame -> (node -> node) -> strategy

                                                      

(**************************************************************
 *                       GLOBAL SOLVER                        *
 **************************************************************)
(** Solver type.
    A type for algorithms that solve a paritygame.
 *)
type global_solver = paritygame -> solution * strategy






