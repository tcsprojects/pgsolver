open Tcsset;;
open Paritygame;;

class map_pg :
  object ('self)
           
    val mutable nodes : (Pgnode.node, Pgpriority.priority * Pgplayer.player * Pgnodeset.nodeset * Pgnodeset.nodeset * string option) TreeMap.t

                                                                                                                            
    (********** GENERAL **********)  
    method size : int
                    
    method copy : 'self
                    
    method iterate : (Pgnode.node -> Pgpriority.priority * Pgplayer.player * Pgnodeset.nodeset * Pgnodeset.nodeset * string option -> unit) -> unit
                                                                                                                                                         
    method edge_iterate : (Pgnode.node -> Pgnode.node -> unit) -> unit

    method map : (Pgnode.node -> Pgpriority.priority * Pgplayer.player * Pgnodeset.nodeset * Pgnodeset.nodeset * string option ->
                  Pgpriority.priority * Pgplayer.player * Pgnodeset.nodeset * Pgnodeset.nodeset * string option) -> 'self
                                                                                                                          
    method map2 : 'a. (Pgnode.node -> (Pgpriority.priority * Pgplayer.player * Pgnodeset.nodeset * Pgnodeset.nodeset * string option) -> 'a) -> 'a array
                                                                                                                                                     

    (********** GETTERS **********)                                                                                                                                                 
    method get_node : Pgnode.node -> Pgpriority.priority * Pgplayer.player * Pgnodeset.nodeset * Pgnodeset.nodeset * string option

    method get_priority : Pgnode.node -> Pgpriority.priority

    method get_owner : Pgnode.node -> Pgplayer.player

    method get_successors : Pgnode.node -> Pgnodeset.nodeset

    method get_predecessors : Pgnode.node -> Pgnodeset.nodeset

    method get_desc : Pgnode.node -> string option
                                                
    method get_desc' : Pgnode.node -> string

    method find_desc : string option -> Pgnode.node

    method is_defined : Pgnode.node -> bool

    method format_game : string

    method get_boxes : Pgnodeset.nodeset -> Pgnodeset.nodeset

    method get_diamonds : Pgnodeset.nodeset -> Pgnodeset.nodeset

    method get_index : int

    method get_max : Paritygame.pg_ordering -> Pgnode.node

    method get_max_prio : Pgpriority.priority

    method get_max_prio_for : Pgplayer.player -> Pgpriority.priority

    method get_max_prio_node : Pgnode.node

    method get_max_rew_node_for : Pgplayer.player -> Pgnode.node

    method get_min : Paritygame.pg_ordering -> Pgnode.node

    method get_min_prio : Pgpriority.priority

    method get_prio_nodes : Pgpriority.priority -> Pgnodeset.nodeset

    method get_priorities : Pgpriority.priority list

    method get_selected_priorities : (Pgpriority.priority -> bool) -> Pgpriority.priority list

    method edge_count : int

    method node_count : int

    method print : unit
  
    method to_dotty : Pgsolution.solution -> Pgstrategy.strategy -> out_channel -> unit

    method to_dotty_file : Pgsolution.solution -> Pgstrategy.strategy -> string -> unit

    method to_string : string

                         
    (********** SETTERS **********)
    method set_node' : Pgnode.node -> Pgpriority.priority * Pgplayer.player * Pgnodeset.nodeset * Pgnodeset.nodeset * string option -> unit

    method set_node : Pgnode.node -> Pgpriority.priority -> Pgplayer.player -> Pgnodeset.nodeset -> Pgnodeset.nodeset -> string option -> unit
                                                
    method set_priority : Pgnode.node -> Pgpriority.priority -> unit

    method set_owner : Pgnode.node -> Pgplayer.player -> unit

    method set_desc : Pgnode.node -> string option -> unit

    method set_desc' : Pgnode.node -> string -> unit

    method add_edge : Pgnode.node -> Pgnode.node -> unit

    method del_edge : Pgnode.node -> Pgnode.node -> unit
                                                              
    method remove_nodes : Pgnodeset.nodeset -> unit

    method remove_edges : (Pgnode.node * Pgnode.node) list -> unit


    (********** NODE COLLECTION  **********)
    method collect_nodes : (Pgnode.node -> Pgpriority.priority * Pgplayer.player * Pgnodeset.nodeset * Pgnodeset.nodeset * string option -> bool) -> Pgnodeset.nodeset

    method collect_nodes_by_prio : (Pgpriority.priority -> bool) -> Pgnodeset.nodeset

    method collect_nodes_by_owner : (Pgplayer.player -> bool) -> Pgnodeset.nodeset * Pgnodeset.nodeset

    method collect_max_prio_nodes : Pgnodeset.nodeset

    method collect_max_parity_nodes : Pgnodeset.nodeset


    (********** SUBGAME **********)
    method subgame_by_edge_pred : paritygame -> (Pgnode.node -> Pgnode.node -> bool) -> paritygame

    method subgame_by_list : Pgnodeset.nodeset -> 'self * (Pgnode.node -> Pgnode.node) * (Pgnode.node -> Pgnode.node)

    method subgame_by_node_filter : (Pgnode.node -> bool) -> 'self * (Pgnode.node -> Pgnode.node) * (Pgnode.node -> Pgnode.node)

    method subgame_by_strat : paritygame -> Pgstrategy.strategy -> paritygame

    method subgame_by_strat_pl : paritygame -> Pgstrategy.strategy -> Pgplayer.player -> paritygame


    (********** DOMINION **********)
    method set_closed : Pgnodeset.nodeset -> Pgplayer.player -> bool

    method set_dominion : ('self -> Pgsolution.solution * Pgstrategy.strategy) -> Pgnodeset.nodeset -> Pgplayer.player -> Pgstrategy.strategy option

    (********** DECOMPOSITION **********)                                                                                                                                             
    method strongly_connected_components : Pgnodeset.nodeset array * (Pgnode.node -> Paritygame.scc)* Paritygame.scc list array * Paritygame.scc list

    method sccs_compute_connectors : Pgnodeset.nodeset array * (Pgnode.node -> Paritygame.scc) * Paritygame.scc list array * Paritygame.scc list ->
                                     (Paritygame.scc * Paritygame.scc, (Pgnode.node * Pgnode.node) list) Hashtbl.t


    (********** ATTRACTOR CLOSURE **********) 
    method attr_closure_inplace' : Pgstrategy.strategy -> Pgplayer.player -> Pgnodeset.nodeset -> bool -> (Pgnode.node -> bool) -> bool -> Pgnodeset.nodeset

    method attr_closure_inplace : Pgstrategy.strategy -> Pgplayer.player -> Pgnodeset.nodeset -> Pgnodeset.nodeset
                                                                                                                                                    
    method attractor_closure_inplace_sol_strat : (Pgnode.node -> bool) -> Pgsolution.solution -> Pgstrategy.strategy -> Pgnodeset.nodeset ->
                                                  Pgnodeset.nodeset -> Pgnodeset.nodeset * Pgnodeset.nodeset


    (********** PARTIAL PARITYGAME **********)
    method induce_partialparitygame : Pgnode.node -> Paritygame.partial_paritygame

    method induce_counting_partialparitygame : Pgnode.node -> int ref * Paritygame.partial_paritygame

    method partially_solve_dominion : Pgnode.node -> Paritygame.partial_solver -> Pgsolution.solution * Pgstrategy.strategy

    method partially_solve_game : Paritygame.partial_solver -> Pgsolution.solution * Pgstrategy.strategy



    (********** GAME INFORMATION **********)
    method get_player_decision_info : bool * bool

    method is_single_parity_game : Pgpriority.priority option

    method number_of_strategies : Pgplayer.player -> int -> int


    (********** DYNAMIC PARITYGAME **********)
    method to_dynamic_paritygame : Paritygame.dynamic_paritygame

    method to_dynamic_paritygame_by_strategy : Pgstrategy.strategy -> Paritygame.dynamic_paritygame
  end