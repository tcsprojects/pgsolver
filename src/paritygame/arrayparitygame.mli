val add_edge_in_node_array :
  ('a * 'b * Paritygame.nodeset * Paritygame.nodeset * 'c) array ->
  Paritygame.node -> Paritygame.node -> unit
class array_pg :
  ?initFunc:(Paritygame.node ->
             Paritygame.priority * Paritygame.player * Paritygame.node list *
             string option) ->
  int ->
  object ('b)
    val mutable nodes :
      (Paritygame.priority * Paritygame.player * Paritygame.nodeset *
       Paritygame.nodeset * string option)
      array
    method add_edge : Paritygame.node -> Paritygame.node -> unit
    method attr_closure_inplace :
      Paritygame.strategy ->
      Paritygame.player -> Paritygame.nodeset -> Paritygame.nodeset
    method attr_closure_inplace' :
      Paritygame.strategy ->
      Paritygame.player ->
      Paritygame.nodeset ->
      bool -> (Paritygame.node -> bool) -> bool -> Paritygame.nodeset
    method attractor_closure_inplace_sol_strat :
      (Paritygame.node -> bool) ->
      Paritygame.solution ->
      Paritygame.strategy ->
      Paritygame.nodeset ->
      Paritygame.nodeset -> Paritygame.nodeset * Paritygame.nodeset
    method collect_max_parity_nodes : Paritygame.nodeset
    method collect_max_prio_nodes : Paritygame.nodeset
    method collect_nodes :
      (Paritygame.node ->
       Paritygame.priority * Paritygame.player * Paritygame.nodeset *
       Paritygame.nodeset * string option -> bool) ->
      Paritygame.nodeset
    method collect_nodes_by_owner :
      (Paritygame.player -> bool) -> Paritygame.nodeset * Paritygame.nodeset
    method collect_nodes_by_prio :
      (Paritygame.priority -> bool) -> Paritygame.nodeset
    method compute_priority_reach_array :
      Paritygame.player -> Paritygame.priority array array
    method copy : 'b
    method del_edge : Paritygame.node -> Paritygame.node -> unit
    method edge_count : int
    method edge_iterate :
      (Paritygame.node -> Paritygame.node -> unit) -> unit
    method find_desc : string option -> Paritygame.node
    method format_game : string
    method get_boxes : Paritygame.NodeSet.t -> Paritygame.NodeSet.t
    method get_desc : Paritygame.node -> string option
    method get_desc' : Paritygame.node -> string
    method get_diamonds : Paritygame.NodeSet.t -> Paritygame.NodeSet.t
    method get_index : int
    method get_max : Paritygame.pg_ordering -> Paritygame.node
    method get_max_prio : Paritygame.priority
    method get_max_prio_for : Paritygame.player -> Paritygame.priority
    method get_max_prio_node : Paritygame.node
    method get_max_rew_node_for : Paritygame.player -> Paritygame.node
    method get_min : Paritygame.pg_ordering -> Paritygame.node
    method get_min_prio : Paritygame.priority
    method get_node :
      int ->
      Paritygame.priority * Paritygame.player * Paritygame.nodeset *
      Paritygame.nodeset * string option
    method get_owner : Paritygame.node -> Paritygame.player
    method get_player_decision_info : bool * bool
    method get_predecessors : Paritygame.node -> Paritygame.nodeset
    method get_prio_nodes : Paritygame.priority -> Paritygame.nodeset
    method get_priorities : Paritygame.priority list
    method get_priority : Paritygame.node -> Paritygame.priority
    method get_selected_priorities :
      (Paritygame.priority -> bool) -> Paritygame.priority list
    method get_successors : Paritygame.node -> Paritygame.nodeset
    method induce_counting_partialparitygame :
      Paritygame.node -> int ref * Paritygame.partial_paritygame
    method induce_partialparitygame :
      Paritygame.node -> Paritygame.partial_paritygame
    method private init :
      (Paritygame.node ->
       Paritygame.priority * Paritygame.player * Paritygame.node list *
       string option) ->
      unit
    method is_defined : Paritygame.node -> bool
    method is_single_parity_game : Paritygame.priority option
    method iterate :
      (Paritygame.node ->
       Paritygame.priority * Paritygame.player * Paritygame.nodeset *
       Paritygame.nodeset * string option -> unit) ->
      unit
    method map :
      (Paritygame.node ->
       Paritygame.priority * Paritygame.player * Paritygame.nodeset *
       Paritygame.nodeset * string option ->
       Paritygame.priority * Paritygame.player * Paritygame.nodeset *
       Paritygame.nodeset * string option) ->
      'b
    method map2 :
      (Paritygame.node ->
       Paritygame.priority * Paritygame.player * Paritygame.nodeset *
       Paritygame.nodeset * string option -> 'a) ->
      'a array
    method node_count : int
    method number_of_strategies : Paritygame.player -> int -> int
    method partially_solve_dominion :
      Paritygame.node ->
      Paritygame.partial_solver -> Paritygame.solution * Paritygame.strategy
    method partially_solve_game :
      Paritygame.partial_solver -> Paritygame.solution * Paritygame.strategy
    method print : unit
    method remove_edges : (Paritygame.node * Paritygame.node) list -> unit
    method remove_nodes : Paritygame.nodeset -> unit
    method sccs_compute_connectors :
      Paritygame.nodeset array * Paritygame.scc array *
      Paritygame.scc list array * Paritygame.scc list ->
      (Paritygame.scc * Paritygame.scc,
       (Paritygame.scc * Paritygame.scc) list)
      Hashtbl.t
    method set_closed : Paritygame.nodeset -> Paritygame.player -> bool
    method set_desc : Paritygame.node -> string option -> unit
    method set_desc' : Paritygame.node -> string -> unit
    method set_dominion :
      ('b -> Paritygame.solution * Paritygame.strategy) ->
      Paritygame.nodeset -> Paritygame.player -> Paritygame.strategy option
    method set_node :
      int ->
      Paritygame.priority ->
      Paritygame.player ->
      Paritygame.nodeset -> Paritygame.nodeset -> string option -> unit
    method set_node' :
      int ->
      Paritygame.priority * Paritygame.player * Paritygame.nodeset *
      Paritygame.nodeset * string option -> unit
    method set_owner : Paritygame.node -> Paritygame.player -> unit
    method set_priority : Paritygame.node -> Paritygame.priority -> unit
    method size : int
    method sort :
      (Paritygame.priority * Paritygame.player * Paritygame.nodeset *
       Paritygame.nodeset * string option ->
       Paritygame.priority * Paritygame.player * Paritygame.nodeset *
       Paritygame.nodeset * string option -> int) ->
      unit
    method strongly_connected_components :
      Paritygame.nodeset array * Paritygame.scc array *
      Paritygame.scc list array * Paritygame.scc list
    method subgame_by_edge_pred :
      (Paritygame.node -> Paritygame.node -> bool) -> 'b
    method subgame_by_list : Paritygame.nodeset -> 'b
    method subgame_by_node_filter :
      (Paritygame.node -> bool) ->
      'b * (Paritygame.node -> Paritygame.node) *
      (Paritygame.node -> Paritygame.node)
    method subgame_by_node_pred : (Paritygame.node -> bool) -> 'b
    method subgame_by_strat : Paritygame.strategy -> 'b
    method subgame_by_strat_pl :
      Paritygame.strategy -> Paritygame.player -> 'b
    method to_dotty :
      Paritygame.solution -> Paritygame.strategy -> out_channel -> unit
    method to_dotty_file :
      Paritygame.solution -> Paritygame.strategy -> string -> unit
    method to_dynamic_paritygame : Paritygame.dynamic_paritygame
    method to_dynamic_paritygame_by_strategy :
      Paritygame.strategy -> Paritygame.dynamic_paritygame
    method to_string : string
  end
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
