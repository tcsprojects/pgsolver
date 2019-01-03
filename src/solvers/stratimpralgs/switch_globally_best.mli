open Paritygame;;
open Stratimpralgs;;
open Pgnode;;
open Pgstrategy;;

val register: unit -> unit

val improvement_policy_optimize_all_globally' :
  (node -> node -> bool) ->
  paritygame -> node_total_ordering_fun -> strategy -> game_valuation -> strategy

val improvement_policy_optimize_all_globally :
  paritygame -> node_total_ordering_fun -> strategy -> game_valuation -> strategy

