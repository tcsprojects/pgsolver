open Paritygame ;;

val solve_scc_reach: paritygame -> player -> (int * int) array array -> ((int * int) array -> int -> unit) -> solution * strategy  (* TODO: replace int by proper types from paritygame.mli, is it node?  -- ints, counting number of occurrences *)

val solve : paritygame -> solution * strategy
