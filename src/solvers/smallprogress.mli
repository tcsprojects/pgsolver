open Paritygame ;;

val solve_scc_reach: paritygame -> player -> (int * int) array array -> ((int * int) array -> int -> unit) -> solution * strategy

val solve : paritygame -> solution * strategy
val register: unit -> unit