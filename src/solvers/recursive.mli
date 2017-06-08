open Paritygame ;;
open Basics ;;
open Univsolve;;

val solve : paritygame -> solution * strategy
val register : unit -> unit

val solve2 : (paritygame -> solution * strategy) ref -> paritygame -> solution * strategy

val fallback_solve: paritygame -> (paritygame -> solution * strategy) -> universal_solve_options -> solution * strategy

val mcnaughton_zielonka : paritygame -> universal_solve_options -> solution * strategy

