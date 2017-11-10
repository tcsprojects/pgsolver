open Pgsolution
open Tcsgameparser
open Paritygame
open Pgstrategy

val parse_parity_game: in_channel -> paritygame
val parse_init_parity_game: in_channel -> Pgnode.node * paritygame
val parse_solution: in_channel -> solution * strategy
