open Tcsgameparser
open Paritygame

val parse_parity_game: in_channel -> paritygame
val parse_init_parity_game: in_channel -> node * paritygame
val parse_solution: in_channel -> solution * strategy
