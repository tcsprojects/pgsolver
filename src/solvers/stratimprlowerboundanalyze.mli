open Tcsset
open Paritygame
open Pgstrategy
open Stratimpralgs


type 'a mda = Non | Ele of 'a | Arr of ('a mda) array

val mda_as_ele: 'a mda -> 'a

val mda_as_arr: 'a mda -> ('a mda) array

type 'a mda_flattened1 = (int list, 'a array) TreeMap.t

type ('a, 'b) desc_map = ('a, 'b mda) TreeMap.t

type parsed_node = char * int list

type game_desc_map = (char, int) desc_map

type game_desc_binary_map = (char, bool) desc_map

val parse_game_desc_map: paritygame -> game_desc_map

val game_desc_map_format: game_desc_map -> string

val parse_binary_strategy: paritygame -> strategy -> (parsed_node -> parsed_node -> bool) -> game_desc_binary_map

val game_desc_binary_map_format: game_desc_binary_map -> string

val parse_binary_improving: paritygame -> node_total_ordering_fun -> strategy -> game_valuation -> game_desc_binary_map

type 'a desc_idx = 'a * (int list)

val mda_is_subset_of: 'a mda -> 'a mda -> bool

val desc_map_is_subset_of: ('a, 'b) desc_map -> ('a, 'b) desc_map -> bool

val desc_map_set: ('a, 'b) desc_map -> 'a desc_idx -> 'b -> ('a, 'b) desc_map

val desc_map_get: ('a, 'b) desc_map -> 'a desc_idx -> 'b mda

val desc_map_g: ('a, 'b) desc_map -> 'a -> 'b mda

val desc_map_s: ('a, 'b) desc_map -> 'a -> 'b mda -> ('a, 'b) desc_map
