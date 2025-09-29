open Paritygame

(**************************************************************
 * Priority Promotion Approach Definitions                    *
 **************************************************************)

val is_empty_bset : BitSet.t -> int -> bool
val bit_dom_array_bound : int array -> int -> int -> BitSet.t
val collect_nodes_bit : paritygame -> int -> BitSet.t
val pre_bit : paritygame -> BitSet.t -> BitSet.t -> player -> BitSet.t -> int -> unit
val attr_bit : paritygame -> BitSet.t -> int Queue.t -> int Queue.t -> strategy -> player -> BitSet.t -> int Enum.t -> BitSet.t