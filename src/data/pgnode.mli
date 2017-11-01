(**************************************************************
 *                          NODES                             *
 **************************************************************)

(** Type for nodes. Currently  represented as integer value.
    WARNING: May becomes abstract in the future.
*)
type node = int

(** Returns an undefined node.
    This conforms the integer value of -1.
*)
val nd_undef  : node

(** Creates a string representation of the delivered node.

    @param node node which should be represented as string
    @return string representation
*)
val nd_show : node -> string

(** Compares two nodes.

    @param node node n_one to compare
    @param node node n_two to compare with
    @return -1 if n_one < n_two, 0 if n_one = n_two, 1 if n_one > n_two
*)
val nd_compare : node -> node -> int

