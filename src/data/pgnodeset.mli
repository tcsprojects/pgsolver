open Pgnode

(**************************************************************
 *                          NODESET                           *
 **************************************************************)

(* Functions for representing sets of nodes, particularly for successors
   and predecessors of given nodes *)

(** Type for nodesets.

    @see <https://github.com/tcsprojects/tcslib/blob/master/src/data/tcsset.ml> Treeset of TCSSet
*)
type nodeset

(** Checks if the given nodeset is empty.

    @param nodeset set to be checked
    @return if nodeset is empty
*)
val ns_isEmpty : nodeset -> bool

(** Checks if the given node is a member of the given nodeset.

    @param node node to be checked
    @param nodeset set to check if node is included
    @return if node is included
*)
val ns_elem    : node -> nodeset -> bool

(** Compares two nodesets.

    @param nodeset nodeset ns_one to compare
    @param nodeset nodeset ns_two to compare with
    @return -1 if ns_one < ns_two, 0 if ns_one = ns_two, 1 if ns_one > ns_two
*)
val ns_compare : nodeset -> nodeset -> int

(** Constructor for empty nodeset.

    @return empty nodeset
*)
val ns_empty   : nodeset

(** Creates nodeset out of nodelist.

   @param nodelist list of nodes from which set should be created
   @return set of nodes from node list
*)
val ns_make    : node list -> nodeset

(** Returns the number of nodes in a nodeset, e.g. the size.

    @param nodeset nodeset which size should be calculated
    @return size of nodeset
*)
val ns_size    : nodeset -> int


(********** ITERATOR FUNCTIONS **********)
(** Fold nodeset.
 *)
val ns_fold    : ('a -> node -> 'a) -> 'a -> nodeset -> 'a

(** Iterate nodeset.
 *)
val ns_iter    : (node -> unit) -> nodeset -> unit

(** Map nodeset.
 *)
val ns_map     : (node -> node) -> nodeset -> nodeset

(** Filter nodeset via filter function

    @param (node -> bool) filter funciton
    @param nodeset nodeset to be filtered
    @retrun filtered nodeset
 *)
val ns_filter  : (node -> bool) -> nodeset -> nodeset


(********** FINDING ELEMENTS **********)
(** Checks if via specified node does exist.

    @param (node -> bool) specifier
    @param nodeset nodeset to be checked
    @return if node with specification exists
 *)
val ns_exists  : (node -> bool) -> nodeset -> bool

(** Checks if specification holds for all nodes.

    @param (node -> bool) specifier
    @param nodeset nodeset to be checked
    @return if specification holds for all nodes
 *)
val ns_forall  : (node -> bool) -> nodeset -> bool

(** Finds and returns specified node option.

    @param (node -> bool) specifier
    @param nodeset nodeset to be searched
    @return if found Some node if not None
 *)
val ns_find    : (node -> bool) -> nodeset -> node

(** Returns maximum node.
    The maximum is defined via given comparator function.

    @param nodeset nodeset to be searched
    @param (node -> node -> bool) comparator function ( true node one is smaller, false node one is bigger ).
    @return maximum node for comparator
 *)
val ns_max     : nodeset -> (node -> node -> bool) -> node

(** Returns a randomly chosen element from a node set.

    @param nodeset nodeset to get random node from
    @return random node
 *)
val ns_some    : nodeset -> node

(** Returns the smallest (by name) node in a nodeset.

    @param nodeset nodeset to get smallest node from
    @return smallest node by name
 *)
val ns_first   : nodeset -> node

(** Returns greatest (by name) node in a nodeset.

    @param nodeset nodeset to get greatest node from
    @param greatest node by name
*)
val ns_last    : nodeset -> node

val ns_subset: nodeset -> nodeset -> bool

(** Add node to nodeset.

    @param node node to add
    @param nodeset nodeset to add to
    @return extended nodeset
 *)

(********** MODIFICATION **********)
val ns_add     : node -> nodeset -> nodeset

(** Delete node from nodeset.

    @param node node to be deleted
    @param nodeset nodeset to remove node from
    @return narrowed nodeset
 *)
val ns_del     : node -> nodeset -> nodeset

(** Unifies two nodesets.

    @param nodeset nodeset one to be unified
    @param nodeset nodeset two to be unified
    @return unified nodeset from nodeset one and nodeset two
 *)
val ns_union   : nodeset -> nodeset -> nodeset


(** Extract a list of nodes from a nodeset.

    @param nodeset nodeset to extract nodes as list from
    @return node list from nodeset
 *)
val ns_nodes   : nodeset -> node list


val ns_diff: nodeset -> nodeset -> nodeset

val ns_inter: nodeset -> nodeset -> nodeset

val ns_union: nodeset -> nodeset -> nodeset

val ns_random    : nodeset -> node
