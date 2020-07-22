open Pgnode;;
open Tcsset;;
open Tcsbasedata;;

(**************************************************************
 *                          NODESET                           *
 **************************************************************)

type nodeset = node TreeSet.t

let ns_isEmpty = TreeSet.is_empty
let ns_compare = TreeSet.compare
let ns_empty = TreeSet.empty Pgnode.nd_compare
let ns_elem = TreeSet.mem
let ns_fold f acc ns = TreeSet.fold (fun x y -> f y x) ns acc
let ns_iter = TreeSet.iter
let ns_filter = TreeSet.filter
let ns_map = TreeSet.map
let ns_size = TreeSet.cardinal
let ns_exists = TreeSet.exists
let ns_forall = TreeSet.for_all
let ns_first = TreeSet.min_elt
let ns_last = TreeSet.max_elt
let ns_some = TreeSet.choose
let ns_add = TreeSet.add
let ns_del = TreeSet.remove
let ns_union = TreeSet.union
let ns_make = TreeSet.of_list Pgnode.nd_compare
let ns_nodes = TreeSet.elements
let ns_diff = TreeSet.diff
let ns_inter = TreeSet.inter
let ns_union = TreeSet.union
let ns_subset = TreeSet.subset

let ns_find f ns =
    OptionUtils.get_some (ns_fold (fun a v -> if a = None && f v then Some v else a) None ns)

let ns_some ws =
  let n = ns_size ws in
  let i = ref (Random.int n) in
  ns_find (fun v ->
    decr i;
    !i = -1
  ) ws

let ns_max ns lessf = ns_fold (fun v -> fun w -> if lessf v w then w else v) (ns_some ns) ns

let ns_nodes = TreeSet.elements

let ns_random tr =
  let a = Array.of_list (ns_nodes tr) in
  a.(0)
