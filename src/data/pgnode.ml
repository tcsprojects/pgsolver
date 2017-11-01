(**************************************************************
 *                          NODES                             *
 **************************************************************)
type node = int

let nd_undef = -1
let nd_make v = v
let nd_reveal v = v

let nd_show = string_of_int

let nd_compare = compare
