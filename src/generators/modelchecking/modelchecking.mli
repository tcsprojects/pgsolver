open Tcsset;;

module type TransitionSystem =
  sig
    type proposition
    type action
    type state

    val initstate : state
    val labels : state -> proposition Tcsset.t
    val successors : state -> action -> state Tcsset.t

    val action_list : action list
    (* val prop_list : proposition list *)
				
    val show_state : state -> string
  end;;
