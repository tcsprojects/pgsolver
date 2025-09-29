open Paritygame ;;
  
type ('p,'a) mucalc = Prop of 'p
		    | Var of string
		    | Neg of ('p,'a) mucalc
		    | Imp of ('p,'a) mucalc * ('p,'a) mucalc
		    | Conj of ('p,'a) mucalc list 
		    | Disj of ('p,'a) mucalc list
		    | Diam of 'a option * (('p,'a) mucalc)
		    | Box  of 'a option * (('p,'a) mucalc)
		    | Mu of string * (('p,'a) mucalc)
		    | Nu of string * (('p,'a) mucalc)


module type VerificationProblem =
  sig
    type state
    type proposition
    type action

    val actions : unit -> action list				

    val initstate  : unit -> state
    val labels     : state -> proposition -> bool 
    val successors : state -> action -> state list 

    val show_state : state -> string
    val show_proposition : proposition -> string
    val show_action : action -> string
				  
    val property : unit -> (proposition,action) mucalc
  end;;

module Make: functor (T: VerificationProblem) -> PGBuilder
