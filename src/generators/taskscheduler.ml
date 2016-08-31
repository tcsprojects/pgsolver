open Paritygame;;

let number_cpus = ref 2
let number_tasks = ref 5
let waiting_time = ref 7
let running_time = ref 3
		       
module Tasks =
  struct
    type status = Waiting
		| Interrupted
		| Sleeping of int
		| Running of int
    type importance = int
    type task = status * importance
    type tasks = task array

    type formulaType = FP of int * int             (* fixpoint formula with priority and next subformula *)
                     | BOOL of player * int * int     (* con-/disjunction with player and left and right subformula *)
                     | MOD of player * int            (* modality with player and next subformula *)
                     | PROP of (tasks -> bool)  (* proposition with function that evaluates it in a state *)

    type formula = int

    let formula = Array.init (!number_tasks * 2 + ....) 
						    
    type gamenode = tasks * formula
			     
    let initconf = Array.init (fun i -> (Waiting !waiting_time, i))	   
  end;;
