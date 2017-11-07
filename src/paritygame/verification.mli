open Paritygame;;

type verifier = paritygame -> Pgsolution.solution -> strategy -> (Pgnode.node list * string) option
									  
(* Default verification *)
val verify_solution_strategy: verifier

(* Using universal_solve *)
val verify_solution_strategy_univ: verifier

(* Using compute_winning_nodes_direct *)
val verify_solution_strategy_direct: verifier

(* Using a generic algorithm *)
val verify_solution_strategy_generic: verifier

