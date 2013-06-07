open Paritygame;;


(* Default verification *)
val verify_solution_strategy: paritygame -> solution -> strategy -> (int list * string) option

(* Using universal_solve *)
val verify_solution_strategy_univ: paritygame -> solution -> strategy -> (int list * string) option

(* Using compute_winning_nodes_direct *)
val verify_solution_strategy_direct: paritygame -> solution -> strategy -> (int list * string) option

(* Using a generic algorithm *)
val verify_solution_strategy_generic: paritygame -> solution -> strategy -> (int list * string) option

