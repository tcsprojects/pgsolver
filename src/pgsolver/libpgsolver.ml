open Tcsbasedata;;

let __dummy_global = [Recursive.solve, Stratimprovement.solve, Optstratimprov.solve, Localmodelchecker.solve, Dominiondecomp.solve, Guessstrategy.solve, Smallprogress.solve, Bigstep.solve]

type paritygame = (int * int * int array * string option) array
type solution = int array
type strategy = int array
type global_solver_factory = string array -> (paritygame -> solution * strategy)

let mem_solver = Solvers.mem_solver

let find_solver = Solvers.find_solver

let enum_solvers = Solvers.enum_solvers

let fold_solvers = Solvers.fold_solvers


let __dummy_partial = [Localmodelchecker.partially_solve, Stratimprlocal.partially_solve, Stratimprlocal2.partially_solve]

type partial_paritygame = int * (int -> int Enumerators.enumerator) * (int -> int * int) * (int -> string option)
type partial_solution = int -> int * int option
type partial_solver_factory = string array -> (partial_paritygame -> partial_solution)

let mem_partial_solver = Solvers.mem_partial_solver

let find_partial_solver = Solvers.find_partial_solver

let enum_partial_solvers = Solvers.enum_partial_solvers

let fold_partial_solvers = Solvers.fold_partial_solvers
