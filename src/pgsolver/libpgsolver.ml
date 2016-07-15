open Tcsbasedata;;
open Paritygame;;
  
let __dummy_global = [Recursive.solve, Stratimprovement.solve, Optstratimprov.solve, Localmodelchecker.solve, Dominiondecomp.solve, Guessstrategy.solve, Smallprogress.solve, Bigstep.solve]

type global_solver_factory = string array -> (paritygame -> solution * strategy)

let encode fact args =
		let f = fact args in
		(* fun pg -> f (Paritygame.pg_init (Array.length pg) (fun i -> pg.(i))) *)
		fun game -> f (Paritygame.pg_init (pg_size game) (fun i -> pg_get_node game i))

let mem_solver = Solvers.mem_solver

let find_solver s =
	let (fact, x, y) = Solvers.find_solver s in
	(encode fact, x, y)

let enum_solvers f = Solvers.enum_solvers (fun fact -> f (encode fact))

let fold_solvers f = Solvers.fold_solvers (fun fact -> f (encode fact))


let __dummy_partial = [Localmodelchecker.partially_solve, Stratimprlocal.partially_solve, Stratimprlocal2.partially_solve]

type partial_paritygame = int * (int -> int Enumerators.enumerator) * (int -> int * int) * (int -> string option)
type partial_solution = int -> int * int option
type partial_solver_factory = string array -> (partial_paritygame -> partial_solution)

let mem_partial_solver = Solvers.mem_partial_solver

let find_partial_solver = Solvers.find_partial_solver

let enum_partial_solvers = Solvers.enum_partial_solvers

let fold_partial_solvers = Solvers.fold_partial_solvers
