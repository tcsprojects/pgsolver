open Paritygame;;
open Tcsset;;

type global_solver_factory = string array -> global_solver

let solvermap = ref TreeMap.empty_def;;

let register_solver_factory solver_func identifier abbreviation description =
	if TreeMap.mem identifier !solvermap
	then failwith ("Solver `" ^ identifier ^ "' already registered!\n")
	else solvermap := TreeMap.add identifier (solver_func, abbreviation, description) !solvermap;;
	
let register_solver solver_func = register_solver_factory (fun _ -> solver_func);;

let mem_solver identifier = TreeMap.mem identifier !solvermap;;

let find_solver identifier = TreeMap.find identifier !solvermap;;

let enum_solvers it = TreeMap.iter (fun i (f, a, d) -> it f i a d) !solvermap;;

let fold_solvers fo b = TreeMap.fold (fun i (f, a, d) x -> fo f i a d x) !solvermap b;;


type partial_solver_factory = string array -> partial_solver

let partialsolvermap = ref TreeMap.empty_def;;

let register_partial_solver_factory solver_func identifier abbreviation description =
	if TreeMap.mem identifier !partialsolvermap
	then failwith ("Partial Solver `" ^ identifier ^ "' already registered!\n")
	else partialsolvermap := TreeMap.add identifier (solver_func, abbreviation, description) !partialsolvermap;;
	
let register_partial_solver solver_func = register_partial_solver_factory (fun _ -> solver_func);;

let mem_partial_solver identifier = TreeMap.mem identifier !partialsolvermap;;

let find_partial_solver identifier = TreeMap.find identifier !partialsolvermap;;

let enum_partial_solvers it = TreeMap.iter (fun i (f, a, d) -> it f i a d) !partialsolvermap;;

let fold_partial_solvers fo b = TreeMap.fold (fun i (f, a, d) x -> fo f i a d x) !partialsolvermap b;;