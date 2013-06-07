open Tcsbasedata

type paritygame = (int * int * int array * string option) array
type solution = int array
type strategy = int array
type global_solver_factory = string array -> (paritygame -> solution * strategy)

val mem_solver: string -> bool

val find_solver: string -> global_solver_factory * string * string

val enum_solvers: (global_solver_factory -> string -> string -> string -> unit) -> unit

val fold_solvers: (global_solver_factory -> string -> string -> string -> 'a -> 'a) -> 'a -> 'a


type partial_paritygame = int * (int -> int Enumerators.enumerator) * (int -> int * int) * (int -> string option)
type partial_solution = int -> int * int option
type partial_solver_factory = string array -> (partial_paritygame -> partial_solution)

val mem_partial_solver: string -> bool

val find_partial_solver: string -> partial_solver_factory * string * string

val enum_partial_solvers: (partial_solver_factory -> string -> string -> string -> unit) -> unit

val fold_partial_solvers: (partial_solver_factory -> string -> string -> string -> 'a -> 'a) -> 'a -> 'a
