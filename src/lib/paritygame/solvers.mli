open Paritygame
open Solverregistry

val register_solver_factory: global_solver_factory -> string -> string -> string -> unit

val register_solver: global_solver -> string -> string -> string -> unit

val mem_solver: string -> bool

val find_solver: string -> global_solver_factory * string * string

val enum_solvers: (global_solver_factory -> string -> string -> string -> unit) -> unit

val fold_solvers: (global_solver_factory -> string -> string -> string -> 'a -> 'a) -> 'a -> 'a


val register_partial_solver_factory: partial_solver_factory -> string -> string -> string -> unit

val register_partial_solver: partial_solver -> string -> string -> string -> unit

val mem_partial_solver: string -> bool

val find_partial_solver: string -> partial_solver_factory * string * string

val enum_partial_solvers: (partial_solver_factory -> string -> string -> string -> unit) -> unit

val fold_partial_solvers: (partial_solver_factory -> string -> string -> string -> 'a -> 'a) -> 'a -> 'a