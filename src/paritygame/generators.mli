open Paritygame;;

val register_generator: (string array -> paritygame) -> string -> string -> unit

val mem_generator: string -> bool

val find_generator: string -> (string array -> paritygame) * string

val enum_generators: ((string array -> paritygame) -> string -> string -> unit) -> unit

val fold_generators: ((string array -> paritygame) -> string -> string -> 'a -> 'a) -> 'a -> 'a

val run_command_line_generator: string -> unit