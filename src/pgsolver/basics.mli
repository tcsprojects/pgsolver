val message_ch : out_channel ref
val verbosity : int ref

type verbosity_level = int

val init_message_timing : unit -> unit
				    
val verbosity_level_verbose: verbosity_level

val verbosity_level_default: verbosity_level

(* Calling message v (fun _ -> s) outputs the string s on STDOUT if the verbosity level is greater than or equal to v. *)
val message : verbosity_level -> (unit -> string) -> unit

val message_depth : verbosity_level -> int -> (unit -> string) -> unit

val message_depth_tagged : verbosity_level -> int -> (unit -> string) -> (unit -> string) -> unit

val message_incrdepth: unit -> unit
val message_decrdepth: unit -> unit

val message_autod : verbosity_level -> (unit -> string) -> unit

val message_autotagged : verbosity_level -> (unit -> string) -> (unit -> string) -> unit

(* val message_autotagged : verbosity_level -> (unit -> string) -> (unit -> string) -> unit *)

val message_autotagged_newline : verbosity_level -> (unit -> string) -> (unit -> string) -> unit
