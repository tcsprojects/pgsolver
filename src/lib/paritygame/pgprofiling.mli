type prof_function
		       
val prof_priority     : prof_function
val prof_owner        : prof_function  
val prof_successors   : prof_function
val prof_predecessors : prof_function
val prof_definedcheck : prof_function
			  
val prof_declare_originator   : string -> unit
val prof_undeclare_originator : unit -> unit
		     
val prof_access : int -> prof_function -> unit

val prof_print_results : unit -> unit
