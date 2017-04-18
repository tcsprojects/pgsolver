let pgsolver_collection_version = "4.0"
let authors = "Oliver Friedmann (University of Munich) and Martin Lange (University of Kassel)"
let timeperiod = "2008-2017"
let url = "http://tcsprojects.org"

let get_title s =
	"PGSolver Collection Ver. " ^ pgsolver_collection_version ^ ": " ^
	s ^ "\n" ^
	"Authors: " ^ authors ^ ", " ^ timeperiod ^ "\n" ^
	url ^ "\n\n"
