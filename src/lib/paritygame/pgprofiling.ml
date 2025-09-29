open Paritygame;;
  
type prof_function = int
		       
let prof_priority = 0
let prof_owner = 1
let prof_successors = 2
let prof_predecessors = 3
let prof_definedcheck = 4
let prof_untouched = [|0;0;0;0;0|]

let prof_current_functions = ref [] 

let prof_start_time = ref 0.0
		    
module ProfTimingMap = Map.Make(struct
			    type t = string
			    let compare = compare
			  end);;
  
let prof_timing_map = ref ProfTimingMap.empty
			  
let prof_declare_originator s =
  prof_current_functions := s :: !prof_current_functions;
  prof_start_time := Sys.time ()

let prof_undeclare_originator _ =
  let e = Sys.time () -. !prof_start_time in
  match !prof_current_functions with
    f::fs -> prof_current_functions := fs;
	     let t = try
		       ProfTimingMap.find f !prof_timing_map
	              with Not_found -> 0.0
	     in
	     prof_timing_map := ProfTimingMap.add f (t +. e) !prof_timing_map
  | [] -> failwith "Profiling.prof_undeclare_originator: no more profiled functions on the call stack!"

module ProfNodes = Map.Make(struct
			     type t = string * node
			     let compare = compare
			   end);;
let prof_nodes = ref ProfNodes.empty
		     		     
let prof_access v i =
  match !prof_current_functions with
    f::_ -> let pv = try
		       ProfNodes.find (f,v) !prof_nodes
	             with Not_found -> prof_untouched 
	    in
	    pv.(i) <- pv.(i) + 1;
	    prof_nodes := ProfNodes.add (f,v) pv !prof_nodes
  | [] -> ()

let prof_print_results _ =
  print_string ("Profiling information:\n");
  ProfTimingMap.iter (fun s -> fun t -> print_string ("Function \"" ^ s ^ "\":\n");
					print_string ("  overall runtime         : " ^ string_of_float (t *. 1000.0) ^ "msec\n");
					let (np,no,ns,nd,nc) = ProfNodes.fold (fun (f,_) -> fun acs -> fun (b0,b1,b2,b3,b4) ->
												       if f=s then
													 (b0+acs.(0), b1+acs.(1), b2+acs.(2), b3+acs.(3), b4+acs.(4))
												       else
													 (b0,b1,b2,b3,b4))
									      !prof_nodes
									      (0,0,0,0,0)
					in
					print_string ("  accumulated accesses to\n"); 
					print_string ("        ..priority fields : " ^ string_of_int np ^ "\n"); 
					print_string ("           ..owner fields : " ^ string_of_int no ^ "\n"); 
					print_string ("       ..successor fields : " ^ string_of_int ns ^ "\n"); 
					print_string ("     ..predecessor fields : " ^ string_of_int nd ^ "\n");
					print_string ("  checks for (un)defined  : " ^ string_of_int nc ^ "\n")
		     )
		     !prof_timing_map
  
