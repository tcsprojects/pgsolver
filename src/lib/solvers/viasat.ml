open Paritygame ;;
open Basics ;;
open Univsolve ;;
open Tcstiming;;
open Satwrapper;;
open Tcsarray;;


module Varencoding =
struct
  let solver = ref (None)

  let getsolver _ = match !solver with Some s -> s | _ -> failwith "impossible"

  let number_of_variables = ref 0

  let fresh = ref false (* important!!!! *)

  type vars = S of int
            | TE of (int * int)
            | TA of (int * int)
            | GR of (int * int * int * int)
            | GE of (int * int * int * int)
            | X of (int * int * int)
            | Other

  let show_var s x = (if s=1 then "-" else "") ^
                     (match x with S(v) -> "S(" ^ string_of_int v ^")"
		                 | TE(v,w) -> "Te(" ^ string_of_int v ^ "," ^ string_of_int w ^ ")"
		                 | TA(v,w) -> "Ta(" ^ string_of_int v ^ "," ^ string_of_int w ^ ")"
		                 | GR(v,w,i,j) -> "Gr(" ^ string_of_int v ^ "," ^ string_of_int w ^ "," ^
                                                  string_of_int i ^ "," ^ string_of_int j ^ ")"
		                 | GE(v,w,i,j) -> "Ge(" ^ string_of_int v ^ "," ^ string_of_int w ^ "," ^
                                                  string_of_int i ^ "," ^ string_of_int j ^ ")"
			         | X(v,i,j) -> "X(" ^ string_of_int v ^ "," ^ string_of_int i ^ "," ^
                                                      string_of_int j ^ ")"
	  		         | _ -> failwith "Can only show S,Te,Ta,Gr,Ge, and X variables!")


  exception Invalid_code of int

  module Single =
  struct
    type t = int

    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end;;

  module Pair =
  struct
    type t = int * int

    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end;;

  module Triple =
  struct
    type t = int * int * int

    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end;;

  module Quadruple =
  struct
    type t = int * int * int * int

    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end;;

  module SingleTable    = Hashtbl.Make(Single)    ;;
  module PairTable      = Hashtbl.Make(Pair)      ;;
  module TripleTable    = Hashtbl.Make(Triple)    ;;
  module QuadrupleTable = Hashtbl.Make(Quadruple) ;;

  let stable = SingleTable.create 100
  let tetable = PairTable.create 200
  let tatable = PairTable.create 200
  let grtable = QuadrupleTable.create 400
  let getable = QuadrupleTable.create 400
  let xtable = TripleTable.create 200

  let getGenericVar table finder adder key =
     try
       finder table key
     with
       Not_found -> (let n = (getsolver ())#add_variable in
		     incr number_of_variables;
                     adder table key n;
                     n)

  let getSVar    = getGenericVar stable SingleTable.find SingleTable.add
  let getNSVar k  = -(getSVar k)
  let getTEVar    = getGenericVar tetable PairTable.find PairTable.add
  let getNTEVar k = -(getTEVar k)
  let getTAVar    = getGenericVar tatable PairTable.find PairTable.add
  let getNTAVar k = -(getTAVar k)
  let getXVar     = getGenericVar xtable TripleTable.find TripleTable.add
  let getNXVar k  = -(getXVar k)
  let getGEVar    = getGenericVar getable QuadrupleTable.find QuadrupleTable.add
  let getNGEVar k = -(getGEVar k)
  let getGRVar    = getGenericVar grtable QuadrupleTable.find QuadrupleTable.add
  let getNGRVar k = -(getGRVar k)

  let init_encoding _ =
    (if not !fresh then (
      solver := Some ((Satsolvers.get_default ())#new_instance);
      (*solver_SetNumVariables !solver 0;*)
	number_of_variables := 0;
      SingleTable.clear stable;
      PairTable.clear tetable;
      PairTable.clear tatable;
      QuadrupleTable.clear grtable;
      QuadrupleTable.clear getable;
      TripleTable.clear xtable)
   else fresh := false);
   getsolver ()

  let release_encoding _ =
	(getsolver ())#dispose
(*    solver_ReleaseManager !solver; *)

end;;

open Varencoding ;;


module OrderedList =
struct

  let rec insert x = function [] -> [x]
                            | (y::l) -> if x < y
                                        then x::y::l
                                        else if x=y
                                             then y::l
                                             else y::(insert x l)

  let rec smaller x = function [] -> []
                             | (y::l) -> if y >= x
                                         then []
                                         else y::(smaller x l)

  let rec smaller_equals x = function [] -> []
                                    | (y::l) -> if y > x
                                                then []
                                                else y::(smaller_equals x l)

  let rec greater_equals x = function [] -> []
                                    | (y::l) -> if y < x
                                                then greater_equals x l
                                                else y::l

  let rec greater x = function [] -> []
                             | (y::l) -> if y <= x
                                         then greater x l
                                         else y::l

end ;;

open OrderedList ;;


let half_lower n = int_of_float (floor ((float_of_int n) /. 2.))
let half_upper n = int_of_float (ceil ((float_of_int n) /. 2.))



let solve' game =

  let msg_tagged v = message_autotagged v (fun _ -> "VIASAT") in
  let msg_plain = message in


  let solver = init_encoding () in

  let number_of_clauses = ref 0 in

  let schedule_clause c = message 3 (fun _ -> IntArrayUtils.format c ^ "\n");
                          incr number_of_clauses;
                          solver#add_clause c
  in

  let prio_table = SingleTable.create 100 in
  let present_prios  = ref [] in
	
  let m = pg_size game in
	
  pg_iterate (fun i -> fun (p,_,_,_,_) -> let n = try
					       SingleTable.find prio_table p
					     with _ -> 0
					   in
					   SingleTable.remove prio_table p;
					   SingleTable.add prio_table p (n+1);
					   
					   present_prios := insert p !present_prios
	     ) game;
  

  let binlog n = int_of_float (ceil ((log (float_of_int n)) /. (log 2.))) in
  let odd p = p mod 2 = 1 in
  let even p = p mod 2 = 0 in
  
			  
  let hbit prio =
    try
      let n = SingleTable.find prio_table prio in
      max 0 ((binlog (n+1))-1)
    with _ -> (-1)
  in

  message 3 (fun _ -> "    Priorities present in current subgame are {" ^
			String.concat "," (List.map string_of_int !present_prios) ^ "}\n");
  message 3 (fun _ -> "    Number of nodes with certain priority in current subgame: " ^
			String.concat ", "
				      (SingleTable.fold (fun prio -> fun n -> fun l ->
									      ("<" ^ string_of_int prio ^ ":" ^ string_of_int n ^ ">") :: l)
							prio_table [])
			^ "\n");
  
  msg_tagged 2 (fun _ -> "Using backend sat solver: " ^ (Satsolvers.get_default ())#identifier ^ "\n");
  msg_tagged 2 (fun _ -> "Number of nodes in the game    : " ^ string_of_int m ^ "\n");
	
  message 3 (fun _ -> "Creating and scheduling clauses for addition ...\n");
  pg_iterate (fun v -> fun (_,pl,ws,_,_) -> 
		       (if pl=plr_Even then 
			  begin
			    message 3 (fun _ -> "  `" ^ show_var 0 (S v) ^ " -> " ^
						  String.concat " + " (List.map (fun w -> show_var 0 (TE (v,w))) (ns_nodes ws)) ^ "': ");
			    schedule_clause (Array.append [|getNSVar v|] (Array.map (fun w -> getTEVar (v,w)) (Array.of_list (ns_nodes ws))));
			    ns_iter (fun w -> message 3 (fun _ -> "  `" ^ show_var 1 (S v) ^ " -> " ^ show_var 0 (TA (v,w)) ^ "': ");
					      schedule_clause [| getSVar v; getTAVar (v,w) |]) ws
			  end
			else
			  begin
			    message 3 (fun _ -> "  `" ^ show_var 1 (S v) ^ " -> " ^
						  String.concat " + " (List.map (fun w -> show_var 0 (TA (v,w))) (ns_nodes ws)) ^ "': ");
			    schedule_clause (Array.append [|getSVar v|] (Array.map (fun w -> getTAVar (v,w)) (Array.of_list (ns_nodes (ws)))));
			    ns_iter (fun w -> message 3 (fun _ -> "  `" ^ show_var 0 (S v) ^ " -> " ^ show_var 0 (TE (v,w)) ^ "': ");
					      schedule_clause [| getNSVar v; getTEVar (v,w) |]) ws
			  end);
		       ns_iter (fun w -> message 3 (fun _ -> "  `" ^ show_var 0 (TE (v,w)) ^ " -> " ^ show_var 0 (S w) ^ "': ");
					 schedule_clause [| getNTEVar (v,w) ; getSVar w |];
					 message 3 (fun _ -> "  `" ^ show_var 0 (TA (v,w)) ^ " -> " ^ show_var 1 (S w) ^ "': ");
					 schedule_clause [| getNTAVar (v,w); getNSVar w |]) ws
	     ) game;
	
  let to_expand = ref [] in
  let remember var = to_expand := var::!to_expand in
	
  pg_iterate (fun v -> fun (p,_,ws,_,_) ->   
	  ns_iter (fun w ->
		   let bigger_prios = List.filter (fun r -> r > p) !present_prios in
		   List.iter (fun p' -> let k = (v,w,p',hbit p') in
					message 3 (fun _ -> "  `" ^ show_var 0 (TE (v,w)) ^ " -> " ^ show_var 0 (GE k) ^ "': ");
					schedule_clause [| getNTEVar (v,w) ; getGEVar k |];
					remember (GE k)) (List.filter odd bigger_prios);
		   List.iter (fun p' -> let k = (v,w,p',hbit p') in
					message 3 (fun _ -> "  `" ^ show_var 0 (TA (v,w)) ^ " -> " ^ show_var 0 (GE k) ^ "': ");
					schedule_clause [| getNTAVar (v,w) ; getGEVar k |];
					remember (GE k)) (List.filter even bigger_prios);
		   let k = (v,w,p,hbit p) in
		   (if odd p
		    then (message 3 (fun _ -> "  `" ^ show_var 0 (TE (v,w)) ^ " -> " ^ show_var 0 (GR k) ^ "': ");
			  schedule_clause [| getNTEVar (v,w) ; getGRVar k |])
		    else (message 3 (fun _ -> "  `" ^ show_var 0 (TA (v,w)) ^ " -> " ^ show_var 0 (GR k) ^ "': ");
			  schedule_clause [| getNTAVar (v,w) ; getGRVar k |]));
		   remember (GR k)) ws
	     ) game;
	
  while !to_expand <> [] do
    (match List.hd !to_expand with
       GE (v,w,i,m) -> message 3 (fun _ -> "  `" ^ show_var 0 (GE (v,w,i,0)) ^ " -> "
					   ^ show_var 1 (X (w,i,0)) ^ " + " ^ show_var 0 (X (v,i,0)) ^ "': ");
		       schedule_clause [| getNGEVar (v,w,i,0) ; getNXVar (w,i,0) ; getXVar (v,i,0) |];
		       for j=1 to m do
                         let g = getNGEVar (v,w,i,j) in
                         let y = getNXVar (w,i,j) in
                         let x = getXVar (v,i,j) in
                         let g' = getGEVar (v,w,i,j-1) in
                         message 3 (fun _ -> "  `" ^ show_var 0 (GE (v,w,i,j)) ^ " * "
					     ^ show_var 0 (X (w,i,j)) ^ " -> " ^ show_var 0 (X (v,i,j)) ^ "': ");
                         schedule_clause [| g ; y ; x |];
                         message 3 (fun _ -> "  `" ^ show_var 0 (GE (v,w,i,j)) ^ " * "
					     ^ show_var 0 (X (w,i,j)) ^ " -> " ^ show_var 0 (GE (v,w,i,j-1)) ^ "': ");
                         schedule_clause [| g ; y ; g' |];
                         message 3 (fun _ -> "  `" ^ show_var 0 (GE (v,w,i,j)) ^ " * "
					     ^ show_var 1 (X (v,i,j)) ^ " -> " ^ show_var 0 (GE (v,w,i,j-1)) ^ "': ");
                         schedule_clause [| g ; x ; g' |]
		       done
     | GR (v,w,i,m) -> let g = getNGRVar (v,w,i,0) in
		       message 3 (fun _ -> "  `" ^ show_var 0 (GR (v,w,i,0)) ^ " -> " ^ show_var 1 (X (w,i,0)) ^ "': ");
		       schedule_clause [| g ; getNXVar (w,i,0) |];
		       message 3 (fun _ -> "  `" ^ show_var 0 (GR (v,w,i,0)) ^ " -> " ^ show_var 0 (X (v,i,0)) ^ "': ");
		       schedule_clause [| g ; getXVar (v,i,0) |];
		       for j=1 to m do
                         let g = getNGRVar (v,w,i,j) in
                         let y = getNXVar (w,i,j) in
                         let x = getXVar (v,i,j) in
                         let g' = getGRVar (v,w,i,j-1) in
                         message 3 (fun _ -> "  `" ^ show_var 0 (GR (v,w,i,j)) ^ " * "
					     ^ show_var 0 (X (w,i,j)) ^ " -> " ^ show_var 0 (X (v,i,j)) ^ "': ");
                         schedule_clause [| g ; y ; x |];
                         message 3 (fun _ -> "  `" ^ show_var 0 (GR (v,w,i,j)) ^ " * "
					     ^ show_var 0 (X (w,i,j)) ^ " -> " ^ show_var 0 (GR (v,w,i,j-1)) ^ "': ");
                         schedule_clause [| g ; y ; g' |];
                         message 3 (fun _ -> "  `" ^ show_var 0 (GR (v,w,i,j)) ^ " * "
					     ^ show_var 1 (X (v,i,j)) ^ " -> " ^ show_var 0 (GR (v,w,i,j-1)) ^ "': ");
                         schedule_clause [| g ; x ; g' |]
		       done
     | _ -> failwith "How did that get here ???"
		     
    );
    to_expand := List.tl !to_expand
  done;
  
  msg_tagged 2 (fun _ -> "Number of clauses: " ^ string_of_int !number_of_clauses ^ " \n");
  msg_tagged 2 (fun _ -> "Number of variables: " ^ string_of_int !number_of_variables ^ " \n");
  let tim = SimpleTiming.init false in
  msg_tagged 2 (fun _ -> SimpleTiming.start tim; "SAT Solving ... ");
  (match solver#solve with
     SolveSatisfiable -> ()
   | x -> failwith ("satsolver reports " ^ format_solve_result x ^ " instead of SAT"));
  msg_plain 2 (fun _ -> SimpleTiming.stop tim; "done: " ^ SimpleTiming.format tim ^ "\n");
  
  let solution = Array.make m (plr_Even) in
  let strategy = Array.make m (-1) in
	
  message 3 (fun _ -> "Obtaining values of S-variables ...\n");
	
  SingleTable.iter
    (fun v -> fun i ->
	      message 3 (fun _ -> "  " ^ show_var 0 (S v) ^ ": ");
	      let x = if solver#get_assignment i then 1 else 0 in
	      let winner = 1-x in
	      message 3 (fun _ -> string_of_int x ^ ", successor: ");
	      
	      let succs = pg_get_successors game v in
	      let get_fun = if winner = 0 then getTEVar else getTAVar in
	      let y = try
		  ns_find (fun w -> let j = get_fun (v,w) in solver#get_assignment j) succs
                with _ -> failwith "[Viasat] Fatal error: node belonging to strategy has no successor within that strategy!!!"
	      in
	      message 3 (fun _ -> (string_of_int y) ^ "\n");
	      solution.(v) <- if winner=0 then plr_Even else plr_Odd;
	      strategy.(v) <- y) stable;
  
  release_encoding ();
  (solution,strategy)
	  
	  
let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;
  

let register _ = Solverregistry.register_solver solve "viasat" "vs" "use the small progress measure enc. for prop. logic and a reduction to SAT";;

