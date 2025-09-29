open Paritygame;;

let n = ref 0
let with_self_cycles = ref true
			   
module Clique =
  struct
    type gamenode = int

    let compare = compare

    let owner i = plr_benefits i

    let priority i = i

    let successors i = 
      let rec succs acc j =
	if j=i
	then (if !with_self_cycles
	      then succs (j::acc) (j-1)
	      else succs acc (j-1))
	else (if j<0
	      then acc
	      else succs (j::acc) (j-1))
      in
      succs [] !n

    let show_node i = Some (string_of_int i)

    let initnodes _ = [0]
  end;;

module CliqueGame = Build(Clique);;
  
let generator_game_func arguments = 

  let show_help _ =
    print_string (Info.get_title "Clique Game Generator");
    print_string ("Usage: cliquegame n <cycles>\n\n" ^
		    "       where n = Number of nodes\n" ^
		      "             <cycles> is either the string > self < or absent\n\n")
  in

  (try
    n := int_of_string arguments.(0);
    with_self_cycles := Array.length arguments = 2 && arguments.(1) = "self"
  with _ -> (show_help (); exit 1));

  let successors i = 
    let rec succs acc j =
      if j=i
      then (if !with_self_cycles
	    then succs (j::acc) (j-1)
	    else succs acc (j-1))
      else (if j<0
	    then acc
	    else succs (j::acc) (j-1))
    in
    succs [] (!n-1)
  in
  pg_init !n (fun v -> (v, plr_benefits v, successors v, Some (nd_show v)))


		     
(* CliqueGame.build () *)


let register _ = Generatorregistry.register_generator generator_game_func "cliquegame" "Clique Game";;
