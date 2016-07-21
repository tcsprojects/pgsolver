open Paritygame;;

let generator_game_func arguments = 

	let show_help _ =
		print_string (Info.get_title "Clique Game Generator");
		print_string ("Usage: cliquegame n <cycles>\n\n" ^
					  "       where n = Number of nodes\n" ^
						  "             <cycles> is either the string > self < or absent\n\n")
	in
						  
	let successors n i s =
	  let rec succs acc j =
		if j=i
		then (if s
			  then succs (j::acc) (j-1)
			  else succs acc (j-1))
		else (if j<0
			  then acc
			  else succs (j::acc) (j-1))
	  in
	  succs [] n
	in
	
  let (size,with_self_cycles) =
    try
      (int_of_string arguments.(0), Array.length arguments = 2 && arguments.(1) = "self")
    with _ -> (show_help (); exit 1)
  in

  let game = pg_create size in
  for i=0 to size-1 do
    pg_set_priority game i i;
    pg_set_owner game i (i mod 2);
    pg_set_desc game i (Some (string_of_int i));
    List.iter (fun w -> pg_add_edge game i w) (successors (size-1) i with_self_cycles)) 
  done;
  game;;

Generators.register_generator generator_game_func "cliquegame" "Clique Game";;
