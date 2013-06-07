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

  Array.init size (fun i ->
    (i,
	 i mod 2,
	 Array.of_list (successors (size-1) i with_self_cycles),
	 Some (string_of_int i))
  );;

Generators.register_generator generator_game_func "cliquegame" "Clique Game";;