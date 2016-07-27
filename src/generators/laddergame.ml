open Paritygame;;

let generator_game_func arguments = 

	let show_help _ =
		print_string (Info.get_title "Ladder Game Generator");
		print_string ("Usage: laddergame n\n\n" ^
					  "       where n = n-th ladder game\n\n")

	in
	
  if (Array.length arguments != 1) then (show_help (); exit 1);

  let n = int_of_string arguments.(0) in
  let m = 2*n in  
  let game = pg_create m in
  for i=0 to m-1 do
    pg_set_priority game i (i mod 2);
    pg_set_owner game i (plr_benefits i);
    pg_set_desc game i (Some (string_of_int i));
    pg_add_edge game i ((i+1) mod m);
    pg_add_edge game i ((i+2) mod m)
  done;
  game;;
  
Generators.register_generator generator_game_func "laddergame" "Ladder Game";;
