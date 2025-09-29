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

  pg_init m (fun v -> (v mod 2, plr_benefits v, [ ((v+1) mod m); ((v+2) mod m) ], Some (nd_show v)));;
  
let register _ = Generatorregistry.register_generator generator_game_func "laddergame" "Ladder Game";;
