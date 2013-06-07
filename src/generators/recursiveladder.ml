let generator_game_func arguments =

	let show_help _ =
		print_string (Info.get_title "Recursive Ladder Game Generator");
		print_string ("Usage: recursiveladder n\n\n" ^
					  "       where n = n-th recursive ladder game\n\n")
	in

  if (Array.length arguments != 1) then (show_help (); exit 1);

  let height = 1 + int_of_string arguments.(0) in

  let w = 5 in

  let pg = Paritygame.pg_create (5 * height - 5) in

  for i = 0 to height-1 do
  	let start_idx = i * w - 2 in
  	let p = (i + 1) * 3 + 2 in
  	let start_pl = i mod 2 in
  	let o = 1 - start_pl in
  	let mx = i = height - 1 in
  	let mfx = i = height - 2 in
  	let mn = i = 0 in

  	let prnt j pr pl succs =
		pg.(start_idx + j) <- (pr, (start_pl + pl) mod 2, Array.of_list (List.map (fun k -> k + start_idx) succs), Some (string_of_int (start_idx + j)))
	in

	if not mn then (
        prnt 0 o       1 [3-w;1];
        prnt 1 o       0 (if mx then [0] else [0;2]);
    );
    if not mx then (
    	prnt 2 p       1 [w+1;3];
        prnt 3 (p - 1) 0 ((if mn then [] else [3-w])@(if mfx then [] else [w+3])@[4]);
        prnt 4 (p - 2) 1 (if mx then [3] else [3;w+1])
    )
  done;
  pg;;

Generators.register_generator generator_game_func "recursiveladder" "Recursive Ladder Game";;