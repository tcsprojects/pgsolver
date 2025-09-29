open Paritygame;;

let random_game_func arguments =

	let show_help _ =
		print_string (Info.get_title "Random Game Generator");
		print_string ("Usage: randomgame n p l h [x]\n\n" ^
					  "       where n = Number of nodes\n" ^
					  "             p = Highest possibly occurring priority\n" ^
					  "             l = Lowest possible out-degree (l > 0)\n" ^
					  "             h = Highest possible out-degree\n" ^ 
					  "             x = disable self-cycles\n\n")
	in
	
    if (Array.length arguments < 4) || (Array.length arguments > 5) then (show_help (); exit 1);

    let size = int_of_string arguments.(0) in
    let max_prio = 1+(int_of_string arguments.(1)) in
    let outdegmin = int_of_string arguments.(2) in

    if outdegmin < 1 then (show_help (); exit 1);

    let outdegmax = int_of_string arguments.(3) in
    let self_cycles = Array.length arguments = 4 in

    Random.self_init ();

    pg_init size (fun i -> (Random.int max_prio,
			    plr_random (),
			    List.map (fun j -> if j < i || self_cycles then j else j + 1)
				     (Array.to_list (Tcsmaths.RandomUtils.get_pairwise_different_from_range (outdegmin + Random.int (outdegmax - outdegmin + 1))
													    0
													    (size-1 - (if self_cycles then 0 else 1)))),
			    Some (nd_show i)));;

let register _ =
    Generatorregistry.register_generator random_game_func "randomgame" "Random Game";;
