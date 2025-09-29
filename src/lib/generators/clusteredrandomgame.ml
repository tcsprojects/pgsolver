open Tcsmaths.RandomUtils;;
open Tcsset;;
open Paritygame;;
  
let generator_game_func arguments = 

	let show_help _ =
		print_string (Info.get_title "Clustered Random Game Generator");
		print_string ("Usage: clusteredrandomgame n p l h r a b x y\n\n" ^
					  "       where n = Number of nodes\n" ^
					  "             p = Highest possibly occurring priority\n" ^
					  "             l = Lowest possible out-degree in the lowest layer\n" ^
					  "             h = Highest possible out-degree in the lowest layer\n" ^
					  "             r = Recursion depth\n" ^
					  "             a = Lowest possible recursion breadth\n" ^
					  "             b = Highest possible recursion breadth\n" ^
					  "             x = Lowest possible inter-connection edges\n" ^
					  "             y = Highest possible inter-connection edges\n\n")
	in

  if (Array.length arguments != 9) then (show_help (); exit 1);

  let size      = int_of_string arguments.(0) in
  let max_prio  = 1+(int_of_string arguments.(1)) in
  let outdegmin = int_of_string arguments.(2) in

  if outdegmin < 1 then (show_help (); exit 1);

  let outdegmax = int_of_string arguments.(3) in
  let recdepth  = int_of_string arguments.(4) in
  let recintmin = int_of_string arguments.(5) in
  let recintmax = int_of_string arguments.(6) in
  let recconmin = int_of_string arguments.(7) in
  let recconmax = int_of_string arguments.(8) in

  Random.self_init ();

  let pg = Array.make size [] in

  let rec generate recd rangemin rangemax =
    let d = rangemax - rangemin + 1 in
  	if (recd = 0) || (d < recintmin) then
  		for i = rangemin to rangemax do
  			let outdeg = min d (randrange outdegmin outdegmax) in
  			let succs = Array.to_list (get_pairwise_different_from_range outdeg rangemin rangemax) in
  			pg.(i) <- succs
  		done
  	else (
  		let parts = min d (randrange recintmin recintmax) in
  		let randarray = get_pairwise_different_from_range (parts - 1) (rangemin+1) rangemax in
  		let partslist = (rangemax + 1) :: (Array.to_list randarray) in
  		let _ = List.fold_left (fun head next_head ->
  			generate (recd - 1) head (next_head - 1);
  			next_head
  		) rangemin (List.sort compare partslist) in
  		let inter_edges = randrange recconmin recconmax in
  		for i = 0 to inter_edges - 1 do
  			let from_node = randrange rangemin rangemax in
  			let to_node = randrange rangemin rangemax in
  			pg.(from_node) <- to_node::pg.(from_node)
  		done
  	)
  in

  generate recdepth 0 (size - 1);
  
  pg_init size (fun v -> (Random.int max_prio,
			  plr_random (),
			  TreeSet.remove_list_dups pg.(v),
			  Some (nd_show v))
  );;

let register _ = Generatorregistry.register_generator generator_game_func "clusteredrg" "Clustered Random Game";;
