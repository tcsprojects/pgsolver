open Tcsarray;;
open Paritygame;;
  
let generator_game_func arguments = 

	let show_help _ =
		print_string (Info.get_title "Steady Game Generator");
		print_string ("Usage: steadygame n l h x y\n\n" ^
					  "       where n = Number of nodes\n" ^
					  "             l = Lowest possible out-degree\n" ^
					  "             h = Highest possible out-degree\n" ^
					  "             x = Lowest possible in-degree\n" ^
					  "             y = Highest possible in-degree\n\n")
	in

  if (Array.length arguments != 5) then (show_help (); exit 1);

  let size = int_of_string arguments.(0) in
  let outdegmin = int_of_string arguments.(1) in

  if outdegmin < 1 then (show_help (); exit 1);

  let outdegmax = int_of_string arguments.(2) in
  let indegmin = int_of_string arguments.(3) in

  if indegmin < 1 then (show_help (); exit 1);

  let indegmax = int_of_string arguments.(4) in
  let outavail = DynArray.init 0 size (fun i -> i) in
  let inavail = DynArray.init 0 size (fun i -> i) in
  let outtodo = ref size in
  let intodo = ref size in

  Random.self_init ();

  let pg = Array.make size [] in
  let pgtr = Array.make size 0 in

  while ((!outtodo > 0 && DynArray.length inavail > 0) || (!intodo > 0 && DynArray.length outavail > 0)) do
  	let ii = Random.int (DynArray.length outavail) in
  	let i = DynArray.get outavail ii in
  	let findin forb =
  		let jj = ref (Random.int (DynArray.length inavail)) in
        while (List.mem (DynArray.get inavail !jj) forb) && (DynArray.length inavail > 1) do
            jj := Random.int (DynArray.length inavail)
        done;
        !jj
    in
  	let jj = findin (i::pg.(i)) in
    let j = DynArray.get inavail jj in
  	pg.(i) <- j::pg.(i);
  	pgtr.(j) <- pgtr.(j) + 1;
  	let leni = List.length pg.(i) in
  	let lenj = pgtr.(j) in
  	if leni = outdegmin	then decr outtodo
  	else if leni = outdegmax then DynArray.delete outavail ii;
  	if lenj = indegmin	then decr intodo
  	else if lenj = indegmax then DynArray.delete inavail jj;
  done;

  pg_init size (fun i -> (i,
			  plr_random (),
			  pg.(i),
			  Some (nd_show i))
	       );;

let register _ = Generatorregistry.register_generator generator_game_func "steadygame" "Steady Game";;
