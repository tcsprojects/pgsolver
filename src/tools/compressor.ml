open Arg ;;
open Tcsargs;;
open Basics ;;
open Paritygame;;
open Transformations;;



let sccs_perform_inplace game performer =
	let (sccs, sccindex, topology, roots) = strongly_connected_components game in
	let visited = Array.make (Array.length sccs) false in

	let handlescc r =
		let subgame = subgame_by_list game sccs.(r) in
		performer subgame;
		let i = ref 0 in
		ns_iter (fun j ->
			   pg_set_priority game j (pg_get_priority subgame !i);
			   pg_set_owner game j (pg_get_owner subgame !i);
			   incr i
		) sccs.(r);
	in

	let rec perform r =
		if not visited.(r) then (
			List.iter perform topology.(r);
			handlescc r;
			visited.(r) <- true
		)
	in

	List.iter perform roots;;



module CommandLine =
struct
  let priority_compression = ref false
  let keep_fake_alternation = ref false

  let priority_propagation = ref false
  let anti_propagation = ref false

  let on_sccs = ref true

  let node_compression = ref false
  let input_file = ref ""

  let speclist =  [(["--priorities"; "-pr"], Unit(fun _ -> priority_compression := true),
                      "\n     priority compression") ;
                   (["--fakealt"; "-fa"], Unit(fun _ -> keep_fake_alternation := true),
                      "\n     keep fake alternation w.r.t. priorities") ;
                   (["--priopropagation"; "-pp"], Unit(fun _ -> priority_propagation := true),
                      "\n     perform priority propagation") ;
                   (["--antipropagation"; "-ap"], Unit(fun _ -> anti_propagation := true),
                      "\n     perform anti propagation") ;
                   (["--wholegame"; "-wg"], Unit(fun _ -> on_sccs := false),
                      "\n     perform compression on the whole game instead of on the sccs") ;
                   (["--nodes"; "-no"], Unit(fun _ -> node_compression := true),
                      "\n     node compression") ]

  let header = Info.get_title "Compressor Tool"
end ;;

open CommandLine ;;


let _ =
  SimpleArgs.parsedef speclist (fun f -> input_file := f) (header ^ "Usage: compressor [options] [infile]\n" ^
                                              "Compresses the parity game given in <infile>. If this argument is omitted it reads a game from STDIN.\n\nOptions are");

  let in_channel = if !input_file = "" then stdin else open_in !input_file in

  let game = ref (Parsers.parse_parity_game in_channel) in

  if !anti_propagation
  then (if !on_sccs then sccs_perform_inplace !game anti_propagation_inplace else anti_propagation_inplace !game);

  if !priority_propagation
  then (if !on_sccs then sccs_perform_inplace !game priority_propagation_inplace else priority_propagation_inplace !game);

  if !priority_compression
  then (if !on_sccs then sccs_perform_inplace !game (fun g -> let _ = compact_prio_inplace g (not !keep_fake_alternation) in ()) else let _ = compact_prio_inplace !game (not !keep_fake_alternation) in ());

  if !node_compression
  then (
  	let (game', _, _) = compress_nodes !game in
  	game := game'
  );

  Paritygame.print_game !game
