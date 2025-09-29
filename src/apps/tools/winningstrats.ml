open Arg;;
open Tcsargs;;
open Paritygame;;
  
module CommandLine =
struct
  let input_file = ref ""

  let player = ref plr_Even

  let speclist =  [(["--player"; "-p"], Int(fun i -> if i = 1 then player := plr_Odd),
                      "\n     winning strategy for player (default: 0)") ]
                   
  let header = Info.get_title "Winning Strategies "
end ;;

open CommandLine ;;


let _ =
  SimpleArgs.parsedef speclist (fun f -> input_file := f) (header ^ "Usage: winningstrats [infile]\n" ^
                                              "Computes all winning strategies for a player. If this argument is omitted it reads a game from STDIN.");

  let in_channel = if !input_file = "" then stdin else open_in !input_file in

  let game = Parsers.parse_parity_game in_channel in
	
	let solve = let (solve, _, _) = Solvers.find_solver "recursive" in solve [||] in
	
	let (sol, strat) = solve game in
	
  print_string ("\nWinning Set:\n " ^ Paritygame.format_solution sol ^ "\n");
	
	print_string ("\nWinning Strategies for Player " ^ string_of_int (if !player = plr_Even then 0 else 1) ^ "\n");

	let number = ref 0 in
	
  let rec work i =
		if (i >= pg_size game) then (
			let game2 = Paritygame.subgame_by_strat game strat in
			let (sol2, _) = solve game2 in
			let good = ref true in
			for j = 0 to pg_size game - 1 do
				if (sol.(j) != sol2.(j)) then
					good := false;
			done;
			if (!good) then (
				let strat2 = Array.copy strat in
				for j = 0 to Array.length strat2 - 1 do
					if (sol.(j) != !player) then
						strat2.(j) <- -1;
				done;
			  print_string ("\n" ^ format_strategy strat2 ^ "\n");
				number := !number + 1
			);
		) else (
			if (sol.(i) == !player && strat.(i) != -1) then (
			  ns_iter (fun w -> strat.(i) <- w;
					    work (i + 1))
				  (pg_get_successors game i)
			) else (
				work (i+1);
			)
		)
	in
	
	work 0;
	
	print_string ("\nTotal: " ^ string_of_int !number ^ "\n\n");;
