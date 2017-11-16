open Arg;;
open Tcsargs;;
open Paritygame;;
open Pgnodeset;;
open Pgplayer;;
open Pgsolution;;
open Pgnode;;
open Arrayparitygame;;

  
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
	
  print_string ("\nWinning Set:\n " ^ sol#format ^ "\n");
	
	print_string ("\nWinning Strategies for Player " ^ string_of_int (if !player = plr_Even then 0 else 1) ^ "\n");

	let number = ref 0 in
	
  let rec work i =
		if (i >= game#size ) then (
			let game2 = game#subgame_by_strat (new array_pg game#size) strat in
			let (sol2, _) = solve game2 in
			let good = ref true in
			for j = 0 to game#size  - 1 do
				if (sol#get j != sol2#get j) then
					good := false;
			done;
			if (!good) then (
				let strat2 = strat#copy in
				for j = 0 to game#size - 1 do
					if (sol#get j != !player) then
						strat2#set j nd_undef;
				done;
			  print_string ("\n" ^ strat2#format ^ "\n");
				number := !number + 1
			);
		) else (
			if (sol#get i == !player && strat#get i != nd_undef) then (
			  ns_iter (fun w -> strat#set i w;
					    work (i + 1))
				  (game#get_successors  i)
			) else (
				work (i+1);
			)
		)
	in
	
	work 0;
	
	print_string ("\nTotal: " ^ string_of_int !number ^ "\n\n");;
