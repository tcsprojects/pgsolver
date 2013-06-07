(* size: 4 * n
   index: 4 * n
   counter: 6 * 2^n - 5
*)

open Paritygame;;

type gamenode = Player0 of int
              | Player1 of int
              | Player1x of int

let generator_game_func arguments =

	let show_help _ =
		print_string (Info.get_title "Modelchecker Ladder Game Generator");
		print_string ("Usage: modelcheckerladder n\n\n" ^
					  "       where n = n-th model checker ladder game\n\n")
	in

  if (Array.length arguments != 1) then (show_help (); exit 1);

  let pg = SymbolicParityGame.create_new (Player0 0) in

  let n = int_of_string arguments.(0) in

  let fmt i = string_of_int (n - i) in

  let symb_to_str = function Player0 i -> "a" ^ fmt i | Player1 i -> "b" ^ fmt i | Player1x i -> "c" ^ fmt i in

  let add sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in

  for i = 0 to n - 1 do
  	add (Player0 i) (2 * n - 2 * i) 1 [Player1 i];
  	add (Player1 i) 0 1 [Player1x i; Player0 (i + 1)];
  	add (Player1x i) (2 * n - 2 * i - 1) 1 [Player0 (i + 1)]
  done;
  add (Player0 n) 0 1 [Player0 0];

  SymbolicParityGame.to_paritygame pg;;


Generators.register_generator generator_game_func "modelcheckerladder" "Model Checker Ladder";;