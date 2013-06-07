open Paritygame;;
open Arg;;

type gamenode = V of int | U of int

let symb_to_str = function 
|	V i -> "v" ^ string_of_int i
|	U i -> "u" ^ string_of_int i

let generator_game_func arguments =
	
    if (Array.length arguments != 1) then (failwith "specify index of game");
	let n = int_of_string arguments.(0) in

	let pg = SymbolicParityGame.create_new (V 1) in

	let addnode sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in
   
	for i = 0 to n-1 do
		addnode (U (i + 1)) 1 0 [V (2 * i + 1); U (i + 1)];
		addnode (V (2 * i)) (2 * i + 2) 0 [(if i = 0 then V (2 * i) else V (2 * i - 1))];
		addnode (V (2 * i + 1)) (2 * i + 3) 0 [V (2 * i)];
	done;

	SymbolicParityGame.to_paritygame pg;;
	
Generators.register_generator generator_game_func "recursivedullgame" "Recursive Dull Game";;