(* size: 4 * n
   index: 4 * n
   counter: 6 * 2^n - 5
*)

open Paritygame;;

let n = ref 0

module ModelcheckerLadderGame = Build( 
  struct
    type gamenode = Player0 of int
		  | Player1 of int
		  | Player1x of int

    let compare = compare
		
    let owner _ = plr_Odd
		
    let priority = function
      |	Player0 i -> 2 * !n - 2 * i
      |	Player1 i -> 0
      | Player1x i -> 2 * !n - 2 * i - 1
					 
    let show_node =
      let fmt x i = Some (x ^ string_of_int (!n - i)) in
      function 
      | Player0 i -> fmt "a" i
      | Player1 i -> fmt "b" i
      | Player1x i -> fmt "c" i
			  
    let successors = function
      | Player0 i -> [if i = !n then Player0 0 else Player1 i]
      | Player1 i -> [Player1x i; Player0 (i + 1)]
      | Player1x i -> [Player0 (i + 1)]

    let initnodes _ = [ Player0 0 ]
end);;


let generator_game_func arguments =

  let show_help _ =
    print_string (Info.get_title "Modelchecker Ladder Game Generator");
    print_string ("Usage: modelcheckerladder n\n\n" ^
		    "       where n = n-th model checker ladder game\n\n")
  in
  
  if (Array.length arguments != 1) then (show_help (); exit 1);

  n := int_of_string arguments.(0);

  ModelcheckerLadderGame.build () ;;


let register _ = Generatorregistry.register_generator generator_game_func "modelcheckerladder" "Model Checker Ladder";;
