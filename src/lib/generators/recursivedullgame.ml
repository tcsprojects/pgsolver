open Paritygame;;
open Arg;;

let n = ref 0

module RecursiveDullGame = Build( 
  struct
    type gamenode = V of int | U of int
    let compare = compare
		
    let owner _ = plr_Even
		
    let priority = function
      |	U _ -> 1
      |	V i -> i + 2
		     
    let show_node = function 
      |	V i -> Some ("v" ^ string_of_int i)
      |	U i -> Some ("u" ^ string_of_int i)
		    
    let successors = function
      | U i -> [V (2 * i - 1); U i]
      | V i -> [if i = 0 then V i else V (i-1)]

    let initnodes _ = (Array.to_list (Array.init !n (fun i -> U (i+1))))
end);;
				   
let generator_game_func arguments =
	
  if (Array.length arguments != 1) then (failwith "specify index of game");
  n := int_of_string arguments.(0);
	
  RecursiveDullGame.build () ;;

  
let register _ = Generatorregistry.register_generator generator_game_func "recursivedullgame" "Recursive Dull Game";;
