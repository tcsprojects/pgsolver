open Paritygame;;

let height = ref 0
let width = ref 0

let rec range x y = if x > y then [] else x :: (range (x+1) y)

module JGame = Build(
  struct
    type gamenode = Left of int * int
		  | Right of int * int
		  | Above of int * int
			       
    let compare = compare

    let owner = function Left(0,_)  -> plr_Even
		       | Right(0,_) -> plr_Odd
		       | Left(_,_)  -> plr_Odd
		       | Right(_,_) -> plr_Even
		       | Above(_,_) -> plr_Even
		       
    let priority = function Left(0,_)  -> 0
			  | Right(0,_) -> 1
			  | Left(h,_)  -> 2*h
			  | Right(h,_) -> 2*h
			  | Above(h,_) -> 2*h+1

    let successors = function Left(0,0)  -> [ Right(0,0) ]
			    | Left(0,w)  -> if w < !width then
					      [ Right(0,w-1); Right(0,w) ]
					    else
					      [ Right(0,w-1) ]
			    | Right(0,w) -> (Left(0,w))::(Left(0,w+1))::(List.map (fun v -> Right(v,w)) (range 1 (!height-1)))
			    | Left(h,0)  -> [ Above(h,0); Right(h,0) ]
			    | Left(h,w)  -> if w < !width then
					      [ Right(h,w-1); Above(h,w); Right(h,w) ]
					    else
					      [ Right(h,w-1) ]
			    | Above(h,w) -> [ Right(h,w) ]
			    | Right(h,w) -> [ Left(h,w); Left(h,w+1); Right(0,w) ]

    let show_node = function Left(h,w)  -> Some("L(" ^ string_of_int h ^ "," ^ string_of_int w ^ ")")
			   | Right(h,w) -> Some("R(" ^ string_of_int h ^ "," ^ string_of_int w ^ ")")
			   | Above(h,w) -> Some("A(" ^ string_of_int h ^ "," ^ string_of_int w ^ ")")

    let initnodes _ = [ Left(0,0) ]

  end);;
		    
let generator_game_func arguments = 

  let show_help _ =
    print_string (Info.get_title "Jurdzinski Game Generator");
    print_string ("Usage: jurdzinskigame h w\n\n" ^
		    "       generates the Jurdzinski game of height h and width w for h,w >= 0\n" ^
		      "       as a max-parity game\n\n")
  in
  
  (try
      height := int_of_string arguments.(0);
      width := int_of_string arguments.(1)					 
  with _ -> (show_help (); exit 1));

  if not (!height >= 0 && !width >= 0) then (show_help(); exit 1);

  JGame.build ()


let register _ = Generatorregistry.register_generator generator_game_func "jurdzinskigame" "Jurdzinski Game";;
