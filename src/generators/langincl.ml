open Paritygame;;

let n = ref 0
let m = ref 2

let rec range x y = if x > y then [] else x :: (range (x+1) y)
let even x = (x mod 2) = 0

module LangInclGame = Build( 
  struct
    type gamenode = int * int
    let compare = compare

    let owner _ = plr_Odd

    let initnodes _ = [ (0,0) ]
		     
    let priority (u,v) = if even v then 2 else if u > 0 then 1 else 0

    let dettrans v a = 
      let row = v mod !n in
(*      let col = v / !n in *)
      if a-1 = row then
        (v + !n) mod (!m * !n)
      else
        !n+a-1

    let successors (u,v) = 
      let symbols = range 1 !n in
      if u=0 then 
        (List.map (fun a -> (0,dettrans v a)) symbols) @
        (List.map (fun a -> (a,dettrans v a)) symbols)
      else if u > (!m-2) * !n then
        [(0, dettrans v (u - (!m-2) * !n))]
      else [(u + !n, dettrans v (u mod !n))]

    let show_node (u,v) = Some ("(" ^ string_of_int u ^ "," ^ string_of_int v ^ ")")
  end);;

let generator_game_func arguments = 

	let show_help _ =
		print_string (Info.get_title "Language Inclusion Problem Game Generator");
		print_string ("Usage: langincl n [m]\n\n" ^
			      "       where n = number of alphabet symbols for the language of infinitely many repetitions'\n" ^
                              "             m = length of parts with repeated symbols (>=2), default is 2\n\n")

	in
	
  if (Array.length arguments < 1) || (Array.length arguments > 2) then (show_help (); exit 1);

  n := int_of_string arguments.(0);
  (try 
    m := int_of_string arguments.(1)  
  with Invalid_argument _ -> ());

  LangInclGame.build () 



let register _ = Generatorregistry.register_generator generator_game_func "langincl" "Language Inclusion Problem Game";;
