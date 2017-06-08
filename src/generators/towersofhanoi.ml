open Paritygame ;;

let levels = ref 0
		 
module ToHGame = Build( 
  struct
    type gamenode = int list array
			       
    let compare = compare

    let owner _ = plr_Even

    let finished ts = (ts.(0) = []) && (ts.(2) = [])

    let priority ts =
      if finished ts then 0 else 1 

    let successors ts =
      if finished ts then [ts] else
	begin
	  let tops = [| if ts.(0) <> [] then List.hd ts.(0) else 0; 
			if ts.(1) <> [] then List.hd ts.(1) else 0;
			if ts.(2) <> [] then List.hd ts.(2) else 0 |]
	  in
	  let succs = ref [] in
	  let moves = [ (0,1,2); (0,2,1); (1,0,2); (1,2,0); (2,0,1); (2,1,0) ] in
	  
	  List.iter (fun (f,t,o) -> if tops.(f) > 0 && (tops.(f) < tops.(t) || tops.(t) = 0) then
				      begin
					let tt = Array.make 3 [] in
					tt.(f) <- List.tl ts.(f);
					tt.(t) <- tops.(f) :: ts.(t);
					tt.(o) <- ts.(o);
					succs := tt :: !succs
				      end)
		    moves;
	  !succs
	end
	  

    let show_node ts = Some ("[" ^ (String.concat "," (List.map string_of_int ts.(0))) ^ "] " ^
			       "[" ^ (String.concat "," (List.map string_of_int ts.(1))) ^ "] " ^
				 "[" ^ (String.concat "," (List.map string_of_int ts.(2))) ^ "]")

    let initnodes _ = 
      let rec lvs i aux = if i=0 then aux else lvs (i-1) (i::aux)
      in
      [ [| lvs !levels []; []; [] |] ]

  end);;


  
let towers_of_hanoi_func arguments = 

  let show_help _ =
    print_string (Info.get_title "Towers of Hanoi Reachability Game Generator");
    print_string ("Usage: towersofhanoi n \n\n" ^
		    "       where n = number of levels that the tower to be moved has (>= 1)\n\n")
		 
  in
	
  if (Array.length arguments <> 1) then (show_help (); exit 1);

  (try
      levels := int_of_string arguments.(0)
    with _ -> (show_help (); exit 1));

  if not (!levels > 0) then (show_help(); exit 1);


  ToHGame.build ()


let register _ = Generatorregistry.register_generator towers_of_hanoi_func "towersofhanoi" "Towers of Hanoi Reachability Game";;



			       


