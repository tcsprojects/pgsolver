let generator_game_func arguments = 

	let show_help _ =
		print_string (Info.get_title "Jurdzinski Game Generator");
		print_string ("Usage: jurdzinskigame n m\n\n" ^
					  "       where (n,m) = (n,m)-th jurdzinski game\n\n")
	in

	let rec range i j = if i=j then [j] else i::(range (i+1) j) in

  if (Array.length arguments != 2) then (show_help (); exit 1);

  let d = int_of_string arguments.(0) in
  let w = int_of_string arguments.(1) in

  let name i j = 2*w*i + j in
  
  let pg = Paritygame.pg_create (1 + name (2*d) (2*w-1)) in
  
  let print_node name prio player edges ann =
	pg.(name) <- (prio, player, Array.of_list edges, ann)
  in

  let m = (2*d+2) in

  for i=0 to d-1 do
    let i'=2*i in
    for j=0 to w-1 do
      let j' = 2*j in
      print_node (name i' j') (m-i'-1) 0 [name (i'+1) (j'+1)] None
    done
  done;
  for i=0 to d-1 do
    let i'=2*i+1 in
    print_node (name i' 0) (m-i'-1) 1 [name i' 1; name (i'-1) 0] None;
    for j=1 to w-1 do
      let j' = 2*j in
      print_node (name i' j') (m-i'-1) 1 [name i' (j'-1); name i' (j'+1); name (i'-1) j'] None
    done;
    for j=0 to w-2 do
      let j' = 2*j+1 in
      print_node (name i' j') (m-i'-1) 0 [name i' (j'-1); name i' (j'+1); name (2*d) j'] None
    done;
    print_node (name i' (2*w-1)) (m-i'-1) 0 [name i' (2*w-1); name i' (2*w-2)] None
  done;

  print_node (name (2*d) 0) 0 0 [name (2*d) 1] None;
  for j=1 to w-1 do
    let j' = 2*j in
    print_node (name (2*d) j') 0 0 [name (2*d) (j'-1); name (2*d) (j'+1)] None
  done;
  for j=0 to w-2 do
    let j' = 2*j+1 in
    print_node (name (2*d) j') 1 1 ((name (2*d) (j'-1))::(name (2*d) (j'+1))::
                                          (List.map (fun i -> name (2*i+1) j') (range 0 (d-1)))) None
  done;
  print_node (name (2*d) (2*w-1)) 1 1 ((name (2*d) (2*w-2))::(name (2*d) (2*w-1))::
                                             (List.map (fun i -> name (2*i+1) (2*w-1)) (range 0 (d-1)))) None;
  (* letzte Zeile separat *)
   pg;;


Generators.register_generator generator_game_func "jurdzinskigame" "Jurdzinski Game";;
