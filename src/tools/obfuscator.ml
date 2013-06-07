open Arg;;
open Tcsargs;;

module CommandLine =
struct
  let input_file = ref ""

  let obfuscate_nodes = ref true

  let obfuscate_edges = ref true

  let reverse_edges = ref false

  let speclist =  [(["--disablenodeobfuscation"; "-dn"], Unit(fun _ -> obfuscate_nodes := false),
                      "\n     keep ordering of nodes") ;
                   (["--disableedgeobfuscation"; "-de"], Unit(fun _ -> obfuscate_edges := false),
                      "\n     keep ordering of edges");
                   (["--justreverseedges"; "-rv"], Unit(fun _ -> obfuscate_edges := false; reverse_edges := true),
                      "\n     reverse edges (disables a previous `-de')")]
                   
  let header = Info.get_title "Obfuscator Tool"
end ;;

open CommandLine ;;


let _ =
  SimpleArgs.parsedef speclist (fun f -> input_file := f) (header ^ "Usage: obfuscator [infile]\n" ^
                                              "Obfuscates the parity game given in <infile>. If this argument is omitted it reads a game from STDIN.");

  let in_channel = if !input_file = "" then stdin else open_in !input_file in

  let game = Paritygame.parse_parity_game in_channel in

  let m = Array.length game in
  let swap = Array.make m 0 in

  for i=0 to m-1 do
    swap.(i) <- i
  done;

  Random.self_init ();

  if !obfuscate_nodes then (
      for k=1 to 10*(m-1) do
        let i = Random.int m in
        let j = Random.int m in

        let x = swap.(i) in
        swap.(i) <- swap.(j);
        swap.(j) <- x
      done
  );

(*  print_string "Original game:\n";
  Paritygame.print_game game;

  print_string "Swapping table:\n";
  Array.iteri (fun i -> fun j -> print_string ("  " ^ string_of_int i ^ " -> " ^ string_of_int j ^ "\n")) swap;
*)
  let game' = Array.make m (0,0,[||],None) in

  for i=0 to m-1 do
    let (p,pl,succs,name) = game.(i) in

    let n = Array.length succs in
    let succs' = Array.copy succs in
    if !obfuscate_edges then (
        for k=1 to 10*(n-1) do
          let j = Random.int n in
          let j' = Random.int n in

          let x = succs'.(j) in
          succs'.(j) <- succs'.(j');
          succs'.(j') <- x
        done
    );
    let succs' = if !reverse_edges then
                    Array.init n (fun i -> succs'.(n-i-1))
                 else succs' 
    in
    game'.(swap.(i)) <- (p, pl, Array.map (fun v -> swap.(v)) succs', name)
  done;

(*  print_string "Obfuscated game:\n"; *)
  Paritygame.print_game game'
