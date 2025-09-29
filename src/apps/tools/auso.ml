open Arg ;;
open Tcsargs;;
open Tcsbasedata;;
open Basics ;;
open Paritygame ;;
open Tcsstrings ;;
open Str ;;
open Stratimpralgs;;
open Tcslist;;
open Tcsarray;;

let out s =
	print_string s;
	flush stdout

let _ =
	let header = Info.get_title "Parity Game to AUSO / GridUSO transformer" in
  
	SimpleArgs.parsedef [] (fun _ -> ()) (header ^ "Usage: auso\n" ^
                                           "Computes the AUSO / GridUSO associated with a parity game.");

	let (in_channel,name) = (stdin,"STDIN") in

	let game = Parsers.parse_parity_game in_channel in
    let n = pg_size game in
    
    (* Remove trivial player 0 nodes *)
    pg_iterate (fun i -> fun (_,pl,tr,_,_) -> if (pl = plr_Even) && (ns_size tr = 1)
					      then pg_set_owner game i plr_Odd)
	       game;
  
    (* Multiplier Table *)
    let m = Array.make n 1 in
    for i = n-1 downto 1 do
        let pl = pg_get_owner game i in
        let tr = pg_get_successors game i in
        if (pl = plr_Even)
        then m.(i-1) <- ns_size tr * m.(i)
        else m.(i-1) <- m.(i)
    done;
  
    (* Map Strategies to Ints *)
    let strategy_to_int strategy =
      let x = ref 0 in
      for i = n-1 downto 0 do
	let pl = pg_get_owner game i in 
        let tr = pg_get_successors game i in
        if (pl = plr_Even) then (
          let j = ArrayUtils.index_of (Array.of_list (ns_nodes tr)) strategy.(i) in
          x := !x + m.(i) * j;
		)
      done;
      !x
    in
  
    (* Map Int to Strategies *)
    (*
  	let int_to_strategy numb =
      let strategy = Array.make n (-1) in
      let numb = ref numb in
      for i = 0 to n-1 do
        let (_, pl, tr, _) = game.(i) in
        if (pl = 0) then (
          strategy.(i) <- tr.(!numb / m.(i));
          numb := !numb mod m.(i);
		)
      done;
      strategy
   in
   *)
     
   let format_strategy strategy =
     let s = ref "" in
     for i = 0 to n-1 do
       let pl = pg_get_owner game i in
       let tr = pg_get_successors game i in
       if (pl = plr_Even) then (
         let j = ArrayUtils.index_of (Array.of_list (ns_nodes tr)) strategy.(i) in
         s := !s ^ string_of_int j
       )
     done;
     !s
   in
     
   let rec iterate strategy index callback =
     if index >= n
     then callback strategy
     else
       let pl = pg_get_owner game index in
       let tr = (Array.of_list (ns_nodes (pg_get_successors game index))) in
       if pl = plr_Odd
       then iterate strategy (index + 1) callback
       else
         for j = 0 to Array.length tr - 1 do
           strategy.(index) <- tr.(j);
           iterate strategy (index + 1) callback
         done
   in
      
   out "* ";
   pg_iterate (fun i -> fun (_,pl,_,_,desc) -> if pl = plr_Even then
						 match desc with
						   None -> print_string " _" |
						   Some s -> print_string (" " ^ s)) game;
   out "\n";
      
   iterate (Array.make n (-1)) 0 (fun strategy ->
     let temp = Array.copy strategy in
     let valu = evaluate_strategy game node_total_ordering_by_position strategy in
     let l = ref [] in
     let k = ref [] in
     pg_iterate (fun i -> fun (_,pl,succs,_,_) -> let tr = Array.of_list (ns_nodes succs) in
						  if (pl = plr_Even) then
						    for j = 0 to Array.length tr - 1 do
						      let c = node_valuation_ordering game node_total_ordering_by_position valu.(strategy.(i)) valu.(tr.(j)) in
						      if (c != 0) then (
							temp.(i) <- tr.(j);
							if c < 0
							then l := strategy_to_int temp :: !l
							else k := strategy_to_int temp :: !k;
							temp.(i) <- strategy.(i)
						      )
						    done;
		) game;
     let l = List.rev !l in
     let k = List.rev !k in
     out (string_of_int (strategy_to_int strategy) ^ "(" ^ format_strategy strategy ^ "): ");
     out (ListUtils.custom_format string_of_int "" "" ", " l);
     out (" | ");
     out (ListUtils.custom_format string_of_int "" "" ", " k);
     out ("\n");
   );
   
   out "\n";;
