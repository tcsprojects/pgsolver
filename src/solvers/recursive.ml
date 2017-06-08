(* The recursive algorithm
 *
 * from:
 * Wieslaw Zielonka. Infinite Games on Finitely Coloured Graphs with Applications to Automata on Infinite Trees
 * Theor. Comput. Sci. 200(1-2): 135-183 (1998)
 *)

open Basics ;;
open Paritygame ;;
open Univsolve;;



let solver rec_solve game =
    let msg_tagged v = message_autotagged v (fun _ -> "RECURSIVE") in
    let msg_plain = message in
    let l = pg_size game in
    msg_tagged 3 (fun _ -> "Solving the game: " ^ format_game game ^ "\n");
    let solution = sol_create game in
    let strategy = Array.make l (-1) in

    let max_prio = ref (-1) in
    let more_than_one = ref false in
    pg_iterate (fun v -> fun (pr,_,_,_,_) -> if pr <> -1
					     then (if !max_prio > -1 && pr <> !max_prio then more_than_one := true;
						   if pr > !max_prio then max_prio := pr)) game;

    if !max_prio = -1 then (solution, strategy)
    else if not !more_than_one
    then (message 3 (fun _ -> "  Only " ^ (if !max_prio mod 2 = 0 then "even" else "odd") ^
                     " priority " ^ string_of_int !max_prio ^ " present\n");
          let winner = plr_benefits !max_prio in
	  pg_iterate (fun v -> fun (p',pl',ws,_,_) -> if p' <> -1
						      then (solution.(v) <- winner;
							    if pl'=winner then strategy.(v) <- ns_some ws)) game;
          message 3 (fun _ -> "  Returned solution:\n    " ^
                     format_solution solution ^ "\n");
          message 3 (fun _ -> "  Returned strategy:\n    " ^
                     format_strategy strategy ^ "\n");
          (solution,strategy))
    else (
    	  let max_prio = ref (pg_max_prio game) in
    	  let pl = plr_benefits !max_prio in
          msg_tagged 3 (fun _ -> "Highest priority " ^ string_of_int !max_prio ^ " belongs to player " ^
                              plr_show pl ^ "\n");
          let nodes_with_prio_p = collect_max_parity_nodes game in

          msg_tagged 3 (fun _ -> "The following nodes have priority " ^ string_of_int !max_prio ^
                              ": P = {" ^ String.concat "," (List.map string_of_int (ns_nodes nodes_with_prio_p))
                              ^ "}\n");

          let attr = attr_closure_inplace game strategy pl nodes_with_prio_p in

          msg_tagged 3 (fun _ -> "The attractor of P for player " ^ plr_show pl ^ " is: {" ^
                     String.concat "," (List.map string_of_int (ns_nodes attr)) ^ "}\n");

        (*
          let game' = pg_copy game in
          pg_remove_nodes game' attr;
          *)
            let (game', map_to_sub, map_to_game) = subgame_by_node_filter game (fun i ->
                not (ns_elem i attr)
            ) in

          msg_tagged 3 (fun _ -> "First recursive descent to subgame ...\n");

          let (sol,str) = !rec_solve game' in

          msg_tagged 3 (fun _ -> "Checking whether the opponent wins from any node of the subgame ...");

          if not (Array.fold_left (fun b -> fun s -> b || s = plr_opponent pl) false sol)
          then (msg_plain 3 (fun _ -> " no\n");
                msg_tagged 3 (fun _ -> "Merging and completing strategies and solutions:\n");

        		pg_iterate (fun v (_,pl',_,_,_) ->
        		    solution.(map_to_game v) <- pl;
                    if pl' = pl then strategy.(map_to_game v) <- map_to_game str.(v)
                ) game';
                ns_iter (fun v -> solution.(v) <- pl) attr;
                ns_iter (fun v -> if pg_get_owner game v = pl then let ws = pg_get_successors game v in strategy.(v) <- ns_some ws) nodes_with_prio_p;

                msg_tagged 3 (fun _ -> "Solution: " ^ format_solution solution ^ "\n");
                msg_tagged 3 (fun _ -> "Strategy: " ^ format_strategy strategy ^ "\n");
                (solution,strategy))
          else (message 3 (fun _ -> " yes\n");
                let opponent_win = ref ns_empty in
                let opp = plr_opponent pl in
                Array.iteri (fun v s ->
                  if s = opp then opponent_win := ns_add (map_to_game v) !opponent_win
                ) sol;

                msg_tagged 3 (fun _ -> "Opponent " ^ plr_show opp ^ " wins from nodes Q = {" ^
                          String.concat "," (List.map string_of_int (ns_nodes !opponent_win)) ^ "}\n");

                ns_iter (fun v -> strategy.(v) <- -1) attr;

                let attr = attr_closure_inplace game strategy opp !opponent_win in
                msg_tagged 3 (fun _ -> "The attractor of Q for player " ^ plr_show opp ^ " is: {" ^
                          String.concat "," (List.map string_of_int (ns_nodes attr)) ^ "}\n");

                ns_iter (fun v -> solution.(v) <- opp) attr;
                ns_iter (fun v ->
                    solution.(v) <- opp;
                    if pg_get_owner game v = opp
                    then strategy.(v) <- map_to_game str.(map_to_sub v)
                ) !opponent_win;

(*
                let game' = pg_copy game in
                pg_remove_nodes game' attr;

                msg_tagged 3 (fun _ -> "Second recursive descent to subgame ....\n");

                let (sol,str) = !rec_solve game' in

                msg_tagged 3 (fun _ -> "Merging and completing strategies and solutions:\n");

                pg_iterate (fun v -> fun (_,ow,_,_,_) -> solution.(v) <- sol.(v);
							 if ow = sol.(v) then strategy.(v) <- str.(v)) game';
							 *)

                let (game', map_to_sub, map_to_game) = subgame_by_node_filter game (fun i ->
                    not (ns_elem i attr)
                ) in

                msg_tagged 3 (fun _ -> "Second recursive descent to subgame ....\n");

                let (sol,str) = !rec_solve game' in

                msg_tagged 3 (fun _ -> "Merging and completing strategies and solutions:\n");

                pg_iterate (fun v -> fun (_,ow,_,_,_) -> solution.(map_to_game v) <- sol.(v);
							 if ow = sol.(v) then strategy.(map_to_game v) <- map_to_game str.(v)) game';


                msg_tagged 3 (fun _ -> "Solution: " ^ format_solution solution ^ "\n");
                msg_tagged 3 (fun _ -> "Strategy: " ^ format_strategy strategy ^ "\n");

                (solution,strategy)));;

let mcnaughton_zielonka game options =
	let f = ref (fun _ -> ([||], [||])) in
	f := universal_solve (universal_options_alter_verb options verbosity_level_default) (solver f);
(*	f := solver f; *)
	universal_solve options (solver f) game;;

let solve2 recsolve = solver recsolve;;

let solve game =
  mcnaughton_zielonka game (universal_solve_init_options_verbose !universal_solve_global_options);;


let fallback_solve game backend options =
	let f = ref (fun _ -> ([||], [||])) in
	f := universal_solve (universal_options_alter_verb options verbosity_level_default) (solver f);
	universal_solve_fallback options backend (solver f) game;;



let register _ = Solverregistry.register_solver solve "recursive" "re" "use the recursive algorithm due to McNaughton / Zielonka";;
