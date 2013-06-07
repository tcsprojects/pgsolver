open Basics ;;
open Paritygame ;;
open Univsolve;;
open Solvers;;



let solver rec_solve game =
    let msg_tagged v = message_autotagged v (fun _ -> "RECURSIVE") in
    let msg_plain = message in
    let l = Array.length game in
    msg_tagged 3 (fun _ -> "Solving the game: " ^ format_game game ^ "\n");
    let solution = Array.make l (-1) in
    let strategy = Array.make l (-1) in

    let max_prio = ref (-1) in
    let more_than_one = ref false in
    for v=0 to l-1 do
      let (p,_,_,_) = game.(v) in
      if p <> -1
      then (if !max_prio > -1 && p <> !max_prio then more_than_one := true;
            if p > !max_prio then max_prio := p)
    done;

    if !max_prio = -1 then (solution, strategy)
    else if not !more_than_one
    then (message 3 (fun _ -> "  Only " ^ (if !max_prio mod 2 = 0 then "even" else "odd") ^
                     " priority " ^ string_of_int !max_prio ^ " present\n");
          let winner = !max_prio mod 2 in
          for v=0 to l-1 do
            let (p',pl',ws,_) = game.(v) in
            if p' <> -1
            then (solution.(v) <- winner;
                  if pl'=winner then strategy.(v) <- ws.(0))
          done;
          message 3 (fun _ -> "  Returned solution:\n    " ^
                     format_solution solution ^ "\n");
          message 3 (fun _ -> "  Returned strategy:\n    " ^
                     format_strategy strategy ^ "\n");
          (solution,strategy))
    else (
    	  let max_prio = ref (pg_max_prio game) in
    	  let pl = !max_prio mod 2 in
          msg_tagged 3 (fun _ -> "Highest priority " ^ string_of_int !max_prio ^ " belongs to player " ^
                              string_of_int pl ^ "\n");
          let nodes_with_prio_p = collect_max_parity_nodes game in

          msg_tagged 3 (fun _ -> "The following nodes have priority " ^ string_of_int !max_prio ^
                              ": P = {" ^ String.concat "," (List.map string_of_int nodes_with_prio_p)
                              ^ "}\n");

          let attr = attr_closure_inplace game strategy pl nodes_with_prio_p in

          msg_tagged 3 (fun _ -> "The attractor of P for player " ^ string_of_int pl ^ " is: {" ^
                     String.concat "," (List.map string_of_int attr) ^ "}\n");

          let game' = Array.copy game in
          pg_remove_nodes game' attr;
          msg_tagged 3 (fun _ -> "First recursive descent to subgame ...\n");

          let (sol,str) = !rec_solve game' in

          msg_tagged 3 (fun _ -> "Checking whether the opponent wins from any node of the subgame ...");

          if not (Array.fold_left (fun b -> fun s -> b || s = 1-pl) false sol)
          then (msg_plain 3 (fun _ -> " no\n");
                msg_tagged 3 (fun _ -> "Merging and completing strategies and solutions:\n");

                for v=0 to l-1 do
                  let (p',pl',_,_) = game'.(v) in
                  if p' > -1 then (solution.(v) <- pl;
                                   if pl' = pl then strategy.(v) <- str.(v))
                done;
                List.iter (fun v -> solution.(v) <- pl) attr;
                List.iter (fun v -> let (_,pl',ws,_) = game.(v) in
                                    if pl' = pl then strategy.(v) <- ws.(0)) nodes_with_prio_p;

                msg_tagged 3 (fun _ -> "Solution: " ^ format_solution solution ^ "\n");
                msg_tagged 3 (fun _ -> "Strategy: " ^ format_strategy strategy ^ "\n");
                (solution,strategy))
          else (message 3 (fun _ -> " yes\n");
                let opponent_win = ref [] in
                let opp = 1-pl in
                for v=0 to (l-1) do
                  if sol.(v) = opp then opponent_win := v :: !opponent_win
                done;
                msg_tagged 3 (fun _ -> "Opponent " ^ string_of_int opp ^ " wins from nodes Q = {" ^
                          String.concat "," (List.map string_of_int !opponent_win) ^ "}\n");

                List.iter (fun v -> strategy.(v) <- -1) attr;

                let attr = attr_closure_inplace game strategy opp !opponent_win in
                msg_tagged 3 (fun _ -> "The attractor of Q for player " ^ string_of_int opp ^ " is: {" ^
                          String.concat "," (List.map string_of_int attr) ^ "}\n");

                List.iter (fun v -> solution.(v) <- opp) attr;
                List.iter (fun v -> solution.(v) <- opp;
                                    let (p',pl',_,_) = game'.(v) in
                                    if p' > -1 && pl'=opp then strategy.(v) <- str.(v)) !opponent_win;

                let game' = Array.copy game in
                pg_remove_nodes game' attr;

                msg_tagged 3 (fun _ -> "Second recursive descent to subgame ....\n");

                let (sol,str) = !rec_solve game' in

                msg_tagged 3 (fun _ -> "Merging and completing strategies and solutions:\n");

                for v=0 to l-1 do
                  let (p',pl',_,_) = game'.(v) in
                  if p' > -1 then (solution.(v) <- sol.(v);
                                   if pl' = sol.(v) then strategy.(v) <- str.(v))
                done;

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



let _ = register_solver solve "recursive" "re" "use the recursive algorithm due to McNaughton / Zielonka";;
