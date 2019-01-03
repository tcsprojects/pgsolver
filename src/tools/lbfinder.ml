open Arg ;;
open Tcsargs;;
open Basics ;;
open Paritygame ;;
open Tcsstrings ;;
open Tcsarray;;
open Tcslist;;
open Univsolve;;
open Solvers ;;
open Str ;;
open Stratimpralgs ;;
open Pgnodeset;;
open Pgplayer;;
open Switch_zadeh;;


module CommandLine =
struct
  let solver = ref None

  let speclist = [
  ]

		      @

  fold_solvers (fun solve ident abbrev desc arr ->
  					(["--" ^ ident; "-" ^ abbrev], String(fun solveargs -> let solve = solve (Array.of_list (Tcsstrings.StringUtils.explode solveargs ' ')) in solver := Some solve),
  					 "\n     Use solver: " ^ desc)::arr
  ) []

  let header = Info.get_title "LB Finder"
end ;;

open CommandLine ;;


let out s =
	print_string s;
	flush stdout


let iterations game tie_list =
    (Univsolve.universal_solve_global_options := fun gen_stat verb -> {
        generate_statistics = gen_stat ;
        verb_level = verb ;
        global_optimization = false ;
        decompose_sccs = false ;
        solve_special_games = false ;
        local_optimization = false ;
        globalopt_remove_useless_self_cycles = false ;
        globalopt_solve_useful_self_cycles = false ;
        solvespec_single_parity = false ;
        solvespec_single_player = false ;
        localopt_priority_propagation = false ;
        localopt_compact_priorities = false ;
      });
    let itercounter = ref 0 in
	_strat_impr_callback := Some (fun strat counter -> itercounter := counter);
    lre_sub_exp_list := tie_list;
	(match !solver with
		None -> ()
	|	Some solve -> let _ = solve game in ());
	!itercounter


let format_tie_list =
    ListUtils.custom_format (fun ((c1,i1),(c2,i2)) ->
        "(('" ^ String.make 1 c1 ^ "'," ^ string_of_int i1 ^ "),('" ^ String.make 1 c2 ^ "'," ^ string_of_int i2 ^ "))"
    ) "[\n" "]\n" ";\n"

let generate_tie_list game =
    let mp = function 			None -> ('!', 0)
                      		|	Some s ->
                      				if String.length s = 1
                      				then (String.get s 0, 0)
                      				else if String.get s 1 = '('
                      				then (String.get s 0, 0)
                      				else (String.get s 0, int_of_string (StringUtils.rest_string s 1))
                      				in
    let l = ref [] in
    game#iterate (fun v (_, pl, edges, _, desc_v) ->
        if (plr_Even = pl && ns_size edges > 1) then (
            ns_iter (fun w ->
                let desc_w = game#get_desc w in
                l := (mp desc_v, mp desc_w)::!l
            ) edges
        );
    );
    !l


let next_tie_list tie_list =
    let a = Array.of_list tie_list in
    let n = Array.length a in
    let r = Random.int 3 in
    for k = 0 to r do
        let i = Random.int n in
        let j = Random.int n in
        let tmp = a.(i) in
        if i <= j then (
            for k = i + 1 to j do
                a.(k-1) <- a.(k)
            done
        ) else (
            for k = i - 1 downto j do
                a.(k+1) <- a.(k)
            done
        );
        a.(j) <- tmp
        (*
        let i = Random.int n in
        let j = Random.int n in
        if (j >= 0 && j < n) then (
            let tmp = a.(i) in
            a.(i) <- a.(j);
            a.(j) <- tmp
        )
        *)
    done;
    Array.to_list a


let _ =
  Random.self_init ();
  SimpleArgs.parsedef speclist (fun _ -> ()) (header ^ "\nOptions are");
  let game = Parsers.parse_parity_game stdin in
  let current_tie_list = ref (generate_tie_list game)
  (*let current_tie_list = ref !lre_sub_exp_list in*)
 in
  let current_tie_iter = ref (iterations game !current_tie_list) in
  out ("Iterations: " ^ string_of_int !current_tie_iter ^ "\n");
  while true do
    let next_tie_list = next_tie_list !current_tie_list in
    let next_tie_iter = iterations game next_tie_list in
    if (next_tie_iter > !current_tie_iter) then (
        out (format_tie_list next_tie_list);
        out ("Iterations: " ^ string_of_int next_tie_iter ^ " / " ^ string_of_int !current_tie_iter ^ "\n");
    );
    if (next_tie_iter >= !current_tie_iter) then (
        current_tie_list := next_tie_list;
        current_tie_iter := next_tie_iter
    );
  done;;
