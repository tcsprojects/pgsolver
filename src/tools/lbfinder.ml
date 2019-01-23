open Arg ;;
open Tcsargs;;
open Basics ;;
open Paritygame ;;
open Tcsstrings ;;
open Tcsbasedata ;;
open Tcsarray;;
open Tcslist;;
open Tcsset ;;
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


	let zadeh_game_compute_state game valu =
		let desc i = OptionUtils.get_some (game#get_desc  i) in
		let find s =
			let i = ref 0 in
			while (!i < game#size ) && (desc !i <> s) do
				incr i
			done;
			if !i < game#size  then Some !i else None
		in
		let leadsto i j =
			let (_, path, _) = valu.(OptionUtils.get_some i) in
			TreeSet.mem (OptionUtils.get_some j) path
		in
		let n = ref 0 in
		while (find ("E" ^ string_of_int !n) != None) do incr n done;
		let n = !n in
		(* cycle left, cycle right, up left, up right, pair selector, lane *)
		let state = Array.make n (0,0,0,0,0,0) in

		for i = 0 to n - 1 do
			state.(i) <- (
				(if (leadsto (find ("a" ^ string_of_int i)) (find ("E" ^ string_of_int i))) &&
					 (leadsto (find ("b" ^ string_of_int i)) (find ("E" ^ string_of_int i))) &&
						 (leadsto (find ("e" ^ string_of_int i)) (find ("E" ^ string_of_int i))) then 1 else 0),
			  (if (i < n-1) && (leadsto (find ("v" ^ string_of_int i)) (find ("X" ^ string_of_int i))) &&
				   (leadsto (find ("w" ^ string_of_int i)) (find ("X" ^ string_of_int i))) &&
		 			   (leadsto (find ("f" ^ string_of_int i)) (find ("X" ^ string_of_int i))) then 1 else 0),
			  (if (leadsto (find ("g" ^ string_of_int i)) (find ("c" ^ string_of_int i))) then 1 else 0),
				(if (i < n-1) && (leadsto (find ("s" ^ string_of_int i)) (find ("u" ^ string_of_int i))) then 1 else 0),
				(if (leadsto (find ("d" ^ string_of_int i)) (find ("E" ^ string_of_int i))) then 0 else 1),
				(if (leadsto (find ("m" ^ string_of_int i)) (find ("d" ^ string_of_int i))) then 1 else 0)
				)
	    done;

		let state' = Array.make (n+1) 0 in
		let lane' = Array.make (n+1) 1 in
		let ps' = Array.make (n+1) 1 in
		state'.(n) <- 0;
		for i = n - 1 downto 0 do
			let (cl,cr,ul,ur,ps,la) = state.(i) in
			let sl = cl * ul * (1 - ps) * (1 - state'.(i+1) * ps'.(i+1)) * (if i < n-2 then lane'.(i+2) else 1) in
			let sr = cr * ur * ps * ps'.(i+1) * state'.(i+1) in
			state'.(i) <- sl + sr;
			ps'.(i) <- sl * (1 - ps) + sr * ps;
			lane'.(i) <- if state'.(i) = 1 then la else 1 - la
		done;
    state'


let state_to_number state =
		let k = ref 0 in
		let a = ref 1 in
		Array.iter (fun b ->
			k := !k + !a * b;
			a := !a * 2
		) state;
		!k


let iterations game tie_list init_list =
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
		let bincounter = ref 0 in
		let lastbincounter = ref (-1) in
	_strat_impr_callback := Some (fun strat counter ->
		itercounter := counter;
		let node_compare = node_total_ordering_by_position in
		let valu = evaluate_strategy game node_compare strat in
		let bc = state_to_number (zadeh_game_compute_state game valu) in
		if (bc > !lastbincounter) then (
			incr bincounter;
			lastbincounter := bc
			);
		(*out( string_of_int (bc) ^ "\n")*)
	);
	  initial_strategy_list := Some init_list;
    lre_sub_exp_list := tie_list;
	(match !solver with
		None -> ()
	|	Some solve -> let _ = solve game in ());
	(!itercounter, !bincounter)


let format_tie_list =
    ListUtils.custom_format (fun ((c1,i1),(c2,i2)) ->
        "(('" ^ String.make 1 c1 ^ "'," ^ string_of_int i1 ^ "),('" ^ String.make 1 c2 ^ "'," ^ string_of_int i2 ^ "))"
    ) "[\n" "]\n" ";\n"

		let mp = function 			None -> ('!', 0)
                      		|	Some s ->
                      				if String.length s = 1
                      				then (String.get s 0, 0)
                      				else if String.get s 1 = '('
                      				then (String.get s 0, 0)
                      				else (String.get s 0, int_of_string (StringUtils.rest_string s 1))

let generate_tie_list game =
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


let generate_init_list game =
		let l = ref [] in
		game#iterate (fun v (_, pl, edges, _, desc_v) ->
        if (plr_Even = pl && ns_size edges > 1) then (
            let w = ns_some edges in
						let desc_w = game#get_desc w in
						l := (mp desc_v, mp desc_w)::!l
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


let next_init_list game strategy =
  let strategy = strategy#copy in
	let n = game#size in
	let r = Random.int 3 in
	for k = 0 to r do
			let i = Random.int n in
			if game#get_owner i = plr_Even then (
					let edges = Array.of_list (ns_nodes (game#get_successors i)) in
					let j = Random.int (Array.length edges) in
					strategy#set i edges.(j)
			)
	done;
	strategy



let _ =
  Random.self_init ();
  SimpleArgs.parsedef speclist (fun _ -> ()) (header ^ "\nOptions are");
  let game = Parsers.parse_parity_game stdin in
	let init_strategy = ref (initial_strategy_by_best_reward game) in
  let current_tie_list = ref (generate_tie_list game)
	in
	(*
  let current_tie_list = ref [
(('q',0),('m',1));
(('a',0),('o',0));
(('w',0),('q',0));
(('m',1),('m',2));
(('s',0),('m',0));
(('o',0),('m',1));
(('d',1),('X',1));
(('p',0),('d',0));
(('a',3),('o',3));
(('o',2),('d',0));
(('i',1),('m',1));
(('s',1),('m',0));
(('q',1),('m',1));
(('a',0),('E',0));
(('g',2),('c',2));
(('e',0),('E',0));
(('d',2),('X',2));
(('g',2),('m',0));
(('p',3),('m',1));
(('p',1),('d',0));
(('v',2),('r',2));
(('m',3),('Y',0));
(('g',1),('m',0));
(('e',1),('p',1));
(('e',2),('E',2));
(('b',1),('p',1));
(('o',3),('d',0));
(('s',2),('m',0));
(('q',2),('m',1));
(('a',2),('o',2));
(('s',1),('u',1));
(('a',3),('E',3));
(('p',3),('d',0));
(('h',0),('d',0));
(('b',3),('p',3));
(('b',0),('E',0));
(('e',3),('p',3));
(('a',1),('o',1));
(('g',0),('m',0));
(('f',1),('i',1));
(('b',0),('p',0));
(('g',3),('m',0));
(('q',1),('d',0));
(('m',2),('m',3));
(('v',0),('r',0));
(('w',2),('q',2));
(('r',0),('m',1));
(('r',0),('d',0));
(('d',2),('E',2));
(('d',1),('E',1));
(('i',1),('d',0));
(('w',0),('X',0));
(('o',3),('m',1));
(('o',0),('d',0));
(('v',1),('r',1));
(('p',2),('d',0));
(('o',1),('m',1));
(('e',0),('p',0));
(('v',0),('X',0));
(('e',2),('p',2));
(('g',1),('c',1));
(('r',2),('m',1));
(('o',1),('d',0));
(('b',3),('E',3));
(('p',0),('m',1));
(('m',1),('d',1));
(('s',0),('u',0));
(('q',0),('d',0));
(('h',1),('m',1));
(('r',1),('d',0));
(('f',2),('X',2));
(('f',2),('i',2));
(('w',1),('X',1));
(('b',2),('E',2));
(('i',0),('d',0));
(('v',2),('X',2));
(('m',2),('d',2));
(('v',1),('X',1));
(('r',2),('d',0));
(('r',1),('m',1));
(('h',0),('m',1));
(('i',2),('d',0));
(('p',2),('m',1));
(('s',2),('u',2));
(('b',1),('E',1));
(('h',2),('d',0));
(('e',1),('E',1));
(('f',1),('X',1));
(('f',0),('X',0));
(('o',2),('m',1));
(('h',3),('d',0));
(('m',3),('d',3));
(('d',0),('E',0));
(('g',3),('c',3));
(('h',3),('m',1));
(('q',2),('d',0));
(('w',1),('q',1));
(('f',0),('i',0));
(('i',2),('m',1));
(('p',1),('m',1));
(('h',1),('d',0));
(('w',2),('X',2));
(('m',0),('d',0));
(('h',2),('m',1));
(('a',1),('E',1));
(('b',2),('p',2));
(('d',0),('X',0));
(('a',2),('E',2));
(('g',0),('c',0));
(('i',0),('m',1));
(('m',0),('m',1));
(('e',3),('E',3))] in *)
  let current_tie_iter = ref (iterations game !current_tie_list !init_strategy) in
  out ("Iterations: " ^ string_of_int (fst !current_tie_iter) ^ " / " ^ string_of_int (snd !current_tie_iter) ^ "\n");
  while true do
    let next_tie_list = next_tie_list !current_tie_list in
		let next_init_strategy = next_init_list game !init_strategy in
    let next_tie_iter = iterations game next_tie_list next_init_strategy in
    if (fst next_tie_iter > fst !current_tie_iter || (fst next_tie_iter = fst !current_tie_iter && snd next_tie_iter > snd !current_tie_iter)) then (
        out (format_tie_list next_tie_list);
        out ("Iterations: " ^ string_of_int (fst !current_tie_iter) ^ " / " ^ string_of_int (snd !current_tie_iter) ^ "\n");
    );
    if (fst next_tie_iter >= fst !current_tie_iter && snd next_tie_iter >= snd !current_tie_iter) then (
        current_tie_list := next_tie_list;
				init_strategy := next_init_strategy;
        current_tie_iter := next_tie_iter
    );
  done;;
