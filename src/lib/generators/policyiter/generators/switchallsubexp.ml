open Arg ;;
open Tcsargs;;
open Tcsbasedata;;
open Paritygame;;
open Stratimprgenerators;;

type gamenode = DecLaneEven of int (*a*)
			  | DecLaneOdd of int (*b*)
			  | DecLaneRoot (*c*)
			  | CycleNode of int (*d*)
			  | CycleCenter of int (*e*)
			  | CycleAccess of int (*f*)
			  | CycleSelector of int (*g*)
			  | CycleLeaver of int (*h*)
			  | UpperSelector of int (*k*)
			  | FinalSink (*p*)
			  | FinalCycle (*q*)
			  | BitSelector (*r*)
			  | StartEven (*s*)
			  | Helper of int

let symb_to_str = function DecLaneEven i -> "a" ^ string_of_int i | DecLaneOdd i -> "b" ^ string_of_int i | DecLaneRoot -> "c" | CycleNode i -> "d" ^ string_of_int i | CycleCenter i -> "e" ^ string_of_int i | CycleAccess i -> "f" ^ string_of_int i | CycleSelector i -> "g" ^ string_of_int i | CycleLeaver i -> "h" ^ string_of_int i | UpperSelector i -> "k" ^ string_of_int i | FinalSink -> "p" | FinalCycle -> "q" | StartEven -> "s" | BitSelector -> "r" | Helper i -> "h" ^ string_of_int i

let mkli n f = (Array.to_list (Array.init n f))


let generator_game_func arguments =

	let n = ref None in
	let od2 = ref false in
	SimpleArgs.parsearr arguments
	                    [(["-binary"], Unit (fun _ -> od2 := true), "\n     binary-case")]
	                    (fun s -> n := Some (int_of_string s)) ("Options are")
	                    SimpleArgs.argprint_help SimpleArgs.argprint_bad;
	let n = OptionUtils.get_some !n in
	let od2 = !od2 in


	let helpercnt = ref 0 in

	let pg = SymbolicParityGame.create_new FinalCycle in

	let add sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in

	let rec left_exp = function [] -> failwith "imp" | [a] -> a
	|	a::b::r -> incr helpercnt; add (Helper !helpercnt) 0 plr_Even [a; b]; left_exp ((Helper !helpercnt)::r)
	in

	let left_exp' l =
		if List.length l <= 2 then l
		else let r = List.rev l in
			 let (h, t) = (List.hd r, List.rev (List.tl r)) in
			 [left_exp t; h]
	in

	let lexp l = if od2 then left_exp' l else l in

	add FinalCycle 1 plr_Odd [FinalCycle];
	add StartEven (8 * n + 6) plr_Even (lexp (FinalSink::(mkli n (fun j -> CycleAccess j))));
	add DecLaneRoot (8 * n + 4) plr_Even [StartEven; BitSelector];
	add BitSelector (8 * n + 8) plr_Even (lexp (FinalSink::(mkli n (fun j -> CycleSelector j))));
	add FinalSink (12 * n + 10) plr_Odd [FinalCycle];

	for i = 0 to 2 * n - 1 do
		add (DecLaneEven i) (4 * n + 2 * i + 4) plr_Odd [DecLaneOdd i];
		add (DecLaneOdd i) (4 * n + 2 * i + 3) plr_Even (if i = 0 then lexp [DecLaneRoot; BitSelector; StartEven] else lexp [DecLaneOdd (i - 1); BitSelector; StartEven])
	done;

	for i = 0 to n - 1 do
		add (CycleNode i) (4 * i + 3) plr_Even (lexp ([StartEven; CycleCenter i] @ (mkli (2 * i + 2) (fun j -> DecLaneEven j)) @ [BitSelector]));
		add (CycleCenter i) (4 * i + 4) plr_Odd [CycleNode i; CycleLeaver i];
		add (CycleLeaver i) (8 * n + 4 * i + 12) plr_Odd [UpperSelector i];
		add (UpperSelector i) (8 * n + 4 * i + 9) plr_Even (lexp (FinalSink::(mkli (n - i - 1) (fun j -> CycleSelector (n - j - 1)))));
        add (CycleAccess i) (8 * n + 4 * i + 11) plr_Odd [CycleCenter i];
        add (CycleSelector i) (4 * i + 6) plr_Even [CycleAccess i; UpperSelector i];
	done;

	SymbolicParityGame.to_paritygame pg;;


let register _ =
    register_strat_impr_gen {
        ident = "switchallsubexp";
        description = "Binary-Case Subexponential Lower Bound for (Voege's) switch-all rule";
        parity_game = Some generator_game_func;
        generalized_mdp = None;
    }
