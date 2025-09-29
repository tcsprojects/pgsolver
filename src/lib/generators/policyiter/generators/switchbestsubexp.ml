open Arg ;;
open Tcsargs;;
open Tcsbasedata;;
open Paritygame;;
open Stratimprgenerators;;


type gamenode = DecLaneEven of int (*a*)
			  | DecLaneOdd of int (*b*)
			  | DecLaneRoot (*c*)
			  | CycleNode0 of int (*e*)
			  | CycleNode1 of int (*f*)
			  | CycleNode2 of int (*g*)
			  | CycleCenter of int (*h*)
			  | CycleAccess of int (*k*)
			  | CycleSelector of int (*l*)
			  | CycleSelectorX of int (*y*)
			  | CycleLeaver of int (*m*)
			  | UpperSelector of int (*z*)
			  | FinalSink (*p*)
			  | FinalCycle (*q*)
			  | StartEven (*s*)
			  | BitSelector (*u*)
			  | Helper of int

let symb_to_str = function DecLaneEven i ->"a" ^ string_of_int i | DecLaneOdd i ->"b" ^ string_of_int i | DecLaneRoot ->"c" | CycleNode0 i ->"e" ^ string_of_int i | CycleNode1 i ->"f" ^ string_of_int i | CycleNode2 i ->"g" ^ string_of_int i | CycleCenter i ->"h" ^ string_of_int i | CycleAccess i ->"k" ^ string_of_int i | CycleSelector i ->"l" ^ string_of_int i | CycleSelectorX i ->"y" ^ string_of_int i | CycleLeaver i ->"m" ^ string_of_int i | UpperSelector i ->"z" ^ string_of_int i | FinalSink ->"p" | FinalCycle ->"q" | StartEven ->"s" | BitSelector ->"u" | Helper i ->"h" ^ string_of_int i

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

	add DecLaneRoot (20 * n) plr_Odd [BitSelector];
	add StartEven (20 * n + 2) plr_Even (lexp (FinalSink::(mkli n (fun j -> CycleAccess j))));
	add BitSelector (20 * n + 4) plr_Even (lexp (FinalSink::(mkli n (fun j -> CycleSelectorX j))));

	add FinalSink (24 * n + 6) plr_Odd [FinalCycle];

	for i = 0 to 6 * n - 3 do
		add (DecLaneEven i) (8 * n + 4 + 2 * i) plr_Odd [DecLaneOdd i];
		add (DecLaneOdd i) (8 * n + 3 + 2 * i) plr_Even (lexp [(if i = 0 then DecLaneRoot else DecLaneOdd (i - 1)); BitSelector; StartEven])
	done;

	for i = 0 to n - 1 do
		add (CycleNode0 i) (i * 8 + 3) plr_Even (lexp (([StartEven; CycleNode1 i; DecLaneRoot] @ (mkli (2 * i + 1) (fun j -> DecLaneEven (3 * j + 2))))));
		add (CycleNode1 i) (i * 8 + 5) plr_Even (lexp (((CycleNode2 i)::(mkli (2 * i + 1) (fun j -> DecLaneEven (3 * j + 1))))));
		add (CycleNode2 i) (i * 8 + 7) plr_Even (lexp (((CycleCenter i)::(mkli (2 * i + 2) (fun j -> DecLaneEven (3 * j))))));
		add (CycleCenter i) (i * 8 + 8) plr_Odd [CycleNode0 i; CycleLeaver i];
        add (CycleSelector i) (i * 8 + 9) plr_Even [CycleAccess i; UpperSelector i];
        add (CycleSelectorX i) (i * 8 + 10) plr_Even [CycleSelector i; UpperSelector i];
		add (UpperSelector i) (20 * n + 5 + 4 * i) plr_Even (lexp (FinalSink::(mkli (n - i - 1) (fun j -> CycleSelectorX (n - j - 1)))));
        add (CycleAccess i) (20 * n + 7 + 4 * i) plr_Odd [CycleCenter i];
		add (CycleLeaver i) (20 * n + 8 + 4 * i) plr_Odd [UpperSelector i];
	done;

	SymbolicParityGame.to_paritygame pg;;

let register _ =
    register_strat_impr_gen {
        ident = "switchbestsubexp";
        description = "Binary-Case Subexponential Lower Bound for (Schewe's) switch-best rule";
        parity_game = Some generator_game_func;
        generalized_mdp = None;
    }
