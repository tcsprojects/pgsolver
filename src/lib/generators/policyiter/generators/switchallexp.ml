open Paritygame;;
open Stratimprgenerators;;

type gamenode = DecLaneEven of int (*a*)
			  | DecLaneOdd of int (*b*)
			  | DecLaneOddRoot (*d*)
			  | CycleNode of int (*e*)
			  | CycleToLaneEven of int (*f*)
			  | CycleToLaneOdd of int (*g*)
			  | CycleCenter of int (*h*)
			  | CycleAccess of int (*k*)
			  | CycleSelector of int (*l*)
			  | CycleSelectorDecelerator of int (*r*)
			  | CycleLeaver of int (*m*)
			  | UpperSelector of int (*z*)
			  | UpperSelectorHelper of int (*v*)
			  | FinalSink (*p*)
			  | FinalCycle (*q*)
			  | StartEven (*s*)
			  | StartSelector (*t*)
			  | HighEntryBit (*w*)
			  | LowestBit (*y*)
			  | BitSelector (*x*)

let symb_to_str = function DecLaneEven i ->"a" ^ string_of_int i | DecLaneOdd i ->"b" ^ string_of_int i | DecLaneOddRoot ->"d" | CycleNode i ->"e" ^ string_of_int i | CycleToLaneEven i ->"f" ^ string_of_int i | CycleToLaneOdd i ->"g" ^ string_of_int i | CycleCenter i ->"h" ^ string_of_int i | CycleAccess i ->"k" ^ string_of_int i | CycleSelector i ->"l" ^ string_of_int i | CycleSelectorDecelerator i ->"r" ^ string_of_int i | CycleLeaver i ->"m" ^ string_of_int i | UpperSelector i ->"z" ^ string_of_int i | UpperSelectorHelper i -> "v" ^ string_of_int i | FinalSink ->"p" | FinalCycle ->"q" | StartEven ->"s" | StartSelector ->"t" | HighEntryBit ->"w" | LowestBit ->"y" | BitSelector ->"x"

let mkli n f = (Array.to_list (Array.init n f))


let generator_game_func arguments = 

    if (Array.length arguments != 1) then (failwith "specify index of game");
	let n = int_of_string arguments.(0) in

	let pg = SymbolicParityGame.create_new FinalCycle in

	let add sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in

	add FinalCycle 1 plr_Odd [FinalCycle];
	add StartEven 2 plr_Even (FinalSink::(mkli n (fun j -> CycleAccess j)));
	add StartSelector 3 plr_Even [BitSelector; StartEven];
	add LowestBit 5 plr_Even [HighEntryBit; CycleSelector 0];
	add HighEntryBit 7 plr_Even (LowestBit::FinalSink::(mkli (n - 1) (fun j -> CycleSelector (j + 1)))); (***)
	add DecLaneOddRoot (12 * n + 8) plr_Even [StartSelector; BitSelector];
	add BitSelector (12 * n + 10) plr_Even [HighEntryBit; LowestBit];
	add FinalSink (16 * n + 12) plr_Odd [FinalCycle];

	for i = 0 to 2 * n - 1 do
		add (DecLaneEven i) (8 * n + 2 * i + 8) plr_Odd [DecLaneOdd i];
		add (DecLaneOdd i) (8 * n + 2 * i + 7) plr_Even [(if i < 2 then DecLaneOddRoot else DecLaneOdd (i - 1)); BitSelector; StartEven]
	done;
	
	for i = 0 to n - 1 do
		add (CycleNode i) (6 * n + 2 * i + 7) plr_Even [DecLaneEven 0; BitSelector; StartEven; CycleCenter i; CycleToLaneEven i; CycleToLaneOdd i]; (***)
		add (CycleToLaneEven i) (6 * i + 8) plr_Even ((if i = 0 then [] else [CycleToLaneEven (i-1)]) @ [DecLaneOddRoot; DecLaneEven (2 * i)]); (***)
		add (CycleToLaneOdd i) (6 * i + 10) plr_Even ((if i = 0 then [] else [CycleToLaneOdd (i-1)]) @ [DecLaneOddRoot; DecLaneEven (2 * i + 1)]); (***)
		add (CycleCenter i) (6 * n + 2 * i + 8) plr_Odd [CycleNode i; CycleLeaver i];
		add (CycleLeaver i) (12 * n + 4 * i + 14) plr_Odd [UpperSelector i];
		add (UpperSelector i) (12 * n + 4 * i + 11) plr_Even (if i = n - 1 then [FinalSink] else [FinalSink; UpperSelectorHelper i]);
        add (CycleAccess i) (12 * n + 4 * i + 13) plr_Odd [CycleCenter i];
        add (CycleSelector i) (6 * i + 11) plr_Even [CycleAccess i; UpperSelector i];
        add (CycleSelectorDecelerator i) (6 * i + 12) plr_Even [CycleSelector i; UpperSelector i];
		if i < n - 1 then add (UpperSelectorHelper i) (6 * i + 9) plr_Even (if i = n - 2 then [FinalSink; CycleSelectorDecelerator (n - 1)] else [UpperSelectorHelper (i+1); CycleSelectorDecelerator (i + 1)])
	done;

	SymbolicParityGame.to_paritygame pg;;


let register _ =
    register_strat_impr_gen {
        ident = "switchallexp";
        description = "Exponential Lower Bound for (Voege's) switch-all rule";
        parity_game = Some generator_game_func;
        generalized_mdp = None;
    }
