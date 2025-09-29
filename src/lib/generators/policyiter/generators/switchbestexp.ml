open Paritygame;;
open Stratimprgenerators;;

type gamenode = DecLaneEven of int (*a*)
			  | DecLaneOdd of int (*b*)
			  | DecLaneOddRoot (*d*)
			  | CycleNode0 of int (*e*)
			  | CycleNode1 of int (*f*)
			  | CycleNode2 of int (*g*)
			  | CycleNodeToLane0 of int (*e'*)
			  | CycleNodeToLane1 of int (*f'*)
			  | CycleNodeToLane2 of int (*g'*)  
			  | CycleCenter of int (*h*)
			  | CycleAccessDistributer (* c *)
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

let symb_to_str = function DecLaneEven i ->"a" ^ string_of_int i | DecLaneOdd i ->"b" ^ string_of_int i | DecLaneOddRoot ->"d" | CycleNode0 i ->"e" ^ string_of_int i | CycleNode1 i ->"f" ^ string_of_int i | CycleNode2 i ->"g" ^ string_of_int i | CycleNodeToLane0 i ->"e'" ^ string_of_int i | CycleNodeToLane1 i ->"f'" ^ string_of_int i | CycleNodeToLane2 i ->"g'" ^ string_of_int i | CycleCenter i ->"h" ^ string_of_int i | CycleAccessDistributer ->"c" | CycleAccess i ->"k" ^ string_of_int i | CycleSelector i ->"l" ^ string_of_int i | CycleSelectorDecelerator i ->"r" ^ string_of_int i | CycleLeaver i ->"m" ^ string_of_int i | UpperSelector i ->"z" ^ string_of_int i | UpperSelectorHelper i -> "v" ^ string_of_int i | FinalSink ->"p" | FinalCycle ->"q" | StartEven ->"s" | StartSelector ->"t" | HighEntryBit ->"w" | LowestBit ->"y" | BitSelector ->"x"

let mkli n f = (Array.to_list (Array.init n f))


let generator_game_func arguments = 

    if (Array.length arguments != 1) then (failwith "specify index of game");
	let n = int_of_string arguments.(0) in

	let pg = SymbolicParityGame.create_new FinalCycle in

	let add sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in

	add FinalCycle 1 plr_Odd [FinalCycle];
	add StartEven 2 plr_Even [FinalSink; CycleAccessDistributer];
	add StartSelector 3 plr_Even [BitSelector; StartEven];
	add CycleAccessDistributer 4 plr_Even (mkli n (fun j -> CycleAccess j));
	add LowestBit 5 plr_Even [HighEntryBit; CycleSelector 0];
	add HighEntryBit 7 plr_Even (LowestBit::FinalSink::(mkli (n - 1) (fun j -> CycleSelector (j + 1))));
	add DecLaneOddRoot (24 * n + 12) plr_Even [StartSelector; BitSelector];
	add BitSelector (24 * n + 14) plr_Even [HighEntryBit; LowestBit];
	add FinalSink (26 * n + 16) plr_Odd [FinalCycle];

	for i = 0 to 6 * n do
		add (DecLaneEven i) (12 * n + 2 * i + 10) plr_Odd [DecLaneOdd i];
		add (DecLaneOdd i) (12 * n + 2 * i + 9) plr_Even [(if i < 2 then DecLaneOddRoot else DecLaneOdd (i - 1)); BitSelector; StartEven]
	done;
	
	for i = 0 to n - 1 do
		add (CycleNode0 i) (6 * n + 6 * i + 9) plr_Even [CycleNode1 i; DecLaneOddRoot; CycleNodeToLane0 i];
		add (CycleNode1 i) (6 * n + 6 * i + 11) plr_Even [CycleNode2 i; CycleAccessDistributer; CycleNodeToLane1 i; DecLaneEven (6 * i + 4)];
		add (CycleNode2 i) (6 * n + 6 * i + 13) plr_Even [DecLaneEven 0; CycleCenter i; CycleNodeToLane2 i];
		add (CycleNodeToLane0 i) (6 * i + 8) plr_Even ((if i = 0 then [] else [CycleNodeToLane0 (i-1)]) @ [DecLaneOddRoot; DecLaneEven (6 * i + 2); DecLaneEven (6 * i + 5)]);
		add (CycleNodeToLane1 i) (6 * i + 10) plr_Even ((if i = 0 then [] else [CycleNodeToLane1 (i-1)]) @ [DecLaneOddRoot; DecLaneEven (6 * i + 1)] @ (if i < n - 1 then [DecLaneEven (6 * i + 4)] else []));
		add (CycleNodeToLane2 i) (6 * i + 12) plr_Even ((if i = 0 then [] else [CycleNodeToLane2 (i-1)]) @ [DecLaneOddRoot; DecLaneEven (6 * i + 3); DecLaneEven (6 * i + 6)]);
		add (CycleCenter i) (6 * n + 6 * i + 14) plr_Odd [CycleNode0 i; CycleLeaver i];
		add (CycleLeaver i) (24 * n + 4 * i + 18) plr_Odd [UpperSelector i];
		add (UpperSelector i) (24 * n + 4 * i + 15) plr_Even (if i = n - 1 then [FinalSink] else [FinalSink; UpperSelectorHelper i]);
        add (CycleAccess i) (24 * n + 4 * i + 17) plr_Odd [CycleCenter i];
        add (CycleSelector i) (6 * i + 11) plr_Even [CycleAccess i; UpperSelector i];
        add (CycleSelectorDecelerator i) (6 * i + 13) plr_Even [CycleSelector i; UpperSelector i];
		if i < n - 1 then add (UpperSelectorHelper i) (6 * i + 9) plr_Even (if i = n - 2 then [FinalSink; CycleSelectorDecelerator (n - 1)] else [UpperSelectorHelper (i+1); CycleSelectorDecelerator (i + 1)])
	done;

	SymbolicParityGame.to_paritygame pg;;

let register _ =
    register_strat_impr_gen {
        ident = "switchbestexp";
        description = "Exponential Lower Bound for (Voege's) switch-all rule";
        parity_game = Some generator_game_func;
        generalized_mdp = None;
    }
