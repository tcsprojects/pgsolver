open Arg ;;
open Tcsargs;;
open Tcsbasedata;;
open Paritygame;;
open Stratimprgenerators;;

type gamenode = DecLaneEven of int (*a*)
			  | DecLaneOdd of int (*t*)
			  | DecLaneRoot (*c*)
			  | CycleNode of int * int (*d*)
			  | CycleCenter of int (*e*)
			  | CycleCenterBadEntry of int (*m*)
			  | CycleCenterBadEntryX of int (*q*)
			  | CycleNodeCho of int * int (*u*)
			  | CycleBadEntrySel of int * int (*v*)
			  | CycleBadEntrySelX of int * int (*w*)
			  | CycleAccess of int (*f*)
			  | CycleSelector of int (*g*)
			  | CycleLeaver of int (*h*)
			  | UpperSelector of int (*k*)
			  | FinalSink (*z*)
			  | FinalCycle (*x*)
			  | BitSelector (*r*)
			  | StartEven (*s*)

let symb_to_str = function DecLaneEven i -> "a" ^ string_of_int i | DecLaneOdd i -> "t" ^ string_of_int i | DecLaneRoot -> "c" | CycleNode (i,j) -> "d(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleCenter i -> "e" ^ string_of_int i | CycleCenterBadEntry i -> "m" ^ string_of_int i | CycleCenterBadEntryX i -> "q" ^ string_of_int i | CycleNodeCho (i,j) -> "u(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleBadEntrySel (i,j) -> "v(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleBadEntrySelX (i,j) -> "w(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | CycleAccess i -> "f" ^ string_of_int i | CycleSelector i -> "g" ^ string_of_int i | CycleLeaver i -> "h" ^ string_of_int i | UpperSelector i -> "k" ^ string_of_int i | FinalSink -> "z" | FinalCycle -> "x" | StartEven -> "s" | BitSelector -> "r"  

let mkli n f = (Array.to_list (Array.init n f))


let generator_game_func arguments =

	let n = ref None in
	let withfearnley = ref true in
	SimpleArgs.parsearr arguments
	                    [(["-nogadget"], Unit (fun _ -> withfearnley := false), "\n     disable fearnley gadgets")]
	                    (fun s -> n := Some (int_of_string s)) ("Options are")
	                    SimpleArgs.argprint_help SimpleArgs.argprint_bad;
	let n = OptionUtils.get_some !n in
	let withfearnley = !withfearnley in
	
	let pg = SymbolicParityGame.create_new FinalCycle in

	let add sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in

	add FinalCycle 1 plr_Odd [FinalCycle];
	add StartEven (10 * n + 6) plr_Even ( (FinalSink::(mkli n (fun j -> CycleAccess j))));
	add DecLaneRoot (10 * n + 4) plr_Even [StartEven; BitSelector];
	add BitSelector (10 * n + 8) plr_Even ( (FinalSink::(mkli n (fun j -> CycleSelector j))));
	add FinalSink (18 * n + 10) plr_Odd [FinalCycle];

	for i = 0 to 3 * n - 1 do
		add (DecLaneEven i) (4 * n + 2 * i + 4) plr_Odd [DecLaneOdd i];
		add (DecLaneOdd i) (4 * n + 2 * i + 3) plr_Even (if i = 0 then  [DecLaneRoot; BitSelector; StartEven] else  [DecLaneOdd (i - 1); BitSelector; StartEven])
	done;

	for i = 0 to n - 1 do
		if withfearnley then (
			for j = i + 1 to n - 1 do
				add (CycleNodeCho (i,j)) 3 plr_Even [CycleBadEntrySel (i,j); CycleBadEntrySelX (i,j)];
				add (CycleBadEntrySel (i,j)) 3 plr_Odd [CycleCenterBadEntry j; (if j = i+1 then CycleCenter i else CycleNodeCho (i,j-1))];
				add (CycleBadEntrySelX (i,j)) 2 plr_Odd [CycleCenterBadEntryX j; (if j = i+1 then CycleCenter i else CycleNodeCho (i,j-1))];
			done;
		);
		add (CycleNode (i,1)) (4 * i + 3) plr_Even ( ([StartEven; CycleNode (i,0)] @ (mkli (3 * i + 3) (fun j -> DecLaneEven j)) @ [BitSelector]));
		add (CycleNode (i,0)) (4 * i + 3) plr_Even ( ([StartEven; (if (i = n-1) || not withfearnley then CycleCenter i else CycleNodeCho (i,n-1))] @ (mkli (3 * i + 3) (fun j -> DecLaneEven j)) @ [BitSelector]));
		add (CycleCenter i) (4 * i + 4) plr_Odd [CycleNode (i,1); CycleLeaver i];
		if withfearnley then (
			add (CycleCenterBadEntry i) (10 * n + 9 + 2 * i) plr_Odd [CycleCenter i];
			add (CycleCenterBadEntryX i) (12 * n + 6 * i + 13) plr_Odd [CycleLeaver i];
		);
		add (CycleLeaver i) (12 * n + 6 * i + 14) plr_Odd [UpperSelector i];
		add (UpperSelector i) (12 * n + 6 * i + 9) plr_Even ((FinalSink::(mkli (n - i - 1) (fun j -> CycleSelector (n - j - 1)))));
        add (CycleAccess i) (12 * n + 6 * i + 11) plr_Odd [CycleCenter i];
        add (CycleSelector i) (4 * i + 6) plr_Even [CycleAccess i; UpperSelector i];
	done;

	SymbolicParityGame.to_paritygame pg;;

let register _ =
    register_strat_impr_gen {
        ident = "fearnleysubexp";
        description = "Subexponential Lower Bound for Fearnley's rule";
        parity_game = Some generator_game_func;
        generalized_mdp = None;
    }
