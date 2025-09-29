open Arg ;;
open Tcsargs;;
open Tcsbasedata;;
open Paritygame;;
open Stratimprgenerators;;
open Mdp;;

type gamenode =
	FinalCycle
|	FinalSink
|	LowSel
|	CycleExit of int
|	UpperSelector of int
|	CycleSelector of int
|	CycleCenter of int
|	CycleNode of int * int
|	CycleEntry of int

let symb_to_str = function 
|	CycleCenter i -> "e" ^ string_of_int i
|	CycleNode (i,k) -> "d(" ^ string_of_int i ^ "," ^ string_of_int k ^ ")"
|	CycleExit i -> "y" ^ string_of_int i
|	CycleEntry i -> "q" ^ string_of_int i
|	CycleSelector i -> "u" ^ string_of_int i
|	UpperSelector i -> "w" ^ string_of_int i
|	FinalSink -> "s"
|	FinalCycle -> "t"
|	LowSel -> "p"

let generator_game_func arguments =
		
	let n = ref None in
	let binary = ref false in
	SimpleArgs.parsearr arguments
	                    [(["-binary"], Unit (fun _ -> binary := true), "\n     binary-case")]
	                    (fun s -> n := Some (int_of_string s)) ("Options are")
	                    SimpleArgs.argprint_help SimpleArgs.argprint_bad;
	let n = OptionUtils.get_some !n in
	let binary = !binary in

	let pg = SymbolicParityGame.create_new FinalCycle in

	let addnode sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in
	
	addnode FinalCycle 1 plr_Odd [FinalCycle];
	addnode FinalSink (2 * n + 10) plr_Odd [FinalCycle];
	addnode (UpperSelector n) 3 plr_Even [FinalSink];
	addnode (CycleSelector n) 3 plr_Even [FinalSink];
	addnode LowSel 8 plr_Odd [CycleEntry 0];
	
	for i = 0 to n - 1 do
		addnode (CycleExit i) (2 * i + 10) plr_Odd [UpperSelector (i + 1)];
		addnode (CycleEntry i) (2 * i + 9) plr_Odd [CycleCenter i];
		addnode (CycleCenter i) 6 plr_Odd [CycleNode (i,0); CycleExit i];
		addnode (UpperSelector i) 3 plr_Even [UpperSelector (i+1); CycleEntry i];
		addnode (CycleSelector i) 3 plr_Even [CycleSelector (i+1); CycleEntry i];
		if binary then
			for j = 0 to 2 * i + 1 do
				addnode (CycleNode (i,j)) 5 plr_Even [FinalSink; (if j mod 2 = 0 then LowSel else CycleSelector 0);
				                                          (if j < 2 * i + 1 then CycleNode (i,j+1) else CycleCenter i)]
			done
		else
			for j = 0 to i do
				addnode (CycleNode (i,j)) 5 plr_Even [CycleSelector 0; FinalSink; LowSel; (if j < i then CycleNode (i,j+1) else CycleCenter i)]
			done;
	done;

	SymbolicParityGame.to_paritygame pg;;

let generator_mdp_func arguments =
	let game = generator_game_func arguments in
	parity_game_to_generalized_mdp game 8 (fun _ j -> pg_get_priority game j >= 8);;

let register _ =
    register_strat_impr_gen {
        ident = "cunninghamsubexp";
        description = "Binary Subexponential Lower Bound for Cunningham's rule";
        parity_game = Some generator_game_func;
        generalized_mdp = Some generator_mdp_func;
    }
