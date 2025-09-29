open Paritygame;;
open Stratimprgenerators;;
open Mdp;;

type gamenode =
	FinalCycle (*Z*)                      (*t*)
|	FinalSink (*Y*)                       (*-*)
|	PairSelector of int (*d*)             (*k*)
|	CycleCenter1 of int (*E*)             (*A*)
|	CycleCenter2 of int (*E'*)            (*A'*)
|	CycleNodeX1 of int (*a*)              (*b*)
|	CycleNodeX2 of int (*a'*)             (*b'*)
|	CycleNodeY1 of int (*b*)              (*c*)
|	CycleNodeY2 of int (*b'*)             (*c'*)
|	CycleNodeX1T of int (*a*)              (*b*)
|	CycleNodeX2T of int (*a'*)             (*b'*)
|	CycleNodeY1T of int (*b*)              (*c*)
|	CycleNodeY2T of int (*b'*)             (*c'*)
|	UpDown1 of int (*g*)                  (*d*)
|	UpDown2 of int (*g'*)                 (*d'*)
|	UpperSelector1 of int (*c*)           (*h*)
|	UpperSelector2 of int (*c'*)          (*h'*)
|	Selector of int (*f*)                        (*s*)
|   Helper of int



let symb_to_str = function
	FinalCycle       -> "Z"
|	FinalSink        -> "Y"
|	PairSelector i   -> "d" ^ string_of_int i
|	CycleCenter1 i   -> "E" ^ string_of_int i
|	CycleCenter2 i   -> "X" ^ string_of_int i
|	CycleNodeX1 i    -> "a" ^ string_of_int i
|	CycleNodeX2 i    -> "w" ^ string_of_int i
|	CycleNodeY1 i    -> "b" ^ string_of_int i
|	CycleNodeY2 i    -> "v" ^ string_of_int i
|	CycleNodeX1T i    -> "o" ^ string_of_int i
|	CycleNodeX2T i    -> "q" ^ string_of_int i
|	CycleNodeY1T i    -> "p" ^ string_of_int i
|	CycleNodeY2T i    -> "r" ^ string_of_int i
|	UpperSelector1 i -> "c" ^ string_of_int i
|	UpperSelector2 i -> "z" ^ string_of_int i
|	Selector i        -> "m" ^ string_of_int i
|	UpDown1 i        -> "g" ^ string_of_int i
|	UpDown2 i        -> "s" ^ string_of_int i
|   Helper i         -> "u" ^ string_of_int i
  

let generator_game_func arguments =

    if (Array.length arguments != 1) then (failwith "specify index of game");
	let n = int_of_string arguments.(0) in

	let pg = SymbolicParityGame.create_new FinalCycle in

	let add	sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in
	
	let add1 sy pr li = add sy pr plr_Odd li in
	
	let add0 sy pr li = add sy pr plr_Even li in
	
	add1  FinalCycle 1 [FinalCycle];
	add1  FinalSink (2 * n + 12) [FinalCycle];
(*	add1  (PairSelector n) (2 * n + 11) [FinalSink];*)
	
	for i = 0 to n - 1 do
		add0  (CycleNodeX1 i)  3  	 		                  [CycleCenter1 i; CycleNodeX1T i];
		add0  (CycleNodeX1T i)  3  	 		                  [PairSelector 0; Selector 1];
		if i < n-1 then add0  (CycleNodeX2 i)  3  	 		  [CycleCenter2 i;CycleNodeX2T i];
		if i < n-1 then add0  (CycleNodeX2T i)  3  	 		  [PairSelector 0;Selector 1];
		add0  (CycleNodeY1 i)  3  	 		                  [CycleCenter1 i; CycleNodeY1T i];
		add0  (CycleNodeY1T i)  3  	 		                  [PairSelector 0; Selector 1];
		if i < n-1 then add0  (CycleNodeY2 i)  3	 		    [CycleCenter2 i; CycleNodeY2T i];
		if i < n-1 then add0  (CycleNodeY2T i)  3	 		    [PairSelector 0; Selector 1];

		add0  (PairSelector i) (2 * i + 11)   (if i < n-1 then [CycleCenter1 i; CycleCenter2 i] else [CycleCenter1 i]);
		add0  (Selector i) 3 [PairSelector i; (if i < n - 1 then Selector (i+1) else FinalSink)];
		add1  (CycleCenter1 i) 6  			  [CycleNodeX1 i; CycleNodeY1 i; UpDown1 i];
		if i < n-1 then add1  (CycleCenter2 i) 4  	 		  [CycleNodeX2 i; CycleNodeY2 i; UpDown2 i];
		add0  (UpDown1 i)      10             [UpperSelector1 i; Selector 0];
		if i < n-1 then add0  (UpDown2 i)      8              [Helper i; Selector 0];
		add0  (UpperSelector1 i) (2 * i + 12) [if i < n - 2 then Selector (i+2) else FinalSink];
		if i < n-1 then (
    	    add1  (Helper i) 5 [UpperSelector2 i];
    		add0  (UpperSelector2 i) (2 * i + 12) [PairSelector (i+1)];
        );
	done;

	SymbolicParityGame.to_paritygame pg;;


let generator_mdp_func arguments =
	let game = generator_game_func arguments in
	parity_game_to_generalized_mdp game 8 (fun _ j -> pg_get_priority game j >= 8);;

let register _ =
    register_strat_impr_gen {
        ident = "zadehexp";
        description = "Exponential Lower Bound for Zadeh's rule";
        parity_game = Some generator_game_func;
        generalized_mdp = Some generator_mdp_func;
    }
