open Paritygame;;
open Stratimprgenerators;;
open Mdp;;

type gamenode =
	FinalCycle (*Z*)                      (*t*)
|	FinalSink (*Y*)                       (*-*)
|	PairSelector of int (*d*)             (*k*)
|	CycleEntry1 of int  (*h*) 
|	CycleEntry2 of int  (*h'*) 
|	CycleCenter1 of int (*E*)             (*A*)
|	CycleCenter2 of int (*E'*)            (*A'*)
|	CycleNodeX1 of int (*a*)              (*b*)
|	CycleNodeX2 of int (*a'*)             (*b'*)
|	CycleNodeY1 of int (*b*)              (*c*)
|	CycleNodeY2 of int (*b'*)             (*c'*)
|	UpDown1 of int (*g*)                  (*d*)
|	UpDown2 of int (*g'*)                 (*d'*)
|	UpperSelector1 of int (*c*)           (*h*)
|	UpperSelector2 of int (*c'*)          (*h'*)
|	Selector (*f*)                        (*s*)



let symb_to_str = function
	FinalCycle       -> "Z"
|	FinalSink        -> "Y"
|	PairSelector i   -> "d" ^ string_of_int i
|	CycleEntry1 i    -> "h" ^ string_of_int i
|	CycleEntry2 i    -> "k" ^ string_of_int i
|	CycleCenter1 i   -> "E" ^ string_of_int i
|	CycleCenter2 i   -> "X" ^ string_of_int i
|	CycleNodeX1 i    -> "a" ^ string_of_int i
|	CycleNodeX2 i    -> "w" ^ string_of_int i
|	CycleNodeY1 i    -> "b" ^ string_of_int i
|	CycleNodeY2 i    -> "v" ^ string_of_int i
|	UpperSelector1 i -> "c" ^ string_of_int i
|	UpperSelector2 i -> "u" ^ string_of_int i
|	Selector         -> "f"
|	UpDown1 i        -> "g" ^ string_of_int i
|	UpDown2 i        -> "s" ^ string_of_int i

let mkli n f = if n < 1 then [] else (Array.to_list (Array.init n f))

let mkli2 i n f = mkli (n - i) (fun j -> f (i + j))

let generator_game_func arguments =

    if (Array.length arguments != 1) then (failwith "specify index of game");
	let n = int_of_string arguments.(0) in

	let pg = SymbolicParityGame.create_new FinalCycle in

	let add	sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in
	
	let add1 sy pr li = add sy pr plr_Odd li in
	
	let add0 sy pr li = add sy pr plr_Even li in
	
	add1  FinalCycle 1 [FinalCycle];
	add1  FinalSink (2 * n + 14) [FinalCycle];
	add0  Selector 3 (FinalSink::mkli n (fun j -> PairSelector j));
	add1  (PairSelector n) (2 * n + 13) [FinalSink];
	
	for i = 0 to n - 1 do
		add0  (PairSelector i) (2 * i + 13)   (FinalSink::CycleCenter1 i::CycleCenter2 i::mkli n (fun j -> PairSelector j));
		add1  (CycleEntry1 i) 11              [CycleCenter1 i];
		add1  (CycleEntry2 i) 11              [CycleCenter2 i];
		add1  (CycleCenter1 i) 6  			  [CycleNodeX1 i; CycleNodeY1 i; UpDown1 i];
		add1  (CycleCenter2 i) 4  	 		  [CycleNodeX2 i; CycleNodeY2 i; UpDown2 i];
		add0  (CycleNodeX1 i)  3  	 		  (FinalSink::CycleCenter1 i::mkli n (fun j -> PairSelector j));
		add0  (CycleNodeX2 i)  3  	 		  (FinalSink::CycleCenter2 i::mkli n (fun j -> PairSelector j));
		add0  (CycleNodeY1 i)  3  	 		  (FinalSink::CycleCenter1 i::mkli n (fun j -> PairSelector j));
		add0  (CycleNodeY2 i)  3	 		  (FinalSink::CycleCenter2 i::mkli n (fun j -> PairSelector j));
		add0  (UpDown1 i)      10             [UpperSelector1 i; Selector];
		add0  (UpDown2 i)      8              [UpperSelector2 i; Selector];
		add0  (UpperSelector1 i) (2 * i + 14) (FinalSink::mkli2 (i+2) n (fun j -> PairSelector j));
		add0  (UpperSelector2 i) (2 * i + 14) [PairSelector (i+1)];
	done;

	SymbolicParityGame.to_paritygame pg;;


let generator_mdp_func arguments =
	let game = generator_game_func arguments in
	parity_game_to_generalized_mdp game 8 (fun _ j -> pg_get_priority game j >= 8);;

let register _ =
    register_strat_impr_gen {
        ident = "zadehsubexp";
        description = "Subexponential Lower Bound for Zadeh's rule";
        parity_game = Some generator_game_func;
        generalized_mdp = Some generator_mdp_func;
    }
