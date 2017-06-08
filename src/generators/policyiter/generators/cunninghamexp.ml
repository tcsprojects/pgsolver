open Paritygame;;
open Stratimprgenerators;;
open Mdp;;
open Arg;;
open Tcsargs;;

let auso_compat = ref false
let index_one = ref false
let bits = ref 0
let mapindex i = string_of_int (if !index_one then i + 1 else i)

type gamenode =
	FinalCycle
|	CycleExit of int
|	UpperSelector of int
|	CycleSelector of int 
|	CycleCenter of int
|	CycleNodePaced of int
|	CycleNodeLaned of int
|	CycleEntry of int
|	PacerNode of int
|	PacerEntry of int
|   AusoHelper of int

let symb_to_str = function 
|	CycleCenter i -> "x" ^ mapindex i
|	CycleNodePaced i -> "e" ^ mapindex i
|	CycleNodeLaned i -> "d" ^ mapindex i
|	PacerNode i -> "c" ^ mapindex i
|	CycleExit i -> "y" ^ mapindex i
|	CycleEntry i -> "q" ^ mapindex i
|	CycleSelector i -> "b" ^ mapindex i
|	UpperSelector i -> "a" ^ mapindex i
|	FinalCycle -> "t"
|	PacerEntry i -> "p" ^ mapindex i
|   AusoHelper i -> "z" ^ mapindex i

let generator_game_func arguments =

  	  let speclist =  [(["-tiefree"], Unit(fun _ -> auso_compat := true),
						  "\n     make game tie-free") ;
					   (["-indexone"], Unit(fun _ -> index_one := true),
                            "\n     start with index one") ] in

	SimpleArgs.parsearr arguments speclist (fun n -> bits := int_of_string n) ("Options are") SimpleArgs.argprint_help SimpleArgs.argprint_bad;
    let n = !bits in
    let auso_compat = !auso_compat in

    let pg = SymbolicParityGame.create_new FinalCycle in

	let addnode sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in
    
    if (*auso_compat*) true then (
        let touchnode sy = SymbolicParityGame.touch_node pg sy in
  	
        for i = 0 to n - 1 do
     		if i > 0 then touchnode (UpperSelector i);
   		done;
  		for i = 0 to n - 1 do
     		if i > 0 then touchnode (CycleSelector i);
   		done;
  		for i = 0 to n - 1 do
     		touchnode (PacerNode i);
   		done;
        for i = 0 to n - 1 do
     		if i > 0 then touchnode (CycleNodeLaned i);
  	    done;
  	    for i = 0 to n - 1 do
     		touchnode (CycleNodePaced i);
   	    done;
    );
  
	addnode FinalCycle 1 plr_Odd [FinalCycle];
	addnode (UpperSelector n) 3 plr_Odd [FinalCycle];
	addnode (PacerEntry 0) 8 plr_Odd [if auso_compat then AusoHelper 0 else PacerNode (n-1)];
	addnode (PacerEntry 1) 3 plr_Odd [PacerEntry 0];
   
	for i = 0 to n - 1 do
        if auso_compat then addnode (AusoHelper i) 3 plr_Odd [if i = n-1 then PacerNode (n-1) else AusoHelper (i+1)];
	
        addnode (CycleExit i) (2 * i + 10) plr_Odd [UpperSelector (i + 1)];
		addnode (CycleEntry i) (2 * i + 9) plr_Odd [CycleCenter i];
		addnode (CycleCenter i) 6 plr_Odd (if i > 0 then [CycleNodePaced i; CycleNodeLaned i; CycleExit i] else [CycleNodePaced i; CycleExit i]);
		addnode (CycleCenter (i+n)) 5 plr_Odd [CycleCenter i];
		if i < n - 1 then addnode (UpperSelector (i+1)) 3 plr_Even [UpperSelector (i+2); CycleEntry (i+1)];
		if i < n - 1 then addnode (CycleSelector (i+1)) 3 plr_Even (if i > 0 then [CycleSelector i; CycleEntry i] else [CycleEntry 0]);
		addnode (PacerNode i) 3 plr_Even (if i = 0 then [CycleEntry 0] else [PacerNode (i-1); CycleEntry i]);
		addnode (CycleNodePaced i) 5 plr_Even [(if i = 0 then PacerEntry 1 else PacerEntry 0); CycleCenter i];
		if i > 0 then addnode (CycleNodeLaned i) 5 plr_Even [CycleSelector i; CycleCenter (i+n)];
	done;

	SymbolicParityGame.to_paritygame pg;;

let generator_mdp_func arguments =
	let game = generator_game_func arguments in
        parity_game_to_generalized_mdp game 8 (fun _ j -> pg_get_priority game j >= 8);;

let register _ =
    register_strat_impr_gen {
        ident = "cunninghamexp";
        description = "Binary-Case Exponential Lower Bound for Cunningham's rule";
        parity_game = Some generator_game_func;
        generalized_mdp = Some generator_mdp_func;
    }
