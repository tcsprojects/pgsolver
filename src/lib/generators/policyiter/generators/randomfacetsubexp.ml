open Arg ;;
open Tcsargs;;
open Tcsbasedata;;
open Paritygame;;
open Stratimprgenerators;;
open Mdp;;

type gamenode = CycleNode of int * int (*d*)
			  | CycleCenter of int (*e*)
			  | CycleEntry of int * int (*c*)
			  | AccessNode of int * int * int (*b*)
			  | AccessCenter of int * int (*a*)
			  | AccessEntry of int * int * int (*r*)
			  | LaneEntry of int * int (* g *)
			  | LaneEntryAccess of int * int (* g *)
			  | UpperNode of int (*k*)
			  | FinalCycle (*q*)
			  | FinalSink (* p *)
			  | Helper of int (*h*)

let symb_to_str = function CycleNode (i,j) -> "d(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | AccessNode (i,j,k) -> "b(" ^ string_of_int i ^ "," ^ string_of_int j ^ "," ^ string_of_int k ^ ")" | CycleCenter i -> "e" ^ string_of_int i | UpperNode i -> "k" ^ string_of_int i | AccessCenter (i, j) -> "a(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | FinalCycle -> "q" | FinalSink -> "p" | CycleEntry (i,j) -> "c(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | AccessEntry (i,j,k) -> "r(" ^ string_of_int i ^ "," ^ string_of_int j ^ "," ^ string_of_int k ^ ")" | LaneEntry (i,j) -> "c(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")" | Helper i -> "h" ^ string_of_int i | _ -> ""

let mkli n f = (Array.to_list (Array.init n f))

let mkli2 i n f = mkli (n - i) (fun j -> f (i + j))

let binlog n = int_of_float (ceil ((log (float_of_int n)) /. (log 2.)))


let generator_game_func arguments =

	  let bit_count = ref (-1) in
	  let bit_perimeter = ref (-1) in
	  let access_perimeter = ref (-1) in
	  let access_copies = ref (-1) in
	  let reset_copies = ref (-1) in
  	  let binary_cycle_nodes = ref false in
	  let binary_access_nodes = ref false in
	  let binary_uplink_nodes = ref false in

	  let speclist =  [(["-bp"], Int(fun i -> bit_perimeter := i),
						  "\n     set bit perimeter manually") ;
					   (["-ap"], Int(fun i -> access_perimeter := i),
						  "\n     set access perimeter manually") ;
					   (["-ac"], Int(fun i -> access_copies := i),
						  "\n     set access copies manually") ;
					   (["-re"], Int(fun i -> reset_copies := i),
						  "\n     set reset copies manually") ;
					   (["-bcn"], Unit(fun _ -> binary_cycle_nodes := true),
						  "\n     make cycle nodes binary") ;
					   (["-ban"], Unit(fun _ -> binary_access_nodes := true),
						  "\n     make access nodes binary") ;
					   (["-bun"], Unit(fun _ -> binary_uplink_nodes := true),
						  "\n     make uplink nodes binary") ;
		] in

	  
	SimpleArgs.parsearr arguments speclist (fun n -> bit_count := int_of_string n) ("Options are") SimpleArgs.argprint_help SimpleArgs.argprint_bad;

    let n = if !bit_count < 0 then failwith "Index not specified." else !bit_count in
    
    let d = if !access_perimeter > 0 then !access_perimeter else binlog n in
    
    let cd = if !bit_perimeter > 0 then !bit_perimeter else binlog n * d in
    
    let m = if !access_copies > 0 then !access_copies else binlog n in
    
    let r = if !reset_copies > 0 then !reset_copies else binlog n in

	let d = if !binary_access_nodes then r * d else d in
	let r_d = if !binary_cycle_nodes then 1 else r in
	let cd = if !binary_cycle_nodes then r * cd else cd in
	let r_cd = if !binary_cycle_nodes then 1 else r in
	
	let pg = SymbolicParityGame.create_new FinalCycle in

	let addnode sy pr pl li = SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy)) in
	
	let helpercnt = ref 0 in
	let addhelper pr pl li =
		incr helpercnt;
		addnode (Helper !helpercnt) pr pl li;
		Helper !helpercnt
	in
	
	let rec divide = function
		[] -> ([], [])
	|	[x] -> ([x], [])
	|	x::y::r -> let (a,b) = divide r in
				   (x::a,y::b)
	in
	
	let rec make_tree = function
		[] -> failwith "impossible"
	|	[x] -> x
	|	[x;y] -> addhelper 3 plr_Even [x;y]
	|	li -> let (left, right) = divide li in
	          addhelper 3 plr_Even [make_tree left; make_tree right]
	in
	
	addnode FinalCycle 1 plr_Odd [FinalCycle];
	addnode FinalSink (6 + 2 * n) plr_Odd [FinalCycle];

	for i = 0 to n - 1 do		
		addnode (CycleCenter i) 2 plr_Odd (UpperNode i::(mkli cd (fun j -> CycleNode (i, j))));
		
		if i = n - 1 then (
			addnode (UpperNode i) (6 + 2 * i) plr_Even [FinalSink];
		) else if not !binary_uplink_nodes then (
			addnode (UpperNode i) (6 + 2 * i) plr_Even (List.flatten (mkli m (fun k -> mkli r (fun s -> AccessEntry (i+1,k,s)))));
		) else (
			addnode (UpperNode i) (6 + 2 * i) plr_Even [make_tree (mkli r (fun s -> make_tree (mkli m (fun k -> AccessEntry (i+1,k,s)))))];
		);
		
		for j = 0 to cd - 1 do
            addnode (CycleNode (i,j)) 0 plr_Even (FinalSink::CycleCenter i::mkli r_cd (fun s -> LaneEntry (i,s)))
		done;
		for k = 0 to m - 1 do
			addnode (AccessCenter (i,k)) 2 plr_Odd (CycleEntry (i,0)::(mkli d (fun j -> AccessNode (i,k,j))));
			for j = 0 to d - 1 do
	            addnode (AccessNode (i,k,j)) 0 plr_Even (FinalSink::AccessCenter (i,k)::mkli r_d (fun s -> LaneEntryAccess (i,s)))
			done;
			for s = 0 to r - 1 do
				addnode (AccessEntry (i,k,s)) 0 plr_Odd [AccessCenter (i,k)]
			done
		done;		
		for s = 0 to r - 1 do
			addnode (LaneEntry (i,s)) 0 plr_Odd [if i = n - 1 then FinalSink else CycleNode (i+1,0)];
			addnode (LaneEntryAccess (i,s)) 4 plr_Odd [if i = n - 1 then FinalSink else CycleNode (i+1,0)];
			addnode (CycleEntry (i,s)) 2 plr_Odd [CycleCenter i];
		done;
	done;

	SymbolicParityGame.to_paritygame pg;;


let generator_mdp_func arguments =
	let game = generator_game_func arguments in
	parity_game_to_generalized_mdp game 4 (fun _ j -> pg_get_priority game j >= 2);;


let register _ =
    register_strat_impr_gen {
        ident = "randomfacetsubexp";
        description = "Subexponential Lower Bound for the Random Facet rule";
        parity_game = Some generator_game_func;
        generalized_mdp = Some generator_mdp_func;
    }
