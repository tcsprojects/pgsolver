open Arg ;;
open Tcsargs;;
open Tcsbasedata;;
open Paritygame;;
open Stratimprgenerators;;
open Tcsmaths;;
open Tcsstrings;;
open Tcsarray;;

let n = ref 0
let accesslen = ref 2
let pacedlen = ref 6
let cyclelen = ref 8

type gamenode =
	Sink (*T*)
|	SinkEntry (*t*)
|	PacerExit (*L*)
|	SuccessorLane of int (*c*)
|	SelectorLane of int (*q*)
|	SelectorHighEntry (*Q*)
|	SelectorLowEntry of int (*G*)
|	AccessCenter of int (*A*)
|	AccessNode of int * int (*a*)
|	AccessLeaver of int (*K*)
|	CountingEntry of int (*H*)
|	CountingCenter of int (*B*)
|	CountingLowNode of int * int (*b*)
|	CountingLowLeaver of int * int (*M*)
|	CountingLowLane of int * int (*P*)
|	CountingExit of int (*C*)
|	PacedNode of int * int (*z*)
|	CountingHighNode of int (*f*)
|	CountingHighLeaver of int (*F*)
|	CountingHighLane of int (*W*)

let symb_to_str = function 
	Sink -> "T"
|	SinkEntry -> "t"
|	PacerExit -> "L"
|	SuccessorLane i -> "c" ^ string_of_int i
|	SelectorLane i -> "q" ^ string_of_int i
|	SelectorHighEntry -> "Q"
|	SelectorLowEntry i -> "G" ^ string_of_int i
|	AccessCenter i -> "A" ^ string_of_int i
|	AccessNode (i,j) -> "a(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")"
|	AccessLeaver i -> "K" ^ string_of_int i
|	CountingEntry i -> "H" ^ string_of_int i
|	CountingCenter i -> "e" ^ string_of_int i
|	CountingLowNode (i,k) -> "d(" ^ string_of_int i ^ "," ^ string_of_int 0 ^ "," ^ string_of_int k ^ ")"
|	CountingLowLeaver (i,j) -> "M(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")"
|	CountingLowLane (i,j) -> "P(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")"
|	CountingExit i -> "C" ^ string_of_int i
|	PacedNode (i,k) -> "d(" ^ string_of_int i ^ "," ^ string_of_int 1 ^ "," ^ string_of_int k ^ ")"
|	CountingHighNode i -> "d(" ^ string_of_int i ^ "," ^ string_of_int 0 ^ "," ^ string_of_int (!cyclelen) ^ ")"
|	CountingHighLeaver i -> "F" ^ string_of_int i
|	CountingHighLane i -> "W" ^ string_of_int i

let mkli n f = if n < 1 then [] else (Array.to_list (Array.init n f))

let mkli2 i n f = mkli (n - i) (fun j -> f (i + j))




let mystrat s n node =
	let a = Array.of_list (List.map (fun l -> Array.map (fun c -> c='1') (StringUtils.to_char_array l)) (StringUtils.explode s '|')) in
	let b = Array.init n (fun i -> ArrayUtils.forall a.(i) (fun _ x -> x)) in
	match node with
		SuccessorLane i -> if i = n then SinkEntry else (if b.(i) then AccessCenter i else SuccessorLane (i+1))
	|	SelectorLane i -> if i = n then SinkEntry else (if b.(i) then CountingEntry i else SelectorLane (i+1))
	|	AccessNode (i,0) -> if b.(i) then AccessCenter i else SinkEntry
	|	AccessNode (i,j) -> if b.(i) then AccessNode (i, j-1) else SinkEntry
	|	PacedNode (i,0) -> if b.(i) then CountingCenter i else PacerExit
	|	PacedNode (i,j) -> if b.(i) then PacedNode (i, j-1) else PacerExit
	|	CountingLowNode (i,0) -> if a.(i).(0) then CountingCenter i else CountingLowLeaver (i,0)
	|	CountingLowNode (i,j) -> if a.(i).(j) then CountingLowNode(i, j-1) else CountingLowLeaver (i,j)
	|	CountingHighNode i -> if a.(i).(!cyclelen) then CountingLowNode (i, !cyclelen - 1) else CountingHighLeaver i
	|	_ -> SinkEntry



let generator_game_func arguments = 

	let strat = ref None in

	SimpleArgs.parsearr arguments
	                     [(["-cl"], Int(fun i -> cyclelen := i),
						  "\n     set cycle len manually") ;
						 (["-al"], Int(fun i -> accesslen := i),
						  "\n     set access len manually") ;
						 (["-pl"], Int(fun i -> pacedlen := i),
						  "\n     set paced len manually") ;
						  (["-int"], String(fun s -> strat := Some (mystrat s)),
                          "\n     internal use")
					    ]
					  (fun s ->
						n := int_of_string s;
						accesslen := !accesslen * !n;
						pacedlen := !pacedlen * !n;
						cyclelen := !cyclelen * !n)
					  ("Options are") SimpleArgs.argprint_help SimpleArgs.argprint_bad;

    let n = !n in
	
	let uu = !accesslen in
	let ss = !pacedlen in
	let tt = !cyclelen in
    		  
	let pg = SymbolicParityGame.create_new Sink in
	

	let add sy pr pl li = 
		match (!strat, pl) with
			(Some str, plr_Even) ->
				SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy ^ "[" ^ symb_to_str (str n sy) ^ "]"))
		|	_ ->
				SymbolicParityGame.add_node pg sy pr pl (Array.of_list li) (Some (symb_to_str sy))
	in
	
	add Sink 1 plr_Odd [Sink];
	add SinkEntry (2 * tt * n + 6 * n + 12) plr_Odd [Sink];
	
	add PacerExit (2 * tt * n + 2 * n + 10) plr_Odd [SuccessorLane 0];
	
	add SelectorHighEntry (2 * tt * n + 2 * n + 8) plr_Odd [SelectorLane 0];
	
	for i = 0 to n do
		add (SuccessorLane i) 3 plr_Even (if i = n then [SinkEntry] else [AccessCenter i; SuccessorLane (i+1)]);
		add (SelectorLane i) 3 plr_Even (if i = n then [SinkEntry] else [CountingEntry i; SelectorLane (i+1)]);
	done;
	
	for i = 0 to n - 1 do
		add (SelectorLowEntry i) (2 * tt * n + 2 * i + 8) plr_Odd [SelectorLane 0];
		add (AccessCenter i) 6 plr_Odd [CountingEntry i; AccessNode (i, uu - 1)];
		
		for j = 0 to uu - 1 do
			add (AccessNode (i,j)) 5 plr_Even [SinkEntry; AccessLeaver i; (if j = 0 then AccessCenter i else AccessNode (i,j-1))]
		done;
		
		add (AccessLeaver i) (2 * tt * n + 2 * n + 4 * i + 11) plr_Odd [SelectorLane 0];
		add (CountingEntry i) (2 * tt * n + 2 * n + 4 * i + 13) plr_Odd [CountingCenter i];
		add (CountingCenter i) 6 plr_Odd [CountingExit i; CountingHighNode i; PacedNode (i, ss - 1)];
		add (CountingHighNode i) 5 plr_Even [CountingHighLeaver i; CountingLowNode (i, tt - 1)];
		add (CountingHighLeaver i) 7 plr_Odd (if i = 0 then [SelectorHighEntry] else [SelectorHighEntry; CountingHighLane (i-1)]);
		add (CountingHighLane i) 3 plr_Odd (if i = 0 then [CountingLowNode (i, tt - 1)] else [CountingLowNode (i, tt - 1); CountingHighLane (i-1)]);
		
		for j = 0 to tt - 1 do
			add (CountingLowNode (i,j)) 5 plr_Even [SinkEntry; CountingLowLeaver (i,j); (if j = 0 then CountingCenter i else CountingLowNode (i,j-1))];
			add (CountingLowLeaver (i,j)) (2 * tt - 2 * j + 2 * tt * i + 7) plr_Odd (if i = 0 then [SelectorLowEntry i] else [CountingLowLane (i-1,j); SelectorLowEntry i]);
			add (CountingLowLane (i,j)) 3 plr_Odd (if i = 0 then [CountingLowNode (i,j)] else [CountingLowNode (i,j); CountingLowLane (i-1,j)])
		done;
		
		add (CountingExit i) (2 * tt * n + 2 * n + 4 * i + 14) plr_Odd [SuccessorLane (i+1)];
		
		for j = 0 to ss - 1 do
			add (PacedNode (i,j)) 5 plr_Even [SinkEntry; PacerExit; (if j = 0 then CountingCenter i else PacedNode (i,j-1))]
		done;
	done;

	SymbolicParityGame.to_paritygame pg;;


let register _ =
    register_strat_impr_gen {
        ident = "randomedgeexptest";
        description = "Experimental Exponential Lower Bound for the random-edge rule";
        parity_game = Some generator_game_func;
        generalized_mdp = None;
    }
