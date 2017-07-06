(*
	APT: Algorithm presented in "Solving Parity Games Using An Automata-Based Algorithm"  by
	Antonio Di Stasio, Aniello Murano, Giuseppe Perelli and Moshe Vardi, 
	based on an idea of Moshe Vardi and Orna Kupferman presented some years ago in 
	"Weak alternating automata and tree automata emptiness".

*)

open Basics ;;
open Paritygame;;
open Solvers;;
open Set;;
open Univsolve ;;

let rec fold_until p = function
	| x :: xs when p x -> true
 	| x :: xs -> fold_until p xs
 	| [] -> false
 
type node_t = Node of node
           
module NodesSet = Set.Make( 
  struct
    let compare = compare
    type t =  node_t
    type elt =  node_t
  end )             
    
 
let check game node visiting avoiding i strategy=

    let pl = pg_get_owner game node in
    let suc = pg_get_successors game node in
    let acc=ref(false) in

    let nodes_suc =ns_nodes suc in 

    if(pl = i)then             
    (    
       let test = fold_until (fun x -> 
                                          if (NodesSet.mem (Node x) visiting) then (
                                             strategy.(node) <- x;
                                              true)
                                          else
                                              false
                    ) nodes_suc in 
       if (test = true)then 
       (
       		acc:=true;
       )   
       else (
          if( strategy.(node) > -1 )then
            strategy.(node) <- -1;

       )

   	)
    else
    (  
      let test = fold_until (fun x -> 
                                      if (NodesSet.mem (Node x) avoiding) then(
                                          strategy.(node) <- x;
                                          true
                                      )
                                      else
                                          false
              ) nodes_suc in

     	if(test = true)then
     	(
     		acc:=false;
     	)
     	else(
        if( strategy.(node) > -1 )then
            strategy.(node) <- -1;
        acc:= true;

      )
    );         

     (!acc)
;;


let force game visiting avoiding i strategy=

	let ris = ref(NodesSet.empty) in 
	let l = pg_size game in

	for j=0 to l-1 do
    (  
    	let acc = check game j visiting avoiding i strategy in
        if(acc = true)then
        	ris:= NodesSet.add (Node j) !ris        
    )
    done;             

	(!ris)
;; 
 
let rec win game nodes alpha visiting avoiding pl strategy=

	let w = ref(NodesSet.empty) in

	if((List.length alpha) >0) then
		w := NodesSet.diff nodes (min_fixed_point game nodes alpha avoiding visiting (plr_opponent pl) strategy)
	else(
		w:=force game visiting avoiding pl strategy ;	
	);

	(!w)

and min_fixed_point game nodes alpha visiting avoiding pl strategy=

  let y1 = ref(NodesSet.empty) in
	let y2 = ref(NodesSet.empty) in
	let app=ref(NodesSet.empty) in
        
	let v' = ref(NodesSet.union visiting (List.hd alpha)) in 
	let a' = ref(avoiding) in
	    
	let alpha' = ref(List.tl alpha) in   
    	    	    
  y2 := win game nodes !alpha' !v' !a' pl strategy;

	app:=!v';       

  while( y2 <> y1 ) do 
  (     	
    y1 := !y2 ; 

		v' := NodesSet.union visiting (NodesSet.inter ( List.hd alpha) !y1);(* V U F_i intersect Y*)
	  a' := NodesSet.union avoiding (NodesSet.diff (List.hd alpha) !y1);(* A U (F_i \ Y)*)

	    
	  if(not (NodesSet.equal !app !v'))then
	   	y2:= win game nodes !alpha' !v' !a' pl strategy
	  else
	   	y2:=!y1;

	  app:=!v';

	 )
	 done;	

	 (!y2) 
	;;	

(* ----------------- M A I N -----------------*)
let solver_apt_vardi game  =

    
    let l = pg_size game in            

    let max_prio = pg_max_prio game in

    let solution = sol_create game in
    let strategy = Array.make l (-1) in 
  
    let nodes =  ref(NodesSet.empty) in
    
      
    let app=Array.make (max_prio+1) NodesSet.empty in   
    
    for i=0 to l-1 do 
        nodes := NodesSet.add (Node i) !nodes;
        let pr = pg_get_priority game i in
        app.(pr) <- NodesSet.add (Node i) app.(pr); 
    done;
      
    let b' = ref([]) in
    for i= 0 to max_prio do
       b':= app.(i) :: !b'
    done;     
    
    let v = ref(NodesSet.empty) in
    let a = ref(NodesSet.empty) in  

    let pl = ref(plr_undef) in

    if(max_prio mod 2 = 0) then
      pl:=plr_Odd
    else
      pl:=plr_Even;
    
    let acc = win game !nodes !b' !v !a !pl strategy in
    
    for i=0 to l-1 do
       if (NodesSet.mem (Node i) acc) then 
          solution.(i)<- !pl
       else    
          solution.(i)<- plr_opponent !pl
    done;           
     
      (solution,strategy);;

let solve game =

    let (solution,strategy) = (*universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) *)solver_apt_vardi game in
    (solution,strategy)   
    

     ;; 
    
let register _ =
    Solverregistry.register_solver solve "apt" "apt" "use the algorithm due to Vardi / Kupferman";;
