open Basics ;;
open Paritygame ;;
open Univsolve;;
open Solvers;;

module IntSet = Set.Make(
struct
  type t = int
  let compare = compare
end);;

let solve' game =
  let msg_tagged v = message_autotagged v (fun _ -> "VIAMU") in
  let msg_plain = message in
  
  let show_nodeSet s = "{" ^ String.concat "," (List.map string_of_int (IntSet.elements s)) ^ "}" in
  
  let n = pg_size game in
  let min_prio = pg_min_prio game in
  let max_prio = pg_max_prio game in
  let u = max_prio - min_prio + 1 in
  let tg = game_to_transposed_graph game in
  let strategy = Array.init n (fun i -> let (_,_,ws,_) = game.(i) in ws.(0)) in
  
  let list_to_set = List.fold_left (fun s -> fun v -> IntSet.add v s) IntSet.empty in
  
  let all_nodes_list = collect_nodes game (fun _ -> fun _ -> true) in
  let all_nodes = list_to_set all_nodes_list in
  
  let x = Array.make u IntSet.empty in
  let y = Array.make u IntSet.empty in
  let d = Array.make (u+1) IntSet.empty in    
  let b = Array.make (u+1) all_nodes in    
  let pr = Array.init u (fun i -> list_to_set (collect_nodes game (fun _ -> fun (p,_,_,_) -> i+min_prio=p))) in
  let pr_complement = Array.init u (fun i -> IntSet.diff all_nodes pr.(i)) in
  let v0 = list_to_set (collect_nodes game (fun _ -> fun (_,o,_,_) -> o=0)) in
  let v1 = list_to_set (collect_nodes game (fun _ -> fun (_,o,_,_) -> o=1)) in 


  (* compute the set of all nodes that have a successor in t 
     the second argument states whether a witnessing edge should be recorded in strategy for player 0 *)  

  let diamond t strategyActivated =
    IntSet.fold (fun v -> fun s -> List.fold_left (fun s' -> fun u -> let (pr,pl,_,_) = game.(u) in
                                                                      if (pr >=0) then 
                                                                        begin
                                                                          if pl=0 && strategyActivated then strategy.(u) <- v;
                                                                          IntSet.add u s'
                                                                        end
                                                                      else s')  
                                                  s tg.(v)) 
                t IntSet.empty 
  in


  (* Compute the set of all nodes such that all successors are in t.
     It uses the diamond function to restrict the search space.
     It records edges that witness why a node does not belong to (box t) in strategy for player 1. *)  

  let box t =
    let c = diamond t false in      
    IntSet.filter (fun i -> let (pr, pl, successors, _) = game.(i) in
                            if pr >= 0 then
                              if pl=1 then
                                Array.fold_left (fun b -> fun w -> let found = IntSet.mem w t in
                                                                   if not found then strategy.(i) <- w; 
                                                                   b && found) 
                                                 true successors
                              else
                                Array.fold_left (fun b -> fun w -> b && IntSet.mem w t) true successors
                            else
                              false
                  ) c 
  in 

  let compute_DiamondUnion j =
    IntSet.union (diamond (IntSet.inter pr.(j) x.(j)) true) d.(j+1) in
 
  let compute_BoxInter j =
    IntSet.inter (box (IntSet.union pr_complement.(j) x.(j))) b.(j+1) in
    
  (* initalisiere array x, d und b *)
  for i = u-1 downto 0 do 
    x.(i) <- if (i mod 2 = 0) then pr.(i) else IntSet.empty;
    d.(i) <- compute_DiamondUnion i;
    b.(i) <- compute_BoxInter i;
  done;   
                      
  let rec recsolve i =
    let prefix = String.make (max_prio-i) ' ' in
      let j = i-min_prio in
      if j < 0 then
          IntSet.union (IntSet.inter v0 d.(0)) (IntSet.inter v1 b.(0))
      else
        begin
          (* initialisiere x, d und b *)
          x.(j) <- if i mod 2 = 0 then pr.(j) else IntSet.empty;
          d.(j) <- compute_DiamondUnion j;              
          b.(j) <- compute_BoxInter j;  
                 
          let condition = ref true in
          while !condition do           
            y.(j) <- x.(j);
            x.(j) <- recsolve (i-1);
            d.(j) <- compute_DiamondUnion j;
            b.(j) <- compute_BoxInter j;
          
            if (j > 0) then
              begin
                x.(j) <- IntSet.inter x.(0) pr.(j);
                if not (IntSet.equal x.(j) y.(j)) then 
                  begin
                    b.(j) <- compute_BoxInter j;
                    d.(j) <- compute_DiamondUnion j;
                  end;
              end;   
            condition := (not (IntSet.equal x.(j) y.(j)))
          done;
          x.(0)
        end
    in
 
    let sol_set = recsolve max_prio in
    let solution = Array.make n (1) in
    IntSet.iter (fun v -> solution.(v) <- 0) sol_set;
     
    (solution, strategy)

    let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;
 
    register_solver solve "fpiter" "fp" "use the fixpoint iteration algorithm";;
