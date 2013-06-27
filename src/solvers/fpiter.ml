open Basics ;;
open Paritygame ;;
open Univsolve;;
open Solvers;;

module IntSet = Set.Make(
struct
  type t = int
  let compare = compare
end);;

let rec repeat_until command condition =
  command (); 
  if not (condition ()) then 
    repeat_until command condition

let even i = (i mod 2 = 0)
let odd i = (i mod 2 = 1)
 
let solve' game =
  let msg_tagged v = message_autotagged v (fun _ -> "FPITER") in
(*  let msg_plain = message in *)
  
  let show_nodeSet s = "{" ^ String.concat "," (List.map string_of_int (IntSet.elements s)) ^ "}" in
  
  let n = pg_size game in
  let min_prio = pg_min_prio game in
  let max_prio = pg_max_prio game in
  let prios = max_prio - min_prio + 1 in
  let tg = game_to_transposed_graph game in
  let strategy = Array.init n (fun i -> let (_,_,ws,_) = game.(i) in ws.(0)) in
  
  let list_to_set = List.fold_left (fun s -> fun v -> IntSet.add v s) IntSet.empty in
  
  let all_nodes_list = collect_nodes game (fun _ -> fun _ -> true) in
  let all_nodes = list_to_set all_nodes_list in

  (* arrays used to hold the current and last values of X(i) as well as the evaluation of the Walukiewicz formula in the
     innermost iteration *)  
  let x = Array.make prios IntSet.empty in
  let y = Array.make prios IntSet.empty in
  let a = ref IntSet.empty in

  (* arrays used to abbreviate the unions and intersections in the Walukiewicz formula *)
  let diams  = Array.make prios IntSet.empty in        (* D(i) = <>(Pr(i) and X(i)) *)
  let unions = Array.make (prios+1) IntSet.empty in    (* U(i) = D(i) or U(i+1) *)
  let boxes  = Array.make prios all_nodes in           (* B(i) = [](-Pr(i) or X(i)) *)
  let isects = Array.make (prios+1) all_nodes in       (* I(i) = B(i) and I(i+1) *)

  let pr = Array.init prios (fun i -> list_to_set (collect_nodes game (fun _ -> fun (p,_,_,_) -> i+min_prio=p))) in
  let pr_complement = Array.init prios (fun i -> IntSet.diff all_nodes pr.(i)) in
  let v0 = list_to_set (collect_nodes game (fun _ -> fun (_,o,_,_) -> o=0)) in
  let v1 = list_to_set (collect_nodes game (fun _ -> fun (_,o,_,_) -> o=1)) in 

  let moment = ref [] in
  let make_prefix _ = "[" ^ String.concat "," (List.fold_right (fun i -> fun l -> l @ [string_of_int i]) !moment []) ^ "] " in

  (* compute the set of all nodes that have a successor in t 
     the second argument states whether a witnessing edge should be recorded in strategy for player 0 *)  

  let diamond t strategyActivated =
    IntSet.fold (fun v -> fun s -> 
                   List.fold_left (fun s' -> fun u -> 
                                     let (pr,pl,_,_) = game.(u) in
                                     if (pr >=0) then 
                                       begin
                                         let yes = pl=0 && strategyActivated && 
                                                   let w = strategy.(u) in
                                                   let (pr',_,_,_) = game.(w) in
                                                   not (IntSet.mem w x.(pr' - min_prio))
                                         in
                                         if yes then 
                                           begin
                                             msg_tagged 3 (fun _ -> make_prefix () ^ "recording strategy decision " ^ string_of_int u ^ 
                                                                    "->" ^ string_of_int v ^ " for player 0\n");
                                             strategy.(u) <- v
                                           end;
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
    IntSet.filter (fun v -> let (pr, pl, successors, _) = game.(v) in
                            if pr >= 0 then
                              if pl=1 then
                                Array.fold_left (fun b -> fun w -> let found = IntSet.mem w t in
                                                                   let yes = not found &&  
                                                                             let w' = strategy.(v) in
                                                                             let (pr',_,_,_) = game.(w') in
                                                                             IntSet.mem w' x.(pr' - min_prio)
                                                                   in
                                                                   if yes then 
                                                                     begin
                                                                       msg_tagged 3 (fun _ -> make_prefix () ^ 
                                                                                              "recording strategy decision " ^ 
                                                                                              string_of_int v ^ "->" ^ 
                                                                                              string_of_int w ^ " for player 1\n");
                                                                       strategy.(v) <- w
                                                                     end; 
                                                                   b && found) 
                                                 true successors
                              else
                                Array.fold_left (fun b -> fun w -> b && IntSet.mem w t) true successors
                            else
                              false
                  ) c 
  in 

  let update_modal_terms j first = 
    diams.(j)  <- diamond x.(j) true;
    unions.(j) <- IntSet.union diams.(j) unions.(j+1);
    boxes.(j)  <- box (IntSet.union pr_complement.(j) x.(j));
    isects.(j) <- IntSet.inter boxes.(j) isects.(j+1)
  in

  let rec recsolve i =
    let j = i-min_prio in

    if j < 0 then
      begin
        a := IntSet.union (IntSet.inter v0 unions.(0)) (IntSet.inter v1 isects.(0));
        msg_tagged 3 (fun _ -> make_prefix () ^ "computed " ^ show_nodeSet !a ^ "\n")
      end
    else
      begin
        moment := 0 :: !moment;

        msg_tagged 2 (fun _ -> make_prefix () ^ "Entering fixpoint iteration for priority " ^ string_of_int i ^ "\n");
        x.(j) <- if even i then pr.(j) else IntSet.empty;
        msg_tagged 3 (fun _ -> make_prefix () ^ "Initialising X(" ^ string_of_int i ^ ") with " ^ show_nodeSet x.(j) ^ "\n");
                 
        let first = ref true in
        repeat_until
          (fun _ -> 
            update_modal_terms j !first;
            first := false;
            msg_tagged 3 (fun _ -> make_prefix () ^ "Updating d(" ^ string_of_int i ^ ") to " ^ show_nodeSet diams.(j) ^ "\n");
            msg_tagged 3 (fun _ -> make_prefix () ^ "Updating D(" ^ string_of_int i ^ ") to " ^ show_nodeSet unions.(j) ^ "\n");
            msg_tagged 3 (fun _ -> make_prefix () ^ "Updating b(" ^ string_of_int i ^ ") to " ^ show_nodeSet boxes.(j) ^ "\n");
            msg_tagged 3 (fun _ -> make_prefix () ^ "Updating B(" ^ string_of_int i ^ ") to " ^ show_nodeSet isects.(j) ^ "\n");

            y.(j) <- x.(j);
            recsolve (i-1);
            x.(j) <- IntSet.inter !a pr.(j);

            msg_tagged 3 (fun _ -> make_prefix () ^ "New value of X(" ^ string_of_int i ^ ") is " ^ show_nodeSet x.(j) ^ "\n");
            msg_tagged 3 (fun _ -> make_prefix () ^ "Old value of X(" ^ string_of_int i ^ ") was " ^ show_nodeSet y.(j) ^ "\n");

            moment := ((List.hd !moment)+1) :: (List.tl !moment)
          )
          (fun _ -> (((even i) && IntSet.subset y.(j) x.(j)) || (odd i) && IntSet.subset x.(j) y.(j)));

        moment := List.tl !moment
      end
    in
 
    msg_tagged 2 (fun _ -> "Starting fixpoint iteration algorithm.\n");
    msg_tagged 3 (fun _ -> "initialisations:\n");
(*    (* initalisiere array x, d und b *)
    for i = prios-1 downto 0 do 
      x.(i) <- if (i mod 2 = 0) then pr.(i) else IntSet.empty;
      msg_tagged 3 (fun _ -> make_prefix () ^ "value of X(" ^ string_of_int (i+min_prio) ^ "): " ^ show_nodeSet x.(i) ^ "\n");
      d.(i) <- compute_DiamondUnion i;
      msg_tagged 3 (fun _ -> make_prefix () ^ "value of D(" ^ string_of_int (i+min_prio) ^ "): " ^ show_nodeSet d.(i) ^ "\n");
      b.(i) <- compute_BoxInter i;
      msg_tagged 3 (fun _ -> make_prefix () ^ "value of B(" ^ string_of_int (i+min_prio) ^ "): " ^ show_nodeSet b.(i) ^ "\n")
    done;   
*)                      

    recsolve max_prio; 
    let sol_set = !a in
    let solution = Array.make n (1) in
    IntSet.iter (fun v -> solution.(v) <- 0) sol_set;
    msg_tagged 2 (fun _ -> "Fixpoint iteration algorithm finished.\n");

    (solution, strategy)

    let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;
 
    register_solver solve "fpiter" "fp" "use the fixpoint iteration algorithm";;
