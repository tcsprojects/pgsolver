(* The Fixpoint-Iteration Algorithm
 *
 * from:
 * F. Bruse, M. Falk, M. Lange. The Fixpoint-Iteration Algorithm for Parity Games
 * In Proc. of the 5th Int. Symp. on Games, Automata, Logics and Formal Verification, Gandalf'14, Verona, IT, 2014
 * vol. 161 of Elect. Proc. in Theor. Computer Science, pages 116-130
 *)


open Basics ;;
open Paritygame ;;
open Univsolve;;
open Tcsbasedata;;

let even i = (i mod 2 = 0)
let odd i = (i mod 2 = 1)

(* convert a list to a set of nodes *)
let list_to_set = ns_fold (fun s -> fun v -> NodeSet.add v s) NodeSet.empty
 
let solve' game =
  let msg_tagged v = message_autotagged v (fun _ -> "FPITER") in
  
  let show_nodeSet s = "{" ^ String.concat "," (List.map string_of_int (NodeSet.elements s)) ^ "}" in
  
  let n = pg_size game in
  let min_prio = pg_min_prio game in
  let max_prio = pg_max_prio game in
  let prios = max_prio - min_prio + 1 in
  let (even_prios,odd_prios) = let a = prios / 2 in
                               if odd (max_prio - min_prio) then (a,a) 
                               else if even min_prio then (a+1,a) else (a,a+1)
  in

  let strategy = Array.make n (-1) in
  let evt_pos_strategy = Array.make n [] in
  
  let all_nodes_list = collect_nodes game (fun _ -> fun _ -> true) in
  let all_nodes = list_to_set all_nodes_list in

  (* arrays used to abbreviate the unions and intersections in the Walukiewicz formula *)
  let diams  = Array.make prios NodeSet.empty in        (* D(i) = <>(Pr(i) and X(i)) *)
  let unions = Array.make (prios+1) NodeSet.empty in    (* U(i) = D(i) or U(i+1) *)
  let boxes  = Array.make prios all_nodes in           (* B(i) = [](-Pr(i) or X(i)) *)
  let isects = Array.make (prios+1) all_nodes in       (* I(i) = B(i) and I(i+1) *)

  let pr = Array.init prios (fun i -> list_to_set (collect_nodes_by_prio game (fun p -> i+min_prio=p))) in
  let npr = Array.init prios (fun i -> NodeSet.cardinal pr.(i)) in
  let pr_complement = Array.init prios (fun i -> NodeSet.diff all_nodes pr.(i)) in
  let (v0',v1') = collect_nodes_by_owner game (fun o -> o = plr_Even) in
  let (v0,v1) = (list_to_set v0', list_to_set v1') in 

  (* variable used to hold the current winning positions *)  
  let x = Array.make prios NodeSet.empty in
  let win = ref NodeSet.empty in

  (* These functions are used to avoid direct access to any array storing values for each priority.
     They normalise the array indices such that the least priority occupies the array field 0 *)

  let set a p v = a.(p - min_prio) <- v in
  let get a p = a.(p - min_prio) in

  let even_moment = Array.make even_prios 0 in
  let odd_moment = Array.make odd_prios 0 in

  let encode prio = if even (prio - min_prio) then (prio - min_prio) / 2 else (prio - min_prio - 1) / 2 in
  let decode i = let j = 2*i + min_prio in 
                 if even min_prio then (j, j+1) else (j+1,j)
  in 

  let reset_moment p =
    let i = encode p in 
    if even p then even_moment.(i) <- 0 else odd_moment.(i) <- 0
  in
  let increase_moment p =
    let i = encode p in
    if even p then even_moment.(i) <- even_moment.(i) + 1 else odd_moment.(i) <- odd_moment.(i) + 1
  in

  let rec zip l = function [] -> l
                         | (b::bs) -> (match l with
                                            (c::cs) -> c::b::(zip cs bs)
			                  | []      -> b::bs)
  in

(*  let null_moment = Array.make prios 0 in *)

  let show_timestamp m = "[" ^ String.concat "," (Array.fold_right (fun i -> fun l -> l @ [string_of_int i]) m []) ^ "]" in
  let show_moment _ =
    let evens = Array.fold_right (fun i -> fun l -> l @ [i]) even_moment [] in
    let odds =  Array.fold_right (fun i -> fun l -> l @ [i]) odd_moment  [] in
    let ends = if even max_prio then 
                  zip evens odds 
               else
                  zip odds evens
    in  
    "[" ^ String.concat "," (List.map string_of_int ends) ^ "]" 
  in

  let rec initialise p = 
    if p >= min_prio then
      begin
        set x p (if odd p then NodeSet.empty else get pr p);
        msg_tagged 3 (fun _ -> "  Initialising X(" ^ string_of_int p ^ ") to " ^ show_nodeSet (get x p) ^ "\n");
        initialise (p-2)
      end
  in

  let rec update_modal_terms p =
    set diams  p (NodeSet.inter v0 (diamond game (get x p)));
    set unions p (NodeSet.union (get diams p) (get unions (p+1)));
    set boxes  p (NodeSet.inter v1 (box game (NodeSet.union (get pr_complement p) (get x p))));
    set isects p (NodeSet.inter (get boxes p) (get isects (p+1)));
    msg_tagged 4 (fun _ -> "  Current value: X(" ^ string_of_int p ^ ") = " ^ show_nodeSet (get x p) ^ "\n");
    msg_tagged 4 (fun _ -> "  Current value: D(" ^ string_of_int p ^ ") = " ^ show_nodeSet (get diams p) ^ "\n");
    msg_tagged 4 (fun _ -> "  Current value: U(" ^ string_of_int p ^ ") = " ^ show_nodeSet (get unions p) ^ "\n");
    msg_tagged 4 (fun _ -> "  Current value: B(" ^ string_of_int p ^ ") = " ^ show_nodeSet (get boxes p) ^ "\n");
    msg_tagged 4 (fun _ -> "  Current value: I(" ^ string_of_int p ^ ") = " ^ show_nodeSet (get isects p) ^ "\n");
    if p > min_prio then update_modal_terms (p-1)
  in

  (* returns a node in the list ws of some priority p which also belongs to x.(p) *)

  let rec find_witness = function []    -> failwith "Solvers.Fpiter.find_witness: no witness found!"
                                | w::ws -> let pr = pg_get_priority game w in
                                           if NodeSet.mem w (get x pr) then w else find_witness ws
  in

  (* returns a node in the list ws of some priority p which does not belong to x.(p) *)

  let rec find_cntexmpl = function []    -> failwith "Solvers.Fpiter.find_cntexmpl: no counterexample found!"
                                 | w::ws -> let pr = pg_get_priority game w in
                                            if not (NodeSet.mem w (get x pr)) then w else find_cntexmpl ws
  in

  let record_decision owner v w =
    let moment = if owner = plr_Even then odd_moment else even_moment in 
    match evt_pos_strategy.(v) with
       [] -> evt_pos_strategy.(v) <- [ (w, Array.copy moment) ];
             msg_tagged 3 (fun _ -> "  Recording strategy decision " ^ string_of_int v ^ " -> " ^ 
                                    string_of_int w ^ " for player " ^ plr_show owner ^ " at moment " ^ show_moment () ^ "\n")
     | (w', mnt')::str -> 
             if mnt' <> moment then
               begin 
                 evt_pos_strategy.(v) <- (w, Array.copy moment) :: evt_pos_strategy.(v);
                 msg_tagged 3 (fun _ -> "  Recording strategy decision " ^ string_of_int v ^ " -> " ^ 
                                      string_of_int w ^ " for player " ^ plr_show owner ^ " at moment " ^ show_moment () ^ "\n")
               end
             else
               begin
                 evt_pos_strategy.(v) <- (w, mnt') :: str;
                 msg_tagged 3 (fun _ -> "  Changing strategy decision to " ^ string_of_int v ^ " -> " ^ 
                                        string_of_int w ^ " for player " ^ plr_show owner ^ " at moment " ^ show_moment () ^ "\n")
               end
  in

  msg_tagged 2 (fun _ -> "Starting fixpoint iteration algorithm.\n");

  let curr_prio = ref min_prio in
  let continue = ref true in
  initialise max_prio;
  initialise (max_prio-1);
  update_modal_terms max_prio;

  while !curr_prio <= max_prio do
    win := NodeSet.union (get unions min_prio) (get isects min_prio);
    msg_tagged 3 (fun _ -> show_moment () ^ " Computed current winning set " ^ show_nodeSet !win ^ "\n");

    curr_prio := min_prio;
    continue := true;
    while !curr_prio <= max_prio && !continue do
      let win_mod_prio = NodeSet.inter (get pr !curr_prio) !win in
      increase_moment !curr_prio;
      if even !curr_prio then
        begin
          if NodeSet.subset (get x !curr_prio) win_mod_prio then
            begin 
              msg_tagged 3 (fun _ -> show_moment () ^ " Fixpoint reached: X(" ^ string_of_int !curr_prio ^ ") = " ^ 
                                     show_nodeSet win_mod_prio ^ "\n"); 
              NodeSet.iter (fun v -> if pg_get_owner game v = plr_Even then
                                       begin
                                         let w = find_witness (ns_nodes (pg_get_successors game v)) in
                                         record_decision plr_Even v w 
                                       end)
                           win_mod_prio; 
              reset_moment !curr_prio;
              incr curr_prio
            end
          else
            begin
              let old = get x !curr_prio in
              let now_out = NodeSet.diff old win_mod_prio in

              NodeSet.iter (fun v -> if pg_get_owner game v = plr_Odd then
                                       begin
                                         let w = find_cntexmpl (ns_nodes (pg_get_successors game v)) in
                                         record_decision plr_Odd v w 
                                       end)
                           now_out;
              set x !curr_prio win_mod_prio;
              msg_tagged 3 (fun _ -> show_moment () ^ " Updating: X(" ^ string_of_int !curr_prio ^ ") = " ^ 
                                     show_nodeSet (get x !curr_prio) ^ "\n"); 

              initialise (!curr_prio - 1);
              update_modal_terms !curr_prio;
              continue := false
            end
        end
      else
        begin
          if NodeSet.subset win_mod_prio (get x !curr_prio) then
            begin
              msg_tagged 3 (fun _ -> show_moment () ^ " Fixpoint reached: X(" ^ string_of_int !curr_prio ^ ") = " ^ 
                                     show_nodeSet win_mod_prio ^ "\n");
              NodeSet.iter (fun v -> if pg_get_owner game v = plr_Odd then
                                       begin
                                         let w = find_cntexmpl (ns_nodes (pg_get_successors game v)) in
                                         record_decision plr_Odd v w
                                       end)
                           (NodeSet.diff (get pr !curr_prio) win_mod_prio); 
              reset_moment !curr_prio;
              incr curr_prio
            end
          else
            begin
              let old = get x !curr_prio in
              let now_in = NodeSet.diff win_mod_prio old in

              NodeSet.iter (fun v -> if pg_get_owner game v = plr_Even then
                                       begin
                                         let w = find_witness (ns_nodes (pg_get_successors game v)) in
                                         record_decision plr_Even v w
                                       end)
                           now_in;
              set x !curr_prio win_mod_prio;
              msg_tagged 3 (fun _ -> show_moment () ^ " Updating: X(" ^ string_of_int !curr_prio ^ ") = " ^ 
                                     show_nodeSet (get x !curr_prio) ^ "\n"); 

              initialise (!curr_prio - 1);
              update_modal_terms !curr_prio;
              continue := false
            end
        end
    done
  done;

  let solution = sol_init game (fun v -> plr_Odd) in
  NodeSet.iter (fun v -> solution.(v) <- plr_Even) !win;

  (* now turn eventually positional strategy into a positional strategy *)

  let even_top_moment = Array.init even_prios (fun i -> let (p,_) = decode i in 2 + npr.(p - min_prio)) in
  let odd_top_moment  = Array.init odd_prios  (fun i -> let (_,p) = decode i in 2 + npr.(p - min_prio)) in

  let relevant_parity winner = if winner = plr_Even then odd else even in

  let compare winner prio ts1 ts2 =
    let rec cmp p =
        if p >= prio then
          let i = encode p in
          let c = compare ts1.(i) ts2.(i) in
          if c <> 0 then
            c
          else 
            cmp (p-2) 
        else  
          0
    in
    cmp (if relevant_parity winner max_prio then max_prio else max_prio - 1)
  in
        
  let next_smallest winner prio ts =
    msg_tagged 3 (fun _ -> "  Computing next smallest timestamp of " ^ show_timestamp ts ^ ": ");
    let t = Array.copy ts in
    let i = encode prio in
    let np = ref (prio - 1) in
    if relevant_parity winner prio then 
      begin 
        t.(i) <- t.(i) - 1;
        decr np
      end;
    while !np >= min_prio do
      t.(encode !np) <- 2 + npr.(!np - min_prio);
      np := !np - 2
    done;
    message 3 (fun _ -> show_timestamp t ^ "\n");
    t
  in

  let discard_decisions v winner prio m =
    while let (w,m') = List.hd evt_pos_strategy.(v) in 
          msg_tagged 3 (fun _ -> "  Checking decision " ^ string_of_int v ^ " -> " ^ 
                                 string_of_int w ^ " of moment " ^ show_timestamp m' ^ "... ");
          let c = compare winner prio m m' in
          c < 0 (* || (c = 0 && relevant_parity winner prio) *) do
      evt_pos_strategy.(v) <- List.tl evt_pos_strategy.(v);
      message 3 (fun _ -> "discarded\n")
    done;
    message 3 (fun _ -> "preserved\n")
  in

  msg_tagged 2 (fun _ -> "Now turning eventually positional strategy into a positional one.\n");

  let next_node = ref 0 in
  let todo = ref [] in
  let last_visit = Array.make n None in

  while !next_node < n do
    let top_moment = if solution.(!next_node) = plr_Odd then even_top_moment else odd_top_moment
    in
    todo := [ (!next_node, top_moment) ];
    while !todo <> [] do
      let (v, bound) = List.hd !todo in
      todo := List.tl !todo;
      let prio = pg_get_priority game v in
      let owner = pg_get_owner game v in
      let ws = pg_get_successors game v in 
      let winner = solution.(v) in
      msg_tagged 3 (fun _ -> "  Processing node " ^ string_of_int v ^ " at time " ^ show_timestamp bound ^ "\n");
      if last_visit.(v) = None || 
         let lstvis = OptionUtils.get_some last_visit.(v) in
(*         msg_tagged 3 (fun _ -> "Now comparing " ^ show_timestamp bound ^ " against " ^ show_timestamp lstvis ^ " w.r.t. " ^ 
                                string_of_int prio ^ " ... "); *)
         compare winner prio bound lstvis < 0 then
        begin
(*          message 3 (fun _ -> " less!\n"); *)
          if winner = owner then
            begin
              discard_decisions v winner prio bound; 
              let (w,mmnt) = List.hd evt_pos_strategy.(v) in
              strategy.(v) <- w;
              todo := (w, next_smallest winner prio mmnt) :: !todo
            end
          else
            begin
              let ns = next_smallest winner prio bound in
              ns_iter (fun w -> todo := (w, ns) :: !todo) ws
            end;
          last_visit.(v) <- Some bound
        end
(*      else
        message 3 (fun _ -> " greater or equal!\n") *)
    done;
    msg_tagged 3 (fun _ -> "Searching for next node to visit after " ^ string_of_int !next_node ^ " ");
    while !next_node < n && (strategy.(!next_node) > -1 || let o = pg_get_owner game !next_node in solution.(!next_node) <> o) do
      incr next_node;
      message 3 (fun _ -> ".")
    done;
    message 3 (fun _ -> "\n")
  done;
  
  msg_tagged 2 (fun _ -> "Fixpoint iteration algorithm finished.\n");

  (solution, strategy)


(* use the universal solver with the fixpoint-iteration algorithm as backend *)
let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;
 

let register _ =
    Solverregistry.register_solver solve "fpiter" "fi" "use the (optimised) iterative fixpoint iteration algorithm";;
