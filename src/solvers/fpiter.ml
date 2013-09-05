open Basics ;;
open Paritygame ;;
open Univsolve;;
open Solvers;;

(*
let rec repeat_until command condition =
  command (); 
  if not (condition ()) then 
    repeat_until command condition

let rec repeat_while command condition =
  command (); 
  if condition () then 
    repeat_while command condition
*)

let even i = (i mod 2 = 0)
let odd i = (i mod 2 = 1)

let list_to_set = List.fold_left (fun s -> fun v -> NodeSet.add v s) NodeSet.empty
 
let solve' game =
  let msg_tagged v = message_autotagged v (fun _ -> "FPITER2") in
(*  let msg_plain = message in *)
  
  let show_nodeSet s = "{" ^ String.concat "," (List.map string_of_int (NodeSet.elements s)) ^ "}" in
  
  let n = pg_size game in
  let min_prio = pg_min_prio game in
  let max_prio = pg_max_prio game in
  let prios = max_prio - min_prio + 1 in
  let tg = game_to_transposed_graph game in
  let strategy = Array.init n (fun i -> let (_,_,ws,_) = game.(i) in ws.(0)) in
  
  let all_nodes_list = collect_nodes game (fun _ -> fun _ -> true) in
  let all_nodes = list_to_set all_nodes_list in

  (* arrays used to abbreviate the unions and intersections in the Walukiewicz formula *)
  let diams  = Array.make prios NodeSet.empty in        (* D(i) = <>(Pr(i) and X(i)) *)
  let unions = Array.make (prios+1) NodeSet.empty in    (* U(i) = D(i) or U(i+1) *)
  let boxes  = Array.make prios all_nodes in           (* B(i) = [](-Pr(i) or X(i)) *)
  let isects = Array.make (prios+1) all_nodes in       (* I(i) = B(i) and I(i+1) *)

  let pr = Array.init prios (fun i -> list_to_set (collect_nodes game (fun _ -> fun (p,_,_,_) -> i+min_prio=p))) in
  let pr_complement = Array.init prios (fun i -> NodeSet.diff all_nodes pr.(i)) in
  let v0 = list_to_set (collect_nodes game (fun _ -> fun (_,o,_,_) -> o=0)) in
  let v1 = list_to_set (collect_nodes game (fun _ -> fun (_,o,_,_) -> o=1)) in 

  (* variable used to hold the current winning positions *)  
  let x = Array.make prios NodeSet.empty in
  let win = ref NodeSet.empty in

  (* These functions are used to avoid direct access to any array storing values for each priority.
     They normalise the array indices such that the least priority occupies the array field 0 *)
  let set a p v = a.(p - min_prio) <- v in
  let get a p = a.(p - min_prio) in

  let moment = Array.make prios 0 in
  let make_prefix _ = "[" ^ String.concat "," (Array.fold_right (fun i -> fun l -> l @ [string_of_int i]) moment []) ^ "] " in

  let rec initialise p = 
    if p >= min_prio then
      begin
        set x p (if odd p then NodeSet.empty else get pr p);
        msg_tagged 3 (fun _ -> "Initialising X(" ^ string_of_int p ^ ") to " ^ show_nodeSet (get x p) ^ "\n");
        initialise (p-1)
      end
  in

  let rec update_modal_terms p =
    set diams  p (NodeSet.inter v0 (diamond_with_transposed_graph (get x p) game tg));
    set unions p (NodeSet.union (get diams p) (get unions (p+1)));
    set boxes  p (NodeSet.inter v1 (box_with_transposed_graph (NodeSet.union (get pr_complement p) (get x p)) game tg));
    set isects p (NodeSet.inter (get boxes p) (get isects (p+1)));
    msg_tagged 3 (fun _ -> "Current value: X(" ^ string_of_int p ^ ") = " ^ show_nodeSet (get x p) ^ "\n");
    msg_tagged 3 (fun _ -> "Update modal term: D(" ^ string_of_int p ^ ") = " ^ show_nodeSet (get diams p) ^ "\n");
    msg_tagged 3 (fun _ -> "Update modal term: U(" ^ string_of_int p ^ ") = " ^ show_nodeSet (get unions p) ^ "\n");
    msg_tagged 3 (fun _ -> "Update modal term: B(" ^ string_of_int p ^ ") = " ^ show_nodeSet (get boxes p) ^ "\n");
    msg_tagged 3 (fun _ -> "Update modal term: I(" ^ string_of_int p ^ ") = " ^ show_nodeSet (get isects p) ^ "\n");
    if p > min_prio then update_modal_terms (p-1)
  in

  msg_tagged 2 (fun _ -> "Starting fixpoint iteration algorithm.\n");

  let curr_prio = ref min_prio in
  let continue = ref true in
  initialise max_prio;
  update_modal_terms max_prio;

  while !curr_prio <= max_prio do
    win := NodeSet.union (get unions min_prio) (get isects min_prio);
    msg_tagged 3 (fun _ -> "Computed current winning set " ^ show_nodeSet !win ^ "\n");

    curr_prio := min_prio;
    continue := true;
    while !curr_prio <= max_prio && !continue do
      let win_mod_prio = NodeSet.inter (get pr !curr_prio) !win in
      if even !curr_prio then
        begin
          if NodeSet.subset (get x !curr_prio) win_mod_prio then
            begin 
              msg_tagged 3 (fun _ -> "Fixpoint reached: X(" ^ string_of_int !curr_prio ^ ") = " ^ show_nodeSet win_mod_prio ^ "\n"); 
              set moment !curr_prio 0;
              incr curr_prio
            end
          else
            begin
              set moment !curr_prio ((get moment !curr_prio) + 1);
              set x !curr_prio win_mod_prio;
              msg_tagged 3 (fun _ -> make_prefix () ^ "Updating: X(" ^ string_of_int !curr_prio ^ ") = " ^ 
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
              msg_tagged 3 (fun _ -> "Fixpoint reached: X(" ^ string_of_int !curr_prio ^ ") = " ^ show_nodeSet win_mod_prio ^ "\n"); 
              set moment !curr_prio 0;
              incr curr_prio
            end
          else
            begin
              set moment !curr_prio ((get moment !curr_prio) + 1);
              set x !curr_prio win_mod_prio;
              msg_tagged 3 (fun _ -> make_prefix () ^ "Updating: X(" ^ string_of_int !curr_prio ^ ") = " ^ 
                                     show_nodeSet (get x !curr_prio) ^ "\n"); 
              initialise (!curr_prio - 1);
              update_modal_terms !curr_prio;
              continue := false
            end
        end
    done
  done;

  let solution = Array.make n (1) in
  NodeSet.iter (fun v -> solution.(v) <- 0) !win;
  msg_tagged 2 (fun _ -> "Fixpoint iteration algorithm finished.\n");

  (solution, strategy)


let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;
 
register_solver solve "fpiter2" "fi" "use the fixpoint iteration algorithm (non-recursive)";;
