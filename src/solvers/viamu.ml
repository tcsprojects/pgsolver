(* Implemented by Michael Falk, 2013, University of Kassel *)

open Basics ;;
open Paritygame ;;
open Univsolve;;
open Solvers;;


module IntSet = Set.Make(
  struct
    type t = int
    let compare = compare
  end);;

let solve game =
   let n = pg_size game in
   let min_prio = pg_min_prio game in
   let max_prio = pg_max_prio game in
   let d = max_prio - min_prio + 1 in

   let list_to_set = List.fold_left (fun s -> fun v -> IntSet.add v s) IntSet.empty in

   let all_nodes_list = collect_nodes game (fun _ -> fun _ -> true) in
   let all_nodes = list_to_set all_nodes_list in

   let x = Array.make d IntSet.empty in
   let y = Array.make d IntSet.empty in
   let pr = Array.init d (fun i -> list_to_set (collect_nodes game (fun _ -> fun (p,_,_,_) -> i+min_prio=p))) in
   let v0 = list_to_set (collect_nodes game (fun _ -> fun (_,o,_,_) -> o=0)) in
   let v1 = list_to_set (collect_nodes game (fun _ -> fun (_,o,_,_) -> o=1)) in

   let compute_union m n =
     let rec comp_un acc i =
        if i > n then acc
        else comp_un (IntSet.union acc (IntSet.inter pr.(i-m)  x.(i-min_prio))) (i+1)
     in
     comp_un IntSet.empty m
   in     
   
     let diamand t =
     let s = ref IntSet.empty in
     for i = 0 to (Array.length game) - 1 do
       let (pr, _, successor, _) = game.(i) in
       if (pr >= 0) && IntSet.exists (fun x -> List.mem x (Array.to_list successor)) t then
         s := IntSet.add i !s
     done;
     !s
     
   in
         
   let box t =
     let s = ref IntSet.empty in
       for i = 0 to (Array.length game) - 1 do
         let (pr, _, successor, _) = game.(i) in
           if (pr >= 0) && List.for_all (fun x -> IntSet.mem x t) (Array.to_list successor) then
             s := IntSet.add i !s
       done;
       !s
         
   in
   
   let rec recsolve i =
     if i < min_prio then
       begin
         IntSet.union
           (IntSet.inter v0 (diamand (compute_union min_prio max_prio)))
           (IntSet.inter v1 (box (compute_union min_prio max_prio)))
       end
     else
       begin
         let s = if i mod 2 = 0 then all_nodes else IntSet.empty in 
         let j = i-min_prio in
         x.(j) <- s;
         let rec loop _ =
             y.(j) <- x.(j);
             x.(j) <- recsolve (i-1);
             if not (IntSet.equal x.(j) y.(j)) then loop ()
         in
         loop ();
         x.(j)
       end
   in

   let sol_set = recsolve max_prio in
   let solution = Array.make n (1) in
   IntSet.iter (fun v -> solution.(v) <- 0) sol_set;

   (solution, Array.make n (-1));;

let _ = register_solver solve "viamucalculus" "vm" "use the reduction to mu-calculus model checking (does not compute winning strategies!)";;
