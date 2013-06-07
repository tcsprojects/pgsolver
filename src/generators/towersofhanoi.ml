open Paritygame ;;

type towers = int list array

type tower = A | B | C

let moves = [ (0,1,2); (0,2,1); (1,0,2); (1,2,0); (2,0,1); (2,1,0) ]

let show_state ts = "A=[" ^ (String.concat "," (List.map string_of_int ts.(0))) ^ "]; " ^
                    "B=[" ^ (String.concat "," (List.map string_of_int ts.(1))) ^ "]; " ^
                    "C=[" ^ (String.concat "," (List.map string_of_int ts.(2))) ^ "]"

let show_conf el f = show_state el ^ " |= " ^ string_of_int f

let successors ts = 
  let tops = [| if ts.(0) <> [] then List.hd ts.(0) else 0; 
                if ts.(1) <> [] then List.hd ts.(1) else 0;
                if ts.(2) <> [] then List.hd ts.(2) else 0 |]
  in
  let succs = ref [] in

  List.iter (fun (f,t,o) -> if tops.(f) > 0 && (tops.(f) < tops.(t) || tops.(t) = 0) then
                              begin
                                let tt = Array.make 3 [] in
                                tt.(f) <- List.tl ts.(f);
                                tt.(t) <- tops.(f) :: ts.(t);
                                tt.(o) <- ts.(o);
                                succs := tt :: !succs
                              end)
            moves;
  !succs
                 

let finished ts = (ts.(0) = []) && (ts.(2) = []) 

type formulaType = FP of int * int             (* fixpoint formula with priority and next subformula *)
                 | BOOL of int * int * int     (* con-/disjunction with player and left and right subformula *)
                 | MOD of int * int            (* modality with player and next subformula *)
                 | PROP of (towers -> bool)  (* proposition with function that evaluates it in a state *)

(* CTL-formula: EF finished,
   im mu-Kalkül: mu X. finished \/ <>X *)

let formula = [| FP(1,1);          
                 BOOL(0,2,3);      
                 PROP(finished);   
                 MOD(0,0) |]

module GameNode = 
struct
  type t = towers * int
  let compare = compare
end ;;

module Coding = Map.Make(GameNode) ;;

let towers_of_hanoi_func arguments = 

  let show_help _ =
    print_string (Info.get_title "Towers of Hanoi Planning Game Generator");
    print_string ("Usage: towersofhanoi n \n\n" ^
		  "       where n = Number of levels that the tower to be moved has (>= 1)\n\n")
  in
  if (Array.length arguments <> 1) then (show_help (); exit 1);

  let levels = (try
                 int_of_string arguments.(0)
                with _ -> (show_help (); exit 1))
  in

  if not (levels > 0) then (show_help(); exit 1);

  let initialGameNode = 
    let rec lvs i aux = if i=0 then aux else lvs (i-1) (i::aux)
    in
    ([| lvs levels []; []; [] |], 0)
  in

  let size = 
    let rec size_aux i aux = if i=0 then aux else size_aux (i-1) (3 * aux) in
    4 * (size_aux levels 1)
  in
  let visited = Array.make size false in

  let encoding = ref Coding.empty in
  let index = ref 0 in
  let newIndex _ = 
    let i = !index in 
    incr index; i 
  in

  let encode el f =
    try
      Coding.find (el,f) !encoding
    with 
      Not_found -> begin
                     let i = newIndex () in
                     encoding := Coding.add (el,f) i !encoding;
                     i
                   end
  in
  
  let finished = ref [] in
  let (el,f) = initialGameNode in
  let todo = ref [ (el, f, encode el f) ] in

  while !todo <> [] do
    let (el,f,i) = List.hd !todo in
    todo := List.tl !todo;
    if not visited.(i) then
    begin
      match formula.(f) with
            FP(p,g) -> let v = encode el g in
                       finished := (i, (p,0,[|v|],show_conf el f)) :: !finished;
                       todo := (el,g,v) :: !todo 
          | BOOL(pl,g,h) -> let v = encode el g in
                            let w = encode el h in
                            finished := (i, (0,pl,[|v;w|],show_conf el f)) :: !finished;
                            todo := (el,g,v) :: (el,h,w) :: !todo
          | MOD(pl,g) -> let nextnodes = List.map (fun el -> (el, g, encode el g)) (successors el) in
                         let nextnodes_coded = Array.of_list (List.map (fun (_,_,v) -> v) nextnodes) in
                         finished := (i, (0,pl,nextnodes_coded, show_conf el f)) :: !finished;
                         todo := nextnodes @ !todo
          | PROP(p) -> finished := (i, ((if p el then 0 else 1), 0, [|i|], show_conf el f)) :: !finished
    end;
    visited.(i) <- true
  done;
  
  let game = pg_create !index in
  List.iter (fun (i, (p,pl,succs,name)) -> game.(i) <- (p,pl,succs,Some name)) !finished;
  game
  



let _ = Generators.register_generator towers_of_hanoi_func "towersofhanoi" "Towers of Hanoi Planning Game";;
