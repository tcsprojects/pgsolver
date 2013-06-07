open Tcsset ;;
open Paritygame ;;

let rec range i j = if j >= i then i :: (range (i+1) j) else []

type elevator = { position: int;
                  dooropen: bool;
                  requests: int list }

let storeys = ref 0

let show_state el = "[" ^ (String.make el.position '.') ^ (if el.dooropen then "O" else "X") ^ 
                    (String.make (!storeys - el.position - 1) '.') ^ "] " ^
                    String.concat "," (List.map string_of_int el.requests)

(* fair means that any new request will be added to the end of the list (FIFO),
   whereas unfair means that it will be added to the beginning (LIFO) *)
let fair = ref true

let rec insert x = 
  function []    -> [x]
         | y::ys -> if x=y then y::ys else y::(insert x ys)

let successors el = 
  (* any button could be pressed or no requests could come in *)
  let els = el::(List.map (fun i -> { position = el.position; 
                                      dooropen = el.dooropen; 
                                      requests = if !fair 
                                                 then insert i el.requests
                                                 else i::(List.filter (fun j -> j<>i) el.requests)}) 
                          (range 0 (!storeys-1))) in

  (* for each possibility decide in which direction to go *)
  TreeSet.remove_list_dups (List.map (fun el -> if el.requests <> [] && not el.dooropen then
                                   begin
                                     let r = List.hd el.requests in
                                     let delta = compare r el.position in
                                     let newposition = el.position + delta in
                                     { position = newposition;
                                       dooropen = (newposition=r);
                                       requests = if newposition=r then List.tl el.requests else el.requests }
                                     end
			           else
				     { position = el.position;
				       dooropen = false;
				       requests = el.requests })
		        els)

let isPressed i el = List.mem i el.requests

let isAt i el = (i = el.position)


type formulaType = FP of int * int             (* fixpoint formula with priority and next subformula *)
                 | BOOL of int * int * int     (* con-/disjunction with player and left and right subformula *)
                 | MOD of int * int            (* modality with player and next subformula *)
                 | PROP of (elevator -> bool)  (* proposition with function that evaluates it in a state *)


(* CTL*-Formel:   AG(EGF isPressed(i) -> EGF isAt(i))
   im mu-Kalkül:  nu X.[]X & ((mu Y.nu Z. (-isPressed(i) | []Y) & []Z) | 
                              (nu V.mu W. isAt(i) & <> V | <>W)) 

0   X
1   []X & (Y | V)
2   []X
3   Y | V
4   Y
5   Z
6   (..) & []Z
7   -isPressed(i) | []Y
8   -isPressed(i)
9   []Y 
10  []Z
11  V
12  W
13  (..) | <>W
14  isAt(i) & <>V
15  isAt(i)
16  <>V
17  <>W

*)

(*
let formula i = [| FP(0,1);
                   BOOL(1,2,3);
                   MOD(1,0);
                   BOOL(0,4,11);
                   FP(1,5);
                   FP(0,6);
                   BOOL(1,7,10);
                   BOOL(0,8,9);
                   PROP(isPressed i);
                   MOD(1,4); 
                   MOD(1,5);
                   FP(2,12);
                   FP(1,13);
                   BOOL(0,14,17);
                   BOOL(1,15,16);
                   PROP(isAt i);
                   MOD(0,11);
                   MOD(0,12) |]
*)


(* CTL*-Formel:   A(GF isPressed(i) -> GF isAt(i)) == -E(GF isPressed(i) /\ FG -isAT(i))
   im mu-Kalkül:  nu X.mu Y.nu Z.[]X /\ (isAt(i) \/ ([]Z /\ (-isPressed(i) \/ []Y)))  

0   X
1   []X /\ (isAt(i) \/ ([]Z /\ (-isPressed(i) \/ []Y)))
2   []X
3   isAt(i) \/ ([]Z /\ (-isPressed(i) \/ []Y))
4   isAt(i) 
5   []Z /\ (-isPressed(i) \/ []Y)
6   []Z
7   Z
8   -isPressed(i) \/ []Y
9   -isPressed(i)
10  []Y
11  Y
*)

let formula i = [| FP(2,1);
                   BOOL(1,2,3);
                   MOD(1,0);
                   BOOL(0,4,5);
                   PROP(isAt i);
                   BOOL(1,6,8);
                   MOD(1,7);
                   FP(0,1);
                   BOOL(0,9,10);
                   PROP(fun el -> not (isPressed i el));
                   MOD(1,11);
                   FP(1,1) |]



module GameNode = 
struct
  type t = elevator * int
  let compare = compare
end ;;

module Coding = Map.Make(GameNode) ;;

let initialGameNode = ({position = 0; dooropen = false; requests = []}, 0)

let computeGameSize _ = 
  let numberSubformulas = Array.length (formula 1) in
  let numberStoreys = !storeys in
  let numberRequestlists = 
    let rec possibilities n k = if k=0 then 1 else n * (possibilities (n-1) (k-1)) in
    let sum = List.fold_left (+) 0 in
    sum (List.map (fun i -> possibilities numberStoreys i) (range 0 numberStoreys))
  in
  numberSubformulas * numberStoreys * 2 * numberRequestlists

let show_conf el f = show_state el ^ " |= " ^ string_of_int f

let elevator_verification_func arguments = 

  let show_help _ =
    print_string (Info.get_title "Elevator Verification Game Generator");
    print_string ("Usage: elevatorverificationgame [-u] n \n\n" ^
		  "       where n = Number of storeys that the elevator serves (>= 1)\n" ^
                  "             the optional argument -u causes the elevator to be unfair " ^
                  "(and thus not have the desired property\n\n")
  in
  if (Array.length arguments > 2 || Array.length arguments < 1) then (show_help (); exit 1);

  (try
    if arguments.(0) = "-u" then
      begin
        fair := false;
        storeys := int_of_string arguments.(1)
      end
    else
      storeys := int_of_string arguments.(0)
  with _ -> (show_help (); exit 1));

  let size = computeGameSize () in
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
  let formula = formula (!storeys -1) in

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
  

let _ = Generators.register_generator elevator_verification_func "elevatorvergm" "Elevator Verification Game";;
