(* The genetic heuristic
 *
 * from:
 * ... uh well ... actually no-one wants to be responsible for this kind of nonsense. Unthinkable that it
 * would have been published anywhere, maybe GandALF ... or ICTAC. Or TSGAPG, the Transsylvanian Symposium 
 * on Genetic Algorithms for Parity Games. 
 *)

open Paritygame ;;
open Univsolve ;;
open Tcsarray ;;
open Basics ;;
open Arg;;
open Tcsargs;;

let _ = Random.self_init ()

let msg_tagged v = message_autotagged v (fun _ -> "GENETIC")
let msg_plain = message


(* An evaluated strategy is a strategy in which each edge has a local value remembering the number of times 
   this edge has been used in a winning play. There are also two global values remembering how often each
   of the players has won a play using this strategy. *)

type evaluated_strategy = (int * int) array * int * int

let show (str,g0,g1) = 
  let show i = match i with -1 -> "_" | _ -> string_of_int i in
  "[" ^ String.concat "," (Array.to_list (Array.mapi (fun i -> fun (w,e) -> string_of_int i ^ "-(" ^ string_of_int e ^ ")->" ^ show w) str)) ^ "]"
  ^ ":" ^ string_of_int g0 ^ ":" ^ string_of_int g1

module Parameters = 
struct

  let max_pool_size = ref 100
  let initial_population = ref 2
  let mutation_prob = ref 0.02
  let procreation_prob = ref 0.1
  let creation_prob = ref 0.2
  let test_breakpoint = ref 20


  (* decide whether or not to mutate in the i-th round *)
  (* here: independent of round number *)
  let mutate_now _ = 
    let x = Random.float 1.0 in
    x < !mutation_prob

 
  (* decide whether or not to procreate in the i-th round *)
  (* here: independent of round number *)
  let procreate_now _ = 
    let x = Random.float 1.0 in
    x < !procreation_prob


  (* decide whether or not to create a new strategy in the i-th round *)
  (* makes more sense in the beginning, for otherwise it will be kicked out immediately *)
  let create_now i =
    let x = Random.float 1.0 in
    let i = (float_of_int i) /. (float_of_int ((!max_pool_size - !initial_population) / 2)) in
    x < 1.0 /. ((i +. 1.0) *. (i +. 1.0))


  (* decide whether or not to test a random strategy in the i-th round *)
  (* here: low probability before !test_breakpoint, high afterwards, and continuously increasing *)
  let test_now i = 
    let x = Random.float 3.1415 in
    x < (atan ((float_of_int (i - !test_breakpoint)) /. 2.0)) +. 3.1415 /. 2.0

end ;;

module RandomDistribution =
struct
    
  (* normal distribution over [0,..,n-1] *)
  let normal = Random.int

  (* distribution weakly biased towards low values *)
  let biased_low n = 
    let x = Random.float 1.0 in
    let x = x *. x in
    int_of_float (x *. (float_of_int n))

  (* distribution strongly biased towards low values *)
  let very_biased_low n = 
    let x = Random.float 1.0 in
    let x = x *. x *. x in
    int_of_float (x *. (float_of_int n))

  (* distribution weakly biased towards high values *)
  let biased_high n = 
    let x = Random.float 1.0 in
    let x = sqrt x in
    int_of_float (x *. (float_of_int n))

end ;;

module Pool =
struct

  open Parameters ;;

  let pool = DynArray.create ([|(0,0)|],0,0)

  let init _ = 
    DynArray.clear pool


  (* computes the global evaluation of a strategy for a position in the pool *)

  let evaluate estr =
    let (_,g0,g1) = estr in
    (* max g0 g1 *)  (* Alternative g0 + g1 oder g0^2 + g1^2 *)
    g0 * g0 + g1 * g1

  (* inserts a strategy into the pool at the appropriate position *)

  let insert estr =
    let rec search x start finish =
      if start >= finish then 
        start 
      else
        let middle = start + ((finish - start) / 2) in
        let y = evaluate (DynArray.get pool middle) in
        if x > y then
          search x start middle
        else
          search x (middle+1) finish
    in
    DynArray.insert pool (search (evaluate estr) 0 ((DynArray.length pool)-1)) estr;
    if DynArray.length pool > !max_pool_size then DynArray.delete_last pool
    

  (* select a strategy randomly from the pool and take it out *)

  let select_and_remove f = 
    let n = DynArray.length pool in
    let i = f n in
    let estr = DynArray.get pool i in
    DynArray.delete pool i;
    estr


  (* select a strategy randomly from the pool and leave it in *)

  let select f = 
    let n = DynArray.length pool in
    let i = f n in
    DynArray.get pool i

end ;;



module Genetics =
struct

  (* create a new random strategy *)

  let give_birth game =
    let l = pg_size game in
    let str = Array.make l (0,0) in
    pg_iterate (fun v -> fun (_,_,ws,_,_) -> str.(v) <- (ns_some ws, 0)) game;
    (str,0,0)


  (* mutate a strategy once and update its evaluation of an edge *)
  (* (the current version keeps the global evaluation intact) *)

  let mutate game estr =
    let (str,g0,g1) = estr in
    let str = Array.copy str in
    let l = Array.length str in

    let v = Random.int l in
    let ws = Array.of_list (ns_nodes (pg_get_successors game v)) in
    
    let (o,_) = str.(v) in
    let n = ws.(Random.int (Array.length ws)) in
    if n <> o then
      begin
        str.(v) <- (n,0);
        (str,g0,g1)
      end
    else
      (str,g0,g1)

    
  (* create a strategy out of two other strategies *)
  (* choices in the child strategy are randomly inherited from either of the parents, distribution is biased
     according to their local evaluations *)
  (* local evaluations are transferred, global evaluations are the arithmetic middle weighted by the choice distribution *)
  (* requires both strategies to be of the same length! *)

  let procreate estr1 estr2 =
    let (str1,g01,g11) = estr1 in
    let (str2,g02,g12) = estr2 in
    let l = Array.length str1 in
    let str = Array.make l (0,0) in
    let counter = ref 0 in
    for i=0 to l-1 do
      let (_,v1) = str1.(i) in
      let (_,v2) = str2.(i) in
      str.(i) <- if Random.float 1.0 < (float_of_int (v1+1)) /. (float_of_int (v1+v2+2)) then
                   begin
                     incr counter;
                     str1.(i)
                   end
                 else 
                   str2.(i)
    done;
    (str, ((g01 * !counter) + (g02 * (l - !counter))) / l, ((g11 * !counter) + (g12 * (l - !counter))) / l)


  (* performs a competition between two strategies and returns two strategies with updated scores *)
  (* the two strategies must be of equal length *)
  (* modifies the two strategies inplace! *)

  let compete game estr0 estr1 =
    let (str0,g00,g01) = estr0 in
    let (str1,g10,g11) = estr1 in
    let wins0 = ref g00 in
    let wins1 = ref g11 in

    let l = Array.length str1 in
    let passed = Array.make l (-2) in  (* -2 = not passed yet, -1 = passed but winner not clear yet, 0 = winner is 0, 1 = winner is 1 *)
    let choices0 = ref [] in
    let choices1 = ref [] in
    let next_start = ref 0 in

    let history = ref [] in
    let node = ref 0 in
    let found = ref false in
    let max_prio = ref 0 in
    let finished = ref false in
    let winner = ref plr_undef in

    while !next_start < l do

      (* create and record a play, then determine the winner *)
      while not !finished do
        if passed.(!node) >= 0 then
          (* current play has joined an old play for which the winner has been determined already *)
          begin
            winner := if passed.(!node) = 1 then plr_Odd else if passed.(!node) = 0 then plr_Even else plr_undef;
            finished := true
          end
        else if passed.(!node) = -1 then
          (* play has formed a loop, remember: !node is now the looping node in this play *)
          begin
            max_prio := pg_get_priority game !node;
            while not !found do
              let v = List.hd !history in
              history := List.tl !history;
              max_prio := max !max_prio (pg_get_priority game v);
              found := (v = !node)
            done;
            winner := plr_benefits !max_prio;
            finished := true
          end
        else
          (* play needs to be continued *)
          begin
            passed.(!node) <- -1;
            history := !node :: !history;
            let p = pg_get_owner game !node in
	    let choices = if p = plr_Even then choices0 else choices1 in
            choices := !node :: !choices;
            let str = if p = plr_Even then str0 else str1 in
            node := fst str.(!node)
          end
      done;
      
      (* record winner globally *)
      if !winner = plr_Even then incr wins0 else incr wins1;
      
      (* record winner locally *)
      let vs = if !winner = plr_Even then !choices0 else !choices1 in
      let str = if !winner = plr_Even then str0 else str1 in
      List.iter (fun v -> let (c,e) = str.(v) in
                          str.(v) <- (c,e+1);
                          passed.(v) <- if !winner = plr_Even then 0 else 1) vs;
      List.iter (fun v -> passed.(v) <- if !winner = plr_Even then 0 else 1) (if !winner = plr_Even then !choices1 else !choices0);


      (* tidy data structures *)
      history := [];
      found := false;
      choices0 := [];
      choices1 := [];
      winner := plr_undef;
      finished := false;

      (* find next node to start a play from *)
      while !next_start < l && passed.(!next_start) >= 0 do
        incr next_start
      done;
      node := !next_start
    done;
    ((str0,!wins0,g01),(str1,g10,!wins1))

end ;;


open Parameters ;;
open Pool ;;
open Genetics ;;

let solve' game =

  (* create initial population of strategies *)
  msg_tagged 2 (fun _ -> "Creating initial population of " ^ string_of_int !initial_population ^ " strategies.\n");
  Pool.init ();
  for i=1 to !initial_population do
    let estr = give_birth game in
    insert estr;
    msg_tagged 3 (fun _ -> (show estr) ^ "\n");
  done;
  msg_tagged 3 (fun _ -> "Genetic pool now has " ^ string_of_int (DynArray.length pool) ^ " strategies.\n");

  let sol = sol_create game in
  let wstr = ref [||] in

  (* iterate over competitions, and possibly procreations, mutations and tests until a winning strategy has been found *)
  let found = ref false in
  let counter = ref 0 in
  msg_tagged 2 (fun _ -> "Starting evolution ...\n");
  while not !found do
    msg_tagged 2 (fun _ -> "Entering round " ^ string_of_int !counter ^ " of the evolution.\n");

    (* competition *)
    msg_tagged 2 (fun _ -> "Selecting and removing two strategies for a competition.\n");
    let estr1 = select_and_remove RandomDistribution.normal in
    let estr2 = select_and_remove RandomDistribution.normal in
    let (estr1',estr2') = compete game estr1 estr2 in
    msg_tagged 3 (fun _ -> "Player 0 before: " ^ (show estr1) ^ "\n");
    msg_tagged 3 (fun _ -> "Player 0 after : " ^ (show estr1') ^ "\n");
    msg_tagged 3 (fun _ -> "Player 1 before: " ^ (show estr2) ^ "\n");
    msg_tagged 3 (fun _ -> "Player 1 after : " ^ (show estr2') ^ "\n");
    insert estr1';
    insert estr2';

    (* conditional mutation phase for some strategy *)
    if mutate_now !counter then
      begin
        msg_tagged 2 (fun _ -> "Selecting a strategy for a mutation.\n");
        let estr = select RandomDistribution.biased_high in
        let estr' = mutate game estr in
        msg_tagged 3 (fun _ -> "1) before: " ^ (show estr) ^ "\n");
        msg_tagged 3 (fun _ -> "1) after : " ^ (show estr') ^ "\n");
        insert estr'
      end;

    (* conditional procreation phase for some strategies *)
    if procreate_now !counter then
      begin
        msg_tagged 2 (fun _ -> "Selecting two strategies for procreation.\n");
        let estr1 = select RandomDistribution.biased_low in
        let estr2 = select RandomDistribution.biased_low in
        let estr = procreate estr1 estr2 in 
        msg_tagged 3 (fun _ -> "Parent 1: " ^ (show estr1) ^ "\n");
        msg_tagged 3 (fun _ -> "Parent 2: " ^ (show estr2) ^ "\n");
        msg_tagged 3 (fun _ -> "Child   : " ^ (show estr) ^ "\n");
        insert estr
      end;

    (* conditional creation phase for some new strategy *)
    if create_now !counter then
      begin
        msg_tagged 2 (fun _ -> "Given birth to a new strategy.\n");
        let estr = give_birth game in
        msg_tagged 3 (fun _ -> "Newbie: " ^ (show estr) ^ "\n");
        insert estr
      end;

    msg_tagged 3 (fun _ -> "The pool now has " ^ string_of_int (DynArray.length Pool.pool) ^ " strategies.\n");
    msg_tagged 3 (fun _ -> "Their global evaluations are [" ^
      String.concat ";" (List.map (fun (_,g0,g1) -> "(" ^ string_of_int g0 ^ "," ^ string_of_int g1 ^ ")") (DynArray.to_list Pool.pool))
      ^ "]\n"); 

    (* conditional test whether some strategy is winning *)
    if test_now !counter then
      begin
        msg_tagged 2 (fun _ -> "Selecting a strategy for test on being winning.\n");
        let estr = select RandomDistribution.very_biased_low in
        let (str,g0,g1) = estr in
        msg_tagged 3 (fun _ -> "Candidate: " ^ (show estr) ^ "\n");
 
        (* strip the evaluated strategy *)
        let str = Array.init (Array.length str) (fun i -> fst str.(i)) in

        (* not only choose a strategy randomly but also the player whom we consider *)
        let p = if g0 >= g1 then plr_Even else plr_Odd in
        msg_tagged 3 (fun _ -> "Looks like I should better start with the test for player " ^ plr_show p ^ ".\n");

        let ws = compute_winning_nodes 3 game str p in (* die 3 muss ersetzt werden!!! *)
        msg_tagged 3 (fun _ -> "The strategy is winning for player " ^ plr_show p ^ " on nodes {" ^ String.concat "," (List.map string_of_int ws) ^ "}.\n");
        if ws <> [] then
          begin
            found := true;
            wstr := str;
            List.iter (fun v -> sol.(v) <- p) ws
          end
        else 
          begin
            let p = plr_opponent p in
            msg_tagged 3 (fun _ -> "Alright. Will try player " ^ plr_show p ^ " as well, if I must.\n");
            let ws = compute_winning_nodes 3 game str p in (* die 3 muss ersetzt werden!!! *)
            msg_tagged 3 (fun _ -> "The strategy is winning on nodes {" ^ String.concat "," (List.map string_of_int ws) ^ "}.\n");
            if ws <> [] then
              begin
                found := true;
                wstr := str;
                List.iter (fun v -> sol.(v) <- p) ws
              end
          end
      end;

    incr counter
  done;
  (sol,!wstr)



module CommandLine = struct
  let speclist =  [(["--maxpoolsize"; "-mps"], Int(fun i -> Parameters.max_pool_size := i),
                      "<size>\n     set max pool size (default is 1000)");
                   (["--initpop"; "-ip"], Int(fun i -> Parameters.initial_population := i),
                      "<size>\n     set initial population size (default is 10)");
                   (["--mutationprob"; "-mp"], Float(fun f -> Parameters.mutation_prob := f),
                      "<probability>\n     set mutation probability (default is 0.02)");
                   (["--procreationprob"; "-pcp"], Float(fun f -> Parameters.procreation_prob := f),
                      "<probability>\n     set procreation probability (default is 0.01)");
                   (["--creationprob"; "-cp"], Float(fun f -> Parameters.creation_prob := f),
                      "<probability>\n     set creation probability (default is 0.02)");
                   (["--initpop"; "-ip"], Int(fun i -> Parameters.test_breakpoint := i),
                      "<iterations>\n     set test breakpoint iterations (default is 1000)")
                      ]

  let parse s = 
  	SimpleArgs.parsearr s speclist (fun _ -> ()) "Genetic Algorithm\n" SimpleArgs.argprint_help SimpleArgs.argprint_bad

end ;;


open CommandLine ;;



let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;



let register _ = Solverregistry.register_solver_factory (fun s -> parse s; solve) "genetic" "gn" "use a genetic algorithm (aka heuristic)";;

