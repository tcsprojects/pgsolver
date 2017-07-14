(* Copyright 2017 Patrick Totzke
 * This file is released under the BSD licence. For details see https://opensource.org/licenses/BSD-3-Clause.
 *
 * This is a (completely imperative) implementation of Jurdzinski/Lazic's
 * Succinct small progress measure algorithm: https://arxiv.org/abs/1702.05051 .
 *)

open Basics ;;
open Paritygame ;;
open Univsolve;;
open Tcsqueue;;


(* -------------  AUXILIARY FUNCTIONS -------------------------------------- *)
(* define local logging functions *)
let log_debug msg = message_autotagged 3 (fun _ -> "SSMP") (fun _ -> msg ^ "\n") ;;
let log_verb msg = message_autotagged 2 (fun _ -> "SSMP") (fun _ -> msg ^ "\n") ;;
let log_info msg = message_autotagged 2 (fun _ -> "SSMP") (fun _ -> msg ^ "\n") ;;


(* define logarithm base 2. Thanks for nothing ocaml. *)
let ld x = int_of_float(ceil (log (float_of_int x) /. log 2.0));;


(* find the largest index <= start in an array such that the corresponding *)
(* element satisfies the given predicate *)
let rec find_lastindex_with a pred start =
    if start = -1 then -1 else
    if pred a.(start) then start else
        find_lastindex_with a pred (start-1)
;;


(* This renders a Paritygame.nodeset as e.g. as "{1,2,3,4}".
 * We'll use it when logging the successors of a node. *)
let ns_format nodeset =
    let commajoin l r = match l with
    | "" -> r
    | _ -> l ^ "," ^ r in
    let stringtuple = List.map string_of_int (ns_nodes nodeset) in
    "{" ^ (List.fold_left commajoin "" stringtuple) ^ "}"
;;



(* -------------  BITSTRINGS ---------------------------------------------- *)
(* Our procedure is based on a particular ordering on bitstrings.
 * We define bitstrings as arrays of Bools; the ordering is defined
 * in the function bitstring_compare below.
 *)
type bitstring = bool array;;


(* use the alias "eps" to denote the the empty bitstring *)
let eps = [||];;


(* format a bitstring as a normal string for logging *)
let bitstring_format bs =
    if bs = eps
      then "ε"
      else
        let bool_to_char b = if b then '1' else '0'in
        String.init (Array.length bs) (fun i -> (bool_to_char bs.(i)))
;;


(* bitstring comparison. This returns -1,0 or 1, as the standard "compare" function
 *
 * For every but b and bitstrings s,s' we have
 * 1) 0s < eps
 * 2) eps < 1s
 * 3) bs < bs' <=> s < s'
 *)
let rec bitstring_compare x y =
  let length_x = Array.length x in
  let length_y = Array.length y in
  (* recursively traverse x and y from left to right, starting at index i *)
  let rec aux i =
      if i < length_x then (* x has element x.(i) *)
          if i < length_y then (* y has element y.(i) *)
              if x.(i) <> y.(i) then compare x.(i) y.(i)
              else aux (i+1)
          else (* y.(i)=eps, x not *)
              if x.(i) then 1 else -1
      else (* x.(i)=eps *)
          if i < length_y then (* y has element y.(i), x.(i)=eps*)
              if y.(i) then -1 else 1
          else (* y.(i)=eps = x.(i) *)
              0
  in
  aux 0
;;


(* aliases for <, =, > *)
let bitstring_less x y = (bitstring_compare x y) = -1;;
let bitstring_greater x y = (bitstring_compare x y) = 1;;
let bitstring_equal x y = (bitstring_compare x y) = 0;;

(* -------------  END OF BITSTRING DEFINITIONS --------------- *)



(* -------------- ADAPTIVE COUNTERS -------------------------- *)
(* An adaptive counters is either Top or an array of bitstrings.
 * The length of the tuple is unspecified in the type definition and formatting;
 * All other functions work on "h-counters": tuples of length h.
 * *)
type adaptivecounter = Top | AC of bitstring array;;


(* format adaptive counters in tuple notation for human consumption *)
let ac_format ac =
    let commajoin l r = match l with
    | "" -> r
    | _ -> l ^ "," ^ r in
    match ac with
    | Top -> "T"
    | AC bitstrings ->
        let stringtuple = Array.map bitstring_format bitstrings in
        "(" ^ (Array.fold_left commajoin "" stringtuple) ^ ")"
;;


(* compare adaptive h-counters lexicographically based on bitstring comparisons.
 *
 * INPUTS:
 * - integers h and p,
 * - adaptive h-counters x and y. These are either Top or bitstrings of length h.
 * OUTPUT: -1, 0 or 1
 * The comparison is only for the components corresponding to priorities >= p.
 * This means that we compare only prefixes of length h-(p/2).
 *)
let ac_compare h p x y =
    match (x,y) with
    | (Top, Top) -> 0  (* hardcode results for artificial top element *)
    | (Top, _) -> 1
    | (_, Top) -> -1
    | (AC acx, AC acy) ->
        (* use bitstring comparisons lexicographically otherwise *)
        let maxindex = h - (p/2) in (* truncate after the maximal interesting index*)
        let rec aux i =  (* compare indices from left to right *)
            if i = maxindex
            then 0
            else  (* use bitstrin comparison *)
              if acx.(i) <> acy.(i)
              then (* done if bitstrings are different *)
                  bitstring_compare acx.(i) acy.(i)
              else (* otherwise look at next index*)
                  aux (i+1)
        in
        aux 0
;;


(* aliases for <, =, >.
 * These are actually operations <|_p, =|_p and >|_p for ataptive h-counters from the paper.
 *)
let ac_less h p x y = (ac_compare h p x y) = -1;;
let ac_greater h p x y = (ac_compare h p x y) = 1;;
let ac_equal h p x y = (ac_compare h p x y) = 0;;


(* computes the combined length of the bistrings in all components
corresponding to parities >=p, in the adaptive h-counter ac.
Unless ac=Top, this means summing up the lengths of the first h-(p/2) entries.
*)
let ac_bitstring_length h p ac =
    match ac with
    | Top -> 0
    | AC bitstrings ->
            let sum = ref 0 in
            for i = 0 to (h-(p/2))-1 do
                sum := !sum + (Array.length bitstrings.(i))
                done;
            !sum
;;


(* truncate adaptive h-counter for priority p.
 * This means replacing the last (p/2) components by eps *)
let ac_truncate h p ac =
    match ac with
    | Top -> Top
    | AC bitstrings ->
    let pindex = h - (p/2)-1 in
    AC (Array.init h (fun i -> if i > pindex then [||] else bitstrings.(i)))
;;


(* compute the minimal l-bouded adaptive h-counter:
* this is of the form (000..0,eps,eps,...eps), where the first component has l zeros
* and the remaining h-1 components are eps. *)
let ac_min l h = AC (Array.append
                [|(Array.make l false)|] (* lx0 on the first position *)
                (Array.make (h-1) eps)  (* eps on the last h-1 positions *)
);;


(* compute the least l-bounded adaptive h-counter p-above a given counter ac. *)
let ac_least_above l h p ac =
    match ac with
    | Top -> Top      (* If the given ac is Top, just return Top. *)
    | AC original ->  (* do some work otherwise *)

    (* identify the largest array index after truncation. *)
    (* if p-truncation yields the empty tuple the next p-larger counter is Top. *)
    let pindex = h - (p/2)-1 in
    if pindex = -1
    then
        Top
    else
        (
        (* define some arbitrary h-counter below Top to write to. *)
        let resultarr = Array.copy original in
        let result = ref Top in  (* this is the default return value *)


        (* There are three cases, depending on the total length of the bitstrings,
         * and on the last non-empty component of the counter *)
        let bitstring_length = ac_bitstring_length h p ac in
        if bitstring_length < l
        then (
            (* CASE 1: the total bitstring length can be extended *)
            (* update resultarr.pindex  to  original.pindex · 100..0 *)
            let zeros = Array.make (l-(bitstring_length)-1) false in
            resultarr.(pindex) <- Array.concat [original.(pindex); [|true|]; zeros];
            (* set all following elemts to eps *)
            Array.fill resultarr (pindex+1) (h-pindex-1) eps;
            result := AC resultarr;
        )
        else ( (* the total bitstring is already of maximal length *)
            (* find the last nonempty bitstring of the ac before or at pindex *)
            (* This exists since bitstring_length = l. *)
            let lne_index = find_lastindex_with original (fun x -> x!=eps) pindex in
            let lne = original.(lne_index) in
            let lne_length = Array.length lne in
            log_debug ("lne_index: " ^ (string_of_int lne_index  ));

            (* find the last bit 0 in this nonempty bistring *)
            let zeroindex = find_lastindex_with lne (fun x -> x=false)
                                                    (Array.length lne-1) in
            log_debug ("zeroindex: " ^ (string_of_int lne_index  ));

            if zeroindex > -1
            then (
                (* CASE 2: the total length is l and the least non-empty component has zeros. *)
                (* copy the prefix before the last zero to resultarr *)
                resultarr.(lne_index) <- Array.init (zeroindex) (fun i -> lne.(i));
                (* unless lne_index is maximal, add zeros to the next larger index *)
                if lne_index < h-1
                then(
                    let suffix_length = (lne_length - zeroindex) in
                    let zeros = Array.make suffix_length false in
                    resultarr.(lne_index+1) <- zeros;
                    log_debug ("suffix: " ^ (bitstring_format zeros));
                    log_debug ("resultarr: " ^ (ac_format (AC resultarr)));
                )
                else ();
                (* fill the remaining indices with eps to get a h-tuple *)
                if lne_index < h -2
                then
                    Array.fill resultarr (lne_index+2) (h-lne_index-2) eps
                else ();
                result := AC resultarr;  (* set result for this case *)
            )
            else (
                (* CASE 3: the total length is l and the least non-empty component has NO zeros. *)
                (* lne is of the form 111..1 *)
                if lne_index = 0
                then
                    (* we have l 1's on the first position, the next higher up is Top *)
                    result := Top
                else
                (
                    (* if lne is at position (j+1)>0, set resultarr.j to (original.j 1 0000) *)
                    resultarr.(lne_index-1) <- Array.concat [
                        original.(lne_index-1);
                        [|true|];
                        (Array.make (lne_length-1) false)
                    ];
                    (* fill the remaining indices with eps to get a h-tuple *)
                    Array.fill resultarr (lne_index) (h-lne_index) eps;
                result := AC resultarr;  (* set result for this case *)
                );
            );
        );
        log_debug ("the least "
          ^ (string_of_int h) ^ "-counter "
          ^ (string_of_int p) ^ "-above "
          ^ (ac_format (AC original))
          ^ " is " ^ (ac_format !result));
        !result
        );
;;
(* -------------- END OF ADAPTIVE COUNTERS DEFINITIONS ---------------- *)



(* -------------- PROGRESS MEASURES ----------------------------------- *)
(* A progress measure maps each of the n states to an adaptive counter. We
 * represent this as (n-1)-array of adaptive counters *)
type progressmeasure = adaptivecounter array;;


(* format progress measures for logging *)
let pm_format mu =
    let stringtuple = Array.mapi (fun i v -> (string_of_int i) ^ " -> " ^ (ac_format v) ^ "\n") mu in
    Array.fold_left (^) "" stringtuple
;;
(* -------------- END OF PROGRESS MEASURE DEFINITIONSS ---------------- *)



(* -------------- MAIN SOLVER ----------------------------------------- *)
let solve' game =
    log_debug "Now solving the following subgame:";
    log_debug (format_game game);

    (* compute some constants from the game *)
    let n = pg_size game in               (* number of vertices *)
    let maxprio = pg_max_prio game in     (* number of priorities *)
    let d = maxprio + (maxprio mod 2) in  (* largest even number >= maxprio *)
    let l = ld n in                       (* maximal length of bitstrings *)
    let h = (d/2) in                      (* length of the counters *)

    log_info ("The game has "
      ^ (string_of_int n) ^ " states with maximal priority "
      ^ (string_of_int maxprio)
      ^ ". We are looking at "
      ^ (string_of_int l) ^ "-bounded adaptive "
      ^ (string_of_int h) ^ "-counters. "
    );

    (* compute lift(mu,v,w): the least s >= v progressive in mu[v->s] *)
    (* here, mu is a progress measure, and v and w are nodes (integers) *)
    let lift mu v w =
        let vprio = pg_get_priority game v in
        log_debug ("computing lift for nodes "
              ^ (nd_show v) ^ " with measure " ^ (ac_format mu.(v))
              ^" and node "
              ^ (nd_show w) ^ " with measure " ^ (ac_format mu.(w))
              );
        log_debug ("mu(v) : " ^ (ac_format mu.(v)) ^ " truncated is " ^(ac_format (ac_truncate h vprio mu.(v))) ^
                    "mu(w) : " ^ (ac_format mu.(w)) ^ " truncated is " ^(ac_format (ac_truncate h vprio mu.(w))));

        let res = ref Top in
        if even vprio
        then (
            (* v has even prio *)
            if (ac_less h vprio mu.(v) mu.(w))
            then (
                res := if mu.(w) = Top then Top else (ac_truncate h vprio mu.(w));
                log_debug ("truncate for prio " ^ (string_of_int vprio));
            )
            else (
                res := mu.(v);
                log_debug ("priority was even, v's AC was >= w's AC
                            mu(v): " ^ (ac_format mu.(v)) ^ "
                            mu(w): " ^ (ac_format mu.(w)));
            )
        )
        else (
            (* v has odd prio *)
            if (ac_greater h vprio mu.(v) mu.(w))
            then (
                res := mu.(v);
                log_debug ("priority was odd, v's AC was > w's AC
                            mu(v): " ^ (ac_format mu.(v)) ^ "
                            mu(w): " ^ (ac_format mu.(w)));
            )
            else (
                res := (ac_least_above l h vprio mu.(w));
                log_debug ("priority was odd, mu(v) <= mu(w) :. least AC above mu(w) returned");
            )
        );
        log_debug ("lift of "
        ^ (ac_format mu.(v))
        ^ " and "
        ^ (ac_format mu.(w))
        ^ " is "
        ^ (ac_format !res)
        );
        !res
    in

    (* compute the pair (w,lift(mu,v,w)) - a successor of v and its lifting -
     * which maximizes or minimizes the second component (depending on who owns node v)
     * among all successors of v.
     *)
    let best_successor_lift mu v =
        let vplayer = pg_get_owner game v in
        let succs = pg_get_successors game v in

        (* define order on pairs (node, lift(mu,v,node)) based on the
         * players preference for the second component: Odd wants to maximize. *)
        let better (a, lift_a) (b,lift_b) =
            (if vplayer = plr_Odd then ac_greater else ac_less) h 0 lift_a lift_b in
        let first_elem = if vplayer = plr_Odd then (v,mu.(v)) else (v,Top) in
        (* map list of successors to list of pairs (node, lift(mu,v node)) and
         * reduce to best pair, starting with the current measure for state v. *)
        List.fold_left (fun a b -> if better a b then a else b)
                       first_elem
                       (List.map (fun w -> (w, (lift mu v w))) (ns_nodes succs))
    in

    (* START OF REFINEMENT PROCEDURE *)
    log_info ("Init small progress measure..\n");
    (* initialize the progress measure with minimal counters for all states *)
    let mu = Array.make n (ac_min l h) in
    log_debug (pm_format mu);

    (* create and initialize queue of dirty states
     * the queue itself ensures that each element occurs at most once. *)
    let queue = SingleOccQueue.create () in
    for i = 0 to n - 1 do
        SingleOccQueue.add i queue;
    done;

    (* update until queue is empty *)
    log_info ("refining..");
    while not (SingleOccQueue.is_empty queue) do
        let v = SingleOccQueue.take queue in
        let vplayer = pg_get_owner game v in
        let succs = pg_get_successors game v in
        log_verb ("Dequeued state: " ^ (string_of_int v)
                   ^ ". Owner: " ^ (plr_show vplayer)
                   ^ " successors: " ^ ns_format succs
        );

        if ns_size succs > 0  (* only change mu if v has successors *)
        then (
            let candidate, candidate_lift = best_successor_lift mu v in
            log_verb ("candidate "
              ^ (nd_show candidate)
              ^ " with lift: " ^ (ac_format candidate_lift)
            );

            if (candidate_lift != mu.(v))
            then (
                mu.(v) <- candidate_lift;
                log_verb ("Updating progress measure.");
                log_debug (pm_format mu);
                let predecessors = (pg_get_predecessors game v) in
                log_verb ("Enqueuing predecessors of "
                  ^ (string_of_int v)
                  ^ " : " ^ (ns_format predecessors)
                );
                ns_iter (fun j -> SingleOccQueue.add j queue) predecessors;
            )
            else (
                log_debug ("candidate measure "
                  ^ (ac_format candidate_lift)
                  ^ " is not bigger than current measure "
                  ^ (ac_format mu.(v))
                  ^ ". No update."
                  );
            );
        )
        else (
        log_debug ("State " ^ (string_of_int v) ^ "has no successors");
        );
    done;

    log_verb ("Final progress measure: \n" ^ (pm_format mu));

    (* PREPARE OUTPUT *)
    log_info ("extract winning set..");

    (* Derive winning set from the SMP:
     * Odd wins all states with measure Top. *)
    let sol = sol_create game in
    for i = 0 to n - 1 do
        log_debug ("Checking " ^ string_of_int i);
        sol.(i) <- if mu.(i) = Top then plr_Odd else plr_Even;
    done;

    log_info ("extract player 0 strategy..");
    let strat = Array.make n nd_undef in
    for i = 0 to n - 1 do
        if ((pg_get_owner game i) = plr_Even)
        then
            (* Even picks a successor with minimal measure.
             * We re-use function "best_successor_lift", which recomputes
             * the measures of successors and returns a minimal one. *)
            let bestsucc, _ = best_successor_lift mu i in
            strat.(i) <- bestsucc
        else
            strat.(i) <- nd_undef
    done;

    (* TODO: get player 1 strategy by solving the dual.. *)

  (* Return with a winning set and winning strategy. *)
  (sol, strat)
;;
(* -------------- END OF MAIN SOLVER ----------------------------------- *)



let invert_game game =
    pg_init (pg_size game) (fun i ->
        1 + pg_get_priority game i,
        plr_opponent (pg_get_owner game i),
        ns_nodes (pg_get_successors game i),
        pg_get_desc game i
    )

let solve_for_player player solver game =
    let (sol, strat) = solver game in
    let (subgame_other_player, map_to_sub, map_to_game) = subgame_by_node_filter game (fun i -> sol.(i) != player) in
    if (pg_size subgame_other_player > 0) then (
        let subgame_other_player = invert_game subgame_other_player in
        let (_, strat') = solver subgame_other_player in
        Array.iteri (fun i j ->
            if (pg_get_owner subgame_other_player i = player)
            then strat.(map_to_game i) <- map_to_game j
        ) strat'
    );
    (sol, strat)


(* wrap a universal solver around our implementation *)
let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) (solve_for_player plr_Even solve') game;;
(* register with pgsolver *)

let register _ = Solverregistry.register_solver solve "succinctsmallprog" "sspm" "use the succinct small progress measure algorithm of Jurdzinski/Lazic";;