
(*******************************************************************************
 *                                                                             *
 * The Delayed Promotion Procedure                                             *
 *                                                                             *
 * from:                                                                       *
 * M. Benerecetti, D. Dell'Erba, and F. Mogavero.                              *
 * A Delayed Promotion Policy for Parity Games,                                *
 * GandALF 2016, EPTCS 226, pp. 30-45, 2016                                    *
 *                                                                             *
 ******************************************************************************)

open Basics;;
open Paritygame;;
open Univsolve;;
open Paritygamebitset;;
open FSet;;
open FMap;;

let not_none = function
    None -> false
  |	_ -> true;;

let query game region r p player was_open att_qP att_qO subgame_vr reg_strategy vr l =
  let r_dom = bit_dom_array_bound r l p in
  if was_open = 1
  then BitSet.differentiate subgame_vr !region
  else (
    for i = 0 to l - 1 do
      if (BitSet.is_set vr i) && not (BitSet.is_set r_dom i)
      then BitSet.set subgame_vr i
      else BitSet.unset subgame_vr i
    done;
  );
  player := plr_benefits p;
  let target = BitSet.create l in
  for i = 0 to l - 1 do
    reg_strategy.(i) <- -1;
    if r.(i) = p
    then BitSet.set target i
  done;
  let reg_en = BitSet.enum target in
  region := attr_bit game subgame_vr att_qP att_qO reg_strategy !player target (Enum.clone reg_en);
  (target,reg_en,BitSet.enum r_dom);;

let dispatcher game region r p player f max_f delay_even delay_odd even_set odd_set pre r_enum target target_enum max_col r_strategy subgame_vr reg_strategy vr l =
  let pre_set = BitSet.diff vr region in
  Enum.iter (fun v -> BitSet.unset pre_set v) (Enum.clone r_enum);
  Enum.iter (fun i ->
      let pl = pg_get_owner game i in
      let ws = pg_get_successors game i in
      if pl = player
      then (
        if r_strategy.(i) = -1
        then (
          let w = ns_fold (fun b -> fun w -> if (b > -1 || not (BitSet.is_set vr w) || not (BitSet.is_set region w)) then b else w) (-1) ws in
          if w > -1 then reg_strategy.(i) <- w;
        )
        else reg_strategy.(i) <- r_strategy.(i);
      )
    ) target_enum;
  pre_bit game pre subgame_vr (plr_opponent player) pre_set l;
  if not (is_empty_bset (BitSet.inter pre target) l)
  then (
    Enum.iter(fun i -> f.(i) <- p;) (BitSet.enum region);
    1
  )
  else (
    let r_pl = BitSet.create l in
    if player = plr_Even
    then Enum.iter (fun i -> if r.(i) mod 2 = 0 then BitSet.set r_pl i) r_enum
    else Enum.iter (fun i -> if r.(i) mod 2 = 1 then BitSet.set r_pl i) r_enum;
    let min = ref (max_col) in
    Enum.iter (fun v ->
        let ws = pg_get_successors game v in
        ns_iter (fun w -> if BitSet.is_set vr w && BitSet.is_set r_pl w
                  then (
                    if f.(w) < !min
                    then min := f.(w);
                  )
                ) ws;
      ) (BitSet.enum region);
    if !min <> (max_col)
    then (
      let check = ref false in
      let over = if player = plr_Even
        then FSet.min_elt odd_set
        else FSet.min_elt even_set in
      if over < Some !min && not_none over
      then check := true;
      if not !check && (not (FMap.is_empty !delay_even) || not (FMap.is_empty !delay_odd))
      then (
        FMap.iter (fun a b -> if not !check && !min >= a && !min <= b then check := true) !delay_even;
        FMap.iter (fun a b -> if not !check && !min >= a && !min <= b then check := true) !delay_odd;
      );
      if !check
      then (
        for i = 0 to l - 1 do
          if BitSet.is_set region i
          then f.(i) <- !min;
        done;
        if !max_f < !min
        then max_f := !min;
        if Array.fold_left (fun b -> fun w -> b || w < p && w > -1) false f
        then (
          if p mod 2 = 0
          then delay_even := FMap.add p !min !delay_even
          else delay_odd := FMap.add p !min !delay_odd;
          1
        )
        else -1;
      )
      else (!min;)
    )
    else 0;
  );;

let successor game region r p player action f max_f delay_even delay_odd even_set odd_set r_strategy reg_strategy vr l =
  if action = 1
  then (
    let new_p = ref 0 in
    for i = 0 to l - 1 do
      if BitSet.is_set region i
      then r.(i) <- !p;
      if reg_strategy.(i) <> -1
      then r_strategy.(i) <- reg_strategy.(i);
      if r.(i) < !p && r.(i) > !new_p
      then new_p := r.(i);
    done;
    p := !new_p;
  )
  else (
    if action = -1
    then (
      p := !max_f;
      for i = 0 to l - 1 do
        if BitSet.is_set vr i && f.(i) < !p && f.(i) mod 2 <> !p mod 2
        then (
          let pr = pg_get_priority game i in
          r_strategy.(i) <- -1;
          r.(i) <- pr;
          f.(i) <- pr;
        )
        else r.(i) <- f.(i);
        if reg_strategy.(i) <> -1
        then r_strategy.(i) <- reg_strategy.(i);
      done;
      even_set := FSet.empty;
      odd_set := FSet.empty;
      delay_odd := FMap.empty;
      delay_even := FMap.empty;
      max_f := 0;
    )
    else (
      let old_p = !p in
      p := action;
      for i = 0 to l - 1 do
        if BitSet.is_set region i
        then (
          r.(i) <- !p;
          f.(i) <- !p;
        )
        else (
          if BitSet.is_set vr i && r.(i) < !p && plr_benefits r.(i) <> player
          then (
            let pr = pg_get_priority game i in
            r.(i) <- pr;
            f.(i) <- pr;
            r_strategy.(i) <- -1;
          );
        );
        if reg_strategy.(i) <> -1
        then r_strategy.(i) <- reg_strategy.(i);
      done;
      if player = plr_Even
      then (
        even_set := FSet.remove old_p !even_set;
        even_set := FSet.add !p !even_set
      )
      else (
        odd_set := FSet.remove old_p !odd_set;
        odd_set := FSet.add !p !odd_set
      );
      if !max_f < !p
      then max_f := 0;
    );
  );;

let search game =
  let tot_query = ref 0 in
  let tot_promo = ref 0 in
  let queries = ref 0 in
  let promo = ref 0 in
  let max_query = ref 0 in
  let max_promo = ref 0 in
  let wr_count = ref 0 in
  let l = pg_size game in
  let reg_strategy = Array.make l (-1) in
  let r_strategy = Array.make l (-1) in
  let solution = sol_create game in
  let str_out = Array.make l (-1) in
  let empty = ref false in
  let p = ref (-1) in
  let r = Array.make l (-1) in
  let region = ref (BitSet.create l) in
  let vr = collect_nodes_bit game l in
  let subgame_vr = BitSet.copy vr in
  let node_count = ref 0 in
  let pre = BitSet.create l in
  let att_qP = Queue.create () in
  let att_qO = Queue.create () in
  let action = ref 0 in
  let player = ref plr_undef in
  let f = Array.make l (-1) in
  let max_f = ref 0 in
  let even_set = ref FSet.empty in
  let odd_set = ref FSet.empty in
  let delay_even = ref FMap.empty in
  let delay_odd = ref FMap.empty in
  for i = 0 to l - 1 do
    if BitSet.is_set vr i
    then (
      incr node_count;
      let pr = pg_get_priority game i in
      r.(i) <- pr;
      f.(i) <- pr;
      if pr > !p then p := pr;
    );
  done;
  let max_col = ref (1 + !p) in
  while not !empty do
    incr tot_query;
    incr queries;
    let (target',tar_en',rd_en') = query game region r !p player !action att_qP att_qO subgame_vr reg_strategy vr l in
    action :=  dispatcher game !region r !p !player f max_f delay_even delay_odd !even_set !odd_set pre rd_en' target' tar_en' !max_col r_strategy subgame_vr reg_strategy vr l;
    while !action <> 0 do
      incr tot_query;
      incr queries;
      if !action > 1
      then (
        incr tot_promo;
        incr promo;
      );
      successor game !region r p !player !action f max_f delay_even delay_odd even_set odd_set r_strategy reg_strategy vr l;
      let (target,tar_en,rd_en) = query game region r !p player !action att_qP att_qO subgame_vr reg_strategy vr l in
      action :=  dispatcher game !region r !p !player f max_f delay_even delay_odd !even_set !odd_set pre rd_en target tar_en !max_col r_strategy subgame_vr reg_strategy vr l;
    done;
    if !promo > !max_promo
    then max_promo := !promo;
    if !queries > !max_query
    then max_query := !queries;
    incr wr_count;
    if BitSet.count !region = !node_count
    then (
      empty := true;
      for i = 0 to l - 1 do
        if reg_strategy.(i) <> -1
        then str_out.(i) <- reg_strategy.(i);
        if BitSet.is_set !region i
        then sol_set solution i !player;
      done;
    )
    else (
      let region' = attr_bit game vr att_qP att_qO reg_strategy !player !region (BitSet.enum !region) in
      node_count := 0;
      empty := true;
      p := 0;
      action := 0;
      for i = 0 to l - 1 do
        if reg_strategy.(i) <> -1
        then str_out.(i) <- reg_strategy.(i);
        if BitSet.is_set region' i
        then solution.(i) <- !player;
        r_strategy.(i) <- -1;
        if solution.(i) = plr_undef && BitSet.is_set vr i
        then (
          incr node_count;
          empty := false;
          let pr = pg_get_priority game i in
          r.(i) <- pr;
          f.(i) <- pr;
          if pr > !p then p := pr;
          BitSet.set subgame_vr i;
        )
        else (
          r.(i) <- -1;
          f.(i) <- -1;
          BitSet.unset vr i;
          BitSet.unset subgame_vr i;
        );
      done;
      player := plr_undef;
      max_col := 1 + !p;
      promo := 0;
      queries := 0;
      max_f := 0;
      even_set := FSet.empty;
      odd_set := FSet.empty;
      delay_even := FMap.empty;
      delay_odd := FMap.empty;
    );
  done;
  (solution,str_out,!tot_query,!tot_promo,!max_query,!max_promo,!wr_count);;

let ppdelaysolve game =
  let msg_tagged v = message_autotagged v (fun _ -> "DP") in
  let (solution,strategy,queries,proms,maxq,maxp,wr) = search game in
  msg_tagged 2 (fun _ -> "\n");
  msg_tagged 2 (fun _ -> "Total number of queries: " ^ string_of_int queries ^ "\n");
  msg_tagged 2 (fun _ -> "Total number of promotions: " ^ string_of_int proms ^ "\n");
  msg_tagged 2 (fun _ -> "Maximum number of queries: " ^ string_of_int maxq ^ "\n");
  msg_tagged 2 (fun _ -> "Maximum number of promotions: " ^ string_of_int maxp ^ "\n");
  msg_tagged 2 (fun _ -> "Number of dominions: " ^ string_of_int wr ^ "\n");
  (solution,strategy);;

let solve game = ppdelaysolve game;;



let solveuniv game =
  let opt = (universal_solve_init_options_verbose !universal_solve_global_options) in
  universal_solve opt ppdelaysolve game;;

let register _ =
    Solverregistry.register_solver solve "priopromdel" "dp" "use the Delayed Promotion procedure";
    Solverregistry.register_solver solveuniv "priopromdeluniv" "dpuniv" "use the Delayed Promotion procedure integrated with the universal solver";;
