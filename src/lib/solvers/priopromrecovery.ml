
(*******************************************************************************
 *                                                                             *
 * The Region Recovery Procedure                                               *
 *                                                                             *
 * from:                                                                       *
 * M. Benerecetti, D. Dell'Erba, and F. Mogavero.                              *
 * Improving Priority Promotion for Parity Games,                              *
 * HVC 2016, LNCS 10028, Springer, pp. 1-17, 2016.                             *
 *                                                                             *
 ******************************************************************************)

open Basics;;
open Paritygame;;
open Univsolve;;
open Paritygamebitset;;


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

let dispatcher game region r player pre r_enum target target_enum max_col r_strategy subgame_vr reg_strategy vr l =
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
  then 1
  else (
    let r_pl = BitSet.create l in
    if player = plr_Even
    then Enum.iter (fun i -> if r.(i) mod 2 = 0 then BitSet.set r_pl i) r_enum
    else Enum.iter (fun i -> if r.(i) mod 2 = 1 then BitSet.set r_pl i) r_enum;
    let min = ref (max_col) in
    Enum.iter (fun v ->
        let ws = pg_get_successors game v in
        let color = ref (max_col) in
        ns_iter (fun w -> if BitSet.is_set vr w && BitSet.is_set r_pl w
                  then (
                    if r.(w) < !color
                    then (
                      color := r.(w);
                      if r.(w) < !min
                      then min := r.(w);
                    )
                  );
                ) ws;
      ) (BitSet.enum region);
    if !min <> (max_col)
    then !min
    else 0;
  );;

let successor game region r p action r_strategy reg_strategy l =
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
    let reg_check = ref true in
    let reg_search = ref true in
    let j = ref 0 in
    while !reg_search do
      while !j < l && !reg_check do
        if r.(!j) = !p
        then (
          let pr = pg_get_priority game !j in
          let pl = pg_get_owner game !j in
          let ws = pg_get_successors game !j in
          if pl = plr_benefits !p
          then (
            if r_strategy.(!j) > -1 && r.(r_strategy.(!j)) <> !p
            then reg_check := false;
          )
          else (
            if pr <> !p
            then reg_check := ns_fold (fun b -> fun w -> b && r.(w) >= !p) true ws;
          );
        );
        incr j;
      done;
      j := 0;
      if !reg_check
      then reg_search := false
      else (
        reg_check := true;
        new_p := 0;
        for i = 0 to l - 1 do
          if r.(i) = !p
          then (
            let pr = pg_get_priority game i in
            r.(i) <- pr;
            r_strategy.(i) <- -1;
          );
          if r.(i) <= !p && r.(i) > !new_p
          then new_p := r.(i);
        done;
        if !new_p = !p
        then reg_search := false
        else p := !new_p;
      );
    done;
  )
  else (
    p := action;
    for i = 0 to l - 1 do
      if BitSet.is_set region i
      then r.(i) <- !p;
      if reg_strategy.(i) <> -1
      then r_strategy.(i) <- reg_strategy.(i);
    done;
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
  for i = 0 to l - 1 do
    if BitSet.is_set vr i
    then (
      incr node_count;
      let pr = pg_get_priority game i in
      r.(i) <- pr;
      if pr > !p then p := pr;
    );
  done;
  let max_col = ref (1 + !p) in
  while not !empty do
    incr tot_query;
    incr queries;
    let (target',tar_en',rd_en') = query game region r !p player !action att_qP att_qO subgame_vr reg_strategy vr l in
    action := dispatcher game !region r !player pre rd_en' target' tar_en' !max_col r_strategy subgame_vr reg_strategy vr l;
    while !action <> 0 do
      incr tot_query;
      incr queries;
      if !action > 1
      then (
        incr tot_promo;
        incr promo;
      );
      successor game !region r p !action r_strategy reg_strategy l;
      let (target,tar_en,rd_en) = query game region r !p player !action att_qP att_qO subgame_vr reg_strategy vr l in
      action := dispatcher game !region r !player pre rd_en target tar_en !max_col r_strategy subgame_vr reg_strategy vr l;
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
          if pr > !p then p := pr;
          BitSet.set subgame_vr i;
        )
        else (
          r.(i) <- -1;
          BitSet.unset vr i;
          BitSet.unset subgame_vr i;
        );
      done;
      player := plr_undef;
      max_col := 1 + !p;
      promo := 0;
      queries := 0;
    );
  done;
  (solution,str_out,!tot_query,!tot_promo,!max_query,!max_promo,!wr_count);;

let pprecoverysolve game =
  let msg_tagged v = message_autotagged v (fun _ -> "RR") in
  let (solution,strategy,queries,proms,maxq,maxp,wr) = search game in
  msg_tagged 2 (fun _ -> "\n");
  msg_tagged 2 (fun _ -> "Number of queries: " ^ string_of_int queries ^ "\n");
  msg_tagged 2 (fun _ -> "Number of promotions: " ^ string_of_int proms ^ "\n");
  msg_tagged 2 (fun _ -> "Maximum number of queries: " ^ string_of_int maxq ^ "\n");
  msg_tagged 2 (fun _ -> "Maximum number of promotions: " ^ string_of_int maxp ^ "\n");
  msg_tagged 2 (fun _ -> "Number of dominions: " ^ string_of_int wr ^ "\n");
  (solution,strategy);;

let solve game = pprecoverysolve game;;


let solveuniv game =
  let opt = (universal_solve_init_options_verbose !universal_solve_global_options) in
  universal_solve opt pprecoverysolve game;;


let register _ =
    Solverregistry.register_solver solve "priopromrec" "rr" "use the Region Recovery procedure";
    Solverregistry.register_solver solveuniv "priopromrecuniv" "rruniv" "use the Region Recovery procedure integrated with the universal solver";;
