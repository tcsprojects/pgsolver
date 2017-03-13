open Basics;;
open Paritygame;;

let is_empty_bset bset length =
	let i = ref 0 in
	let empty = ref true in
	while !i < length && !empty do
	  if BitSet.is_set bset !i
	  then empty := false
	  else incr i;
	done;
	!empty;;

let bit_dom_array_bound array length bound =
	let dom = BitSet.create length in
	for i = 0 to length - 1 do
	  if array.(i) > bound
	  then BitSet.set dom i
	done;
	dom;;

let collect_nodes_bit game length =
  let bset = BitSet.create length in
  for i = 0 to length - 1 do
    if pg_isDefined game i
    then BitSet.set bset i
  done;
  bset;;

let pre_bit game pre subgame_vr player target length =
	for i = 0 to length - 1 do
	  if BitSet.is_set subgame_vr i
	  then (
      let pl = pg_get_owner game i in
      let ws = pg_get_successors game i in
      let take = ref false in
      if pl = player
      then take := ns_fold (fun b -> fun w -> b || (BitSet.is_set target w)) false ws
      else (
        if pl != plr_undef
        then (
          take := true;
          ns_iter (fun v ->
                    if (BitSet.is_set subgame_vr v) && not (BitSet.is_set target v)
                    then take := false
                  ) ws;
        );
      );
      if !take then BitSet.set pre i else BitSet.unset pre i;
	  );
	done;;

let attr_bit game game_vr todoS todoQ strategy player region region_en =
  let attr = BitSet.copy region in
  let used = BitSet.copy region in
  Enum.iter (fun v ->
              ns_iter (fun w -> if not (BitSet.is_set used w)
                then (
                  let pl = pg_get_owner game w in
                  BitSet.set used w;
                  if pl<>player
                  then Queue.add w todoQ
                  else (Queue.add w todoS; strategy.(w) <- v;)
                );
              ) (ns_filter (fun w -> BitSet.is_set game_vr w) (pg_get_predecessors game v))
            ) region_en;
  while not (Queue.is_empty todoS && Queue.is_empty todoQ) do
    if not (Queue.is_empty todoS)
    then (
      let v = Queue.take todoS in
      BitSet.set attr v;
      ns_iter (fun w -> if not (BitSet.is_set used w)
        then (
          let pl = pg_get_owner game w in
          BitSet.set used w;
          if pl<>player
          then Queue.add w todoQ
          else (Queue.add w todoS; strategy.(w) <- v;)
        );
      ) (ns_filter (fun w -> BitSet.is_set game_vr w) (pg_get_predecessors game v));
    )
    else (
      let v = Queue.take todoQ in
      let ws = pg_get_successors game v in
      if ns_fold (fun b w -> b && (if BitSet.is_set game_vr w then BitSet.is_set attr w else true)) true ws
      then (
        BitSet.set attr v;
        ns_iter (fun w -> if not (BitSet.is_set used w)
          then (
            let pl = pg_get_owner game w in
            BitSet.set used w;
            if pl<>player
            then Queue.add w todoQ
            else (Queue.add w todoS; strategy.(w) <- v;)
          );
        ) (ns_filter (fun w -> BitSet.is_set game_vr w) (pg_get_predecessors game v));
      )
      else BitSet.unset used v;
    );
  done;
  attr;;
