(*
Zadeh: Select improving edge that has been selected least often

Least Basic Iterations: Select the improving edge that has been in the strategy for the least number of iterations

Least Recently Basic: Select the improving edge that left the strategy least-recently

Least Recently Entered: Select the improving edge that entered the strategy least-recently thus far

*)

open Tcsarray;;
open Tcslist;;
open Tcsbasedata;;

type pair = int * int
type edge = int * int * int

type cycleconfig = (bool array array * bool) array
type improvableconfig = bool array array array
type occurrencerec = pair array array array

type bitset = bool array

type engagers = bool array

type strategyiteration =
	((int -> bool) * (int -> int -> int -> bool)) *
	(cycleconfig -> occurrencerec -> edge list -> edge * (bool array)) *
	(cycleconfig -> edge -> occurrencerec -> unit)

let id x = x

let format_bool = function true -> "1" | false -> "0"

let format_pair ((x, y): pair) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let format_edge ((x, y, z): edge) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ "," ^ string_of_int z ^ ")"

let format_cycle_config (cycle_config: cycleconfig) =
  ArrayUtils.custom_format (fun (a, b) ->
    (ArrayUtils.custom_format (ArrayUtils.custom_format format_bool "" "" "") "" "" " " a) ^ " " ^ (format_bool b)
  ) "" "" "\n" cycle_config

let format_improvable_config (improvable_config: improvableconfig) =
  ArrayUtils.custom_format (ArrayUtils.custom_format (ArrayUtils.custom_format format_bool "" "" "") "" "" " ") "" "" "\n" improvable_config

let format_occurrence_rec (occurrence_rec: occurrencerec) =
  ArrayUtils.custom_format (ArrayUtils.custom_format (ArrayUtils.custom_format format_pair "" "" "") "" "" " ") "" "" "\n" occurrence_rec

let format_bit_set (bit_set: bitset) = ArrayUtils.custom_format format_bool "" "" "" bit_set

let format_switches (switches: edge list) = ListUtils.custom_format format_edge "" "" ", " switches

let fstsnd v (f,s) = if v then s else f

let setfstsnd v (f,s) o = if v then (f, o) else (o, s)

let readnode (cycle_config: cycleconfig) (i,j,k) = (fst cycle_config.(i)).(j).(k)

let evaluate_cycle_config (cycle_config: cycleconfig) =
  let n = Array.length cycle_config in
  let bit_set = Array.make n false in
  let engaged = ref true in
  for i = n - 1 downto 0 do
    if !engaged then (
      let (cyc, eng) = cycle_config.(i) in
      let act = if i+1 < n && bit_set.(i+1) then 1 else 0 in
      let set = Array.for_all id cyc.(act) in
      bit_set.(i) <- set;
      if set then engaged := eng
    )
  done;
  (bit_set: bitset)

let bitset_to_number (bit_set: bitset) =
  let n = Array.length bit_set in
  let result = ref 0 in
  for i = n - 1 downto 0 do
    result := 2 * !result + (if bit_set.(i) then 1 else 0)
  done;
  (!result : int)

let improvable_cycle_config (cycle_config: cycleconfig) =
  let n = Array.length cycle_config in
  let improvable_config = Array.make n [||] in
  let engaged = ref true in
  let bit_set = Array.make n false in
  for i = n - 1 downto 0 do
    let (cyc, eng) = cycle_config.(i) in
    if not !engaged then (
      improvable_config.(i) <- Array.map (fun can ->
        Array.map (fun setIn ->
          setIn
        ) can
      ) cyc
    ) else (
      let act = if i+1 < n && bit_set.(i+1) then 1 else 0 in
      let set = Array.for_all id cyc.(act) in
      bit_set.(i) <- set;
      if set then engaged := eng;
      improvable_config.(i) <- Array.mapi (fun a ->
        Array.map (fun setIn ->
          if a = act
          then not set
          else true
        )
      ) cyc
    )
  done;
  (improvable_config: improvableconfig)


let init_cycle_config (n: int) (m: int) (l: int) ((f: int -> bool), (g: int -> int -> int -> bool)) =
  ((Array.init n (fun i -> (Array.init m (fun j -> Array.init l (g i j)), f i))): cycleconfig)

let init_occurrence_record (cycle_config: cycleconfig) =
  ((Array.map (fun (x,_) -> Array.map (Array.map (fun _ -> (0,0))) x) cycle_config): occurrencerec)

let apply_switch (cycle_config: cycleconfig) ((i,j,k): edge) =
  (fst cycle_config.(i)).(j).(k) <- not (readnode cycle_config (i,j,k))

let apply_engagers (cycle_config: cycleconfig) (ens: engagers) =
  Array.iteri (fun i e ->
		if (e) then let (f, s) = cycle_config.(i) in cycle_config.(i) <- (f, not s)
	) ens

let set_single_occurrence_record (cycle_config: cycleconfig) ((i,j,k): edge) (v: bool) (o: int) (occurrence_record: occurrencerec) =
  occurrence_record.(i).(j).(k) <- setfstsnd v occurrence_record.(i).(j).(k) o

let increase_single_occurrence_record (cycle_config: cycleconfig) ((i,j,k): edge) (v: bool) (occurrence_record: occurrencerec) =
	set_single_occurrence_record cycle_config (i,j,k) v (1 + fstsnd v occurrence_record.(i).(j).(k)) occurrence_record

let iterate_nodes (cycle_config: cycleconfig) (f: edge -> bool -> unit) =
  Array.iteri (fun i (a,_) -> Array.iteri (fun j -> Array.iteri (fun k -> f (i,j,k))) a ) cycle_config

let extract_switches_occurrence_record_values (cycle_config: cycleconfig) (improvable_config: improvableconfig) (occurrence_record: occurrencerec) =
  let switches = ref [] in
  iterate_nodes cycle_config (fun (i,j,k) v ->
    if improvable_config.(i).(j).(k)
    then switches := ((i,j,k), fstsnd v occurrence_record.(i).(j).(k))::!switches
  );
  !switches

let eligible_switches switches  =
  let o' = List.fold_left min (if List.length switches > 0 then snd (List.hd switches) else 0) (List.map snd switches) in
  List.map fst (List.filter (fun (_, o) -> o = o') switches)

let iterate_occurrence_record (occurrence_record: occurrencerec) (f: edge -> int -> unit)  =
  Array.iteri (fun i -> Array.iteri (fun j -> Array.iteri (fun k (c, d) ->
		f (i,j,k) c;
		f (i,j,k) d
	))) occurrence_record

let occurrence_record_max (occurrence_record: occurrencerec) =
	let m = ref 0 in
	iterate_occurrence_record occurrence_record (fun (i,j,k) o -> m := max !m o);
	!m

let zeros_in_row (cycle_config: cycleconfig) (i: int) (j: int) =
  Array.fold_left (fun a b -> if not b then a + 1 else a) 0 (fst cycle_config.(i)).(j)

let zadeh: strategyiteration = (
  ((fun _ -> false), (fun _ _ _ -> false)),
  (fun cycle_config occurrence_record switches ->
	  let n = Array.length cycle_config in
		(* Try to switch single open switch that will not fully close a cycle *)
		let found = ref None in
		List.iter (fun ((i,j,k) as sw) ->
			if (not (fst cycle_config.(i)).(j).(k)) && (zeros_in_row cycle_config i j > 1)
			then found := Some sw;
		) switches;
		if (!found != None) then (OptionUtils.get_some !found, Array.make n false)
		else (
			(* Find lowest unset bit that can be set *)
			let found = ref None in
			let bitset = evaluate_cycle_config cycle_config in
			(* TODO: find minimum one that works - line 181 is just copy&paste, please adjust *)
			List.iter (fun ((i,j,k) as sw) ->
				if (zeros_in_row cycle_config i j = 1) && ((j = 0 && i = n-1) || (if bitset.(i+1) then j = 1 else j = 0))
				then found := Some sw;
			) switches;
			if (!found != None) then (OptionUtils.get_some !found, Array.make n false)
			else (
				failwith "nope"
			)
		)
	),
	  (*(List.hd switches, Array.make n false)) *)
	(fun cycle_config sw occurrence_record ->
		increase_single_occurrence_record cycle_config sw (not (readnode cycle_config sw)) occurrence_record)
)

let lbi: strategyiteration = (
	((fun _ -> false), (fun _ _ _ -> false)),
	(fun cycle_config occurrence_record switches ->
	  let n = Array.length cycle_config in
	  (List.hd switches, Array.make n false)),
	(fun cycle_config sw occurrence_record ->
		iterate_nodes cycle_config (fun sw v ->
	    increase_single_occurrence_record cycle_config sw v occurrence_record
	  )
	)
)

let lrb: strategyiteration = (
	((fun _ -> false), (fun _ _ _ -> false)),
	(fun cycle_config occurrence_record switches ->
	  let n = Array.length cycle_config in
	  (List.hd switches, Array.make n false)),
	(fun cycle_config sw occurrence_record ->
		let o = 1 + occurrence_record_max occurrence_record in
		set_single_occurrence_record cycle_config sw (readnode cycle_config sw) o occurrence_record
	)
)

let lre: strategyiteration = (
	((fun _ -> false), (fun _ _ _ -> false)),
	(fun cycle_config occurrence_record switches ->
	  let n = Array.length cycle_config in
	  (List.hd switches, Array.make n false)),
	(fun cycle_config sw occurrence_record ->
		let o = 1 + occurrence_record_max occurrence_record in
		set_single_occurrence_record cycle_config sw (not (readnode cycle_config sw)) o occurrence_record
	)
)


let _ =
  let (initial_strategy, tie_breaking_rule, occurrence_record_updater) = zadeh in

  let c = init_cycle_config 3 2 2 initial_strategy in
	let o = init_occurrence_record c in
	let iter = ref 0 in
	let alldone = ref false in

  while not !alldone do
		let i = improvable_cycle_config c in
		let esw = eligible_switches (extract_switches_occurrence_record_values c i o) in

	  print_string ("Iteration " ^ string_of_int !iter ^ "\n\n");
	  print_string (format_cycle_config c ^ "\n\n");
		print_string (format_occurrence_rec o ^ "\n\n");
	  print_string (format_improvable_config i ^ "\n\n");
		print_string (format_switches esw ^ "\n\n");
		print_string ("----------------------------------" ^ "\n\n");

		if List.length esw > 0 then (
			let (sw, en) = tie_breaking_rule c o esw in
		  apply_switch c sw;
			apply_engagers c en;
			occurrence_record_updater c sw o;
			incr iter
		) else
		  alldone := true
	done



(*

00 00 0
00 00 0
00 00 0

....

01 01 0
01 01 0
01 01 0

01 01 0
01 01 0
01 11 0

....

00 00 0
00 00 0
00 11 0

00 00 0
00 00 0
00 11 1

....

10 10 0
10 10 0
10 11 1

10 10 0
10 11 0
10 11 1

....

00 00 0
00 11 0
00 00 1

00 00 0
00 11 1
00 00 0

....

01 01 0
01 11 1
01 01 0

01 01 0
01 11 1
11 01 0

....

00 00 0
00 11 1
11 00 0

00 00 0
00 11 1
11 00 1

....

10 10 0
10 11 1
11 10 1

10 11 0
10 11 1
11 10 1

....

00 11 0
00 00 1
00 00 1

00 11 1
00 00 0
00 00 0

....

01 11 1
01 01 0
01 01 0

..............
*)
