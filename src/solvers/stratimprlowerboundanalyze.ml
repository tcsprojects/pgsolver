open Tcsset
open Tcsbasedata
open Pgnode
open Paritygame
open Stratimpralgs
open Tcslist
open Tcsarray


type 'a mda = Non | Ele of 'a | Arr of ('a mda) array

let mda_as_ele = function Ele x -> x | _ -> failwith "Ele expected"
let mda_as_arr = function Arr x -> x | _ -> failwith "Arr expected"

let rec mda_get arr = function [] -> arr | (x::xs) -> mda_get (mda_as_arr arr).(x) xs

let mda_get_arr arr idxs = mda_as_arr (mda_get arr idxs)

let mda_get_ele arr idxs = mda_as_ele (mda_get arr idxs)

let rec mda_set md t = function [] -> Ele t | (x::xs) ->
  match md with
    Ele _ -> failwith "Non or Arr expected"
  | Non ->
      let a = Array.make (x+1) Non in
      a.(x) <- mda_set Non t xs;
      Arr a
  | Arr a ->
      if (Array.length a > x) then (
          a.(x) <- mda_set a.(x) t xs;
          Arr a
      ) else (
        let a = Array.init (x+1) (fun i -> if i < Array.length a then a.(i) else Non) in
        a.(x) <- mda_set Non t xs;
        Arr a
      )

type 'a mda_flattened1 = (int list, 'a array) TreeMap.t

let rec mda_flatten1 = function
  Arr a -> if Array.length a = 0 then TreeMap.empty_def
           else (
             match a.(0) with
               Non -> TreeMap.empty_def
             | Ele b -> TreeMap.singleton_def [] (Array.map mda_as_ele a)
             | Arr _ -> let idx = ref (-1) in
                        Array.fold_left (fun r i ->
                          incr idx;
                          TreeMap.fold (fun k -> TreeMap.add ([!idx] @ k)) (mda_flatten1 i) r
                        ) TreeMap.empty_def a
           )
| _ -> TreeMap.empty_def

type ('a, 'b) desc_map = ('a, 'b mda) TreeMap.t

let desc_map_format fmta fmtb dm =
  TreeMap.fold (fun a b ->
    let fa = fmta a in
    TreeMap.fold (fun l ar s ->
      s ^ fa ^ "\t" ^ IntListUtils.format_plain l ^ "\t" ^ ArrayUtils.format fmtb ar ^ "\n"
    ) (mda_flatten1 b)
  ) dm ""


type 'a desc_idx = 'a * (int list)

let desc_map_set dm (c, idxs) target =
  if TreeMap.mem c dm
  then TreeMap.add c (mda_set (TreeMap.find c dm) target idxs) dm
  else TreeMap.add c (mda_set Non target idxs) dm

let desc_map_get dm (c, idxs) = mda_get (TreeMap.find c dm) idxs

let desc_map_g dm c = TreeMap.find c dm

let desc_map_s dm c a = TreeMap.add c a dm

type parsed_node = char * int list

let parse_node s =
  let c = String.get s 0 in
  let s = String.sub s 1 (String.length s - 1) in
  if (String.length s > 0) then (
    match int_of_string_opt s with
      None -> (c, List.map int_of_string (String.split_on_char ',' (String.sub s 1 (String.length s - 2))))
    | Some i -> (c, [i])
  ) else
    (c, [])

type game_desc_map = (char, int) desc_map

let game_desc_map_format = desc_map_format Char.escaped string_of_int

let parse_game_desc_map (game: paritygame) =
  let gdm = ref (TreeMap.empty_def) in
  game#iterate (fun i (_, _, _, _, desc) -> gdm := desc_map_set !gdm (parse_node (OptionUtils.get_some desc)) i);
  !gdm



type game_desc_binary_map = (char, bool) desc_map

let game_desc_binary_map_format = desc_map_format Char.escaped (fun b -> if b then "1" else "0")

let parse_binary_strategy game strategy mapper =
  let h i = parse_node (OptionUtils.get_some (game#get_desc i)) in
  let gdbm = ref (TreeMap.empty_def) in
  strategy#iter (fun i j ->
    try
      if i != nd_undef && j != nd_undef
      then gdbm := desc_map_set !gdbm (h i) (mapper (h i) (h j))
    with _ -> ()
  );
  !gdbm

let parse_binary_improving game nto strategy valu =
  let h i = parse_node (OptionUtils.get_some (game#get_desc i)) in
  let gdbm = ref (TreeMap.empty_def) in
  strategy#iter (fun i j ->
    if i != nd_undef && j != nd_undef
    then gdbm := desc_map_set !gdbm (h i) (node_valuation_ordering game nto valu.(j) valu.(best_decision_by_valuation_ordering game nto valu i) < 0)
  );
  !gdbm


let rec mda_is_subset_of a b =
  match (a, b) with
    (Arr a, Arr b) ->
      let sub = ref true in
      for i = 0 to Array.length a - 1 do
        sub := !sub && mda_is_subset_of a.(i) b.(i)
      done;
      !sub
  | _ -> a = b

let desc_map_is_subset_of a b = TreeMap.for_all (fun k v -> mda_is_subset_of v (TreeMap.find k b)) a
