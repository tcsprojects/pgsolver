let good p = p mod 2 = 1;;

let update_summary summary p =
  let idx = ref (-1) in
  let goodidx = ref (-1) in
  Array.iteri (fun i e ->
    match e with
    | Some q ->
      if good q && !goodidx = i-1
      then goodidx := i;
      if q < p
      then idx := i
    | None -> ()
  ) summary;
  if good p && !goodidx >= !idx
  then idx := !goodidx + 1;
  Array.init (max (Array.length summary) (!idx + 1)) (fun i ->
    if i < !idx then None
    else if i = !idx then Some p
    else summary.(i)
  );;

let rec priopath_rev_to_summary = function
  [] -> [||]
| x::xs -> update_summary (priopath_rev_to_summary xs) x;;

let priopath_to_summary xs = priopath_rev_to_summary (List.rev xs);;

let compare_summaries summary1 summary2 =
  let mp = function
    None -> 0
  | Some p -> if good p then p + 1 else -(p + 1)
  in
  let l1 = Array.length summary1 in
  let l2 = Array.length summary2 in
  let result = ref 0 in
  for i = (max l1 l2) - 1 downto 0 do
    let e1 = mp (if i < l1 then summary1.(i) else None) in
    let e2 = mp (if i < l2 then summary2.(i) else None) in
    if !result = 0 && e1 != e2 then result := if e1 > e2 then 1 else -1
  done;
  !result;;

let priopath = [1;6;7;5;1;4;5;3;2;1;3;2;3;1;3;3;1;2;1];;

priopath_to_summary priopath;
