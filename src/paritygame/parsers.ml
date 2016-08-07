open Paritygame;;
open Bytes;;
  
let parse_init_parity_game in_channel = 
	let game = ref (pg_create 0) in
	let add v pr pl succs desc =
		pg_set_priority !game v pr;
		pg_set_owner !game v pl;
		pg_set_desc !game v (if desc = "" then None else Some desc);
		List.iter (fun w -> pg_add_edge !game v w) succs
	in
	let queue = ref [] in
	let max_node = ref (-1) in
	let adder = ref (fun v pr pl succs desc ->
		queue := (v, pr, pl, succs, desc)::!queue;
		max_node := max !max_node v 
	) in
	let init_value = ref 0 in
	Tcsgameparser.parse_parity_game (fun n ->
		game := pg_create (n + 1);
		adder := add
	) (fun i -> init_value := i) !adder (fun _ -> ()) in_channel;
	if !queue != [] then (
		game := pg_create (!max_node + 1);
		List.iter (fun (v, pr, pl, succs, desc) -> add v pr pl succs desc) !queue
	);
	(!init_value, !game)

let parse_parity_game in_channel = snd (parse_init_parity_game in_channel)
 
let parse_solution = Tcsgameparser.parse_explicit_parity_solution


let parse_parity_game in_channel =
  let max_prio = ref -1 in  (* for possible later use, so that it does not have to be recomputed *)
  let min_prio = ref -1 in  (* dito *)

  let line = ref 1 in
  let col = ref 1 in
  
  let nodes = ref -1 in
  let buf = Bytes.create 100 in (* TODO: assuming for now that no token is longer than that ... better implement some error handling later *)
  let i = ref 0 in
  let c = ref ' ' in
  
  let advance _ = incr i in  (* implement error handling here: increase buffer capacity *)
  
  let isWhitespace c = (c=' ') || (c='\n') || (c='\r') || (c='\t') in
  let isReturn c = (c='\n') || (c='\r') in

  let take_next _ =
    c := input_char in_channel;
    (if isReturn !c then
      begin
	col := 0; incr line
      end
    else
      incr col);
    !c
  in
  
  let parse_error s = failwith ("Error parsing parity game in line " ^ string_of_int !line ^ ", column " ^ string_of_int !col ^ ": " ^ s) in

  let parse_token_until_whitespace _ =
    c := ' ';
    i := 0;
    while isWhitespace c do
      c := take_next ();
      if not (isWhitespace c) then
	begin
	  set buf 0 c;
	  advance ()
	end
    done;
    while not (isWhitespace !c) do
      c := take_next ();
      set buf !i c;
      advance ()
    done;
    sub_string buf 0 !i 
  in
    
  let parse_token_until_semicolon _ =
    c := ' ';
    i := 0;
    while isWhitespace c do
      c := take_next ();
      if not (isWhitespace c) then
	begin
	  set buf 0 c;
	  advance ()
	end
    done;
    while !c <> ';' do
      c := take_next ();
      set buf !i c;
      advance ()
    done;
    sub_string buf 0 !i 
  in

  if parse_token_until_whitespace _ <> "parity" then parse_error "does not look like a parity game";
  (try
      nodes := int_of_string (parse_token_until_semicolon _)
    with End_of_file -> parse_error "unexpected end of parity game"
       | Failure e -> parse_error ("does not look like a valid number of nodes: " ^ e));
  
  let game = pg_create !nodes in

  let finished = ref false in
  let node = ref -1 in
  let prio = ref -1 in
  let own = ref -1 in
  let succs = ref [] in
  let desc = ref None in
  
  while not !finished do
    (* parsing number of next node *)
    (try
	node := int_of_string (parse_token_until_whitespace ())
      with End_of_file -> parse_error "unexpected end of parity game"
	 | Failure e -> parse_error ("does not look like a valid identifier for a node: " ^ e));

    (* parsing its priority *)
    (try
	prio := int_of_string (parse_token_until_whitespace ())
      with End_of_file -> parse_error "unexpected end of node description: expecting priority now"
	 | Failure e -> parse_error ("does not look like a valid priority for a node: " ^ e));
 
    (* parsing its owner *)
    (try
	own := int_of_string (parse_token_until_whitespace ())
      with End_of_file -> parse_error "unexpected end of node description: expecting owner now"
	 | Failure e -> parse_error ("does not look like a valid owner for a node: " ^ e));
 
    (* parsing its successors *)
    (try
	let succsstr := (parse_token_until_whitespace ()) in
	succs := [];
	let i = ref 0 in
	let j = ref 0 in
	(try
	  while true do
	    j := String.index_from succsstr !i ',';
	    succs := (int_of_string (String.sub succsstr !i (!j - !i))) :: !succs;
	    j := i;
	  done
	with Not_found -> succs := (int_of_string (String.sub succsstr !i ((String.length succsstr) - !i))) :: !succs)
      with End_of_file -> parse_error "unexpected end of node description: expecting list of successors now"
	 | Failure e -> parse_error ("does not look like a valid identifier for a node: " ^ e));
 
  done

    
