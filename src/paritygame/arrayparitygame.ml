open Paritygame;;
open Tcsbasedata;;
open Tcsarray;;
open Tcsset;;
open Tcslist;;
open Tcsgraph;;

  
(**************************************************************
 *                          TOOLS                             *
 **************************************************************)
(** Method for adding an edge between two nodes in an (priority * player * successors * predecessors * description) array.
    
    @param array array to set edge (v,u) in
    @param predecessor v
    @param successor u
*)
let add_edge_in_node_array array predecessor successor =
  let (prioPred, ownerPred, successorsPred, predecessorsPred, descPred) = Array.get array predecessor in
  Array.set array predecessor (prioPred, ownerPred, (ns_add successor successorsPred), predecessorsPred, descPred);
  let (prioSucc, ownerSucc, successorsSucc, predecessorsSucc, descSucc) = Array.get array successor in
  Array.set array successor (prioSucc, ownerSucc, successorsSucc, (ns_add predecessor predecessorsSucc), descSucc);;




(**************************************************************
 *                       ARRAY PARITYGAME                     *
 **************************************************************)
(** Array-Paritygame class. 
    This class inherits from the general paritygame class and replaces the former paritygame type.
    The structure behind this class is an (priority * player * nodeset * nodeset * string option) array. 
*)
class array_pg ?initFunc (initSize : int) =
object (self : 'self)
  inherit paritygame
  val mutable nodes : (priority * player * nodeset * nodeset * string option) array = Array.make initSize (-1, plr_undef, ns_empty, ns_empty, None)

  initializer
    match initFunc with
    | None -> ()
    | Some f -> self#init f

                          
  (********** GENERAL **********)    
  method private init f =
    for i=0 to self#size-1 do
      let (pr, pl, succs, name) = f i in
           self#set_priority i pr;
           self#set_owner i pl;
           self#set_desc i name;
           List.iter (fun w -> self#add_edge i w) succs
    done
      
  method size =
    Array.length nodes

  method copy =
   {<  >}

  method sort f =
    Array.sort f nodes

  method iterate f =
    for i=0 to (self#size) - 1 do
      if self#is_defined i then f i (self#get_node i)
    done

  method edge_iterate f =
    self#iterate(fun v -> fun (_,_,succs,_,_) -> ns_iter (fun w -> f v w) succs)

  method map f =
    let newNodes = Array.mapi f nodes in
    {<nodes = newNodes>}

  method map2 : 'a. (node -> (priority * player * nodeset * nodeset * string option) -> 'a) -> 'a array =
    fun f -> Array.mapi f nodes

                        
  (********** GETTERS **********)
  method get_node i =
    nodes.(i)

  method get_priority i =
    let (pr,_,_,_,_) = nodes.(i) in pr
                                 
  method get_owner i =
    let (_,pl,_,_,_) = nodes.(i) in pl

  method get_successors i =
    let (_,_,succs,_,_) = nodes.(i) in succs

  method get_predecessors i =
    let (_,_,_,preds,_) = nodes.(i) in preds

  method get_desc i =
    let (_,_,_,_,desc) = nodes.(i) in desc

  method get_desc' i =
    match self#get_desc i with
      None -> ""
    | Some s -> s

  method find_desc desc =
    ArrayUtils.find (fun (_,_,_,_,desc') -> desc = desc') nodes
                    
  method is_defined v =
    let (p,_,_,_,_) = nodes.(v) in p >= 0
                                          
  method format_game =
    "[" ^
  String.concat ";"
                (List.filter (fun s -> s <> "")
                             (Array.to_list (Array.mapi (fun i -> fun (p,(pl : player),ws,_,_) ->
                                              if p <> -1 then string_of_int i ^ ":" ^ string_of_int p ^ "," ^
                                                              plr_show pl ^ ",{" ^
                                                              String.concat "," (List.map string_of_int (ns_nodes ws))
                                                              ^ "}"
                                                         else "") nodes)))
  ^ "]"

  (********** SETTERS **********)
  method set_node' i node =
    nodes.(i) <- node

  method set_node i pr pl succs preds desc =
    self#set_node' i (pr, pl, succs, preds, desc)

  method set_priority i pr =
    let (_, pl, succs, preds, desc) = self#get_node i in
    self#set_node i pr pl succs preds desc

  method set_owner i pl =
    let (pr, _, succs, preds, desc) = self#get_node i in
    self#set_node i pr pl succs preds desc

  method set_desc i desc =
    let (pr, pl, succs, preds, _) = self#get_node i in
    self#set_node i pr pl succs preds desc

  method set_desc' i desc =
    self#set_desc i (if desc = "" then None else Some desc)

  method add_edge v u =
    add_edge_in_node_array nodes v u

  method del_edge v u =
    let (pr,pl,succs,preds,desc) = self#get_node v in
    self#set_node' v (pr, pl, ns_del u succs, preds, desc);
    let (pr,pl,succs,preds,desc) = self#get_node u in
    self#set_node' u (pr, pl, succs, ns_del v preds, desc)
                   
  method remove_nodes nodes =
    ns_iter (fun v -> let succs = self#get_successors v in
		      ns_iter (fun u -> self#del_edge v u) succs;
		      let preds = self#get_predecessors v in
		      ns_iter (fun u -> self#del_edge u v) preds;
		      self#set_priority v (-1);
		      self#set_owner v plr_undef;
		      self#set_desc v None
	    ) nodes

  method remove_edges edges =
    List.iter (fun (v,w) -> self#del_edge v w) edges


  (********** SUBGAME **********)
  method subgame_by_edge_pred pred =
    let newNodes = Array.make self#size (-1, plr_undef, ns_empty, ns_empty, None)  in
    for i = 0 to self#size - 1 do
      let (_,_,currSucc, currPred,_) = Array.get newNodes i in
      Array.set newNodes i ((self#get_priority i), (self#get_owner i), currSucc, currPred, (self#get_desc i));
      ns_iter (fun j -> if pred i j then add_edge_in_node_array newNodes i j) (self#get_successors i)
    done;
    {<nodes = newNodes>}

  method subgame_by_node_pred pred =
    let newNodes = Array.make self#size (-1, plr_undef, ns_empty, ns_empty, None)  in
    for i = 0 to self#size - 1 do
      if pred i then
        begin
          let (_,_,currSucc, currPred,_) = Array.get newNodes i in
          Array.set newNodes i ((self#get_priority i), (self#get_owner i), currSucc, currPred, (self#get_desc i));
          ns_iter (fun j -> add_edge_in_node_array newNodes i j) (self#get_successors i)
        end
    done;
    {<nodes = newNodes>}

  method subgame_by_list nodeSetList  =
    let nodeSetListSize = ns_size nodeSetList in
    let newNodesArray = Array.make nodeSetListSize (-1, plr_undef, ns_empty, ns_empty, None) in
    (* mapping table: key=oldPos, value=newPos*)
    let newPosTable = Hashtbl.create nodeSetListSize in

    (*create mapping (oldPos -> newPos)*)
    let i = ref 0 in
    ns_iter (fun node -> Hashtbl.add newPosTable node !i; i := !i+1) nodeSetList;

    (*mapping function for predecessors and successors*)
    let mapNodeSet nodeSet =
      let newNodeSet = ns_filter(fun node -> (ns_elem node nodeSetList)) nodeSet in
      ns_map(fun node -> Hashtbl.find newPosTable node ) newNodeSet in


    (*fill newNodesArray with mapped nodes *)
    let j = ref 0 in
    ns_iter
      (fun node -> let (prio,plr,succs,preds,desc) = self#get_node node in
                   Array.set newNodesArray !j (prio, plr, mapNodeSet succs, mapNodeSet preds, desc);
                   j := !j +1) nodeSetList;
    {<nodes = newNodesArray>}

  method subgame_by_node_filter pred =
    let map_to_sub = ref TreeMap.empty_def in
    let map_to_game = ref TreeMap.empty_def in

    self#iterate (fun i _ ->
        if pred i then (
            map_to_sub := TreeMap.add i (TreeMap.cardinal !map_to_game) !map_to_sub;
            map_to_game := TreeMap.add (TreeMap.cardinal !map_to_game) i !map_to_game
        )
    );
    let initFunc = (fun i ->
        let j = TreeMap.find i !map_to_game in
        let li = ref [] in
        ns_iter (fun k ->
            if (TreeMap.mem k !map_to_sub)
            then li := (TreeMap.find k !map_to_sub) :: !li
        ) (self#get_successors j);
        (self#get_priority j,
         self#get_owner j,
         !li, 
         self#get_desc j)
    ) in
    let newNodes = Array.make (TreeMap.cardinal !map_to_game) (-1, plr_undef, ns_empty, ns_empty, None)  in
    for i=0 to (TreeMap.cardinal !map_to_game)-1 do
      let (pr, pl, succs, name) = initFunc i in
      let (_,_,currSucc,currPred,_) = Array.get newNodes i in
      Array.set newNodes i (pr, pl, currSucc, currPred, name);
           List.iter (fun w -> add_edge_in_node_array newNodes i w) succs
    done;
    ({<nodes = newNodes>}, (fun i -> TreeMap.find i !map_to_sub), (fun i -> TreeMap.find i !map_to_game))      
end;;



  
(********************************************************
 *                 PARITYGAME BUILDER                   *
 ********************************************************)
(* This can be used to build parity games starting from a particular node in an on-the-fly fashion.
   It is particularly useful when the resulting size is not (easily) known in advance. For an example
   of its use, see src/generators/langincl.ml .

   To use it, define a module of the type PGDescription using some type gamenode to represent nodes and
   giving functions that read off the priority, owner successors, a possible string representation of a
   game node, and a list of particular initial nodes.
   The module obtained by applying the functor Build then gives you a module with a function that
   builds a parity game containing all the game nodes that are reachable these initial ones. Additionally,
   you get functions that take nodes as arguments from which to build the parity game.
 *)
  
module type PGDescription =
  sig
    type gamenode

    val compare    : gamenode -> gamenode -> int

    val owner      : gamenode -> player
    val priority   : gamenode -> priority
    val successors : gamenode -> gamenode list
    val show_node  : gamenode -> string option

    val initnodes  : unit -> gamenode list
  end;;


module type PGBuilder =
  sig
    type gamenode

    val build            : unit -> array_pg
    val build_from_node  : gamenode -> array_pg
    val build_from_nodes : gamenode list -> array_pg
  end

module Build(T: PGDescription) : (PGBuilder with type gamenode = T.gamenode ) =
  struct

    type gamenode = T.gamenode

    module Encoding = Map.Make(
      struct
        type t = T.gamenode
        let compare = compare
      end);;

    let codes = ref Encoding.empty

    let next_code = ref 0


    let encode v = try
                     Encoding.find v !codes
                   with Not_found -> begin
                                       codes := Encoding.add v !next_code !codes;
                                       incr next_code;
                                       !next_code - 1
                                     end

    let build_from_nodes vlist =
      let rec iterate acc visited =
        function []          -> acc
               | ((v,c)::vs) -> begin
                                if NodeSet.mem c visited then
                                  iterate acc visited vs
                                else
                                  let ws = T.successors v in
                                  let ds = List.map encode ws in
                                  iterate ((c, T.owner v, T.priority v, ds, T.show_node v) :: acc) (NodeSet.add c visited) ((List.combine ws ds) @ vs)
                              end
      in
      let nodes = iterate [] NodeSet.empty (List.map (fun v -> (v,encode v)) vlist) in
      let game = new array_pg (List.length nodes) in
      let rec transform =
        function []                  -> ()
	       | ((v,o,p,ws,nm)::ns) -> game#set_priority v p;
					game#set_owner v o;
					game#set_desc v nm;
					List.iter (fun w -> game#add_edge v w) ws;
                                        transform ns
      in
      transform nodes;
      game

    let build_from_node v = build_from_nodes [v]

    let build _ = build_from_nodes (T.initnodes ())
  end;;
