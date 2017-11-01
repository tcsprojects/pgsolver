open Paritygame;;
open Tcsbasedata;;
open Tcsarray;;
open Tcsset;;
open Tcslist;;
open Tcsgraph;;
open Pgnodeset;;
open Pgnode;;

(**************************************************************
 *                          TOOLS                             *
 **************************************************************)
let add_edge_in_node_map map predecessor successor =
    let (prioPred, ownerPred, successorsPred, predecessorsPred, descPred) = TreeMap.find predecessor map in
    let map = TreeMap.add predecessor (prioPred, ownerPred, (ns_add successor successorsPred), predecessorsPred, descPred) map in
    let (prioSucc, ownerSucc, successorsSucc, predecessorsSucc, descSucc) = TreeMap.find successor map in
    TreeMap.add successor (prioSucc, ownerSucc, successorsSucc, (ns_add predecessor predecessorsSucc), descSucc) map

class map_pg =
object (self : 'self)
  inherit paritygame
  val mutable nodes : (node, priority * player * nodeset * nodeset * string option) TreeMap.t = TreeMap.empty_def

  method size =
    TreeMap.cardinal nodes

  method copy =
    let new_nodes = nodes in
    {< nodes = new_nodes >}

  method iterate f =
    TreeMap.iter f nodes

  method edge_iterate f =
    self#iterate(fun v -> fun (_,_,succs,_,_) -> ns_iter (fun w -> f v w) succs)

  method map f =
    let newNodes = TreeMap.mapi f nodes in
    {<nodes = newNodes>}

  (*see interface declaration *)
  method map2 =
    failwith "not implemented"


  (********** GETTERS **********)
  method get_node i =
    TreeMap.find i nodes

  method get_priority i =
    let (pr,_,_,_,_) = TreeMap.find i nodes in pr

  method get_owner i =
    let (_,pl,_,_,_) = TreeMap.find i nodes in pl

  method get_successors i =
    let (_,_,succs,_,_) = TreeMap.find i nodes in succs

  method get_predecessors i =
    let (_,_,_,preds,_) = TreeMap.find i nodes in preds

  method get_desc i =
    let (_,_,_,_,desc) = TreeMap.find i nodes in desc

  method get_desc' i =
    match self#get_desc i with
      None -> ""
    | Some s -> s

  method is_defined v =
    TreeMap.mem v nodes
                                          
  method find_desc desc =
    OptionUtils.get_some (TreeMap.fold (fun i (_,_,_,_,desc') a ->
        if a = None && desc' = desc then Some i else None
    ) nodes None)

  method format_game =
    "[" ^
  String.concat ";"
                             (List.map (fun (i, (p,(pl : player),ws,_,_)) ->
                                                    string_of_int i ^ ":" ^ string_of_int p ^ "," ^
                                                              plr_show pl ^ ",{" ^
                                                              String.concat "," (List.map string_of_int (ns_nodes ws))
                                                              ^ "}"
                                                         ) (TreeMap.pairs nodes))
  ^ "]"

  (********** SETTERS **********)
  method set_node' i node =
    nodes <- TreeMap.add i node nodes

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
    nodes <- add_edge_in_node_map nodes v u

  method del_edge v u =
    let (pr,pl,succs,preds,desc) = self#get_node v in
    self#set_node' v (pr, pl, ns_del u succs, preds, desc);
    let (pr,pl,succs,preds,desc) = self#get_node u in
    self#set_node' u (pr, pl, succs, ns_del v preds, desc)
                   
  method remove_edges edges =
    List.iter (fun (v,w) -> self#del_edge v w) edges

  method remove_nodes rm_nodes =
    ns_iter (fun v -> let succs = self#get_successors v in
		      ns_iter (fun u -> self#del_edge v u) succs;
		      let preds = self#get_predecessors v in
		      ns_iter (fun u -> self#del_edge u v) preds;
		      nodes <- TreeMap.remove v nodes
	    ) rm_nodes



  (********** SUBGAME **********)
  method subgame_by_edge_pred pred =
    let newNodes = TreeMap.fold (fun k (pr, plr, _, _, desc) ->
      TreeMap.add k (pr, plr, ns_empty, ns_empty, desc)
    ) nodes (TreeMap.empty_def) in
    let newNodes = TreeMap.fold (fun i (_, _, succ, _, _) newNodes ->
      ns_fold (fun newNodes j -> if pred i j then add_edge_in_node_map newNodes i j else newNodes) newNodes succ
    ) nodes newNodes in
    {<nodes = newNodes>}

  method subgame_by_node_pred pred =
    let newNodes = TreeMap.fold (fun k (pr, plr, _, _, desc) newNodes ->
      if pred k then TreeMap.add k (pr, plr, ns_empty, ns_empty, desc) newNodes else newNodes
    ) nodes (TreeMap.empty_def) in
    let newNodes = TreeMap.fold (fun i (_, _, succ, _, _) newNodes ->
      if pred i then ns_fold (fun newNodes j -> if pred j then add_edge_in_node_map newNodes i j else newNodes) newNodes succ else newNodes
    ) nodes newNodes in
    {<nodes = newNodes>}

  method subgame_by_list nodeSetList =
    let newNodes = ns_fold (fun newNodes v ->
      let (pr, pl, _, _, desc) = self#get_node v in
      TreeMap.add v (pr, pl, ns_empty, ns_empty, desc) newNodes
    ) (TreeMap.empty_def) nodeSetList in
    let newNodes = ns_fold (fun newNodes v ->
      let (pr, pl, succ, pred, desc) = self#get_node v in
      let succ = ns_filter (fun k -> TreeMap.mem k newNodes) succ in
      let pred = ns_filter (fun k -> TreeMap.mem k newNodes) pred in
      TreeMap.add v (pr, pl, succ, pred, desc) newNodes
    ) newNodes nodeSetList in
    {<nodes = newNodes>}


  method subgame_by_node_filter pred =
    let id (i: Pgnode.node) = i in
    (self#subgame_by_node_pred pred, id, id)

end;;