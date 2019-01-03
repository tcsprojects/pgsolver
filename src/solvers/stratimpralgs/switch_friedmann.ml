open Stratimpralgs;;
open Paritygame;;
open Tcsset;;
open Tcsbasedata;;
open Univsolve;;
open Tcsarray;;
open Tcslist;;
open Tcsgraph;;
open Pgnodeset;;
open Pgplayer;;
open Pgpriority;;
open Switch_globally_best;;



let switch_cycle game node_total_ordering old_strategy valu =
    let cycle_edge v w =
        let (_, x, _) = valu.(w) in
        TreeSet.mem v x
    in
    let empty_image v =
        ns_forall (fun w ->
            node_valuation_ordering game node_total_ordering valu.(w) valu.(old_strategy#get v) < 0 ||
            not (cycle_edge v w)
        ) (game#get_successors v)
    in
    let filter v w =
        cycle_edge v w || (old_strategy#get v = w && empty_image v)
    in
    improvement_policy_optimize_all_globally' filter game node_total_ordering old_strategy valu


let improvement_policy_optimize_friedmann game node_total_ordering m old_strategy valu =
	let n = game#size in
	let policy = if (m / n) mod 2 = 0
	    then improvement_policy_optimize_all_locally
	    else switch_cycle
    in
	(policy game node_total_ordering old_strategy valu, m + 1)



let strategy_improvement_optimize_friedmann_policy game =
	strategy_improvement_by_policy game improvement_policy_optimize_friedmann 0 false "STRIMPR_FRIED";;


let register _ =
    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_friedmann_policy g)
        "switchfried" "sfd" "magical algorithm";;

