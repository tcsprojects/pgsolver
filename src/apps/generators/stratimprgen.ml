open Arg ;;
open Tcsargs;;
open Tcsbasedata;;
open Paritygame;;
open Mdp;;
open Stratimprgenerators;;

Stratimprgenlist.register();;

module CommandLine =
struct

  type action = NoAction | GeneratePg of strat_impr_gen | GenerateMdp of strat_impr_gen | GenerateLp of strat_impr_gen | GenerateDualLp of strat_impr_gen
  
  let action = ref NoAction
  let subargs = ref ""
  
  let genlistpg = fold_strat_impr_gen (fun gen t -> if gen.parity_game = None then t else t ^ " " ^ gen.ident) ""
  let genlistmdp = fold_strat_impr_gen (fun gen t -> if gen.generalized_mdp = None then t else t ^ " " ^ gen.ident) ""
  
  let speclist =  [
					(["-pg"], Tuple [String (fun s -> action := GeneratePg (find_strat_impr_gen s));
					                 String (fun a -> subargs := a)],
                     "<generator> \"<args>\"\n     generate parity game, valid ones are:" ^ genlistpg);
					(["-mdp"], Tuple [String (fun s -> action := GenerateMdp (find_strat_impr_gen s));
					                 String (fun a -> subargs := a)],
                     "<generator> \"<args>\"\n     generate mdp, valid ones are:" ^ genlistmdp);
					(["-lp"], Tuple [String (fun s -> action := GenerateLp (find_strat_impr_gen s));
					                 String (fun a -> subargs := a)],
                     "<generator> \"<args>\"\n     generate primal lp, valid ones are:" ^ genlistmdp);
					(["-duallp"], Tuple [String (fun s -> action := GenerateDualLp (find_strat_impr_gen s));
					                 String (fun a -> subargs := a)],
                     "<generator> \"<args>\"\n     generate dual lp, valid ones are:" ^ genlistmdp);
		           ]

  let header = Info.get_title "Strategy Improvement Generator Tool"
  
  let usage = "Usage: stratimprgen [-pg | -mdp | -lp | -duallp] <generator> \"<args>\"\n"
end ;;


open CommandLine ;;


let _ =
  SimpleArgs.parsedef speclist (fun _ -> failwith "no direct argument expected") (header ^ usage ^ "\nOptions are");
  
  match !action with
  	NoAction -> ()
  | GeneratePg generator ->
  		let game = (OptionUtils.get_some (generator.parity_game) (Array.of_list (Tcsstrings.StringUtils.explode !subargs ' '))) in
  		print_game game
  | GenerateMdp generator ->
  		let generalized_mdp = (OptionUtils.get_some (generator.generalized_mdp) (Array.of_list (Tcsstrings.StringUtils.explode !subargs ' '))) in
  		print_mdp (generalized_mdp_to_mdp generalized_mdp)
  | GenerateLp generator ->
  		let generalized_mdp = (OptionUtils.get_some (generator.generalized_mdp) (Array.of_list (Tcsstrings.StringUtils.explode !subargs ' '))) in
  		print_lp (unichain_mdp_to_primal_lp (generalized_mdp_to_mdp generalized_mdp))
  | GenerateDualLp generator ->
  		let generalized_mdp = (OptionUtils.get_some (generator.generalized_mdp) (Array.of_list (Tcsstrings.StringUtils.explode !subargs ' '))) in
  		print_lp (unichain_mdp_to_dual_lp (generalized_mdp_to_mdp generalized_mdp))
