open Paritygame;;
open Tcsmaths;;

type generalized_mdp_node = Controller of int array * string option
                 		  | Randomizer of (BigFloat.t * int) array
                 		  | Reward of (BigFloat.t * int)
                 		  | Sink
                 
type generalized_mdp = generalized_mdp_node array

val parity_game_to_generalized_mdp: paritygame -> int -> (int -> int -> bool) -> generalized_mdp

type mdp = (((BigFloat.t * BigFloat.t * int) array) array * string option) array

val generalized_mdp_to_mdp: generalized_mdp -> mdp

val print_mdp: mdp -> unit

type lp_objective = Maximize | Minimize

type lp_constraint_type = LPEq | LPGeq

type lp = lp_objective * (string option array) * (BigFloat.t array) * (BigFloat.t array * BigFloat.t * lp_constraint_type * string option) array

val unichain_mdp_to_primal_lp: mdp -> lp

val unichain_mdp_to_dual_lp: mdp -> lp

val print_lp: lp -> unit