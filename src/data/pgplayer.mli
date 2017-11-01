(**************************************************************
 *                      PLAYER /                      *
 **************************************************************)
(** Type of player.
*)
type player = PlayerUndefined | PlayerEven | PlayerOdd


(********** PLAYER FUNCTIONS **********)
(** Player even. This player conforms the integer value 0.

    @return player even.
*)
val plr_Even  : player

(** Player odd. This player conforms the integer value 1.

    @return player odd
*)
val plr_Odd   : player

(** Undefined player. This player conforms the integer value -1.

    @return undefined player
*)
val plr_undef : player

(** Returns a random player.

    @return player even or player odd
*)
val plr_random : unit -> player

(** Returns the opponent player.

    @param player delivered player
    @return player odd if player even was delivered. player even, if player odd was delivered.
*)
val plr_opponent : player -> player

(** Returns a string representation of the player.

    @param player player to be shown
    @return string representation of given player
*)
val plr_show : player -> string

(** Applies function to both players.

    @param f function to be applied to players
*)
val plr_iterate : (player -> unit) -> unit

