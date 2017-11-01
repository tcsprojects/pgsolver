open Pgplayer


(**************************************************************
 *                       / PRIORITY                     *
 **************************************************************)
(** Type of priority of a node. Currently represented as an integer value.
    This type may become abstract in the future.
*)
type priority = int



(********** PRIORITY FUNCTIONS **********)

(** Returns the player which benefits from the given priority.
    Is the priority even, player even benefits. Is the priority odd, player odd benefits.

    @param priority priority to check for
    @return player which benefits from delivered priority
*)
val plr_benefits : priority -> player


(** Checks if a given priority is good for a given player.

    @param priority priority to be checked with player
    @param player player to be checked with priority
    @return true, if priority is good for player. false, if priority is not good for player
*)
val prio_good_for_player : priority -> player -> bool

(** Checks if a priority is odd.

    @param priority priority to be checked
    @return if priority is odd
*)
val odd: priority -> bool

(** Checks if a priority is even.

    @param priority priority to be checked
    @return if priority is even
*)
val even: priority -> bool


val prio_undef: priority


(** Returns reward of given priority for given player.

    @param player player to check for reward
    @param priority priority to check for
    @return negative (priority x -1) if bad for player, priority if good for player
*)
val reward            : player -> priority -> priority
