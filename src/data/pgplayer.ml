(**************************************************************
 *                       PLAYER/                      *
 **************************************************************)
type player = PlayerUndefined | PlayerEven | PlayerOdd

(********** PLAYER FUNCTIONS **********)
let plr_Even = PlayerEven
let plr_Odd = PlayerOdd
let plr_Zero = plr_Even
let plr_One = plr_Odd
let plr_undef = PlayerUndefined

let plr_iterate f =
  f plr_Even; f plr_Odd

let plr_opponent = function
    PlayerEven -> PlayerOdd
|   PlayerOdd -> PlayerEven
|   x -> x

let plr_show = function
    PlayerEven -> "0"
|   PlayerOdd -> "1"
|   _ -> "_"

let plr_random _ = match (Random.int 2) with
    0 -> PlayerEven
|   1 -> PlayerOdd
|   _ -> PlayerUndefined
