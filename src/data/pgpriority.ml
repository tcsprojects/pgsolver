open Pgplayer;;

(**************************************************************
 *                       PLAYER/PRIORITY                      *
 **************************************************************)
type priority = int

(********** PRIORITY FUNCTION **********)
let plr_benefits pr = if pr mod 2 = 0 then plr_Even else plr_Odd

let prio_good_for_player pr pl = if pl = plr_Even then pr mod 2 = 0 else pr mod 2 = 1
let odd pr = pr mod 2 = 1
let even pr = pr mod 2 = 0

let prio_undef = -1

let reward player prio = if (prio <= prio_undef) || (plr_benefits prio = player) then prio else -prio
