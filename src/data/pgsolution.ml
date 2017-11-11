open Pgnode;;
open Pgplayer;;
open Tcsset;;


exception UnmergableSolutions

class virtual solution = object(self: 'self)

    method virtual get: node -> player

    method virtual set: node -> player -> unit

    method virtual for_all: (node -> player -> bool) -> bool

    method virtual copy: 'self

    method exists f = not (self#for_all (fun v p -> not (f v p)))

    method iter f =
        let _ = self#for_all (fun v p ->
            f v p;
            true
        ) in
        ()

    method find f =
        let i = ref None in
        let _ = self#for_all (fun v p ->
            if f v p then (
                i := Some v;
                false
            ) else true
        ) in
        !i

    method number_solved =
        let count = ref 0 in
        self#iter (fun _ player ->
            if player != plr_undef then incr count
        );
        !count

    method format =
        let s = ref "[" in
        let first = ref true in
        self#iter (fun i w ->
            if !first
            then first := false
            else s := !s ^ ",";
            s := !s ^ nd_show i ^ ":" ^ plr_show w;
        );
        !s ^ "]"

    method merge_inplace (other: 'self) =
        other#iter (fun v p ->
            if (p != plr_undef) then (
                let p' = self#get v in
                if (p' = plr_undef)
                then self#set v p
                else if (p' != p)
                then raise UnmergableSolutions
            )
        )

end


class array_solution (initSize: int) = object (self: 'self)
    inherit solution

    val mutable nodes : player array = Array.make initSize plr_undef

    method copy = {< nodes = nodes >}

    method get v =
        nodes.(nd_to_int v)

    method set v p =
        nodes.(nd_to_int v) <- p

    method for_all f =
        let result = ref true in
        let n = Array.length nodes in
        let i = ref 0 in
        while !result && !i < n do
            result := f (int_to_nd !i) nodes.(!i);
            incr i;
        done;
        !result

end


class map_solution = object (self: 'self)
    inherit solution

    val mutable nodes : (node, player) TreeMap.t = TreeMap.empty_def

    method copy = {< nodes = nodes >}

    method get v =
        try
            TreeMap.find v nodes
        with Not_found -> plr_undef

    method set v p =
        nodes <- TreeMap.add v p nodes

    method for_all f =
        TreeMap.for_all f nodes

end

