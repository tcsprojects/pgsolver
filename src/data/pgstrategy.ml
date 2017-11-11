open Pgnode;;
open Pgplayer;;
open Tcsset;;


exception UnmergableStrategies

class virtual strategy = object(self: 'self)

    method virtual get: node -> node

    method virtual set: node -> node -> unit

    method virtual for_all: (node -> node -> bool) -> bool

    method virtual copy: 'self

    method exists f = not (self#for_all (fun v w -> not (f v w)))

    method map f =
        let c = self#copy in
        c#iter (fun i j -> c#set i (f i j));
        c

    method iter f =
        let _ = self#for_all (fun v w ->
            f v w;
            true
        ) in
        ()

    method find f =
        let i = ref None in
        let _ = self#for_all (fun v w ->
            if f v w then (
                i := Some v;
                false
            ) else true
        ) in
        !i

    method format =
        let s = ref "[" in
        let first = ref true in
        self#iter (fun i w ->
            if !first
            then first := false
            else s := !s ^ ",";
            s := !s ^ nd_show i ^ "->" ^ nd_show w;
        );
        !s ^ "]"

    method merge_inplace (other: 'self) =
        other#iter (fun v w ->
            if (w != nd_undef) then (
                let w' = self#get v in
                if (w' = nd_undef)
                then self#set v w
                else if (w' != w)
                then raise UnmergableStrategies
            )
        )

end


class array_strategy (initSize: int) = object (self: 'self)
    inherit strategy

    val mutable nodes : node array = Array.make initSize nd_undef

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


class map_strategy = object (self: 'self)
    inherit strategy

    val mutable nodes : (node, node) TreeMap.t = TreeMap.empty_def

    method copy = {< nodes = nodes >}

    method get v =
        try
            TreeMap.find v nodes
        with Not_found -> nd_undef

    method set v p =
        nodes <- TreeMap.add v p nodes

    method for_all f =
        TreeMap.for_all f nodes

end

