open Pgnode
open Pgplayer


exception UnmergableSolutions

class virtual solution : object('self)

    method virtual get: node -> player

    method virtual set: node -> player -> unit

    method virtual for_all: (node -> player -> bool) -> bool

    method virtual copy: 'self

    method exists: (node -> player -> bool) -> bool

    method find: (node -> player -> bool) -> node option

    method iter: (node -> player -> unit) -> unit

    method number_solved: int

    method format: string

    (** Calling merge_solutions_inplace sol1 sol2 adds all solution informations from sol2 to sol1.
        Throws an UnmergableSolutions-Exception if the domain of both solutions is not empty.

        @param solution solution one to merge
    *)
    method merge_inplace: 'self -> unit

end


class array_solution: int -> object ('self)

    method copy: 'self

    method get: node -> player

    method set: node -> player -> unit

    method find: (node -> player -> bool) -> node option

    method iter: (node -> player -> unit) -> unit

    method exists: (node -> player -> bool) -> bool

    method for_all: (node -> player -> bool) -> bool

    method number_solved: int

    method format: string

    method merge_inplace: 'self -> unit

end


class map_solution: object ('self)

    method copy: 'self

    method get: node -> player

    method set: node -> player -> unit

    method find: (node -> player -> bool) -> node option

    method iter: (node -> player -> unit) -> unit

    method exists: (node -> player -> bool) -> bool

    method for_all: (node -> player -> bool) -> bool

    method number_solved: int

    method format: string

    method merge_inplace: 'self -> unit

end