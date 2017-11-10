open Pgnode
open Pgplayer




exception UnmergableStrategies

class virtual strategy : object('self)

    (** Get decision for a node based on strategy.

        @param node node to get decision for
        @param successor of node based on strategy
     *)
    method virtual get: node -> node

    (** Records the strategy decision based node -> node parameters.

        @param strategy strategy to record decision for
        @param node predecessor
        @param node successor
     *)
    method virtual set: node -> node -> unit

    method virtual for_all: (node -> node -> bool) -> bool

    method virtual copy: 'self

    method map: (node -> node -> node) -> 'self

    method exists: (node -> node -> bool) -> bool

    method find: (node -> node -> bool) -> node option

    (** Iterate over all nodes and their corresponding successors
        in a strategy.

        @param (node -> node -> unit) function to use for each pair of node + successor
     *)
    method iter: (node -> node -> unit) -> unit

    (** Returns string representation of strategy.

        @return string representation of strategy
     *)
    method format: string

    (** Calling merge_inplace strat1 strat2 adds all strategy decisions from strat2 to strat1.
        Throws an StraegiesUnmergable-Exception if the domain of both strategies is not empty.

        @param strategy strategy two to merge
    *)
    method merge_inplace: 'self -> unit

end


class array_strategy: int -> object ('self)

    method copy: 'self

    method map: (node -> node -> node) -> 'self

    method get: node -> node

    method set: node -> node -> unit

    method find: (node -> node -> bool) -> node option

    method iter: (node -> node -> unit) -> unit

    method exists: (node -> node -> bool) -> bool

    method for_all: (node -> node -> bool) -> bool

    method format: string

    method merge_inplace: 'self -> unit

end


class map_strategy: object ('self)

    method copy: 'self

    method map: (node -> node -> node) -> 'self

    method get: node -> node

    method set: node -> node -> unit

    method find: (node -> node -> bool) -> node option

    method iter: (node -> node -> unit) -> unit

    method exists: (node -> node -> bool) -> bool

    method for_all: (node -> node -> bool) -> bool

    method format: string

    method merge_inplace: 'self -> unit

end