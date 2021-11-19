open Graph

val find_path: 'a graph -> id -> id -> ('a -> 'a -> bool) -> ('a -> 'b -> 'a) -> 'a out_arcs
