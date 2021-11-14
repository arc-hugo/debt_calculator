open Graph

let clone_nodes (gr: 'a graph) = Graph.n_fold gr (fun ngr i -> Graph.new_node ngr i) Graph.empty_graph
let gmap (gr: 'a graph) (f: ('a -> 'b)) = let cgr = clone_nodes gr in Graph.e_fold gr (fun ngr i1 i2 v -> Graph.new_arc ngr i1 i2 (f v)) cgr
let add_arc (gr: int graph) (i1: id) (i2: id) (n: int) = match Graph.find_arc gr i1 i2 with
    | Some l -> Graph.new_arc gr i1 i2 (n+l)
    | None -> Graph.new_arc gr i1 i2 n