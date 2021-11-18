open Graph
open Tools

exception Dijkstra_exception of string

type 'a status = Marked of 'a | Current of 'a | Unknown of 'a

let find_path (gr: 'a graph) (s: id) (t: id) (cmp: ('a -> 'a -> bool) (op: ('a -> 'b -> 'a)) =
   if (not(node_exists s)) || (not(node_exists t)) 
   then raise Dijkstra_exception "start or target not found"

   let dgr = n_fold (gmap (fun a -> Unknown of 'a) gr) in
