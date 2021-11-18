(* Set of tools used with the Graph module *)
open Graph

(**************  METHODES  **************)

(* Returns a new graph having the same nodes but no arcs *)
val clone_nodes: 'a graph -> 'b graph
(* Maps a function on all arcs *)
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
(* Add value to an arc between two ids.
 * If the arc doesn't exist, it is created. *)
val add_arc: int graph -> id -> id -> int -> int graph

