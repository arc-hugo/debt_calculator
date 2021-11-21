(* Module of Ford-Fulkerson algorithm.
 * To be used with the Graph module. *)
open Graph

(* Type of flow running through the graph's arcs.
 * Meant to be used as a label. *)
type flow


(**************  CONSTRUCTORS  **************)
(* Create a flow graph from a int graph. *)
val new_flow_graph : int graph -> flow graph

(**************  ALGORITHM  **************)
(* Apply Ford-Fulkerson algorithm to a glow graph between a start and a target.
 * @raise FordFulkerson_exception if start or target not found. *)
val ford_fulkerson : flow graph -> id -> id -> flow graph
