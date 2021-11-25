(* Module of Ford-Fulkerson algorithm.
 * To be used with the Graph module. *)
open Graph

(* Type of flow running through the graph's arcs.
 * Meant to be used as a label. *)
type flow

exception Flow_exception of string
exception FordFulkerson_exception of string

(**************  GETTERS  **************)
(* Return current flow of an arc between id1 and id2.
 * Return None if the arc does not exist.
 * @raise Flow_exception id id1 is unknown. *)
val current_flow : flow graph -> id -> id -> int option
(* Return max flow of an arc between id1 and id2.
 * Return None if the arc does not exist.
 * @raise Flow_exception id id1 is unknown. *)
val max_flow : flow graph -> id -> id -> int option

(**************  ALGORITHM  **************)
(* Apply Ford-Fulkerson algorithm to a glow graph between a start and a target nodes.
 * @raise FordFulkerson_exception if start or target not found. *)
val ford_fulkerson : int graph -> id -> id -> flow graph
