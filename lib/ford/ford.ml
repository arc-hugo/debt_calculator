open Graph
open Tools

(* exception raised by flow getters *)
exception Flow_exception of string

(* exception raised by Ford Fulkerson algorithm *)
exception FordFulkerson_exception of string

(* record for flow results *)
type flow = {
   current: int;
   max: int
}

(* string respresentation of a flow *)
let string_of_flow (f: flow) =
   string_of_int(f.current)^"/"^string_of_int(f.max)

(* sub-function for max_flow and current_flow. *)
let get_flow (gr: flow graph) (id1: id) (id2: id) =
   try
      (* return Some flow between id1 and id2 or None. *)
      match (find_arc gr id1 id2) with
         | Some f -> Some f
         | None -> None
   with
      (* flow exception if id1 doesn't exist. *)
      Graph_error s -> raise (Flow_exception s)

(* return current flow from get_flow result. *)
let current_flow (gr: flow graph) (id1: id) (id2: id) =
   match (get_flow gr id1 id2) with
      | Some f -> Some f.current
      | None -> None

(* return current flow from get_flow result. *)
let max_flow (gr: flow graph) (id1: id) (id2: id) =
   match (get_flow gr id1 id2) with
      | Some f -> Some f.max
      | None -> None

(* return true if n1 or n2 are in the visited nodes list. *)
let rec visit_mem (n1: id) (n2: id) = function
   | [] -> false
   | (n,_)::t -> if (n1 = n) || (n2 = n) then true else visit_mem n1 n2 t

(* pop the first nodes of the visited list if target was not visited. *)
let pop (t: id) (visit: (id * int) list) =
   match (visit = []) || (visit_mem t t visit) with
      | true -> visit
      | false -> List.tl visit

(* true if successor node was not visited and can it's arc can get more flow. *)
let valid_succ (visit: (id * int) list) ((n,f): id * int) = not(visit_mem n n visit) && f > 0

(* depth first search from start node to search a target node. 
 * return the visited node list if target was found or an empty list. *)
let rec dfs (gr: int graph) (s: id) (t: id) (f: int) (visit: (id * int) list) =
   match (visit_mem s t visit) with
      | true -> visit
      | false -> match List.filter (valid_succ visit) (out_arcs gr s) with
         | [] -> if s = t then ((s,f)::visit) else visit
         | succ -> pop t (List.fold_left (fun visit (next,f) -> dfs gr next t f visit) ((s,f)::visit) succ)

(* construct a path with the visited node list. *)
let rec constr_path (gr: int graph) = function
   | [] | _::[] -> []
   | (s,_)::(t,f)::rest -> (s,t,f)::(constr_path gr ((t,f)::rest))

(* return a path between start and target node if it exist. *)
let find_path (gr: int graph) (s: id) (t: id) =
   match node_exists gr s && node_exists gr t with
      | true -> constr_path gr (List.rev(dfs gr s t 0 []))
      | false -> raise (FordFulkerson_exception "find_path: start or target not found")

(* return the min flow that can be added to a given path. *)
let rec min (gr: int graph) = function
   | [] -> Int.max_int
   | (_,_,f)::t -> let sub = (min gr t) in
      if f >= sub then sub else f

(* add the min flow to a given path. *)
let rec update_flow (gr: int graph) (m: int) = function
      | [] -> gr
      | (i1,i2,_)::t -> add_arc (add_arc (update_flow gr m t) i1 i2 (-m)) i2 i1 m

(* convert the last int graph to a flow graph. *)
let result_graph (ogr: int graph) (fgr: int graph) =
   e_fold ogr (fun rgr i1 i2 m -> 
      match (find_arc fgr i1 i2) with
         | Some f -> if m-f >= 0 
            then new_arc rgr i1 i2 {current=m-f;max=m}
            else new_arc rgr i1 i2 {current=m-f;max=m}
         | None -> raise (FordFulkerson_exception "result_graph: arc not found")
   )
   (clone_nodes ogr)

(* apply Ford-Fulkerson algorithm to an int graph from a start node to a target node. *)
let ford_fulkerson (gr: int graph) (s: id) (t: id) =
   let rec aux (gr: int graph) (s: id) (t: id) =
      match (find_path gr s t) with
         | [] -> gr
         | path -> aux (update_flow gr (min gr path) path) s t
   in
   (*Init flow*)
   let fgr = aux gr s t in
   result_graph gr fgr
