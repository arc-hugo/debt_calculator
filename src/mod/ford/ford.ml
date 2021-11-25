open Graph
open Tools

exception Flow_exception of string
exception FordFulkerson_exception of string

type flow = {
   current: int;
   max: int
}

let get_flow (gr: flow graph) (id1: id) (id2: id) =
   try
      match (find_arc gr id1 id2) with
         | Some f -> Some f
         | None -> None
   with
      Graph_error s -> raise (Flow_exception s)

let current_flow (gr: flow graph) (id1: id) (id2: id) =
   match (get_flow gr id1 id2) with
      | Some f -> Some f.current
      | None -> None

let max_flow (gr: flow graph) (id1: id) (id2: id) =
   match (get_flow gr id1 id2) with
      | Some f -> Some f.max
      | None -> None

let rec visit_mem (n1: id) (n2: id) = function
   | [] -> false
   | (n,_)::t -> if (n1 = n) || (n2 = n) then true else visit_mem n1 n2 t

let destack (t: id) (visit: (id * int) list) =
   match (visit = []) || (visit_mem t t visit) with
      | true -> visit
      | false -> List.tl visit

let valid_succ (visit: (id * int) list) ((n2,f): id * int) = not(visit_mem n2 n2 visit) && f > 0

let rec dfs (gr: int graph) (s: id) (t: id) (f: int) (visit: (id * int) list) =
   match (visit_mem s t visit) with
      | true -> visit
      | false -> match List.filter (valid_succ visit) (out_arcs gr s) with
         | [] -> if s = t then ((s,f)::visit) else visit
         | succ -> destack t (List.fold_left (fun visit (next,f) -> dfs gr next t f visit) ((s,f)::visit) succ)

let rec constr_path (gr: int graph) = function
   | [] | _::[] -> []
   | (s,_)::(t,f)::rest -> (s,t,f)::(constr_path gr ((t,f)::rest))

let find_path (gr: int graph) (s: id) (t: id) =
   match node_exists gr s && node_exists gr t with
      | true -> constr_path gr (List.rev(dfs gr s t 0 []))
      | false -> raise (FordFulkerson_exception "find_path: start or target not found")

let rec min (gr: int graph) = function
   | [] -> Int.max_int
   | (_,_,f)::t -> let sub = (min gr t) in
      if f >= sub then sub else f

let rec update_flow (gr: int graph) (m: int) = function
      | [] -> gr
      | (i1,i2,_)::t -> add_arc (add_arc (update_flow gr m t) i1 i2 (-m)) i2 i1 m

let result_graph (ogr: int graph) (fgr: int graph) =
   e_fold ogr (fun rgr i1 i2 m -> 
      match (find_arc fgr i1 i2) with
         | Some f -> new_arc rgr i1 i2 {current=m-f;max=m}
         | None -> raise (FordFulkerson_exception "result_graph: arc not found")
   )
   (clone_nodes ogr)

let ford_fulkerson (gr: int graph) (s: id) (t: id) =
   let rec aux (gr: int graph) (s: id) (t: id) =
      match (find_path gr s t) with
         | [] -> gr
         | path -> aux (update_flow gr (min gr path) path) s t
   in
   (*Init flow*)
   let fgr = aux gr s t in
   result_graph gr fgr
