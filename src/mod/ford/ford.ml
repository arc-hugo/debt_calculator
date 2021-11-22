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

let new_flow_graph (gr: int graph) = gmap gr (fun v -> {current=0; max=v})

let rec constr_path (gr: flow graph) = function
   | [] | _::[] -> []
   | s::t::rest -> match (find_arc gr s t) with
      | Some f -> (s,t,f)::(constr_path gr (t::rest))
      | None -> raise (FordFulkerson_exception "error find arc")

let rec dfs (gr: flow graph) (s: id) (t: id) (visit: id list) =
   if (List.mem s visit) || (List.mem t visit)
   then visit
   else
      let succ = e_fold gr 
         (fun l n1 n2 f -> if (n1 = s) && not(List.mem n2 visit) && (f.max - f.current) > 0 then n2::l else l)
         []
      in
      List.fold_left (fun visit suiv -> dfs gr suiv t visit) (s::visit) succ

let find_path (gr: flow graph) (s: id) (t: id) =
   if (not(node_exists gr s)) || (not(node_exists gr t))
   then raise (FordFulkerson_exception "start or target not found")
   else 
      let path = List.rev(dfs gr s t []) in
      constr_path gr path

let rec min = function
   | [] -> Int.max_int
   | (_,_,f)::t -> let v1 = min t and v2 = (f.max - f.current) in
      if v1 < v2 then v1 else v2

let ford_fulkerson (gr: flow graph) (s: id) (t: id) =
   let rec aux (gr: flow graph) (s: id) (t: id) =
      let path = find_path gr s t in
      if not(path = []) then
         let m = min path in
         let ngr = e_fold gr 
            (fun ngr i1 i2 f ->
               if List.mem (i1,i2,f) path
               then new_arc ngr i1 i2 {f with current=f.current+m}
               else new_arc ngr i1 i2 f
            )
            (clone_nodes gr)
         in
         aux ngr s t
      else
         gr
   in
   (*Init flow*)
   let igr = gmap gr (fun f -> { f with current = 0 }) in
   aux igr s t
