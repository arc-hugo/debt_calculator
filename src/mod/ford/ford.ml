open Graph
open Tools

exception FordFulkerson_exception of string

type flow = {
   current: int;
   max: int
}


let new_flow_graph (gr: int graph) = gmap gr (fun v -> {current = 0; max = v})

let rec constr_path gr = function
   | [] | _::[] -> []
   | s::t::rest -> match (find_arc gr s t) with
      | Some v -> (s,t,v)::(constr_path gr (t::rest))
      | None -> raise (FordFulkerson_exception "error find arc")

let rec dfs (gr: flow graph) (s: id) (t: id) (visit: id list) =
   let rec succ (gr: flow graph) (s: id) (t: id) (visit: id list) = function
      | [] -> []
      | h::rest -> let path = dfs gr h t visit in
      if (List.mem t path) then 
         s::path
      else
         succ gr s t visit rest
   in
   if (s = t) then 
      [t]
   else if not(List.mem s visit) && not(List.mem t visit) then
      let succ_nodes = e_fold gr (fun l _ n2 f -> if (f.current - f.max) > 0 then n2::l else l) [] in
      succ gr s t (s::visit) succ_nodes
   else
      []

let find_path (gr: flow graph) (s: id) (t: id) =
   if (not(node_exists gr s)) || (not(node_exists gr t))
   then raise (FordFulkerson_exception "start or target not found")
   else constr_path gr (dfs gr s t [])

let rec min = function
   | [] -> 0
   | (_,_,f)::t -> let v1 = min t and v2 = (f.max - f.current) in
   if v1 < v2 then
      v1
   else
      v2

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
