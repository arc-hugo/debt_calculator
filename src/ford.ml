open Graph
open Tools

exception FordFulkerson_exception of string



type flow = {
   current: int;
   max: int
}

let find_path (gr: flow graph) (s: id) (t: id) =
   let rec succ gr s t visit = function
	| [] -> []
	| h::rest -> let path = dps gr h t visit in
		if List.mem(t path)
		then s::path
		else succ gr s t visit tail
   in
   let rec dfs gr s t visit =
      if (s = t) then 
         [t]
      else if not(List.mem s visit) && not(List.mem t visit) then
         let succ_nodes = List.fold (fun l (i*f) -> if (f.current - f.max) > 0 then i::l) [] gr in
         succ gr s t (s::visit) succ_nodes
   in
   let constr_path gr = function
      | [] | x::[] -> []
      | s::t::rest -> (find_arc gr s t)::(constr_path gr t::rest)
   in
   if (not(node_exists s)) || (not(node_exists t))
   then FordFulkerson_exception string_of_int(s)^" node or "^string_of_in(t)^" not found"

   constr_path (dfs gr s t)
