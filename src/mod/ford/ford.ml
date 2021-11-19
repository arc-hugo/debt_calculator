open Graph
open Tools

exception FordFulkerson_exception of string

type flow = {
   current: int;
   max: int
}

let find_path (gr: flow graph) (s: id) (t: id) =
   let rec dfs (gr: flow graph) (s: id) (t: id) (visit: id list) =
   	let rec succ (gr: flow graph) (s: id) (t: id) (visit: id list) = function
			| [] -> []
			| h::rest -> let path = dfs gr h t visit in
				if (List.mem t path)
				then s::path
				else succ gr s t visit rest
   	in
      if (s = t) then 
			[t]
      else if not(List.mem s visit) && not(List.mem t visit) then
         let succ_nodes = e_fold gr (fun l _ n2 f -> if (f.current - f.max) > 0 then n2::l else l) [] in
         succ gr s t (s::visit) succ_nodes
		else
			[]
   in
   let rec constr_path gr = function
      | [] | _::[] -> []
      | s::t::rest -> (find_arc gr s t)::(constr_path gr (t::rest))
   in
   if (not(node_exists gr s)) || (not(node_exists gr t))
   then raise (FordFulkerson_exception "start or target not found")
	else constr_path gr (dfs gr s t [])
