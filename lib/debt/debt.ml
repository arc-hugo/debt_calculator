open Str
open Graph
open Tools
open Ford

(* debt record reader *)
let from_file path graph =
   let infile = open_in path in
   let rec loop n names graph =
      (* loop until end of file *)
      try
         let line = input_line infile in

         (* Remove leading and trailing spaces. *)
         let line = split (regexp "[ \t]+") line in

         let (n2, names2, graph2) =
            match (line) with
               (* Ignore empty or incorrect lines *)
               | [] -> (n, names, graph)
               | _::[] -> (n, names, graph)
               | ""::_ -> (n, names, graph)
               (* Dept line*)
               | name::d::_ -> (n+1, (name,int_of_string(d))::names, new_node graph n)
         in
         loop n2 names2 graph2
      with End_of_file -> (List.rev(names),graph) (* Done *)
   in
   let debt = loop 2 [] graph in
   close_in infile;
   debt

(* Link node to source or target depending on their individual debts. *)
let rec link_st graph n = function
      | [] -> graph
      | (_,d)::t -> if d > 0
         then link_st (new_arc graph n 1 d) (n+1) t
         else if d < 0
            then link_st (new_arc graph 0 n (-d)) (n+1) t
         else
            link_st graph (n+1) t

(* Add an arc of great flow between each node that is not source or target. *)
let not_id current graph i =
   match ((i = current) || (i = 0) || (i = 1)) with
      | false -> add_arc graph current i (Int.max_int/2)
      | true -> graph

(* Link all other nodes with arc of great flow. *)
let rec link_all graph max n =
   match (n = max) with
      | false -> link_all (n_fold graph (not_id n) graph) max (n+1)
      | true -> graph

let debt path =
   (* Create source and target. *)
   let graph = new_node (new_node empty_graph 0) 1 in
   let (names, graph) = from_file path graph in

   (* calculate individual debt. *)
   let m = (List.fold_left (fun s (_,d) -> s+d) 0 names)/(List.length names) in
   let indiv_debt = List.map (fun (n,d) -> (n,(d-m))) names in

   (* link to source or target. *)
   let graph = link_st graph 2 indiv_debt in

   (* link all other nodes. *)
   let graph = link_all graph ((List.length names)+2) 2 in

   (* Generate result graph and return it with the names array. *)
   (Array.of_list(List.rev (List.fold_left (fun l (n,_) -> n::l) [] names)), (ford_fulkerson graph 0 1))

(* Filter negative or unused arcs between user nodes. *)
let filter_negat ff names debt id1 id2 =
   match (current_flow debt id1 id2) with
      | Some c -> if c <= 0 
         then Printf.fprintf ff ""
         else Printf.fprintf ff "\"%s\" -> \"%s\" [label = \"%s\"];\n" names.((id1-2)) names.((id2-2)) (string_of_int(c)^"/inf")
      | None -> Printf.fprintf ff ""

(* Print arcs of the graph to the file. *)
let print_arcs ff names debt id1 id2 lbl =
   match (id1,id2,lbl) with
      (* Format of arc from source to other node. *)
      | 0,n,f -> Printf.fprintf ff "\"source\" -> \"%s\" [label = \"%s\"];\n" names.((n-2)) (string_of_flow f)
      (* Format of arc from node to target. *)
      | n,1,f -> Printf.fprintf ff "\"%s\" -> \"target\" [label = \"%s\"];\n" names.((n-2)) (string_of_flow f)
      | n1,n2,_ -> filter_negat ff names debt n1 n2


let export_debt (path: string) ((names,debt): (string array * flow graph)) =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  Printf.fprintf ff "digraph diagram_out{\n   rankdir=LR;\n   size=\"8,5\"\n   node [shape = circle];\n" ;

  (* Write all arcs *)
  e_iter debt (print_arcs ff names debt);

  Printf.fprintf ff "\n}" ;
  
  close_out ff ;
  ()
