open Tools

open Graph
open Gfile

let () =
   Printf.printf "==== UNIT TEST - TOOLS MODULE ====\n";

   (* new graph with 3 nodes *)
   let graph = new_node (new_node (new_node empty_graph 0) 1) 2  in
   (* adding arcs to the graph *)
   let graph = new_arc (new_arc graph 0 1 5) 1 2 10 in

   (* test of clone_nodes function *)
   let cgr = clone_nodes graph in

   (* assert same set of nodes *)
   assert (not(node_exists cgr 0));
   assert (node_exists cgr 1);
   assert (node_exists cgr 2);

   ()
