open Tools
open Graph

(* new graph with 3 nodes *)
let graph = new_node (new_node (new_node empty_graph 0) 1) 2
(* adding arcs to the graph *)
let graph = new_arc (new_arc graph 0 1 5) 1 2 10

let test_clone () =
   Printf.printf "   - test clone_nodes";
   (* test of clone_nodes function *)
   let cgr = clone_nodes graph in
   (* assert same set of nodes *)
   assert (node_exists cgr 0);
   assert (node_exists cgr 1);
   assert (node_exists cgr 2);
   assert ((out_arcs cgr 0) = []);
   assert ((out_arcs cgr 1) = []);
   assert ((out_arcs cgr 2) = []);

   Printf.printf " (done)\n";
   ()

let test_gmap () =
   Printf.printf "   - test gmap";
   (* test of clone_nodes function *)
   let cgr = gmap graph (fun v -> v*2) in
   (* assert same set of nodes *)
   assert (node_exists cgr 0);
   assert (node_exists cgr 1);
   assert (node_exists cgr 2);
   assert ((out_arcs cgr 0) = [(1,10)]);
   assert ((out_arcs cgr 1) = [(2,20)]);
   assert ((out_arcs cgr 2) = []);

   Printf.printf " (done)\n";
   ()

let test_add_arc () =
   Printf.printf "   - test add_arc";
   (* test of clone_nodes function *)
   let cgr = add_arc graph 2 0 20 in
   (* assert same set of nodes *)
   assert (node_exists cgr 0);
   assert (node_exists cgr 1);
   assert (node_exists cgr 2);
   assert ((out_arcs cgr 0) = [(1,5)]);
   assert ((out_arcs cgr 1) = [(2,10)]);
   assert ((out_arcs cgr 2) = [(0,20)]);

   Printf.printf " (done)\n";
   ()

let tests = [test_clone; test_gmap; test_add_arc]

let () = 
   Printf.printf "==== UNIT TEST - TOOLS MODULE ====\n";
   List.iter (fun f -> f ()) tests

