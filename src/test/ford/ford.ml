open Ford
open Graph
open Gfile
open Tools

let graph = gmap (from_file "./ford1") (fun s -> int_of_string s)

let test_new_flow_graph () =
   Printf.printf "   - test new_flow_graph";
   (* test of new_flow_graph function *)
   let fgr = new_flow_graph graph in
   (* assert same set of nodes *)
   assert (node_exists fgr 0);
   assert (node_exists fgr 1);
   assert (node_exists fgr 2);
   assert (node_exists fgr 3);
   (* assert current flow of arcs *)
   assert ((current_flow fgr 0 1) = Some 0);
   assert ((current_flow fgr 0 2) = Some 0);
   assert ((current_flow fgr 1 2) = Some 0);
   assert ((current_flow fgr 2 3) = Some 0);
   assert ((current_flow fgr 1 3) = Some 0);
   (* assert max flow of arcs *)
   assert ((max_flow fgr 0 1) = Some 2);
   assert ((max_flow fgr 0 2) = Some 4);
   assert ((max_flow fgr 1 2) = Some 3);
   assert ((max_flow fgr 2 3) = Some 5);
   assert ((max_flow fgr 1 3) = Some 1);

   Printf.printf " (done)\n";
   ()

let test_ford_fulkerson () =
   (* test of ford_fulkerson function *)
   let fgr = ford_fulkerson (new_flow_graph graph) 0 3 in

   (* assert same set of nodes *)
   assert (node_exists fgr 0);
   assert (node_exists fgr 1);
   assert (node_exists fgr 2);
   assert (node_exists fgr 3);
   
   (* assert max flow of arcs *)
   assert ((max_flow fgr 0 1) = Some 2);
   assert ((max_flow fgr 0 2) = Some 4);
   assert ((max_flow fgr 1 2) = Some 3);
   assert ((max_flow fgr 2 3) = Some 5);
   assert ((max_flow fgr 1 3) = Some 1);
   
   (* assert current flow of arcs *)
   assert ((current_flow fgr 0 1) = Some 2);
   assert ((current_flow fgr 0 2) = Some 4);
   assert ((current_flow fgr 1 2) = Some 1);
   assert ((current_flow fgr 2 3) = Some 5);
   assert ((current_flow fgr 1 3) = Some 1);

   Printf.printf "   - test new_flow_graph (done)\n%!";
   ()

let tests = [test_new_flow_graph; test_ford_fulkerson]

let () = List.iter (fun f -> f ()) tests
