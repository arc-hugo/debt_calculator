open Ford
open Graph
open Gfile
open Tools

let graph1 = gmap (from_file "./ford1") (fun s -> int_of_string s)
let graph2 = gmap (from_file "./ford2") (fun s -> int_of_string s)

let test_ford_fulkerson () =
   (* test of ford_fulkerson function *)
   let fgr1 = ford_fulkerson graph1 0 3 in
   let fgr2 = ford_fulkerson graph2 0 7 in
   
   (* assert same set of nodes *)
   assert (node_exists fgr1 0);
   assert (node_exists fgr1 1);
   assert (node_exists fgr1 2);
   assert (node_exists fgr1 3);
   
   assert (node_exists fgr2 0);
   assert (node_exists fgr2 1);
   assert (node_exists fgr2 2);
   assert (node_exists fgr2 3);
   assert (node_exists fgr2 4);
   assert (node_exists fgr2 5);
   assert (node_exists fgr2 6);
   assert (node_exists fgr2 7);

   (* assert max flow of arcs *)
   assert ((max_flow fgr1 0 1) = Some 2);
   assert ((max_flow fgr1 0 2) = Some 4);
   assert ((max_flow fgr1 1 2) = Some 3);
   assert ((max_flow fgr1 2 3) = Some 5);
   assert ((max_flow fgr1 1 3) = Some 1);

   assert ((max_flow fgr2 0 1) = Some 13);
   assert ((max_flow fgr2 0 2) = Some 7);
   assert ((max_flow fgr2 0 3) = Some 12);
   assert ((max_flow fgr2 1 2) = Some 5);
   assert ((max_flow fgr2 1 4) = Some 8);
   assert ((max_flow fgr2 2 5) = Some 10);
   assert ((max_flow fgr2 2 6) = Some 5);
   assert ((max_flow fgr2 3 2) = Some 2);
   assert ((max_flow fgr2 3 6) = Some 10);
   assert ((max_flow fgr2 4 5) = Some 2);
   assert ((max_flow fgr2 4 7) = Some 10);
   assert ((max_flow fgr2 5 7) = Some 7);
   assert ((max_flow fgr2 6 7) = Some 15);

   (* assert current flow of arcs *)
   assert ((current_flow fgr1 2 3) = Some 5);
   assert ((current_flow fgr1 1 3) = Some 1);

   assert ((current_flow fgr2 4 7) = Some 8);
   assert ((current_flow fgr2 5 7) = Some 7);
   assert ((current_flow fgr2 6 7) = Some 15);

   Printf.printf "   - test ford_fulkerson (done)\n%!";
   ()

let tests = [test_ford_fulkerson]

let () = List.iter (fun f -> f ()) tests
