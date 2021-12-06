open Debt
open Ford
open Graph

let (names,debt) = debt "./debt1"

let test_debt () =
   (* assert same names *)
   assert (names = ["John";"Kate";"Ann"]);
   
   (* assert same set of nodes *)
   assert (node_exists debt 0);
   assert (node_exists debt 1);
   assert (node_exists debt 2);
   assert (node_exists debt 3);
   assert (node_exists debt 4);

   (* assert debt paid *)
   assert ((current_flow debt 0 3) = Some 10);
   assert ((current_flow debt 0 4) = Some 10);

   (* assert money received *)
   assert ((current_flow debt 2 1) = Some 20);
   
   Printf.printf "   - test debt (done)\n%!";
   ()

let tests = [test_debt]

let () = List.iter (fun f -> f ()) tests
