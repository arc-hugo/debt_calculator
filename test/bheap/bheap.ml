open Bheap

let test_min_add () =
   Printf.printf "   - test min_add";
   (* creating a min value binary heap *)
   let heap = empty_bheap (fun a b -> a < b) in
   (* adding nodes to the heap *)
   let heap = add (add (add (add (add heap 0) 70) 3) 5) 19 in

   (* assert the right extract order *)
   let (a,heap) = extract_root heap in
   assert(a = 0);
   let (a,heap) = extract_root heap in
   assert(a = 3);
   let (a,heap) = extract_root heap in
   assert(a = 5);
   let (a,heap) = extract_root heap in
   assert(a = 19);
   let (a,_) = extract_root heap in
   assert(a = 70);

   Printf.printf " (done)\n";
   ()

let test_max_add () =
   Printf.printf "   - test max_add";
   (* creating a max value binary heap *)
   let heap = empty_bheap (fun a b -> a > b) in
   (* adding nodes to the heap *)
   let heap = add (add (add (add (add heap 0) 70) 3) 5) 19 in

   (* assert the right extract order *)
   let (a,heap) = extract_root heap in
   assert(a = 70);
   let (a,heap) = extract_root heap in
   assert(a = 19);
   let (a,heap) = extract_root heap in
   assert(a = 5);
   let (a,heap) = extract_root heap in
   assert(a = 3);
   let (a,_) = extract_root heap in
   assert(a = 0);

   Printf.printf " (done)\n";
   ()

let tests = [test_min_add; test_max_add]

let () = List.iter (fun f -> f ()) tests

