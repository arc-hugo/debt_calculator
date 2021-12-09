(* Read a payment record between a set of person.
 * Calculate the money owed between each other. *)

(* The fomat of file used is :
   Name1 MoneyPaid1\n
   Name2 MoneyPaid2\n
   ....
   NameN MoneyPaidN
   *)
open Graph
open Ford

(**************  ALGORITHM  **************)
(* Read the payment record and return a flow graph representing debt of each person. *)
val debt : string -> (string array * flow graph)
(* Export *)
val export_debt : string -> (string array * flow graph) -> unit
