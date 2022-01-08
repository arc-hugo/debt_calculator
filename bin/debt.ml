open Debt

(* Read a debt record file given by an user and export an output Graphviz file of a resulting flow graph. *)
let () =
   if Array.length Sys.argv <> 3 then
      begin
         Printf.printf
        "\n ✻  Usage: %s infile outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a payment record\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 1
      end ;

    let infile = Sys.argv.(1) 
    and outfile = Sys.argv.(2)
    in

    let debt = debt infile in

    export_debt outfile debt
