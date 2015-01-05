open Prove
open Util

let _ =
  let e = parse_string Parser.expr_line (read_line ()) in
  let proof = prove e in
  List.iter (print_endline
             |> Expression.string_of_expression) proof
