open Prove
open Util

let _ =
  let e = parse_string Parser.expr_line (read_line ()) in
  let proof = prove e in
  ignore (List.map (print_endline
      |> Expression.string_of_expression) proof)
