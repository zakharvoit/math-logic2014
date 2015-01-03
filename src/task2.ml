module V = Verification
open Deduction
open Util

let _ =
  let (assumptions, b)
    = parse_string Parser.assumptions_line (read_line ()) in
  let asserts = Array.map (parse_string Parser.expr_line) (read_lines ()) in
  let proof = V.verify_with_assumptions asserts (Array.of_list assumptions) in
  let deduction = deduce asserts proof (List.hd assumptions) b in
  ignore (Array.map (print_endline |> Expression.string_of_expression) deduction)

