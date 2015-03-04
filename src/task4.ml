open Util
open Arithmetic
open Deduction

let _ =
  let lines = read_lines () in
  let exprs = Array.map (parse_string Parser.expr_line) lines in
  let deduced = deduce exprs [| |] (PVar "A") in
  Array.iter (print_endline
              |> string_of_expression)
             deduced
