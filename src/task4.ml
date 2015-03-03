open Util
open Arithmetic
open Verification

let _ =
  let lines = read_lines () in
  let exprs = Array.map (parse_string Parser.expr_line) lines in
  let annotations = verify [| |] exprs in
  Array.iter (print_endline
              |> string_of_annotation)
             annotations
