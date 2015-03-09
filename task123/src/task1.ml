open Verification
open Util

let _ =
      let lines       = read_lines () in
      let asserts     = Array.map (parse_string Parser.expr_line) lines in
      let annotations = verify asserts in
      for i = 0 to Array.length lines - 1 do
        print_endline ("(" ^ string_of_int (i + 1) ^ ") "
                       ^ lines.(i) ^ " "
                       ^ string_of_annotation annotations.(i))
      done
