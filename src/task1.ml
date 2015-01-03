open Verification
open Util

let _ =
  let lines_list   = ref [] in
  try
    while true do
      lines_list := String.trim (read_line ()) :: !lines_list
    done
  with End_of_file -> begin
      let lines       = Array.of_list (List.rev !lines_list) in
      let asserts     = Array.map (parse_string Parser.expr_line) lines in
      let annotations = verify asserts in
      for i = 0 to Array.length lines - 1 do
        print_endline ("(" ^ string_of_int (i + 1) ^ ") "
                       ^ lines.(i) ^ " "
                       ^ string_of_annotation annotations.(i))
      done
    end
