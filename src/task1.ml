open Verification
open Util

let _ =
  let asserts = ref [] in
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.expr_line Lexer.token lexbuf in
      asserts := result :: !asserts
    done
  with Lexer.Eof ->
    ignore (Array.map (print_endline |> string_of_annotation)
              (verify (List.rev !asserts)));
    ()
