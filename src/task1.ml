open Lexer

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.expr_line Lexer.token lexbuf in
      print_endline (Expression.string_of_expression result);
    done
  with Lexer.Eof ->
    exit 0
