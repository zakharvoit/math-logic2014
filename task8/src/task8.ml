let _ = let equal = Parser.comparison Lexer.token (Lexing.from_channel stdin) in
        print_endline (if equal then "Равны" else "Не равны")
