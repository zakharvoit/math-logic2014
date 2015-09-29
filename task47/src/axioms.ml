let classical_axioms = Array.map (Util.parse_string Parser.expr_line)
                       [| "A -> B -> A"
                        ; "(A -> B) -> (A -> B -> C) -> (A -> C)"
                        ; "A -> B -> A & B"
                        ; "A & B -> A"
                        ; "A & B -> B"
                        ; "A -> A | B"
                        ; "B -> A | B"
                        ; "(A -> C) -> (B -> C) -> (A | B -> C)"
                        ; "(A -> B) -> (A -> !B) -> !A"
                        ; "!!A -> A"
                       |]

let formal_axioms = Array.map (Util.parse_string Parser.expr_line)
                              [| "a = b -> a' = b'"
                               ; "a = b -> a = c -> b = c"
                               ; "a' = b' -> a = b"
                               ; "!a' = 0"
                               ; "a + b' = (a + b)'"
                               ; "a + 0 = a"
                               ; "a * 0 = 0"
                               ; "a * b' = a * b + a"
                              |]
