(* Function composition operator *)
let (|>) f g x = f (g x)

let parse_string parser = parser Lexer.token
                               |> Lexing.from_string
                               |> (fun s -> s ^ "\n")
