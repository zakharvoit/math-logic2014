(* Function composition operator *)
let (|>) f g x = f (g x)

let parse_string parser = parser Lexer.token
                               |> Lexing.from_string
                               |> (fun s -> s ^ "\n")

let read_lines _ =
  let lines_list   = ref [] in
  try
    while true do
      lines_list := String.trim (read_line ()) :: !lines_list
    done;
    [| |]
  with End_of_file -> Array.of_list (List.rev !lines_list)
