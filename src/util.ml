(* Function composition operator *)
let (|>) f g x = f (g x)

let parse_string parser = parser Lexer.token
                               |> Lexing.from_string
                               |> (fun s -> s ^ "\n")

let trim s = 
  let is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false in
  let len = String.length s in
  let i = ref 0 in
  while !i < len && is_space (String.get s !i) do
    incr i
  done;
  let j = ref (len - 1) in
  while !j >= !i && is_space (String.get s !j) do
    decr j
  done;
  if !i = 0 && !j = len - 1 then
    s
  else if !j >= !i then
    String.sub s !i (!j - !i + 1)
  else
    ""
;;

let f_read_lines channel =
 let lines_list   = ref [] in
  try
    while true do
      lines_list := trim (input_line channel) :: !lines_list
    done;
    [| |]
  with End_of_file -> Array.of_list (List.rev !lines_list)
  
let read_lines _ = f_read_lines stdin
