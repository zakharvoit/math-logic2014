open Util
open Arithmetic
open Deduction
open Verification

let _ =
  let print_exprs = Array.iter (print_endline
                               |> string_of_expression) in
  let (assumpts, goal) = parse_string Parser.assumptions_line (read_line () ^ "\n") in
  let lines = read_lines () in
  let exprs = Array.map (parse_string Parser.expr_line) lines in
  let rest = if List.length assumpts = 0
             then []
             else List.tl assumpts in
  let rest_arr = Array.of_list (List.rev rest) in
  for i = 0 to Array.length rest_arr - 1 do
    print_string (string_of_expression rest_arr.(i));
    if i < Array.length rest_arr - 1 then
      print_string ", ";
  done;
  print_string " |- ";
  if List.length assumpts > 0 then
    print_string (string_of_expression (List.hd assumpts) ^ " -> ");
  print_endline (string_of_expression goal);
  let err_message i = "Ввод некорректен начиная с формулы номер " ^ string_of_int i in
  try
    let verified = verify (Array.of_list assumpts) exprs in
    if List.length assumpts = 0 then
      print_exprs exprs
    else
      let deduced = deduce exprs verified rest_arr (List.hd assumpts) in
      print_exprs deduced
  with
  | NotFound i
    -> print_endline (err_message i)
  | NotFree (i, t, e, s)
    -> print_endline (err_message i
                      ^ ": терм "
                      ^ string_of_term t
                      ^ " не свободен для подстановки в формулу "
                      ^ string_of_expression e
                      ^ " вместо переменной "
                      ^ s)
  | FreeIn (i, s, e)
    -> print_endline (err_message i
                      ^ ": переменная "
                      ^ s
                      ^ " входит свободно в формулу "
                     ^ string_of_expression e)

    
