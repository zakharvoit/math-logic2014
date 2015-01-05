module V = Verification
open Deduction
open Util

let _ =
  let (assumptions, b)
    = parse_string Parser.assumptions_line (read_line ()) in
  let asserts = Array.map (parse_string Parser.expr_line) (read_lines ()) in
  let proof = V.verify_with_assumptions asserts (Array.of_list assumptions) in
  let deduction = deduce asserts proof (List.hd assumptions) b in
  let assumpts_str = Array.of_list (List.map Expression.string_of_expression assumptions) in
  for i = Array.length assumpts_str - 1 downto 1 do
    print_string assumpts_str.(i);
    if i > 1 then print_string ", "
    else print_string " ";
  done;
  print_string "|- ";
  print_string (assumpts_str.(0) ^ " -> ");
  print_endline (Expression.string_of_expression b);
  Array.iter (print_endline |> Expression.string_of_expression) deduction
