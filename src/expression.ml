open Token

type expression = Var of string
		| Not of expression
		| And of expression * expression
		| Or of expression * expression
		| Impl of expression * expression

let rec string_of_expression = function
  | Var s       -> s
  | Not e       -> "!" ^ string_of_expression e
  | And (a, b)  -> "(" ^ string_of_expression a ^ " & " ^ string_of_expression b ^ ")"
  | Or (a, b)   -> "(" ^ string_of_expression a ^ " | " ^ string_of_expression b ^ ")"
  | Impl (a, b) -> "(" ^ string_of_expression a ^ " -> " ^ string_of_expression b ^ ")"
