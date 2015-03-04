type term = Zero
          | Succ of term
          | Plus of term * term
          | Mul of term * term
          | Var of string

type expression = Impl of expression * expression
                | Or of expression * expression
                | And of expression * expression
                | Not of expression
                | Forall of string * expression
                | Exists of string * expression
                | Predicate of string * (term list)
                | PVar of string

let rec string_of_term = function
  | Zero        -> "0"
  | Var s       -> s
  | Succ a      -> string_of_term a ^ "'"
  | Plus (a, b) -> string_of_term a ^ " + " ^ string_of_term b
  | Mul (a, b)  -> string_of_term a ^ " + " ^ string_of_term b

let rec string_of_expression =
  let rec comma_separate = function
    | []  -> ""
    | [a] -> a
    | x :: xs -> x ^ "," ^ comma_separate xs
  in function
  | Not e            -> "!" ^ string_of_expression e
  | And (a, b)       -> "(" ^ string_of_expression a ^ " & " ^ string_of_expression b ^ ")"
  | Or (a, b)        -> "(" ^ string_of_expression a ^ " | " ^ string_of_expression b ^ ")"
  | Impl (a, b)      -> "(" ^ string_of_expression a ^ " -> " ^ string_of_expression b ^ ")"
  | Forall (x, e)    -> "@" ^ x ^ " " ^ string_of_expression e
  | Exists (x, e)    -> "?" ^ x ^ " " ^ string_of_expression e
  | Predicate (p, l) -> if p = "="
                        then string_of_term (List.hd l) ^ " = " ^ string_of_term (List.hd (List.tl l))
                        else p ^ "(" ^ comma_separate (List.map string_of_term l) ^ ")"
  | PVar s           -> s

let rec substitute get_var = function
  | PVar name   -> get_var name
  | Not a       -> Not (substitute get_var a)
  | And (a, b)  -> And (substitute get_var a,
                        substitute get_var b)
  | Or (a, b)   -> Or (substitute get_var a,
                        substitute get_var b)
  | Impl (a, b) -> Impl (substitute get_var a,
                        substitute get_var b)
  | other       -> other
