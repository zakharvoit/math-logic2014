type term = Zero
          | Succ of term
          | Plus of term * term
          | Mul of term * term
          | Var of string
          | Function of string * (term list)

type expression = Impl of expression * expression
                | Or of expression * expression
                | And of expression * expression
                | Not of expression
                | Forall of string * expression
                | Exists of string * expression
                | Predicate of string * (term list)
                | PVar of string

type var_or_expr = Variable of string | Expr of expression

let to_variable = function
  | Variable a -> a
  | _ -> failwith "Term expected"

let to_expr = function
  | Expr a -> a
  | _ -> failwith "Term expected"

let rec comma_separate = function
    | []  -> ""
    | [a] -> a
    | x :: xs -> x ^ "," ^ comma_separate xs

let rec string_of_term = function
  | Zero        -> "0"
  | Var s       -> s
  | Succ a      -> string_of_term a ^ "'"
  | Plus (a, b) -> string_of_term a ^ " + " ^ string_of_term b
  | Mul (a, b)  -> string_of_term a ^ " + " ^ string_of_term b
  | Function (s, args) -> s ^ "(" ^ comma_separate (List.map string_of_term args) ^ ")"

let rec string_of_expression = function
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


let rec substitute_in_term get_var = function
  | Var name -> Var (to_variable (get_var name))
  | Plus (a, b) -> Plus (substitute_in_term get_var a,
                        substitute_in_term get_var b)
  | Mul (a, b) -> Mul (substitute_in_term get_var a,
                       substitute_in_term get_var b)
  | Succ a -> Succ (substitute_in_term get_var a)
  | Zero -> Zero
  | Function (s, args) -> Function (s, List.map (substitute_in_term get_var) args)

let rec substitute get_var = function
  | PVar name   -> to_expr (get_var name)
  | Not a       -> Not (substitute get_var a)
  | And (a, b)  -> And (substitute get_var a,
                        substitute get_var b)
  | Or (a, b)   -> Or (substitute get_var a,
                        substitute get_var b)
  | Impl (a, b) -> Impl (substitute get_var a,
                        substitute get_var b)
  | Forall (x, a) -> Forall (to_variable (get_var x), substitute get_var a)
  | Exists (x, a) -> Exists (to_variable (get_var x), substitute get_var a)
  | Predicate (s, args) -> Predicate (s, List.map (substitute_in_term get_var) args)
