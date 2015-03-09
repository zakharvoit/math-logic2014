module H = Hashtbl

type expression = Var of string
		| Not of expression
		| And of expression * expression
		| Or of expression * expression
		| Impl of expression * expression

let (!@) e = Not e
let (@&) a b = And (a, b)
let (@|) a b = Or (a, b)
let (@->) a b = Impl (a, b)

let rec string_of_expression = function
  | Var s       -> s
  | Not e       -> "!" ^ string_of_expression e
  | And (a, b)  -> "(" ^ string_of_expression a ^ " & " ^ string_of_expression b ^ ")"
  | Or (a, b)   -> "(" ^ string_of_expression a ^ " | " ^ string_of_expression b ^ ")"
  | Impl (a, b) -> "(" ^ string_of_expression a ^ " -> " ^ string_of_expression b ^ ")"

class matcher =
object (self)
  val context = (H.create 1024 : (string, expression) H.t)

  method var_matches a b =
    if H.mem context b then
      H.find context b = a
    else begin
        H.add context b a;
        true
      end

  method matches a b = match (a, b) with
    | (Impl (a1, a2), Impl (b1, b2))
    | (Or (a1, a2), Or (b1, b2))
    | (And (a1, a2), And (b1, b2)) -> self#matches a1 b1 && self#matches a2 b2
    | (Not a', Not b')             -> self#matches a' b'
    | (a', Var sb)                 -> self#var_matches a' sb
    | (_, _)                       -> false
end

let (=~~) a b =
  let m = new matcher in
  m#matches a b
