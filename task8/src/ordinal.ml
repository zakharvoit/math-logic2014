type ordinal = Nat of int
             | Big of ordinal * int * ordinal

let zero  = Nat 0
let one   = Nat 1
let omega = Big (one, 1, zero)

let rec to_str = function
  | Nat x         -> string_of_int x
  | Big (e, c, r) -> string_of_int c ^ "*"
                     ^ ("w^("^ to_str e ^ ")")
                     ^ "+" ^ to_str r

(* Natural expontiation in O(log n) *)
let rec exp_nat a n =
  if n = 0 then 1
  else if n mod 2 = 0 then let b = exp_nat a (n / 2) in b * b
  else exp_nat a (n - 1) * a

(* First exponent *)
let fe = function
  | Nat _         -> zero
  | Big (e, _, _) -> e

(* First coefficient *)
let fc = function
  | Nat a         -> a
  | Big (_, c, _) -> c

let to_nat = function
  | Nat a -> a
  | _     -> failwith "to_nat: Nat expected"

let rest = function
  | Nat a         -> Nat 0 (* failwith "rest: Not Nat expected" *)
  | Big (_, _, r) -> r

let atom = function
  | Nat _ -> true
  | _     -> false

let unite a b = let (x, y) = a in Big (x, y, b)
let monomial a = unite a zero

let first = function
  | Nat a         -> failwith "first: Nat not expected"
  | Big (e, c, r) -> (e, c)

let rec firstn a n =
  if n = 0 then Nat 0
  else unite (first a) (firstn (rest a) (n - 1))

let rec add_powers a b = match a with
  | Nat 0         -> b
  | Big (e, c, r) -> Big (e, c, add_powers r b)
  | _             -> failwith "add_powers: a should and with zero"

let rec ord_length = function
  | Nat _         -> 0
  | Big (_, _, l) -> ord_length l + 1

let rec restn a n =
  if n = 0 then a
  else restn (rest a) (n - 1)

let rec cmp a b = match (a, b) with
  | (Nat a, Nat b) -> compare a b
  | (Nat a, _)     -> -1
  | (_, Nat b)     -> 1
  | _              -> let cmp_e = cmp (fe a) (fe b) in
                      let cmp_c = compare (fc a) (fc b) in
                      if cmp_e != 0 then cmp_e
                      else if cmp_c != 0 then cmp_c
                      else cmp (rest a) (rest b)

let rec add a b = match (a, b) with
  | (Nat a, Nat b) -> Nat (a + b)
  | _              -> let cmp_e = cmp (fe a) (fe b) in
                      if cmp_e < 0 then b
                      else if cmp_e = 0 then unite (fe a, fc a + fc b) (rest b)
                      else unite (fe a, fc a) (add (rest a) b)

let rec sub a b = match (a, b) with
  | (Nat a, Nat b) -> if a <= b then zero
                      else Nat (a - b)
  | _              -> let cmp_e = cmp (fe a) (fe b) in
                      let cmp_c = compare (fc a) (fc b) in
                      if cmp_e < 0 then zero
                      else if cmp_e > 0 then a
                      else if cmp_c < 0 then zero
                      else if cmp_c > 0 then unite (fe a, fc a - fc b) (rest a)
                      else sub (rest a) (rest b)

let rec padd a b n = add_powers (firstn a n) (add (restn a n) b)

let rec mul a b = match (a, b) with
  | (Nat 0, _)      -> zero
  | (_, Nat 0)      -> zero
  | (Nat a, Nat b) -> Nat (a * b)
  | (_, Nat b)     -> unite (fe a, fc a * b) (rest a)
  | _              -> unite (add (fe a) (fe b), fc b) (mul a (rest b))

let rec exp_nat_ord a_ord b =
  let a = to_nat (a_ord) in
  if (fe b) = one
  then monomial (Nat (fc b), exp_nat a (to_nat (rest b)))
  else if atom (rest b)
  then let e = monomial (sub (fe b) one, fc b) in
       monomial (e, exp_nat a (to_nat (rest b)))
  else let c = exp_nat_ord a_ord (rest b) in
       monomial (unite (sub (fe b) one, 1) (fe c), fc c)

let rec exp_limit_nat a b_ord =
  let b = to_nat b_ord in
  if b = 1 then a
  else mul (monomial (mul (fe a) (Nat (b - 1)), 1)) a

let rec is_limit = function
  | Nat a -> a = 0
  | a     -> is_limit (rest a)

let rec limitpart = function
  | Nat _         -> zero
  | Big (a, b, c) -> unite (a, b) (limitpart c)

let rec natpart = function
  | Nat a -> Nat a
  | a     -> natpart (rest a)


let rec exp_helper a p n q =
  if q = 0 then p
  else padd (mul (exp_limit_nat a (Nat q)) p) (exp_helper a p n (q - 1)) n

let rec exp_ord_nat a q_ord =
  let q = to_nat q_ord in
  if q = 0 then one
  else if q = 1 then a
  else if is_limit a then exp_limit_nat a q_ord
  else let c = limitpart a in
       let n = ord_length a in
       padd (firstn (exp_limit_nat c (Nat q)) n)
            (exp_helper c (natpart a) n (q - 1)) n

let rec exp_ord_ord a b =
  mul (monomial (mul (fe a) (limitpart b), 1)) (exp_ord_nat a (natpart b))

let rec exp a b = match (a, b) with
  | (_, Nat 0) | (Nat 1, _) -> one
  | (Nat 0, _)              -> zero
  | (Nat a, Nat b)          -> Nat (exp_nat a b)
  | (Nat _, _)              -> exp_nat_ord a b
  | (_, Nat _)              -> exp_ord_nat a b
  | _                       -> exp_ord_ord a b
