type ordinal = Nat of int
             | Big of ordinal * int * ordinal

(* First exponent *)
let fe = function
  | Nat _ -> Nat 0
  | Big (e, _, _) -> e

(* First coefficient *)
let fc = function
  | Nat a -> a
  | Big (_, c, _) -> c


let rest = function
  | Nat a -> failwith "Rest: Not Nat expected"
  | Big (_, _, r) -> r

let unite a b = match a with
  | (x, y) -> Big (x, y, b)
  | _ -> failwith "Unite: Pair expected"

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
                      else if cmp_e == 0 then unite (fe a, fc a + fc b) (rest b)
                      else unite (fe a, fc a) (add (rest a) b)

let rec sub a b = match (a, b) with
  | (Nat a, Nat b) -> if a <= b then Nat 0
                      else Nat (a - b)
  | _              -> let cmp_e = cmp (fe a) (fe b) in
                      let cmp_c = compare (fc a) (fc b) in
                      if cmp_e < 0 then Nat 0
                      else if cmp_e > 0 then a
                      else if cmp_c < 0 then Nat 0
                      else if cmp_c > 0 then unite (fe a, fc a - fc b) (rest a)
                      else sub (rest a) (rest b)

let rec mul a b = match (a, b) with
  | (Nat 0, _)     -> Nat 0
  | (_, Nat 0)     -> Nat 0
  | (Nat a, Nat b) -> Nat (a * b)
  | (_, Nat b)     -> unite (fe a, fc a * b) (rest a)
  | _              -> unite (add (fe a) (fe b), fc b) (mul a (rest b))

let rec exp a b = failwith "Expontiation for now is not supported"
