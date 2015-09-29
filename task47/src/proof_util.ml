open Arithmetic
open Deduction
open Verification

module H = Hashtbl

let rec repr = function
  | 0 -> Zero
  | x -> Succ (repr (x - 1))

let desucc = function
  | Succ x -> x
  | _ -> failwith "desucc: not succ"

let (%&&) a b = And (a, b)
let (+||) a b = Or (a, b)
let (^->) a b = Impl (a, b)
let (^^-->) a p =
  let p_a = Array.of_list p in
  let annot = verify (Array.of_list a) p_a in
  let a' = List.rev a in
  Array.to_list (deduce p_a annot (Array.of_list (List.tl a')) (List.hd a'))
let (!!!) a = Not a
let (!@@) a = fun x -> Forall (a, x)
let (!??) a = fun x -> Exists (a, x)
let (@===) a b = Predicate ("=", [a ; b])

let (+++) a b = Plus (a, b)
let (!++) a = Succ a
let (%**) a b = Mul (a, b)

let remove_repeats l =
  let table = H.create 100000 in
  let rec fold acc = function
    | [] -> acc
    | x :: xs -> if H.mem table x then fold acc xs
		 else begin H.add table x (); fold (x :: acc) xs end
  in List.rev (fold [] l)
	    
let concat a b = a ^ b
		  
    
