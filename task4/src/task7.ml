open Util
open Arithmetic
open Proofs
open Proof_util

(* a = b |- a + [x] = b + [x]  *)
let rec add_constant a b = function
  | 0 -> add_zero a b
  | x -> add_constant a b (x - 1) @ add_one a b (repr (x - 1))

(* |- [a] + [b] = [a + b] *)
let rec prove_sum a = function
  | 0 -> generate_axiom 0 (repr a) Zero Zero
  | b' -> let b = b' - 1 in
	  prove_sum a b @
	  step_sum (repr a) (repr b) (repr (a + b))
    

(* |- [a] * [b] = [a * b] *)
let rec prove_mul a = function
  | 0 -> generate_axiom 6 (repr a) Zero Zero
  | b' -> let b = b' - 1 in
	  prove_mul a b @ (* [a] * [b] = [a * b] *)
	  add_constant (Mul (repr a, repr b))
		       (repr (a * b))
		       a @ (* [a] * [b] + [a] = [a * b] + [a] *)
          prove_sum (a * b) a @ (* [a * b] + [a] = [a * b + a] *)
	  step_mul (repr a) (repr b) (repr (a * b)) (repr (a * b + a))

let prove_divisibility a b = (* TODO: Add \exists *)
  prove_mul a (b / a)

let _ = let a = read_int () in
	let b = read_int () in
	if b mod a = 0 then
	  List.iter (print_endline |> string_of_expression) (remove_repeats (prove_divisibility a b))
	else
	  failwith "A should divide B"
