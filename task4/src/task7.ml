open Util
open Arithmetic
open Proofs
open Proof_util
open Verification
open Minimizer

(* a = b |- a + [x] = b + [x]  *)
let rec add_constant a b = function
  | 0 -> add_zero a b
  | x -> add_constant a b (x - 1) @@@ add_one a b (repr (x - 1))

(* |- [a] + [b] = [a + b] *)
let rec prove_sum a = function
  | 0 -> generate_axiom 0 (repr a) Zero Zero
  | b' -> let b = b' - 1 in
	  prove_sum a b @@@
	  step_sum (repr a) (repr b) (repr (a + b))
    

(* |- [a] * [b] = [a * b] *)
let rec prove_mul a = function
  | 0 -> generate_axiom 6 (repr a) Zero Zero
  | b' -> let b = b' - 1 in
	  prove_mul a b @@@ (* [a] * [b] = [a * b] *)
	  add_constant (Mul (repr a, repr b))
		       (repr (a * b))
		       a @@@ (* [a] * [b] + [a] = [a * b] + [a] *)
          prove_sum (a * b) a @@@ (* [a * b] + [a] = [a * b + a] *)
	  step_mul (repr a) (repr b) (repr (a * b)) (repr (a * b + a))

let prove_divisibility a b = (* TODO: Add \exists *)
  prove_mul a (b / a)

let prove_needed a b = if a mod b = 0 then prove_divisibility b a
		       else not_divides b a (Var "x")

let _ = let a = read_int () in
	let b = read_int () in
	let raw_proof = prove_needed a b in
	prerr_endline "Proof generated";
	let proof = Array.of_list (remove_repeats raw_proof) in
	prerr_endline "Repeats removed";
	let annotations = verify (Array.make 0 (PVar "A")) proof in
	let proof' = minimize proof annotations in
	prerr_endline "Proof minimized";
	List.iter (print_endline |> string_of_expression) proof';
	prerr_endline "Proof printed out";
