open Proof_util
open Arithmetic
open Axioms

let substitute_var x s e = Verification.substitute x s e

let truth = PVar "A"   ^->   PVar "A"   ^->   PVar "A"

(* a     +++  ||  - @x a *)
let add_forall_quantifier x a =
  [ truth
  ; a   ^->   truth   ^->   a
  ; truth   ^->   a
  ; truth   ^->     !@@  x a
  ;   !@@  x a
  ]

(* @x a     +++  ||  - a[x := t] *)
let remove_forall_quantifier x a t =
  let result = substitute_var x t a in
  [   !@@  x a   ^->   result
  ; result
  ]

(* a     +++  ||  - a[x := t] *)
let generalize_expression a x t =
  add_forall_quantifier x a
  @ remove_forall_quantifier x a t

(*     +++  ||  - axiom(i)["a" := a, "b" := b, "c" := c] *)
let generate_axiom i a b c =
  let axiom = formal_axioms.(i) in
  let prove_a = generalize_expression axiom "a" a in
  let with_a = substitute_var "a" a axiom in
  let prove_b = generalize_expression with_a "b" b in
  let with_b = substitute_var "b" b with_a in
  let prove_c = generalize_expression with_b "c" c in
  axiom :: prove_a @ prove_b @ prove_c

(*     +++  ||  - a = a *)
let eq_reflexive a =
  generate_axiom 5 a   Zero     Zero   @
  generate_axiom 1 (a   +++     Zero  ) a a @
  [ (a   +++     Zero      @===   a)   ^->   (a    @===   a)
  ; a    @===   a
  ]

(* a = b     +++  ||  - b = a *)
let eq_commutative a b =
  eq_reflexive a @
  generate_axiom 1 a b a @
  [ (a    @===   a)   ^->   (b    @===   a)
  ; b    @===   a
  ]

(* a = b, b = c     +++  ||  - a = c *)
let eq_transitive a b c =
  generate_axiom 1 b a c @
  eq_commutative a b @
  [ (b    @===   c)   ^->   (a    @===   c)
  ; a    @===   c
  ]

(* a = b     +++  ||  - a   +++     Zero   = b   +++     Zero   *)
let add_zero a b =
  generate_axiom 5 a   Zero     Zero   @
  generate_axiom 5 b   Zero     Zero   @
  eq_commutative (b   +++     Zero  ) b @
  eq_transitive (a   +++     Zero  ) a b @
  eq_transitive (a   +++     Zero  ) b (b   +++     Zero  )
  
(* a   +++   x = b   +++   x     +++  ||  - a   +++   x' = b   +++   x' *)
let add_one a b x =
  generate_axiom (1 - 1) (a   +++   x) (b   +++   x)   Zero   @
  generate_axiom 4 a x   Zero   @
  generate_axiom 4 b x   Zero   @
  [   !++  (a   +++   x)    @===     !++  (b   +++   x) ] @
  eq_transitive (a   +++     !++  x)   !++  (a   +++   x)   !++  (b   +++   x) @
  eq_commutative (b   +++     !++  x)   !++  (b   +++   x) @
  eq_transitive (a   +++     !++  x)   !++  (b   +++   x) (b   +++     !++  x)

(* a   +++   b = c     +++  ||  - a   +++   b' = c' *)
let step_sum a b c =
  generate_axiom (1 - 1) (a   +++   b) c Zero @
  generate_axiom 4 a b Zero @
  [   !++  (a   +++   b)    @===     !++  c ] @
  eq_transitive (a   +++     !++  b)   !++  (a   +++   b)   !++  c

(* a   %**   b = [a   %**   b],
   a   %**   b   +++   a = [a   %**   b]   +++   a,
   [a   %**   b]   +++   a = [a   %**   b   +++   a]
       +++  ||  - a   %**   b' = [a   %**   b   +++   a], where c = [a   %**   b], d = [c   +++   a] *)
let step_mul a b c d =
  generate_axiom 7 a b   Zero   @
  eq_transitive (a   %**     !++  b) (a   %**   b   +++   a) (c   +++   a) @
  eq_transitive (a   %**     !++  b) (c   +++   a) d
