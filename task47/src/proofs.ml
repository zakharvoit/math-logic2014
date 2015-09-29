open Util
open Proof_util
open Arithmetic
open Axioms

let substitute_var x s e = Verification.substitute x s e

let truth = PVar "A"   ^->   PVar "A"   ^->   PVar "A"

(* let debug_stmt mess = let name = concat mess "DBG" in  [ PVar name   ^->   PVar name   ^->   PVar name ] *)
let debug_stmt _ = []

(* a     +++  ||  - @@@x a *)
let add_forall_quantifier x a =
  [ truth
  ; a   ^->   truth   ^->   a
  ; truth   ^->   a
  ; truth   ^->     !@@  x a
  ;   !@@  x a
  ]

(* @@@x a     +++  ||  - a[x := t] *)
let remove_forall_quantifier x a t =
  let result = substitute_var x t a in
  [   !@@  x a   ^->   result
  ; result
  ]

(* a     +++  ||  - a[x := t] *)
let generalize_expression a x t =
  add_forall_quantifier x a
  @@@ remove_forall_quantifier x a t

(*     +++  ||  - axiom(i)["a" := a, "b" := b, "c" := c] *)
let generate_axiom i a b c =
  let axiom = formal_axioms.(i) in
  let prove_a = generalize_expression axiom "a" a in
  let with_a = substitute_var "a" a axiom in
  let prove_b = generalize_expression with_a "b" b in
  let with_b = substitute_var "b" b with_a in
  let prove_c = generalize_expression with_b "c" c in
  axiom :: prove_a @@@ prove_b @@@ prove_c

(*     +++  ||  - a = a *)
let eq_reflexive a = debug_stmt "EQREFLEXIVE" @@@
  generate_axiom 5 a   Zero     Zero   @@@
  generate_axiom 1 (a   +++     Zero  ) a a @@@
  [ (a   +++     Zero      @===   a)   ^->   (a    @===   a)
  ; a    @===   a
  ] @@@ debug_stmt "EQREFLEXIVEEND"

(* a = b     +++  ||  - b = a *)
let eq_commutative a b = debug_stmt "EQCOMMUTATIVE" @@@
  (* if a =   Zero   then if b =   !++    Zero   then failwith "OLOLOLO" else () else (); *)
  eq_reflexive a @@@
  generate_axiom 1 a b a @@@
  [ a    @===   b
  ; (a    @===   a)   ^->   (b    @===   a)
  ; b    @===   a
  ] @@@ debug_stmt "EQCOMMUTATIVEEND"

(* a = b, b = c     +++  ||  - a = c *)
let eq_transitive a b c = debug_stmt "EQTRANSITIVE" @@@
  generate_axiom 1 b a c @@@
  eq_commutative a b @@@
  [ (b    @===   c)   ^->   (a    @===   c)
  ; a    @===   c
  ] @@@ debug_stmt "EQTRANSITIVEEND"

(* a = b     +++  ||  - a   +++     Zero   = b   +++     Zero   *)
let add_zero a b =
  generate_axiom 5 a   Zero     Zero   @@@
  generate_axiom 5 b   Zero     Zero   @@@
  eq_commutative (b   +++     Zero  ) b @@@
  eq_transitive (a   +++     Zero  ) a b @@@
  eq_transitive (a   +++     Zero  ) b (b   +++     Zero  )

(* a   +++   x = b   +++   x     +++  ||  - a   +++   x' = b   +++   x' *)
let add_one a b x =
  generate_axiom (1 - 1) (a   +++   x) (b   +++   x)   Zero   @@@
  generate_axiom 4 a x   Zero   @@@
  generate_axiom 4 b x   Zero   @@@
  [   !++  (a   +++   x)    @===     !++  (b   +++   x) ] @@@
  eq_transitive (a   +++     !++  x)   !++  (a   +++   x)   !++  (b   +++   x) @@@
  eq_commutative (b   +++     !++  x)   !++  (b   +++   x) @@@
  eq_transitive (a   +++     !++  x)   !++  (b   +++   x) (b   +++     !++  x)

(* a   +++   b = c     +++  ||  - a   +++   b' = c' *)
let step_sum a b c = debug_stmt "STEPSUM" @@@
  generate_axiom (1 - 1) (a   +++   b) c Zero @@@
  generate_axiom 4 a b Zero @@@
  [   !++  (a   +++   b)    @===     !++  c ] @@@
  eq_transitive (a   +++     !++  b)   !++  (a   +++   b)   !++  c @@@ debug_stmt "STEPSUMEND"

(* a   %**   b = [a   %**   b],
   a   %**   b   +++   a = [a   %**   b]   +++   a,
   [a   %**   b]   +++   a = [a   %**   b   +++   a]
       +++  ||  - a   %**   b' = [a   %**   b   +++   a], where c = [a   %**   b], d = [c   +++   a] *)
let step_mul a b c d = debug_stmt "STEPMUL" @@@
  generate_axiom 7 a b   Zero   @@@
  eq_transitive (a   %**     !++  b) (a   %**   b   +++   a) (c   +++   a) @@@
  eq_transitive (a   %**     !++  b) (c   +++   a) d  @@@ debug_stmt "STEPMULEND"

(*   !!!  (a    @===   b)     +++  ||  -   !!!  (b    @===   a) *)
let eq_commutative_under_not a b =
  ([  !!!  (a    @===   b); b    @===   a]   ^^-->   eq_commutative b a @@@ [   !!!  (a    @===   b) ]) @@@
  [ ((b    @===   a)   ^->   (a    @===   b))   ^->   ((b    @===   a)   ^->     !!!  (a    @===   b))   ^->     !!!  (b    @===   a)
  ; ((b    @===   a)   ^->     !!!  (a    @===   b))   ^->     !!!  (b    @===   a)
  ;   !!!  (b    @===   a)
  ]

(* a = b,   !!!  (b = c)     +++  ||  -   !!!  (a = c) *)
let lemma_not_equals a b c =
  debug_stmt "LEMMANEBEGIN" @@@
  ([ a    @===   b;   !!!  (b    @===   c); a    @===   c ]   ^^-->  
  [ a    @===   b
  ; a    @===   c
  ;   !!!  (b    @===   c)
  ] @@@
  eq_commutative a b @@@
  eq_transitive b a c
  ) @@@
  [ ((a    @===   c)   ^->   (b    @===   c))   ^->   ((a    @===   c)   ^->     !!!  (b    @===   c))   ^->     !!!  (a    @===   c)
  ; ((a    @===   c)   ^->     !!!  (b    @===   c))   ^->     !!!  (a    @===   c)
  ;   !!!  (a    @===   c)
  ]

(*     +++  ||  -   Zero     %**   a =   Zero   *)
let rec mul_left a =
  if a =   Zero   then generate_axiom 6 a   Zero     Zero  
  else let b = desucc a in
       mul_left b @@@
       generate_axiom 7   Zero   b   Zero   @@@
       generate_axiom 5 (  Zero     %**   b)   Zero     Zero   @@@
       eq_transitive (  Zero     %**   a) (  Zero     %**   b   +++     Zero  ) (  Zero     %**   b) @@@
       eq_transitive (  Zero     %**   a) (  Zero     %**   b)   Zero  

(*     +++  ||  -   !!!  (  Zero     %**   a = b') *)
let base_division a b = debug_stmt "BASEDIVISION" @@@
  mul_left a @@@
  generate_axiom 3 b   Zero     Zero   @@@
  eq_commutative   !++  b   Zero   @@@
  lemma_not_equals (  Zero     %**   a)   Zero     !++  b @@@ debug_stmt "BASEDIVISIONEND"

(*     +++  ||  -   Zero     +++   a = a *)
let rec add_zero_left a = debug_stmt "ADDZEROLEFT" @@@ (
  if a =   Zero   then generate_axiom 5 a   Zero     Zero  
  else let a' = desucc a in
       add_zero_left a' @@@
       eq_commutative (  Zero     +++   a') a' @@@
       generate_axiom (1-1) a' (  Zero     +++   a')   Zero   @@@
       [   !++  a'    @===     !++  (  Zero     +++   a') ] @@@
       generate_axiom 4   Zero   a'   Zero   @@@
       eq_commutative (  Zero     +++     !++  a')   !++  (  Zero     +++   a') @@@
       eq_transitive   !++  a'   !++  (  Zero     +++   a') (  Zero     +++     !++  a') @@@
       eq_commutative   !++  a' (  Zero     +++     !++  a')) @@@ debug_stmt "ADDZEROLEFTEND"

(*     +++  ||  - a'   +++   b = (a   +++   b)' *)
let rec axiom_4_reversed a b = debug_stmt "AXIOM4REVERSED" @@@ (
  if b =   Zero   then generate_axiom 5   !++  a   Zero     Zero   @@@
		generate_axiom 5 a   Zero     Zero   @@@
		generate_axiom (1-1) (a   +++     Zero  ) a   Zero   @@@
		[   !++  (a   +++     Zero  )    @===     !++  a ] @@@
		eq_commutative   !++  (a   +++     Zero  )   !++  a @@@
		eq_transitive (  !++  a   +++     Zero  )   !++  a   !++  (a   +++     Zero  )
  else let b' = desucc b in
       axiom_4_reversed a b' @@@
       generate_axiom 4   !++  a b'   Zero   @@@
       generate_axiom (1-1) (  !++  a   +++   b')   !++  (a   +++   b')   Zero   @@@
       [   !++  (  !++  a   +++   b')    @===     !++  (  !++  (a   +++   b')) ] @@@
       eq_transitive (  !++  a   +++   b)   !++  (  !++  a   +++   b')   !++  (  !++  (a   +++   b')) @@@
       generate_axiom 4 a b'   Zero   @@@
       generate_axiom (1-1) (a   +++   b)   !++  (a   +++   b')   Zero   @@@
       [   !++  (a   +++   b)    @===     !++  (  !++  (a   +++   b')) ] @@@
       eq_commutative   !++  (a   +++   b)   !++  (  !++  (a   +++   b')) @@@
       eq_transitive (  !++  a   +++   b)   !++  (  !++  (a   +++   b'))   !++  (a   +++   b) ) @@@ debug_stmt "AXIOM4REVERSEDEND"

(*     +++  ||  - a   +++   b = b   +++   a, where a and b both are constants *)
let rec add_commutative a b = debug_stmt "ADDCOMMUTATIVE" @@@ (
  if a =   Zero   then add_zero_left b
		@@@ generate_axiom 5 b   Zero     Zero  
		@@@ eq_commutative (b   +++     Zero  ) b
		@@@ eq_transitive (  Zero     +++   b) b (b   +++     Zero  )
  else let b' = desucc b in
       add_commutative a b' @@@
       generate_axiom 4 a b'   Zero   @@@
       axiom_4_reversed b' a @@@
       generate_axiom (1-1) (a   +++   b') (b'   +++   a)   Zero   @@@
       eq_transitive (a   +++   b)   !++  (a   +++   b')   !++  (b'   +++   a) @@@
       eq_commutative (b   +++   a)   !++  (b'   +++   a) @@@
       eq_transitive (a   +++   b)   !++  (b'   +++   a) (b   +++   a) ) @@@ debug_stmt "ADDCOMMUTATIVEEND"

(* a   +++     Zero   = b   +++     Zero       +++  ||  - a = b *)
let subtract_zero a b =
  generate_axiom 5 a   Zero     Zero   @@@
  generate_axiom 5 b   Zero     Zero   @@@
  eq_commutative (a   +++     Zero  ) a @@@
  eq_transitive a (a   +++     Zero  ) (b   +++     Zero  ) @@@
  eq_transitive a (b   +++     Zero  ) b

(* a   +++   [c] = b   +++   [c]     +++  ||  - a = b*)
let rec subtract_equal a b c' = debug_stmt "SUBTRACTEQUAL" @@@ (
  if c' = (1-1) then subtract_zero a b
  else let c = repr (c' - 1) in
       generate_axiom 4 a c   Zero   @@@
       generate_axiom 4 b c   Zero   @@@
       eq_transitive (a   +++     !++  c) (b   +++     !++  c)   !++  (b   +++   c) @@@
       eq_commutative (a   +++     !++  c)   !++  (a   +++   c) @@@
       eq_transitive   !++  (a   +++   c) (a   +++     !++  c)   !++  (b   +++   c) @@@
       generate_axiom 2 (a   +++   c) (b   +++   c)   Zero   @@@
       [ a   +++   c    @===   b   +++   c ] @@@
       subtract_equal a b (c' - 1) ) @@@ debug_stmt "SUBTRACTEQUALEND"

(* a_ <= b_     +++  ||  - [b_] = [b_ - a_]   +++   [a_] *)
let rec subtract_const b_ a_ = debug_stmt "SUBTRACTCONST" @@@ (
  let b = repr b_ in
  let a = repr a_ in
  if a_ = (1-1) then generate_axiom 5 b   Zero     Zero   @@@ eq_commutative (b   +++     Zero  ) b
  else let c_ = a_ - 1 in
       let c = repr c_ in
       let b_c = repr (b_ - c_) in
       let b_a = repr (b_ - a_) in
       subtract_const b_ c_ @@@
       axiom_4_reversed b_a c @@@
       eq_transitive b (b_c   +++   c)   !++  (b_a   +++   c) @@@
       generate_axiom 4 b_a c   Zero   @@@
       eq_commutative (b_a   +++   a)   !++  (b_a   +++   c) @@@
       eq_transitive b   !++  (b_a   +++   c) (b_a   +++   a) ) @@@ debug_stmt "SUBTRACTCONSTEND"

(* (a   ^->   b),   !!!  b     +++  ||  -   !!!  a *)
let contraposition a b =
  [ (a   ^->   b)   ^->   (a   ^->     !!!  b)   ^->     !!!  a
  ; (a   ^->   b)
  ; (a   ^->     !!!  b)   ^->     !!!  a
  ;   !!!  b   ^->   a   ^->     !!!  b
  ;   !!!  b
  ; a   ^->     !!!  b
  ;   !!!  a
  ]
    
(* a, b     +++  ||  - a   %&&   b *)
let and_when_both a b =
  [ a   ^->   b   ^->   (a   %&&   b)
  ; b   ^->   (a   %&&   b)
  ; a   %&&   b
  ]

(*     +++  ||  -   !!!  (x   +++   [a] = [b]) *)
let rec not_divides_lemma_gt x _a _b =
  let a = repr _a in let b = repr _b in
  if _b = (1-1) then
    let d = desucc a in
    generate_axiom 4 x d   Zero   @@@
    generate_axiom 3 (x   +++   d)   Zero     Zero   @@@
    lemma_not_equals (x   +++     !++  d)   !++  (x   +++   d)   Zero   @@@ debug_stmt "LEMMA1"
  else
    let _a' = _a - 1 in let a' = repr _a' in
    let _b' = _b - 1 in let b' = repr _b' in
    not_divides_lemma_gt x _a' _b' @@@
    generate_axiom 4 x a'   Zero   @@@
    generate_axiom 2 (x   +++   a') b'   Zero   @@@
    contraposition (  !++  (x   +++   a')    @===     !++  b') ((x   +++   a')    @===   b') @@@
    lemma_not_equals (x   +++   a)   !++  (x   +++   a') b @@@ debug_stmt "LEMMA2"

(* 1 <= a <= b, a mod b   !!!  =   Zero  ;     +++  ||  -   !!!  ([a]   %**   c' = [b]) *)
let rec not_divides_step_le a_ b_ c =
  let a = repr a_ in let b = repr b_ in
  let d_ = b_ - a_ in let d = repr d_ in
  ([ a   %**     !++  c    @===   b ]   ^^-->   (
  generate_axiom 7 a c   Zero   @@@
  [ a   %**     !++  c    @===   b ] @@@
  eq_commutative (a   %**     !++  c) b @@@
  eq_transitive b (a   %**     !++  c) (a   %**   c   +++   a) @@@
  subtract_const b_ a_ @@@
  eq_commutative b (a   %**   c   +++   a) @@@
  eq_transitive (a   %**   c   +++   a) b (d   +++   a) @@@
  subtract_equal (a   %**   c) d a_
   )) @@@ 
  not_divides a_ d_ c @@@
  (* We know, that a   %**     !++  c    @===   b   ^->   a   %**   c    @===   d and   !!!  (a   %**   c    @===   d), now using contraposition lemma *)
  contraposition (a   %**     !++  c    @===   b) (a   %**   c    @===   d)

(* 1 <= b < a, a mod b   !!!  =   Zero  ;     +++  ||  -   !!!  ([a]   %**   c' = [b])*)
and not_divides_step_gt _a _b c =
  let a = repr _a in let b = repr _b in
  generate_axiom 7 a c   Zero   @@@
  not_divides_lemma_gt (a   %**   c) _a _b @@@
  lemma_not_equals (a   %**     !++  c) (a   %**   c   +++   a) b @@@ debug_stmt "LEMMA3"

and not_divides_step a b c =
  if a <= b then not_divides_step_le a b c
  else not_divides_step_gt a b c

(* a   %**     Zero     !!!  = b *)
and mul_zero_right a b =
  generate_axiom 6 a   Zero     Zero   @@@
  generate_axiom 3 (desucc b)   Zero     Zero   @@@
  eq_commutative_under_not b   Zero   @@@
  lemma_not_equals (a   %**     Zero  )   Zero   b  @@@ debug_stmt "LEMMA4"

(* a mod b   !!!  =   Zero  ;     +++  ||  -   !!!  ([a]   %**   c = [b])  *)
and not_divides a_ b_ c =
  if a_ = (1-1) then base_division c (repr (b_ - 1))
  else let x = Var "x" in
       let a = repr a_ in let b = repr b_ in
       mul_zero_right a b @@@
       not_divides_step a_ b_ x @@@
       [   !!!  (a   %**     !++  x    @===   b)   ^->     !!!  (a   %**   x    @===   b)   ^->     !!!  (a   %**     !++  x    @===   b)
       ;   !!!  (a   %**   x    @===   b)   ^->     !!!  (a   %**     !++  x    @===   b)
       ; (  !!!  (a   %**   x    @===   b)   ^->     !!!  (a   %**     !++  x    @===   b))   ^->   truth   ^->   (  !!!  (a   %**   x    @===   b)   ^->     !!!  (a   %**     !++  x    @===   b))
       ; truth   ^->   (  !!!  (a   %**   x    @===   b)   ^->     !!!  (a   %**     !++  x    @===   b))
       ; truth   ^->   (  !@@   "x" (  !!!  (a   %**   x    @===   b)   ^->     !!!  (a   %**     !++  x    @===   b)))
       ; (  !@@   "x" (  !!!  (a   %**   x    @===   b)   ^->     !!!  (a   %**     !++  x    @===   b)))
       ] @@@
       and_when_both   !!!  (a   %**     Zero      @===   b) (  !@@   "x" (  !!!  (a   %**   x    @===   b)   ^->     !!!  (a   %**     !++  x    @===   b))) @@@
       [ (  !!!  (a   %**     Zero      @===   b)   %&&   (  !@@   "x" (  !!!  (a   %**   x    @===   b)   ^->     !!!  (a   %**     !++  x    @===   b))))   ^->     !!!  (a   %**   x    @===   b)
       ;   !!!  (a   %**   x    @===   b)
       ]

(*   !!!  P     +++  ||  -   !!!     !??  x P *)
let introduce_exists p =
    ( [  !!!  p; p]   ^^-->   ([ p;   !!!  p ]   ^^-->   (
        ([ p;   !!!  p;   !??   "x" p ]   ^^-->   [ p ]) @@@
        ([ p;   !!!  p;   !??   "x" p ]   ^^-->   [   !!!  p ]) @@@
        [ ((  !??   "x" p)   ^->   p)   ^->   ((  !??   "x" p)   ^->     !!!  p)   ^->   (  !!!  (  !??   "x" p))
        ; ((  !??   "x" p)   ^->     !!!  p)   ^->   (  !!!  (  !??   "x" p))
        ;   !!!  (  !??   "x" p)
        ]
        )) @@@
    [   !!!  p
    ;   !!!  (  !??   "x" p)
    ]) @@@ (* We have that   !!!  p     +++  ||  - p   ^->     !!!  (  !??   "x" p) *)
    [ (  !??   "x" p)   ^->     !!!  (  !??   "x" p) ] @@@
    ( [   !??   "x" p ]   ^^-->   [   !??   "x" p ]) @@@
    [ ((  !??   "x" p)   ^->   (  !??   "x" p))   ^->   ((  !??   "x" p)   ^->     !!!  (  !??   "x" p))   ^->     !!!  (  !??   "x" p)
    ; ((  !??   "x" p)   ^->     !!!  (  !??   "x" p))   ^->     !!!  (  !??   "x" p)
    ;   !!!  (  !??   "x" p)
    ]