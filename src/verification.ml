open Util
open Axioms
open Arithmetic

module H = Hashtbl

type annotation = ByAxiom of int
                | ByModusPonens of int * int
                | ByRule1 of int
                | ByRule2 of int
                | ByAssumption of int
                | NotProved

let string_of_annotation =
  (fun s -> "(" ^ s ^ ")")
  |> function
    | NotProved            -> "Не доказано"
    | ByAxiom a            -> "Сх. акс. " ^ string_of_int (a + 1)
    | ByAssumption a       -> "Предположение " ^ string_of_int (a + 1)
    | ByModusPonens (a, b) -> "M.P. "
                              ^ string_of_int (a + 1) ^ ", "
                              ^ string_of_int (b + 1)
    | ByRule1 a            -> "Rule 1"
                                ^ string_of_int a
    | ByRule2 a            -> "Rule 2"
                                ^ string_of_int a
 
exception AxiomFound of int
exception AssumptionFound of int
exception Rule1Found of int
exception Rule2Found of int
exception ModusPonensFound of int * int
exception NotFound

let matches a b =
  let context = H.create 16 in
  let term_context = H.create 16 in

  let term_var_matches a b =
    if H.mem term_context b then
      H.find term_context b = a
    else begin
        H.add term_context b a;
        true
      end
  in

  let var_matches a b =
    if H.mem context b then
      H.find context b = a
    else begin
        H.add context b a;
          true
        end
  in

  let rec term_matches a b = match (a, b) with
    | (Plus (a1, a2), Plus (b1, b2))
    | (Mul (a1, a2), Mul (b1, b2))
      -> term_matches a1 b1 && term_matches a2 b2
    | (Var a', b')
      -> term_var_matches a' b'
    | (Succ a', Succ b')
      -> term_matches a' b'
    | (Zero, Zero)
      -> true
    | (_, _)
      -> false
  in
      
  let rec matches' a b = match (a, b) with
    | (Impl (a1, a2), Impl (b1, b2))
    | (Or (a1, a2), Or (b1, b2))
    | (And (a1, a2), And (b1, b2)) -> matches' a1 b1 && matches' a2 b2
    | (Not a', Not b')             -> matches' a' b'
    | (a', PVar name)              -> var_matches a' name
    | (Forall (x, a'), Forall (y, b'))
    | (Exists (x, a'), Exists (y, b')) when x = y
      -> matches' a' b'
    | (Predicate (p, a'), Predicate (q, b')) when p = q
      -> List.fold_left (&&) true (List.map2 term_matches a' b')
    | (_, _)                       -> false
  in

  matches' a b

let rec substitute_term x s = function
  | Plus (a, b) -> Plus (substitute_term x s a, substitute_term x s b)
  | Mul (a, b)  -> Mul (substitute_term x s a, substitute_term x s b)
  | Succ a -> Succ (substitute_term x s a)
  | Zero  -> Zero
  | Var y -> if y = x
             then s
             else Var y

let rec substitute x s = function
  | Forall (v, e) ->
     if x <> v
     then Forall (v, (substitute x s e))
     else Forall (v, e)
  | Exists (v, e) ->
     if x <> v
     then Exists (v, (substitute x s e))
     else Exists (v, e)
  | Predicate (p, args) ->
     Predicate (p, List.map (substitute_term x s) args)
  | Impl (a, b) -> Impl (substitute x s a, substitute x s b)
  | Or (a, b) -> Or (substitute x s a, substitute x s b)
  | And (a, b) -> And (substitute x s a, substitute x s b)
  | Not a -> Not (substitute x s a)
  | other -> other

(* Is x from a always substituted in b by the same subexpression? *)
let substituted x a b =
  let substitution = ref Zero in
  let found = ref false in

  let rec substituted_term a b =
    match (a, b) with
    | (Var y, b') when y = x
      -> if !found
         then !substitution = b'
         else begin
             found := true;
             substitution := b';
             true
           end
    | (Var y, Var z)
      -> y = z
    | (Plus (a1, a2), Plus (b1, b2))
    | (Mul (a1, a2), Plus (b1, b2))
      -> substituted_term a1 b1 && substituted_term b1 b2
    | (Succ a', Succ b')
      -> substituted_term a' b'
    | (Zero, Zero)
      -> true
    | _
      -> false
  in

  let all_substituted a b =
    List.length a = List.length b
    && List.fold_left (&&) true
                      (List.map2 substituted_term a b)
  in

  let rec substituted' a b =
    match (a, b) with
    | (Impl (a1, a2), Impl (b1, b2))
    | (Or (a1, a2), Or (b1, b2))
    | (And (a1, a2), And (b1, b2))
      -> (substituted' a1 b1) && (substituted' a2 b2)
    | (Forall (y, a'), Forall (z, b'))
    | (Exists (y, a'), Exists (z, b')) when (y <> x) && (y <> z)
      -> substituted' a' b'
    | (Not a', Not b')
      -> substituted' a' b'
    | (Forall (y, a'), _)
    | (Exists (y, a'), _) when y = x
      -> true
    | (PVar a', PVar b')
      -> a' = b'
    | (Predicate (ap, a'), Predicate (bp, b'))
      -> all_substituted a' b'
    | _
      -> false
  in
  substituted' a b
            
let verify assumpts proof =
  let right = H.create 1024 in
  let answer = H.create 1024 in
  let proved = H.create 1024 in
  let annotations = Array.make (Array.length proof) NotProved in
  
  let check_list l p f =
    for i = 0 to Array.length l - 1 do
      if p l.(i) then begin
          f i
        end
    done
  in

  let check_axiom e = check_list classical_axioms
                                 (matches e)
                                 (fun i -> raise (AxiomFound i))
  in

  let check_formal_axiom e = check_list formal_axioms
                                        (matches e)
                                        (fun i -> raise (AxiomFound (12 + i)))
  in

  let check_induction e =
    match e with
    | Impl (And (a, Forall (x, Impl (b, c))), d) when b = d
                                                      && substitute x Zero d = a
                                                      && substitute x (Succ (Var x)) d = c
      -> raise (AxiomFound 20)
    | _
      -> ()
  in

  let check_predicate_axiom e =
    match e with
    | Impl (Forall (x, a), b) when substituted x a b
      -> raise (AxiomFound 10)
    | Impl (b, Exists (x, a)) when substituted x a b
      -> raise (AxiomFound 11)
    | _ -> ()
  in

  let check_assumption e = check_list assumpts
                                      ((=) e)
                                      (fun i -> raise (AssumptionFound i))
  in

  let check_modus_ponens e =
    if not (H.mem answer e) then ()
    else
      let (a, b) = H.find answer e in
      raise (ModusPonensFound (a, b))
  in

  let check_predicate_rule1 = function
    | Impl (a, Forall (x, b)) when H.mem proved (Impl (a, b))
      -> raise (Rule1Found (H.find proved (Impl (a, b))))
    | _ -> ()
  in

  let check_predicate_rule2 = function
    | Impl (Exists (x, a), b) when H.mem proved (Impl (a, b))
      -> raise (Rule2Found (H.find proved (Impl (a, b))))
    | _ -> ()
  in

  let update_modus_ponens e pos =
    if H.mem right e
    then begin
        List.iter (fun i ->
                   let a = proof.(i) in
                   match a with
                   | Impl (l, r) -> H.replace answer r (pos, i)
                   | _             -> failwith "Impl expected"
                  ) (H.find right e);
        H.replace right e []
      end;
    begin match e with
          | Impl (l, r) -> if H.mem proved l
                             then H.replace answer r (H.find proved l, pos)
                             else if H.mem right l
                             then H.replace right l (pos :: H.find right l)
                             else H.add right l [pos]
          | _             -> ()
    end;
    H.replace proved e pos
  in

  let add i =
    let e = proof.(i) in
    begin
      try
        check_axiom e;
        check_predicate_axiom e;
        check_formal_axiom e;
        check_induction e;
        check_assumption e;
        check_modus_ponens e;
        check_predicate_rule1 e;
        check_predicate_rule2 e;
      with
      | AxiomFound a            -> annotations.(i) <- ByAxiom a
      | AssumptionFound a       -> annotations.(i) <- ByAssumption a
      | ModusPonensFound (a, b) -> annotations.(i) <- ByModusPonens (a, b)
      | Rule1Found a            -> annotations.(i) <- ByRule1 a
      | Rule2Found a            -> annotations.(i) <- ByRule2 a
      | AssumptionFound a       -> annotations.(i) <- ByAssumption a
      | NotFound                -> annotations.(i) <- NotProved
    end;
    update_modus_ponens e i
  in

  for i = 0 to Array.length proof - 1 do
    add i
  done;
  annotations
