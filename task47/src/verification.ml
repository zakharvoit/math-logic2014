open Util
open Axioms
open Arithmetic

module H = Hashtbl

type annotation = ByAxiom of int
                | ByModusPonens of int * int
                | ByRule1 of int
                | ByRule2 of int
                | ByAssumption of int

let string_of_annotation =
  (fun s -> "(" ^ s ^ ")")
  |> function
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

exception NotFound of int
exception NotFree of int * term * expression * string
exception FreeIn of int * string * expression

let free_vars_in_expr =
  let rec free_vars_in_all bound free = function
    | [] -> free
    | (x :: xs) -> free_vars_in_all bound (free_vars_in_term bound free x) xs
  and free_vars_in_term bound free = function
    | Plus (a, b)
    | Mul (a, b) -> free_vars_in_term bound (free_vars_in_term bound free a) b
    | Succ a -> free_vars_in_term bound free a
    | Zero -> free
    | Var a -> if List.mem a bound
               then free
               else a :: free
    | Function (f, args)
      -> free_vars_in_all bound free args
  in

  let rec free_vars' bound (free : string list) = function
    | And (a, b)
    | Or (a, b)
    | Impl (a, b) -> free_vars' bound (free_vars' bound free a) b
    | Not a -> free_vars' bound free a
    | Forall (x, e)
    | Exists (x, e) -> free_vars' (x :: bound) free e
    | PVar _ -> free
    | Predicate (s, args) -> free_vars_in_all bound free args

  in free_vars' [] []

let free_vars = 
  let rec free_vars_in_term bound free = function
    | Plus (a, b)
    | Mul (a, b) -> free_vars_in_term bound (free_vars_in_term bound free a) b
    | Succ a -> free_vars_in_term bound free a
    | Zero -> free
    | Var a -> if List.mem a bound
               then free
               else a :: free
    | Function (s, args)
      -> Util.sort_uniq compare (List.concat 
          (List.map (free_vars_in_term bound free) args))
   in free_vars_in_term [] []

(* Can be x substituted with a in b? *)
let free_for_substitution a x b =
  let free = free_vars a in

  let rec free_in_term bound = function
    | Plus (a, b)
    | Mul (a, b) -> free_in_term bound a && free_in_term bound b
    | Succ a -> free_in_term bound a
    | Zero -> true
    | Var a when a = x
              && not (List.mem a bound)
              -> List.fold_left
                            (&&)
                            true
                            (List.map (fun x -> not (List.mem x bound)) free)
    | Var a -> true
    | Function (s, args)
      -> List.fold_left (&&) true (List.map (free_in_term bound) args)
  in

  let rec free_for_substitution' bound = function
    | And (a, b)
    | Or (a, b)
    | Impl (a, b) -> free_for_substitution' bound a && free_for_substitution' bound a
    | Not a -> free_for_substitution' bound a
    | Forall (x, e)
    | Exists (x, e) -> free_for_substitution' (x :: bound) e
    | PVar _ -> true
    | Predicate (s, args) -> List.fold_left (&&) true (List.map (free_in_term bound) args)

  in free_for_substitution' [] b

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
    | (Function (s, a), Function (s', a')) when s = s'
      -> List.fold_left (&&) true (List.map2 (term_matches) a a')
    | (_, _) -> false
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
  | Function (name, args)
    -> Function (name, List.map (substitute_term x s) args)

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
let get_substitution x a b =
  let substitution = ref None in

  let rec substituted_term a b x_free =
    match (a, b) with
    | (Var y, b') when y = x && x_free
      -> begin match !substitution with
         | Some s -> s = b'
         | None -> begin
             substitution := Some b';
             true
           end
         end
    | (Var y, Var z)
      -> y = z
    | (Plus (a1, a2), Plus (b1, b2))
    | (Mul (a1, a2), Mul (b1, b2))
      -> substituted_term a1 b1 x_free && substituted_term a2 b2 x_free
    | (Succ a', Succ b')
      -> substituted_term a' b' x_free
    | (Zero, Zero)
      -> true
    | (Function (f1, p1), Function (f2, p2)) when f1 = f2
      -> all_substituted p1 p2 x_free
    | _
      -> false
  and all_substituted a b x_free =
    List.length a = List.length b
    && List.fold_left (&&) true
                      (List.map2 (fun x y -> substituted_term x y x_free) a b)
  in

  let rec substituted' a b x_free =
    match (a, b) with
    | (Impl (a1, a2), Impl (b1, b2))
    | (Or (a1, a2), Or (b1, b2))
    | (And (a1, a2), And (b1, b2))
      -> (substituted' a1 b1 x_free) && (substituted' a2 b2 x_free)
    | (Forall (y, a'), Forall (z, b'))
    | (Exists (y, a'), Exists (z, b')) when (not x_free || (y <> x))
                                            && (y = z)
      -> substituted' a' b' x_free
    | (Not a', Not b')
      -> substituted' a' b' x_free
    | (Forall (y, a'), Forall (z, b'))
    | (Exists (y, a'), Exists (z, b')) when (x_free && y = x && z = x)
      -> substituted' a' b' false
    | (PVar a', PVar b')
      -> a' = b'
    | (Predicate (ap, a'), Predicate (bp, b'))
      -> all_substituted a' b' x_free
    | _
      -> false
  in
  if substituted' a b true then
    match !substitution with
    | Some s -> Some s
    | None -> Some Zero
  else
    None

let substituted x a b =
  match get_substitution x a b with
  | Some _ -> true
  | None -> false

let verify assumpts proof =
  let right = H.create 1024 in
  let answer = H.create 1024 in
  let proved = H.create 1024 in
  let annotations = Array.make (Array.length proof) (ByAxiom 0) in

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
    let check_for_free x a b =
      match get_substitution x a b with
      | Some s -> free_for_substitution s x a
      | None -> false
    in

    match e with
    | Impl (Forall (x, a), b) when check_for_free x a b
      -> raise (AxiomFound 10)
    | Impl (b, Exists (x, a)) when check_for_free x a b
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
    | Impl (a, Forall (x, b)) when not (List.mem x (free_vars_in_expr a))
                                   && H.mem proved (Impl (a, b))
      -> raise (Rule1Found (H.find proved (Impl (a, b))))
    | _ -> ()
  in

  let check_predicate_rule2 = function
    | Impl (Exists (x, a), b) when not (List.mem x (free_vars_in_expr b))
                                   && H.mem proved (Impl (a, b))
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

  let check_if_may_be_axiom i =
    let check_substitution x a b = 
      match get_substitution x a b with
      | Some s -> raise (NotFree (i, s, a, x))
      | None -> failwith "Expected substitution"
    in function
    | Impl (Forall (x, a), b) when substituted x a b
      -> check_substitution x a b
    | Impl (b, Exists (x, a)) when substituted x a b
      -> check_substitution x a b
    | _ -> ()
  in

  let check_if_may_be_rule i e =
    let check_predicate_rule1' = function
      | Impl (a, Forall (x, b)) when H.mem proved (Impl (a, b))
        -> raise (FreeIn (i, x, a))
      | _ -> ()
    in

    let check_predicate_rule2' = function
      | Impl (Exists (x, a), b) when H.mem proved (Impl (a, b))
        -> raise (FreeIn (i, x, b))
      | _ -> ()
    in
    check_predicate_rule1' e; check_predicate_rule2' e
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

        check_if_may_be_axiom i e;
        check_if_may_be_rule i e;
        raise (NotFound i)
      with
      | AxiomFound a            -> annotations.(i) <- ByAxiom a
      | AssumptionFound a       -> annotations.(i) <- ByAssumption a
      | ModusPonensFound (a, b) -> annotations.(i) <- ByModusPonens (a, b)
      | Rule1Found a            -> annotations.(i) <- ByRule1 a
      | Rule2Found a            -> annotations.(i) <- ByRule2 a
      | NotFound a              -> begin for j = 0 to a do
					   print_int (j + 1);
					   print_string "    ";
					   print_string (string_of_expression proof.(j));
					   print_string "    ";
					   print_endline (string_of_annotation annotations.(j));
					 done;
					 print_endline ("Error at line " ^ string_of_int (a + 1));
					 failwith "Ended execution"
				   end
      | FreeIn (j, x, a) -> begin
	  for i = j - 50 to j do
	    print_endline ("(" ^ string_of_int i ^ ")    " ^ string_of_expression proof.(i)
			  ^ "     " ^ string_of_annotation annotations.(i));
	  done;
	  print_endline ("FreeIn: " ^ string_of_int j ^ " for variable " ^ x ^ " and expression " ^ string_of_expression a);
	  failwith "Ended execution"
	end
      (* Don't catch other exceptions, delegate its to the main function. *)
    end;
    update_modus_ponens e i;
  in

  for i = 0 to Array.length proof - 1 do
    add i
  done;
  annotations
