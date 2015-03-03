open Util
open Axioms
open Arithmetic

module H = Hashtbl

type annotation = ByAxiom of int
                | ByModusPonens of int * int
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
 
exception AxiomFound of int
exception AssumptionFound of int
exception ModusPonensFound of int * int
exception NotFound

let matches a b =
  let context = H.create 16 in

  let var_matches a b =
    if H.mem context b then
      H.find context b = a
    else begin
        H.add context b a;
          true
        end
  in

  let rec matches' a b = match (a, b) with
    | (Impl (a1, a2), Impl (b1, b2))
    | (Or (a1, a2), Or (b1, b2))
    | (And (a1, a2), And (b1, b2)) -> matches' a1 b1 && matches' a2 b2
    | (Not a', Not b')             -> matches' a' b'
    | (a', PVar name)              -> var_matches a' name
    | (_, _)                       -> false
  in

  matches' a b

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
        check_assumption e;
        check_modus_ponens e;
      with
      | AxiomFound a            -> annotations.(i) <- ByAxiom a
      | AssumptionFound a       -> annotations.(i) <- ByAssumption a
      | ModusPonensFound (a, b) -> annotations.(i) <- ByModusPonens (a, b)
      | NotFound                -> annotations.(i) <- NotProved
    end;
    update_modus_ponens e i
  in

  for i = 0 to Array.length proof - 1 do
    add i
  done;
  annotations
