open Util

module E = Expression
module A = Axioms
module H = Hashtbl

let (=~~) = E.(=~~)

exception AxiomFound of int
exception AssumptionFound of int
exception ModusPonensFound of int * int
exception NotFound

type annotation = NotProved
                | ByAxiom of int
                | ByAssumption of int
                | ByModusPonens of int * int

let string_of_annotation = (fun s -> "(" ^ s ^ ")")
                         |> function
  | NotProved            -> "Не доказано"
  | ByAxiom a            -> "Сх. акс. " ^ string_of_int (a + 1)
  | ByAssumption a       -> "Предположение " ^ string_of_int (a + 1)
  | ByModusPonens (a, b) -> "M.P. "
                            ^ string_of_int (a + 1) ^ ", "
                            ^ string_of_int (b + 1)

class verifier (asserts, assumptions) =
object (self)
  val assertions         = (asserts : E.expression array)
  val annotations        = Array.make (Array.length asserts) NotProved
  val assumptions        = assumptions
  val right              = H.create 1024
  val answer             = H.create 1024
  val proved             = H.create 1024
                                    

  method annotations = annotations

  method check_list e cmp result axioms =
    for i = 0 to Array.length axioms - 1 do
      if cmp e axioms.(i) then begin
          raise (result i)
        end
    done

  method check_assumptions e = self#check_list
                                 e
                                 (=)
                                 (fun a -> AssumptionFound a)
                                 assumptions

  method check_axioms e = self#check_list
                            e
                            (=~~)
                            (fun a -> AxiomFound a)
                            A.axioms

  method check_modus_ponens e =
    if not (H.mem answer e) then raise NotFound
    else
      let (a, b) = H.find answer e in
      raise (ModusPonensFound (a, b))

  method update_modus_ponens e pos =
    if H.mem right e
    then begin
      List.iter (fun i ->
                 let a = assertions.(i) in
                 match a with
                 | Impl (l, r) -> H.replace answer r (pos, i)
                 | _           -> failwith "Impl expected"
                ) (H.find right e);
      H.replace right e []
      end;
    begin match e with
          | E.Impl (l, r) -> if H.mem proved l
                             then H.replace answer r (H.find proved l, pos)
                             else if H.mem right l
                             then H.replace right l (pos :: H.find right l)
                             else H.add right l [pos]
          | _           -> ()
    end;
    H.replace proved e pos
                     
  method add pos =
    let e = assertions.(pos) in
    
    begin
      try 
        self#check_axioms e;
        self#check_assumptions e;
        self#check_modus_ponens e
      with
      | AxiomFound a            -> annotations.(pos) <- ByAxiom a
      | AssumptionFound a       -> annotations.(pos) <- ByAssumption a
      | ModusPonensFound (a, b) -> annotations.(pos) <- ByModusPonens (a, b)
      | NotFound                -> annotations.(pos) <- NotProved
    end;
    self#update_modus_ponens e pos

  method add_all =
    for i = 0 to Array.length assertions - 1 do
      self#add i
    done
end

let verify_with_assumptions assertions assumptions =
  let v = new verifier(assertions, assumptions) in
  v#add_all;
  v#annotations

let verify assertions = verify_with_assumptions assertions [| |]

