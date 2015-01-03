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

  method annotations = annotations

  method check_list e result axioms =
    for i = 0 to Array.length axioms - 1 do
      if e =~~ axioms.(i) then begin
          raise (result i)
        end
    done

  method check_assumptions e = self#check_list
                                 e
                                 (fun a -> AssumptionFound a)
                                 assumptions
  method check_axioms e = self#check_list
                            e
                            (fun a -> AxiomFound a)
                            A.axioms

  method check_modus_ponens e pos =
    let right_hand = H.create 1024 in
    for i = 0 to pos - 1 do
      match assertions.(i) with
      | E.Impl (left, right)
           when right = e -> H.replace right_hand left i
      | _                 -> ()
    done;
    for i = 0 to pos - 1 do
      if H.mem right_hand assertions.(i) then
        raise (ModusPonensFound (i, H.find right_hand assertions.(i)))
    done;
    raise NotFound

  method add pos =
    let e = assertions.(pos) in
    try 
      self#check_axioms e;
      self#check_assumptions e;
      self#check_modus_ponens e pos
    with
    | AxiomFound a            -> annotations.(pos) <- ByAxiom a
    | AssumptionFound a       -> annotations.(pos) <- ByAssumption a
    | ModusPonensFound (a, b) -> annotations.(pos) <- ByModusPonens (a, b)
    | NotFound                -> annotations.(pos) <- NotProved

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

