module E = Expression
module A = Axioms
module H = Hashtbl

let (=~~) = E.(=~~)

exception AxiomFound of int
exception ModusPonensFound of int * int
exception NotFound

type annotation = NotProved
                | ByAxiom of int
                | ByModusPonens of int * int

let string_of_annotation = function
  | NotProved            -> "not proved"
  | ByAxiom a            -> "by axiom " ^ string_of_int (a + 1)
  | ByModusPonens (a, b) -> "by M. P. "
                            ^ string_of_int (a + 1) ^ ", "
                            ^ string_of_int (b + 1)

class verifier (list) =
object (self)
  val mutable assertions = Array.of_list list
  val annotations        = Array.make (List.length list) NotProved

  method annotations = annotations

  method check_axioms e =
    for i = 0 to Array.length A.axioms - 1 do
      if e =~~ A.axioms.(i) then begin
          raise (AxiomFound i)
        end
    done

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
      self#check_modus_ponens e pos
    with
    | AxiomFound a            -> annotations.(pos) <- ByAxiom a
    | ModusPonensFound (a, b) -> annotations.(pos) <- ByModusPonens (a, b)
    | NotFound                -> annotations.(pos) <- NotProved

  method add_all =
    for i = 0 to Array.length assertions - 1 do
      self#add i
    done
end

let verify assertions =
  let v = new verifier(assertions) in
  v#add_all;
  v#annotations
