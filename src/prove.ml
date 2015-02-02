open Expression
open Util
open Deduction

module H = Hashtbl

exception UnknownFunction
exception UnknownVariable of string
exception NotProveable of int

let get_variables =
  let rec get_variables' vars = function
  | Var s       -> s :: vars
  | Not a       -> get_variables' vars a
  | And (a, b)
  | Or (a, b)
  | Impl (a, b) -> let vars' = get_variables' vars a in
                   get_variables' vars' b
  in (Util.sort_uniq compare) |> (get_variables' [])

let inverse_vars vars =
  let table = H.create (Array.length vars) in
  for i = 0 to Array.length vars - 1 do
    H.add table vars.(i) i
  done;
  table

let rec substitute a_val b_val = function
  | Var "A"     -> a_val
  | Var "B"     -> b_val
  | Var name    -> raise (UnknownVariable name)
  | Not a       -> Not (substitute a_val b_val a)
  | And (a, b)  -> And (substitute a_val b_val a,
                        substitute a_val b_val b)
  | Or (a, b)   -> Or (substitute a_val b_val a,
                        substitute a_val b_val b)
  | Impl (a, b) -> Impl (substitute a_val b_val a,
                        substitute a_val b_val b)

let get_standard_proof name a_val b_val =
  let name_f = open_in ("res/proofs/" ^ name) in
  let lines = f_read_lines name_f in
  close_in name_f;
  let proof = Array.map (parse_string Parser.expr_line) lines in
  List.rev (Array.to_list (Array.map (substitute a_val b_val) proof))

let apply_op = function
  | "and"  -> (&&)
  | "or"   -> (||)
  | "impl" -> fun a b -> (not a) || (a && b)
  | _      -> raise UnknownFunction

class prover e =
  let vars_ = Array.of_list (get_variables e) in
  let id_   = inverse_vars vars_ in
  object (self)
    val vars           = vars_
    val id             = id_
    val mutable proofs = Array.make (1 lsl Array.length vars_) []

    method get_proof_with_assumptions mask =
      let rec get_proof' = function
        | Var s       -> let pos = H.find id s in
                         if (mask land (1 lsl pos)) <> 0
                         then (true, [Var s])
                         else (false, [!@(Var s)])
        | Not a       -> combine_not a
        | And (a, b)  -> combine "and" a b
        | Or (a, b)   -> combine "or" a b
        | Impl (a, b) -> combine "impl" a b
                                 
      and combine op a b =
        let (res_a, proof_a) = get_proof' a in
        let (res_b, proof_b) = get_proof' b in
        let name = op ^ "_" ^ string_of_bool res_a
                   ^ "_" ^ string_of_bool res_b in
        (apply_op op res_a res_b,
         (get_standard_proof name a b)
         @ proof_a
         @ proof_b)

      and combine_not a =
        let (result, proof) = get_proof' a in
        (not result,
         (get_standard_proof ("not_" ^ string_of_bool result) a (Var "b"))
         @ proof)
      in
      let (result, proof) = (get_proof' e) in
      (result, List.rev proof)

    method get_assumptions mask size =
      let rec get_assumptions' i =
        if i = size
        then []
        else if (mask land (1 lsl i)) != 0
        then Var vars.(i) :: get_assumptions' (i + 1)
        else !@(Var vars.(i)) :: get_assumptions' (i + 1)
      in get_assumptions' 0

    method prove =
      for mask = 0 to (1 lsl Array.length vars) - 1 do
        let (result, proof) = self#get_proof_with_assumptions mask in
        if result
        then proofs.(mask) <- proof
        else raise (NotProveable mask)
      done;

      for candidate = Array.length vars - 1 downto 0 do
        let new_proofs = Array.make (1 lsl candidate) [] in
        for mask = 0 to (1 lsl candidate) - 1 do
          let mask_false = mask in
          let mask_true = mask + (1 lsl candidate) in
          let proof_false = proofs.(mask_false) in
          let proof_true = proofs.(mask_true) in
          let current_var = Var vars.(candidate) in
          let assumpts = self#get_assumptions mask candidate in
          let assumpts_false = !@ current_var
                               :: assumpts in
          let assumpts_true = current_var
                              :: assumpts in
          let deduced_false = deduce_2 assumpts_false proof_false e in
          let deduced_true = deduce_2 assumpts_true proof_true e in
          let excluded_assumption = get_standard_proof "excluded_assumption"
                                                        e
                                                        current_var in
          new_proofs.(mask) <- deduced_false
                               @ deduced_true
                               @ List.rev excluded_assumption
        done;
        proofs <- new_proofs
      done;
      proofs.(0)
  end

let prove e = (new prover(e))#prove
