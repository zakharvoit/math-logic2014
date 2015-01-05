open Expression
module V = Verification

exception NotProved of int

let deduce asserts proof a b =
  let new_asserts = ref [] in
  for i = 0 to Array.length asserts - 1 do
    let e = asserts.(i) in
    match proof.(i) with
    | _ when e = a                 ->
       new_asserts :=
         (a @-> a) 
         :: (a @-> (a @-> a) @-> a)
         :: ((a @-> (a @-> a) @-> a) @-> a @-> a)
         :: ((a @-> (a @-> a)) @-> (a @-> (a @-> a) @-> a) @-> a @-> a)
         :: (a @-> a @-> a)
         :: !new_asserts
    | V.ByAxiom _
    | V.ByAssumption _             ->
       new_asserts := (a @-> e)
                      :: (e @-> a @-> e)
                      :: e
                      :: !new_asserts
   | V.ByModusPonens (c_pos, d_pos)  ->
       let c = asserts.(c_pos) in
       let d = asserts.(d_pos) in
       new_asserts :=
         (a @-> e)
         :: ((a @-> d) @-> (a @-> e))
         :: ((a @-> c) @-> (a @-> d) @-> (a @-> e))
         :: !new_asserts
    | V.NotProved                  -> raise (NotProved i)
  done;
  Array.of_list (List.rev !new_asserts)

let deduce_2 assumptions proof assertion =
  let proof_arr = Array.of_list proof in
  let annotations = V.verify_with_assumptions proof_arr (Array.of_list assumptions) in
  Array.to_list (deduce proof_arr annotations (List.hd assumptions) assertion)
