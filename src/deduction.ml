open Expression
module V = Verification

exception NotProved

let deduce asserts proof a b =
  let new_asserts = ref [] in
  for i = 0 to Array.length asserts - 1 do
    let e = asserts.(i) in
    match proof.(i) with
    | V.ByAxiom _
    | V.ByAssumption _             ->
       new_asserts := (a @-> e)
                      :: (e @-> a @-> e)
                      :: e
                      :: !new_asserts
    | _ when e = a                 ->
       new_asserts :=
         (a @-> a) 
         :: (a @-> (a @-> a) @-> a)
         :: ((a @-> (a @-> a) @-> a) @-> a @-> a)
         :: ((a @-> (a @-> a)) @-> (a @-> (a @-> a) @-> a) @-> a @-> a)
         :: (a @-> a @-> a)
         :: !new_asserts
    | V.ByModusPonens (c_pos, d_pos)  ->
       let c = asserts.(c_pos) in
       let d = asserts.(d_pos) in
       new_asserts :=
         (a @-> e)
         :: ((a @-> c) @-> (a @-> e))
         :: ((a @-> d) @-> (a @-> c) @-> (a @-> e))
         :: !new_asserts
    | V.NotProved                  -> raise NotProved
  done;
  Array.of_list (List.rev !new_asserts)
