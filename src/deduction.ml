open Arithmetic
open Util

module V = Verification

exception NotProved of int
exception UnknownVariable of string

let deduce proof annotations assumpts a =
  let new_proof = ref [] in
  for i = 0 to Array.length proof - 1 do
    let e = proof.(i) in
    match annotations.(i) with
    | _ when e = a                 ->
       let naming = function
         | "A" -> a
         | name -> raise (UnknownVariable name)
       in
       let a_is_a = get_standard_proof "a_is_a" naming in
       new_proof := a_is_a
                      @ !new_proof
    | V.ByAxiom _
    | V.ByAssumption _             ->
       let naming = function
         | "A" -> a
         | "B" -> e
         | name -> raise (UnknownVariable name)
       in
       let deduce_by_assumpt = get_standard_proof "deduce_assumpt" naming in
       new_proof := deduce_by_assumpt
                      @ !new_proof
    | V.ByModusPonens (c_pos, d_pos)  ->
       let c = proof.(c_pos) in
       let d = proof.(d_pos) in
       let naming = function
         | "A" -> a
         | "B" -> e
         | "C" -> c
         | "D" -> d
         | name -> raise (UnknownVariable name)
       in
       let deduce_by_mp = get_standard_proof "deduce_by_mp" naming in
       new_proof := deduce_by_mp
                      @ !new_proof
    | V.ByRule1 prev_pos ->
       let prev = proof.(prev_pos) in
       let (b, c) = match prev with
         | Impl (b, c) -> (b, c)
         | _ -> failwith ("Unexpected " ^ string_of_expression prev)
       in
       let naming = function
         | "A" -> a
         | "B" -> b
         | "C" -> c
         | name -> raise (UnknownVariable name)
       in
       let deduce_by_rule1 = get_standard_proof "deduce_rule1" naming in
       new_proof := deduce_by_rule1
                      @ !new_proof
    | V.ByRule2 prev_pos ->
       let prev = proof.(prev_pos) in
       let (b, c) = match prev with
         | Impl (b, c) -> (b, c)
         | _ -> failwith ("Unexpected " ^ string_of_expression prev)
       in
       let naming = function
         | "A" -> a
         | "B" -> b
         | "C" -> c
         | name -> raise (UnknownVariable name)
       in
       let deduce_by_rule2 = get_standard_proof "deduce_rule2" naming in
       new_proof := deduce_by_rule2
                      @ !new_proof
  done;
  Array.of_list (List.rev !new_proof)
