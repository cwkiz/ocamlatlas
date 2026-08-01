open Ocamlatlas

let permutation_order p =
  let identity = Permutation.id (Array.length p) in
  let rec loop power n =
    if power = identity then n
    else loop (Permutation.comp p power) (n + 1)
  in
  loop p 1

(* Assertion case S3 again, based on old test which is now reused in example*)

let () = 
  assert (PermutationGroup.degree (PermutationGroup.create [ [| 1; 2; 0 |];]) = 3)

let () = 
  assert (List.length (PermutationGroup.elements (PermutationGroup.create [ [| 1; 2; 0 |];])) = 3)