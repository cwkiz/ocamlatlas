open Ocamlatlas

let permutation_order p =
  let identity = Permutation.id (Array.length p) in
  let rec loop power n =
    if power = identity then n
    else loop (Permutation.comp p power) (n + 1)
  in
  loop p 1

let report name generators =
  let group = PermutationGroup.create generators in
  let elements = PermutationGroup.elements group in
  let orders =
    elements
    |> List.map permutation_order
    |> List.sort_uniq compare
  in
  Printf.printf "%s\n" name;
  Printf.printf "  degree: %d\n" (PermutationGroup.degree group);
  Printf.printf "  generators: %d\n" (List.length generators);
  Printf.printf "  elements: %d\n" (List.length elements);
  Printf.printf "  element orders: %s\n\n"
    (String.concat ", " (List.map string_of_int orders))

let () =
  report "C3" [
    [|1; 2; 0|];
  ];

  report "S3" [
    [|1; 0; 2|];
    [|1; 2; 0|];
  ];

  report "D4" [
    [|1; 2; 3; 0|];
    [|0; 3; 2; 1|];
  ];

  report "A4" [
    [|1; 2; 0; 3|];
    [|1; 3; 2; 0|];
  ];

  report "S4" [
    [|1; 0; 2; 3|];
    [|1; 2; 3; 0|];
  ];

  report "C7" [
    [|1; 2; 3; 4; 5; 6; 0|];
  ]
