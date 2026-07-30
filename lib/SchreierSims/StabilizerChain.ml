type level = {
  base_point : int;
  orbit : Orbit.t;
  generators : Permutation.t list;
}

type t = { degree : int; levels : level list }

let build group ~base =
  let degree = PermutationGroup.degree group in
  List.iter
    (fun point ->
      if point < 0 || point >= degree then
        invalid_arg "StabilizerChain.build: every base point must be in range!")
    base;
  let rec build_levels generators = function
    | [] -> []
    | base_point :: remaining_base ->
        let orbit = Orbit.create ~degree ~generators ~start:base_point in
        let level = { base_point; orbit; generators } in
        let stabilizer_generators = Orbit.stabilizer_generators orbit in
        level :: build_levels stabilizer_generators remaining_base
  in
  { degree; levels = build_levels (PermutationGroup.generators group) base }

let degree chain = chain.degree
let levels chain = chain.levels
let base_point level = level.base_point
let orbit level = level.orbit
let generators level = level.generators

let order chain =
  List.fold_left
    (fun product level -> product * List.length (Orbit.points level.orbit))
    1 chain.levels
