type t = {
  degree : int;
  base : int list;
  by_level : Permutation.t list array;
  permanent : Permutation.t list;
}

let create ~degree ~base levels =
  if degree < 0 then invalid_arg "StrongGenerators.create: degree must be non-negative";
  if List.length base <> List.length levels then
    invalid_arg "StrongGenerators.create: one generator set is required per base point";
  List.iter
    (fun point -> if point < 0 || point >= degree then
      invalid_arg "StrongGenerators.create: base point outside the action") base;
  let check generator =
    Permutation.validate generator;
    if Array.length generator <> degree then
      invalid_arg "StrongGenerators.create: generator has the wrong degree"
  in
  let rec check_prefix count generator = function
    | _ when count = 0 -> ()
    | point :: rest ->
        if generator.(point) <> point then
          invalid_arg
            "StrongGenerators.create: a level generator must fix earlier base points";
        check_prefix (count - 1) generator rest
    | [] -> assert false
  in
  List.iteri
    (fun level generators ->
      List.iter
        (fun generator ->
          check generator;
          check_prefix level generator base)
        generators)
    levels;
  let permanent =
    levels
    |> List.flatten
    |> List.fold_left (fun generators generator ->
      if List.exists (Permutation.equal generator) generators then generators
      else generator :: generators) []
    |> List.rev
  in
  { degree; base; by_level = Array.of_list levels; permanent }

let degree t = t.degree
let base t = t.base
let levels t = Array.length t.by_level
let at_level t i =
  if i < 0 || i >= Array.length t.by_level then invalid_arg "StrongGenerators.at_level";
  t.by_level.(i)
let permanent t = t.permanent
let generators = permanent
