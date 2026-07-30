
type t = {
  degree : int;
  generators : Permutation.t list;
}

let create (generators : Permutation.t list) : t =
  match generators with
  | [] ->
      invalid_arg
        "PermutationGroup.create: cannot infer the degree of an empty generating set"
  | first :: _ ->
      let degree = Array.length first in
      List.iter
        (fun generator ->
          Permutation.validate generator;
          if Array.length generator <> degree then
            invalid_arg
              "PermutationGroup.create: every generator must have the same degree")
        generators;
      { degree; generators }

let trivial (degree : int) : t =
  if degree < 0 then
    invalid_arg "PermutationGroup.trivial: degree must be non-negative";
  { degree; generators = [] }

let degree (group : t) : int = group.degree
let generators (group : t) : Permutation.t list = group.generators

module PrmBFS = Bfs.Make (struct
  type t = Permutation.t

  let equal = Permutation.equal
  let hash = Permutation.hash
end)

let elements (group : t) : Permutation.t list =
  let identity = Permutation.id group.degree in
  PrmBFS.traverse
    ~start:identity
    ~neighbours:(fun permutation ->
      List.map (Permutation.comp permutation) group.generators)
