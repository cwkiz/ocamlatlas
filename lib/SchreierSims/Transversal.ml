module IntBfs = Bfs.Make (struct
  type t = int
  let equal = Int.equal
  let hash = Hashtbl.hash
end)

type t = {
  degree : int;
  base : int;
  points : int list;
  parents : (int * Permutation.t) option array;
  representatives : Permutation.t option array;
}

let valid_point transversal point = point >= 0 && point < transversal.degree

let create ~degree ~generators ~base =
  if degree < 0 then invalid_arg "Transversal.create: degree must be non-negative";
  if base < 0 || base >= degree then
    invalid_arg "Transversal.create: base must be a point of the action";
  List.iter
    (fun generator ->
      Permutation.validate generator;
      if Array.length generator <> degree then
        invalid_arg "Transversal.create: generators must have the supplied degree")
    generators;
  let parents = Array.make degree None in
  let representatives = Array.make degree None in
  representatives.(base) <- Some (Permutation.id degree);
  let points =
    IntBfs.traverse_with_tree ~start:base
      ~neighbours:(fun point -> List.map (fun generator -> generator.(point)) generators)
      ~on_discover:(fun ~parent point ->
        let generator =
          List.find (fun candidate -> candidate.(parent) = point) generators
        in
        let parent_representative =
          match representatives.(parent) with Some p -> p | None -> assert false
        in
        parents.(point) <- Some (parent, generator);
        representatives.(point) <-
          Some (Permutation.comp generator parent_representative))
  in
  { degree; base; points; parents; representatives }

let degree transversal = transversal.degree
let base transversal = transversal.base
let points transversal = transversal.points
let mem transversal point =
  valid_point transversal point && Option.is_some transversal.representatives.(point)
let representative transversal point =
  if valid_point transversal point then transversal.representatives.(point) else None
let parent transversal point =
  if valid_point transversal point then transversal.parents.(point) else None
