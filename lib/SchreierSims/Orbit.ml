type t = {
  generators : Permutation.t list;
  transversal : Transversal.t;
}

let create ~degree ~generators ~start =
  { generators; transversal = Transversal.create ~degree ~generators ~base:start }

let base orbit = Transversal.base orbit.transversal
let degree orbit = Transversal.degree orbit.transversal
let points orbit = Transversal.points orbit.transversal
let mem orbit = Transversal.mem orbit.transversal
let parent orbit = Transversal.parent orbit.transversal
let transversal orbit = Transversal.representative orbit.transversal
let transversal_object orbit = orbit.transversal

let stabilizer_generators orbit =
  let identity = Permutation.id (degree orbit) in
  let add_if_new generator generators =
    if List.exists (Permutation.equal generator) generators then generators
    else generator :: generators
  in
  let representative point =
    match transversal orbit point with Some permutation -> permutation | None -> assert false
  in
  points orbit
  |> List.fold_left (fun result point ->
    let point_representative = representative point in
    List.fold_left (fun result generator ->
      let image = generator.(point) in
      let schreier_generator =
        Permutation.comp (Permutation.inv (representative image))
          (Permutation.comp generator point_representative)
      in
      if Permutation.equal schreier_generator identity then result
      else add_if_new schreier_generator result) result orbit.generators) []
  |> List.rev
