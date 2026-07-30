  module IntBfs = Bfs.Make (struct
    type t = int

    let equal = Int.equal
    let hash = Hashtbl.hash
  end)

  type t = {
    degree : int;
    base : int;
    generators : Permutation.t list;
    points : int list;
    parents : (int * Permutation.t) option array;
    representatives : Permutation.t option array;
  }

  let valid_point orbit point = point >= 0 && point < orbit.degree

  let create ~degree ~generators ~start =
    if degree < 0 then invalid_arg "Orbit.create: degree must be non-negative";
    if start < 0 || start >= degree then
      invalid_arg "Orbit.create: start must be a point of the action";
    List.iter
      (fun generator ->
        Permutation.validate generator;
        if Array.length generator <> degree then
          invalid_arg "Orbit.create: generators must have the supplied degree")
      generators;
    let parents = Array.make degree None in
    let representatives = Array.make degree None in
    representatives.(start) <- Some (Permutation.id degree);
    let points =
      IntBfs.traverse_with_tree
        ~start
        ~neighbours:(fun point ->
          List.map (fun generator -> generator.(point)) generators)
        ~on_discover:(fun ~parent point ->
          let generator =
            List.find (fun candidate -> candidate.(parent) = point) generators
          in
          let parent_representative =
            match representatives.(parent) with
            | Some representative -> representative
            | None -> assert false
          in
          parents.(point) <- Some (parent, generator);
          representatives.(point) <-
            Some (Permutation.comp generator parent_representative))
    in
    { degree; base = start; generators; points; parents; representatives }

  let base orbit = orbit.base
  let degree orbit = orbit.degree
  let points orbit = orbit.points
  let mem orbit point = valid_point orbit point && Option.is_some orbit.representatives.(point)

  let parent orbit point =
    if not (valid_point orbit point) then None else orbit.parents.(point)

  let transversal orbit point =
    if not (valid_point orbit point) then None else orbit.representatives.(point)

  let stabilizer_generators orbit =
    let identity = Permutation.id orbit.degree in
    let add_if_new generator generators =
      if List.exists (Permutation.equal generator) generators then generators
      else generator :: generators
    in
    let generators =
      List.fold_left
        (fun result point ->
          let representative =
            match orbit.representatives.(point) with
            | Some permutation -> permutation
            | None -> assert false
          in
          List.fold_left
            (fun result generator ->
              let image = generator.(point) in
              let image_representative =
                match orbit.representatives.(image) with
                | Some permutation -> permutation
                | None -> assert false
              in
              let schreier_generator =
                Permutation.comp
                  (Permutation.inv image_representative)
                  (Permutation.comp generator representative)
              in
              if Permutation.equal schreier_generator identity then result
              else add_if_new schreier_generator result)
            result orbit.generators)
        [] orbit.points
    in
    List.rev generators
