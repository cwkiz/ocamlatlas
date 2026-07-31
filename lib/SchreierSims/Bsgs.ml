type level = {
  base_point : int;
  generators : Permutation.t list;
  orbit : Orbit.t;
}

type t = {
  degree : int;
  base : int list;
  levels : level list;
  strong_generators : StrongGenerators.t;
}

let complete_base degree prefix =
  let seen = Array.make degree false in
  List.iter (fun point ->
    if point < 0 || point >= degree then invalid_arg "Bsgs.create: base point outside the action";
    if seen.(point) then invalid_arg "Bsgs.create: base points must be distinct";
    seen.(point) <- true) 
  prefix;
 prefix @ List.filter (fun p -> not seen.(p)) (List.init degree Fun.id)

let create ?(base = []) group =
  let degree = PermutationGroup.degree group in
  let base = complete_base degree base in
  let active_generators = ref (PermutationGroup.generators group) in
  let levels_rev = ref [] in
  List.iter
    (fun base_point ->
      let generators = !active_generators in
      let orbit = Orbit.create ~degree ~generators ~start:base_point in
      levels_rev := { base_point; generators; orbit } :: !levels_rev;
      active_generators := Orbit.stabilizer_generators orbit)
    base;
  let levels = List.rev !levels_rev in
  let strong_generators =
    StrongGenerators.create ~degree ~base (List.map (fun level -> level.generators) levels)
  in
  { degree; base; levels; strong_generators }

let degree bsgs = bsgs.degree
let base bsgs = bsgs.base
let levels bsgs = bsgs.levels
let base_point level = level.base_point
let orbit level = level.orbit
let transversal level = Orbit.transversal_object level.orbit
let generators level = level.generators
let strong_generators bsgs = bsgs.strong_generators

let order bsgs =
  List.fold_left (fun n level -> n * List.length (Orbit.points level.orbit)) 1 bsgs.levels

let sift bsgs permutation =
  Permutation.validate permutation;
  if Array.length permutation <> bsgs.degree then
    invalid_arg "Bsgs.sift: permutation has the wrong degree";
  let residue = ref permutation in
  let failed = ref false in
  List.iter (fun level ->
    if not !failed then begin
      let image = (!residue).(level.base_point) in
      match Orbit.transversal level.orbit image with
      | None -> failed := true
      | Some representative ->
          residue := Permutation.comp (Permutation.inv representative) !residue
    end) bsgs.levels;
  if !failed || not (Permutation.equal !residue (Permutation.id bsgs.degree))
  then Some !residue else None

let contains bsgs permutation = Option.is_none (sift bsgs permutation)
