(* permutation: Interface for permutations*)


type t = int array
let id (n : int) : t =
  if n < 0 then invalid_arg "Permutation.id: degree must be non-negative";
  Array.init n (fun i -> i)

let equal = Array.equal Int.equal

let compare = Array.compare Int.compare
let hash p =
  Array.fold_left
    (fun h x -> h * 65599 + x)
    0
    p

let validate (p : t) : unit =
  let n = Array.length p in
  let seen = Array.make n false in
  Array.iteri
    (fun i image ->
      if image < 0 || image >= n then
        invalid_arg
          (Printf.sprintf
             "Permutation: element %d maps to %d, outside 0..%d"
             i image (n - 1));
      if seen.(image) then
        invalid_arg
          (Printf.sprintf
             "Permutation: %d is the image of more than one element" image);
      seen.(image) <- true)
    p

let cycles (p : t) : int list list =
  validate p;
  let n = Array.length p in
  let seen = Array.make n false in
  let rec walk i =
    if seen.(i) then []
    else begin
      seen.(i) <- true;
      i :: walk p.(i)
    end
  in
  let rec collect i acc =
    if i = n then List.rev acc
    else collect (i + 1) (if seen.(i) then acc else walk i :: acc)
  in
  collect 0 []

let comp (p : t) (q : t) : t =
  validate p;
  validate q;
  if Array.length p <> Array.length q then
    invalid_arg "Permutation.comp: permutations must have the same degree";
  Array.init (Array.length p) (fun i -> p.(q.(i)))

let to_string (p : t) : string =
  let cycles = cycles p in
  let cycle_strings =
    List.map
      (fun cycle ->
        let elements = List.map string_of_int cycle in
        "(" ^ String.concat " " elements ^ ")")
      cycles
  in
  String.concat "" cycle_strings

 let inv (p : t) : t =
  validate p;
  let n = Array.length p in
  let inverse = Array.make n 0 in
  for i = 0 to n - 1 do
    inverse.(p.(i)) <- i
  done;
  inverse
