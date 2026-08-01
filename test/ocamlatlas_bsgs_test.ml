open Ocamlatlas
open Unit

let assert_equal_int expected actual = assert (expected = actual)
let assert_true value = assert value

(* Assertion BSGS test for case group S3*)
let () =
  let s3 =
    PermutationGroup.create [ [| 1; 0; 2 |]; [| 1; 2; 0 |] ]
  in
  let bsgs = Bsgs.create ~base:[ 0 ] s3 in
  assert_equal_int 6 (Bsgs.order bsgs);
  assert_true
    (List.length (StrongGenerators.permanent (Bsgs.strong_generators bsgs)) > 2);
  List.iter (fun g -> assert_true (Bsgs.contains bsgs g))
    (PermutationGroup.elements s3);
  let level = List.hd (Bsgs.levels bsgs) in
  let transversal = Bsgs.transversal level in
  List.iter (fun point ->
    match Transversal.representative transversal point with
    | Some representative -> assert_equal_int point representative.(Transversal.base transversal)
    | None -> assert false) (Transversal.points transversal);
  let c3 = PermutationGroup.create [ [| 1; 2; 0 |] ] in
  let cyclic_bsgs = Bsgs.create c3 in
  assert_true (not (Bsgs.contains cyclic_bsgs [| 1; 0; 2 |]));
  (match Bsgs.sift cyclic_bsgs [|1;0;2|] with
| Some (level, residue) ->
    Printf.printf "the unit is still mid u failed sifting C3 at level %d\n" level;
    Printf.printf "%s\n" (Permutation.to_string residue) (* (0 2 1)(0 1) eq (1 2) *)
| None ->
    Printf.printf "member\n");
  let klein_four =
    PermutationGroup.create [ [| 1; 0; 3; 2 |]; [| 2; 3; 0; 1 |] ]
  in
  let klein_bsgs = Bsgs.create ~base:[ 0 ] klein_four in
  assert_equal_int 4 (Bsgs.order klein_bsgs);
  List.iter (fun g -> assert_true (Bsgs.contains klein_bsgs g))
    (PermutationGroup.elements klein_four);
  let point_stabilizer = PermutationGroup.create [ [| 0; 2; 1; 3 |] ] in
  let point_stabilizer_bsgs = Bsgs.create ~base:[ 0 ] point_stabilizer in
  (match Bsgs.sift point_stabilizer_bsgs [| 0; 3; 2; 1 |] with
   | Some (1, residue) -> assert_true (Permutation.equal residue [| 0; 3; 2; 1 |])
   | Some _ -> assert false
   | None -> assert false)
