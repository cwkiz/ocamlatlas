open Ocamlatlas

let assert_equal_int expected actual = assert (expected = actual)
let assert_true value = assert value

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
  assert_true (Option.is_some (Bsgs.sift cyclic_bsgs [| 1; 0; 2 |]))
