open Ocamlatlas
let g =
  PermutationGroup.create
    [ permutation_for_(0 1); permutation_for_(1 2) ]


let stabilizerchain = StabilizerChain.build ~degree:3 ~generators:(PermutationGroup.generators g) ~base:[0] 
let orbit1 = Orbit.create ~degree:3 ~generators:(PermutationGroup.generators g) ~start:0 