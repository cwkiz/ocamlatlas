

type G

let commutator g x y =
  G.mul g
    (G.mul g (G.inv g x) (G.inv g y))
    (G.mul g x y)

let conjugate g x y =
  G.mul g
    (G.inv g y)
    (G.mul g x y)

let power g x n =
  let ( * ) = G.mul g in
  let rec aux acc n =
    if n = 0 then
      acc
    else
      aux (acc * x) (n - 1)
  in
  aux (G.id g) n (* fix the syntactic sugar tmw im done *)

