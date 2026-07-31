(** Strong generators of a BSGS *)

type t

val create : degree:int -> base:int list -> Permutation.t list list -> t
val degree : t -> int
val base : t -> int list
val levels : t -> int
val at_level : t -> int -> Permutation.t list
(** Every generator in strong gen. set, permanent list *)
val permanent : t -> Permutation.t list
val generators : t -> Permutation.t list
