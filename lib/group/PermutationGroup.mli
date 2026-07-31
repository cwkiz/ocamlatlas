(** it's a permutation group *)

type t = { degree : int; generators : Permutation.t list }

val create : Permutation.t list -> t
val trivial : int -> t
val degree : t -> int
val generators : t -> Permutation.t list
val elements : t -> Permutation.t list
