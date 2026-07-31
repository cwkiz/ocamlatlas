

type t

val create : degree:int -> generators:Permutation.t list -> base:int -> t
val degree : t -> int
val base : t -> int
val points : t -> int list
val mem : t -> int -> bool
val representative : t -> int -> Permutation.t option
val parent : t -> int -> (int * Permutation.t) option
