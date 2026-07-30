(** An orbit linked to its Schreier tree and transversal. *)

type t

val create :
  degree:int -> generators:Permutation.t list -> start:int -> t

val base : t -> int
val degree : t -> int
val points : t -> int list
val mem : t -> int -> bool

val parent : t -> int -> (int * Permutation.t) option
val transversal : t -> int -> Permutation.t option
val stabilizer_generators : t -> Permutation.t list
