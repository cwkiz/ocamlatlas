(** A Schreier--Sims stabilizer chain for a chosen base. *)

type level
type t

val build : PermutationGroup.t -> base:int list -> t
val degree : t -> int
val levels : t -> level list
val base_point : level -> int
val orbit : level -> Orbit.t
val generators : level -> Permutation.t list
val order : t -> int
