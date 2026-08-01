(** Base and strong generating set (BSGS) for a finite permutation group. *)

type level
type t

val create : ?base:int list -> PermutationGroup.t -> t
val degree : t -> int
val base : t -> int list
val levels : t -> level list
val base_point : level -> int
val orbit : level -> Orbit.t
val transversal : level -> Transversal.t
val generators : level -> Permutation.t list
val strong_generators : t -> StrongGenerators.t
val order : t -> int

(** [sift b g] is [None] when [g] belongs to the group represented by [b].
    Otherwise it returns [(i, r)], where [i] is the zero-based first level
    whose orbit does not contain the current image, and [r] is the residue
    before sifting at that level.  A failure at [List.length (levels b)]
    denotes a non-identity residue after every level. *)
val sift : t -> Permutation.t -> (int * Permutation.t) option
val contains : t -> Permutation.t -> bool
