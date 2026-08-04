(** BboxGroup:Black box group interface,
original impl: https://brauer.maths.qmul.ac.uk/Atlas/info/interpret.html *)

module type BLACK_BOX = sig
  type t
  type element

  val one        : t -> element
  val random     : t -> element

  val equal      : t -> element -> element -> bool

  val multiply   : t -> element -> element -> element
  val inverse    : t -> element -> element

  val power      : t -> int -> element -> element
  val conjugate  : t -> element -> by:element -> element
  val commutator : t -> element -> element -> element

  val order      : t -> element -> int
  val has_order  : t -> element -> int -> bool

end