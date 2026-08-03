(** BboxGroup:Black box group interface,
original impl: https://brauer.maths.qmul.ac.uk/Atlas/info/interpret.html *)

module type BLACK_BOX = sig
  type group
  type element

  val random : group -> element

  val mul : group -> element -> element -> element

  val inv : group -> element -> element

  val order : group -> element -> int

  val equal : group -> element -> element -> bool
end