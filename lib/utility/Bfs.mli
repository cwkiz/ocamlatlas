(** Breadth-first traversal over one fixed vertex type. *)

module type VERTEX = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
end

module Make (Vertex : VERTEX) : sig
  type vertex = Vertex.t

  (** [traverse ~start ~neighbours] visits every vertex reachable from
      [start], in breadth-first discovery order. *)
  val traverse : start:vertex -> neighbours:(vertex -> vertex list) -> vertex list

  (** Like [traverse], calling [on_discover] once for every non-root vertex.
      The first argument is its predecessor in the breadth-first tree. *)
  val traverse_with_tree :
    start:vertex ->
    neighbours:(vertex -> vertex list) ->
    on_discover:(parent:vertex -> vertex -> unit) ->
    vertex list
end
