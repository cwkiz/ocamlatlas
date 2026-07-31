
module type VERTEX = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
end

module Make (Vertex : VERTEX) : sig
  type vertex = Vertex.t

  val traverse : start:vertex -> neighbours:(vertex -> vertex list) -> vertex list

  val traverse_with_tree :
    start:vertex ->
    neighbours:(vertex -> vertex list) ->
    on_discover:(parent:vertex -> vertex -> unit) ->
    vertex list
end
