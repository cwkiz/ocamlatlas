module type VERTEX = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
end

module Make (Vertex : VERTEX) = struct
  module Seen = Hashtbl.Make (Vertex)

  type vertex = Vertex.t

  let traverse_with_tree ~start ~neighbours ~on_discover =
    let seen = Seen.create 64 in
    let queue : vertex Queue.t = Queue.create () in
    let visited = ref [ start ] in
    Seen.add seen start ();
    Queue.add start queue;
    while not (Queue.is_empty queue) do
      let current = Queue.take queue in
      List.iter
        (fun next ->
          if not (Seen.mem seen next) then begin
            Seen.add seen next ();
            on_discover ~parent:current next;
            Queue.add next queue;
            visited := next :: !visited
          end)
        (neighbours current)
    done;
    List.rev !visited

  let traverse ~start ~neighbours =
    traverse_with_tree ~start ~neighbours ~on_discover:(fun ~parent:_ _ -> ())
end
