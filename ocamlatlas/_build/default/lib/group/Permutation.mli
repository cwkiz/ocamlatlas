type t = int array
val id : int -> t
val equal : int array -> int array -> bool
val compare : int array -> int array -> int
val hash : int array -> int
val validate : t -> unit
val cycles : t -> int list list
val comp : t -> t -> t
val inv : t -> t
