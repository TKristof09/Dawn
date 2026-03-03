type t

val create : unit -> t
val define : t -> Node.t -> int
val set_name : t -> int -> string -> unit
val link : t -> (Node.t, Graph.readwrite) Graph.t -> Node.t -> Node.t list
val get_name : t -> int -> string
